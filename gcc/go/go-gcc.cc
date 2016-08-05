// go-gcc.cc -- Go frontend to gcc IR.
// Copyright (C) 2011-2016 Free Software Foundation, Inc.
// Contributed by Ian Lance Taylor, Google.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "go-system.h"

// This has to be included outside of extern "C", so we have to
// include it here before tree.h includes it later.
#include <gmp.h>

#include "tree.h"
#include "fold-const.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "varasm.h"
#include "tree-iterator.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "convert.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "langhooks.h"
#include "toplev.h"
#include "output.h"
#include "realmpfr.h"
#include "builtins.h"

#include "go-c.h"

#include "gogo.h"
#include "backend.h"

// A class wrapping a tree.

class Gcc_tree
{
 public:
  Gcc_tree(tree t)
    : t_(t)
  { }

  tree
  get_tree() const
  { return this->t_; }

  void
  set_tree(tree t)
  { this->t_ = t; }

 private:
  tree t_;
};

// In gcc, types, expressions, and statements are all trees.
class Btype : public Gcc_tree
{
 public:
  Btype(tree t)
    : Gcc_tree(t)
  { }
};

class Bexpression : public Gcc_tree
{
 public:
  Bexpression(tree t)
    : Gcc_tree(t)
  { }
};

class Bstatement : public Gcc_tree
{
 public:
  Bstatement(tree t)
    : Gcc_tree(t)
  { }
};

class Bfunction : public Gcc_tree
{
 public:
  Bfunction(tree t)
    : Gcc_tree(t)
  { }
};

class Bblock : public Gcc_tree
{
 public:
  Bblock(tree t)
    : Gcc_tree(t)
  { }
};

class Blabel : public Gcc_tree
{
 public:
  Blabel(tree t)
    : Gcc_tree(t)
  { }
};

// Bvariable is a bit more complicated, because of zero-sized types.
// The GNU linker does not permit dynamic variables with zero size.
// When we see such a variable, we generate a version of the type with
// non-zero size.  However, when referring to the global variable, we
// want an expression of zero size; otherwise, if, say, the global
// variable is passed to a function, we will be passing a
// non-zero-sized value to a zero-sized value, which can lead to a
// miscompilation.

class Bvariable
{
 public:
  Bvariable(tree t)
    : t_(t), orig_type_(NULL)
  { }

  Bvariable(tree t, tree orig_type)
    : t_(t), orig_type_(orig_type)
  { }

  // Get the tree for use as an expression.
  tree
  get_tree(Location) const;

  // Get the actual decl;
  tree
  get_decl() const
  { return this->t_; }

 private:
  tree t_;
  tree orig_type_;
};

// Get the tree of a variable for use as an expression.  If this is a
// zero-sized global, create an expression that refers to the decl but
// has zero size.
tree
Bvariable::get_tree(Location location) const
{
  if (this->orig_type_ == NULL
      || this->t_ == error_mark_node
      || TREE_TYPE(this->t_) == this->orig_type_)
    return this->t_;
  // Return *(orig_type*)&decl.  */
  tree t = build_fold_addr_expr_loc(location.gcc_location(), this->t_);
  t = fold_build1_loc(location.gcc_location(), NOP_EXPR,
		      build_pointer_type(this->orig_type_), t);
  return build_fold_indirect_ref_loc(location.gcc_location(), t);
}

// This file implements the interface between the Go frontend proper
// and the gcc IR.  This implements specific instantiations of
// abstract classes defined by the Go frontend proper.  The Go
// frontend proper class methods of these classes to generate the
// backend representation.

class Gcc_backend : public Backend
{
 public:
  Gcc_backend();

  // Types.

  Btype*
  error_type()
  { return this->make_type(error_mark_node); }

  Btype*
  void_type()
  { return this->make_type(void_type_node); }

  Btype*
  bool_type()
  { return this->make_type(boolean_type_node); }

  Btype*
  integer_type(bool, int);

  Btype*
  float_type(int);

  Btype*
  complex_type(int);

  Btype*
  pointer_type(Btype*);

  Btype*
  function_type(const Btyped_identifier&,
		const std::vector<Btyped_identifier>&,
		const std::vector<Btyped_identifier>&,
		Btype*,
		const Location);

  Btype*
  struct_type(const std::vector<Btyped_identifier>&);

  Btype*
  array_type(Btype*, Bexpression*);

  Btype*
  placeholder_pointer_type(const std::string&, Location, bool);

  bool
  set_placeholder_pointer_type(Btype*, Btype*);

  bool
  set_placeholder_function_type(Btype*, Btype*);

  Btype*
  placeholder_struct_type(const std::string&, Location);

  bool
  set_placeholder_struct_type(Btype* placeholder,
			      const std::vector<Btyped_identifier>&);

  Btype*
  placeholder_array_type(const std::string&, Location);

  bool
  set_placeholder_array_type(Btype*, Btype*, Bexpression*);

  Btype*
  named_type(const std::string&, Btype*, Location);

  Btype*
  circular_pointer_type(Btype*, bool);

  bool
  is_circular_pointer_type(Btype*);

  int64_t
  type_size(Btype*);

  int64_t
  type_alignment(Btype*);

  int64_t
  type_field_alignment(Btype*);

  int64_t
  type_field_offset(Btype*, size_t index);

  // Expressions.

  Bexpression*
  zero_expression(Btype*);

  Bexpression*
  error_expression()
  { return this->make_expression(error_mark_node); }

  Bexpression*
  nil_pointer_expression()
  { return this->make_expression(null_pointer_node); }

  Bexpression*
  var_expression(Bvariable* var, Location);

  Bexpression*
  indirect_expression(Btype*, Bexpression* expr, bool known_valid, Location);

  Bexpression*
  named_constant_expression(Btype* btype, const std::string& name,
			    Bexpression* val, Location);

  Bexpression*
  integer_constant_expression(Btype* btype, mpz_t val);

  Bexpression*
  float_constant_expression(Btype* btype, mpfr_t val);

  Bexpression*
  complex_constant_expression(Btype* btype, mpc_t val);

  Bexpression*
  string_constant_expression(const std::string& val);

  Bexpression*
  boolean_constant_expression(bool val);

  Bexpression*
  real_part_expression(Bexpression* bcomplex, Location);

  Bexpression*
  imag_part_expression(Bexpression* bcomplex, Location);

  Bexpression*
  complex_expression(Bexpression* breal, Bexpression* bimag, Location);

  Bexpression*
  convert_expression(Btype* type, Bexpression* expr, Location);

  Bexpression*
  function_code_expression(Bfunction*, Location);

  Bexpression*
  address_expression(Bexpression*, Location);

  Bexpression*
  struct_field_expression(Bexpression*, size_t, Location);

  Bexpression*
  compound_expression(Bstatement*, Bexpression*, Location);

  Bexpression*
  conditional_expression(Btype*, Bexpression*, Bexpression*, Bexpression*,
                         Location);

  Bexpression*
  unary_expression(Operator, Bexpression*, Location);

  Bexpression*
  binary_expression(Operator, Bexpression*, Bexpression*, Location);

  Bexpression*
  constructor_expression(Btype*, const std::vector<Bexpression*>&, Location);

  Bexpression*
  array_constructor_expression(Btype*, const std::vector<unsigned long>&,
                               const std::vector<Bexpression*>&, Location);

  Bexpression*
  pointer_offset_expression(Bexpression* base, Bexpression* offset, Location);

  Bexpression*
  array_index_expression(Bexpression* array, Bexpression* index, Location);

  Bexpression*
  call_expression(Bexpression* fn, const std::vector<Bexpression*>& args,
                  Bexpression* static_chain, Location);

  Bexpression*
  stack_allocation_expression(int64_t size, Location);

  // Statements.

  Bstatement*
  error_statement()
  { return this->make_statement(error_mark_node); }

  Bstatement*
  expression_statement(Bexpression*);

  Bstatement*
  init_statement(Bvariable* var, Bexpression* init);

  Bstatement*
  assignment_statement(Bexpression* lhs, Bexpression* rhs, Location);

  Bstatement*
  return_statement(Bfunction*, const std::vector<Bexpression*>&,
		   Location);

  Bstatement*
  if_statement(Bexpression* condition, Bblock* then_block, Bblock* else_block,
	       Location);

  Bstatement*
  switch_statement(Bfunction* function, Bexpression* value,
		   const std::vector<std::vector<Bexpression*> >& cases,
		   const std::vector<Bstatement*>& statements,
		   Location);

  Bstatement*
  compound_statement(Bstatement*, Bstatement*);

  Bstatement*
  statement_list(const std::vector<Bstatement*>&);

  Bstatement*
  exception_handler_statement(Bstatement* bstat, Bstatement* except_stmt,
                              Bstatement* finally_stmt, Location);

  // Blocks.

  Bblock*
  block(Bfunction*, Bblock*, const std::vector<Bvariable*>&,
	Location, Location);

  void
  block_add_statements(Bblock*, const std::vector<Bstatement*>&);

  Bstatement*
  block_statement(Bblock*);

  // Variables.

  Bvariable*
  error_variable()
  { return new Bvariable(error_mark_node); }

  Bvariable*
  global_variable(const std::string& package_name,
		  const std::string& pkgpath,
		  const std::string& name,
		  Btype* btype,
		  bool is_external,
		  bool is_hidden,
		  bool in_unique_section,
		  Location location);

  void
  global_variable_set_init(Bvariable*, Bexpression*);

  Bvariable*
  local_variable(Bfunction*, const std::string&, Btype*, bool,
		 Location);

  Bvariable*
  parameter_variable(Bfunction*, const std::string&, Btype*, bool,
		     Location);

  Bvariable*
  static_chain_variable(Bfunction*, const std::string&, Btype*, Location);

  Bvariable*
  temporary_variable(Bfunction*, Bblock*, Btype*, Bexpression*, bool,
		     Location, Bstatement**);

  Bvariable*
  implicit_variable(const std::string&, Btype*, bool, bool, bool,
		    int64_t);

  void
  implicit_variable_set_init(Bvariable*, const std::string&, Btype*,
			     bool, bool, bool, Bexpression*);

  Bvariable*
  implicit_variable_reference(const std::string&, Btype*);

  Bvariable*
  immutable_struct(const std::string&, bool, bool, Btype*, Location);

  void
  immutable_struct_set_init(Bvariable*, const std::string&, bool, bool, Btype*,
			    Location, Bexpression*);

  Bvariable*
  immutable_struct_reference(const std::string&, Btype*, Location);

  // Labels.

  Blabel*
  label(Bfunction*, const std::string& name, Location);

  Bstatement*
  label_definition_statement(Blabel*);

  Bstatement*
  goto_statement(Blabel*, Location);

  Bexpression*
  label_address(Blabel*, Location);

  // Functions.

  Bfunction*
  error_function()
  { return this->make_function(error_mark_node); }

  Bfunction*
  function(Btype* fntype, const std::string& name, const std::string& asm_name,
           bool is_visible, bool is_declaration, bool is_inlinable,
           bool disable_split_stack, bool in_unique_section, Location);

  Bstatement*
  function_defer_statement(Bfunction* function, Bexpression* undefer,
                           Bexpression* defer, Location);

  bool
  function_set_parameters(Bfunction* function, const std::vector<Bvariable*>&);

  bool
  function_set_body(Bfunction* function, Bstatement* code_stmt);

  Bfunction*
  lookup_builtin(const std::string&);

  void
  write_global_definitions(const std::vector<Btype*>&,
                           const std::vector<Bexpression*>&,
                           const std::vector<Bfunction*>&,
                           const std::vector<Bvariable*>&);

 private:
  // Make a Bexpression from a tree.
  Bexpression*
  make_expression(tree t)
  { return new Bexpression(t); }

  // Make a Bstatement from a tree.
  Bstatement*
  make_statement(tree t)
  { return new Bstatement(t); }

  // Make a Btype from a tree.
  Btype*
  make_type(tree t)
  { return new Btype(t); }

  Bfunction*
  make_function(tree t)
  { return new Bfunction(t); }

  Btype*
  fill_in_struct(Btype*, const std::vector<Btyped_identifier>&);

  Btype*
  fill_in_array(Btype*, Btype*, Bexpression*);

  tree
  non_zero_size_type(tree);

private:
  void
  define_builtin(built_in_function bcode, const char* name, const char* libname,
		 tree fntype, bool const_p, bool noreturn_p);

  // A mapping of the GCC built-ins exposed to GCCGo.
  std::map<std::string, Bfunction*> builtin_functions_;
};

// A helper function to create a GCC identifier from a C++ string.

static inline tree
get_identifier_from_string(const std::string& str)
{
  return get_identifier_with_length(str.data(), str.length());
}

// Return whether the character c is OK to use in the assembler.

static bool
char_needs_encoding(char c)
{
  switch (c)
    {
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
    case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
    case 's': case 't': case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z':
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    case '_': case '.': case '$': case '/':
      return false;
    default:
      return true;
    }
}

// Return whether the identifier needs to be translated because it
// contains non-ASCII characters.

static bool
needs_encoding(const std::string& str)
{
  for (std::string::const_iterator p = str.begin();
       p != str.end();
       ++p)
    if (char_needs_encoding(*p))
      return true;
  return false;
}

// Pull the next UTF-8 character out of P and store it in *PC.  Return
// the number of bytes read.

static size_t
fetch_utf8_char(const char* p, unsigned int* pc)
{
  unsigned char c = *p;
  if ((c & 0x80) == 0)
    {
      *pc = c;
      return 1;
    }
  size_t len = 0;
  while ((c & 0x80) != 0)
    {
      ++len;
      c <<= 1;
    }
  unsigned int rc = *p & ((1 << (7 - len)) - 1);
  for (size_t i = 1; i < len; i++)
    {
      unsigned int u = p[i];
      rc <<= 6;
      rc |= u & 0x3f;
    }
  *pc = rc;
  return len;
}

// Encode an identifier using ASCII characters.

static std::string
encode_id(const std::string id)
{
  std::string ret;
  const char* p = id.c_str();
  const char* pend = p + id.length();
  while (p < pend)
    {
      unsigned int c;
      size_t len = fetch_utf8_char(p, &c);
      if (len == 1 && !char_needs_encoding(c))
	ret += c;
      else
	{
	  ret += "$U";
	  char buf[30];
	  snprintf(buf, sizeof buf, "%x", c);
	  ret += buf;
	  ret += "$";
	}
      p += len;
    }
  return ret;
}

// Define the built-in functions that are exposed to GCCGo.

Gcc_backend::Gcc_backend()
{
  /* We need to define the fetch_and_add functions, since we use them
     for ++ and --.  */
  tree t = this->integer_type(BITS_PER_UNIT, 1)->get_tree();
  tree p = build_pointer_type(build_qualified_type(t, TYPE_QUAL_VOLATILE));
  this->define_builtin(BUILT_IN_SYNC_ADD_AND_FETCH_1, "__sync_fetch_and_add_1",
		       NULL, build_function_type_list(t, p, t, NULL_TREE),
		       false, false);

  t = this->integer_type(BITS_PER_UNIT * 2, 1)->get_tree();
  p = build_pointer_type(build_qualified_type(t, TYPE_QUAL_VOLATILE));
  this->define_builtin(BUILT_IN_SYNC_ADD_AND_FETCH_2, "__sync_fetch_and_add_2",
		       NULL, build_function_type_list(t, p, t, NULL_TREE),
		       false, false);

  t = this->integer_type(BITS_PER_UNIT * 4, 1)->get_tree();
  p = build_pointer_type(build_qualified_type(t, TYPE_QUAL_VOLATILE));
  this->define_builtin(BUILT_IN_SYNC_ADD_AND_FETCH_4, "__sync_fetch_and_add_4",
		       NULL, build_function_type_list(t, p, t, NULL_TREE),
		       false, false);

  t = this->integer_type(BITS_PER_UNIT * 8, 1)->get_tree();
  p = build_pointer_type(build_qualified_type(t, TYPE_QUAL_VOLATILE));
  this->define_builtin(BUILT_IN_SYNC_ADD_AND_FETCH_8, "__sync_fetch_and_add_8",
		       NULL, build_function_type_list(t, p, t, NULL_TREE),
		       false, false);

  // We use __builtin_expect for magic import functions.
  this->define_builtin(BUILT_IN_EXPECT, "__builtin_expect", NULL,
		       build_function_type_list(long_integer_type_node,
						long_integer_type_node,
						long_integer_type_node,
						NULL_TREE),
		       true, false);

  // We use __builtin_memcmp for struct comparisons.
  this->define_builtin(BUILT_IN_MEMCMP, "__builtin_memcmp", "memcmp",
		       build_function_type_list(integer_type_node,
						const_ptr_type_node,
						const_ptr_type_node,
						size_type_node,
						NULL_TREE),
		       false, false);

  // We provide some functions for the math library.
  tree math_function_type = build_function_type_list(double_type_node,
						     double_type_node,
						     NULL_TREE);
  tree math_function_type_long =
    build_function_type_list(long_double_type_node, long_double_type_node,
			     long_double_type_node, NULL_TREE);
  tree math_function_type_two = build_function_type_list(double_type_node,
							 double_type_node,
							 double_type_node,
							 NULL_TREE);
  tree math_function_type_long_two =
    build_function_type_list(long_double_type_node, long_double_type_node,
			     long_double_type_node, NULL_TREE);
  this->define_builtin(BUILT_IN_ACOS, "__builtin_acos", "acos",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_ACOSL, "__builtin_acosl", "acosl",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_ASIN, "__builtin_asin", "asin",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_ASINL, "__builtin_asinl", "asinl",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_ATAN, "__builtin_atan", "atan",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_ATANL, "__builtin_atanl", "atanl",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_ATAN2, "__builtin_atan2", "atan2",
		       math_function_type_two, true, false);
  this->define_builtin(BUILT_IN_ATAN2L, "__builtin_atan2l", "atan2l",
		       math_function_type_long_two, true, false);
  this->define_builtin(BUILT_IN_CEIL, "__builtin_ceil", "ceil",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_CEILL, "__builtin_ceill", "ceill",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_COS, "__builtin_cos", "cos",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_COSL, "__builtin_cosl", "cosl",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_EXP, "__builtin_exp", "exp",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_EXPL, "__builtin_expl", "expl",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_EXPM1, "__builtin_expm1", "expm1",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_EXPM1L, "__builtin_expm1l", "expm1l",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_FABS, "__builtin_fabs", "fabs",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_FABSL, "__builtin_fabsl", "fabsl",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_FLOOR, "__builtin_floor", "floor",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_FLOORL, "__builtin_floorl", "floorl",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_FMOD, "__builtin_fmod", "fmod",
		       math_function_type_two, true, false);
  this->define_builtin(BUILT_IN_FMODL, "__builtin_fmodl", "fmodl",
		       math_function_type_long_two, true, false);
  this->define_builtin(BUILT_IN_LDEXP, "__builtin_ldexp", "ldexp",
		       build_function_type_list(double_type_node,
						double_type_node,
						integer_type_node,
						NULL_TREE),
		       true, false);
  this->define_builtin(BUILT_IN_LDEXPL, "__builtin_ldexpl", "ldexpl",
		       build_function_type_list(long_double_type_node,
						long_double_type_node,
						integer_type_node,
						NULL_TREE),
		       true, false);
  this->define_builtin(BUILT_IN_LOG, "__builtin_log", "log",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_LOGL, "__builtin_logl", "logl",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_LOG1P, "__builtin_log1p", "log1p",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_LOG1PL, "__builtin_log1pl", "log1pl",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_LOG10, "__builtin_log10", "log10",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_LOG10L, "__builtin_log10l", "log10l",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_LOG2, "__builtin_log2", "log2",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_LOG2L, "__builtin_log2l", "log2l",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_SIN, "__builtin_sin", "sin",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_SINL, "__builtin_sinl", "sinl",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_SQRT, "__builtin_sqrt", "sqrt",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_SQRTL, "__builtin_sqrtl", "sqrtl",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_TAN, "__builtin_tan", "tan",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_TANL, "__builtin_tanl", "tanl",
		       math_function_type_long, true, false);
  this->define_builtin(BUILT_IN_TRUNC, "__builtin_trunc", "trunc",
		       math_function_type, true, false);
  this->define_builtin(BUILT_IN_TRUNCL, "__builtin_truncl", "truncl",
		       math_function_type_long, true, false);

  // We use __builtin_return_address in the thunk we build for
  // functions which call recover.
  this->define_builtin(BUILT_IN_RETURN_ADDRESS, "__builtin_return_address",
		       NULL,
		       build_function_type_list(ptr_type_node,
						unsigned_type_node,
						NULL_TREE),
		       false, false);

  // The compiler uses __builtin_trap for some exception handling
  // cases.
  this->define_builtin(BUILT_IN_TRAP, "__builtin_trap", NULL,
		       build_function_type(void_type_node, void_list_node),
		       false, true);
}

// Get an unnamed integer type.

Btype*
Gcc_backend::integer_type(bool is_unsigned, int bits)
{
  tree type;
  if (is_unsigned)
    {
      if (bits == INT_TYPE_SIZE)
        type = unsigned_type_node;
      else if (bits == CHAR_TYPE_SIZE)
        type = unsigned_char_type_node;
      else if (bits == SHORT_TYPE_SIZE)
        type = short_unsigned_type_node;
      else if (bits == LONG_TYPE_SIZE)
        type = long_unsigned_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
        type = long_long_unsigned_type_node;
      else
        type = make_unsigned_type(bits);
    }
  else
    {
      if (bits == INT_TYPE_SIZE)
        type = integer_type_node;
      else if (bits == CHAR_TYPE_SIZE)
        type = signed_char_type_node;
      else if (bits == SHORT_TYPE_SIZE)
        type = short_integer_type_node;
      else if (bits == LONG_TYPE_SIZE)
        type = long_integer_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
        type = long_long_integer_type_node;
      else
        type = make_signed_type(bits);
    }
  return this->make_type(type);
}

// Get an unnamed float type.

Btype*
Gcc_backend::float_type(int bits)
{
  tree type;
  if (bits == FLOAT_TYPE_SIZE)
    type = float_type_node;
  else if (bits == DOUBLE_TYPE_SIZE)
    type = double_type_node;
  else if (bits == LONG_DOUBLE_TYPE_SIZE)
    type = long_double_type_node;
  else
    {
      type = make_node(REAL_TYPE);
      TYPE_PRECISION(type) = bits;
      layout_type(type);
    }
  return this->make_type(type);
}

// Get an unnamed complex type.

Btype*
Gcc_backend::complex_type(int bits)
{
  tree type;
  if (bits == FLOAT_TYPE_SIZE * 2)
    type = complex_float_type_node;
  else if (bits == DOUBLE_TYPE_SIZE * 2)
    type = complex_double_type_node;
  else if (bits == LONG_DOUBLE_TYPE_SIZE * 2)
    type = complex_long_double_type_node;
  else
    {
      type = make_node(REAL_TYPE);
      TYPE_PRECISION(type) = bits / 2;
      layout_type(type);
      type = build_complex_type(type);
    }
  return this->make_type(type);
}

// Get a pointer type.

Btype*
Gcc_backend::pointer_type(Btype* to_type)
{
  tree to_type_tree = to_type->get_tree();
  if (to_type_tree == error_mark_node)
    return this->error_type();
  tree type = build_pointer_type(to_type_tree);
  return this->make_type(type);
}

// Make a function type.

Btype*
Gcc_backend::function_type(const Btyped_identifier& receiver,
			   const std::vector<Btyped_identifier>& parameters,
			   const std::vector<Btyped_identifier>& results,
			   Btype* result_struct,
			   Location)
{
  tree args = NULL_TREE;
  tree* pp = &args;
  if (receiver.btype != NULL)
    {
      tree t = receiver.btype->get_tree();
      if (t == error_mark_node)
	return this->error_type();
      *pp = tree_cons(NULL_TREE, t, NULL_TREE);
      pp = &TREE_CHAIN(*pp);
    }

  for (std::vector<Btyped_identifier>::const_iterator p = parameters.begin();
       p != parameters.end();
       ++p)
    {
      tree t = p->btype->get_tree();
      if (t == error_mark_node)
	return this->error_type();
      *pp = tree_cons(NULL_TREE, t, NULL_TREE);
      pp = &TREE_CHAIN(*pp);
    }

  // Varargs is handled entirely at the Go level.  When converted to
  // GENERIC functions are not varargs.
  *pp = void_list_node;

  tree result;
  if (results.empty())
    result = void_type_node;
  else if (results.size() == 1)
    result = results.front().btype->get_tree();
  else
    {
      gcc_assert(result_struct != NULL);
      result = result_struct->get_tree();
    }
  if (result == error_mark_node)
    return this->error_type();

  tree fntype = build_function_type(result, args);
  if (fntype == error_mark_node)
    return this->error_type();

  return this->make_type(build_pointer_type(fntype));
}

// Make a struct type.

Btype*
Gcc_backend::struct_type(const std::vector<Btyped_identifier>& fields)
{
  return this->fill_in_struct(this->make_type(make_node(RECORD_TYPE)), fields);
}

// Fill in the fields of a struct type.

Btype*
Gcc_backend::fill_in_struct(Btype* fill,
			    const std::vector<Btyped_identifier>& fields)
{
  tree fill_tree = fill->get_tree();
  tree field_trees = NULL_TREE;
  tree* pp = &field_trees;
  for (std::vector<Btyped_identifier>::const_iterator p = fields.begin();
       p != fields.end();
       ++p)
    {
      tree name_tree = get_identifier_from_string(p->name);
      tree type_tree = p->btype->get_tree();
      if (type_tree == error_mark_node)
	return this->error_type();
      tree field = build_decl(p->location.gcc_location(), FIELD_DECL, name_tree,
                              type_tree);
      DECL_CONTEXT(field) = fill_tree;
      *pp = field;
      pp = &DECL_CHAIN(field);
    }
  TYPE_FIELDS(fill_tree) = field_trees;
  layout_type(fill_tree);
  return fill;
}

// Make an array type.

Btype*
Gcc_backend::array_type(Btype* element_btype, Bexpression* length)
{
  return this->fill_in_array(this->make_type(make_node(ARRAY_TYPE)),
			     element_btype, length);
}

// Fill in an array type.

Btype*
Gcc_backend::fill_in_array(Btype* fill, Btype* element_type,
			   Bexpression* length)
{
  tree element_type_tree = element_type->get_tree();
  tree length_tree = length->get_tree();
  if (element_type_tree == error_mark_node || length_tree == error_mark_node)
    return this->error_type();

  gcc_assert(TYPE_SIZE(element_type_tree) != NULL_TREE);

  length_tree = fold_convert(sizetype, length_tree);

  // build_index_type takes the maximum index, which is one less than
  // the length.
  tree index_type_tree = build_index_type(fold_build2(MINUS_EXPR, sizetype,
						      length_tree,
						      size_one_node));

  tree fill_tree = fill->get_tree();
  TREE_TYPE(fill_tree) = element_type_tree;
  TYPE_DOMAIN(fill_tree) = index_type_tree;
  TYPE_ADDR_SPACE(fill_tree) = TYPE_ADDR_SPACE(element_type_tree);
  layout_type(fill_tree);

  if (TYPE_STRUCTURAL_EQUALITY_P(element_type_tree))
    SET_TYPE_STRUCTURAL_EQUALITY(fill_tree);
  else if (TYPE_CANONICAL(element_type_tree) != element_type_tree
	   || TYPE_CANONICAL(index_type_tree) != index_type_tree)
    TYPE_CANONICAL(fill_tree) =
      build_array_type(TYPE_CANONICAL(element_type_tree),
		       TYPE_CANONICAL(index_type_tree));

  return fill;
}

// Create a placeholder for a pointer type.

Btype*
Gcc_backend::placeholder_pointer_type(const std::string& name,
				      Location location, bool)
{
  tree ret = build_distinct_type_copy(ptr_type_node);
  if (!name.empty())
    {
      tree decl = build_decl(location.gcc_location(), TYPE_DECL,
			     get_identifier_from_string(name),
			     ret);
      TYPE_NAME(ret) = decl;
    }
  return this->make_type(ret);
}

// Set the real target type for a placeholder pointer type.

bool
Gcc_backend::set_placeholder_pointer_type(Btype* placeholder,
					  Btype* to_type)
{
  tree pt = placeholder->get_tree();
  if (pt == error_mark_node)
    return false;
  gcc_assert(TREE_CODE(pt) == POINTER_TYPE);
  tree tt = to_type->get_tree();
  if (tt == error_mark_node)
    {
      placeholder->set_tree(error_mark_node);
      return false;
    }
  gcc_assert(TREE_CODE(tt) == POINTER_TYPE);
  TREE_TYPE(pt) = TREE_TYPE(tt);
  if (TYPE_NAME(pt) != NULL_TREE)
    {
      // Build the data structure gcc wants to see for a typedef.
      tree copy = build_variant_type_copy(pt);
      TYPE_NAME(copy) = NULL_TREE;
      DECL_ORIGINAL_TYPE(TYPE_NAME(pt)) = copy;
    }
  return true;
}

// Set the real values for a placeholder function type.

bool
Gcc_backend::set_placeholder_function_type(Btype* placeholder, Btype* ft)
{
  return this->set_placeholder_pointer_type(placeholder, ft);
}

// Create a placeholder for a struct type.

Btype*
Gcc_backend::placeholder_struct_type(const std::string& name,
				     Location location)
{
  tree ret = make_node(RECORD_TYPE);
  if (!name.empty())
    {
      tree decl = build_decl(location.gcc_location(), TYPE_DECL,
			     get_identifier_from_string(name),
			     ret);
      TYPE_NAME(ret) = decl;
    }
  return this->make_type(ret);
}

// Fill in the fields of a placeholder struct type.

bool
Gcc_backend::set_placeholder_struct_type(
    Btype* placeholder,
    const std::vector<Btyped_identifier>& fields)
{
  tree t = placeholder->get_tree();
  gcc_assert(TREE_CODE(t) == RECORD_TYPE && TYPE_FIELDS(t) == NULL_TREE);
  Btype* r = this->fill_in_struct(placeholder, fields);

  if (TYPE_NAME(t) != NULL_TREE)
    {
      // Build the data structure gcc wants to see for a typedef.
      tree copy = build_distinct_type_copy(t);
      TYPE_NAME(copy) = NULL_TREE;
      DECL_ORIGINAL_TYPE(TYPE_NAME(t)) = copy;
    }

  return r->get_tree() != error_mark_node;
}

// Create a placeholder for an array type.

Btype*
Gcc_backend::placeholder_array_type(const std::string& name,
				    Location location)
{
  tree ret = make_node(ARRAY_TYPE);
  tree decl = build_decl(location.gcc_location(), TYPE_DECL,
			 get_identifier_from_string(name),
			 ret);
  TYPE_NAME(ret) = decl;
  return this->make_type(ret);
}

// Fill in the fields of a placeholder array type.

bool
Gcc_backend::set_placeholder_array_type(Btype* placeholder,
					Btype* element_btype,
					Bexpression* length)
{
  tree t = placeholder->get_tree();
  gcc_assert(TREE_CODE(t) == ARRAY_TYPE && TREE_TYPE(t) == NULL_TREE);
  Btype* r = this->fill_in_array(placeholder, element_btype, length);

  // Build the data structure gcc wants to see for a typedef.
  tree copy = build_distinct_type_copy(t);
  TYPE_NAME(copy) = NULL_TREE;
  DECL_ORIGINAL_TYPE(TYPE_NAME(t)) = copy;

  return r->get_tree() != error_mark_node;
}

// Return a named version of a type.

Btype*
Gcc_backend::named_type(const std::string& name, Btype* btype,
			Location location)
{
  tree type = btype->get_tree();
  if (type == error_mark_node)
    return this->error_type();

  // The middle-end expects a basic type to have a name.  In Go every
  // basic type will have a name.  The first time we see a basic type,
  // give it whatever Go name we have at this point.
  if (TYPE_NAME(type) == NULL_TREE
      && location.gcc_location() == BUILTINS_LOCATION
      && (TREE_CODE(type) == INTEGER_TYPE
	  || TREE_CODE(type) == REAL_TYPE
	  || TREE_CODE(type) == COMPLEX_TYPE
	  || TREE_CODE(type) == BOOLEAN_TYPE))
    {
      tree decl = build_decl(BUILTINS_LOCATION, TYPE_DECL,
			     get_identifier_from_string(name),
			     type);
      TYPE_NAME(type) = decl;
      return this->make_type(type);
    }

  tree copy = build_variant_type_copy(type);
  tree decl = build_decl(location.gcc_location(), TYPE_DECL,
			 get_identifier_from_string(name),
			 copy);
  DECL_ORIGINAL_TYPE(decl) = type;
  TYPE_NAME(copy) = decl;
  return this->make_type(copy);
}

// Return a pointer type used as a marker for a circular type.

Btype*
Gcc_backend::circular_pointer_type(Btype*, bool)
{
  return this->make_type(ptr_type_node);
}

// Return whether we might be looking at a circular type.

bool
Gcc_backend::is_circular_pointer_type(Btype* btype)
{
  return btype->get_tree() == ptr_type_node;
}

// Return the size of a type.

int64_t
Gcc_backend::type_size(Btype* btype)
{
  tree t = btype->get_tree();
  if (t == error_mark_node)
    return 1;
  t = TYPE_SIZE_UNIT(t);
  gcc_assert(tree_fits_uhwi_p (t));
  unsigned HOST_WIDE_INT val_wide = TREE_INT_CST_LOW(t);
  int64_t ret = static_cast<int64_t>(val_wide);
  if (ret < 0 || static_cast<unsigned HOST_WIDE_INT>(ret) != val_wide)
    return -1;
  return ret;
}

// Return the alignment of a type.

int64_t
Gcc_backend::type_alignment(Btype* btype)
{
  tree t = btype->get_tree();
  if (t == error_mark_node)
    return 1;
  return TYPE_ALIGN_UNIT(t);
}

// Return the alignment of a struct field of type BTYPE.

int64_t
Gcc_backend::type_field_alignment(Btype* btype)
{
  tree t = btype->get_tree();
  if (t == error_mark_node)
    return 1;
  return go_field_alignment(t);
}

// Return the offset of a field in a struct.

int64_t
Gcc_backend::type_field_offset(Btype* btype, size_t index)
{
  tree struct_tree = btype->get_tree();
  if (struct_tree == error_mark_node)
    return 0;
  gcc_assert(TREE_CODE(struct_tree) == RECORD_TYPE);
  tree field = TYPE_FIELDS(struct_tree);
  for (; index > 0; --index)
    {
      field = DECL_CHAIN(field);
      gcc_assert(field != NULL_TREE);
    }
  HOST_WIDE_INT offset_wide = int_byte_position(field);
  int64_t ret = static_cast<int64_t>(offset_wide);
  gcc_assert(ret == offset_wide);
  return ret;
}

// Return the zero value for a type.

Bexpression*
Gcc_backend::zero_expression(Btype* btype)
{
  tree t = btype->get_tree();
  tree ret;
  if (t == error_mark_node)
    ret = error_mark_node;
  else
    ret = build_zero_cst(t);
  return this->make_expression(ret);
}

// An expression that references a variable.

Bexpression*
Gcc_backend::var_expression(Bvariable* var, Location location)
{
  tree ret = var->get_tree(location);
  if (ret == error_mark_node)
    return this->error_expression();
  return this->make_expression(ret);
}

// An expression that indirectly references an expression.

Bexpression*
Gcc_backend::indirect_expression(Btype* btype, Bexpression* expr,
				 bool known_valid, Location location)
{
  tree expr_tree = expr->get_tree();
  tree type_tree = btype->get_tree();
  if (expr_tree == error_mark_node || type_tree == error_mark_node)
    return this->error_expression();

  // If the type of EXPR is a recursive pointer type, then we
  // need to insert a cast before indirecting.
  tree target_type_tree = TREE_TYPE(TREE_TYPE(expr_tree));
  if (VOID_TYPE_P(target_type_tree))
    expr_tree = fold_convert_loc(location.gcc_location(),
				 build_pointer_type(type_tree), expr_tree);

  tree ret = build_fold_indirect_ref_loc(location.gcc_location(),
                                         expr_tree);
  if (known_valid)
    TREE_THIS_NOTRAP(ret) = 1;
  return this->make_expression(ret);
}

// Return an expression that declares a constant named NAME with the
// constant value VAL in BTYPE.

Bexpression*
Gcc_backend::named_constant_expression(Btype* btype, const std::string& name,
				       Bexpression* val, Location location)
{
  tree type_tree = btype->get_tree();
  tree const_val = val->get_tree();
  if (type_tree == error_mark_node || const_val == error_mark_node)
    return this->error_expression();

  tree name_tree = get_identifier_from_string(name);
  tree decl = build_decl(location.gcc_location(), CONST_DECL, name_tree,
			 type_tree);
  DECL_INITIAL(decl) = const_val;
  TREE_CONSTANT(decl) = 1;
  TREE_READONLY(decl) = 1;

  go_preserve_from_gc(decl);
  return this->make_expression(decl);
}

// Return a typed value as a constant integer.

Bexpression*
Gcc_backend::integer_constant_expression(Btype* btype, mpz_t val)
{
  tree t = btype->get_tree();
  if (t == error_mark_node)
    return this->error_expression();

  tree ret = double_int_to_tree(t, mpz_get_double_int(t, val, true));
  return this->make_expression(ret);
}

// Return a typed value as a constant floating-point number.

Bexpression*
Gcc_backend::float_constant_expression(Btype* btype, mpfr_t val)
{
  tree t = btype->get_tree();
  tree ret;
  if (t == error_mark_node)
    return this->error_expression();

  REAL_VALUE_TYPE r1;
  real_from_mpfr(&r1, val, t, GMP_RNDN);
  REAL_VALUE_TYPE r2;
  real_convert(&r2, TYPE_MODE(t), &r1);
  ret = build_real(t, r2);
  return this->make_expression(ret);
}

// Return a typed real and imaginary value as a constant complex number.

Bexpression*
Gcc_backend::complex_constant_expression(Btype* btype, mpc_t val)
{
  tree t = btype->get_tree();
  tree ret;
  if (t == error_mark_node)
    return this->error_expression();

  REAL_VALUE_TYPE r1;
  real_from_mpfr(&r1, mpc_realref(val), TREE_TYPE(t), GMP_RNDN);
  REAL_VALUE_TYPE r2;
  real_convert(&r2, TYPE_MODE(TREE_TYPE(t)), &r1);

  REAL_VALUE_TYPE r3;
  real_from_mpfr(&r3, mpc_imagref(val), TREE_TYPE(t), GMP_RNDN);
  REAL_VALUE_TYPE r4;
  real_convert(&r4, TYPE_MODE(TREE_TYPE(t)), &r3);

  ret = build_complex(t, build_real(TREE_TYPE(t), r2),
                      build_real(TREE_TYPE(t), r4));
  return this->make_expression(ret);
}

// Make a constant string expression.

Bexpression*
Gcc_backend::string_constant_expression(const std::string& val)
{
  tree index_type = build_index_type(size_int(val.length()));
  tree const_char_type = build_qualified_type(unsigned_char_type_node,
					      TYPE_QUAL_CONST);
  tree string_type = build_array_type(const_char_type, index_type);
  TYPE_STRING_FLAG(string_type) = 1;
  tree string_val = build_string(val.length(), val.data());
  TREE_TYPE(string_val) = string_type;

  return this->make_expression(string_val);
}

// Make a constant boolean expression.

Bexpression*
Gcc_backend::boolean_constant_expression(bool val)
{
  tree bool_cst = val ? boolean_true_node : boolean_false_node;
  return this->make_expression(bool_cst);
}

// Return the real part of a complex expression.

Bexpression*
Gcc_backend::real_part_expression(Bexpression* bcomplex, Location location)
{
  tree complex_tree = bcomplex->get_tree();
  if (complex_tree == error_mark_node)
    return this->error_expression();
  gcc_assert(COMPLEX_FLOAT_TYPE_P(TREE_TYPE(complex_tree)));
  tree ret = fold_build1_loc(location.gcc_location(), REALPART_EXPR,
                             TREE_TYPE(TREE_TYPE(complex_tree)),
                             complex_tree);
  return this->make_expression(ret);
}

// Return the imaginary part of a complex expression.

Bexpression*
Gcc_backend::imag_part_expression(Bexpression* bcomplex, Location location)
{
  tree complex_tree = bcomplex->get_tree();
  if (complex_tree == error_mark_node)
    return this->error_expression();
  gcc_assert(COMPLEX_FLOAT_TYPE_P(TREE_TYPE(complex_tree)));
  tree ret = fold_build1_loc(location.gcc_location(), IMAGPART_EXPR,
                             TREE_TYPE(TREE_TYPE(complex_tree)),
                             complex_tree);
  return this->make_expression(ret);
}

// Make a complex expression given its real and imaginary parts.

Bexpression*
Gcc_backend::complex_expression(Bexpression* breal, Bexpression* bimag,
                                Location location)
{
  tree real_tree = breal->get_tree();
  tree imag_tree = bimag->get_tree();
  if (real_tree == error_mark_node || imag_tree == error_mark_node)
    return this->error_expression();
  gcc_assert(TYPE_MAIN_VARIANT(TREE_TYPE(real_tree))
            == TYPE_MAIN_VARIANT(TREE_TYPE(imag_tree)));
  gcc_assert(SCALAR_FLOAT_TYPE_P(TREE_TYPE(real_tree)));
  tree ret = fold_build2_loc(location.gcc_location(), COMPLEX_EXPR,
                             build_complex_type(TREE_TYPE(real_tree)),
                             real_tree, imag_tree);
  return this->make_expression(ret);
}

// An expression that converts an expression to a different type.

Bexpression*
Gcc_backend::convert_expression(Btype* type, Bexpression* expr,
				Location location)
{
  tree type_tree = type->get_tree();
  tree expr_tree = expr->get_tree();
  if (type_tree == error_mark_node
      || expr_tree == error_mark_node
      || TREE_TYPE(expr_tree) == error_mark_node)
    return this->error_expression();

  tree ret;
  if (this->type_size(type) == 0)
    {
      // Do not convert zero-sized types.
      ret = expr_tree;
    }
  else if (TREE_CODE(type_tree) == INTEGER_TYPE)
    ret = fold(convert_to_integer(type_tree, expr_tree));
  else if (TREE_CODE(type_tree) == REAL_TYPE)
    ret = fold(convert_to_real(type_tree, expr_tree));
  else if (TREE_CODE(type_tree) == COMPLEX_TYPE)
    ret = fold(convert_to_complex(type_tree, expr_tree));
  else if (TREE_CODE(type_tree) == POINTER_TYPE
           && TREE_CODE(TREE_TYPE(expr_tree)) == INTEGER_TYPE)
    ret = fold(convert_to_pointer(type_tree, expr_tree));
  else if (TREE_CODE(type_tree) == RECORD_TYPE
           || TREE_CODE(type_tree) == ARRAY_TYPE)
    ret = fold_build1_loc(location.gcc_location(), VIEW_CONVERT_EXPR,
                          type_tree, expr_tree);
  else
    ret = fold_convert_loc(location.gcc_location(), type_tree, expr_tree);

  return this->make_expression(ret);
}

// Get the address of a function.

Bexpression*
Gcc_backend::function_code_expression(Bfunction* bfunc, Location location)
{
  tree func = bfunc->get_tree();
  if (func == error_mark_node)
    return this->error_expression();

  tree ret = build_fold_addr_expr_loc(location.gcc_location(), func);
  return this->make_expression(ret);
}

// Get the address of an expression.

Bexpression*
Gcc_backend::address_expression(Bexpression* bexpr, Location location)
{
  tree expr = bexpr->get_tree();
  if (expr == error_mark_node)
    return this->error_expression();

  tree ret = build_fold_addr_expr_loc(location.gcc_location(), expr);
  return this->make_expression(ret);
}

// Return an expression for the field at INDEX in BSTRUCT.

Bexpression*
Gcc_backend::struct_field_expression(Bexpression* bstruct, size_t index,
                                     Location location)
{
  tree struct_tree = bstruct->get_tree();
  if (struct_tree == error_mark_node
      || TREE_TYPE(struct_tree) == error_mark_node)
    return this->error_expression();
  gcc_assert(TREE_CODE(TREE_TYPE(struct_tree)) == RECORD_TYPE);
  tree field = TYPE_FIELDS(TREE_TYPE(struct_tree));
  if (field == NULL_TREE)
  {
    // This can happen for a type which refers to itself indirectly
    // and then turns out to be erroneous.
    return this->error_expression();
  }
  for (unsigned int i = index; i > 0; --i)
  {
    field = DECL_CHAIN(field);
    gcc_assert(field != NULL_TREE);
  }
  if (TREE_TYPE(field) == error_mark_node)
    return this->error_expression();
  tree ret = fold_build3_loc(location.gcc_location(), COMPONENT_REF,
                             TREE_TYPE(field), struct_tree, field,
                             NULL_TREE);
  if (TREE_CONSTANT(struct_tree))
    TREE_CONSTANT(ret) = 1;
  return this->make_expression(ret);
}

// Return an expression that executes BSTAT before BEXPR.

Bexpression*
Gcc_backend::compound_expression(Bstatement* bstat, Bexpression* bexpr,
                                 Location location)
{
  tree stat = bstat->get_tree();
  tree expr = bexpr->get_tree();
  if (stat == error_mark_node || expr == error_mark_node)
    return this->error_expression();
  tree ret = fold_build2_loc(location.gcc_location(), COMPOUND_EXPR,
                             TREE_TYPE(expr), stat, expr);
  return this->make_expression(ret);
}

// Return an expression that executes THEN_EXPR if CONDITION is true, or
// ELSE_EXPR otherwise.

Bexpression*
Gcc_backend::conditional_expression(Btype* btype, Bexpression* condition,
                                    Bexpression* then_expr,
                                    Bexpression* else_expr, Location location)
{
  tree type_tree = btype == NULL ? void_type_node : btype->get_tree();
  tree cond_tree = condition->get_tree();
  tree then_tree = then_expr->get_tree();
  tree else_tree = else_expr == NULL ? NULL_TREE : else_expr->get_tree();
  if (type_tree == error_mark_node
      || cond_tree == error_mark_node
      || then_tree == error_mark_node
      || else_tree == error_mark_node)
    return this->error_expression();
  tree ret = build3_loc(location.gcc_location(), COND_EXPR, type_tree,
                        cond_tree, then_tree, else_tree);
  return this->make_expression(ret);
}

// Return an expression for the unary operation OP EXPR.

Bexpression*
Gcc_backend::unary_expression(Operator op, Bexpression* expr, Location location)
{
  tree expr_tree = expr->get_tree();
  if (expr_tree == error_mark_node
      || TREE_TYPE(expr_tree) == error_mark_node)
    return this->error_expression();

  tree type_tree = TREE_TYPE(expr_tree);
  enum tree_code code;
  switch (op)
    {
    case OPERATOR_MINUS:
      {
        tree computed_type = excess_precision_type(type_tree);
        if (computed_type != NULL_TREE)
          {
            expr_tree = convert(computed_type, expr_tree);
            type_tree = computed_type;
          }
        code = NEGATE_EXPR;
        break;
      }
    case OPERATOR_NOT:
      code = TRUTH_NOT_EXPR;
      break;
    case OPERATOR_XOR:
      code = BIT_NOT_EXPR;
      break;
    default:
      gcc_unreachable();
      break;
    }

  tree ret = fold_build1_loc(location.gcc_location(), code, type_tree,
                             expr_tree);
  return this->make_expression(ret);
}

// Convert a gofrontend operator to an equivalent tree_code.

static enum tree_code
operator_to_tree_code(Operator op, tree type)
{
  enum tree_code code;
  switch (op)
    {
    case OPERATOR_EQEQ:
      code = EQ_EXPR;
      break;
    case OPERATOR_NOTEQ:
      code = NE_EXPR;
      break;
    case OPERATOR_LT:
      code = LT_EXPR;
      break;
    case OPERATOR_LE:
      code = LE_EXPR;
      break;
    case OPERATOR_GT:
      code = GT_EXPR;
      break;
    case OPERATOR_GE:
      code = GE_EXPR;
      break;
    case OPERATOR_OROR:
      code = TRUTH_ORIF_EXPR;
      break;
    case OPERATOR_ANDAND:
      code = TRUTH_ANDIF_EXPR;
      break;
    case OPERATOR_PLUS:
      code = PLUS_EXPR;
      break;
    case OPERATOR_MINUS:
      code = MINUS_EXPR;
      break;
    case OPERATOR_OR:
      code = BIT_IOR_EXPR;
      break;
    case OPERATOR_XOR:
      code = BIT_XOR_EXPR;
      break;
    case OPERATOR_MULT:
      code = MULT_EXPR;
      break;
    case OPERATOR_DIV:
      if (TREE_CODE(type) == REAL_TYPE || TREE_CODE(type) == COMPLEX_TYPE)
	code = RDIV_EXPR;
      else
	code = TRUNC_DIV_EXPR;
      break;
    case OPERATOR_MOD:
      code = TRUNC_MOD_EXPR;
      break;
    case OPERATOR_LSHIFT:
      code = LSHIFT_EXPR;
      break;
    case OPERATOR_RSHIFT:
      code = RSHIFT_EXPR;
      break;
    case OPERATOR_AND:
      code = BIT_AND_EXPR;
      break;
    case OPERATOR_BITCLEAR:
      code = BIT_AND_EXPR;
      break;
    default:
      gcc_unreachable();
    }

  return code;
}

// Return an expression for the binary operation LEFT OP RIGHT.

Bexpression*
Gcc_backend::binary_expression(Operator op, Bexpression* left,
                               Bexpression* right, Location location)
{
  tree left_tree = left->get_tree();
  tree right_tree = right->get_tree();
  if (left_tree == error_mark_node
      || right_tree == error_mark_node)
    return this->error_expression();
  enum tree_code code = operator_to_tree_code(op, TREE_TYPE(left_tree));

  bool use_left_type = op != OPERATOR_OROR && op != OPERATOR_ANDAND;
  tree type_tree = use_left_type ? TREE_TYPE(left_tree) : TREE_TYPE(right_tree);
  tree computed_type = excess_precision_type(type_tree);
  if (computed_type != NULL_TREE)
    {
      left_tree = convert(computed_type, left_tree);
      right_tree = convert(computed_type, right_tree);
      type_tree = computed_type;
    }

  // For comparison operators, the resulting type should be boolean.
  switch (op)
    {
    case OPERATOR_EQEQ:
    case OPERATOR_NOTEQ:
    case OPERATOR_LT:
    case OPERATOR_LE:
    case OPERATOR_GT:
    case OPERATOR_GE:
      type_tree = boolean_type_node;
      break;
    default:
      break;
    }

  tree ret = fold_build2_loc(location.gcc_location(), code, type_tree,
                             left_tree, right_tree);
  return this->make_expression(ret);
}

// Return an expression that constructs BTYPE with VALS.

Bexpression*
Gcc_backend::constructor_expression(Btype* btype,
                                    const std::vector<Bexpression*>& vals,
                                    Location location)
{
  tree type_tree = btype->get_tree();
  if (type_tree == error_mark_node)
    return this->error_expression();

  vec<constructor_elt, va_gc> *init;
  vec_alloc(init, vals.size());

  tree sink = NULL_TREE;
  bool is_constant = true;
  tree field = TYPE_FIELDS(type_tree);
  for (std::vector<Bexpression*>::const_iterator p = vals.begin();
       p != vals.end();
       ++p, field = DECL_CHAIN(field))
    {
      gcc_assert(field != NULL_TREE);
      tree val = (*p)->get_tree();
      if (TREE_TYPE(field) == error_mark_node
          || val == error_mark_node
          || TREE_TYPE(val) == error_mark_node)
        return this->error_expression();

      if (int_size_in_bytes(TREE_TYPE(field)) == 0)
	{
	  // GIMPLE cannot represent indices of zero-sized types so
	  // trying to construct a map with zero-sized keys might lead
	  // to errors.  Instead, we evaluate each expression that
	  // would have been added as a map element for its
	  // side-effects and construct an empty map.
	  append_to_statement_list(val, &sink);
	  continue;
	}

      constructor_elt empty = {NULL, NULL};
      constructor_elt* elt = init->quick_push(empty);
      elt->index = field;
      elt->value = fold_convert_loc(location.gcc_location(), TREE_TYPE(field),
                                    val);
      if (!TREE_CONSTANT(elt->value))
	is_constant = false;
    }
  gcc_assert(field == NULL_TREE);
  tree ret = build_constructor(type_tree, init);
  if (is_constant)
    TREE_CONSTANT(ret) = 1;
  if (sink != NULL_TREE)
    ret = fold_build2_loc(location.gcc_location(), COMPOUND_EXPR,
			  type_tree, sink, ret);
  return this->make_expression(ret);
}

Bexpression*
Gcc_backend::array_constructor_expression(
    Btype* array_btype, const std::vector<unsigned long>& indexes,
    const std::vector<Bexpression*>& vals, Location location)
{
  tree type_tree = array_btype->get_tree();
  if (type_tree == error_mark_node)
    return this->error_expression();

  gcc_assert(indexes.size() == vals.size());

  tree element_type = TREE_TYPE(type_tree);
  HOST_WIDE_INT element_size = int_size_in_bytes(element_type);
  vec<constructor_elt, va_gc> *init;
  vec_alloc(init, element_size == 0 ? 0 : vals.size());

  tree sink = NULL_TREE;
  bool is_constant = true;
  for (size_t i = 0; i < vals.size(); ++i)
    {
      tree index = size_int(indexes[i]);
      tree val = (vals[i])->get_tree();

      if (index == error_mark_node
          || val == error_mark_node)
        return this->error_expression();

      if (element_size == 0)
       {
         // GIMPLE cannot represent arrays of zero-sized types so trying
         // to construct an array of zero-sized values might lead to errors.
         // Instead, we evaluate each expression that would have been added as
         // an array value for its side-effects and construct an empty array.
	 append_to_statement_list(val, &sink);
         continue;
       }

      if (!TREE_CONSTANT(val))
        is_constant = false;

      constructor_elt empty = {NULL, NULL};
      constructor_elt* elt = init->quick_push(empty);
      elt->index = index;
      elt->value = val;
    }

  tree ret = build_constructor(type_tree, init);
  if (is_constant)
    TREE_CONSTANT(ret) = 1;
  if (sink != NULL_TREE)
    ret = fold_build2_loc(location.gcc_location(), COMPOUND_EXPR,
                         type_tree, sink, ret);
  return this->make_expression(ret);
}

// Return an expression for the address of BASE[INDEX].

Bexpression*
Gcc_backend::pointer_offset_expression(Bexpression* base, Bexpression* index,
                                       Location location)
{
  tree base_tree = base->get_tree();
  tree index_tree = index->get_tree();
  tree element_type_tree = TREE_TYPE(TREE_TYPE(base_tree));
  if (base_tree == error_mark_node
      || TREE_TYPE(base_tree) == error_mark_node
      || index_tree == error_mark_node
      || element_type_tree == error_mark_node)
    return this->error_expression();

  tree element_size = TYPE_SIZE_UNIT(element_type_tree);
  index_tree = fold_convert_loc(location.gcc_location(), sizetype, index_tree);
  tree offset = fold_build2_loc(location.gcc_location(), MULT_EXPR, sizetype,
                                index_tree, element_size);
  tree ptr = fold_build2_loc(location.gcc_location(), POINTER_PLUS_EXPR,
                             TREE_TYPE(base_tree), base_tree, offset);
  return this->make_expression(ptr);
}

// Return an expression representing ARRAY[INDEX]

Bexpression*
Gcc_backend::array_index_expression(Bexpression* array, Bexpression* index,
                                    Location location)
{
  tree array_tree = array->get_tree();
  tree index_tree = index->get_tree();
  if (array_tree == error_mark_node
      || TREE_TYPE(array_tree) == error_mark_node
      || index_tree == error_mark_node)
    return this->error_expression();

  tree ret = build4_loc(location.gcc_location(), ARRAY_REF,
			TREE_TYPE(TREE_TYPE(array_tree)), array_tree,
                        index_tree, NULL_TREE, NULL_TREE);
  return this->make_expression(ret);
}

// Create an expression for a call to FN_EXPR with FN_ARGS.
Bexpression*
Gcc_backend::call_expression(Bexpression* fn_expr,
                             const std::vector<Bexpression*>& fn_args,
                             Bexpression* chain_expr, Location location)
{
  tree fn = fn_expr->get_tree();
  if (fn == error_mark_node || TREE_TYPE(fn) == error_mark_node)
    return this->error_expression();

  gcc_assert(FUNCTION_POINTER_TYPE_P(TREE_TYPE(fn)));
  tree rettype = TREE_TYPE(TREE_TYPE(TREE_TYPE(fn)));

  size_t nargs = fn_args.size();
  tree* args = nargs == 0 ? NULL : new tree[nargs];
  for (size_t i = 0; i < nargs; ++i)
    {
      args[i] = fn_args.at(i)->get_tree();
      if (args[i] == error_mark_node)
        return this->error_expression();
    }

  tree fndecl = fn;
  if (TREE_CODE(fndecl) == ADDR_EXPR)
    fndecl = TREE_OPERAND(fndecl, 0);

  // This is to support builtin math functions when using 80387 math.
  tree excess_type = NULL_TREE;
  if (optimize
      && TREE_CODE(fndecl) == FUNCTION_DECL
      && DECL_IS_BUILTIN(fndecl)
      && DECL_BUILT_IN_CLASS(fndecl) == BUILT_IN_NORMAL
      && nargs > 0
      && ((SCALAR_FLOAT_TYPE_P(rettype)
	   && SCALAR_FLOAT_TYPE_P(TREE_TYPE(args[0])))
	  || (COMPLEX_FLOAT_TYPE_P(rettype)
	      && COMPLEX_FLOAT_TYPE_P(TREE_TYPE(args[0])))))
    {
      excess_type = excess_precision_type(TREE_TYPE(args[0]));
      if (excess_type != NULL_TREE)
	{
	  tree excess_fndecl = mathfn_built_in(excess_type,
					       DECL_FUNCTION_CODE(fndecl));
	  if (excess_fndecl == NULL_TREE)
	    excess_type = NULL_TREE;
	  else
	    {
	      fn = build_fold_addr_expr_loc(location.gcc_location(),
                                            excess_fndecl);
	      for (size_t i = 0; i < nargs; ++i)
		{
		  if (SCALAR_FLOAT_TYPE_P(TREE_TYPE(args[i]))
		      || COMPLEX_FLOAT_TYPE_P(TREE_TYPE(args[i])))
		    args[i] = ::convert(excess_type, args[i]);
		}
	    }
	}
    }

  tree ret =
      build_call_array_loc(location.gcc_location(),
                           excess_type != NULL_TREE ? excess_type : rettype,
                           fn, nargs, args);

  if (chain_expr)
    CALL_EXPR_STATIC_CHAIN (ret) = chain_expr->get_tree();

  if (excess_type != NULL_TREE)
    {
      // Calling convert here can undo our excess precision change.
      // That may or may not be a bug in convert_to_real.
      ret = build1_loc(location.gcc_location(), NOP_EXPR, rettype, ret);
    }

  delete[] args;
  return this->make_expression(ret);
}

// Return an expression that allocates SIZE bytes on the stack.

Bexpression*
Gcc_backend::stack_allocation_expression(int64_t size, Location location)
{
  tree alloca = builtin_decl_explicit(BUILT_IN_ALLOCA);
  tree size_tree = build_int_cst(integer_type_node, size);
  tree ret = build_call_expr_loc(location.gcc_location(), alloca, 1, size_tree);
  return this->make_expression(ret);
}

// An expression as a statement.

Bstatement*
Gcc_backend::expression_statement(Bexpression* expr)
{
  return this->make_statement(expr->get_tree());
}

// Variable initialization.

Bstatement*
Gcc_backend::init_statement(Bvariable* var, Bexpression* init)
{
  tree var_tree = var->get_decl();
  tree init_tree = init->get_tree();
  if (var_tree == error_mark_node || init_tree == error_mark_node)
    return this->error_statement();
  gcc_assert(TREE_CODE(var_tree) == VAR_DECL);

  // To avoid problems with GNU ld, we don't make zero-sized
  // externally visible variables.  That might lead us to doing an
  // initialization of a zero-sized expression to a non-zero sized
  // variable, or vice-versa.  Avoid crashes by omitting the
  // initializer.  Such initializations don't mean anything anyhow.
  if (int_size_in_bytes(TREE_TYPE(var_tree)) != 0
      && init_tree != NULL_TREE
      && int_size_in_bytes(TREE_TYPE(init_tree)) != 0)
    {
      DECL_INITIAL(var_tree) = init_tree;
      init_tree = NULL_TREE;
    }

  tree ret = build1_loc(DECL_SOURCE_LOCATION(var_tree), DECL_EXPR,
			void_type_node, var_tree);
  if (init_tree != NULL_TREE)
    ret = build2_loc(DECL_SOURCE_LOCATION(var_tree), COMPOUND_EXPR,
		     void_type_node, init_tree, ret);

  return this->make_statement(ret);
}

// Assignment.

Bstatement*
Gcc_backend::assignment_statement(Bexpression* lhs, Bexpression* rhs,
				  Location location)
{
  tree lhs_tree = lhs->get_tree();
  tree rhs_tree = rhs->get_tree();
  if (lhs_tree == error_mark_node || rhs_tree == error_mark_node)
    return this->error_statement();

  // To avoid problems with GNU ld, we don't make zero-sized
  // externally visible variables.  That might lead us to doing an
  // assignment of a zero-sized expression to a non-zero sized
  // expression; avoid crashes here by avoiding assignments of
  // zero-sized expressions.  Such assignments don't really mean
  // anything anyhow.
  if (int_size_in_bytes(TREE_TYPE(lhs_tree)) == 0
      || int_size_in_bytes(TREE_TYPE(rhs_tree)) == 0)
    return this->compound_statement(this->expression_statement(lhs),
				    this->expression_statement(rhs));

  // Sometimes the same unnamed Go type can be created multiple times
  // and thus have multiple tree representations.  Make sure this does
  // not confuse the middle-end.
  if (TREE_TYPE(lhs_tree) != TREE_TYPE(rhs_tree))
    {
      tree lhs_type_tree = TREE_TYPE(lhs_tree);
      gcc_assert(TREE_CODE(lhs_type_tree) == TREE_CODE(TREE_TYPE(rhs_tree)));
      if (POINTER_TYPE_P(lhs_type_tree)
	  || INTEGRAL_TYPE_P(lhs_type_tree)
	  || SCALAR_FLOAT_TYPE_P(lhs_type_tree)
	  || COMPLEX_FLOAT_TYPE_P(lhs_type_tree))
	rhs_tree = fold_convert_loc(location.gcc_location(), lhs_type_tree,
				    rhs_tree);
      else if (TREE_CODE(lhs_type_tree) == RECORD_TYPE
	       || TREE_CODE(lhs_type_tree) == ARRAY_TYPE)
	{
	  gcc_assert(int_size_in_bytes(lhs_type_tree)
		     == int_size_in_bytes(TREE_TYPE(rhs_tree)));
	  rhs_tree = fold_build1_loc(location.gcc_location(),
				     VIEW_CONVERT_EXPR,
				     lhs_type_tree, rhs_tree);
	}
    }

  return this->make_statement(fold_build2_loc(location.gcc_location(),
                                              MODIFY_EXPR,
					      void_type_node,
					      lhs_tree, rhs_tree));
}

// Return.

Bstatement*
Gcc_backend::return_statement(Bfunction* bfunction,
			      const std::vector<Bexpression*>& vals,
			      Location location)
{
  tree fntree = bfunction->get_tree();
  if (fntree == error_mark_node)
    return this->error_statement();
  tree result = DECL_RESULT(fntree);
  if (result == error_mark_node)
    return this->error_statement();

  tree ret;
  if (vals.empty())
    ret = fold_build1_loc(location.gcc_location(), RETURN_EXPR, void_type_node,
                          NULL_TREE);
  else if (vals.size() == 1)
    {
      tree val = vals.front()->get_tree();
      if (val == error_mark_node)
	return this->error_statement();
      tree set = fold_build2_loc(location.gcc_location(), MODIFY_EXPR,
                                 void_type_node, result,
                                 vals.front()->get_tree());
      ret = fold_build1_loc(location.gcc_location(), RETURN_EXPR,
                            void_type_node, set);
    }
  else
    {
      // To return multiple values, copy the values into a temporary
      // variable of the right structure type, and then assign the
      // temporary variable to the DECL_RESULT in the return
      // statement.
      tree stmt_list = NULL_TREE;
      tree rettype = TREE_TYPE(result);

      if (DECL_STRUCT_FUNCTION(fntree) == NULL)
	push_struct_function(fntree);
      else
	push_cfun(DECL_STRUCT_FUNCTION(fntree));
      tree rettmp = create_tmp_var(rettype, "RESULT");
      pop_cfun();

      tree field = TYPE_FIELDS(rettype);
      for (std::vector<Bexpression*>::const_iterator p = vals.begin();
	   p != vals.end();
	   p++, field = DECL_CHAIN(field))
	{
	  gcc_assert(field != NULL_TREE);
	  tree ref = fold_build3_loc(location.gcc_location(), COMPONENT_REF,
                                     TREE_TYPE(field), rettmp, field,
                                     NULL_TREE);
	  tree val = (*p)->get_tree();
	  if (val == error_mark_node)
	    return this->error_statement();
	  tree set = fold_build2_loc(location.gcc_location(), MODIFY_EXPR,
                                     void_type_node,
				     ref, (*p)->get_tree());
	  append_to_statement_list(set, &stmt_list);
	}
      gcc_assert(field == NULL_TREE);
      tree set = fold_build2_loc(location.gcc_location(), MODIFY_EXPR,
                                 void_type_node,
				 result, rettmp);
      tree ret_expr = fold_build1_loc(location.gcc_location(), RETURN_EXPR,
                                      void_type_node, set);
      append_to_statement_list(ret_expr, &stmt_list);
      ret = stmt_list;
    }
  return this->make_statement(ret);
}

// Create a statement that attempts to execute BSTAT and calls EXCEPT_STMT if an
// error occurs.  EXCEPT_STMT may be NULL.  FINALLY_STMT may be NULL and if not
// NULL, it will always be executed.  This is used for handling defers in Go
// functions.  In C++, the resulting code is of this form:
//   try { BSTAT; } catch { EXCEPT_STMT; } finally { FINALLY_STMT; }

Bstatement*
Gcc_backend::exception_handler_statement(Bstatement* bstat,
                                         Bstatement* except_stmt,
                                         Bstatement* finally_stmt,
                                         Location location)
{
  tree stat_tree = bstat->get_tree();
  tree except_tree = except_stmt == NULL ? NULL_TREE : except_stmt->get_tree();
  tree finally_tree = finally_stmt == NULL
      ? NULL_TREE
      : finally_stmt->get_tree();

  if (stat_tree == error_mark_node
      || except_tree == error_mark_node
      || finally_tree == error_mark_node)
    return this->error_statement();

  if (except_tree != NULL_TREE)
    stat_tree = build2_loc(location.gcc_location(), TRY_CATCH_EXPR,
                           void_type_node, stat_tree,
                           build2_loc(location.gcc_location(), CATCH_EXPR,
                                      void_type_node, NULL, except_tree));
  if (finally_tree != NULL_TREE)
    stat_tree = build2_loc(location.gcc_location(), TRY_FINALLY_EXPR,
                           void_type_node, stat_tree, finally_tree);
  return this->make_statement(stat_tree);
}

// If.

Bstatement*
Gcc_backend::if_statement(Bexpression* condition, Bblock* then_block,
			  Bblock* else_block, Location location)
{
  tree cond_tree = condition->get_tree();
  tree then_tree = then_block->get_tree();
  tree else_tree = else_block == NULL ? NULL_TREE : else_block->get_tree();
  if (cond_tree == error_mark_node
      || then_tree == error_mark_node
      || else_tree == error_mark_node)
    return this->error_statement();
  tree ret = build3_loc(location.gcc_location(), COND_EXPR, void_type_node,
                        cond_tree, then_tree, else_tree);
  return this->make_statement(ret);
}

// Switch.

Bstatement*
Gcc_backend::switch_statement(
    Bfunction* function,
    Bexpression* value,
    const std::vector<std::vector<Bexpression*> >& cases,
    const std::vector<Bstatement*>& statements,
    Location switch_location)
{
  gcc_assert(cases.size() == statements.size());

  tree decl = function->get_tree();
  if (DECL_STRUCT_FUNCTION(decl) == NULL)
    push_struct_function(decl);
  else
    push_cfun(DECL_STRUCT_FUNCTION(decl));

  tree stmt_list = NULL_TREE;
  std::vector<std::vector<Bexpression*> >::const_iterator pc = cases.begin();
  for (std::vector<Bstatement*>::const_iterator ps = statements.begin();
       ps != statements.end();
       ++ps, ++pc)
    {
      if (pc->empty())
	{
	  source_location loc = (*ps != NULL
                                 ? EXPR_LOCATION((*ps)->get_tree())
                                 : UNKNOWN_LOCATION);
	  tree label = create_artificial_label(loc);
	  tree c = build_case_label(NULL_TREE, NULL_TREE, label);
	  append_to_statement_list(c, &stmt_list);
	}
      else
	{
	  for (std::vector<Bexpression*>::const_iterator pcv = pc->begin();
	       pcv != pc->end();
	       ++pcv)
	    {
	      tree t = (*pcv)->get_tree();
	      if (t == error_mark_node)
		return this->error_statement();
	      source_location loc = EXPR_LOCATION(t);
	      tree label = create_artificial_label(loc);
	      tree c = build_case_label((*pcv)->get_tree(), NULL_TREE, label);
	      append_to_statement_list(c, &stmt_list);
	    }
	}

      if (*ps != NULL)
	{
	  tree t = (*ps)->get_tree();
	  if (t == error_mark_node)
	    return this->error_statement();
	  append_to_statement_list(t, &stmt_list);
	}
    }
  pop_cfun();

  tree tv = value->get_tree();
  if (tv == error_mark_node)
    return this->error_statement();
  tree t = build3_loc(switch_location.gcc_location(), SWITCH_EXPR,
                      NULL_TREE, tv, stmt_list, NULL_TREE);
  return this->make_statement(t);
}

// Pair of statements.

Bstatement*
Gcc_backend::compound_statement(Bstatement* s1, Bstatement* s2)
{
  tree stmt_list = NULL_TREE;
  tree t = s1->get_tree();
  if (t == error_mark_node)
    return this->error_statement();
  append_to_statement_list(t, &stmt_list);
  t = s2->get_tree();
  if (t == error_mark_node)
    return this->error_statement();
  append_to_statement_list(t, &stmt_list);

  // If neither statement has any side effects, stmt_list can be NULL
  // at this point.
  if (stmt_list == NULL_TREE)
    stmt_list = integer_zero_node;

  return this->make_statement(stmt_list);
}

// List of statements.

Bstatement*
Gcc_backend::statement_list(const std::vector<Bstatement*>& statements)
{
  tree stmt_list = NULL_TREE;
  for (std::vector<Bstatement*>::const_iterator p = statements.begin();
       p != statements.end();
       ++p)
    {
      tree t = (*p)->get_tree();
      if (t == error_mark_node)
	return this->error_statement();
      append_to_statement_list(t, &stmt_list);
    }
  return this->make_statement(stmt_list);
}

// Make a block.  For some reason gcc uses a dual structure for
// blocks: BLOCK tree nodes and BIND_EXPR tree nodes.  Since the
// BIND_EXPR node points to the BLOCK node, we store the BIND_EXPR in
// the Bblock.

Bblock*
Gcc_backend::block(Bfunction* function, Bblock* enclosing,
		   const std::vector<Bvariable*>& vars,
		   Location start_location,
		   Location)
{
  tree block_tree = make_node(BLOCK);
  if (enclosing == NULL)
    {
      tree fndecl = function->get_tree();
      gcc_assert(fndecl != NULL_TREE);

      // We may have already created a block for local variables when
      // we take the address of a parameter.
      if (DECL_INITIAL(fndecl) == NULL_TREE)
	{
	  BLOCK_SUPERCONTEXT(block_tree) = fndecl;
	  DECL_INITIAL(fndecl) = block_tree;
	}
      else
	{
	  tree superblock_tree = DECL_INITIAL(fndecl);
	  BLOCK_SUPERCONTEXT(block_tree) = superblock_tree;
	  tree* pp;
	  for (pp = &BLOCK_SUBBLOCKS(superblock_tree);
	       *pp != NULL_TREE;
	       pp = &BLOCK_CHAIN(*pp))
	    ;
	  *pp = block_tree;
	}
    }
  else
    {
      tree superbind_tree = enclosing->get_tree();
      tree superblock_tree = BIND_EXPR_BLOCK(superbind_tree);
      gcc_assert(TREE_CODE(superblock_tree) == BLOCK);

      BLOCK_SUPERCONTEXT(block_tree) = superblock_tree;
      tree* pp;
      for (pp = &BLOCK_SUBBLOCKS(superblock_tree);
	   *pp != NULL_TREE;
	   pp = &BLOCK_CHAIN(*pp))
	;
      *pp = block_tree;
    }

  tree* pp = &BLOCK_VARS(block_tree);
  for (std::vector<Bvariable*>::const_iterator pv = vars.begin();
       pv != vars.end();
       ++pv)
    {
      *pp = (*pv)->get_decl();
      if (*pp != error_mark_node)
	pp = &DECL_CHAIN(*pp);
    }
  *pp = NULL_TREE;

  TREE_USED(block_tree) = 1;

  tree bind_tree = build3_loc(start_location.gcc_location(), BIND_EXPR,
                              void_type_node, BLOCK_VARS(block_tree),
                              NULL_TREE, block_tree);
  TREE_SIDE_EFFECTS(bind_tree) = 1;
  return new Bblock(bind_tree);
}

// Add statements to a block.

void
Gcc_backend::block_add_statements(Bblock* bblock,
				  const std::vector<Bstatement*>& statements)
{
  tree stmt_list = NULL_TREE;
  for (std::vector<Bstatement*>::const_iterator p = statements.begin();
       p != statements.end();
       ++p)
    {
      tree s = (*p)->get_tree();
      if (s != error_mark_node)
	append_to_statement_list(s, &stmt_list);
    }

  tree bind_tree = bblock->get_tree();
  gcc_assert(TREE_CODE(bind_tree) == BIND_EXPR);
  BIND_EXPR_BODY(bind_tree) = stmt_list;
}

// Return a block as a statement.

Bstatement*
Gcc_backend::block_statement(Bblock* bblock)
{
  tree bind_tree = bblock->get_tree();
  gcc_assert(TREE_CODE(bind_tree) == BIND_EXPR);
  return this->make_statement(bind_tree);
}

// This is not static because we declare it with GTY(()) in go-c.h.
tree go_non_zero_struct;

// Return a type corresponding to TYPE with non-zero size.

tree
Gcc_backend::non_zero_size_type(tree type)
{
  if (int_size_in_bytes(type) != 0)
    return type;

  switch (TREE_CODE(type))
    {
    case RECORD_TYPE:
      if (TYPE_FIELDS(type) != NULL_TREE)
	{
	  tree ns = make_node(RECORD_TYPE);
	  tree field_trees = NULL_TREE;
	  tree *pp = &field_trees;
	  for (tree field = TYPE_FIELDS(type);
	       field != NULL_TREE;
	       field = DECL_CHAIN(field))
	    {
	      tree ft = TREE_TYPE(field);
	      if (field == TYPE_FIELDS(type))
		ft = non_zero_size_type(ft);
	      tree f = build_decl(DECL_SOURCE_LOCATION(field), FIELD_DECL,
				  DECL_NAME(field), ft);
	      DECL_CONTEXT(f) = ns;
	      *pp = f;
	      pp = &DECL_CHAIN(f);
	    }
	  TYPE_FIELDS(ns) = field_trees;
	  layout_type(ns);
	  return ns;
	}

      if (go_non_zero_struct == NULL_TREE)
	{
	  type = make_node(RECORD_TYPE);
	  tree field = build_decl(UNKNOWN_LOCATION, FIELD_DECL,
				  get_identifier("dummy"),
				  boolean_type_node);
	  DECL_CONTEXT(field) = type;
	  TYPE_FIELDS(type) = field;
	  layout_type(type);
	  go_non_zero_struct = type;
	}
      return go_non_zero_struct;

    case ARRAY_TYPE:
      {
	tree element_type = non_zero_size_type(TREE_TYPE(type));
	return build_array_type_nelts(element_type, 1);
      }

    default:
      gcc_unreachable();
    }

  gcc_unreachable();
}

// Make a global variable.

Bvariable*
Gcc_backend::global_variable(const std::string& package_name,
			     const std::string& pkgpath,
			     const std::string& name,
			     Btype* btype,
			     bool is_external,
			     bool is_hidden,
			     bool in_unique_section,
			     Location location)
{
  tree type_tree = btype->get_tree();
  if (type_tree == error_mark_node)
    return this->error_variable();

  // The GNU linker does not like dynamic variables with zero size.
  tree orig_type_tree = type_tree;
  if ((is_external || !is_hidden) && int_size_in_bytes(type_tree) == 0)
    type_tree = this->non_zero_size_type(type_tree);

  std::string var_name(package_name);
  var_name.push_back('.');
  var_name.append(name);
  tree decl = build_decl(location.gcc_location(), VAR_DECL,
			 get_identifier_from_string(var_name),
			 type_tree);
  if (is_external)
    DECL_EXTERNAL(decl) = 1;
  else
    TREE_STATIC(decl) = 1;
  if (!is_hidden)
    {
      TREE_PUBLIC(decl) = 1;

      std::string asm_name(pkgpath);
      asm_name.push_back('.');
      asm_name.append(name);
      if (needs_encoding(asm_name))
	asm_name = encode_id(asm_name);
      SET_DECL_ASSEMBLER_NAME(decl, get_identifier_from_string(asm_name));
    }
  else if (needs_encoding(var_name))
    SET_DECL_ASSEMBLER_NAME(decl,
			    get_identifier_from_string(encode_id(var_name)));

  TREE_USED(decl) = 1;

  if (in_unique_section)
    resolve_unique_section (decl, 0, 1);

  go_preserve_from_gc(decl);

  return new Bvariable(decl, orig_type_tree);
}

// Set the initial value of a global variable.

void
Gcc_backend::global_variable_set_init(Bvariable* var, Bexpression* expr)
{
  tree expr_tree = expr->get_tree();
  if (expr_tree == error_mark_node)
    return;
  gcc_assert(TREE_CONSTANT(expr_tree));
  tree var_decl = var->get_decl();
  if (var_decl == error_mark_node)
    return;
  DECL_INITIAL(var_decl) = expr_tree;

  // If this variable goes in a unique section, it may need to go into
  // a different one now that DECL_INITIAL is set.
  if (symtab_node::get(var_decl)
      && symtab_node::get(var_decl)->implicit_section)
    {
      set_decl_section_name (var_decl, NULL);
      resolve_unique_section (var_decl,
			      compute_reloc_for_constant (expr_tree),
			      1);
    }
}

// Make a local variable.

Bvariable*
Gcc_backend::local_variable(Bfunction* function, const std::string& name,
			    Btype* btype, bool is_address_taken,
			    Location location)
{
  tree type_tree = btype->get_tree();
  if (type_tree == error_mark_node)
    return this->error_variable();
  tree decl = build_decl(location.gcc_location(), VAR_DECL,
			 get_identifier_from_string(name),
			 type_tree);
  DECL_CONTEXT(decl) = function->get_tree();
  TREE_USED(decl) = 1;
  if (is_address_taken)
    TREE_ADDRESSABLE(decl) = 1;
  go_preserve_from_gc(decl);
  return new Bvariable(decl);
}

// Make a function parameter variable.

Bvariable*
Gcc_backend::parameter_variable(Bfunction* function, const std::string& name,
				Btype* btype, bool is_address_taken,
				Location location)
{
  tree type_tree = btype->get_tree();
  if (type_tree == error_mark_node)
    return this->error_variable();
  tree decl = build_decl(location.gcc_location(), PARM_DECL,
			 get_identifier_from_string(name),
			 type_tree);
  DECL_CONTEXT(decl) = function->get_tree();
  DECL_ARG_TYPE(decl) = type_tree;
  TREE_USED(decl) = 1;
  if (is_address_taken)
    TREE_ADDRESSABLE(decl) = 1;
  go_preserve_from_gc(decl);
  return new Bvariable(decl);
}

// Make a static chain variable.

Bvariable*
Gcc_backend::static_chain_variable(Bfunction* function, const std::string& name,
				   Btype* btype, Location location)
{
  tree type_tree = btype->get_tree();
  if (type_tree == error_mark_node)
    return this->error_variable();
  tree decl = build_decl(location.gcc_location(), PARM_DECL,
			 get_identifier_from_string(name), type_tree);
  tree fndecl = function->get_tree();
  DECL_CONTEXT(decl) = fndecl;
  DECL_ARG_TYPE(decl) = type_tree;
  TREE_USED(decl) = 1;
  DECL_ARTIFICIAL(decl) = 1;
  DECL_IGNORED_P(decl) = 1;
  TREE_READONLY(decl) = 1;

  struct function *f = DECL_STRUCT_FUNCTION(fndecl);
  if (f == NULL)
    {
      push_struct_function(fndecl);
      pop_cfun();
      f = DECL_STRUCT_FUNCTION(fndecl);
    }
  gcc_assert(f->static_chain_decl == NULL);
  f->static_chain_decl = decl;
  DECL_STATIC_CHAIN(fndecl) = 1;

  go_preserve_from_gc(decl);
  return new Bvariable(decl);
}

// Make a temporary variable.

Bvariable*
Gcc_backend::temporary_variable(Bfunction* function, Bblock* bblock,
				Btype* btype, Bexpression* binit,
				bool is_address_taken,
				Location location,
				Bstatement** pstatement)
{
  gcc_assert(function != NULL);
  tree decl = function->get_tree();
  tree type_tree = btype->get_tree();
  tree init_tree = binit == NULL ? NULL_TREE : binit->get_tree();
  if (type_tree == error_mark_node
      || init_tree == error_mark_node
      || decl == error_mark_node)
    {
      *pstatement = this->error_statement();
      return this->error_variable();
    }

  tree var;
  // We can only use create_tmp_var if the type is not addressable.
  if (!TREE_ADDRESSABLE(type_tree))
    {
      if (DECL_STRUCT_FUNCTION(decl) == NULL)
      	push_struct_function(decl);
      else
      	push_cfun(DECL_STRUCT_FUNCTION(decl));

      var = create_tmp_var(type_tree, "GOTMP");
      pop_cfun();
    }
  else
    {
      gcc_assert(bblock != NULL);
      var = build_decl(location.gcc_location(), VAR_DECL,
		       create_tmp_var_name("GOTMP"),
		       type_tree);
      DECL_ARTIFICIAL(var) = 1;
      DECL_IGNORED_P(var) = 1;
      TREE_USED(var) = 1;
      DECL_CONTEXT(var) = decl;

      // We have to add this variable to the BLOCK and the BIND_EXPR.
      tree bind_tree = bblock->get_tree();
      gcc_assert(TREE_CODE(bind_tree) == BIND_EXPR);
      tree block_tree = BIND_EXPR_BLOCK(bind_tree);
      gcc_assert(TREE_CODE(block_tree) == BLOCK);
      DECL_CHAIN(var) = BLOCK_VARS(block_tree);
      BLOCK_VARS(block_tree) = var;
      BIND_EXPR_VARS(bind_tree) = BLOCK_VARS(block_tree);
    }

  if (this->type_size(btype) != 0 && init_tree != NULL_TREE)
    DECL_INITIAL(var) = fold_convert_loc(location.gcc_location(), type_tree,
                                         init_tree);

  if (is_address_taken)
    TREE_ADDRESSABLE(var) = 1;

  *pstatement = this->make_statement(build1_loc(location.gcc_location(),
                                                DECL_EXPR,
						void_type_node, var));

  // Don't initialize VAR with BINIT, but still evaluate BINIT for
  // its side effects.
  if (this->type_size(btype) == 0 && init_tree != NULL_TREE)
    *pstatement = this->compound_statement(this->expression_statement(binit),
					   *pstatement);

  return new Bvariable(var);
}

// Create an implicit variable that is compiler-defined.  This is used when
// generating GC root variables and storing the values of a slice initializer.

Bvariable*
Gcc_backend::implicit_variable(const std::string& name, Btype* type,
			       bool is_hidden, bool is_constant,
			       bool is_common, int64_t alignment)
{
  tree type_tree = type->get_tree();
  if (type_tree == error_mark_node)
    return this->error_variable();

  tree decl = build_decl(BUILTINS_LOCATION, VAR_DECL,
                         get_identifier_from_string(name), type_tree);
  DECL_EXTERNAL(decl) = 0;
  TREE_PUBLIC(decl) = !is_hidden;
  TREE_STATIC(decl) = 1;
  TREE_USED(decl) = 1;
  DECL_ARTIFICIAL(decl) = 1;
  if (is_common)
    {
      DECL_COMMON(decl) = 1;

      // When the initializer for one implicit_variable refers to another,
      // it needs to know the visibility of the referenced struct so that
      // compute_reloc_for_constant will return the right value.  On many
      // systems calling make_decl_one_only will mark the decl as weak,
      // which will change the return value of compute_reloc_for_constant.
      // We can't reliably call make_decl_one_only yet, because we don't
      // yet know the initializer.  This issue doesn't arise in C because
      // Go initializers, unlike C initializers, can be indirectly
      // recursive.  To ensure that compute_reloc_for_constant computes
      // the right value if some other initializer refers to this one, we
      // mark this symbol as weak here.  We undo that below in
      // immutable_struct_set_init before calling mark_decl_one_only.
      DECL_WEAK(decl) = 1;
    }
  if (is_constant)
    {
      TREE_READONLY(decl) = 1;
      TREE_CONSTANT(decl) = 1;
    }
  if (alignment != 0)
    {
      SET_DECL_ALIGN(decl, alignment * BITS_PER_UNIT);
      DECL_USER_ALIGN(decl) = 1;
    }
  if (needs_encoding(name))
    SET_DECL_ASSEMBLER_NAME(decl, get_identifier_from_string(encode_id(name)));

  go_preserve_from_gc(decl);
  return new Bvariable(decl);
}

// Set the initalizer for a variable created by implicit_variable.
// This is where we finish compiling the variable.

void
Gcc_backend::implicit_variable_set_init(Bvariable* var, const std::string&,
					Btype*, bool, bool, bool is_common,
					Bexpression* init)
{
  tree decl = var->get_decl();
  tree init_tree;
  if (init == NULL)
    init_tree = NULL_TREE;
  else
    init_tree = init->get_tree();
  if (decl == error_mark_node || init_tree == error_mark_node)
    return;

  DECL_INITIAL(decl) = init_tree;

  // Now that DECL_INITIAL is set, we can't call make_decl_one_only.
  // See the comment where DECL_WEAK is set in implicit_variable.
  if (is_common)
    {
      DECL_WEAK(decl) = 0;
      make_decl_one_only(decl, DECL_ASSEMBLER_NAME(decl));
    }

  resolve_unique_section(decl, 2, 1);

  rest_of_decl_compilation(decl, 1, 0);
}

// Return a reference to an implicit variable defined in another package.

Bvariable*
Gcc_backend::implicit_variable_reference(const std::string& name, Btype* btype)
{
  tree type_tree = btype->get_tree();
  if (type_tree == error_mark_node)
    return this->error_variable();

  tree decl = build_decl(BUILTINS_LOCATION, VAR_DECL,
                         get_identifier_from_string(name), type_tree);
  DECL_EXTERNAL(decl) = 0;
  TREE_PUBLIC(decl) = 1;
  TREE_STATIC(decl) = 1;
  DECL_ARTIFICIAL(decl) = 1;
  if (needs_encoding(name))
    SET_DECL_ASSEMBLER_NAME(decl, get_identifier_from_string(encode_id(name)));
  go_preserve_from_gc(decl);
  return new Bvariable(decl);
}

// Create a named immutable initialized data structure.

Bvariable*
Gcc_backend::immutable_struct(const std::string& name, bool is_hidden,
			      bool is_common, Btype* btype, Location location)
{
  tree type_tree = btype->get_tree();
  if (type_tree == error_mark_node)
    return this->error_variable();
  gcc_assert(TREE_CODE(type_tree) == RECORD_TYPE);
  tree decl = build_decl(location.gcc_location(), VAR_DECL,
			 get_identifier_from_string(name),
			 build_qualified_type(type_tree, TYPE_QUAL_CONST));
  TREE_STATIC(decl) = 1;
  TREE_USED(decl) = 1;
  TREE_READONLY(decl) = 1;
  TREE_CONSTANT(decl) = 1;
  DECL_ARTIFICIAL(decl) = 1;
  if (!is_hidden)
    TREE_PUBLIC(decl) = 1;
  if (needs_encoding(name))
    SET_DECL_ASSEMBLER_NAME(decl, get_identifier_from_string(encode_id(name)));

  // When the initializer for one immutable_struct refers to another,
  // it needs to know the visibility of the referenced struct so that
  // compute_reloc_for_constant will return the right value.  On many
  // systems calling make_decl_one_only will mark the decl as weak,
  // which will change the return value of compute_reloc_for_constant.
  // We can't reliably call make_decl_one_only yet, because we don't
  // yet know the initializer.  This issue doesn't arise in C because
  // Go initializers, unlike C initializers, can be indirectly
  // recursive.  To ensure that compute_reloc_for_constant computes
  // the right value if some other initializer refers to this one, we
  // mark this symbol as weak here.  We undo that below in
  // immutable_struct_set_init before calling mark_decl_one_only.
  if (is_common)
    DECL_WEAK(decl) = 1;

  // We don't call rest_of_decl_compilation until we have the
  // initializer.

  go_preserve_from_gc(decl);
  return new Bvariable(decl);
}

// Set the initializer for a variable created by immutable_struct.
// This is where we finish compiling the variable.

void
Gcc_backend::immutable_struct_set_init(Bvariable* var, const std::string&,
				       bool, bool is_common, Btype*, Location,
				       Bexpression* initializer)
{
  tree decl = var->get_decl();
  tree init_tree = initializer->get_tree();
  if (decl == error_mark_node || init_tree == error_mark_node)
    return;

  DECL_INITIAL(decl) = init_tree;

  // Now that DECL_INITIAL is set, we can't call make_decl_one_only.
  // See the comment where DECL_WEAK is set in immutable_struct.
  if (is_common)
    {
      DECL_WEAK(decl) = 0;
      make_decl_one_only(decl, DECL_ASSEMBLER_NAME(decl));
    }

  // These variables are often unneeded in the final program, so put
  // them in their own section so that linker GC can discard them.
  resolve_unique_section(decl,
			 compute_reloc_for_constant (init_tree),
			 1);

  rest_of_decl_compilation(decl, 1, 0);
}

// Return a reference to an immutable initialized data structure
// defined in another package.

Bvariable*
Gcc_backend::immutable_struct_reference(const std::string& name, Btype* btype,
					Location location)
{
  tree type_tree = btype->get_tree();
  if (type_tree == error_mark_node)
    return this->error_variable();
  gcc_assert(TREE_CODE(type_tree) == RECORD_TYPE);
  tree decl = build_decl(location.gcc_location(), VAR_DECL,
			 get_identifier_from_string(name),
			 build_qualified_type(type_tree, TYPE_QUAL_CONST));
  TREE_READONLY(decl) = 1;
  TREE_CONSTANT(decl) = 1;
  DECL_ARTIFICIAL(decl) = 1;
  TREE_PUBLIC(decl) = 1;
  DECL_EXTERNAL(decl) = 1;
  if (needs_encoding(name))
    SET_DECL_ASSEMBLER_NAME(decl, get_identifier_from_string(encode_id(name)));
  go_preserve_from_gc(decl);
  return new Bvariable(decl);
}

// Make a label.

Blabel*
Gcc_backend::label(Bfunction* function, const std::string& name,
		   Location location)
{
  tree decl;
  if (name.empty())
    {
      tree func_tree = function->get_tree();
      if (DECL_STRUCT_FUNCTION(func_tree) == NULL)
	push_struct_function(func_tree);
      else
	push_cfun(DECL_STRUCT_FUNCTION(func_tree));

      decl = create_artificial_label(location.gcc_location());

      pop_cfun();
    }
  else
    {
      tree id = get_identifier_from_string(name);
      decl = build_decl(location.gcc_location(), LABEL_DECL, id,
                        void_type_node);
      DECL_CONTEXT(decl) = function->get_tree();
    }
  return new Blabel(decl);
}

// Make a statement which defines a label.

Bstatement*
Gcc_backend::label_definition_statement(Blabel* label)
{
  tree lab = label->get_tree();
  tree ret = fold_build1_loc(DECL_SOURCE_LOCATION(lab), LABEL_EXPR,
			     void_type_node, lab);
  return this->make_statement(ret);
}

// Make a goto statement.

Bstatement*
Gcc_backend::goto_statement(Blabel* label, Location location)
{
  tree lab = label->get_tree();
  tree ret = fold_build1_loc(location.gcc_location(), GOTO_EXPR, void_type_node,
                             lab);
  return this->make_statement(ret);
}

// Get the address of a label.

Bexpression*
Gcc_backend::label_address(Blabel* label, Location location)
{
  tree lab = label->get_tree();
  TREE_USED(lab) = 1;
  TREE_ADDRESSABLE(lab) = 1;
  tree ret = fold_convert_loc(location.gcc_location(), ptr_type_node,
			      build_fold_addr_expr_loc(location.gcc_location(),
                                                       lab));
  return this->make_expression(ret);
}

// Declare or define a new function.

Bfunction*
Gcc_backend::function(Btype* fntype, const std::string& name,
                      const std::string& asm_name, bool is_visible,
                      bool is_declaration, bool is_inlinable,
                      bool disable_split_stack, bool in_unique_section,
                      Location location)
{
  tree functype = fntype->get_tree();
  if (functype != error_mark_node)
    {
      gcc_assert(FUNCTION_POINTER_TYPE_P(functype));
      functype = TREE_TYPE(functype);
    }
  tree id = get_identifier_from_string(name);
  if (functype == error_mark_node || id == error_mark_node)
    return this->error_function();

  tree decl = build_decl(location.gcc_location(), FUNCTION_DECL, id, functype);
  if (!asm_name.empty())
    SET_DECL_ASSEMBLER_NAME(decl, get_identifier_from_string(asm_name));
  else if (needs_encoding(name))
    SET_DECL_ASSEMBLER_NAME(decl, get_identifier_from_string(encode_id(name)));
  if (is_visible)
    TREE_PUBLIC(decl) = 1;
  if (is_declaration)
    DECL_EXTERNAL(decl) = 1;
  else
    {
      tree restype = TREE_TYPE(functype);
      tree resdecl =
          build_decl(location.gcc_location(), RESULT_DECL, NULL_TREE, restype);
      DECL_ARTIFICIAL(resdecl) = 1;
      DECL_IGNORED_P(resdecl) = 1;
      DECL_CONTEXT(resdecl) = decl;
      DECL_RESULT(decl) = resdecl;
    }
  if (!is_inlinable)
    DECL_UNINLINABLE(decl) = 1;
  if (disable_split_stack)
    {
      tree attr = get_identifier("__no_split_stack__");
      DECL_ATTRIBUTES(decl) = tree_cons(attr, NULL_TREE, NULL_TREE);
    }
  if (in_unique_section)
    resolve_unique_section(decl, 0, 1);

  go_preserve_from_gc(decl);
  return new Bfunction(decl);
}

// Create a statement that runs all deferred calls for FUNCTION.  This should
// be a statement that looks like this in C++:
//   finish:
//     try { UNDEFER; } catch { CHECK_DEFER; goto finish; }

Bstatement*
Gcc_backend::function_defer_statement(Bfunction* function, Bexpression* undefer,
                                      Bexpression* defer, Location location)
{
  tree undefer_tree = undefer->get_tree();
  tree defer_tree = defer->get_tree();
  tree fntree = function->get_tree();

  if (undefer_tree == error_mark_node
      || defer_tree == error_mark_node
      || fntree == error_mark_node)
    return this->error_statement();

  if (DECL_STRUCT_FUNCTION(fntree) == NULL)
    push_struct_function(fntree);
  else
    push_cfun(DECL_STRUCT_FUNCTION(fntree));

  tree stmt_list = NULL;
  Blabel* blabel = this->label(function, "", location);
  Bstatement* label_def = this->label_definition_statement(blabel);
  append_to_statement_list(label_def->get_tree(), &stmt_list);

  Bstatement* jump_stmt = this->goto_statement(blabel, location);
  tree jump = jump_stmt->get_tree();
  tree catch_body = build2(COMPOUND_EXPR, void_type_node, defer_tree, jump);
  catch_body = build2(CATCH_EXPR, void_type_node, NULL, catch_body);
  tree try_catch =
      build2(TRY_CATCH_EXPR, void_type_node, undefer_tree, catch_body);
  append_to_statement_list(try_catch, &stmt_list);
  pop_cfun();

  return this->make_statement(stmt_list);
}

// Record PARAM_VARS as the variables to use for the parameters of FUNCTION.
// This will only be called for a function definition.

bool
Gcc_backend::function_set_parameters(Bfunction* function,
                                     const std::vector<Bvariable*>& param_vars)
{
  tree func_tree = function->get_tree();
  if (func_tree == error_mark_node)
    return false;

  tree params = NULL_TREE;
  tree *pp = &params;
  for (std::vector<Bvariable*>::const_iterator pv = param_vars.begin();
       pv != param_vars.end();
       ++pv)
    {
      *pp = (*pv)->get_decl();
      gcc_assert(*pp != error_mark_node);
      pp = &DECL_CHAIN(*pp);
    }
  *pp = NULL_TREE;
  DECL_ARGUMENTS(func_tree) = params;
  return true;
}

// Set the function body for FUNCTION using the code in CODE_BLOCK.

bool
Gcc_backend::function_set_body(Bfunction* function, Bstatement* code_stmt)
{
  tree func_tree = function->get_tree();
  tree code = code_stmt->get_tree();

  if (func_tree == error_mark_node || code == error_mark_node)
    return false;
  DECL_SAVED_TREE(func_tree) = code;
  return true;
}

// Look up a named built-in function in the current backend implementation.
// Returns NULL if no built-in function by that name exists.

Bfunction*
Gcc_backend::lookup_builtin(const std::string& name)
{
  if (this->builtin_functions_.count(name) != 0)
    return this->builtin_functions_[name];
  return NULL;
}

// Write the definitions for all TYPE_DECLS, CONSTANT_DECLS,
// FUNCTION_DECLS, and VARIABLE_DECLS declared globally, as well as
// emit early debugging information.

void
Gcc_backend::write_global_definitions(
    const std::vector<Btype*>& type_decls,
    const std::vector<Bexpression*>& constant_decls,
    const std::vector<Bfunction*>& function_decls,
    const std::vector<Bvariable*>& variable_decls)
{
  size_t count_definitions = type_decls.size() + constant_decls.size()
      + function_decls.size() + variable_decls.size();

  tree* defs = new tree[count_definitions];

  // Convert all non-erroneous declarations into Gimple form.
  size_t i = 0;
  for (std::vector<Bvariable*>::const_iterator p = variable_decls.begin();
       p != variable_decls.end();
       ++p)
    {
      tree v = (*p)->get_decl();
      if (v != error_mark_node)
        {
          defs[i] = v;
          go_preserve_from_gc(defs[i]);
          ++i;
        }
    }

  for (std::vector<Btype*>::const_iterator p = type_decls.begin();
       p != type_decls.end();
       ++p)
    {
      tree type_tree = (*p)->get_tree();
      if (type_tree != error_mark_node
          && IS_TYPE_OR_DECL_P(type_tree))
        {
          defs[i] = TYPE_NAME(type_tree);
          gcc_assert(defs[i] != NULL);
          go_preserve_from_gc(defs[i]);
          ++i;
        }
    }
  for (std::vector<Bexpression*>::const_iterator p = constant_decls.begin();
       p != constant_decls.end();
       ++p)
    {
      if ((*p)->get_tree() != error_mark_node)
        {
          defs[i] = (*p)->get_tree();
          go_preserve_from_gc(defs[i]);
          ++i;
        }
    }
  for (std::vector<Bfunction*>::const_iterator p = function_decls.begin();
       p != function_decls.end();
       ++p)
    {
      tree decl = (*p)->get_tree();
      if (decl != error_mark_node)
        {
          go_preserve_from_gc(decl);
          gimplify_function_tree(decl);
          cgraph_node::finalize_function(decl, true);

          defs[i] = decl;
          ++i;
        }
    }

  // Pass everything back to the middle-end.

  wrapup_global_declarations(defs, i);

  delete[] defs;
}

// Define a builtin function.  BCODE is the builtin function code
// defined by builtins.def.  NAME is the name of the builtin function.
// LIBNAME is the name of the corresponding library function, and is
// NULL if there isn't one.  FNTYPE is the type of the function.
// CONST_P is true if the function has the const attribute.
// NORETURN_P is true if the function has the noreturn attribute.

void
Gcc_backend::define_builtin(built_in_function bcode, const char* name,
			    const char* libname, tree fntype, bool const_p,
			    bool noreturn_p)
{
  tree decl = add_builtin_function(name, fntype, bcode, BUILT_IN_NORMAL,
				   libname, NULL_TREE);
  if (const_p)
    TREE_READONLY(decl) = 1;
  if (noreturn_p)
    TREE_THIS_VOLATILE(decl) = 1;
  set_builtin_decl(bcode, decl, true);
  this->builtin_functions_[name] = this->make_function(decl);
  if (libname != NULL)
    {
      decl = add_builtin_function(libname, fntype, bcode, BUILT_IN_NORMAL,
				  NULL, NULL_TREE);
      if (const_p)
	TREE_READONLY(decl) = 1;
      if (noreturn_p)
	TREE_THIS_VOLATILE(decl) = 1;
      this->builtin_functions_[libname] = this->make_function(decl);
    }
}

// Return the backend generator.

Backend*
go_get_backend()
{
  return new Gcc_backend();
}
