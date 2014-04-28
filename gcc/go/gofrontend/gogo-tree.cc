// gogo-tree.cc -- convert Go frontend Gogo IR to gcc trees.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "toplev.h"
#include "tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "varasm.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "tree-iterator.h"
#include "cgraph.h"
#include "langhooks.h"
#include "convert.h"
#include "output.h"
#include "diagnostic.h"
#include "go-c.h"

#include "types.h"
#include "expressions.h"
#include "statements.h"
#include "runtime.h"
#include "backend.h"
#include "gogo.h"

// Whether we have seen any errors.

bool
saw_errors()
{
  return errorcount != 0 || sorrycount != 0;
}

// A helper function.

static inline tree
get_identifier_from_string(const std::string& str)
{
  return get_identifier_with_length(str.data(), str.length());
}

// Builtin functions.

static std::map<std::string, tree> builtin_functions;

// Define a builtin function.  BCODE is the builtin function code
// defined by builtins.def.  NAME is the name of the builtin function.
// LIBNAME is the name of the corresponding library function, and is
// NULL if there isn't one.  FNTYPE is the type of the function.
// CONST_P is true if the function has the const attribute.

static void
define_builtin(built_in_function bcode, const char* name, const char* libname,
	       tree fntype, bool const_p)
{
  tree decl = add_builtin_function(name, fntype, bcode, BUILT_IN_NORMAL,
				   libname, NULL_TREE);
  if (const_p)
    TREE_READONLY(decl) = 1;
  set_builtin_decl(bcode, decl, true);
  builtin_functions[name] = decl;
  if (libname != NULL)
    {
      decl = add_builtin_function(libname, fntype, bcode, BUILT_IN_NORMAL,
				  NULL, NULL_TREE);
      if (const_p)
	TREE_READONLY(decl) = 1;
      builtin_functions[libname] = decl;
    }
}

// Create trees for implicit builtin functions.

void
Gogo::define_builtin_function_trees()
{
  /* We need to define the fetch_and_add functions, since we use them
     for ++ and --.  */
  tree t = go_type_for_size(BITS_PER_UNIT, 1);
  tree p = build_pointer_type(build_qualified_type(t, TYPE_QUAL_VOLATILE));
  define_builtin(BUILT_IN_SYNC_ADD_AND_FETCH_1, "__sync_fetch_and_add_1", NULL,
		 build_function_type_list(t, p, t, NULL_TREE), false);

  t = go_type_for_size(BITS_PER_UNIT * 2, 1);
  p = build_pointer_type(build_qualified_type(t, TYPE_QUAL_VOLATILE));
  define_builtin (BUILT_IN_SYNC_ADD_AND_FETCH_2, "__sync_fetch_and_add_2", NULL,
		  build_function_type_list(t, p, t, NULL_TREE), false);

  t = go_type_for_size(BITS_PER_UNIT * 4, 1);
  p = build_pointer_type(build_qualified_type(t, TYPE_QUAL_VOLATILE));
  define_builtin(BUILT_IN_SYNC_ADD_AND_FETCH_4, "__sync_fetch_and_add_4", NULL,
		 build_function_type_list(t, p, t, NULL_TREE), false);

  t = go_type_for_size(BITS_PER_UNIT * 8, 1);
  p = build_pointer_type(build_qualified_type(t, TYPE_QUAL_VOLATILE));
  define_builtin(BUILT_IN_SYNC_ADD_AND_FETCH_8, "__sync_fetch_and_add_8", NULL,
		 build_function_type_list(t, p, t, NULL_TREE), false);

  // We use __builtin_expect for magic import functions.
  define_builtin(BUILT_IN_EXPECT, "__builtin_expect", NULL,
		 build_function_type_list(long_integer_type_node,
					  long_integer_type_node,
					  long_integer_type_node,
					  NULL_TREE),
		 true);

  // We use __builtin_memcmp for struct comparisons.
  define_builtin(BUILT_IN_MEMCMP, "__builtin_memcmp", "memcmp",
		 build_function_type_list(integer_type_node,
					  const_ptr_type_node,
					  const_ptr_type_node,
					  size_type_node,
					  NULL_TREE),
		 false);

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
  define_builtin(BUILT_IN_ACOS, "__builtin_acos", "acos",
		 math_function_type, true);
  define_builtin(BUILT_IN_ACOSL, "__builtin_acosl", "acosl",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_ASIN, "__builtin_asin", "asin",
		 math_function_type, true);
  define_builtin(BUILT_IN_ASINL, "__builtin_asinl", "asinl",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_ATAN, "__builtin_atan", "atan",
		 math_function_type, true);
  define_builtin(BUILT_IN_ATANL, "__builtin_atanl", "atanl",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_ATAN2, "__builtin_atan2", "atan2",
		 math_function_type_two, true);
  define_builtin(BUILT_IN_ATAN2L, "__builtin_atan2l", "atan2l",
		 math_function_type_long_two, true);
  define_builtin(BUILT_IN_CEIL, "__builtin_ceil", "ceil",
		 math_function_type, true);
  define_builtin(BUILT_IN_CEILL, "__builtin_ceill", "ceill",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_COS, "__builtin_cos", "cos",
		 math_function_type, true);
  define_builtin(BUILT_IN_COSL, "__builtin_cosl", "cosl",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_EXP, "__builtin_exp", "exp",
		 math_function_type, true);
  define_builtin(BUILT_IN_EXPL, "__builtin_expl", "expl",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_EXPM1, "__builtin_expm1", "expm1",
		 math_function_type, true);
  define_builtin(BUILT_IN_EXPM1L, "__builtin_expm1l", "expm1l",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_FABS, "__builtin_fabs", "fabs",
		 math_function_type, true);
  define_builtin(BUILT_IN_FABSL, "__builtin_fabsl", "fabsl",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_FLOOR, "__builtin_floor", "floor",
		 math_function_type, true);
  define_builtin(BUILT_IN_FLOORL, "__builtin_floorl", "floorl",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_FMOD, "__builtin_fmod", "fmod",
		 math_function_type_two, true);
  define_builtin(BUILT_IN_FMODL, "__builtin_fmodl", "fmodl",
		 math_function_type_long_two, true);
  define_builtin(BUILT_IN_LDEXP, "__builtin_ldexp", "ldexp",
		 build_function_type_list(double_type_node,
					  double_type_node,
					  integer_type_node,
					  NULL_TREE),
		 true);
  define_builtin(BUILT_IN_LDEXPL, "__builtin_ldexpl", "ldexpl",
		 build_function_type_list(long_double_type_node,
					  long_double_type_node,
					  integer_type_node,
					  NULL_TREE),
		 true);
  define_builtin(BUILT_IN_LOG, "__builtin_log", "log",
		 math_function_type, true);
  define_builtin(BUILT_IN_LOGL, "__builtin_logl", "logl",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_LOG1P, "__builtin_log1p", "log1p",
		 math_function_type, true);
  define_builtin(BUILT_IN_LOG1PL, "__builtin_log1pl", "log1pl",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_LOG10, "__builtin_log10", "log10",
		 math_function_type, true);
  define_builtin(BUILT_IN_LOG10L, "__builtin_log10l", "log10l",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_LOG2, "__builtin_log2", "log2",
		 math_function_type, true);
  define_builtin(BUILT_IN_LOG2L, "__builtin_log2l", "log2l",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_SIN, "__builtin_sin", "sin",
		 math_function_type, true);
  define_builtin(BUILT_IN_SINL, "__builtin_sinl", "sinl",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_SQRT, "__builtin_sqrt", "sqrt",
		 math_function_type, true);
  define_builtin(BUILT_IN_SQRTL, "__builtin_sqrtl", "sqrtl",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_TAN, "__builtin_tan", "tan",
		 math_function_type, true);
  define_builtin(BUILT_IN_TANL, "__builtin_tanl", "tanl",
		 math_function_type_long, true);
  define_builtin(BUILT_IN_TRUNC, "__builtin_trunc", "trunc",
		 math_function_type, true);
  define_builtin(BUILT_IN_TRUNCL, "__builtin_truncl", "truncl",
		 math_function_type_long, true);

  // We use __builtin_return_address in the thunk we build for
  // functions which call recover.
  define_builtin(BUILT_IN_RETURN_ADDRESS, "__builtin_return_address", NULL,
		 build_function_type_list(ptr_type_node,
					  unsigned_type_node,
					  NULL_TREE),
		 false);

  // The compiler uses __builtin_trap for some exception handling
  // cases.
  define_builtin(BUILT_IN_TRAP, "__builtin_trap", NULL,
		 build_function_type(void_type_node, void_list_node),
		 false);
}

// Get the backend representation.

Bfunction*
Function_declaration::get_or_make_decl(Gogo* gogo, Named_object* no)
{
  if (this->fndecl_ == NULL)
    {
      // Let Go code use an asm declaration to pick up a builtin
      // function.
      if (!this->asm_name_.empty())
	{
	  std::map<std::string, tree>::const_iterator p =
	    builtin_functions.find(this->asm_name_);
	  if (p != builtin_functions.end())
	    {
	      this->fndecl_ = tree_to_function(p->second);
	      return this->fndecl_;
	    }
	}

      std::string asm_name;
      if (this->asm_name_.empty())
        {
          asm_name = (no->package() == NULL
                                  ? gogo->pkgpath_symbol()
                                  : no->package()->pkgpath_symbol());
          asm_name.append(1, '.');
          asm_name.append(Gogo::unpack_hidden_name(no->name()));
          if (this->fntype_->is_method())
            {
              asm_name.append(1, '.');
              Type* rtype = this->fntype_->receiver()->type();
              asm_name.append(rtype->mangled_name(gogo));
            }
        }

      Btype* functype = this->fntype_->get_backend_fntype(gogo);
      this->fndecl_ =
          gogo->backend()->function(functype, no->get_id(gogo), asm_name,
                                    true, true, true, false, false,
                                    this->location());
    }

  return this->fndecl_;
}

// Build the descriptor for a function declaration.  This won't
// necessarily happen if the package has just a declaration for the
// function and no other reference to it, but we may still need the
// descriptor for references from other packages.
void
Function_declaration::build_backend_descriptor(Gogo* gogo)
{
  if (this->descriptor_ != NULL)
    {
      Translate_context context(gogo, NULL, NULL, NULL);
      this->descriptor_->get_tree(&context);
    }
}

// Return the integer type to use for a size.

GO_EXTERN_C
tree
go_type_for_size(unsigned int bits, int unsignedp)
{
  const char* name;
  switch (bits)
    {
    case 8:
      name = unsignedp ? "uint8" : "int8";
      break;
    case 16:
      name = unsignedp ? "uint16" : "int16";
      break;
    case 32:
      name = unsignedp ? "uint32" : "int32";
      break;
    case 64:
      name = unsignedp ? "uint64" : "int64";
      break;
    default:
      if (bits == POINTER_SIZE && unsignedp)
	name = "uintptr";
      else
	return NULL_TREE;
    }
  Type* type = Type::lookup_integer_type(name);
  return type_to_tree(type->get_backend(go_get_gogo()));
}

// Return the type to use for a mode.

GO_EXTERN_C
tree
go_type_for_mode(enum machine_mode mode, int unsignedp)
{
  // FIXME: This static_cast should be in machmode.h.
  enum mode_class mc = static_cast<enum mode_class>(GET_MODE_CLASS(mode));
  if (mc == MODE_INT)
    return go_type_for_size(GET_MODE_BITSIZE(mode), unsignedp);
  else if (mc == MODE_FLOAT)
    {
      Type* type;
      switch (GET_MODE_BITSIZE (mode))
	{
	case 32:
	  type = Type::lookup_float_type("float32");
	  break;
	case 64:
	  type = Type::lookup_float_type("float64");
	  break;
	default:
	  // We have to check for long double in order to support
	  // i386 excess precision.
	  if (mode == TYPE_MODE(long_double_type_node))
	    return long_double_type_node;
	  return NULL_TREE;
	}
      return type_to_tree(type->get_backend(go_get_gogo()));
    }
  else if (mc == MODE_COMPLEX_FLOAT)
    {
      Type *type;
      switch (GET_MODE_BITSIZE (mode))
	{
	case 64:
	  type = Type::lookup_complex_type("complex64");
	  break;
	case 128:
	  type = Type::lookup_complex_type("complex128");
	  break;
	default:
	  // We have to check for long double in order to support
	  // i386 excess precision.
	  if (mode == TYPE_MODE(complex_long_double_type_node))
	    return complex_long_double_type_node;
	  return NULL_TREE;
	}
      return type_to_tree(type->get_backend(go_get_gogo()));
    }
  else
    return NULL_TREE;
}

// Build a constructor for a slice.  SLICE_TYPE_TREE is the type of
// the slice.  VALUES is the value pointer and COUNT is the number of
// entries.  If CAPACITY is not NULL, it is the capacity; otherwise
// the capacity and the count are the same.

tree
Gogo::slice_constructor(tree slice_type_tree, tree values, tree count,
			tree capacity)
{
  go_assert(TREE_CODE(slice_type_tree) == RECORD_TYPE);

  vec<constructor_elt, va_gc> *init;
  vec_alloc(init, 3);

  tree field = TYPE_FIELDS(slice_type_tree);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__values") == 0);
  constructor_elt empty = {NULL, NULL};
  constructor_elt* elt = init->quick_push(empty);
  elt->index = field;
  go_assert(TYPE_MAIN_VARIANT(TREE_TYPE(field))
	     == TYPE_MAIN_VARIANT(TREE_TYPE(values)));
  elt->value = values;

  count = fold_convert(sizetype, count);
  if (capacity == NULL_TREE)
    {
      count = save_expr(count);
      capacity = count;
    }

  field = DECL_CHAIN(field);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__count") == 0);
  elt = init->quick_push(empty);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), count);

  field = DECL_CHAIN(field);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__capacity") == 0);
  elt = init->quick_push(empty);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), capacity);

  return build_constructor(slice_type_tree, init);
}

// Mark a function as a builtin library function.

void
Gogo::mark_fndecl_as_builtin_library(tree fndecl)
{
  DECL_EXTERNAL(fndecl) = 1;
  TREE_PUBLIC(fndecl) = 1;
  DECL_ARTIFICIAL(fndecl) = 1;
  TREE_NOTHROW(fndecl) = 1;
  DECL_VISIBILITY(fndecl) = VISIBILITY_DEFAULT;
  DECL_VISIBILITY_SPECIFIED(fndecl) = 1;
}

// Build a call to a builtin function.

tree
Gogo::call_builtin(tree* pdecl, Location location, const char* name,
		   int nargs, tree rettype, ...)
{
  if (rettype == error_mark_node)
    return error_mark_node;

  tree* types = new tree[nargs];
  tree* args = new tree[nargs];

  va_list ap;
  va_start(ap, rettype);
  for (int i = 0; i < nargs; ++i)
    {
      types[i] = va_arg(ap, tree);
      args[i] = va_arg(ap, tree);
      if (types[i] == error_mark_node || args[i] == error_mark_node)
	{
	  delete[] types;
	  delete[] args;
	  return error_mark_node;
	}
    }
  va_end(ap);

  if (*pdecl == NULL_TREE)
    {
      tree fnid = get_identifier(name);

      tree argtypes = NULL_TREE;
      tree* pp = &argtypes;
      for (int i = 0; i < nargs; ++i)
	{
	  *pp = tree_cons(NULL_TREE, types[i], NULL_TREE);
	  pp = &TREE_CHAIN(*pp);
	}
      *pp = void_list_node;

      tree fntype = build_function_type(rettype, argtypes);

      *pdecl = build_decl(BUILTINS_LOCATION, FUNCTION_DECL, fnid, fntype);
      Gogo::mark_fndecl_as_builtin_library(*pdecl);
      go_preserve_from_gc(*pdecl);
    }

  tree fnptr = build_fold_addr_expr(*pdecl);
  if (CAN_HAVE_LOCATION_P(fnptr))
    SET_EXPR_LOCATION(fnptr, location.gcc_location());

  tree ret = build_call_array(rettype, fnptr, nargs, args);
  SET_EXPR_LOCATION(ret, location.gcc_location());

  delete[] types;
  delete[] args;

  return ret;
}
