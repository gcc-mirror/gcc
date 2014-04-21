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

// Get the name to use for the import control function.  If there is a
// global function or variable, then we know that that name must be
// unique in the link, and we use it as the basis for our name.

const std::string&
Gogo::get_init_fn_name()
{
  if (this->init_fn_name_.empty())
    {
      go_assert(this->package_ != NULL);
      if (this->is_main_package())
	{
	  // Use a name which the runtime knows.
	  this->init_fn_name_ = "__go_init_main";
	}
      else
	{
	  std::string s = this->pkgpath_symbol();
	  s.append("..import");
	  this->init_fn_name_ = s;
	}
    }

  return this->init_fn_name_;
}

// Add statements to INIT_STMT_LIST which run the initialization
// functions for imported packages.  This is only used for the "main"
// package.

void
Gogo::init_imports(tree* init_stmt_list)
{
  go_assert(this->is_main_package());

  if (this->imported_init_fns_.empty())
    return;

  tree fntype = build_function_type(void_type_node, void_list_node);

  // We must call them in increasing priority order.
  std::vector<Import_init> v;
  for (std::set<Import_init>::const_iterator p =
	 this->imported_init_fns_.begin();
       p != this->imported_init_fns_.end();
       ++p)
    v.push_back(*p);
  std::sort(v.begin(), v.end());

  for (std::vector<Import_init>::const_iterator p = v.begin();
       p != v.end();
       ++p)
    {
      std::string user_name = p->package_name() + ".init";
      tree decl = build_decl(UNKNOWN_LOCATION, FUNCTION_DECL,
			     get_identifier_from_string(user_name),
			     fntype);
      const std::string& init_name(p->init_name());
      SET_DECL_ASSEMBLER_NAME(decl, get_identifier_from_string(init_name));
      TREE_PUBLIC(decl) = 1;
      DECL_EXTERNAL(decl) = 1;
      append_to_statement_list(build_call_expr(decl, 0), init_stmt_list);
    }
}

// Register global variables with the garbage collector.  We need to
// register all variables which can hold a pointer value.  They become
// roots during the mark phase.  We build a struct that is easy to
// hook into a list of roots.

// struct __go_gc_root_list
// {
//   struct __go_gc_root_list* __next;
//   struct __go_gc_root
//   {
//     void* __decl;
//     size_t __size;
//   } __roots[];
// };

// The last entry in the roots array has a NULL decl field.

void
Gogo::register_gc_vars(const std::vector<Named_object*>& var_gc,
		       tree* init_stmt_list)
{
  if (var_gc.empty())
    return;

  size_t count = var_gc.size();

  tree root_type = Gogo::builtin_struct(NULL, "__go_gc_root", NULL_TREE, 2,
					"__next",
					ptr_type_node,
					"__size",
					sizetype);

  tree index_type = build_index_type(size_int(count));
  tree array_type = build_array_type(root_type, index_type);

  tree root_list_type = make_node(RECORD_TYPE);
  root_list_type = Gogo::builtin_struct(NULL, "__go_gc_root_list",
					root_list_type, 2,
					"__next",
					build_pointer_type(root_list_type),
					"__roots",
					array_type);

  // Build an initialier for the __roots array.

  vec<constructor_elt, va_gc> *roots_init;
  vec_alloc(roots_init, count + 1);

  size_t i = 0;
  for (std::vector<Named_object*>::const_iterator p = var_gc.begin();
       p != var_gc.end();
       ++p, ++i)
    {
      vec<constructor_elt, va_gc> *init;
      vec_alloc(init, 2);

      constructor_elt empty = {NULL, NULL};
      constructor_elt* elt = init->quick_push(empty);
      tree field = TYPE_FIELDS(root_type);
      elt->index = field;
      Bvariable* bvar = (*p)->get_backend_variable(this, NULL);
      tree decl = var_to_tree(bvar);
      go_assert(TREE_CODE(decl) == VAR_DECL);
      elt->value = build_fold_addr_expr(decl);

      elt = init->quick_push(empty);
      field = DECL_CHAIN(field);
      elt->index = field;
      elt->value = DECL_SIZE_UNIT(decl);

      elt = roots_init->quick_push(empty);
      elt->index = size_int(i);
      elt->value = build_constructor(root_type, init);
    }

  // The list ends with a NULL entry.

  vec<constructor_elt, va_gc> *init;
  vec_alloc(init, 2);

  constructor_elt empty = {NULL, NULL};
  constructor_elt* elt = init->quick_push(empty);
  tree field = TYPE_FIELDS(root_type);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), null_pointer_node);

  elt = init->quick_push(empty);
  field = DECL_CHAIN(field);
  elt->index = field;
  elt->value = size_zero_node;

  elt = roots_init->quick_push(empty);
  elt->index = size_int(i);
  elt->value = build_constructor(root_type, init);

  // Build a constructor for the struct.

  vec<constructor_elt, va_gc> *root_list_init;
  vec_alloc(root_list_init, 2);

  elt = root_list_init->quick_push(empty);
  field = TYPE_FIELDS(root_list_type);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), null_pointer_node);

  elt = root_list_init->quick_push(empty);
  field = DECL_CHAIN(field);
  elt->index = field;
  elt->value = build_constructor(array_type, roots_init);

  // Build a decl to register.

  tree decl = build_decl(BUILTINS_LOCATION, VAR_DECL,
			 create_tmp_var_name("gc"), root_list_type);
  DECL_EXTERNAL(decl) = 0;
  TREE_PUBLIC(decl) = 0;
  TREE_STATIC(decl) = 1;
  DECL_ARTIFICIAL(decl) = 1;
  DECL_INITIAL(decl) = build_constructor(root_list_type, root_list_init);
  rest_of_decl_compilation(decl, 1, 0);

  static tree register_gc_fndecl;
  tree call = Gogo::call_builtin(&register_gc_fndecl,
                                 Linemap::predeclared_location(),
                                 "__go_register_gc_roots",
				 1,
				 void_type_node,
				 build_pointer_type(root_list_type),
				 build_fold_addr_expr(decl));
  if (call != error_mark_node)
    append_to_statement_list(call, init_stmt_list);
}

// Build the decl for the initialization function.

tree
Gogo::initialization_function_decl()
{
  // The tedious details of building your own function.  There doesn't
  // seem to be a helper function for this.
  std::string name = this->package_name() + ".init";
  tree fndecl = build_decl(this->package_->location().gcc_location(),
			   FUNCTION_DECL, get_identifier_from_string(name),
			   build_function_type(void_type_node,
					       void_list_node));
  const std::string& asm_name(this->get_init_fn_name());
  SET_DECL_ASSEMBLER_NAME(fndecl, get_identifier_from_string(asm_name));

  tree resdecl = build_decl(this->package_->location().gcc_location(),
			    RESULT_DECL, NULL_TREE, void_type_node);
  DECL_ARTIFICIAL(resdecl) = 1;
  DECL_CONTEXT(resdecl) = fndecl;
  DECL_RESULT(fndecl) = resdecl;

  TREE_STATIC(fndecl) = 1;
  TREE_USED(fndecl) = 1;
  DECL_ARTIFICIAL(fndecl) = 1;
  TREE_PUBLIC(fndecl) = 1;

  DECL_INITIAL(fndecl) = make_node(BLOCK);
  TREE_USED(DECL_INITIAL(fndecl)) = 1;

  return fndecl;
}

// Create the magic initialization function.  INIT_STMT_LIST is the
// code that it needs to run.

void
Gogo::write_initialization_function(tree fndecl, tree init_stmt_list)
{
  // Make sure that we thought we needed an initialization function,
  // as otherwise we will not have reported it in the export data.
  go_assert(this->is_main_package() || this->need_init_fn_);

  if (fndecl == NULL_TREE)
    fndecl = this->initialization_function_decl();

  DECL_SAVED_TREE(fndecl) = init_stmt_list;

  if (DECL_STRUCT_FUNCTION(fndecl) == NULL)
    push_struct_function(fndecl);
  else
    push_cfun(DECL_STRUCT_FUNCTION(fndecl));
  cfun->function_start_locus = this->package_->location().gcc_location();
  cfun->function_end_locus = cfun->function_start_locus;

  gimplify_function_tree(fndecl);

  cgraph_add_new_function(fndecl, false);

  pop_cfun();
}

// Search for references to VAR in any statements or called functions.

class Find_var : public Traverse
{
 public:
  // A hash table we use to avoid looping.  The index is the name of a
  // named object.  We only look through objects defined in this
  // package.
  typedef Unordered_set(const void*) Seen_objects;

  Find_var(Named_object* var, Seen_objects* seen_objects)
    : Traverse(traverse_expressions),
      var_(var), seen_objects_(seen_objects), found_(false)
  { }

  // Whether the variable was found.
  bool
  found() const
  { return this->found_; }

  int
  expression(Expression**);

 private:
  // The variable we are looking for.
  Named_object* var_;
  // Names of objects we have already seen.
  Seen_objects* seen_objects_;
  // True if the variable was found.
  bool found_;
};

// See if EXPR refers to VAR, looking through function calls and
// variable initializations.

int
Find_var::expression(Expression** pexpr)
{
  Expression* e = *pexpr;

  Var_expression* ve = e->var_expression();
  if (ve != NULL)
    {
      Named_object* v = ve->named_object();
      if (v == this->var_)
	{
	  this->found_ = true;
	  return TRAVERSE_EXIT;
	}

      if (v->is_variable() && v->package() == NULL)
	{
	  Expression* init = v->var_value()->init();
	  if (init != NULL)
	    {
	      std::pair<Seen_objects::iterator, bool> ins =
		this->seen_objects_->insert(v);
	      if (ins.second)
		{
		  // This is the first time we have seen this name.
		  if (Expression::traverse(&init, this) == TRAVERSE_EXIT)
		    return TRAVERSE_EXIT;
		}
	    }
	}
    }

  // We traverse the code of any function we see.  Note that this
  // means that we will traverse the code of a function whose address
  // is taken even if it is not called.
  Func_expression* fe = e->func_expression();
  if (fe != NULL)
    {
      const Named_object* f = fe->named_object();
      if (f->is_function() && f->package() == NULL)
	{
	  std::pair<Seen_objects::iterator, bool> ins =
	    this->seen_objects_->insert(f);
	  if (ins.second)
	    {
	      // This is the first time we have seen this name.
	      if (f->func_value()->block()->traverse(this) == TRAVERSE_EXIT)
		return TRAVERSE_EXIT;
	    }
	}
    }

  Temporary_reference_expression* tre = e->temporary_reference_expression();
  if (tre != NULL)
    {
      Temporary_statement* ts = tre->statement();
      Expression* init = ts->init();
      if (init != NULL)
	{
	  std::pair<Seen_objects::iterator, bool> ins =
	    this->seen_objects_->insert(ts);
	  if (ins.second)
	    {
	      // This is the first time we have seen this temporary
	      // statement.
	      if (Expression::traverse(&init, this) == TRAVERSE_EXIT)
		return TRAVERSE_EXIT;
	    }
	}
    }

  return TRAVERSE_CONTINUE;
}

// Return true if EXPR, PREINIT, or DEP refers to VAR.

static bool
expression_requires(Expression* expr, Block* preinit, Named_object* dep,
		    Named_object* var)
{
  Find_var::Seen_objects seen_objects;
  Find_var find_var(var, &seen_objects);
  if (expr != NULL)
    Expression::traverse(&expr, &find_var);
  if (preinit != NULL)
    preinit->traverse(&find_var);
  if (dep != NULL)
    {
      Expression* init = dep->var_value()->init();
      if (init != NULL)
	Expression::traverse(&init, &find_var);
      if (dep->var_value()->has_pre_init())
	dep->var_value()->preinit()->traverse(&find_var);
    }

  return find_var.found();
}

// Sort variable initializations.  If the initialization expression
// for variable A refers directly or indirectly to the initialization
// expression for variable B, then we must initialize B before A.

class Var_init
{
 public:
  Var_init()
    : var_(NULL), init_(NULL)
  { }

  Var_init(Named_object* var, Bstatement* init)
    : var_(var), init_(init)
  { }

  // Return the variable.
  Named_object*
  var() const
  { return this->var_; }

  // Return the initialization expression.
  Bstatement*
  init() const
  { return this->init_; }

 private:
  // The variable being initialized.
  Named_object* var_;
  // The initialization statement.
  Bstatement* init_;
};

typedef std::list<Var_init> Var_inits;

// Sort the variable initializations.  The rule we follow is that we
// emit them in the order they appear in the array, except that if the
// initialization expression for a variable V1 depends upon another
// variable V2 then we initialize V1 after V2.

static void
sort_var_inits(Gogo* gogo, Var_inits* var_inits)
{
  typedef std::pair<Named_object*, Named_object*> No_no;
  typedef std::map<No_no, bool> Cache;
  Cache cache;

  Var_inits ready;
  while (!var_inits->empty())
    {
      Var_inits::iterator p1 = var_inits->begin();
      Named_object* var = p1->var();
      Expression* init = var->var_value()->init();
      Block* preinit = var->var_value()->preinit();
      Named_object* dep = gogo->var_depends_on(var->var_value());

      // Start walking through the list to see which variables VAR
      // needs to wait for.
      Var_inits::iterator p2 = p1;
      ++p2;

      for (; p2 != var_inits->end(); ++p2)
	{
	  Named_object* p2var = p2->var();
	  No_no key(var, p2var);
	  std::pair<Cache::iterator, bool> ins =
	    cache.insert(std::make_pair(key, false));
	  if (ins.second)
	    ins.first->second = expression_requires(init, preinit, dep, p2var);
	  if (ins.first->second)
	    {
	      // Check for cycles.
	      key = std::make_pair(p2var, var);
	      ins = cache.insert(std::make_pair(key, false));
	      if (ins.second)
		ins.first->second =
		  expression_requires(p2var->var_value()->init(),
				      p2var->var_value()->preinit(),
				      gogo->var_depends_on(p2var->var_value()),
				      var);
	      if (ins.first->second)
		{
		  error_at(var->location(),
			   ("initialization expressions for %qs and "
			    "%qs depend upon each other"),
			   var->message_name().c_str(),
			   p2var->message_name().c_str());
		  inform(p2->var()->location(), "%qs defined here",
			 p2var->message_name().c_str());
		  p2 = var_inits->end();
		}
	      else
		{
		  // We can't emit P1 until P2 is emitted.  Move P1.
		  Var_inits::iterator p3 = p2;
		  ++p3;
		  var_inits->splice(p3, *var_inits, p1);
		}
	      break;
	    }
	}

      if (p2 == var_inits->end())
	{
	  // VAR does not depends upon any other initialization expressions.

	  // Check for a loop of VAR on itself.  We only do this if
	  // INIT is not NULL and there is no dependency; when INIT is
	  // NULL, it means that PREINIT sets VAR, which we will
	  // interpret as a loop.
	  if (init != NULL && dep == NULL
	      && expression_requires(init, preinit, NULL, var))
	    error_at(var->location(),
		     "initialization expression for %qs depends upon itself",
		     var->message_name().c_str());
	  ready.splice(ready.end(), *var_inits, p1);
	}
    }

  // Now READY is the list in the desired initialization order.
  var_inits->swap(ready);
}

// Write out the global definitions.

void
Gogo::write_globals()
{
  this->build_interface_method_tables();

  Bindings* bindings = this->current_bindings();

  for (Bindings::const_declarations_iterator p = bindings->begin_declarations();
       p != bindings->end_declarations();
       ++p)
    {
      // If any function declarations needed a descriptor, make sure
      // we build it.
      Named_object* no = p->second;
      if (no->is_function_declaration())
	no->func_declaration_value()->build_backend_descriptor(this);
    }

  size_t count_definitions = bindings->size_definitions();
  size_t count = count_definitions;

  tree* vec = new tree[count];

  tree init_fndecl = NULL_TREE;
  tree init_stmt_list = NULL_TREE;

  if (this->is_main_package())
    this->init_imports(&init_stmt_list);

  // A list of variable initializations.
  Var_inits var_inits;

  // A list of variables which need to be registered with the garbage
  // collector.
  std::vector<Named_object*> var_gc;
  var_gc.reserve(count);

  tree var_init_stmt_list = NULL_TREE;
  size_t i = 0;
  for (Bindings::const_definitions_iterator p = bindings->begin_definitions();
       p != bindings->end_definitions();
       ++p, ++i)
    {
      Named_object* no = *p;

      go_assert(i < count);

      go_assert(!no->is_type_declaration() && !no->is_function_declaration());
      // There is nothing to do for a package.
      if (no->is_package())
	{
	  --i;
	  --count;
	  continue;
	}

      // There is nothing to do for an object which was imported from
      // a different package into the global scope.
      if (no->package() != NULL)
	{
	  --i;
	  --count;
	  continue;
	}

      // Skip blank named functions and constants.
      if ((no->is_function() && no->func_value()->is_sink())
	  || (no->is_const() && no->const_value()->is_sink()))
        {
          --i;
          --count;
          continue;
        }

      // There is nothing useful we can output for constants which
      // have ideal or non-integral type.
      if (no->is_const())
	{
	  Type* type = no->const_value()->type();
	  if (type == NULL)
	    type = no->const_value()->expr()->type();
	  if (type->is_abstract() || type->integer_type() == NULL)
	    {
	      --i;
	      --count;
	      continue;
	    }
	}

      if (!no->is_variable())
	{
	  vec[i] = no->get_tree(this, NULL);
	  if (vec[i] == error_mark_node)
	    {
	      go_assert(saw_errors());
	      --i;
	      --count;
	      continue;
	    }
	}
      else
	{
	  Bvariable* var = no->get_backend_variable(this, NULL);
	  vec[i] = var_to_tree(var);
	  if (vec[i] == error_mark_node)
	    {
	      go_assert(saw_errors());
	      --i;
	      --count;
	      continue;
	    }

	  // Check for a sink variable, which may be used to run an
	  // initializer purely for its side effects.
	  bool is_sink = no->name()[0] == '_' && no->name()[1] == '.';

          Bstatement* var_init_stmt = NULL;
	  if (!no->var_value()->has_pre_init())
	    {
              Bexpression* var_binit = no->var_value()->get_init(this, NULL);
              if (var_binit == NULL)
		;
	      else if (TREE_CONSTANT(expr_to_tree(var_binit)))
		{
		  if (expression_requires(no->var_value()->init(), NULL,
					  this->var_depends_on(no->var_value()),
					  no))
		    error_at(no->location(),
			     "initialization expression for %qs depends "
			     "upon itself",
			     no->message_name().c_str());
		  this->backend()->global_variable_set_init(var, var_binit);
		}
	      else if (is_sink)
		var_init_stmt =
                    this->backend()->expression_statement(var_binit);
	      else
                {
                  Location loc = no->var_value()->location();
                  Bexpression* var_expr =
                      this->backend()->var_expression(var, loc);
                  var_init_stmt =
                      this->backend()->assignment_statement(var_expr, var_binit,
                                                            loc);
                }
	    }
	  else
	    {
	      // We are going to create temporary variables which
	      // means that we need an fndecl.
	      if (init_fndecl == NULL_TREE)
		init_fndecl = this->initialization_function_decl();
	      if (DECL_STRUCT_FUNCTION(init_fndecl) == NULL)
		push_struct_function(init_fndecl);
	      else
		push_cfun(DECL_STRUCT_FUNCTION(init_fndecl));
	      Bvariable* var_decl = is_sink ? NULL : var;
              var_init_stmt =
                  no->var_value()->get_init_block(this, NULL, var_decl);

	      pop_cfun();
	    }

	  if (var_init_stmt != NULL)
	    {
	      if (no->var_value()->init() == NULL
		  && !no->var_value()->has_pre_init())
		append_to_statement_list(stat_to_tree(var_init_stmt),
                                         &var_init_stmt_list);
	      else
		var_inits.push_back(Var_init(no, var_init_stmt));
	    }
	  else if (this->var_depends_on(no->var_value()) != NULL)
	    {
	      // This variable is initialized from something that is
	      // not in its init or preinit.  This variable needs to
	      // participate in dependency analysis sorting, in case
	      // some other variable depends on this one.
              Btype* int_btype =
                  Type::lookup_integer_type("int")->get_backend(this);
              Bexpression* zero = this->backend()->zero_expression(int_btype);
              Bstatement* zero_stmt =
                  this->backend()->expression_statement(zero);
	      var_inits.push_back(Var_init(no, zero_stmt));
	    }

	  if (!is_sink && no->var_value()->type()->has_pointer())
	    var_gc.push_back(no);
	}
    }

  // Register global variables with the garbage collector.
  this->register_gc_vars(var_gc, &init_stmt_list);

  // Simple variable initializations, after all variables are
  // registered.
  append_to_statement_list(var_init_stmt_list, &init_stmt_list);

  // Complex variable initializations, first sorting them into a
  // workable order.
  if (!var_inits.empty())
    {
      sort_var_inits(this, &var_inits);
      for (Var_inits::const_iterator p = var_inits.begin();
	   p != var_inits.end();
	   ++p)
	append_to_statement_list(stat_to_tree(p->init()), &init_stmt_list);
    }

  // After all the variables are initialized, call the "init"
  // functions if there are any.
  for (std::vector<Named_object*>::const_iterator p =
	 this->init_functions_.begin();
       p != this->init_functions_.end();
       ++p)
    {
      tree decl = (*p)->get_tree(this, NULL);
      tree call = build_call_expr(decl, 0);
      append_to_statement_list(call, &init_stmt_list);
    }

  // Set up a magic function to do all the initialization actions.
  // This will be called if this package is imported.
  if (init_stmt_list != NULL_TREE
      || this->need_init_fn_
      || this->is_main_package())
    this->write_initialization_function(init_fndecl, init_stmt_list);

  // We should not have seen any new bindings created during the
  // conversion.
  go_assert(count_definitions == this->current_bindings()->size_definitions());

  // Pass everything back to the middle-end.

  wrapup_global_declarations(vec, count);

  finalize_compilation_unit();

  check_global_declarations(vec, count);
  emit_debug_global_declarations(vec, count);

  delete[] vec;
}

// Get a tree for a named object.

tree
Named_object::get_tree(Gogo* gogo, Named_object* function)
{
  if (this->tree_ != NULL_TREE)
    return this->tree_;

  if (Gogo::is_erroneous_name(this->name_))
    {
      this->tree_ = error_mark_node;
      return error_mark_node;
    }

  tree decl;
  switch (this->classification_)
    {
    case NAMED_OBJECT_CONST:
      {
	Translate_context subcontext(gogo, function, NULL, NULL);
	Type* type = this->u_.const_value->type();
	Location loc = this->location();

	Expression* const_ref = Expression::make_const_reference(this, loc);
        Bexpression* const_decl =
	  tree_to_expr(const_ref->get_tree(&subcontext));
	if (type != NULL && type->is_numeric_type())
	  {
	    Btype* btype = type->get_backend(gogo);
	    std::string name = this->get_id(gogo);
            const_decl =
	      gogo->backend()->named_constant_expression(btype, name,
							 const_decl, loc);
	  }
	decl = expr_to_tree(const_decl);
      }
      break;

    case NAMED_OBJECT_TYPE:
      {
	Named_type* named_type = this->u_.type_value;
	tree type_tree = type_to_tree(named_type->get_backend(gogo));
	if (type_tree == error_mark_node)
	  decl = error_mark_node;
	else
	  {
	    decl = TYPE_NAME(type_tree);
	    go_assert(decl != NULL_TREE);

	    // We need to produce a type descriptor for every named
	    // type, and for a pointer to every named type, since
	    // other files or packages might refer to them.  We need
	    // to do this even for hidden types, because they might
	    // still be returned by some function.  Simply calling the
	    // type_descriptor method is enough to create the type
	    // descriptor, even though we don't do anything with it.
	    if (this->package_ == NULL)
	      {
		named_type->
                  type_descriptor_pointer(gogo,
					  Linemap::predeclared_location());
		Type* pn = Type::make_pointer_type(named_type);
		pn->type_descriptor_pointer(gogo,
					    Linemap::predeclared_location());
	      }
	  }
      }
      break;

    case NAMED_OBJECT_TYPE_DECLARATION:
      error("reference to undefined type %qs",
	    this->message_name().c_str());
      return error_mark_node;

    case NAMED_OBJECT_VAR:
    case NAMED_OBJECT_RESULT_VAR:
    case NAMED_OBJECT_SINK:
      go_unreachable();

    case NAMED_OBJECT_FUNC:
      {
	Function* func = this->u_.func_value;
	decl = function_to_tree(func->get_or_make_decl(gogo, this));
	if (decl != error_mark_node)
	  {
	    if (func->block() != NULL)
	      {
		if (DECL_STRUCT_FUNCTION(decl) == NULL)
		  push_struct_function(decl);
		else
		  push_cfun(DECL_STRUCT_FUNCTION(decl));

		cfun->function_start_locus = func->location().gcc_location();
		cfun->function_end_locus =
                  func->block()->end_location().gcc_location();

		func->build(gogo, this);

		gimplify_function_tree(decl);

		cgraph_finalize_function(decl, true);

		pop_cfun();
	      }
	  }
      }
      break;

    case NAMED_OBJECT_ERRONEOUS:
      decl = error_mark_node;
      break;

    default:
      go_unreachable();
    }

  if (TREE_TYPE(decl) == error_mark_node)
    decl = error_mark_node;

  tree ret = decl;

  this->tree_ = ret;

  if (ret != error_mark_node)
    go_preserve_from_gc(ret);

  return ret;
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

// Return the function's decl after it has been built.

tree
Function::get_decl() const
{
  go_assert(this->fndecl_ != NULL);
  return function_to_tree(this->fndecl_);
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

// Build a builtin struct with a list of fields.  The name is
// STRUCT_NAME.  STRUCT_TYPE is NULL_TREE or an empty RECORD_TYPE
// node; this exists so that the struct can have fields which point to
// itself.  If PTYPE is not NULL, store the result in *PTYPE.  There
// are NFIELDS fields.  Each field is a name (a const char*) followed
// by a type (a tree).

tree
Gogo::builtin_struct(tree* ptype, const char* struct_name, tree struct_type,
		     int nfields, ...)
{
  if (ptype != NULL && *ptype != NULL_TREE)
    return *ptype;

  va_list ap;
  va_start(ap, nfields);

  tree fields = NULL_TREE;
  for (int i = 0; i < nfields; ++i)
    {
      const char* field_name = va_arg(ap, const char*);
      tree type = va_arg(ap, tree);
      if (type == error_mark_node)
	{
	  if (ptype != NULL)
	    *ptype = error_mark_node;
	  return error_mark_node;
	}
      tree field = build_decl(BUILTINS_LOCATION, FIELD_DECL,
			      get_identifier(field_name), type);
      DECL_CHAIN(field) = fields;
      fields = field;
    }

  va_end(ap);

  if (struct_type == NULL_TREE)
    struct_type = make_node(RECORD_TYPE);
  finish_builtin_struct(struct_type, struct_name, fields, NULL_TREE);

  if (ptype != NULL)
    {
      go_preserve_from_gc(struct_type);
      *ptype = struct_type;
    }

  return struct_type;
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
