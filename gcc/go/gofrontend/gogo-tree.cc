// gogo-tree.cc -- convert Go frontend Gogo IR to gcc trees.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include <gmp.h>

#ifndef ENABLE_BUILD_WITH_CXX
extern "C"
{
#endif

#include "toplev.h"
#include "tree.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "cgraph.h"
#include "langhooks.h"
#include "convert.h"
#include "output.h"
#include "diagnostic.h"

#ifndef ENABLE_BUILD_WITH_CXX
}
#endif

#include "go-c.h"
#include "types.h"
#include "expressions.h"
#include "statements.h"
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
  built_in_decls[bcode] = decl;
  implicit_built_in_decls[bcode] = decl;
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
  define_builtin(BUILT_IN_ADD_AND_FETCH_1, "__sync_fetch_and_add_1", NULL,
		 build_function_type_list(t, p, t, NULL_TREE), false);

  t = go_type_for_size(BITS_PER_UNIT * 2, 1);
  p = build_pointer_type(build_qualified_type(t, TYPE_QUAL_VOLATILE));
  define_builtin (BUILT_IN_ADD_AND_FETCH_2, "__sync_fetch_and_add_2", NULL,
		  build_function_type_list(t, p, t, NULL_TREE), false);

  t = go_type_for_size(BITS_PER_UNIT * 4, 1);
  p = build_pointer_type(build_qualified_type(t, TYPE_QUAL_VOLATILE));
  define_builtin(BUILT_IN_ADD_AND_FETCH_4, "__sync_fetch_and_add_4", NULL,
		 build_function_type_list(t, p, t, NULL_TREE), false);

  t = go_type_for_size(BITS_PER_UNIT * 8, 1);
  p = build_pointer_type(build_qualified_type(t, TYPE_QUAL_VOLATILE));
  define_builtin(BUILT_IN_ADD_AND_FETCH_8, "__sync_fetch_and_add_8", NULL,
		 build_function_type_list(t, p, t, NULL_TREE), false);

  // We use __builtin_expect for magic import functions.
  define_builtin(BUILT_IN_EXPECT, "__builtin_expect", NULL,
		 build_function_type_list(long_integer_type_node,
					  long_integer_type_node,
					  long_integer_type_node,
					  NULL_TREE),
		 true);

  // We use __builtin_memmove for the predeclared copy function.
  define_builtin(BUILT_IN_MEMMOVE, "__builtin_memmove", "memmove",
		 build_function_type_list(ptr_type_node,
					  ptr_type_node,
					  const_ptr_type_node,
					  size_type_node,
					  NULL_TREE),
		 false);

  // We provide sqrt for the math library.
  define_builtin(BUILT_IN_SQRT, "__builtin_sqrt", "sqrt",
		 build_function_type_list(double_type_node,
					  double_type_node,
					  NULL_TREE),
		 true);
  define_builtin(BUILT_IN_SQRTL, "__builtin_sqrtl", "sqrtl",
		 build_function_type_list(long_double_type_node,
					  long_double_type_node,
					  NULL_TREE),
		 true);

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
      gcc_assert(this->package_ != NULL);
      if (this->is_main_package())
	{
	  // Use a name which the runtime knows.
	  this->init_fn_name_ = "__go_init_main";
	}
      else
	{
	  std::string s = this->unique_prefix();
	  s.append(1, '.');
	  s.append(this->package_name());
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
  gcc_assert(this->is_main_package());

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

  VEC(constructor_elt,gc)* roots_init = VEC_alloc(constructor_elt, gc,
						  count + 1);

  size_t i = 0;
  for (std::vector<Named_object*>::const_iterator p = var_gc.begin();
       p != var_gc.end();
       ++p, ++i)
    {
      VEC(constructor_elt,gc)* init = VEC_alloc(constructor_elt, gc, 2);

      constructor_elt* elt = VEC_quick_push(constructor_elt, init, NULL);
      tree field = TYPE_FIELDS(root_type);
      elt->index = field;
      tree decl = (*p)->get_tree(this, NULL);
      gcc_assert(TREE_CODE(decl) == VAR_DECL);
      elt->value = build_fold_addr_expr(decl);

      elt = VEC_quick_push(constructor_elt, init, NULL);
      field = DECL_CHAIN(field);
      elt->index = field;
      elt->value = DECL_SIZE_UNIT(decl);

      elt = VEC_quick_push(constructor_elt, roots_init, NULL);
      elt->index = size_int(i);
      elt->value = build_constructor(root_type, init);
    }

  // The list ends with a NULL entry.

  VEC(constructor_elt,gc)* init = VEC_alloc(constructor_elt, gc, 2);

  constructor_elt* elt = VEC_quick_push(constructor_elt, init, NULL);
  tree field = TYPE_FIELDS(root_type);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), null_pointer_node);

  elt = VEC_quick_push(constructor_elt, init, NULL);
  field = DECL_CHAIN(field);
  elt->index = field;
  elt->value = size_zero_node;

  elt = VEC_quick_push(constructor_elt, roots_init, NULL);
  elt->index = size_int(i);
  elt->value = build_constructor(root_type, init);

  // Build a constructor for the struct.

  VEC(constructor_elt,gc*) root_list_init = VEC_alloc(constructor_elt, gc, 2);

  elt = VEC_quick_push(constructor_elt, root_list_init, NULL);
  field = TYPE_FIELDS(root_list_type);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), null_pointer_node);

  elt = VEC_quick_push(constructor_elt, root_list_init, NULL);
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
  tree call = Gogo::call_builtin(&register_gc_fndecl, BUILTINS_LOCATION,
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
  tree fndecl = build_decl(BUILTINS_LOCATION, FUNCTION_DECL,
			   get_identifier_from_string(name),
			   build_function_type(void_type_node,
					       void_list_node));
  const std::string& asm_name(this->get_init_fn_name());
  SET_DECL_ASSEMBLER_NAME(fndecl, get_identifier_from_string(asm_name));

  tree resdecl = build_decl(BUILTINS_LOCATION, RESULT_DECL, NULL_TREE,
			    void_type_node);
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
  gcc_assert(this->is_main_package() || this->need_init_fn_);

  if (fndecl == NULL_TREE)
    fndecl = this->initialization_function_decl();

  DECL_SAVED_TREE(fndecl) = init_stmt_list;

  current_function_decl = fndecl;
  if (DECL_STRUCT_FUNCTION(fndecl) == NULL)
    push_struct_function(fndecl);
  else
    push_cfun(DECL_STRUCT_FUNCTION(fndecl));
  cfun->function_end_locus = BUILTINS_LOCATION;

  gimplify_function_tree(fndecl);

  cgraph_add_new_function(fndecl, false);
  cgraph_mark_needed_node(cgraph_node(fndecl));

  current_function_decl = NULL_TREE;
  pop_cfun();
}

// Search for references to VAR in any statements or called functions.

class Find_var : public Traverse
{
 public:
  // A hash table we use to avoid looping.  The index is the name of a
  // named object.  We only look through objects defined in this
  // package.
  typedef Unordered_set(std::string) Seen_objects;

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
		this->seen_objects_->insert(v->name());
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
	    this->seen_objects_->insert(f->name());
	  if (ins.second)
	    {
	      // This is the first time we have seen this name.
	      if (f->func_value()->block()->traverse(this) == TRAVERSE_EXIT)
		return TRAVERSE_EXIT;
	    }
	}
    }

  return TRAVERSE_CONTINUE;
}

// Return true if EXPR refers to VAR.

static bool
expression_requires(Expression* expr, Block* preinit, Named_object* var)
{
  Find_var::Seen_objects seen_objects;
  Find_var find_var(var, &seen_objects);
  if (expr != NULL)
    Expression::traverse(&expr, &find_var);
  if (preinit != NULL)
    preinit->traverse(&find_var);
  
  return find_var.found();
}

// Sort variable initializations.  If the initialization expression
// for variable A refers directly or indirectly to the initialization
// expression for variable B, then we must initialize B before A.

class Var_init
{
 public:
  Var_init()
    : var_(NULL), init_(NULL_TREE), waiting_(0)
  { }

  Var_init(Named_object* var, tree init)
    : var_(var), init_(init), waiting_(0)
  { }

  // Return the variable.
  Named_object*
  var() const
  { return this->var_; }

  // Return the initialization expression.
  tree
  init() const
  { return this->init_; }

  // Return the number of variables waiting for this one to be
  // initialized.
  size_t
  waiting() const
  { return this->waiting_; }

  // Increment the number waiting.
  void
  increment_waiting()
  { ++this->waiting_; }

 private:
  // The variable being initialized.
  Named_object* var_;
  // The initialization expression to run.
  tree init_;
  // The number of variables which are waiting for this one.
  size_t waiting_;
};

typedef std::list<Var_init> Var_inits;

// Sort the variable initializations.  The rule we follow is that we
// emit them in the order they appear in the array, except that if the
// initialization expression for a variable V1 depends upon another
// variable V2 then we initialize V1 after V2.

static void
sort_var_inits(Var_inits* var_inits)
{
  Var_inits ready;
  while (!var_inits->empty())
    {
      Var_inits::iterator p1 = var_inits->begin();
      Named_object* var = p1->var();
      Expression* init = var->var_value()->init();
      Block* preinit = var->var_value()->preinit();

      // Start walking through the list to see which variables VAR
      // needs to wait for.  We can skip P1->WAITING variables--that
      // is the number we've already checked.
      Var_inits::iterator p2 = p1;
      ++p2;
      for (size_t i = p1->waiting(); i > 0; --i)
	++p2;

      for (; p2 != var_inits->end(); ++p2)
	{
	  if (expression_requires(init, preinit, p2->var()))
	    {
	      // Check for cycles.
	      if (expression_requires(p2->var()->var_value()->init(),
				      p2->var()->var_value()->preinit(),
				      var))
		{
		  error_at(var->location(),
			   ("initialization expressions for %qs and "
			    "%qs depend upon each other"),
			   var->message_name().c_str(),
			   p2->var()->message_name().c_str());
		  inform(p2->var()->location(), "%qs defined here",
			 p2->var()->message_name().c_str());
		  p2 = var_inits->end();
		}
	      else
		{
		  // We can't emit P1 until P2 is emitted.  Move P1.
		  // Note that the WAITING loop always executes at
		  // least once, which is what we want.
		  p2->increment_waiting();
		  Var_inits::iterator p3 = p2;
		  for (size_t i = p2->waiting(); i > 0; --i)
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
	  // INIT is not NULL; when INIT is NULL, it means that
	  // PREINIT sets VAR, which we will interpret as a loop.
	  if (init != NULL && expression_requires(init, preinit, var))
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
  this->convert_named_types();
  this->build_interface_method_tables();

  Bindings* bindings = this->current_bindings();
  size_t count = bindings->size_definitions();

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

      gcc_assert(!no->is_type_declaration() && !no->is_function_declaration());
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

      // There is nothing useful we can output for constants which
      // have ideal or non-integeral type.
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

      vec[i] = no->get_tree(this, NULL);

      if (vec[i] == error_mark_node)
	{
	  gcc_assert(saw_errors());
	  --i;
	  --count;
	  continue;
	}

      // If a variable is initialized to a non-constant value, do the
      // initialization in an initialization function.
      if (TREE_CODE(vec[i]) == VAR_DECL)
	{
	  gcc_assert(no->is_variable());

	  // Check for a sink variable, which may be used to run
	  // an initializer purely for its side effects.
	  bool is_sink = no->name()[0] == '_' && no->name()[1] == '.';

	  tree var_init_tree = NULL_TREE;
	  if (!no->var_value()->has_pre_init())
	    {
	      tree init = no->var_value()->get_init_tree(this, NULL);
	      if (init == error_mark_node)
		gcc_assert(saw_errors());
	      else if (init == NULL_TREE)
		;
	      else if (TREE_CONSTANT(init))
		DECL_INITIAL(vec[i]) = init;
	      else if (is_sink)
		var_init_tree = init;
	      else
		var_init_tree = fold_build2_loc(no->location(), MODIFY_EXPR,
						void_type_node, vec[i], init);
	    }
	  else
	    {
	      // We are going to create temporary variables which
	      // means that we need an fndecl.
	      if (init_fndecl == NULL_TREE)
		init_fndecl = this->initialization_function_decl();
	      current_function_decl = init_fndecl;
	      if (DECL_STRUCT_FUNCTION(init_fndecl) == NULL)
		push_struct_function(init_fndecl);
	      else
		push_cfun(DECL_STRUCT_FUNCTION(init_fndecl));

	      tree var_decl = is_sink ? NULL_TREE : vec[i];
	      var_init_tree = no->var_value()->get_init_block(this, NULL,
							      var_decl);

	      current_function_decl = NULL_TREE;
	      pop_cfun();
	    }

	  if (var_init_tree != NULL_TREE && var_init_tree != error_mark_node)
	    {
	      if (no->var_value()->init() == NULL
		  && !no->var_value()->has_pre_init())
		append_to_statement_list(var_init_tree, &var_init_stmt_list);
	      else
		var_inits.push_back(Var_init(no, var_init_tree));
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
      sort_var_inits(&var_inits);
      for (Var_inits::const_iterator p = var_inits.begin();
	   p != var_inits.end();
	   ++p)
	append_to_statement_list(p->init(), &init_stmt_list);
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

  // Pass everything back to the middle-end.

  wrapup_global_declarations(vec, count);

  cgraph_finalize_compilation_unit();

  check_global_declarations(vec, count);
  emit_debug_global_declarations(vec, count);

  delete[] vec;
}

// Get a tree for the identifier for a named object.

tree
Named_object::get_id(Gogo* gogo)
{
  std::string decl_name;
  if (this->is_function_declaration()
      && !this->func_declaration_value()->asm_name().empty())
    decl_name = this->func_declaration_value()->asm_name();
  else if ((this->is_variable() && !this->var_value()->is_global())
	   || (this->is_type()
	       && this->type_value()->location() == BUILTINS_LOCATION))
    {
      // We don't need the package name for local variables or builtin
      // types.
      decl_name = Gogo::unpack_hidden_name(this->name_);
    }
  else
    {
      std::string package_name;
      if (this->package_ == NULL)
	package_name = gogo->package_name();
      else
	package_name = this->package_->name();

      decl_name = package_name + '.' + Gogo::unpack_hidden_name(this->name_);

      Function_type* fntype;
      if (this->is_function())
	fntype = this->func_value()->type();
      else if (this->is_function_declaration())
	fntype = this->func_declaration_value()->type();
      else
	fntype = NULL;
      if (fntype != NULL && fntype->is_method())
	{
	  decl_name.push_back('.');
	  decl_name.append(fntype->receiver()->type()->mangled_name(gogo));
	}
    }
  if (this->is_type())
    {
      const Named_object* in_function = this->type_value()->in_function();
      if (in_function != NULL)
	decl_name += '$' + in_function->name();
    }
  return get_identifier_from_string(decl_name);
}

// Get a tree for a named object.

tree
Named_object::get_tree(Gogo* gogo, Named_object* function)
{
  if (this->tree_ != NULL_TREE)
    {
      // If this is a variable whose address is taken, we must rebuild
      // the INDIRECT_REF each time to avoid invalid sharing.
      tree ret = this->tree_;
      if (((this->classification_ == NAMED_OBJECT_VAR
	    && this->var_value()->is_in_heap())
	   || (this->classification_ == NAMED_OBJECT_RESULT_VAR
	       && this->result_var_value()->is_in_heap()))
	  && ret != error_mark_node)
	{
	  gcc_assert(TREE_CODE(ret) == INDIRECT_REF);
	  ret = build_fold_indirect_ref(TREE_OPERAND(ret, 0));
	  TREE_THIS_NOTRAP(ret) = 1;
	}
      return ret;
    }

  tree name;
  if (this->classification_ == NAMED_OBJECT_TYPE)
    name = NULL_TREE;
  else
    name = this->get_id(gogo);
  tree decl;
  switch (this->classification_)
    {
    case NAMED_OBJECT_CONST:
      {
	Named_constant* named_constant = this->u_.const_value;
	Translate_context subcontext(gogo, function, NULL, NULL_TREE);
	tree expr_tree = named_constant->expr()->get_tree(&subcontext);
	if (expr_tree == error_mark_node)
	  decl = error_mark_node;
	else
	  {
	    Type* type = named_constant->type();
	    if (type != NULL && !type->is_abstract())
	      {
		if (!type->is_undefined())
		  expr_tree = fold_convert(type->get_tree(gogo), expr_tree);
		else
		  {
		    // Make sure we report the error.
		    type->base();
		    expr_tree = error_mark_node;
		  }
	      }
	    if (expr_tree == error_mark_node)
	      decl = error_mark_node;
	    else if (INTEGRAL_TYPE_P(TREE_TYPE(expr_tree)))
	      {
		decl = build_decl(named_constant->location(), CONST_DECL,
				  name, TREE_TYPE(expr_tree));
		DECL_INITIAL(decl) = expr_tree;
		TREE_CONSTANT(decl) = 1;
		TREE_READONLY(decl) = 1;
	      }
	    else
	      {
		// A CONST_DECL is only for an enum constant, so we
		// shouldn't use for non-integral types.  Instead we
		// just return the constant itself, rather than a
		// decl.
		decl = expr_tree;
	      }
	  }
      }
      break;

    case NAMED_OBJECT_TYPE:
      {
	Named_type* named_type = this->u_.type_value;
	tree type_tree = named_type->get_tree(gogo);
	if (type_tree == error_mark_node)
	  decl = error_mark_node;
	else
	  {
	    decl = TYPE_NAME(type_tree);
	    gcc_assert(decl != NULL_TREE);

	    // We need to produce a type descriptor for every named
	    // type, and for a pointer to every named type, since
	    // other files or packages might refer to them.  We need
	    // to do this even for hidden types, because they might
	    // still be returned by some function.  Simply calling the
	    // type_descriptor method is enough to create the type
	    // descriptor, even though we don't do anything with it.
	    if (this->package_ == NULL)
	      {
		named_type->type_descriptor_pointer(gogo);
		Type* pn = Type::make_pointer_type(named_type);
		pn->type_descriptor_pointer(gogo);
	      }
	  }
      }
      break;

    case NAMED_OBJECT_TYPE_DECLARATION:
      error("reference to undefined type %qs",
	    this->message_name().c_str());
      return error_mark_node;

    case NAMED_OBJECT_VAR:
      {
	Variable* var = this->u_.var_value;
	Type* type = var->type();
	if (type->is_error_type()
	    || (type->is_undefined()
		&& (!var->is_global() || this->package() == NULL)))
	  {
	    // Force the error for an undefined type, just in case.
	    type->base();
	    decl = error_mark_node;
	  }
	else
	  {
	    tree var_type = type->get_tree(gogo);
	    bool is_parameter = var->is_parameter();
	    if (var->is_receiver() && type->points_to() == NULL)
	      is_parameter = false;
	    if (var->is_in_heap())
	      {
		is_parameter = false;
		var_type = build_pointer_type(var_type);
	      }
	    decl = build_decl(var->location(),
			      is_parameter ? PARM_DECL : VAR_DECL,
			      name, var_type);
	    if (!var->is_global())
	      {
		tree fnid = function->get_id(gogo);
		tree fndecl = function->func_value()->get_or_make_decl(gogo,
								       function,
								       fnid);
		DECL_CONTEXT(decl) = fndecl;
	      }
	    if (is_parameter)
	      DECL_ARG_TYPE(decl) = TREE_TYPE(decl);

	    if (var->is_global())
	      {
		const Package* package = this->package();
		if (package == NULL)
		  TREE_STATIC(decl) = 1;
		else
		  DECL_EXTERNAL(decl) = 1;
		if (!Gogo::is_hidden_name(this->name_))
		  {
		    TREE_PUBLIC(decl) = 1;
		    std::string asm_name = (package == NULL
					    ? gogo->unique_prefix()
					    : package->unique_prefix());
		    asm_name.append(1, '.');
		    asm_name.append(IDENTIFIER_POINTER(name),
				    IDENTIFIER_LENGTH(name));
		    tree asm_id = get_identifier_from_string(asm_name);
		    SET_DECL_ASSEMBLER_NAME(decl, asm_id);
		  }
	      }

	    // FIXME: We should only set this for variables which are
	    // actually used somewhere.
	    TREE_USED(decl) = 1;
	  }
      }
      break;

    case NAMED_OBJECT_RESULT_VAR:
      {
	Result_variable* result = this->u_.result_var_value;
	Type* type = result->type();
	if (type->is_error_type() || type->is_undefined())
	  {
	    // Force the error.
	    type->base();
	    decl = error_mark_node;
	  }
	else
	  {
	    gcc_assert(result->function() == function->func_value());
	    source_location loc = function->location();
	    tree result_type = type->get_tree(gogo);
	    tree init;
	    if (!result->is_in_heap())
	      init = type->get_init_tree(gogo, false);
	    else
	      {
		tree space = gogo->allocate_memory(type,
						   TYPE_SIZE_UNIT(result_type),
						   loc);
		result_type = build_pointer_type(result_type);
		tree subinit = type->get_init_tree(gogo, true);
		if (subinit == NULL_TREE)
		  init = fold_convert_loc(loc, result_type, space);
		else
		  {
		    space = save_expr(space);
		    space = fold_convert_loc(loc, result_type, space);
		    tree spaceref = build_fold_indirect_ref_loc(loc, space);
		    TREE_THIS_NOTRAP(spaceref) = 1;
		    tree set = fold_build2_loc(loc, MODIFY_EXPR, void_type_node,
					       spaceref, subinit);
		    init = fold_build2_loc(loc, COMPOUND_EXPR, TREE_TYPE(space),
					   set, space);
		  }
	      }
	    decl = build_decl(loc, VAR_DECL, name, result_type);
	    tree fnid = function->get_id(gogo);
	    tree fndecl = function->func_value()->get_or_make_decl(gogo,
								   function,
								   fnid);
	    DECL_CONTEXT(decl) = fndecl;
	    DECL_INITIAL(decl) = init;
	    TREE_USED(decl) = 1;
	  }
      }
      break;

    case NAMED_OBJECT_SINK:
      gcc_unreachable();

    case NAMED_OBJECT_FUNC:
      {
	Function* func = this->u_.func_value;
	decl = func->get_or_make_decl(gogo, this, name);
	if (decl != error_mark_node)
	  {
	    if (func->block() != NULL)
	      {
		if (DECL_STRUCT_FUNCTION(decl) == NULL)
		  push_struct_function(decl);
		else
		  push_cfun(DECL_STRUCT_FUNCTION(decl));

		cfun->function_end_locus = func->block()->end_location();

		current_function_decl = decl;

		func->build_tree(gogo, this);

		gimplify_function_tree(decl);

		cgraph_finalize_function(decl, true);

		current_function_decl = NULL_TREE;
		pop_cfun();
	      }
	  }
      }
      break;

    default:
      gcc_unreachable();
    }

  if (TREE_TYPE(decl) == error_mark_node)
    decl = error_mark_node;

  tree ret = decl;

  // If this is a local variable whose address is taken, then we
  // actually store it in the heap.  For uses of the variable we need
  // to return a reference to that heap location.
  if (((this->classification_ == NAMED_OBJECT_VAR
	&& this->var_value()->is_in_heap())
       || (this->classification_ == NAMED_OBJECT_RESULT_VAR
	   && this->result_var_value()->is_in_heap()))
      && ret != error_mark_node)
    {
      gcc_assert(POINTER_TYPE_P(TREE_TYPE(ret)));
      ret = build_fold_indirect_ref(ret);
      TREE_THIS_NOTRAP(ret) = 1;
    }

  this->tree_ = ret;

  if (ret != error_mark_node)
    go_preserve_from_gc(ret);

  return ret;
}

// Get the initial value of a variable as a tree.  This does not
// consider whether the variable is in the heap--it returns the
// initial value as though it were always stored in the stack.

tree
Variable::get_init_tree(Gogo* gogo, Named_object* function)
{
  gcc_assert(this->preinit_ == NULL);
  if (this->init_ == NULL)
    {
      gcc_assert(!this->is_parameter_);
      return this->type_->get_init_tree(gogo, this->is_global_);
    }
  else
    {
      Translate_context context(gogo, function, NULL, NULL_TREE);
      tree rhs_tree = this->init_->get_tree(&context);
      return Expression::convert_for_assignment(&context, this->type(),
						this->init_->type(),
						rhs_tree, this->location());
    }
}

// Get the initial value of a variable when a block is required.
// VAR_DECL is the decl to set; it may be NULL for a sink variable.

tree
Variable::get_init_block(Gogo* gogo, Named_object* function, tree var_decl)
{
  gcc_assert(this->preinit_ != NULL);

  // We want to add the variable assignment to the end of the preinit
  // block.  The preinit block may have a TRY_FINALLY_EXPR and a
  // TRY_CATCH_EXPR; if it does, we want to add to the end of the
  // regular statements.

  Translate_context context(gogo, function, NULL, NULL_TREE);
  tree block_tree = this->preinit_->get_tree(&context);
  if (block_tree == error_mark_node)
    return error_mark_node;
  gcc_assert(TREE_CODE(block_tree) == BIND_EXPR);
  tree statements = BIND_EXPR_BODY(block_tree);
  while (statements != NULL_TREE
	 && (TREE_CODE(statements) == TRY_FINALLY_EXPR
	     || TREE_CODE(statements) == TRY_CATCH_EXPR))
    statements = TREE_OPERAND(statements, 0);

  // It's possible to have pre-init statements without an initializer
  // if the pre-init statements set the variable.
  if (this->init_ != NULL)
    {
      tree rhs_tree = this->init_->get_tree(&context);
      if (rhs_tree == error_mark_node)
	return error_mark_node;
      if (var_decl == NULL_TREE)
	append_to_statement_list(rhs_tree, &statements);
      else
	{
	  tree val = Expression::convert_for_assignment(&context, this->type(),
							this->init_->type(),
							rhs_tree,
							this->location());
	  if (val == error_mark_node)
	    return error_mark_node;
	  tree set = fold_build2_loc(this->location(), MODIFY_EXPR,
				     void_type_node, var_decl, val);
	  append_to_statement_list(set, &statements);
	}
    }

  return block_tree;
}

// Get a tree for a function decl.

tree
Function::get_or_make_decl(Gogo* gogo, Named_object* no, tree id)
{
  if (this->fndecl_ == NULL_TREE)
    {
      tree functype = this->type_->get_tree(gogo);
      if (functype == error_mark_node)
	this->fndecl_ = error_mark_node;
      else
	{
	  // The type of a function comes back as a pointer, but we
	  // want the real function type for a function declaration.
	  gcc_assert(POINTER_TYPE_P(functype));
	  functype = TREE_TYPE(functype);
	  tree decl = build_decl(this->location(), FUNCTION_DECL, id, functype);

	  this->fndecl_ = decl;

	  if (no->package() != NULL)
	    ;
	  else if (this->enclosing_ != NULL || Gogo::is_thunk(no))
	    ;
	  else if (Gogo::unpack_hidden_name(no->name()) == "init"
		   && !this->type_->is_method())
	    ;
	  else if (Gogo::unpack_hidden_name(no->name()) == "main"
		   && gogo->is_main_package())
	    TREE_PUBLIC(decl) = 1;
	  // Methods have to be public even if they are hidden because
	  // they can be pulled into type descriptors when using
	  // anonymous fields.
	  else if (!Gogo::is_hidden_name(no->name())
		   || this->type_->is_method())
	    {
	      TREE_PUBLIC(decl) = 1;
	      std::string asm_name = gogo->unique_prefix();
	      asm_name.append(1, '.');
	      asm_name.append(IDENTIFIER_POINTER(id), IDENTIFIER_LENGTH(id));
	      SET_DECL_ASSEMBLER_NAME(decl,
				      get_identifier_from_string(asm_name));
	    }

	  // Why do we have to do this in the frontend?
	  tree restype = TREE_TYPE(functype);
	  tree resdecl = build_decl(this->location(), RESULT_DECL, NULL_TREE,
				    restype);
	  DECL_ARTIFICIAL(resdecl) = 1;
	  DECL_IGNORED_P(resdecl) = 1;
	  DECL_CONTEXT(resdecl) = decl;
	  DECL_RESULT(decl) = resdecl;

	  if (this->enclosing_ != NULL)
	    DECL_STATIC_CHAIN(decl) = 1;

	  // If a function calls the predeclared recover function, we
	  // can't inline it, because recover behaves differently in a
	  // function passed directly to defer.
	  if (this->calls_recover_ && !this->is_recover_thunk_)
	    DECL_UNINLINABLE(decl) = 1;

	  // If this is a thunk created to call a function which calls
	  // the predeclared recover function, we need to disable
	  // stack splitting for the thunk.
	  if (this->is_recover_thunk_)
	    {
	      tree attr = get_identifier("__no_split_stack__");
	      DECL_ATTRIBUTES(decl) = tree_cons(attr, NULL_TREE, NULL_TREE);
	    }

	  go_preserve_from_gc(decl);

	  if (this->closure_var_ != NULL)
	    {
	      push_struct_function(decl);

	      tree closure_decl = this->closure_var_->get_tree(gogo, no);
	      if (closure_decl == error_mark_node)
		this->fndecl_ = error_mark_node;
	      else
		{
		  DECL_ARTIFICIAL(closure_decl) = 1;
		  DECL_IGNORED_P(closure_decl) = 1;
		  TREE_USED(closure_decl) = 1;
		  DECL_ARG_TYPE(closure_decl) = TREE_TYPE(closure_decl);
		  TREE_READONLY(closure_decl) = 1;

		  DECL_STRUCT_FUNCTION(decl)->static_chain_decl = closure_decl;
		}

	      pop_cfun();
	    }
	}
    }
  return this->fndecl_;
}

// Get a tree for a function declaration.

tree
Function_declaration::get_or_make_decl(Gogo* gogo, Named_object* no, tree id)
{
  if (this->fndecl_ == NULL_TREE)
    {
      // Let Go code use an asm declaration to pick up a builtin
      // function.
      if (!this->asm_name_.empty())
	{
	  std::map<std::string, tree>::const_iterator p =
	    builtin_functions.find(this->asm_name_);
	  if (p != builtin_functions.end())
	    {
	      this->fndecl_ = p->second;
	      return this->fndecl_;
	    }
	}

      tree functype = this->fntype_->get_tree(gogo);
      tree decl;
      if (functype == error_mark_node)
	decl = error_mark_node;
      else
	{
	  // The type of a function comes back as a pointer, but we
	  // want the real function type for a function declaration.
	  gcc_assert(POINTER_TYPE_P(functype));
	  functype = TREE_TYPE(functype);
	  decl = build_decl(this->location(), FUNCTION_DECL, id, functype);
	  TREE_PUBLIC(decl) = 1;
	  DECL_EXTERNAL(decl) = 1;

	  if (this->asm_name_.empty())
	    {
	      std::string asm_name = (no->package() == NULL
				      ? gogo->unique_prefix()
				      : no->package()->unique_prefix());
	      asm_name.append(1, '.');
	      asm_name.append(IDENTIFIER_POINTER(id), IDENTIFIER_LENGTH(id));
	      SET_DECL_ASSEMBLER_NAME(decl,
				      get_identifier_from_string(asm_name));
	    }
	}
      this->fndecl_ = decl;
      go_preserve_from_gc(decl);
    }
  return this->fndecl_;
}

// We always pass the receiver to a method as a pointer.  If the
// receiver is actually declared as a non-pointer type, then we copy
// the value into a local variable, so that it has the right type.  In
// this function we create the real PARM_DECL to use, and set
// DEC_INITIAL of the var_decl to be the value passed in.

tree
Function::make_receiver_parm_decl(Gogo* gogo, Named_object* no, tree var_decl)
{
  if (var_decl == error_mark_node)
    return error_mark_node;
  // If the function takes the address of a receiver which is passed
  // by value, then we will have an INDIRECT_REF here.  We need to get
  // the real variable.
  bool is_in_heap = no->var_value()->is_in_heap();
  tree val_type;
  if (TREE_CODE(var_decl) != INDIRECT_REF)
    {
      gcc_assert(!is_in_heap);
      val_type = TREE_TYPE(var_decl);
    }
  else
    {
      gcc_assert(is_in_heap);
      var_decl = TREE_OPERAND(var_decl, 0);
      if (var_decl == error_mark_node)
	return error_mark_node;
      gcc_assert(POINTER_TYPE_P(TREE_TYPE(var_decl)));
      val_type = TREE_TYPE(TREE_TYPE(var_decl));
    }
  gcc_assert(TREE_CODE(var_decl) == VAR_DECL);
  source_location loc = DECL_SOURCE_LOCATION(var_decl);
  std::string name = IDENTIFIER_POINTER(DECL_NAME(var_decl));
  name += ".pointer";
  tree id = get_identifier_from_string(name);
  tree parm_decl = build_decl(loc, PARM_DECL, id, build_pointer_type(val_type));
  DECL_CONTEXT(parm_decl) = current_function_decl;
  DECL_ARG_TYPE(parm_decl) = TREE_TYPE(parm_decl);

  gcc_assert(DECL_INITIAL(var_decl) == NULL_TREE);
  // The receiver might be passed as a null pointer.
  tree check = fold_build2_loc(loc, NE_EXPR, boolean_type_node, parm_decl,
			       fold_convert_loc(loc, TREE_TYPE(parm_decl),
						null_pointer_node));
  tree ind = build_fold_indirect_ref_loc(loc, parm_decl);
  TREE_THIS_NOTRAP(ind) = 1;
  tree zero_init = no->var_value()->type()->get_init_tree(gogo, false);
  tree init = fold_build3_loc(loc, COND_EXPR, TREE_TYPE(ind),
			      check, ind, zero_init);

  if (is_in_heap)
    {
      tree size = TYPE_SIZE_UNIT(val_type);
      tree space = gogo->allocate_memory(no->var_value()->type(), size,
					 no->location());
      space = save_expr(space);
      space = fold_convert(build_pointer_type(val_type), space);
      tree spaceref = build_fold_indirect_ref_loc(no->location(), space);
      TREE_THIS_NOTRAP(spaceref) = 1;
      tree check = fold_build2_loc(loc, NE_EXPR, boolean_type_node,
				   parm_decl,
				   fold_convert_loc(loc, TREE_TYPE(parm_decl),
						    null_pointer_node));
      tree parmref = build_fold_indirect_ref_loc(no->location(), parm_decl);
      TREE_THIS_NOTRAP(parmref) = 1;
      tree set = fold_build2_loc(loc, MODIFY_EXPR, void_type_node,
				 spaceref, parmref);
      init = fold_build2_loc(loc, COMPOUND_EXPR, TREE_TYPE(space),
			     build3(COND_EXPR, void_type_node,
				    check, set, NULL_TREE),
			     space);
    }

  DECL_INITIAL(var_decl) = init;

  return parm_decl;
}

// If we take the address of a parameter, then we need to copy it into
// the heap.  We will access it as a local variable via an
// indirection.

tree
Function::copy_parm_to_heap(Gogo* gogo, Named_object* no, tree ref)
{
  if (ref == error_mark_node)
    return error_mark_node;

  gcc_assert(TREE_CODE(ref) == INDIRECT_REF);

  tree var_decl = TREE_OPERAND(ref, 0);
  if (var_decl == error_mark_node)
    return error_mark_node;
  gcc_assert(TREE_CODE(var_decl) == VAR_DECL);
  source_location loc = DECL_SOURCE_LOCATION(var_decl);

  std::string name = IDENTIFIER_POINTER(DECL_NAME(var_decl));
  name += ".param";
  tree id = get_identifier_from_string(name);

  tree type = TREE_TYPE(var_decl);
  gcc_assert(POINTER_TYPE_P(type));
  type = TREE_TYPE(type);

  tree parm_decl = build_decl(loc, PARM_DECL, id, type);
  DECL_CONTEXT(parm_decl) = current_function_decl;
  DECL_ARG_TYPE(parm_decl) = type;

  tree size = TYPE_SIZE_UNIT(type);
  tree space = gogo->allocate_memory(no->var_value()->type(), size, loc);
  space = save_expr(space);
  space = fold_convert(TREE_TYPE(var_decl), space);
  tree spaceref = build_fold_indirect_ref_loc(loc, space);
  TREE_THIS_NOTRAP(spaceref) = 1;
  tree init = build2(COMPOUND_EXPR, TREE_TYPE(space),
		     build2(MODIFY_EXPR, void_type_node, spaceref, parm_decl),
		     space);
  DECL_INITIAL(var_decl) = init;

  return parm_decl;
}

// Get a tree for function code.

void
Function::build_tree(Gogo* gogo, Named_object* named_function)
{
  tree fndecl = this->fndecl_;
  gcc_assert(fndecl != NULL_TREE);

  tree params = NULL_TREE;
  tree* pp = &params;

  tree declare_vars = NULL_TREE;
  for (Bindings::const_definitions_iterator p =
	 this->block_->bindings()->begin_definitions();
       p != this->block_->bindings()->end_definitions();
       ++p)
    {
      if ((*p)->is_variable() && (*p)->var_value()->is_parameter())
	{
	  *pp = (*p)->get_tree(gogo, named_function);

	  // We always pass the receiver to a method as a pointer.  If
	  // the receiver is declared as a non-pointer type, then we
	  // copy the value into a local variable.
	  if ((*p)->var_value()->is_receiver()
	      && (*p)->var_value()->type()->points_to() == NULL)
	    {
	      tree parm_decl = this->make_receiver_parm_decl(gogo, *p, *pp);
	      tree var = *pp;
	      if (TREE_CODE(var) == INDIRECT_REF)
		var = TREE_OPERAND(var, 0);
	      if (var != error_mark_node)
		{
		  gcc_assert(TREE_CODE(var) == VAR_DECL);
		  DECL_CHAIN(var) = declare_vars;
		  declare_vars = var;
		}
	      *pp = parm_decl;
	    }
	  else if ((*p)->var_value()->is_in_heap())
	    {
	      // If we take the address of a parameter, then we need
	      // to copy it into the heap.
	      tree parm_decl = this->copy_parm_to_heap(gogo, *p, *pp);
	      if (*pp != error_mark_node)
		{
		  gcc_assert(TREE_CODE(*pp) == INDIRECT_REF);
		  tree var_decl = TREE_OPERAND(*pp, 0);
		  if (var_decl != error_mark_node)
		    {
		      gcc_assert(TREE_CODE(var_decl) == VAR_DECL);
		      DECL_CHAIN(var_decl) = declare_vars;
		      declare_vars = var_decl;
		    }
		}
	      *pp = parm_decl;
	    }

	  if (*pp != error_mark_node)
	    {
	      gcc_assert(TREE_CODE(*pp) == PARM_DECL);
	      pp = &DECL_CHAIN(*pp);
	    }
	}
      else if ((*p)->is_result_variable())
	{
	  tree var_decl = (*p)->get_tree(gogo, named_function);
	  if (var_decl != error_mark_node
	      && (*p)->result_var_value()->is_in_heap())
	    {
	      gcc_assert(TREE_CODE(var_decl) == INDIRECT_REF);
	      var_decl = TREE_OPERAND(var_decl, 0);
	    }
	  if (var_decl != error_mark_node)
	    {
	      gcc_assert(TREE_CODE(var_decl) == VAR_DECL);
	      DECL_CHAIN(var_decl) = declare_vars;
	      declare_vars = var_decl;
	    }
	}
    }
  *pp = NULL_TREE;

  DECL_ARGUMENTS(fndecl) = params;

  if (this->block_ != NULL)
    {
      gcc_assert(DECL_INITIAL(fndecl) == NULL_TREE);

      // Declare variables if necessary.
      tree bind = NULL_TREE;
      if (declare_vars != NULL_TREE)
	{
	  tree block = make_node(BLOCK);
	  BLOCK_SUPERCONTEXT(block) = fndecl;
	  DECL_INITIAL(fndecl) = block;
	  BLOCK_VARS(block) = declare_vars;
	  TREE_USED(block) = 1;
	  bind = build3(BIND_EXPR, void_type_node, BLOCK_VARS(block),
			NULL_TREE, block);
	  TREE_SIDE_EFFECTS(bind) = 1;
	}

      // Build the trees for all the statements in the function.
      Translate_context context(gogo, named_function, NULL, NULL_TREE);
      tree code = this->block_->get_tree(&context);

      tree init = NULL_TREE;
      tree except = NULL_TREE;
      tree fini = NULL_TREE;

      // Initialize variables if necessary.
      for (tree v = declare_vars; v != NULL_TREE; v = DECL_CHAIN(v))
	{
	  tree dv = build1(DECL_EXPR, void_type_node, v);
	  SET_EXPR_LOCATION(dv, DECL_SOURCE_LOCATION(v));
	  append_to_statement_list(dv, &init);
	}

      // If we have a defer stack, initialize it at the start of a
      // function.
      if (this->defer_stack_ != NULL_TREE)
	{
	  tree defer_init = build1(DECL_EXPR, void_type_node,
				   this->defer_stack_);
	  SET_EXPR_LOCATION(defer_init, this->block_->start_location());
	  append_to_statement_list(defer_init, &init);

	  // Clean up the defer stack when we leave the function.
	  this->build_defer_wrapper(gogo, named_function, &except, &fini);
	}

      if (code != NULL_TREE && code != error_mark_node)
	{
	  if (init != NULL_TREE)
	    code = build2(COMPOUND_EXPR, void_type_node, init, code);
	  if (except != NULL_TREE)
	    code = build2(TRY_CATCH_EXPR, void_type_node, code,
			  build2(CATCH_EXPR, void_type_node, NULL, except));
	  if (fini != NULL_TREE)
	    code = build2(TRY_FINALLY_EXPR, void_type_node, code, fini);
	}

      // Stick the code into the block we built for the receiver, if
      // we built on.
      if (bind != NULL_TREE && code != NULL_TREE && code != error_mark_node)
	{
	  BIND_EXPR_BODY(bind) = code;
	  code = bind;
	}

      DECL_SAVED_TREE(fndecl) = code;
    }
}

// Build the wrappers around function code needed if the function has
// any defer statements.  This sets *EXCEPT to an exception handler
// and *FINI to a finally handler.

void
Function::build_defer_wrapper(Gogo* gogo, Named_object* named_function,
			      tree *except, tree *fini)
{
  source_location end_loc = this->block_->end_location();

  // Add an exception handler.  This is used if a panic occurs.  Its
  // purpose is to stop the stack unwinding if a deferred function
  // calls recover.  There are more details in
  // libgo/runtime/go-unwind.c.
  tree stmt_list = NULL_TREE;
  static tree check_fndecl;
  tree call = Gogo::call_builtin(&check_fndecl,
				 end_loc,
				 "__go_check_defer",
				 1,
				 void_type_node,
				 ptr_type_node,
				 this->defer_stack(end_loc));
  if (call != error_mark_node)
    append_to_statement_list(call, &stmt_list);

  tree retval = this->return_value(gogo, named_function, end_loc, &stmt_list);
  tree set;
  if (retval == NULL_TREE)
    set = NULL_TREE;
  else
    set = fold_build2_loc(end_loc, MODIFY_EXPR, void_type_node,
			  DECL_RESULT(this->fndecl_), retval);
  tree ret_stmt = fold_build1_loc(end_loc, RETURN_EXPR, void_type_node, set);
  append_to_statement_list(ret_stmt, &stmt_list);

  gcc_assert(*except == NULL_TREE);
  *except = stmt_list;

  // Add some finally code to run the defer functions.  This is used
  // both in the normal case, when no panic occurs, and also if a
  // panic occurs to run any further defer functions.  Of course, it
  // is possible for a defer function to call panic which should be
  // caught by another defer function.  To handle that we use a loop.
  //  finish:
  //   try { __go_undefer(); } catch { __go_check_defer(); goto finish; }
  //   if (return values are named) return named_vals;

  stmt_list = NULL;

  tree label = create_artificial_label(end_loc);
  tree define_label = fold_build1_loc(end_loc, LABEL_EXPR, void_type_node,
				      label);
  append_to_statement_list(define_label, &stmt_list);

  static tree undefer_fndecl;
  tree undefer = Gogo::call_builtin(&undefer_fndecl,
				    end_loc,
				    "__go_undefer",
				    1,
				    void_type_node,
				    ptr_type_node,
				    this->defer_stack(end_loc));
  if (undefer_fndecl != NULL_TREE)
    TREE_NOTHROW(undefer_fndecl) = 0;

  tree defer = Gogo::call_builtin(&check_fndecl,
				  end_loc,
				  "__go_check_defer",
				  1,
				  void_type_node,
				  ptr_type_node,
				  this->defer_stack(end_loc));
  tree jump = fold_build1_loc(end_loc, GOTO_EXPR, void_type_node, label);
  tree catch_body = build2(COMPOUND_EXPR, void_type_node, defer, jump);
  catch_body = build2(CATCH_EXPR, void_type_node, NULL, catch_body);
  tree try_catch = build2(TRY_CATCH_EXPR, void_type_node, undefer, catch_body);

  append_to_statement_list(try_catch, &stmt_list);

  if (this->type_->results() != NULL
      && !this->type_->results()->empty()
      && !this->type_->results()->front().name().empty())
    {
      // If the result variables are named, we need to return them
      // again, because they might have been changed by a defer
      // function.
      retval = this->return_value(gogo, named_function, end_loc,
				  &stmt_list);
      set = fold_build2_loc(end_loc, MODIFY_EXPR, void_type_node,
			    DECL_RESULT(this->fndecl_), retval);
      ret_stmt = fold_build1_loc(end_loc, RETURN_EXPR, void_type_node, set);
      append_to_statement_list(ret_stmt, &stmt_list);
    }
  
  gcc_assert(*fini == NULL_TREE);
  *fini = stmt_list;
}

// Return the value to assign to DECL_RESULT(this->fndecl_).  This may
// also add statements to STMT_LIST, which need to be executed before
// the assignment.  This is used for a return statement with no
// explicit values.

tree
Function::return_value(Gogo* gogo, Named_object* named_function,
		       source_location location, tree* stmt_list) const
{
  const Typed_identifier_list* results = this->type_->results();
  if (results == NULL || results->empty())
    return NULL_TREE;

  // In the case of an exception handler created for functions with
  // defer statements, the result variables may be unnamed.
  bool is_named = !results->front().name().empty();
  if (is_named)
    {
      gcc_assert(this->named_results_ != NULL);
      if (this->named_results_->size() != results->size())
	{
	  gcc_assert(saw_errors());
	  return error_mark_node;
	}
    }

  tree retval;
  if (results->size() == 1)
    {
      if (is_named)
	return this->named_results_->front()->get_tree(gogo, named_function);
      else
	return results->front().type()->get_init_tree(gogo, false);
    }
  else
    {
      tree rettype = TREE_TYPE(DECL_RESULT(this->fndecl_));
      retval = create_tmp_var(rettype, "RESULT");
      tree field = TYPE_FIELDS(rettype);
      int index = 0;
      for (Typed_identifier_list::const_iterator pr = results->begin();
	   pr != results->end();
	   ++pr, ++index, field = DECL_CHAIN(field))
	{
	  gcc_assert(field != NULL);
	  tree val;
	  if (is_named)
	    val = (*this->named_results_)[index]->get_tree(gogo,
							   named_function);
	  else
	    val = pr->type()->get_init_tree(gogo, false);
	  tree set = fold_build2_loc(location, MODIFY_EXPR, void_type_node,
				     build3(COMPONENT_REF, TREE_TYPE(field),
					    retval, field, NULL_TREE),
				     val);
	  append_to_statement_list(set, stmt_list);
	}
      return retval;
    }
}

// Get the tree for the variable holding the defer stack for this
// function.  At least at present, the value of this variable is not
// used.  However, a pointer to this variable is used as a marker for
// the functions on the defer stack associated with this function.
// Doing things this way permits inlining a function which uses defer.

tree
Function::defer_stack(source_location location)
{
  if (this->defer_stack_ == NULL_TREE)
    {
      tree var = create_tmp_var(ptr_type_node, "DEFER");
      DECL_INITIAL(var) = null_pointer_node;
      DECL_SOURCE_LOCATION(var) = location;
      TREE_ADDRESSABLE(var) = 1;
      this->defer_stack_ = var;
    }
  return fold_convert_loc(location, ptr_type_node,
			  build_fold_addr_expr_loc(location,
						   this->defer_stack_));
}

// Get a tree for the statements in a block.

tree
Block::get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();

  tree block = make_node(BLOCK);

  // Put the new block into the block tree.

  if (context->block() == NULL)
    {
      tree fndecl;
      if (context->function() != NULL)
	fndecl = context->function()->func_value()->get_decl();
      else
	fndecl = current_function_decl;
      gcc_assert(fndecl != NULL_TREE);

      // We may have already created a block for the receiver.
      if (DECL_INITIAL(fndecl) == NULL_TREE)
	{
	  BLOCK_SUPERCONTEXT(block) = fndecl;
	  DECL_INITIAL(fndecl) = block;
	}
      else
	{
	  tree superblock_tree = DECL_INITIAL(fndecl);
	  BLOCK_SUPERCONTEXT(block) = superblock_tree;
	  gcc_assert(BLOCK_CHAIN(block) == NULL_TREE);
	  BLOCK_CHAIN(block) = block;
	}
    }
  else
    {
      tree superblock_tree = context->block_tree();
      BLOCK_SUPERCONTEXT(block) = superblock_tree;
      tree* pp;
      for (pp = &BLOCK_SUBBLOCKS(superblock_tree);
	   *pp != NULL_TREE;
	   pp = &BLOCK_CHAIN(*pp))
	;
      *pp = block;
    }

  // Expand local variables in the block.

  tree* pp = &BLOCK_VARS(block);
  for (Bindings::const_definitions_iterator pv =
	 this->bindings_->begin_definitions();
       pv != this->bindings_->end_definitions();
       ++pv)
    {
      if ((!(*pv)->is_variable() || !(*pv)->var_value()->is_parameter())
	  && !(*pv)->is_result_variable()
	  && !(*pv)->is_const())
	{
	  tree var = (*pv)->get_tree(gogo, context->function());
	  if (var != error_mark_node && TREE_TYPE(var) != error_mark_node)
	    {
	      if ((*pv)->is_variable() && (*pv)->var_value()->is_in_heap())
		{
		  gcc_assert(TREE_CODE(var) == INDIRECT_REF);
		  var = TREE_OPERAND(var, 0);
		  gcc_assert(TREE_CODE(var) == VAR_DECL);
		}
	      *pp = var;
	      pp = &DECL_CHAIN(*pp);
	    }
	}
    }
  *pp = NULL_TREE;

  Translate_context subcontext(context->gogo(), context->function(),
			       this, block);

  tree statements = NULL_TREE;

  // Expand the statements.

  for (std::vector<Statement*>::const_iterator p = this->statements_.begin();
       p != this->statements_.end();
       ++p)
    {
      tree statement = (*p)->get_tree(&subcontext);
      if (statement != error_mark_node)
	append_to_statement_list(statement, &statements);
    }

  TREE_USED(block) = 1;

  tree bind = build3(BIND_EXPR, void_type_node, BLOCK_VARS(block), statements,
		     block);
  TREE_SIDE_EFFECTS(bind) = 1;

  return bind;
}

// Get the LABEL_DECL for a label.

tree
Label::get_decl()
{
  if (this->decl_ == NULL)
    {
      tree id = get_identifier_from_string(this->name_);
      this->decl_ = build_decl(this->location_, LABEL_DECL, id, void_type_node);
      DECL_CONTEXT(this->decl_) = current_function_decl;
    }
  return this->decl_;
}

// Return an expression for the address of this label.

tree
Label::get_addr(source_location location)
{
  tree decl = this->get_decl();
  TREE_USED(decl) = 1;
  TREE_ADDRESSABLE(decl) = 1;
  return fold_convert_loc(location, ptr_type_node,
			  build_fold_addr_expr_loc(location, decl));
}

// Get the LABEL_DECL for an unnamed label.

tree
Unnamed_label::get_decl()
{
  if (this->decl_ == NULL)
    this->decl_ = create_artificial_label(this->location_);
  return this->decl_;
}

// Get the LABEL_EXPR for an unnamed label.

tree
Unnamed_label::get_definition()
{
  tree t = build1(LABEL_EXPR, void_type_node, this->get_decl());
  SET_EXPR_LOCATION(t, this->location_);
  return t;
}

// Return a goto to this label.

tree
Unnamed_label::get_goto(source_location location)
{
  tree t = build1(GOTO_EXPR, void_type_node, this->get_decl());
  SET_EXPR_LOCATION(t, location);
  return t;
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
  return type->get_tree(go_get_gogo());
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
      return type->float_type()->type_tree();
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
      return type->complex_type()->type_tree();
    }
  else
    return NULL_TREE;
}

// Return a tree which allocates SIZE bytes which will holds value of
// type TYPE.

tree
Gogo::allocate_memory(Type* type, tree size, source_location location)
{
  // If the package imports unsafe, then it may play games with
  // pointers that look like integers.
  if (this->imported_unsafe_ || type->has_pointer())
    {
      static tree new_fndecl;
      return Gogo::call_builtin(&new_fndecl,
				location,
				"__go_new",
				1,
				ptr_type_node,
				sizetype,
				size);
    }
  else
    {
      static tree new_nopointers_fndecl;
      return Gogo::call_builtin(&new_nopointers_fndecl,
				location,
				"__go_new_nopointers",
				1,
				ptr_type_node,
				sizetype,
				size);
    }
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

// Return a type to use for pointer to const char for a string.

tree
Gogo::const_char_pointer_type_tree()
{
  static tree type;
  if (type == NULL_TREE)
    {
      tree const_char_type = build_qualified_type(unsigned_char_type_node,
						  TYPE_QUAL_CONST);
      type = build_pointer_type(const_char_type);
      go_preserve_from_gc(type);
    }
  return type;
}

// Return a tree for a string constant.

tree
Gogo::string_constant_tree(const std::string& val)
{
  tree index_type = build_index_type(size_int(val.length()));
  tree const_char_type = build_qualified_type(unsigned_char_type_node,
					      TYPE_QUAL_CONST);
  tree string_type = build_array_type(const_char_type, index_type);
  string_type = build_variant_type_copy(string_type);
  TYPE_STRING_FLAG(string_type) = 1;
  tree string_val = build_string(val.length(), val.data());
  TREE_TYPE(string_val) = string_type;
  return string_val;
}

// Return a tree for a Go string constant.

tree
Gogo::go_string_constant_tree(const std::string& val)
{
  tree string_type = Type::make_string_type()->get_tree(this);

  VEC(constructor_elt, gc)* init = VEC_alloc(constructor_elt, gc, 2);

  constructor_elt* elt = VEC_quick_push(constructor_elt, init, NULL);
  tree field = TYPE_FIELDS(string_type);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__data") == 0);
  elt->index = field;
  tree str = Gogo::string_constant_tree(val);
  elt->value = fold_convert(TREE_TYPE(field),
			    build_fold_addr_expr(str));

  elt = VEC_quick_push(constructor_elt, init, NULL);
  field = DECL_CHAIN(field);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__length") == 0);
  elt->index = field;
  elt->value = build_int_cst_type(TREE_TYPE(field), val.length());

  tree constructor = build_constructor(string_type, init);
  TREE_READONLY(constructor) = 1;
  TREE_CONSTANT(constructor) = 1;

  return constructor;
}

// Return a tree for a pointer to a Go string constant.  This is only
// used for type descriptors, so we return a pointer to a constant
// decl.

tree
Gogo::ptr_go_string_constant_tree(const std::string& val)
{
  tree pval = this->go_string_constant_tree(val);

  tree decl = build_decl(UNKNOWN_LOCATION, VAR_DECL,
			 create_tmp_var_name("SP"), TREE_TYPE(pval));
  DECL_EXTERNAL(decl) = 0;
  TREE_PUBLIC(decl) = 0;
  TREE_USED(decl) = 1;
  TREE_READONLY(decl) = 1;
  TREE_CONSTANT(decl) = 1;
  TREE_STATIC(decl) = 1;
  DECL_ARTIFICIAL(decl) = 1;
  DECL_INITIAL(decl) = pval;
  rest_of_decl_compilation(decl, 1, 0);

  return build_fold_addr_expr(decl);
}

// Build the type of the struct that holds a slice for the given
// element type.

tree
Gogo::slice_type_tree(tree element_type_tree)
{
  // We use int for the count and capacity fields in a slice header.
  // This matches 6g.  The language definition guarantees that we
  // can't allocate space of a size which does not fit in int
  // anyhow. FIXME: integer_type_node is the the C type "int" but is
  // not necessarily the Go type "int".  They will differ when the C
  // type "int" has fewer than 32 bits.
  return Gogo::builtin_struct(NULL, "__go_slice", NULL_TREE, 3,
			      "__values",
			      build_pointer_type(element_type_tree),
			      "__count",
			      integer_type_node,
			      "__capacity",
			      integer_type_node);
}

// Given the tree for a slice type, return the tree for the type of
// the elements of the slice.

tree
Gogo::slice_element_type_tree(tree slice_type_tree)
{
  gcc_assert(TREE_CODE(slice_type_tree) == RECORD_TYPE
	     && POINTER_TYPE_P(TREE_TYPE(TYPE_FIELDS(slice_type_tree))));
  return TREE_TYPE(TREE_TYPE(TYPE_FIELDS(slice_type_tree)));
}

// Build a constructor for a slice.  SLICE_TYPE_TREE is the type of
// the slice.  VALUES is the value pointer and COUNT is the number of
// entries.  If CAPACITY is not NULL, it is the capacity; otherwise
// the capacity and the count are the same.

tree
Gogo::slice_constructor(tree slice_type_tree, tree values, tree count,
			tree capacity)
{
  gcc_assert(TREE_CODE(slice_type_tree) == RECORD_TYPE);

  VEC(constructor_elt,gc)* init = VEC_alloc(constructor_elt, gc, 3);

  tree field = TYPE_FIELDS(slice_type_tree);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__values") == 0);
  constructor_elt* elt = VEC_quick_push(constructor_elt, init, NULL);
  elt->index = field;
  gcc_assert(TYPE_MAIN_VARIANT(TREE_TYPE(field))
	     == TYPE_MAIN_VARIANT(TREE_TYPE(values)));
  elt->value = values;

  count = fold_convert(sizetype, count);
  if (capacity == NULL_TREE)
    {
      count = save_expr(count);
      capacity = count;
    }

  field = DECL_CHAIN(field);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__count") == 0);
  elt = VEC_quick_push(constructor_elt, init, NULL);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), count);

  field = DECL_CHAIN(field);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__capacity") == 0);
  elt = VEC_quick_push(constructor_elt, init, NULL);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), capacity);

  return build_constructor(slice_type_tree, init);
}

// Build a constructor for an empty slice.

tree
Gogo::empty_slice_constructor(tree slice_type_tree)
{
  tree element_field = TYPE_FIELDS(slice_type_tree);
  tree ret = Gogo::slice_constructor(slice_type_tree,
				     fold_convert(TREE_TYPE(element_field),
						  null_pointer_node),
				     size_zero_node,
				     size_zero_node);
  TREE_CONSTANT(ret) = 1;
  return ret;
}

// Build a map descriptor for a map of type MAPTYPE.

tree
Gogo::map_descriptor(Map_type* maptype)
{
  if (this->map_descriptors_ == NULL)
    this->map_descriptors_ = new Map_descriptors(10);

  std::pair<const Map_type*, tree> val(maptype, NULL);
  std::pair<Map_descriptors::iterator, bool> ins =
    this->map_descriptors_->insert(val);
  Map_descriptors::iterator p = ins.first;
  if (!ins.second)
    {
      if (p->second == error_mark_node)
	return error_mark_node;
      gcc_assert(p->second != NULL_TREE && DECL_P(p->second));
      return build_fold_addr_expr(p->second);
    }

  Type* keytype = maptype->key_type();
  Type* valtype = maptype->val_type();

  std::string mangled_name = ("__go_map_" + maptype->mangled_name(this));

  tree id = get_identifier_from_string(mangled_name);

  // Get the type of the map descriptor.  This is __go_map_descriptor
  // in libgo/map.h.

  tree struct_type = this->map_descriptor_type();

  // The map entry type is a struct with three fields.  This struct is
  // specific to MAPTYPE.  Build it.

  tree map_entry_type = make_node(RECORD_TYPE);

  map_entry_type = Gogo::builtin_struct(NULL, "__map", map_entry_type, 3,
					"__next",
					build_pointer_type(map_entry_type),
					"__key",
					keytype->get_tree(this),
					"__val",
					valtype->get_tree(this));
  if (map_entry_type == error_mark_node)
    {
      p->second = error_mark_node;
      return error_mark_node;
    }

  tree map_entry_key_field = DECL_CHAIN(TYPE_FIELDS(map_entry_type));
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(map_entry_key_field)),
		    "__key") == 0);

  tree map_entry_val_field = DECL_CHAIN(map_entry_key_field);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(map_entry_val_field)),
		    "__val") == 0);

  // Initialize the entries.

  tree map_descriptor_field = TYPE_FIELDS(struct_type);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(map_descriptor_field)),
		    "__map_descriptor") == 0);
  tree entry_size_field = DECL_CHAIN(map_descriptor_field);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(entry_size_field)),
		    "__entry_size") == 0);
  tree key_offset_field = DECL_CHAIN(entry_size_field);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(key_offset_field)),
		    "__key_offset") == 0);
  tree val_offset_field = DECL_CHAIN(key_offset_field);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(val_offset_field)),
		    "__val_offset") == 0);

  VEC(constructor_elt, gc)* descriptor = VEC_alloc(constructor_elt, gc, 6);

  constructor_elt* elt = VEC_quick_push(constructor_elt, descriptor, NULL);
  elt->index = map_descriptor_field;
  elt->value = maptype->type_descriptor_pointer(this);

  elt = VEC_quick_push(constructor_elt, descriptor, NULL);
  elt->index = entry_size_field;
  elt->value = TYPE_SIZE_UNIT(map_entry_type);

  elt = VEC_quick_push(constructor_elt, descriptor, NULL);
  elt->index = key_offset_field;
  elt->value = byte_position(map_entry_key_field);

  elt = VEC_quick_push(constructor_elt, descriptor, NULL);
  elt->index = val_offset_field;
  elt->value = byte_position(map_entry_val_field);

  tree constructor = build_constructor(struct_type, descriptor);

  tree decl = build_decl(BUILTINS_LOCATION, VAR_DECL, id, struct_type);
  TREE_STATIC(decl) = 1;
  TREE_USED(decl) = 1;
  TREE_READONLY(decl) = 1;
  TREE_CONSTANT(decl) = 1;
  DECL_INITIAL(decl) = constructor;
  make_decl_one_only(decl, DECL_ASSEMBLER_NAME(decl));
  resolve_unique_section(decl, 1, 0);

  rest_of_decl_compilation(decl, 1, 0);

  go_preserve_from_gc(decl);
  p->second = decl;

  return build_fold_addr_expr(decl);
}

// Return a tree for the type of a map descriptor.  This is struct
// __go_map_descriptor in libgo/runtime/map.h.  This is the same for
// all map types.

tree
Gogo::map_descriptor_type()
{
  static tree struct_type;
  tree dtype = Type::make_type_descriptor_type()->get_tree(this);
  dtype = build_qualified_type(dtype, TYPE_QUAL_CONST);
  return Gogo::builtin_struct(&struct_type, "__go_map_descriptor", NULL_TREE,
			      4,
			      "__map_descriptor",
			      build_pointer_type(dtype),
			      "__entry_size",
			      sizetype,
			      "__key_offset",
			      sizetype,
			      "__val_offset",
			      sizetype);
}

// Return the name to use for a type descriptor decl for TYPE.  This
// is used when TYPE does not have a name.

std::string
Gogo::unnamed_type_descriptor_decl_name(const Type* type)
{
  return "__go_td_" + type->mangled_name(this);
}

// Return the name to use for a type descriptor decl for a type named
// NAME, defined in the function IN_FUNCTION.  IN_FUNCTION will
// normally be NULL.

std::string
Gogo::type_descriptor_decl_name(const Named_object* no,
				const Named_object* in_function)
{
  std::string ret = "__go_tdn_";
  if (no->type_value()->is_builtin())
    gcc_assert(in_function == NULL);
  else
    {
      const std::string& unique_prefix(no->package() == NULL
				       ? this->unique_prefix()
				       : no->package()->unique_prefix());
      const std::string& package_name(no->package() == NULL
				      ? this->package_name()
				      : no->package()->name());
      ret.append(unique_prefix);
      ret.append(1, '.');
      ret.append(package_name);
      ret.append(1, '.');
      if (in_function != NULL)
	{
	  ret.append(Gogo::unpack_hidden_name(in_function->name()));
	  ret.append(1, '.');
	}
    }
  ret.append(no->name());
  return ret;
}

// Where a type descriptor decl should be defined.

Gogo::Type_descriptor_location
Gogo::type_descriptor_location(const Type* type)
{
  const Named_type* name = type->named_type();
  if (name != NULL)
    {
      if (name->named_object()->package() != NULL)
	{
	  // This is a named type defined in a different package.  The
	  // descriptor should be defined in that package.
	  return TYPE_DESCRIPTOR_UNDEFINED;
	}
      else if (name->is_builtin())
	{
	  // We create the descriptor for a builtin type whenever we
	  // need it.
	  return TYPE_DESCRIPTOR_COMMON;
	}
      else
	{
	  // This is a named type defined in this package.  The
	  // descriptor should be defined here.
	  return TYPE_DESCRIPTOR_DEFINED;
	}
    }
  else
    {
      if (type->points_to() != NULL
	  && type->points_to()->named_type() != NULL
	  && type->points_to()->named_type()->named_object()->package() != NULL)
	{
	  // This is an unnamed pointer to a named type defined in a
	  // different package.  The descriptor should be defined in
	  // that package.
	  return TYPE_DESCRIPTOR_UNDEFINED;
	}
      else
	{
	  // This is an unnamed type.  The descriptor could be defined
	  // in any package where it is needed, and the linker will
	  // pick one descriptor to keep.
	  return TYPE_DESCRIPTOR_COMMON;
	}
    }
}

// Build a type descriptor decl for TYPE.  INITIALIZER is a struct
// composite literal which initializers the type descriptor.

void
Gogo::build_type_descriptor_decl(const Type* type, Expression* initializer,
				 tree* pdecl)
{
  const Named_type* name = type->named_type();

  // We can have multiple instances of unnamed types, but we only want
  // to emit the type descriptor once.  We use a hash table to handle
  // this.  This is not necessary for named types, as they are unique,
  // and we store the type descriptor decl in the type itself.
  tree* phash = NULL;
  if (name == NULL)
    {
      if (this->type_descriptor_decls_ == NULL)
	this->type_descriptor_decls_ = new Type_descriptor_decls(10);

      std::pair<Type_descriptor_decls::iterator, bool> ins =
	this->type_descriptor_decls_->insert(std::make_pair(type, NULL_TREE));
      if (!ins.second)
	{
	  // We've already built a type descriptor for this type.
	  *pdecl = ins.first->second;
	  return;
	}
      phash = &ins.first->second;
    }

  std::string decl_name;
  if (name == NULL)
    decl_name = this->unnamed_type_descriptor_decl_name(type);
  else
    decl_name = this->type_descriptor_decl_name(name->named_object(),
						name->in_function());
  tree id = get_identifier_from_string(decl_name);
  tree descriptor_type_tree = initializer->type()->get_tree(this);
  if (descriptor_type_tree == error_mark_node)
    {
      *pdecl = error_mark_node;
      return;
    }
  tree decl = build_decl(name == NULL ? BUILTINS_LOCATION : name->location(),
			 VAR_DECL, id,
			 build_qualified_type(descriptor_type_tree,
					      TYPE_QUAL_CONST));
  TREE_READONLY(decl) = 1;
  TREE_CONSTANT(decl) = 1;
  DECL_ARTIFICIAL(decl) = 1;

  go_preserve_from_gc(decl);
  if (phash != NULL)
    *phash = decl;

  // We store the new DECL now because we may need to refer to it when
  // expanding INITIALIZER.
  *pdecl = decl;

  // If appropriate, just refer to the exported type identifier.
  Gogo::Type_descriptor_location type_descriptor_location =
    this->type_descriptor_location(type);
  if (type_descriptor_location == TYPE_DESCRIPTOR_UNDEFINED)
    {
      TREE_PUBLIC(decl) = 1;
      DECL_EXTERNAL(decl) = 1;
      return;
    }

  TREE_STATIC(decl) = 1;
  TREE_USED(decl) = 1;

  Translate_context context(this, NULL, NULL, NULL);
  context.set_is_const();
  tree constructor = initializer->get_tree(&context);

  if (constructor == error_mark_node)
    gcc_assert(saw_errors());

  DECL_INITIAL(decl) = constructor;

  if (type_descriptor_location == TYPE_DESCRIPTOR_DEFINED)
    TREE_PUBLIC(decl) = 1;
  else
    {
      gcc_assert(type_descriptor_location == TYPE_DESCRIPTOR_COMMON);
      make_decl_one_only(decl, DECL_ASSEMBLER_NAME(decl));
      resolve_unique_section(decl, 1, 0);
    }

  rest_of_decl_compilation(decl, 1, 0);
}

// Build an interface method table for a type: a list of function
// pointers, one for each interface method.  This is used for
// interfaces.

tree
Gogo::interface_method_table_for_type(const Interface_type* interface,
				      Named_type* type,
				      bool is_pointer)
{
  const Typed_identifier_list* interface_methods = interface->methods();
  gcc_assert(!interface_methods->empty());

  std::string mangled_name = ((is_pointer ? "__go_pimt__" : "__go_imt_")
			      + interface->mangled_name(this)
			      + "__"
			      + type->mangled_name(this));

  tree id = get_identifier_from_string(mangled_name);

  // See whether this interface has any hidden methods.
  bool has_hidden_methods = false;
  for (Typed_identifier_list::const_iterator p = interface_methods->begin();
       p != interface_methods->end();
       ++p)
    {
      if (Gogo::is_hidden_name(p->name()))
	{
	  has_hidden_methods = true;
	  break;
	}
    }

  // We already know that the named type is convertible to the
  // interface.  If the interface has hidden methods, and the named
  // type is defined in a different package, then the interface
  // conversion table will be defined by that other package.
  if (has_hidden_methods && type->named_object()->package() != NULL)
    {
      tree array_type = build_array_type(const_ptr_type_node, NULL);
      tree decl = build_decl(BUILTINS_LOCATION, VAR_DECL, id, array_type);
      TREE_READONLY(decl) = 1;
      TREE_CONSTANT(decl) = 1;
      TREE_PUBLIC(decl) = 1;
      DECL_EXTERNAL(decl) = 1;
      go_preserve_from_gc(decl);
      return decl;
    }

  size_t count = interface_methods->size();
  VEC(constructor_elt, gc)* pointers = VEC_alloc(constructor_elt, gc,
						 count + 1);

  // The first element is the type descriptor.
  constructor_elt* elt = VEC_quick_push(constructor_elt, pointers, NULL);
  elt->index = size_zero_node;
  Type* td_type;
  if (!is_pointer)
    td_type = type;
  else
    td_type = Type::make_pointer_type(type);
  elt->value = fold_convert(const_ptr_type_node,
			    td_type->type_descriptor_pointer(this));

  size_t i = 1;
  for (Typed_identifier_list::const_iterator p = interface_methods->begin();
       p != interface_methods->end();
       ++p, ++i)
    {
      bool is_ambiguous;
      Method* m = type->method_function(p->name(), &is_ambiguous);
      gcc_assert(m != NULL);

      Named_object* no = m->named_object();

      tree fnid = no->get_id(this);

      tree fndecl;
      if (no->is_function())
	fndecl = no->func_value()->get_or_make_decl(this, no, fnid);
      else if (no->is_function_declaration())
	fndecl = no->func_declaration_value()->get_or_make_decl(this, no,
								fnid);
      else
	gcc_unreachable();
      fndecl = build_fold_addr_expr(fndecl);

      elt = VEC_quick_push(constructor_elt, pointers, NULL);
      elt->index = size_int(i);
      elt->value = fold_convert(const_ptr_type_node, fndecl);
    }
  gcc_assert(i == count + 1);

  tree array_type = build_array_type(const_ptr_type_node,
				     build_index_type(size_int(count)));
  tree constructor = build_constructor(array_type, pointers);

  tree decl = build_decl(BUILTINS_LOCATION, VAR_DECL, id, array_type);
  TREE_STATIC(decl) = 1;
  TREE_USED(decl) = 1;
  TREE_READONLY(decl) = 1;
  TREE_CONSTANT(decl) = 1;
  DECL_INITIAL(decl) = constructor;

  // If the interface type has hidden methods, then this is the only
  // definition of the table.  Otherwise it is a comdat table which
  // may be defined in multiple packages.
  if (has_hidden_methods)
    TREE_PUBLIC(decl) = 1;
  else
    {
      make_decl_one_only(decl, DECL_ASSEMBLER_NAME(decl));
      resolve_unique_section(decl, 1, 0);
    }

  rest_of_decl_compilation(decl, 1, 0);

  go_preserve_from_gc(decl);

  return decl;
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
Gogo::call_builtin(tree* pdecl, source_location location, const char* name,
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
    SET_EXPR_LOCATION(fnptr, location);

  tree ret = build_call_array(rettype, fnptr, nargs, args);
  SET_EXPR_LOCATION(ret, location);

  delete[] types;
  delete[] args;

  return ret;
}

// Build a call to the runtime error function.

tree
Gogo::runtime_error(int code, source_location location)
{
  static tree runtime_error_fndecl;
  tree ret = Gogo::call_builtin(&runtime_error_fndecl,
				location,
				"__go_runtime_error",
				1,
				void_type_node,
				integer_type_node,
				build_int_cst(integer_type_node, code));
  if (ret == error_mark_node)
    return error_mark_node;
  // The runtime error function panics and does not return.
  TREE_NOTHROW(runtime_error_fndecl) = 0;
  TREE_THIS_VOLATILE(runtime_error_fndecl) = 1;
  return ret;
}

// Send VAL on CHANNEL.  If BLOCKING is true, the resulting tree has a
// void type.  If BLOCKING is false, the resulting tree has a boolean
// type, and it will evaluate as true if the value was sent.  If
// FOR_SELECT is true, this is being done because it was chosen in a
// select statement.

tree
Gogo::send_on_channel(tree channel, tree val, bool blocking, bool for_select,
		      source_location location)
{
  if (channel == error_mark_node || val == error_mark_node)
    return error_mark_node;

  if (int_size_in_bytes(TREE_TYPE(val)) <= 8
      && !AGGREGATE_TYPE_P(TREE_TYPE(val))
      && !FLOAT_TYPE_P(TREE_TYPE(val)))
    {
      val = convert_to_integer(uint64_type_node, val);
      if (blocking)
	{
	  static tree send_small_fndecl;
	  tree ret = Gogo::call_builtin(&send_small_fndecl,
					location,
					"__go_send_small",
					3,
					void_type_node,
					ptr_type_node,
					channel,
					uint64_type_node,
					val,
					boolean_type_node,
					(for_select
					 ? boolean_true_node
					 : boolean_false_node));
	  if (ret == error_mark_node)
	    return error_mark_node;
	  // This can panic if there are too many operations on a
	  // closed channel.
	  TREE_NOTHROW(send_small_fndecl) = 0;
	  return ret;
	}
      else
	{
	  gcc_assert(!for_select);
	  static tree send_nonblocking_small_fndecl;
	  tree ret = Gogo::call_builtin(&send_nonblocking_small_fndecl,
					location,
					"__go_send_nonblocking_small",
					2,
					boolean_type_node,
					ptr_type_node,
					channel,
					uint64_type_node,
					val);
	  if (ret == error_mark_node)
	    return error_mark_node;
	  // This can panic if there are too many operations on a
	  // closed channel.
	  TREE_NOTHROW(send_nonblocking_small_fndecl) = 0;
	  return ret;
	}
    }
  else
    {
      tree make_tmp;
      if (TREE_ADDRESSABLE(TREE_TYPE(val)) || TREE_CODE(val) == VAR_DECL)
	{
	  make_tmp = NULL_TREE;
	  val = build_fold_addr_expr(val);
	  if (DECL_P(val))
	    TREE_ADDRESSABLE(val) = 1;
	}
      else
	{
	  tree tmp = create_tmp_var(TREE_TYPE(val), get_name(val));
	  DECL_IGNORED_P(tmp) = 0;
	  DECL_INITIAL(tmp) = val;
	  TREE_ADDRESSABLE(tmp) = 1;
	  make_tmp = build1(DECL_EXPR, void_type_node, tmp);
	  SET_EXPR_LOCATION(make_tmp, location);
	  val = build_fold_addr_expr(tmp);
	}
      val = fold_convert(ptr_type_node, val);

      tree call;
      if (blocking)
	{
	  static tree send_big_fndecl;
	  call = Gogo::call_builtin(&send_big_fndecl,
				    location,
				    "__go_send_big",
				    3,
				    void_type_node,
				    ptr_type_node,
				    channel,
				    ptr_type_node,
				    val,
				    boolean_type_node,
				    (for_select
				     ? boolean_true_node
				     : boolean_false_node));
	  if (call == error_mark_node)
	    return error_mark_node;
	  // This can panic if there are too many operations on a
	  // closed channel.
	  TREE_NOTHROW(send_big_fndecl) = 0;
	}
      else
	{
	  gcc_assert(!for_select);
	  static tree send_nonblocking_big_fndecl;
	  call = Gogo::call_builtin(&send_nonblocking_big_fndecl,
				    location,
				    "__go_send_nonblocking_big",
				    2,
				    boolean_type_node,
				    ptr_type_node,
				    channel,
				    ptr_type_node,
				    val);
	  if (call == error_mark_node)
	    return error_mark_node;
	  // This can panic if there are too many operations on a
	  // closed channel.
	  TREE_NOTHROW(send_nonblocking_big_fndecl) = 0;
	}

      if (make_tmp == NULL_TREE)
	return call;
      else
	{
	  tree ret = build2(COMPOUND_EXPR, TREE_TYPE(call), make_tmp, call);
	  SET_EXPR_LOCATION(ret, location);
	  return ret;
	}
    }
}

// Return a tree for receiving a value of type TYPE_TREE on CHANNEL.
// This does a blocking receive and returns the value read from the
// channel.  If FOR_SELECT is true, this is being done because it was
// chosen in a select statement.

tree
Gogo::receive_from_channel(tree type_tree, tree channel, bool for_select,
			   source_location location)
{
  if (type_tree == error_mark_node || channel == error_mark_node)
    return error_mark_node;

  if (int_size_in_bytes(type_tree) <= 8
      && !AGGREGATE_TYPE_P(type_tree)
      && !FLOAT_TYPE_P(type_tree))
    {
      static tree receive_small_fndecl;
      tree call = Gogo::call_builtin(&receive_small_fndecl,
				     location,
				     "__go_receive_small",
				     2,
				     uint64_type_node,
				     ptr_type_node,
				     channel,
				     boolean_type_node,
				     (for_select
				      ? boolean_true_node
				      : boolean_false_node));
      if (call == error_mark_node)
	return error_mark_node;
      // This can panic if there are too many operations on a closed
      // channel.
      TREE_NOTHROW(receive_small_fndecl) = 0;
      int bitsize = GET_MODE_BITSIZE(TYPE_MODE(type_tree));
      tree int_type_tree = go_type_for_size(bitsize, 1);
      return fold_convert_loc(location, type_tree,
			      fold_convert_loc(location, int_type_tree,
					       call));
    }
  else
    {
      tree tmp = create_tmp_var(type_tree, get_name(type_tree));
      DECL_IGNORED_P(tmp) = 0;
      TREE_ADDRESSABLE(tmp) = 1;
      tree make_tmp = build1(DECL_EXPR, void_type_node, tmp);
      SET_EXPR_LOCATION(make_tmp, location);
      tree tmpaddr = build_fold_addr_expr(tmp);
      tmpaddr = fold_convert(ptr_type_node, tmpaddr);
      static tree receive_big_fndecl;
      tree call = Gogo::call_builtin(&receive_big_fndecl,
				     location,
				     "__go_receive_big",
				     3,
				     void_type_node,
				     ptr_type_node,
				     channel,
				     ptr_type_node,
				     tmpaddr,
				     boolean_type_node,
				     (for_select
				      ? boolean_true_node
				      : boolean_false_node));
      if (call == error_mark_node)
	return error_mark_node;
      // This can panic if there are too many operations on a closed
      // channel.
      TREE_NOTHROW(receive_big_fndecl) = 0;
      return build2(COMPOUND_EXPR, type_tree, make_tmp,
		    build2(COMPOUND_EXPR, type_tree, call, tmp));
    }
}

// Return the type of a function trampoline.  This is like
// get_trampoline_type in tree-nested.c.

tree
Gogo::trampoline_type_tree()
{
  static tree type_tree;
  if (type_tree == NULL_TREE)
    {
      unsigned int size;
      unsigned int align;
      go_trampoline_info(&size, &align);
      tree t = build_index_type(build_int_cst(integer_type_node, size - 1));
      t = build_array_type(char_type_node, t);

      type_tree = Gogo::builtin_struct(NULL, "__go_trampoline", NULL_TREE, 1,
				       "__data", t);
      t = TYPE_FIELDS(type_tree);
      DECL_ALIGN(t) = align;
      DECL_USER_ALIGN(t) = 1;

      go_preserve_from_gc(type_tree);
    }
  return type_tree;
}

// Make a trampoline which calls FNADDR passing CLOSURE.

tree
Gogo::make_trampoline(tree fnaddr, tree closure, source_location location)
{
  tree trampoline_type = Gogo::trampoline_type_tree();
  tree trampoline_size = TYPE_SIZE_UNIT(trampoline_type);

  closure = save_expr(closure);

  // We allocate the trampoline using a special function which will
  // mark it as executable.
  static tree trampoline_fndecl;
  tree x = Gogo::call_builtin(&trampoline_fndecl,
			      location,
			      "__go_allocate_trampoline",
			      2,
			      ptr_type_node,
			      size_type_node,
			      trampoline_size,
			      ptr_type_node,
			      fold_convert_loc(location, ptr_type_node,
					       closure));
  if (x == error_mark_node)
    return error_mark_node;

  x = save_expr(x);

  // Initialize the trampoline.
  tree ini = build_call_expr(implicit_built_in_decls[BUILT_IN_INIT_TRAMPOLINE],
			     3, x, fnaddr, closure);

  // On some targets the trampoline address needs to be adjusted.  For
  // example, when compiling in Thumb mode on the ARM, the address
  // needs to have the low bit set.
  x = build_call_expr(implicit_built_in_decls[BUILT_IN_ADJUST_TRAMPOLINE],
		      1, x);
  x = fold_convert(TREE_TYPE(fnaddr), x);

  return build2(COMPOUND_EXPR, TREE_TYPE(x), ini, x);
}
