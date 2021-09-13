/* Handle exceptional things in C++.
   Copyright (C) 1989-2021 Free Software Foundation, Inc.
   Contributed by Michael Tiemann <tiemann@cygnus.com>
   Rewritten by Mike Stump <mrs@cygnus.com>, based upon an
   initial re-implementation courtesy Tad Hunt.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "trans-mem.h"
#include "attribs.h"
#include "tree-iterator.h"
#include "target.h"

static void push_eh_cleanup (tree);
static tree prepare_eh_type (tree);
static tree do_begin_catch (void);
static int dtor_nothrow (tree);
static tree do_end_catch (tree);
static void initialize_handler_parm (tree, tree);
static tree do_allocate_exception (tree);
static tree wrap_cleanups_r (tree *, int *, void *);
static int complete_ptr_ref_or_void_ptr_p (tree, tree);
static bool is_admissible_throw_operand_or_catch_parameter (tree, bool);

/* Sets up all the global eh stuff that needs to be initialized at the
   start of compilation.  */

void
init_exception_processing (void)
{
  tree tmp;

  /* void std::terminate (); */
  push_nested_namespace (std_node);
  tmp = build_function_type_list (void_type_node, NULL_TREE);
  terminate_fn = build_cp_library_fn_ptr ("terminate", tmp,
					   ECF_NOTHROW | ECF_NORETURN
					   | ECF_COLD);
  gcc_checking_assert (TREE_THIS_VOLATILE (terminate_fn)
		       && TREE_NOTHROW (terminate_fn));
  pop_nested_namespace (std_node);

  /* void __cxa_call_unexpected(void *); */
  tmp = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);
  call_unexpected_fn
    = push_throw_library_fn (get_identifier ("__cxa_call_unexpected"), tmp);
}

/* Returns an expression to be executed if an unhandled exception is
   propagated out of a cleanup region.  */

tree
cp_protect_cleanup_actions (void)
{
  /* [except.terminate]

     When the destruction of an object during stack unwinding exits
     using an exception ... void terminate(); is called.  */
  return terminate_fn;
}

static tree
prepare_eh_type (tree type)
{
  if (type == NULL_TREE)
    return type;
  if (type == error_mark_node)
    return error_mark_node;

  /* peel back references, so they match.  */
  type = non_reference (type);

  /* Peel off cv qualifiers.  */
  type = TYPE_MAIN_VARIANT (type);

  /* Functions and arrays decay to pointers.  */
  type = type_decays_to (type);

  return type;
}

/* Return the type info for TYPE as used by EH machinery.  */
tree
eh_type_info (tree type)
{
  if (type == NULL_TREE || type == error_mark_node)
    return type;

  return get_tinfo_decl (type);
}

/* Build the address of a typeinfo decl for use in the runtime
   matching field of the exception model.  */

tree
build_eh_type_type (tree type)
{
  tree exp = eh_type_info (type);

  if (!exp)
    return NULL;

  mark_used (exp);

  return convert (ptr_type_node, build_address (exp));
}

tree
build_exc_ptr (void)
{
  return build_call_n (builtin_decl_explicit (BUILT_IN_EH_POINTER),
		       1, integer_zero_node);
}

/* Declare an exception ABI entry point called NAME.
   ECF are the library flags, RTYPE the return type and ARGS[NARGS]
   the parameter types.  We return the DECL -- which might be one
   found via the symbol table pushing, if the user already declared
   it.  If we pushed a new decl, the user will see it.  */

static tree
declare_library_fn_1 (const char *name, int ecf,
		      tree rtype, int nargs, tree args[])
{
  tree ident = get_identifier (name);
  tree except = ecf & ECF_NOTHROW ? empty_except_spec : NULL_TREE;

  /* Make a new decl.  */
  tree arg_list = void_list_node;
  for (unsigned ix = nargs; ix--;)
    arg_list = tree_cons (NULL_TREE, args[ix], arg_list);
  tree fntype = build_function_type (rtype, arg_list);
  tree res = push_library_fn (ident, fntype, except, ecf);

  return res;
}

/* Find or declare a function NAME, returning RTYPE, taking a single
   parameter PTYPE, with an empty exception specification. ECF are the
   library fn flags.  If TM_ECF is non-zero, also find or create a
   transaction variant and record it as a replacement, when flag_tm is
   in effect.

   Note that the C++ ABI document does not have a throw-specifier on
   the routines declared below via this function.  The declarations
   are consistent with the actual implementations in libsupc++.  */

static tree
declare_library_fn (const char *name, tree rtype, tree ptype,
		    int ecf, int tm_ecf)
{
  tree res = declare_library_fn_1 (name, ecf, rtype, ptype ? 1 : 0, &ptype);
  if (res == error_mark_node)
    return res;

  if (tm_ecf && flag_tm)
    {
      char *tm_name = concat ("_ITM_", name + 2, NULL_TREE);

      tree tm_fn = declare_library_fn_1 (tm_name, ecf | tm_ecf, rtype,
					 ptype ? 1 : 0, &ptype);
      free (tm_name);
      if (tm_fn != error_mark_node)
	record_tm_replacement (res, tm_fn);
    }

  return res;
}

/* Build up a call to __cxa_get_exception_ptr so that we can build a
   copy constructor for the thrown object.  */

static tree
do_get_exception_ptr (void)
{
  if (!get_exception_ptr_fn)
    /* Declare void* __cxa_get_exception_ptr (void *) throw().  */
    get_exception_ptr_fn
      = declare_library_fn ("__cxa_get_exception_ptr",
			    ptr_type_node, ptr_type_node,
			    ECF_NOTHROW | ECF_PURE | ECF_LEAF | ECF_TM_PURE,
			    0);

  return cp_build_function_call_nary (get_exception_ptr_fn,
				      tf_warning_or_error,
				      build_exc_ptr (), NULL_TREE);
}

/* Build up a call to __cxa_begin_catch, to tell the runtime that the
   exception has been handled.  */

static tree
do_begin_catch (void)
{
  if (!begin_catch_fn)
    /* Declare void* __cxa_begin_catch (void *) throw().  */
    begin_catch_fn
      = declare_library_fn ("__cxa_begin_catch",
			    ptr_type_node, ptr_type_node, ECF_NOTHROW,
			    ECF_TM_PURE);

  return cp_build_function_call_nary (begin_catch_fn, tf_warning_or_error,
				      build_exc_ptr (), NULL_TREE);
}

/* Returns nonzero if cleaning up an exception of type TYPE (which can be
   NULL_TREE for a ... handler) will not throw an exception.  */

static int
dtor_nothrow (tree type)
{
  if (type == NULL_TREE || type == error_mark_node)
    return 0;

  if (TYPE_HAS_TRIVIAL_DESTRUCTOR (type))
    return 1;

  if (CLASSTYPE_LAZY_DESTRUCTOR (type))
    lazily_declare_fn (sfk_destructor, type);

  return TREE_NOTHROW (CLASSTYPE_DESTRUCTOR (type));
}

/* Build up a call to __cxa_end_catch, to destroy the exception object
   for the current catch block if no others are currently using it.  */

static tree
do_end_catch (tree type)
{
  if (!end_catch_fn)
    /* Declare void __cxa_end_catch ().
       This can throw if the destructor for the exception throws.  */
    end_catch_fn
      = declare_library_fn ("__cxa_end_catch", void_type_node,
			    NULL_TREE, 0, ECF_TM_PURE);

  tree cleanup = cp_build_function_call_vec (end_catch_fn,
					     NULL, tf_warning_or_error);
  if (cleanup != error_mark_node)
    TREE_NOTHROW (cleanup) = dtor_nothrow (type);

  return cleanup;
}

/* This routine creates the cleanup for the current exception.  */

static void
push_eh_cleanup (tree type)
{
  finish_decl_cleanup (NULL_TREE, do_end_catch (type));
}

/* Wrap EXPR in a MUST_NOT_THROW_EXPR expressing that EXPR must
   not throw any exceptions if COND is true.  A condition of
   NULL_TREE is treated as 'true'.  */

tree
build_must_not_throw_expr (tree body, tree cond)
{
  tree type = body ? TREE_TYPE (body) : void_type_node;

  if (!flag_exceptions)
    return body;

  if (!cond)
    /* OK, unconditional.  */;
  else
    {
      tree conv = NULL_TREE;
      if (!type_dependent_expression_p (cond))
	conv = perform_implicit_conversion_flags (boolean_type_node, cond,
						  tf_warning_or_error,
						  LOOKUP_NORMAL);
      if (tree inst = instantiate_non_dependent_or_null (conv))
	cond = cxx_constant_value (inst);
      else
	require_constant_expression (cond);
      if (integer_zerop (cond))
	return body;
      else if (integer_onep (cond))
	cond = NULL_TREE;
    }

  return build2 (MUST_NOT_THROW_EXPR, type, body, cond);
}


/* Initialize the catch parameter DECL.  */

static void
initialize_handler_parm (tree decl, tree exp)
{
  tree init;
  tree init_type;

  /* Make sure we mark the catch param as used, otherwise we'll get a
     warning about an unused ((anonymous)).  */
  TREE_USED (decl) = 1;
  DECL_READ_P (decl) = 1;

  /* Figure out the type that the initializer is.  Pointers are returned
     adjusted by value from __cxa_begin_catch.  Others are returned by
     reference.  */
  init_type = TREE_TYPE (decl);
  if (!INDIRECT_TYPE_P (init_type))
    init_type = build_reference_type (init_type);

  /* Since pointers are passed by value, initialize a reference to
     pointer catch parm with the address of the temporary.  */
  if (TYPE_REF_P (init_type)
      && TYPE_PTR_P (TREE_TYPE (init_type)))
    exp = cp_build_addr_expr (exp, tf_warning_or_error);

  exp = ocp_convert (init_type, exp, CONV_IMPLICIT|CONV_FORCE_TEMP, 0,
		     tf_warning_or_error);

  init = convert_from_reference (exp);

  /* If the constructor for the catch parm exits via an exception, we
     must call terminate.  See eh23.C.  */
  if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl)))
    {
      /* Generate the copy constructor call directly so we can wrap it.
	 See also expand_default_init.  */
      init = ocp_convert (TREE_TYPE (decl), init,
			  CONV_IMPLICIT|CONV_FORCE_TEMP, 0,
			  tf_warning_or_error);
      /* Force cleanups now to avoid nesting problems with the
	 MUST_NOT_THROW_EXPR.  */
      init = fold_build_cleanup_point_expr (TREE_TYPE (init), init);
      init = build_must_not_throw_expr (init, NULL_TREE);
    }

  decl = pushdecl (decl);

  start_decl_1 (decl, true);
  cp_finish_decl (decl, init, /*init_const_expr_p=*/false, NULL_TREE,
		  LOOKUP_ONLYCONVERTING|DIRECT_BIND);
}


/* Routine to see if exception handling is turned on.
   DO_WARN is nonzero if we want to inform the user that exception
   handling is turned off.

   This is used to ensure that -fexceptions has been specified if the
   compiler tries to use any exception-specific functions.  */

static inline int
doing_eh (void)
{
  if (! flag_exceptions)
    {
      static int warned = 0;
      if (! warned)
	{
	  error ("exception handling disabled, use %<-fexceptions%> to enable");
	  warned = 1;
	}
      return 0;
    }
  return 1;
}

/* Call this to start a catch block.  DECL is the catch parameter.  */

tree
expand_start_catch_block (tree decl)
{
  tree exp;
  tree type, init;

  if (! doing_eh ())
    return NULL_TREE;

  if (decl)
    {
      if (!is_admissible_throw_operand_or_catch_parameter (decl, false))
	decl = error_mark_node;

      type = prepare_eh_type (TREE_TYPE (decl));
      mark_used (eh_type_info (type));
    }
  else
    type = NULL_TREE;

  /* Call __cxa_end_catch at the end of processing the exception.  */
  push_eh_cleanup (type);

  init = do_begin_catch ();

  /* If there's no decl at all, then all we need to do is make sure
     to tell the runtime that we've begun handling the exception.  */
  if (decl == NULL || decl == error_mark_node || init == error_mark_node)
    finish_expr_stmt (init);

  /* If the C++ object needs constructing, we need to do that before
     calling __cxa_begin_catch, so that std::uncaught_exception gets
     the right value during the copy constructor.  */
  else if (flag_use_cxa_get_exception_ptr
	   && TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl)))
    {
      exp = do_get_exception_ptr ();
      if (exp != error_mark_node)
	initialize_handler_parm (decl, exp);
      finish_expr_stmt (init);
    }

  /* Otherwise the type uses a bitwise copy, and we don't have to worry
     about the value of std::uncaught_exception and therefore can do the
     copy with the return value of __cxa_end_catch instead.  */
  else
    {
      tree init_type = type;

      /* Pointers are passed by values, everything else by reference.  */
      if (!TYPE_PTR_P (type))
	init_type = build_pointer_type (type);
      if (init_type != TREE_TYPE (init))
	init = build1 (NOP_EXPR, init_type, init);
      exp = create_temporary_var (init_type);
      cp_finish_decl (exp, init, /*init_const_expr=*/false,
		      NULL_TREE, LOOKUP_ONLYCONVERTING);
      DECL_REGISTER (exp) = 1;
      initialize_handler_parm (decl, exp);
    }

  return type;
}


/* Call this to end a catch block.  Its responsible for emitting the
   code to handle jumping back to the correct place, and for emitting
   the label to jump to if this catch block didn't match.  */

void
expand_end_catch_block (void)
{
  if (! doing_eh ())
    return;

  /* The exception being handled is rethrown if control reaches the end of
     a handler of the function-try-block of a constructor or destructor.  */
  if (in_function_try_handler
      && (DECL_CONSTRUCTOR_P (current_function_decl)
	  || DECL_DESTRUCTOR_P (current_function_decl)))
    {
      tree rethrow = build_throw (input_location, NULL_TREE);
      /* Disable all warnings for the generated rethrow statement.  */
      suppress_warning (rethrow);
      finish_expr_stmt (rethrow);
    }
}

tree
begin_eh_spec_block (void)
{
  tree r;
  location_t spec_location = DECL_SOURCE_LOCATION (current_function_decl);

  /* A noexcept specification (or throw() with -fnothrow-opt) is a
     MUST_NOT_THROW_EXPR.  */
  if (TYPE_NOEXCEPT_P (TREE_TYPE (current_function_decl)))
    {
      r = build_stmt (spec_location, MUST_NOT_THROW_EXPR,
		      NULL_TREE, NULL_TREE);
      TREE_SIDE_EFFECTS (r) = 1;
    }
  else
    r = build_stmt (spec_location, EH_SPEC_BLOCK, NULL_TREE, NULL_TREE);
  add_stmt (r);
  TREE_OPERAND (r, 0) = push_stmt_list ();
  return r;
}

void
finish_eh_spec_block (tree raw_raises, tree eh_spec_block)
{
  tree raises;

  TREE_OPERAND (eh_spec_block, 0)
    = pop_stmt_list (TREE_OPERAND (eh_spec_block, 0));

  if (TREE_CODE (eh_spec_block) == MUST_NOT_THROW_EXPR)
    return;

  /* Strip cv quals, etc, from the specification types.  */
  for (raises = NULL_TREE;
       raw_raises && TREE_VALUE (raw_raises);
       raw_raises = TREE_CHAIN (raw_raises))
    {
      tree type = prepare_eh_type (TREE_VALUE (raw_raises));
      tree tinfo = eh_type_info (type);

      mark_used (tinfo);
      raises = tree_cons (NULL_TREE, type, raises);
    }

  EH_SPEC_RAISES (eh_spec_block) = raises;
}

/* Return a pointer to a buffer for an exception object of type TYPE.  */

static tree
do_allocate_exception (tree type)
{
  if (!allocate_exception_fn)
    /* Declare void *__cxa_allocate_exception(size_t) throw().  */
    allocate_exception_fn
      = declare_library_fn ("__cxa_allocate_exception",
			    ptr_type_node, size_type_node,
			    ECF_NOTHROW | ECF_MALLOC | ECF_COLD, ECF_TM_PURE);

  return cp_build_function_call_nary (allocate_exception_fn,
				      tf_warning_or_error,
				      size_in_bytes (type), NULL_TREE);
}

/* Call __cxa_free_exception from a cleanup.  This is never invoked
   directly, but see the comment for stabilize_throw_expr.  */

static tree
do_free_exception (tree ptr)
{
  if (!free_exception_fn)
    /* Declare void __cxa_free_exception (void *) throw().  */
    free_exception_fn
      = declare_library_fn ("__cxa_free_exception",
			    void_type_node, ptr_type_node,
			    ECF_NOTHROW | ECF_LEAF, ECF_TM_PURE);

  return cp_build_function_call_nary (free_exception_fn,
				      tf_warning_or_error, ptr, NULL_TREE);
}

/* Wrap all cleanups for TARGET_EXPRs in MUST_NOT_THROW_EXPR.
   Called from build_throw via walk_tree_without_duplicates.  */

static tree
wrap_cleanups_r (tree *tp, int *walk_subtrees, void * /*data*/)
{
  tree exp = *tp;
  tree cleanup;

  /* Don't walk into types.  */
  if (TYPE_P (exp))
    {
      *walk_subtrees = 0;
      return NULL_TREE;
    }
  if (TREE_CODE (exp) != TARGET_EXPR)
    return NULL_TREE;

  cleanup = TARGET_EXPR_CLEANUP (exp);
  if (cleanup)
    {
      cleanup = build2 (MUST_NOT_THROW_EXPR, void_type_node, cleanup,
			NULL_TREE);
      TARGET_EXPR_CLEANUP (exp) = cleanup;
    }

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Build a throw expression.  */

tree
build_throw (location_t loc, tree exp)
{
  if (exp == error_mark_node)
    return exp;

  if (processing_template_decl)
    {
      if (cfun)
	current_function_returns_abnormally = 1;
      exp = build_min (THROW_EXPR, void_type_node, exp);
      SET_EXPR_LOCATION (exp, loc);
      return exp;
    }

  if (exp && null_node_p (exp))
    warning_at (loc, 0,
		"throwing NULL, which has integral, not pointer type");

  if (exp != NULL_TREE)
    {
      if (!is_admissible_throw_operand_or_catch_parameter (exp, true))
	return error_mark_node;
    }

  if (! doing_eh ())
    return error_mark_node;

  if (exp)
    {
      tree throw_type;
      tree temp_type;
      tree cleanup;
      tree object, ptr;
      tree allocate_expr;

      /* The CLEANUP_TYPE is the internal type of a destructor.  */
      if (!cleanup_type)
	{
	  tree tmp = build_function_type_list (void_type_node,
					       ptr_type_node, NULL_TREE);
	  cleanup_type = build_pointer_type (tmp);
	}

      if (!throw_fn)
	{
	  tree args[3] = {ptr_type_node, ptr_type_node, cleanup_type};

	  throw_fn = declare_library_fn_1 ("__cxa_throw",
					   ECF_NORETURN | ECF_COLD,
					   void_type_node, 3, args);
	  if (flag_tm && throw_fn != error_mark_node)
	    {
	      tree itm_fn = declare_library_fn_1 ("_ITM_cxa_throw",
						  ECF_NORETURN | ECF_COLD,
						  void_type_node, 3, args);
	      if (itm_fn != error_mark_node)
		{
		  apply_tm_attr (itm_fn, get_identifier ("transaction_pure"));
		  record_tm_replacement (throw_fn, itm_fn);
		}
	    }
	}

      /* [except.throw]

	 A throw-expression initializes a temporary object, the type
	 of which is determined by removing any top-level
	 cv-qualifiers from the static type of the operand of throw
	 and adjusting the type from "array of T" or "function return
	 T" to "pointer to T" or "pointer to function returning T"
	 respectively.  */
      temp_type = is_bitfield_expr_with_lowered_type (exp);
      if (!temp_type)
	temp_type = cv_unqualified (type_decays_to (TREE_TYPE (exp)));

      /* OK, this is kind of wacky.  The standard says that we call
	 terminate when the exception handling mechanism, after
	 completing evaluation of the expression to be thrown but
	 before the exception is caught (_except.throw_), calls a
	 user function that exits via an uncaught exception.

	 So we have to protect the actual initialization of the
	 exception object with terminate(), but evaluate the
	 expression first.  Since there could be temps in the
	 expression, we need to handle that, too.  We also expand
	 the call to __cxa_allocate_exception first (which doesn't
	 matter, since it can't throw).  */

      /* Allocate the space for the exception.  */
      allocate_expr = do_allocate_exception (temp_type);
      if (allocate_expr == error_mark_node)
	return error_mark_node;
      allocate_expr = get_target_expr (allocate_expr);
      ptr = TARGET_EXPR_SLOT (allocate_expr);
      TARGET_EXPR_CLEANUP (allocate_expr) = do_free_exception (ptr);
      CLEANUP_EH_ONLY (allocate_expr) = 1;

      object = build_nop (build_pointer_type (temp_type), ptr);
      object = cp_build_fold_indirect_ref (object);

      /* And initialize the exception object.  */
      if (CLASS_TYPE_P (temp_type))
	{
	  int flags = LOOKUP_NORMAL | LOOKUP_ONLYCONVERTING;
	  bool converted = false;
	  location_t exp_loc = cp_expr_loc_or_loc (exp, loc);

	  /* Under C++0x [12.8/16 class.copy], a thrown lvalue is sometimes
	     treated as an rvalue for the purposes of overload resolution
	     to favor move constructors over copy constructors.  */
	  if (tree moved = treat_lvalue_as_rvalue_p (exp, /*return*/false))
	    {
	      if (cxx_dialect < cxx20)
		{
		  releasing_vec exp_vec (make_tree_vector_single (moved));
		  moved = (build_special_member_call
			   (object, complete_ctor_identifier, &exp_vec,
			    TREE_TYPE (object), flags|LOOKUP_PREFER_RVALUE,
			    tf_none));
		  if (moved != error_mark_node)
		    {
		      exp = moved;
		      converted = true;
		    }
		}
	      else
		/* In C++20 we just treat the return value as an rvalue that
		   can bind to lvalue refs.  */
		exp = moved;
	    }

	  /* Call the copy constructor.  */
	  if (!converted)
	    {
	      releasing_vec exp_vec (make_tree_vector_single (exp));
	      exp = (build_special_member_call
		     (object, complete_ctor_identifier, &exp_vec,
		      TREE_TYPE (object), flags, tf_warning_or_error));
	    }

	  if (exp == error_mark_node)
	    {
	      inform (exp_loc, "  in thrown expression");
	      return error_mark_node;
	    }
	}
      else
	{
	  tree tmp = decay_conversion (exp, tf_warning_or_error);
	  if (tmp == error_mark_node)
	    return error_mark_node;
	  exp = build2 (INIT_EXPR, temp_type, object, tmp);
	}

      /* Mark any cleanups from the initialization as MUST_NOT_THROW, since
	 they are run after the exception object is initialized.  */
      cp_walk_tree_without_duplicates (&exp, wrap_cleanups_r, 0);

      /* Prepend the allocation.  */
      exp = build2 (COMPOUND_EXPR, TREE_TYPE (exp), allocate_expr, exp);

      /* Force all the cleanups to be evaluated here so that we don't have
	 to do them during unwinding.  */
      exp = build1 (CLEANUP_POINT_EXPR, void_type_node, exp);

      throw_type = build_eh_type_type (prepare_eh_type (TREE_TYPE (object)));

      cleanup = NULL_TREE;
      if (type_build_dtor_call (TREE_TYPE (object)))
	{
	  tree dtor_fn = lookup_fnfields (TYPE_BINFO (TREE_TYPE (object)),
					  complete_dtor_identifier, 0,
					  tf_warning_or_error);
	  dtor_fn = BASELINK_FUNCTIONS (dtor_fn);
	  mark_used (dtor_fn);
	  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (object)))
	    {
	      cxx_mark_addressable (dtor_fn);
	      /* Pretend it's a normal function.  */
	      cleanup = build1 (ADDR_EXPR, cleanup_type, dtor_fn);
	    }
	}
      if (cleanup == NULL_TREE)
	cleanup = build_int_cst (cleanup_type, 0);

      /* ??? Indicate that this function call throws throw_type.  */
      tree tmp = cp_build_function_call_nary (throw_fn, tf_warning_or_error,
					      ptr, throw_type, cleanup,
					      NULL_TREE);

      /* Tack on the initialization stuff.  */
      exp = build2 (COMPOUND_EXPR, TREE_TYPE (tmp), exp, tmp);
    }
  else
    {
      /* Rethrow current exception.  */
      if (!rethrow_fn)
	{
	  rethrow_fn = declare_library_fn_1 ("__cxa_rethrow",
					     ECF_NORETURN | ECF_COLD,
					     void_type_node, 0, NULL);
	  if (flag_tm && rethrow_fn != error_mark_node)
	    apply_tm_attr (rethrow_fn, get_identifier ("transaction_pure"));
	}

      /* ??? Indicate that this function call allows exceptions of the type
	 of the enclosing catch block (if known).  */
      exp = cp_build_function_call_vec (rethrow_fn, NULL, tf_warning_or_error);
    }

  exp = build1_loc (loc, THROW_EXPR, void_type_node, exp);

  return exp;
}

/* Make sure TYPE is complete, pointer to complete, reference to
   complete, or pointer to cv void. Issue diagnostic on failure.
   Return the zero on failure and nonzero on success. FROM can be
   the expr or decl from whence TYPE came, if available.  */

static int
complete_ptr_ref_or_void_ptr_p (tree type, tree from)
{
  int is_ptr;

  /* Check complete.  */
  type = complete_type_or_else (type, from);
  if (!type)
    return 0;

  /* Or a pointer or ref to one, or cv void *.  */
  is_ptr = TYPE_PTR_P (type);
  if (is_ptr || TYPE_REF_P (type))
    {
      tree core = TREE_TYPE (type);

      if (is_ptr && VOID_TYPE_P (core))
	/* OK */;
      else if (!complete_type_or_else (core, from))
	return 0;
    }
  return 1;
}

/* If IS_THROW is true return truth-value if T is an expression admissible
   in throw-expression, i.e. if it is not of incomplete type or a pointer/
   reference to such a type or of an abstract class type.
   If IS_THROW is false, likewise for a catch parameter, same requirements
   for its type plus rvalue reference type is also not admissible.  */

static bool
is_admissible_throw_operand_or_catch_parameter (tree t, bool is_throw)
{
  tree expr = is_throw ? t : NULL_TREE;
  tree type = TREE_TYPE (t);

  /* C++11 [except.handle] The exception-declaration shall not denote
     an incomplete type, an abstract class type, or an rvalue reference 
     type.  */

  /* 15.1/4 [...] The type of the throw-expression shall not be an
	    incomplete type, or a pointer or a reference to an incomplete
	    type, other than void*, const void*, volatile void*, or
	    const volatile void*.  Except for these restriction and the
	    restrictions on type matching mentioned in 15.3, the operand
	    of throw is treated exactly as a function argument in a call
	    (5.2.2) or the operand of a return statement.  */
  if (!complete_ptr_ref_or_void_ptr_p (type, expr))
    return false;

  tree nonref_type = non_reference (type);
  if (!verify_type_context (input_location, TCTX_EXCEPTIONS, nonref_type))
    return false;

  /* 10.4/3 An abstract class shall not be used as a parameter type,
	    as a function return type or as type of an explicit
	    conversion.  */
  else if (abstract_virtuals_error (is_throw ? ACU_THROW : ACU_CATCH, type))
    return false;
  else if (!is_throw
	   && TYPE_REF_P (type)
	   && TYPE_REF_IS_RVALUE (type))
    {
      error ("cannot declare %<catch%> parameter to be of rvalue "
	     "reference type %qT", type);
      return false;
    }
  else if (variably_modified_type_p (type, NULL_TREE))
    {
      if (is_throw)
	error_at (cp_expr_loc_or_input_loc (expr),
		  "cannot throw expression of type %qT because it involves "
		  "types of variable size", type);
      else
	error ("cannot catch type %qT because it involves types of "
	       "variable size", type);
      return false;
    }

  return true;
}

/* Returns nonzero if FN is a declaration of a standard C library
   function which is known not to throw.

   [lib.res.on.exception.handling]: None of the functions from the
   Standard C library shall report an error by throwing an
   exception, unless it calls a program-supplied function that
   throws an exception.  */

#include "cfns.h"

int
nothrow_libfn_p (const_tree fn)
{
  tree id;

  if (TREE_PUBLIC (fn)
      && DECL_EXTERNAL (fn)
      && DECL_NAMESPACE_SCOPE_P (fn)
      && DECL_EXTERN_C_P (fn))
    /* OK */;
  else
    /* Can't be a C library function.  */
    return 0;

  /* Being a C library function, DECL_ASSEMBLER_NAME == DECL_NAME
     unless the system headers are playing rename tricks, and if
     they are, we don't want to be confused by them.  */
  id = DECL_NAME (fn);
  const struct libc_name_struct *s
    = libc_name::libc_name_p (IDENTIFIER_POINTER (id), IDENTIFIER_LENGTH (id));
  if (s == NULL)
    return 0;
  switch (s->c_ver)
    {
    case 89: return 1;
    case 99: return !flag_iso || flag_isoc99;
    case 11: return !flag_iso || flag_isoc11;
    default: gcc_unreachable ();
    }
}

/* Returns nonzero if an exception of type FROM will be caught by a
   handler for type TO, as per [except.handle].  */

static bool
can_convert_eh (tree to, tree from)
{
  to = non_reference (to);
  from = non_reference (from);

  if (same_type_ignoring_top_level_qualifiers_p (to, from))
    return true;

  if (TYPE_PTR_P (to) && TYPE_PTR_P (from))
    {
      to = TREE_TYPE (to);
      from = TREE_TYPE (from);

      if (! at_least_as_qualified_p (to, from))
	return false;

      if (VOID_TYPE_P (to))
	return true;

      /* Else fall through.  */
    }

  if (CLASS_TYPE_P (to) && CLASS_TYPE_P (from)
      && publicly_uniquely_derived_p (to, from))
    return true;

  return false;
}

/* Check whether any of the handlers in I are shadowed by another handler
   accepting TYPE.  Note that the shadowing may not be complete; even if
   an exception of type B would be caught by a handler for A, there could
   be a derived class C for which A is an ambiguous base but B is not, so
   the handler for B would catch an exception of type C.  */

static void
check_handlers_1 (tree master, tree_stmt_iterator i)
{
  tree type = TREE_TYPE (master);

  for (; !tsi_end_p (i); tsi_next (&i))
    {
      tree handler = tsi_stmt (i);
      if (TREE_TYPE (handler) && can_convert_eh (type, TREE_TYPE (handler)))
	{
	  auto_diagnostic_group d;
	  if (warning_at (EXPR_LOCATION (handler), OPT_Wexceptions,
			  "exception of type %qT will be caught by earlier "
			  "handler", TREE_TYPE (handler)))
	    inform (EXPR_LOCATION (master), "for type %qT", type);
	  break;
	}
    }
}

/* Given a STATEMENT_LIST of HANDLERs, make sure that they're OK.  */

void
check_handlers (tree handlers)
{
  tree_stmt_iterator i;

  /* If we don't have a STATEMENT_LIST, then we've just got one
     handler, and thus nothing to warn about.  */
  if (TREE_CODE (handlers) != STATEMENT_LIST)
    return;

  i = tsi_start (handlers);
  if (!tsi_end_p (i))
    while (1)
      {
	tree handler = tsi_stmt (i);
	tsi_next (&i);

	/* No more handlers; nothing to shadow.  */
	if (tsi_end_p (i))
	  break;
	if (TREE_TYPE (handler) == NULL_TREE)
	  permerror (EXPR_LOCATION (handler), "%<...%>"
		     " handler must be the last handler for its try block");
	else
	  check_handlers_1 (handler, i);
      }
}

/* walk_tree helper for finish_noexcept_expr.  Returns non-null if the
   expression *TP causes the noexcept operator to evaluate to false.

   5.3.7 [expr.noexcept]: The result of the noexcept operator is false if
   in a potentially-evaluated context the expression would contain
   * a potentially evaluated call to a function, member function,
     function pointer, or member function pointer that does not have a
     non-throwing exception-specification (15.4),
   * a potentially evaluated throw-expression (15.1),
   * a potentially evaluated dynamic_cast expression dynamic_cast<T>(v),
     where T is a reference type, that requires a run-time check (5.2.7), or
   * a potentially evaluated typeid expression (5.2.8) applied to a glvalue
     expression whose type is a polymorphic class type (10.3).  */

static tree
check_noexcept_r (tree *tp, int *walk_subtrees, void *)
{
  tree t = *tp;
  enum tree_code code = TREE_CODE (t);

  if (unevaluated_p (code))
    *walk_subtrees = false;
  else if ((code == CALL_EXPR && CALL_EXPR_FN (t))
	   || code == AGGR_INIT_EXPR)
    {
      /* We can only use the exception specification of the called function
	 for determining the value of a noexcept expression; we can't use
	 TREE_NOTHROW, as it might have a different value in another
	 translation unit, creating ODR problems.

         We could use TREE_NOTHROW (t) for !TREE_PUBLIC fns, though... */
      tree fn = cp_get_callee (t);
      if (concept_check_p (fn))
	return NULL_TREE;
      tree type = TREE_TYPE (fn);
      gcc_assert (INDIRECT_TYPE_P (type));
      type = TREE_TYPE (type);

      STRIP_NOPS (fn);
      if (TREE_CODE (fn) == ADDR_EXPR)
	fn = TREE_OPERAND (fn, 0);
      if (TREE_CODE (fn) == FUNCTION_DECL)
	{
	  /* We do use TREE_NOTHROW for ABI internals like __dynamic_cast,
	     and for C library functions known not to throw.  */
	  if (DECL_EXTERN_C_P (fn)
	      && (DECL_ARTIFICIAL (fn)
		  || nothrow_libfn_p (fn)))
	    return TREE_NOTHROW (fn) ? NULL_TREE : fn;
	  /* We used to treat a call to a constexpr function as noexcept if
	     the call was a constant expression (CWG 1129).  This has changed
	     in P0003 whereby noexcept has no special rule for constant
	     expressions anymore.  Since the current behavior is important for
	     certain library functionality, we treat this as a DR, therefore
	     adjusting the behavior for C++11 and C++14.  Previously, we had
	     to evaluate the noexcept-specifier's operand here, but that could
	     cause instantiations that would fail.  */
	}
      if (!TYPE_NOTHROW_P (type))
	return fn;
    }

  return NULL_TREE;
}

/* If a function that causes a noexcept-expression to be false isn't
   defined yet, remember it and check it for TREE_NOTHROW again at EOF.  */

struct GTY(()) pending_noexcept {
  tree fn;
  location_t loc;
};
static GTY(()) vec<pending_noexcept, va_gc> *pending_noexcept_checks;

/* FN is a FUNCTION_DECL that caused a noexcept-expr to be false.  Warn if
   it can't throw.

   TODO: Consider extending -Wnoexcept to do something like walk_subtrees in the
   case of a defaulted function that obtained a noexcept(false) spec.  */

static void
maybe_noexcept_warning (tree fn)
{
  if (TREE_NOTHROW (fn)
      && (!DECL_IN_SYSTEM_HEADER (fn)
	  || global_dc->dc_warn_system_headers))
    {
      auto s = make_temp_override (global_dc->dc_warn_system_headers, true);
      auto_diagnostic_group d;
      if (warning (OPT_Wnoexcept, "noexcept-expression evaluates to %<false%> "
		   "because of a call to %qD", fn))
	inform (DECL_SOURCE_LOCATION (fn),
		"but %qD does not throw; perhaps "
		"it should be declared %<noexcept%>", fn);
    }
}

/* Check any functions that weren't defined earlier when they caused a
   noexcept expression to evaluate to false.  */

void
perform_deferred_noexcept_checks (void)
{
  int i;
  pending_noexcept *p;
  location_t saved_loc = input_location;
  FOR_EACH_VEC_SAFE_ELT (pending_noexcept_checks, i, p)
    {
      input_location = p->loc;
      maybe_noexcept_warning (p->fn);
    }
  input_location = saved_loc;
}

/* Evaluate noexcept ( EXPR ).  */

tree
finish_noexcept_expr (tree expr, tsubst_flags_t complain)
{
  if (expr == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    return build_min (NOEXCEPT_EXPR, boolean_type_node, expr);

  return (expr_noexcept_p (expr, complain)
	  ? boolean_true_node : boolean_false_node);
}

/* Returns whether EXPR is noexcept, possibly warning if allowed by
   COMPLAIN.  */

bool
expr_noexcept_p (tree expr, tsubst_flags_t complain)
{
  tree fn;

  if (expr == error_mark_node)
    return false;

  fn = cp_walk_tree_without_duplicates (&expr, check_noexcept_r, 0);
  if (fn)
    {
      if ((complain & tf_warning) && warn_noexcept
	  && TREE_CODE (fn) == FUNCTION_DECL)
	{
	  if (!DECL_INITIAL (fn))
	    {
	      /* Not defined yet; check again at EOF.  */
	      pending_noexcept p = {fn, input_location};
	      vec_safe_push (pending_noexcept_checks, p);
	    }
	  else
	    maybe_noexcept_warning (fn);
	}
      return false;
    }
  else
    return true;
}

/* Return true iff SPEC is throw() or noexcept(true).  */

bool
nothrow_spec_p (const_tree spec)
{
  gcc_assert (!DEFERRED_NOEXCEPT_SPEC_P (spec));

  if (spec == empty_except_spec
      || spec == noexcept_true_spec)
    return true;

  gcc_assert (!spec
	      || TREE_VALUE (spec)
	      || spec == noexcept_false_spec
	      || TREE_PURPOSE (spec) == error_mark_node
	      || UNPARSED_NOEXCEPT_SPEC_P (spec)
	      || processing_template_decl);

  return false;
}

/* For FUNCTION_TYPE or METHOD_TYPE, true if NODE is noexcept.  This is the
   case for things declared noexcept(true) and, with -fnothrow-opt, for
   throw() functions.  */

bool
type_noexcept_p (const_tree type)
{
  tree spec = TYPE_RAISES_EXCEPTIONS (type);
  gcc_assert (!DEFERRED_NOEXCEPT_SPEC_P (spec));
  if (flag_nothrow_opt)
    return nothrow_spec_p (spec);
  else
    return spec == noexcept_true_spec;
}

/* For FUNCTION_TYPE or METHOD_TYPE, true if NODE can throw any type,
   i.e. no exception-specification or noexcept(false).  */

bool
type_throw_all_p (const_tree type)
{
  tree spec = TYPE_RAISES_EXCEPTIONS (type);
  gcc_assert (!DEFERRED_NOEXCEPT_SPEC_P (spec));
  return spec == NULL_TREE || spec == noexcept_false_spec;
}

/* Create a representation of the noexcept-specification with
   constant-expression of EXPR.  COMPLAIN is as for tsubst.  */

tree
build_noexcept_spec (tree expr, tsubst_flags_t complain)
{
  if (check_for_bare_parameter_packs (expr))
    return error_mark_node;
  if (TREE_CODE (expr) != DEFERRED_NOEXCEPT
      && !value_dependent_expression_p (expr))
    {
      expr = build_converted_constant_bool_expr (expr, complain);
      expr = instantiate_non_dependent_expr_sfinae (expr, complain);
      expr = cxx_constant_value (expr);
    }
  if (TREE_CODE (expr) == INTEGER_CST)
    {
      if (operand_equal_p (expr, boolean_true_node, 0))
	return noexcept_true_spec;
      else
	{
	  gcc_checking_assert (operand_equal_p (expr, boolean_false_node, 0));
	  return noexcept_false_spec;
	}
    }
  else if (expr == error_mark_node)
    return error_mark_node;
  else
    {
      gcc_assert (processing_template_decl
		  || TREE_CODE (expr) == DEFERRED_NOEXCEPT);
      if (TREE_CODE (expr) != DEFERRED_NOEXCEPT)
	/* Avoid problems with a function type built with a dependent typedef
	   being reused in another scope (c++/84045).  */
	expr = strip_typedefs_expr (expr);
      return build_tree_list (expr, NULL_TREE);
    }
}

/* If the current function has a cleanup that might throw, and the return value
   has a non-trivial destructor, return a MODIFY_EXPR to set
   current_retval_sentinel so that we know that the return value needs to be
   destroyed on throw.  Otherwise, returns NULL_TREE.  */

tree
maybe_set_retval_sentinel ()
{
  if (processing_template_decl)
    return NULL_TREE;
  tree retval = DECL_RESULT (current_function_decl);
  if (!TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (retval)))
    return NULL_TREE;
  if (!cp_function_chain->throwing_cleanup)
    return NULL_TREE;

  if (!current_retval_sentinel)
    {
      /* Just create the temporary now, maybe_splice_retval_cleanup
	 will do the rest.  */
      current_retval_sentinel = create_temporary_var (boolean_type_node);
      DECL_INITIAL (current_retval_sentinel) = boolean_false_node;
      pushdecl_outermost_localscope (current_retval_sentinel);
    }

  return build2 (MODIFY_EXPR, boolean_type_node,
		 current_retval_sentinel, boolean_true_node);
}

/* COMPOUND_STMT is the STATEMENT_LIST for the current function body.  If
   current_retval_sentinel was set in this function, wrap the body in a
   CLEANUP_STMT to destroy the return value on throw.  */

void
maybe_splice_retval_cleanup (tree compound_stmt)
{
  /* If need_retval_cleanup set current_retval_sentinel, wrap the function body
     in a CLEANUP_STMT to handle destroying the return value.  */
  if (!DECL_CONSTRUCTOR_P (current_function_decl)
      && !DECL_DESTRUCTOR_P (current_function_decl)
      && current_retval_sentinel)
    {
      location_t loc = DECL_SOURCE_LOCATION (current_function_decl);

      /* Add a DECL_EXPR for current_retval_sentinel.  */
      tree_stmt_iterator iter = tsi_start (compound_stmt);
      tree retval = DECL_RESULT (current_function_decl);
      tree decl_expr = build_stmt (loc, DECL_EXPR, current_retval_sentinel);
      tsi_link_before (&iter, decl_expr, TSI_SAME_STMT);

      /* Skip past other decls, they can't contain a return.  */
      while (TREE_CODE (tsi_stmt (iter)) == DECL_EXPR)
	tsi_next (&iter);
      gcc_assert (!tsi_end_p (iter));

      /* Wrap the rest of the STATEMENT_LIST in a CLEANUP_STMT.  */
      tree stmts = NULL_TREE;
      while (!tsi_end_p (iter))
	{
	  append_to_statement_list_force (tsi_stmt (iter), &stmts);
	  tsi_delink (&iter);
	}
      tree dtor = build_cleanup (retval);
      tree cond = build3 (COND_EXPR, void_type_node, current_retval_sentinel,
			  dtor, void_node);
      tree cleanup = build_stmt (loc, CLEANUP_STMT,
				 stmts, cond, retval);
      CLEANUP_EH_ONLY (cleanup) = true;
      append_to_statement_list_force (cleanup, &compound_stmt);
    }
}

#include "gt-cp-except.h"
