/* Handle exceptional things in C++.
   Copyright (C) 1989, 92-97, 1998, 1999 Free Software Foundation, Inc.
   Contributed by Michael Tiemann <tiemann@cygnus.com>
   Rewritten by Mike Stump <mrs@cygnus.com>, based upon an
   initial re-implementation courtesy Tad Hunt.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "cp-tree.h"
#include "flags.h"
#include "obstack.h"
#include "expr.h"
#include "output.h"
#include "except.h"
#include "function.h"
#include "defaults.h"
#include "toplev.h"
#include "eh-common.h"

rtx expand_builtin_return_addr	PROTO((enum built_in_function, int, rtx));

/* Holds the fndecl for __builtin_return_address.  */
tree builtin_return_address_fndecl;

/* A couple of backend routines from m88k.c */

static void push_eh_cleanup PROTO((void));
static tree build_eh_type_type PROTO((tree));
static tree build_eh_type PROTO((tree));
static void expand_end_eh_spec PROTO((tree));
static tree call_eh_info PROTO((void));
static void push_eh_info PROTO((void));
static tree get_eh_info PROTO((void));
static tree get_eh_value PROTO((void));
#if 0
static tree get_eh_type PROTO((void));
static tree get_eh_caught PROTO((void));
static tree get_eh_handlers PROTO((void));
#endif
static tree do_pop_exception PROTO((void));
static void process_start_catch_block PROTO((tree, tree));
static tree build_eh_type_type_ref PROTO((tree));
static tree build_terminate_handler PROTO((void));
static tree alloc_eh_object PROTO((tree));

#if 0
/* This is the startup, and finish stuff per exception table.  */

/* XXX - Tad: exception handling section */
#ifndef EXCEPT_SECTION_ASM_OP
#define EXCEPT_SECTION_ASM_OP	"section\t.gcc_except_table,\"a\",@progbits"
#endif

#ifdef EXCEPT_SECTION_ASM_OP

 /* on machines which support it, the exception table lives in another section,
	but it needs a label so we can reference it...  This sets up that
    label! */
asm (EXCEPT_SECTION_ASM_OP);
exception_table __EXCEPTION_TABLE__[1] = { (void*)0, (void*)0, (void*)0 };
asm (TEXT_SECTION_ASM_OP);

#endif /* EXCEPT_SECTION_ASM_OP */

#ifdef EXCEPT_SECTION_ASM_OP

 /* we need to know where the end of the exception table is... so this
    is how we do it! */

asm (EXCEPT_SECTION_ASM_OP);
exception_table __EXCEPTION_END__[1] = { (void*)-1, (void*)-1, (void*)-1 };
asm (TEXT_SECTION_ASM_OP);

#endif /* EXCEPT_SECTION_ASM_OP */

#endif

#include "decl.h"
#include "insn-flags.h"
#include "obstack.h"

/* ======================================================================
   Briefly the algorithm works like this:

     When a constructor or start of a try block is encountered,
     push_eh_entry (&eh_stack) is called.  Push_eh_entry () creates a
     new entry in the unwind protection stack and returns a label to
     output to start the protection for that block.

     When a destructor or end try block is encountered, pop_eh_entry
     (&eh_stack) is called.  Pop_eh_entry () returns the eh_entry it
     created when push_eh_entry () was called.  The eh_entry structure
     contains three things at this point.  The start protect label,
     the end protect label, and the exception handler label.  The end
     protect label should be output before the call to the destructor
     (if any). If it was a destructor, then its parse tree is stored
     in the finalization variable in the eh_entry structure.  Otherwise
     the finalization variable is set to NULL to reflect the fact that
     it is the end of a try block.  Next, this modified eh_entry node
     is enqueued in the finalizations queue by calling
     enqueue_eh_entry (&queue,entry).

	+---------------------------------------------------------------+
	|XXX: Will need modification to deal with partially		|
	|			constructed arrays of objects		|
	|								|
	|	Basically, this consists of keeping track of how many	|
	|	of the objects have been constructed already (this	|
	|	should be in a register though, so that shouldn't be a	|
	|	problem.						|
	+---------------------------------------------------------------+

     When a catch block is encountered, there is a lot of work to be
     done.

     Since we don't want to generate the catch block inline with the
     regular flow of the function, we need to have some way of doing
     so.  Luckily, we can use sequences to defer the catch sections.
     When the start of a catch block is encountered, we start the
     sequence.  After the catch block is generated, we end the
     sequence.

     Next we must insure that when the catch block is executed, all
     finalizations for the matching try block have been completed.  If
     any of those finalizations throw an exception, we must call
     terminate according to the ARM (section r.15.6.1).  What this
     means is that we need to dequeue and emit finalizations for each
     entry in the eh_queue until we get to an entry with a NULL
     finalization field.  For any of the finalization entries, if it
     is not a call to terminate (), we must protect it by giving it
     another start label, end label, and exception handler label,
     setting its finalization tree to be a call to terminate (), and
     enqueue'ing this new eh_entry to be output at an outer level.
     Finally, after all that is done, we can get around to outputting
     the catch block which basically wraps all the "catch (...) {...}"
     statements in a big if/then/else construct that matches the
     correct block to call.
     
     ===================================================================== */

/* local globals for function calls
   ====================================================================== */

/* Used to cache "terminate" and "__throw_type_match*".  */
static tree Terminate, CatchMatch;

/* Used to cache __find_first_exception_table_match for throw.  */
static tree FirstExceptionMatch;

/* Used to cache a call to __unwind_function.  */
static tree Unwind;

/* ====================================================================== */


/* ========================================================================= */



/* local globals - these local globals are for storing data necessary for
   generating the exception table and code in the correct order.

   ========================================================================= */

extern rtx catch_clauses;
extern tree const_ptr_type_node;

/* ========================================================================= */

/* sets up all the global eh stuff that needs to be initialized at the
   start of compilation.

   This includes:
		- Setting up all the function call trees.  */

void
init_exception_processing ()
{
  /* void vtype () */
  tree vtype = build_function_type (void_type_node, void_list_node);
  
  if (flag_honor_std)
    push_namespace (get_identifier ("std"));
  Terminate = auto_function (get_identifier ("terminate"),
			     vtype, NOT_BUILT_IN);
  TREE_THIS_VOLATILE (Terminate) = 1;
  if (flag_honor_std)
    pop_namespace ();

  push_lang_context (lang_name_c);

  set_exception_lang_code (EH_LANG_C_plus_plus);
  set_exception_version_code (1);

  CatchMatch
    = builtin_function (flag_rtti
			? "__throw_type_match_rtti"
			: "__throw_type_match",
			build_function_type (ptr_type_node,
					     tree_cons (NULL_TREE, const_ptr_type_node,
							tree_cons (NULL_TREE, const_ptr_type_node,
								   tree_cons (NULL_TREE, ptr_type_node,
									      void_list_node)))),
			NOT_BUILT_IN, NULL_PTR);
  FirstExceptionMatch
    = builtin_function ("__find_first_exception_table_match",
			build_function_type (ptr_type_node,
					     tree_cons (NULL_TREE, ptr_type_node,
							void_list_node)),
			NOT_BUILT_IN, NULL_PTR);
  Unwind
    = builtin_function ("__unwind_function",
			build_function_type (void_type_node,
					     tree_cons (NULL_TREE, ptr_type_node,
							void_list_node)),
			NOT_BUILT_IN, NULL_PTR);

  pop_lang_context ();

  /* If we use setjmp/longjmp EH, arrange for all cleanup actions to
     be protected with __terminate.  */
  protect_cleanup_actions_with_terminate = 1;
}

/* Retrieve a pointer to the cp_eh_info node for the current exception.  */

static tree
call_eh_info ()
{
  tree fn;

  fn = get_identifier ("__start_cp_handler");
  if (IDENTIFIER_GLOBAL_VALUE (fn))
    fn = IDENTIFIER_GLOBAL_VALUE (fn);
  else
    {
      tree t1, t, fields[7];

      /* Declare cp_eh_info * __start_cp_handler (void),
	 as defined in exception.cc. */
      push_obstacks_nochange ();
      end_temporary_allocation ();

      /* struct cp_eh_info.  This must match exception.cc.  Note that this
	 type is not pushed anywhere.  */
      t1= make_lang_type (RECORD_TYPE);
      fields[0] = build_lang_field_decl (FIELD_DECL, 
                    get_identifier ("handler_label"), ptr_type_node);
      fields[1] = build_lang_field_decl (FIELD_DECL, 
                    get_identifier ("dynamic_handler_chain"), ptr_type_node);
      fields[2] = build_lang_field_decl (FIELD_DECL, 
                    get_identifier ("info"), ptr_type_node);
      fields[3] = build_lang_field_decl (FIELD_DECL, 
                    get_identifier ("table_index"), ptr_type_node);
      /* N.B.: The fourth field LEN is expected to be
	 the number of fields - 1, not the total number of fields.  */
      finish_builtin_type (t1, "eh_context", fields, 3, ptr_type_node);
      t1 = build_pointer_type (t1);

      t1= make_lang_type (RECORD_TYPE);
      fields[0] = build_lang_field_decl (FIELD_DECL, 
                    get_identifier ("match_function"), ptr_type_node);
      fields[1] = build_lang_field_decl (FIELD_DECL, 
                    get_identifier ("language"), short_integer_type_node);
      fields[2] = build_lang_field_decl (FIELD_DECL, 
                    get_identifier ("version"), short_integer_type_node);
      /* N.B.: The fourth field LEN is expected to be
	 the number of fields - 1, not the total number of fields.  */
      finish_builtin_type (t1, "__eh_info", fields, 2, ptr_type_node);
      t = make_lang_type (RECORD_TYPE);
      fields[0] = build_lang_field_decl (FIELD_DECL, 
                                              get_identifier ("eh_info"), t1);
      fields[1] = build_lang_field_decl (FIELD_DECL, get_identifier ("value"),
					 ptr_type_node);
      fields[2] = build_lang_field_decl (FIELD_DECL, get_identifier ("type"),
					 ptr_type_node);
      fields[3] = build_lang_field_decl
	(FIELD_DECL, get_identifier ("cleanup"),
	 build_pointer_type (build_function_type
			     (ptr_type_node, tree_cons
			      (NULL_TREE, ptr_type_node, void_list_node))));
      fields[4] = build_lang_field_decl (FIELD_DECL, get_identifier ("caught"),
					 boolean_type_node);
      fields[5] = build_lang_field_decl (FIELD_DECL, get_identifier ("next"),
					 build_pointer_type (t));
      fields[6] = build_lang_field_decl
	(FIELD_DECL, get_identifier ("handlers"), long_integer_type_node);
      /* N.B.: The fourth field LEN is expected to be
	 the number of fields - 1, not the total number of fields.  */
      finish_builtin_type (t, "cp_eh_info", fields, 6, ptr_type_node);
      t = build_pointer_type (t);

      /* And now the function.  */
      fn = build_lang_decl (FUNCTION_DECL, fn,
			    build_function_type (t, void_list_node));
      DECL_EXTERNAL (fn) = 1;
      TREE_PUBLIC (fn) = 1;
      DECL_ARTIFICIAL (fn) = 1;
      pushdecl_top_level (fn);
      make_function_rtl (fn);
      pop_obstacks ();
    }
  mark_used (fn);
  return build_function_call (fn, NULL_TREE);
}

/* Retrieve a pointer to the cp_eh_info node for the current exception
   and save it in the current binding level.  */

static void
push_eh_info ()
{
  tree decl, fn = call_eh_info ();

  /* Remember the pointer to the current exception info; it won't change
     during this catch block.  */
  decl = build_decl (VAR_DECL, get_identifier ("__exception_info"),
		     TREE_TYPE (fn));
  DECL_ARTIFICIAL (decl) = 1;
  DECL_INITIAL (decl) = fn;
  decl = pushdecl (decl);
  cp_finish_decl (decl, fn, NULL_TREE, 0, 0);
}

/* Returns a reference to the cp_eh_info node for the current exception.  */

static tree
get_eh_info ()
{
  /* Look for the pointer pushed in push_eh_info.  */
  tree t = lookup_name (get_identifier ("__exception_info"), 0);
  return build_indirect_ref (t, NULL_PTR);
}

/* Returns a reference to the current exception object.  */

static tree
get_eh_value ()
{
  return build_component_ref (get_eh_info (), get_identifier ("value"),
			      NULL_TREE, 0);
}

/* Returns a reference to the current exception type.  */

#if 0
static tree
get_eh_type ()
{
  return build_component_ref (get_eh_info (), get_identifier ("type"),
			      NULL_TREE, 0);
}

/* Returns a reference to whether or not the current exception
   has been caught.  */

static tree
get_eh_caught ()
{
  return build_component_ref (get_eh_info (), get_identifier ("caught"),
			      NULL_TREE, 0);
}

/* Returns a reference to whether or not the current exception
   has been caught.  */

static tree
get_eh_handlers ()
{
  return build_component_ref (get_eh_info (), get_identifier ("handlers"),
			      NULL_TREE, 0);
}
#endif

/* Build a type value for use at runtime for a type that is matched
   against by the exception handling system.  */

static tree
build_eh_type_type (type)
     tree type;
{
  const char *typestring;
  tree exp;

  if (type == error_mark_node)
    return error_mark_node;

  /* peel back references, so they match.  */
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  /* Peel off cv qualifiers.  */
  type = TYPE_MAIN_VARIANT (type);

  if (flag_rtti)
    return build1 (ADDR_EXPR, ptr_type_node, get_typeid_1 (type));

  typestring = build_overload_name (type, 1, 1);
  exp = combine_strings (build_string (strlen (typestring)+1, typestring));
  return build1 (ADDR_EXPR, ptr_type_node, exp);
}

/* Build the address of a runtime type for use in the runtime matching
   field of the new exception model */

static tree
build_eh_type_type_ref (type)
     tree type;
{
  const char *typestring;
  tree exp;

  if (type == error_mark_node)
    return error_mark_node;

  /* peel back references, so they match.  */
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  /* Peel off cv qualifiers.  */
  type = TYPE_MAIN_VARIANT (type);

  push_obstacks_nochange ();
  end_temporary_allocation ();

  if (flag_rtti)
    {
      exp = get_tinfo_fn (type);
      TREE_USED (exp) = 1;
      mark_inline_for_output (exp);
      exp = build1 (ADDR_EXPR, ptr_type_node, exp);
    }
  else
    {
      typestring = build_overload_name (type, 1, 1);
      exp = combine_strings (build_string (strlen (typestring)+1, typestring));
      exp = build1 (ADDR_EXPR, ptr_type_node, exp);
    }
  pop_obstacks ();
  return (exp);
}


/* Build a type value for use at runtime for a exp that is thrown or
   matched against by the exception handling system.  */

static tree
build_eh_type (exp)
     tree exp;
{
  if (flag_rtti)
    {
      exp = build_typeid (exp);
      return build1 (ADDR_EXPR, ptr_type_node, exp);
    }
  return build_eh_type_type (TREE_TYPE (exp));
}

/* This routine is called to mark all the symbols representing runtime
   type functions in the exception table as haveing been referenced.
   This will make sure code is emitted for them. Called from finish_file. */
void 
mark_all_runtime_matches () 
{
  int x,num;
  void **ptr;
  tree exp;
  
  num = find_all_handler_type_matches (&ptr);
  if (num == 0 || ptr == NULL)
    return;
  
  for (x=0; x <num; x++)
    {
      exp = (tree) ptr[x];
      if (TREE_CODE (exp) == ADDR_EXPR)
        {
          exp = TREE_OPERAND (exp, 0);
          if (TREE_CODE (exp) == FUNCTION_DECL)
            TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (exp)) = 1;
        }
    }
  
  free (ptr);
}

/* Build up a call to __cp_pop_exception, to destroy the exception object
   for the current catch block.  HANDLER is either true or false, telling
   the library whether or not it is being called from an exception handler;
   if it is, it avoids destroying the object on rethrow.  */

static tree
do_pop_exception ()
{
  tree fn, cleanup;
  fn = get_identifier ("__cp_pop_exception");
  if (IDENTIFIER_GLOBAL_VALUE (fn))
    fn = IDENTIFIER_GLOBAL_VALUE (fn);
  else
    {
      /* Declare void __cp_pop_exception (void *),
	 as defined in exception.cc. */
      push_obstacks_nochange ();
      end_temporary_allocation ();
      fn = build_lang_decl
	(FUNCTION_DECL, fn,
	 build_function_type (void_type_node, tree_cons
			      (NULL_TREE, ptr_type_node, void_list_node)));
      DECL_EXTERNAL (fn) = 1;
      TREE_PUBLIC (fn) = 1;
      DECL_ARTIFICIAL (fn) = 1;
      pushdecl_top_level (fn);
      make_function_rtl (fn);
      pop_obstacks ();
    }

  mark_used (fn);
  /* Arrange to do a dynamically scoped cleanup upon exit from this region.  */
  cleanup = lookup_name (get_identifier ("__exception_info"), 0);
  cleanup = build_function_call (fn, expr_tree_cons
				 (NULL_TREE, cleanup, NULL_TREE));
  return cleanup;
}

/* This routine creates the cleanup for the current exception.  */

static void
push_eh_cleanup ()
{
  int yes;

  yes = suspend_momentary ();
  /* All cleanups must last longer than normal.  */
  expand_decl_cleanup (NULL_TREE, do_pop_exception ());
  resume_momentary (yes);
}

/* Build up a call to terminate on the function obstack, for use as an
   exception handler.  */

static tree
build_terminate_handler ()
{
  int yes = suspend_momentary ();
  tree term = build_function_call (Terminate, NULL_TREE);
  resume_momentary (yes);
  return term;
}

/* Call this to start a catch block. Typename is the typename, and identifier
   is the variable to place the object in or NULL if the variable doesn't
   matter.  If typename is NULL, that means its a "catch (...)" or catch
   everything.  In that case we don't need to do any type checking.
   (ie: it ends up as the "else" clause rather than an "else if" clause) */

void
expand_start_catch_block (declspecs, declarator)
     tree declspecs, declarator;
{
  tree decl;

  if (processing_template_decl)
    {
      if (declspecs)
	{
	  decl = grokdeclarator (declarator, declspecs, CATCHPARM,
				 1, NULL_TREE);
	  pushdecl (decl);
	  decl = build_min_nt (DECL_STMT, copy_to_permanent (declarator),
			       copy_to_permanent (declspecs),
			       NULL_TREE);
	  add_tree (decl);
	}
      return;
    }

  if (! doing_eh (1))
    return;

  process_start_catch_block (declspecs, declarator);
}


/* This function performs the expand_start_catch_block functionality for 
   exceptions implemented in the new style. __throw determines whether
   a handler needs to be called or not, so the handler itself has to do
   nothing additional. */

static void 
process_start_catch_block (declspecs, declarator)
     tree declspecs, declarator;
{
  tree decl = NULL_TREE;
  tree init;

  /* Create a binding level for the eh_info and the exception object
     cleanup.  */
  pushlevel (0);
  expand_start_bindings (0);


  if (declspecs)
    {
      decl = grokdeclarator (declarator, declspecs, CATCHPARM, 1, NULL_TREE);

      if (decl == NULL_TREE)
	error ("invalid catch parameter");
    }

  if (decl)
    start_catch_handler (build_eh_type_type_ref (TREE_TYPE (decl)));
  else
    start_catch_handler (CATCH_ALL_TYPE);

  emit_line_note (input_filename, lineno);

  push_eh_info ();

  if (decl)
    {
      tree exp;
      tree init_type;

      /* Make sure we mark the catch param as used, otherwise we'll get
	 a warning about an unused ((anonymous)).  */
      TREE_USED (decl) = 1;

      /* Figure out the type that the initializer is.  */
      init_type = TREE_TYPE (decl);
      if (TREE_CODE (init_type) != REFERENCE_TYPE
	  && TREE_CODE (init_type) != POINTER_TYPE)
	init_type = build_reference_type (init_type);

      exp = get_eh_value ();

      /* Since pointers are passed by value, initialize a reference to
	 pointer catch parm with the address of the value slot.  */
      if (TREE_CODE (init_type) == REFERENCE_TYPE
	  && TREE_CODE (TREE_TYPE (init_type)) == POINTER_TYPE)
	exp = build_unary_op (ADDR_EXPR, exp, 1);

      exp = ocp_convert (init_type , exp, CONV_IMPLICIT|CONV_FORCE_TEMP, 0);

      push_eh_cleanup ();

      /* Create a binding level for the parm.  */
      pushlevel (0);
      expand_start_bindings (0);

      init = convert_from_reference (exp);

      /* If the constructor for the catch parm exits via an exception, we
         must call terminate.  See eh23.C.  */
      if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl)))
	{
	  /* Generate the copy constructor call directly so we can wrap it.
	     See also expand_default_init.  */
	  init = ocp_convert (TREE_TYPE (decl), init,
			      CONV_IMPLICIT|CONV_FORCE_TEMP, 0);
	  init = build (TRY_CATCH_EXPR, TREE_TYPE (init), init,
			build_terminate_handler ());
	}

      /* Let `cp_finish_decl' know that this initializer is ok.  */
      DECL_INITIAL (decl) = init;
      decl = pushdecl (decl);

      start_decl_1 (decl);
      cp_finish_decl (decl, init, NULL_TREE, 0,
		      LOOKUP_ONLYCONVERTING|DIRECT_BIND);
    }
  else
    {
      push_eh_cleanup ();

      /* Create a binding level for the parm.  */
      pushlevel (0);
      expand_start_bindings (0);

      /* Fall into the catch all section.  */
    }

  emit_line_note (input_filename, lineno);
}


/* Call this to end a catch block.  Its responsible for emitting the
   code to handle jumping back to the correct place, and for emitting
   the label to jump to if this catch block didn't match.  */

void
expand_end_catch_block ()
{
  if (! doing_eh (1))
    return;

  /* Cleanup the EH parameter.  */
  expand_end_bindings (getdecls (), kept_level_p (), 0);
  poplevel (kept_level_p (), 1, 0);
      
  /* Cleanup the EH object.  */
  expand_end_bindings (getdecls (), kept_level_p (), 0);
  poplevel (kept_level_p (), 1, 0);

  /* Fall to outside the try statement when done executing handler and
     we fall off end of handler.  This is jump Lresume in the
     documentation.  */
  expand_goto (top_label_entry (&caught_return_label_stack));

  end_catch_handler ();
}

/* An exception spec is implemented more or less like:

   try {
     function body;
   } catch (...) {
     void *p[] = { typeid(raises) };
     __check_eh_spec (p, count);
   }

   __check_eh_spec in exception.cc handles all the details.  */

void
expand_start_eh_spec ()
{
  expand_start_try_stmts ();
}

static void
expand_end_eh_spec (raises)
     tree raises;
{
  tree tmp, fn, decl, types = NULL_TREE;
  int count = 0;

  expand_start_all_catch ();
  expand_start_catch_block (NULL_TREE, NULL_TREE);

  /* Build up an array of type_infos.  */
  for (; raises && TREE_VALUE (raises); raises = TREE_CHAIN (raises))
    {
      types = expr_tree_cons
	(NULL_TREE, build_eh_type_type (TREE_VALUE (raises)), types);
      ++count;
    }

  types = build_nt (CONSTRUCTOR, NULL_TREE, types);
  TREE_HAS_CONSTRUCTOR (types) = 1;

  /* We can't pass the CONSTRUCTOR directly, so stick it in a variable.  */
  tmp = build_cplus_array_type (const_ptr_type_node, NULL_TREE);
  decl = build_decl (VAR_DECL, NULL_TREE, tmp);
  DECL_ARTIFICIAL (decl) = 1;
  DECL_INITIAL (decl) = types;
  cp_finish_decl (decl, types, NULL_TREE, 0, 0);

  decl = decay_conversion (decl);

  fn = get_identifier ("__check_eh_spec");
  if (IDENTIFIER_GLOBAL_VALUE (fn))
    fn = IDENTIFIER_GLOBAL_VALUE (fn);
  else
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();

      tmp = tree_cons
	(NULL_TREE, integer_type_node, tree_cons
	 (NULL_TREE, TREE_TYPE (decl), void_list_node));
      tmp = build_function_type	(void_type_node, tmp);
  
      fn = build_lang_decl (FUNCTION_DECL, fn, tmp);
      DECL_EXTERNAL (fn) = 1;
      TREE_PUBLIC (fn) = 1;
      DECL_ARTIFICIAL (fn) = 1;
      TREE_THIS_VOLATILE (fn) = 1;
      pushdecl_top_level (fn);
      make_function_rtl (fn);
      pop_obstacks ();
    }

  mark_used (fn);
  tmp = expr_tree_cons (NULL_TREE, build_int_2 (count, 0), expr_tree_cons
			(NULL_TREE, decl, NULL_TREE));
  tmp = build_call (fn, TREE_TYPE (TREE_TYPE (fn)), tmp);
  expand_expr (tmp, const0_rtx, VOIDmode, EXPAND_NORMAL);

  expand_end_catch_block ();
  expand_end_all_catch ();
}

/* This is called to expand all the toplevel exception handling
   finalization for a function.  It should only be called once per
   function.  */

void
expand_exception_blocks ()
{
  do_pending_stack_adjust ();
  push_to_sequence (catch_clauses);
  expand_leftover_cleanups ();
  do_pending_stack_adjust ();
  catch_clauses = get_insns ();
  end_sequence ();

  /* Do this after we expand leftover cleanups, so that the
     expand_eh_region_end that expand_end_eh_spec does will match the
     right expand_eh_region_start, and make sure it comes out before
     the terminate protected region.  */
  if (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (current_function_decl)))
    {
     expand_end_eh_spec (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (current_function_decl)));
     do_pending_stack_adjust ();
     push_to_sequence (catch_clauses);
     expand_leftover_cleanups ();
     do_pending_stack_adjust ();
     catch_clauses = get_insns ();
     end_sequence ();
    }

  if (catch_clauses)
    {
      rtx funcend = gen_label_rtx ();
      emit_jump (funcend);

      /* We cannot protect n regions this way if we must flow into the
	 EH region through the top of the region, as we have to with
	 the setjmp/longjmp approach.  */
      if (exceptions_via_longjmp == 0)
	expand_eh_region_start ();

      emit_insns (catch_clauses);
      catch_clauses = NULL_RTX;

      if (exceptions_via_longjmp == 0)
	expand_eh_region_end (build_terminate_handler ());

      expand_leftover_cleanups ();

      emit_label (funcend);
    }
}

tree
start_anon_func ()
{
  static int counter = 0;
  int old_interface_unknown = interface_unknown;
  char name[32];
  tree params;
  tree t;

  push_cp_function_context (NULL_TREE);
  push_to_top_level ();

  /* No need to mangle this.  */
  push_lang_context (lang_name_c);

  interface_unknown = 1;

  params = void_list_node;
  /* tcf stands for throw clean function.  */
  sprintf (name, "__tcf_%d", counter++);
  t = make_call_declarator (get_identifier (name), params, NULL_TREE,
			    NULL_TREE);
  start_function (decl_tree_cons (NULL_TREE, get_identifier ("static"),
				  void_list_node),
		  t, NULL_TREE, 0);
  store_parm_decls ();
  pushlevel (0);
  clear_last_expr ();
  push_momentary ();
  expand_start_bindings (0);
  emit_line_note (input_filename, lineno);

  interface_unknown = old_interface_unknown;

  pop_lang_context ();

  return current_function_decl;
}

void
end_anon_func ()
{
  expand_end_bindings (getdecls (), 1, 0);
  poplevel (1, 0, 0);
  pop_momentary ();

  finish_function (lineno, 0, 0);

  pop_from_top_level ();
  pop_cp_function_context (NULL_TREE);
}

/* Return a pointer to a buffer for an exception object of type TYPE.  */

static tree
alloc_eh_object (type)
     tree type;
{
  tree fn, exp;

  fn = get_identifier ("__eh_alloc");
  if (IDENTIFIER_GLOBAL_VALUE (fn))
    fn = IDENTIFIER_GLOBAL_VALUE (fn);
  else
    {
      /* Declare __eh_alloc (size_t), as defined in exception.cc.  */
      tree tmp;
      push_obstacks_nochange ();
      end_temporary_allocation ();
      tmp = tree_cons (NULL_TREE, sizetype, void_list_node);
      fn = build_lang_decl (FUNCTION_DECL, fn,
			    build_function_type (ptr_type_node, tmp));
      DECL_EXTERNAL (fn) = 1;
      TREE_PUBLIC (fn) = 1;
      DECL_ARTIFICIAL (fn) = 1;
      pushdecl_top_level (fn);
      make_function_rtl (fn);
      pop_obstacks ();
    }

  mark_used (fn);
  exp = build_function_call (fn, expr_tree_cons
			     (NULL_TREE, size_in_bytes (type), NULL_TREE));
  exp = build1 (NOP_EXPR, build_pointer_type (type), exp);
  return exp;
}

/* Expand a throw statement.  This follows the following
   algorithm:

	1. Allocate space to save the current PC onto the stack.
	2. Generate and emit a label and save its address into the
		newly allocated stack space since we can't save the pc directly.
	3. If this is the first call to throw in this function:
		generate a label for the throw block
	4. jump to the throw block label.  */

void
expand_throw (exp)
     tree exp;
{
  tree fn;
  static tree cleanup_type;

  if (! doing_eh (1))
    return;

  if (exp)
    {
      tree throw_type;
      tree cleanup = NULL_TREE, e;

      /* throw expression */
      /* First, decay it.  */
      exp = decay_conversion (exp);

      /* cleanup_type is void (*)(void *, int),
	 the internal type of a destructor. */
      if (cleanup_type == NULL_TREE)
	{
	  push_obstacks_nochange ();
	  end_temporary_allocation ();
	  cleanup_type = build_pointer_type
	    (build_function_type
	     (void_type_node, tree_cons
	      (NULL_TREE, ptr_type_node, tree_cons
	       (NULL_TREE, integer_type_node, void_list_node))));
	  pop_obstacks ();
	}

      if (TYPE_PTR_P (TREE_TYPE (exp)))
	throw_type = build_eh_type (exp);
      else
	{
	  tree object, ptr;

	  /* OK, this is kind of wacky.  The WP says that we call
	     terminate

	     when the exception handling mechanism, after completing
	     evaluation of the expression to be thrown but before the
	     exception is caught (_except.throw_), calls a user function
	     that exits via an uncaught exception.

	     So we have to protect the actual initialization of the
	     exception object with terminate(), but evaluate the expression
	     first.  We also expand the call to __eh_alloc
	     first.  Since there could be temps in the expression, we need
	     to handle that, too.  */

	  expand_start_target_temps ();

#if 0
	  /* Unfortunately, this doesn't work.  */
	  preexpand_calls (exp);
#else
	  /* Store the throw expression into a temp.  This can be less
	     efficient than storing it into the allocated space directly, but
	     oh well.  To do this efficiently we would need to insinuate
	     ourselves into expand_call.  */
	  if (TREE_SIDE_EFFECTS (exp))
	    {
	      tree temp = build_decl (VAR_DECL, NULL_TREE, TREE_TYPE (exp));
	      DECL_ARTIFICIAL (temp) = 1;
	      DECL_RTL (temp) = assign_temp (TREE_TYPE (exp), 2, 0, 1);
	      DECL_INITIAL (temp) = exp;
	      cp_finish_decl (temp, exp, NULL_TREE, 0, LOOKUP_ONLYCONVERTING);
	      exp = temp;
	    }
#endif

	  /* Allocate the space for the exception.  */
	  ptr = save_expr (alloc_eh_object (TREE_TYPE (exp)));
	  expand_expr (ptr, const0_rtx, VOIDmode, 0);

	  expand_eh_region_start ();

	  object = build_indirect_ref (ptr, NULL_PTR);
	  exp = build_modify_expr (object, INIT_EXPR, exp);

	  if (exp == error_mark_node)
	    error ("  in thrown expression");

	  expand_expr (exp, const0_rtx, VOIDmode, 0);
	  expand_eh_region_end (build_terminate_handler ());
	  expand_end_target_temps ();

	  throw_type = build_eh_type (object);

	  if (TYPE_HAS_DESTRUCTOR (TREE_TYPE (object)))
	    {
	      cleanup = lookup_fnfields (TYPE_BINFO (TREE_TYPE (object)),
					 dtor_identifier, 0);
	      cleanup = TREE_VALUE (cleanup);
	      mark_used (cleanup);
	      mark_addressable (cleanup);
	      /* Pretend it's a normal function.  */
	      cleanup = build1 (ADDR_EXPR, cleanup_type, cleanup);
	    }

	  exp = ptr;
	}

      /* Cast EXP to `void *' so that it will match the prototype for
	 __cp_push_exception.  */
      exp = convert (ptr_type_node, exp);

      if (cleanup == NULL_TREE)
	{
	  cleanup = build_int_2 (0, 0);
	  TREE_TYPE (cleanup) = cleanup_type;
	}

      fn = get_identifier ("__cp_push_exception");
      if (IDENTIFIER_GLOBAL_VALUE (fn))
	fn = IDENTIFIER_GLOBAL_VALUE (fn);
      else
	{
	  /* Declare __cp_push_exception (void*, void*, void (*)(void*, int)),
	     as defined in exception.cc.  */
	  tree tmp;
	  push_obstacks_nochange ();
	  end_temporary_allocation ();
	  tmp = tree_cons
	    (NULL_TREE, ptr_type_node, tree_cons
	     (NULL_TREE, ptr_type_node, tree_cons
	      (NULL_TREE, cleanup_type, void_list_node)));
	  fn = build_lang_decl (FUNCTION_DECL, fn,
				build_function_type (void_type_node, tmp));
	  DECL_EXTERNAL (fn) = 1;
	  TREE_PUBLIC (fn) = 1;
	  DECL_ARTIFICIAL (fn) = 1;
	  pushdecl_top_level (fn);
	  make_function_rtl (fn);
	  pop_obstacks ();
	}

      mark_used (fn);
      e = expr_tree_cons (NULL_TREE, exp, expr_tree_cons
			  (NULL_TREE, throw_type, expr_tree_cons
			   (NULL_TREE, cleanup, NULL_TREE)));
      e = build_function_call (fn, e);
      expand_expr (e, const0_rtx, VOIDmode, 0);
    }
  else
    {
      /* rethrow current exception; note that it's no longer caught.  */

      tree fn = get_identifier ("__uncatch_exception");
      if (IDENTIFIER_GLOBAL_VALUE (fn))
	fn = IDENTIFIER_GLOBAL_VALUE (fn);
      else
	{
	  /* Declare void __uncatch_exception (void)
	     as defined in exception.cc. */
	  push_obstacks_nochange ();
	  end_temporary_allocation ();
	  fn = build_lang_decl (FUNCTION_DECL, fn,
				build_function_type (void_type_node,
						     void_list_node));
	  DECL_EXTERNAL (fn) = 1;
	  TREE_PUBLIC (fn) = 1;
	  DECL_ARTIFICIAL (fn) = 1;
	  pushdecl_top_level (fn);
	  make_function_rtl (fn);
	  pop_obstacks ();
	}

      mark_used (fn);
      exp = build_function_call (fn, NULL_TREE);
      expand_expr (exp, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  expand_internal_throw ();
}

/* Build a throw expression.  */

tree
build_throw (e)
     tree e;
{
  if (e == error_mark_node)
    return e;

  if (processing_template_decl)
    return build_min (THROW_EXPR, void_type_node, e);

  if (e == null_node)
    cp_warning ("throwing NULL, which has integral, not pointer type");

  e = build1 (THROW_EXPR, void_type_node, e);
  TREE_SIDE_EFFECTS (e) = 1;
  TREE_USED (e) = 1;

  return e;
}
