/* Handle exceptional things in C++.
   Copyright (C) 1989, 92-95, 1996 Free Software Foundation, Inc.
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
#include "tree.h"
#include "rtl.h"
#include "cp-tree.h"
#include "flags.h"
#include "obstack.h"
#include "expr.h"
#include "output.h"
#include "except.h"
#include "function.h"

rtx expand_builtin_return_addr	PROTO((enum built_in_function, int, rtx));

/* Holds the fndecl for __builtin_return_address.  */
tree builtin_return_address_fndecl;

/* A couple of backend routines from m88k.c */

/* Used to cache a call to __builtin_return_address.  */
static tree BuiltinReturnAddress;
     

#include <stdio.h>

static void
easy_expand_asm (str)
     char *str;
{
  expand_asm (build_string (strlen (str)+1, str));
}


#if 0
/* This is the startup, and finish stuff per exception table.  */

/* XXX - Tad: exception handling section */
#ifndef EXCEPT_SECTION_ASM_OP
#define EXCEPT_SECTION_ASM_OP	"section\t.gcc_except_table,\"a\",@progbits"
#endif

#ifdef EXCEPT_SECTION_ASM_OP
typedef struct {
    void *start_region;
    void *end_region;
    void *exception_handler;
 } exception_table;
#endif /* EXCEPT_SECTION_ASM_OP */

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
     is the the end of a try block.  Next, this modified eh_entry node
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

extern rtx emit_insn		PROTO((rtx));
extern rtx gen_nop		PROTO(());

/* local globals for function calls
   ====================================================================== */

/* Used to cache "terminate", "unexpected", "set_terminate", and
   "set_unexpected" after default_conversion. (lib-except.c)  */
static tree Terminate, Unexpected, SetTerminate, SetUnexpected, CatchMatch;

/* Used to cache __find_first_exception_table_match for throw.  */
static tree FirstExceptionMatch;

/* Used to cache a call to __unwind_function.  */
static tree Unwind;

/* Holds a ready to emit call to "terminate".  */
static tree TerminateFunctionCall;

static tree empty_fndecl;

/* ====================================================================== */


/* ========================================================================= */



/* local globals - these local globals are for storing data necessary for
   generating the exception table and code in the correct order.

   ========================================================================= */

/* Holds the pc for doing "throw" */
static tree saved_pc;
/* Holds the type of the thing being thrown.  */
static tree saved_throw_type;
/* Holds the value being thrown.  */
static tree saved_throw_value;
/* Holds the cleanup for the value being thrown.  */
static tree saved_cleanup;
/* Indicates if we are in a catch clause.  */
static tree saved_in_catch;

extern int throw_used;
extern rtx catch_clauses;

/* ========================================================================= */

/* Cheesyness to save some typing.  Returns the return value rtx.  */

static rtx
do_function_call (func, params, return_type)
     tree func, params, return_type;
{
  tree func_call;
  func_call = build_function_call (func, params);
  expand_call (func_call, NULL_RTX, 0);
  if (return_type != NULL_TREE)
    return hard_function_value (return_type, func_call);
  return NULL_RTX;
}

/* ========================================================================= */

/* sets up all the global eh stuff that needs to be initialized at the
   start of compilation.

   This includes:
		- Setting up all the function call trees.  */

void
init_exception_processing ()
{
  extern tree define_function ();
  tree unexpected_fndecl, terminate_fndecl;
  tree set_unexpected_fndecl, set_terminate_fndecl;
  tree catch_match_fndecl;
  tree find_first_exception_match_fndecl;
  tree unwind_fndecl;
  tree declspecs;
  tree d;

  /* void (*)() */
  tree PFV = build_pointer_type (build_function_type
				 (void_type_node, void_list_node));

  /* Arg list for the build_function_type call for set_terminate and
     set_unexpected.  */
  tree pfvlist = tree_cons (NULL_TREE, PFV, void_list_node);

  /* void (*pfvtype (void (*) ()))() */
  tree pfvtype = build_function_type (PFV, pfvlist);

  /* void vtype () */
  tree vtype = build_function_type (void_type_node, void_list_node);
  
  set_terminate_fndecl = auto_function (get_identifier ("set_terminate"),
					pfvtype, NOT_BUILT_IN);
  set_unexpected_fndecl = auto_function (get_identifier ("set_unexpected"),
					 pfvtype, NOT_BUILT_IN);
  unexpected_fndecl = auto_function (get_identifier ("unexpected"),
				     vtype, NOT_BUILT_IN);
  terminate_fndecl = auto_function (get_identifier ("terminate"),
				    vtype, NOT_BUILT_IN);

  push_lang_context (lang_name_c);

  catch_match_fndecl
    = builtin_function (flag_rtti
			? "__throw_type_match_rtti"
			: "__throw_type_match",
			build_function_type (ptr_type_node,
					     tree_cons (NULL_TREE, ptr_type_node,
							tree_cons (NULL_TREE, ptr_type_node,
								   tree_cons (NULL_TREE, ptr_type_node,
									      void_list_node)))),
			NOT_BUILT_IN, NULL_PTR);
  find_first_exception_match_fndecl
    = builtin_function ("__find_first_exception_table_match",
			build_function_type (ptr_type_node,
					     tree_cons (NULL_TREE, ptr_type_node,
							void_list_node)),
			NOT_BUILT_IN, NULL_PTR);
  unwind_fndecl
    = builtin_function ("__unwind_function",
			build_function_type (void_type_node,
					     tree_cons (NULL_TREE, ptr_type_node,
							void_list_node)),
			NOT_BUILT_IN, NULL_PTR);
  empty_fndecl
    = builtin_function ("__empty",
			build_function_type (void_type_node, void_list_node),
			NOT_BUILT_IN, NULL_PTR);
  DECL_EXTERNAL (empty_fndecl) = 1;
  TREE_PUBLIC (empty_fndecl) = 1;

  Unexpected = default_conversion (unexpected_fndecl);
  Terminate = default_conversion (terminate_fndecl);
  SetTerminate = default_conversion (set_terminate_fndecl);
  SetUnexpected = default_conversion (set_unexpected_fndecl);
  CatchMatch = default_conversion (catch_match_fndecl);
  FirstExceptionMatch = default_conversion (find_first_exception_match_fndecl);
  Unwind = default_conversion (unwind_fndecl);
  BuiltinReturnAddress = default_conversion (builtin_return_address_fndecl);

  TerminateFunctionCall = build_function_call (Terminate, NULL_TREE);

  pop_lang_context ();

  declspecs = tree_cons (NULL_TREE, get_identifier ("void"), NULL_TREE);
  d = build_parse_node (INDIRECT_REF, get_identifier ("__eh_pc"));
  d = start_decl (d, declspecs, 0);
  DECL_COMMON (d) = 1;
  cp_finish_decl (d, NULL_TREE, NULL_TREE, 1, 0);
  saved_pc = lookup_name (get_identifier ("__eh_pc"), 0);

  declspecs = tree_cons (NULL_TREE, get_identifier ("void"), NULL_TREE);
  d = build_parse_node (INDIRECT_REF, get_identifier ("__eh_type"));
  d = start_decl (d, declspecs, 0);
  DECL_COMMON (d) = 1;
  cp_finish_decl (d, NULL_TREE, NULL_TREE, 1, 0);
  saved_throw_type = lookup_name (get_identifier ("__eh_type"), 0);

  declspecs = tree_cons (NULL_TREE, get_identifier ("void"), NULL_TREE);
  d = build_parse_node (INDIRECT_REF, get_identifier ("__eh_value"));
  d = start_decl (d, declspecs, 0);
  DECL_COMMON (d) = 1;
  cp_finish_decl (d, NULL_TREE, NULL_TREE, 1, 0);
  saved_throw_value = lookup_name (get_identifier ("__eh_value"), 0);

  declspecs = tree_cons (NULL_TREE, get_identifier ("void"), NULL_TREE);
  d = build_parse_node (INDIRECT_REF, get_identifier ("__eh_cleanup"));
  d = make_call_declarator (d, void_list_node, NULL_TREE, NULL_TREE);
  d = start_decl (d, declspecs, 0);
  DECL_COMMON (d) = 1;
  cp_finish_decl (d, NULL_TREE, NULL_TREE, 1, 0);
  saved_cleanup = lookup_name (get_identifier ("__eh_cleanup"), 0);

  declspecs = tree_cons (NULL_TREE, get_identifier ("bool"), NULL_TREE);
  d = get_identifier ("__eh_in_catch");
  d = start_decl (d, declspecs, 0);
  DECL_COMMON (d) = 1;
  cp_finish_decl (d, NULL_TREE, NULL_TREE, 1, 0);
  saved_in_catch = lookup_name (get_identifier ("__eh_in_catch"), 0);

  /* If we use setjmp/longjmp EH, arrange for all cleanup actions to
     be protected with __terminate.  */
  protect_cleanup_actions_with_terminate = 1;
}

/* Build a type value for use at runtime for a type that is matched
   against by the exception handling system.  */

static tree
build_eh_type_type (type)
     tree type;
{
  char *typestring;
  tree exp;

  if (type == error_mark_node)
    return error_mark_node;

  /* peel back references, so they match.  */
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  /* Peel off cv qualifiers.  */
  type = TYPE_MAIN_VARIANT (type);

  if (flag_rtti)
    {
      return build1 (ADDR_EXPR, ptr_type_node, get_typeid (type));
    }

  typestring = build_overload_name (type, 1, 1);
  exp = combine_strings (build_string (strlen (typestring)+1, typestring));
  return build1 (ADDR_EXPR, ptr_type_node, exp);
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

/* This routine creates the cleanup for the exception handling object.  */

static void
push_eh_cleanup ()
{
  /* All cleanups must last longer than normal.  */
  int yes = suspend_momentary ();

  /* Arrange to do a dynamically scoped cleanup upon exit from this region.  */
  tree cleanup = build_function_call (saved_cleanup, NULL_TREE);
  cleanup = build (COMPOUND_EXPR, void_type_node, cleanup,
		   build_modify_expr (saved_in_catch, NOP_EXPR,
				      build_modify_expr (saved_throw_type, NOP_EXPR, integer_zero_node)));
  expand_decl_cleanup (NULL_TREE, cleanup);

  resume_momentary (yes);
}


/* call this to start a catch block. Typename is the typename, and identifier
   is the variable to place the object in or NULL if the variable doesn't
   matter.  If typename is NULL, that means its a "catch (...)" or catch
   everything.  In that case we don't need to do any type checking.
   (ie: it ends up as the "else" clause rather than an "else if" clause) */

void
expand_start_catch_block (declspecs, declarator)
     tree declspecs, declarator;
{
  rtx false_label_rtx;
  tree decl = NULL_TREE;
  tree init;

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

  /* Create a binding level for the parm.  */
  pushlevel (0);
  expand_start_bindings (0);

  false_label_rtx = gen_label_rtx ();
  push_label_entry (&false_label_stack, false_label_rtx, NULL_TREE);

  emit_line_note (input_filename, lineno);

  if (declspecs)
    {
      tree exp;
      rtx call_rtx, return_value_rtx;
      tree init_type;

      decl = grokdeclarator (declarator, declspecs, CATCHPARM, 1, NULL_TREE);

      if (decl == NULL_TREE)
	{
	  error ("invalid catch parameter");

	  /* This is cheap, but we want to maintain the data
             structures.  */

	  expand_eh_region_start ();

	  return;
	}

      /* Make sure we mark the catch param as used, otherwise we'll get
	 a warning about an unused ((anonymous)).  */
      TREE_USED (decl) = 1;

      /* Figure out the type that the initializer is.  */
      init_type = TREE_TYPE (decl);
      if (TREE_CODE (init_type) != REFERENCE_TYPE
	  && TREE_CODE (init_type) != POINTER_TYPE)
	init_type = build_reference_type (init_type);

      exp = saved_throw_value;
      exp = tree_cons (NULL_TREE,
		       build_eh_type_type (TREE_TYPE (decl)),
		       tree_cons (NULL_TREE,
				  saved_throw_type,
				  tree_cons (NULL_TREE, exp, NULL_TREE)));
      exp = build_function_call (CatchMatch, exp);
      call_rtx = expand_call (exp, NULL_RTX, 0);
      assemble_external (TREE_OPERAND (CatchMatch, 0));

      return_value_rtx = hard_function_value (ptr_type_node, exp);

      /* did the throw type match function return TRUE? */
      emit_cmp_insn (return_value_rtx, const0_rtx, EQ, NULL_RTX,
		    GET_MODE (return_value_rtx), 0, 0);

      /* if it returned FALSE, jump over the catch block, else fall into it */
      emit_jump_insn (gen_beq (false_label_rtx));

      push_eh_cleanup ();

      init = convert_from_reference (save_expr (make_tree (init_type, call_rtx)));

      /* Do we need the below two lines? */
      /* Let `cp_finish_decl' know that this initializer is ok.  */
      DECL_INITIAL (decl) = init;
      decl = pushdecl (decl);
      cp_finish_decl (decl, init, NULL_TREE, 0, LOOKUP_ONLYCONVERTING);
    }
  else
    {
      push_eh_cleanup ();

      /* Fall into the catch all section.  */
    }

  emit_move_insn (DECL_RTL (saved_in_catch), const1_rtx);

  /* If we are not doing setjmp/longjmp EH, because we are reordered
     out of line, we arrange to rethrow in the outer context so as to
     skip through the terminate region we are nested in, should we
     encounter an exception in the catch handler.

     If we are doing setjmp/longjmp EH, we need to skip through the EH
     object cleanup region.  This isn't quite right, as we really need
     to clean the object up, but we cannot do that until we track
     multiple EH objects.

     Matches the end in expand_end_catch_block.  */
  expand_eh_region_start ();

  emit_line_note (input_filename, lineno);
}



/* Call this to end a catch block.  Its responsible for emitting the
   code to handle jumping back to the correct place, and for emitting
   the label to jump to if this catch block didn't match.  */

void
expand_end_catch_block ()
{
  rtx start_region_label_rtx;
  rtx end_region_label_rtx;
  tree decls, t;

  if (! doing_eh (1))
    return;

  t = make_node (RTL_EXPR);
  TREE_TYPE (t) = void_type_node;
  RTL_EXPR_RTL (t) = const0_rtx;
  TREE_SIDE_EFFECTS (t) = 1;
  start_sequence_for_rtl_expr (t);

  if (exceptions_via_longjmp)
    {
      /* If we are doing setjmp/longjmp EH, we need to skip through
	 the EH object cleanup region.  This isn't quite right, as we
	 really need to clean the object up, but we cannot do that
	 until we track multiple EH objects.  */

      emit_library_call (sjpopnthrow_libfunc, 0, VOIDmode, 0);
      emit_barrier ();
    }
  else
    {
      /* If we are not doing setjmp/longjmp EH, we need an extra
	 region around the whole catch block to skip through the
	 terminate region we are nested in.  */

      expand_internal_throw (DECL_RTL (top_label_entry (&caught_return_label_stack)));
    }

  RTL_EXPR_SEQUENCE (t) = get_insns ();
  end_sequence ();

  /* Matches the start in expand_start_catch_block.  */
  expand_eh_region_end (t);

  /* Fall to outside the try statement when done executing handler and
     we fall off end of handler.  This is jump Lresume in the
     documentation.  */
  expand_goto (top_label_entry (&caught_return_label_stack));

  expand_leftover_cleanups ();

  /* Cleanup the EH parameter.  */
  expand_end_bindings (getdecls (), kept_level_p (), 0);
  poplevel (kept_level_p (), 1, 0);
      
  /* label we emit to jump to if this catch block didn't match.  */
  /* This the closing } in the `if (eq) {' of the documentation.  */
  emit_label (pop_label_entry (&false_label_stack));
}

/* unwind the stack.  */

static void
do_unwind (inner_throw_label)
     rtx inner_throw_label;
{
#if defined (SPARC_STACK_ALIGN) /* was sparc */
  /* This doesn't work for the flat model sparc, I bet.  */
  tree fcall;
  tree params;
  rtx return_val_rtx;
  rtx temp;

  /* Call to  __builtin_return_address. */
  params = tree_cons (NULL_TREE, integer_zero_node, NULL_TREE);
  fcall = build_function_call (BuiltinReturnAddress, params);
  return_val_rtx = expand_expr (fcall, NULL_RTX, Pmode, 0);
  /* In the return, the new pc is pc+8, as the value coming in is
     really the address of the call insn, not the next insn.  */
  temp = gen_reg_rtx (Pmode);
  emit_move_insn (temp, inner_throw_label);
  emit_move_insn (return_val_rtx, plus_constant (temp, -8));
  emit_insn (gen_rtx (USE, VOIDmode, gen_rtx (REG, SImode, 31)));
  easy_expand_asm ("ret");
  easy_expand_asm ("restore");
  emit_barrier ();
#endif
#if defined (ARM_FRAME_RTX)  /* was __arm */
  if (flag_omit_frame_pointer)
    sorry ("this implementation of exception handling requires a frame pointer");

  emit_move_insn (stack_pointer_rtx,
		  gen_rtx (MEM, Pmode, plus_constant (hard_frame_pointer_rtx, -8)));
  emit_move_insn (hard_frame_pointer_rtx,
		  gen_rtx (MEM, Pmode, plus_constant (hard_frame_pointer_rtx, -12)));
#endif
#if defined (TARGET_88000) /* was m88k */
  rtx temp_frame = frame_pointer_rtx;

  temp_frame = memory_address (Pmode, temp_frame);
  temp_frame = copy_to_reg (gen_rtx (MEM, Pmode, temp_frame));

  /* hopefully this will successfully pop the frame! */
  emit_move_insn (frame_pointer_rtx, temp_frame);
  emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
  emit_move_insn (arg_pointer_rtx, frame_pointer_rtx);
  emit_insn (gen_add2_insn (stack_pointer_rtx, gen_rtx (CONST_INT, VOIDmode,
						     (HOST_WIDE_INT)m88k_debugger_offset (stack_pointer_rtx, 0))));

#if 0
  emit_insn (gen_add2_insn (arg_pointer_rtx, gen_rtx (CONST_INT, VOIDmode,
						   -(HOST_WIDE_INT)m88k_debugger_offset (arg_pointer_rtx, 0))));

  emit_move_insn (stack_pointer_rtx, arg_pointer_rtx);

  emit_insn (gen_add2_insn (stack_pointer_rtx, gen_rtx (CONST_INT, VOIDmode,
						     (HOST_WIDE_INT)m88k_debugger_offset (arg_pointer_rtx, 0))));
#endif
#endif
#if ! defined (TARGET_88000) && ! defined (ARM_FRAME_RTX) && ! defined (SPARC_STACK_ALIGN)
  tree fcall;
  tree params;
  rtx return_val_rtx;

#if 0
  /* I would like to do this here, but the move below doesn't seem to work.  */
  /* Call to  __builtin_return_address.  */
  params = tree_cons (NULL_TREE, integer_zero_node, NULL_TREE);
  fcall = build_function_call (BuiltinReturnAddress, params);
  return_val_rtx = expand_expr (fcall, NULL_RTX, Pmode, 0);

  emit_move_insn (return_val_rtx, inner_throw_label);
  /* So, for now, just pass throw label to stack unwinder.  */
#endif
  params = tree_cons (NULL_TREE, make_tree (ptr_type_node,
					    inner_throw_label), NULL_TREE);
  
  do_function_call (Unwind, params, NULL_TREE);
  assemble_external (TREE_OPERAND (Unwind, 0));
  emit_barrier ();
#endif
}


/* Is called from expand_exception_blocks to generate the code in a function
   to "throw" if anything in the function needs to perform a throw.

   expands "throw" as the following pseudo code:

	throw:
		eh = find_first_exception_match (saved_pc);
	    if (!eh) goto gotta_rethrow_it;
		goto eh;

	gotta_rethrow_it:
		saved_pc = __builtin_return_address (0);
		pop_to_previous_level ();
		goto throw;  */

void
expand_builtin_throw ()
{
  tree fcall;
  tree params;
  rtx return_val_rtx;
  rtx gotta_rethrow_it;
  rtx gotta_call_terminate;
  rtx top_of_loop;
  rtx unwind_first;
  tree t;

  if (! doing_eh (0))
    return;

  if (! throw_used)
    return;

  params = void_list_node;
  t = make_call_declarator (get_identifier ("__throw"), params, NULL_TREE,
			    NULL_TREE);
  start_function (decl_tree_cons (NULL_TREE, get_identifier ("static"),
				  void_list_node),
		  t, NULL_TREE, 0);
  store_parm_decls ();
  pushlevel (0);
  clear_last_expr ();
  push_momentary ();
  expand_start_bindings (0);

  gotta_rethrow_it = gen_label_rtx ();
  gotta_call_terminate = gen_label_rtx ();
  top_of_loop = gen_label_rtx ();
  unwind_first = gen_label_rtx ();

  /* These two can be frontend specific.  If wanted, they can go in
     expand_throw.  */
  /* Do we have a valid object we are throwing? */
  emit_cmp_insn (DECL_RTL (saved_throw_type), const0_rtx, EQ, NULL_RTX,
		 GET_MODE (DECL_RTL (saved_throw_type)), 0, 0);
  emit_jump_insn (gen_beq (gotta_call_terminate));

  emit_jump (unwind_first);

  emit_label (top_of_loop);

  /* search for an exception handler for the saved_pc */
  return_val_rtx = do_function_call (FirstExceptionMatch,
				     tree_cons (NULL_TREE, saved_pc, NULL_TREE),
				     ptr_type_node);
  assemble_external (TREE_OPERAND (FirstExceptionMatch, 0));

  /* did we find one? */
  emit_cmp_insn (return_val_rtx, const0_rtx, EQ, NULL_RTX,
		 GET_MODE (return_val_rtx), 0, 0);

  /* if not, jump to gotta_rethrow_it */
  emit_jump_insn (gen_beq (gotta_rethrow_it));

  /* we found it, so jump to it */
  emit_indirect_jump (return_val_rtx);

  /* code to deal with unwinding and looking for it again */
  emit_label (gotta_rethrow_it);

  /* Call to  __builtin_return_address.  */
#if defined (ARM_FRAME_RTX)  /* was __arm */
  /* This should be moved into arm.h:RETURN_ADDR_RTX */
  /* This replaces a 'call' to __builtin_return_address */
  return_val_rtx = gen_reg_rtx (Pmode);
  emit_move_insn (return_val_rtx, gen_rtx (MEM, Pmode, plus_constant (hard_frame_pointer_rtx, -4)));
#else
  params = tree_cons (NULL_TREE, integer_zero_node, NULL_TREE);
  fcall = build_function_call (BuiltinReturnAddress, params);
  return_val_rtx = expand_expr (fcall, NULL_RTX, Pmode, 0);
#endif

  /* Did __builtin_return_address return a valid address?  */
  emit_cmp_insn (return_val_rtx, const0_rtx, EQ, NULL_RTX,
		 GET_MODE (return_val_rtx), 0, 0);

  emit_jump_insn (gen_beq (gotta_call_terminate));

  return_val_rtx = eh_outer_context (return_val_rtx);

  /* Yes it did.  */
  emit_move_insn (eh_saved_pc_rtx, return_val_rtx);

  do_unwind (gen_rtx (LABEL_REF, Pmode, top_of_loop));
  emit_jump (top_of_loop);

  /* no it didn't --> therefore we need to call terminate */
  emit_label (gotta_call_terminate);
  do_function_call (Terminate, NULL_TREE, NULL_TREE);
  assemble_external (TREE_OPERAND (Terminate, 0));

  {
    rtx ret_val, return_val_rtx;
    emit_label (unwind_first);
    ret_val = expand_builtin_return_addr (BUILT_IN_RETURN_ADDRESS,
					  0, hard_frame_pointer_rtx);

    /* Set it up so that we continue inside, at the top of the loop.  */
    emit_move_insn (ret_val, gen_rtx (LABEL_REF, Pmode, top_of_loop));
#ifdef RETURN_ADDR_OFFSET
  return_val_rtx = plus_constant (ret_val, -RETURN_ADDR_OFFSET);
    if (return_val_rtx != ret_val)
      emit_move_insn (ret_val, return_val_rtx);
#endif

    /* Fall into epilogue to unwind prologue.  */
  }

  expand_end_bindings (getdecls (), 1, 0);
  poplevel (1, 0, 0);
  pop_momentary ();

  finish_function (lineno, 0, 0);
}


void
expand_start_eh_spec ()
{
  expand_eh_region_start ();
}

static void
expand_end_eh_spec (raises)
     tree raises;
{
  tree expr, second_try;
  rtx check = gen_label_rtx ();
  rtx cont;
  rtx ret = gen_reg_rtx (Pmode);
  rtx flag = gen_reg_rtx (TYPE_MODE (integer_type_node));
  rtx end = gen_label_rtx ();

  expr = make_node (RTL_EXPR);
  TREE_TYPE (expr) = void_type_node;
  RTL_EXPR_RTL (expr) = const0_rtx;
  TREE_SIDE_EFFECTS (expr) = 1;
  start_sequence_for_rtl_expr (expr);
  cont = gen_label_rtx ();
  emit_move_insn (ret, gen_rtx (LABEL_REF, Pmode, cont));
  emit_jump (check);
  emit_label (cont);
  jumpif (make_tree (integer_type_node, flag), end);
  do_function_call (Terminate, NULL_TREE, NULL_TREE);
  assemble_external (TREE_OPERAND (Terminate, 0));
  emit_barrier ();
  RTL_EXPR_SEQUENCE (expr) = get_insns ();
  end_sequence ();
  
  second_try = expr;

  expr = make_node (RTL_EXPR);
  TREE_TYPE (expr) = void_type_node;
  RTL_EXPR_RTL (expr) = const0_rtx;
  TREE_SIDE_EFFECTS (expr) = 1;
  start_sequence_for_rtl_expr (expr);

  cont = gen_label_rtx ();
  emit_move_insn (ret, gen_rtx (LABEL_REF, Pmode, cont));
  emit_jump (check);
  emit_label (cont);
  jumpif (make_tree (integer_type_node, flag), end);
  expand_eh_region_start ();
  do_function_call (Unexpected, NULL_TREE, NULL_TREE);
  assemble_external (TREE_OPERAND (Unexpected, 0));
  emit_barrier ();

  expand_eh_region_end (second_try);
  
  emit_label (check);
  emit_move_insn (flag, const1_rtx);
  cont = gen_label_rtx ();
  while (raises)
    {
      tree exp;
      tree match_type = TREE_VALUE (raises);
      
      if (match_type)
	{
	  /* check TREE_VALUE (raises) here */
	  exp = saved_throw_value;
	  exp = tree_cons (NULL_TREE,
			   build_eh_type_type (match_type),
			   tree_cons (NULL_TREE,
				      saved_throw_type,
				      tree_cons (NULL_TREE, exp, NULL_TREE)));
	  exp = build_function_call (CatchMatch, exp);
	  assemble_external (TREE_OPERAND (CatchMatch, 0));

	  jumpif (exp, cont);
	}

      raises = TREE_CHAIN (raises);
    }
  emit_move_insn (flag, const0_rtx);
  emit_label (cont);
  emit_indirect_jump (ret);
  emit_label (end);
  
  RTL_EXPR_SEQUENCE (expr) = get_insns ();
  end_sequence ();
  
  expand_eh_region_end (expr);
}

/* This is called to expand all the toplevel exception handling
   finalization for a function.  It should only be called once per
   function.  */

void
expand_exception_blocks ()
{
  rtx funcend;
  rtx insns;

  start_sequence ();

  funcend = gen_label_rtx ();

  start_sequence ();

  /* Add all the catch clauses here.  */
  emit_insns (catch_clauses);
  catch_clauses = NULL_RTX;

  expand_leftover_cleanups ();

  insns = get_insns ();
  end_sequence ();

#if 1
  /* Do this after we expand leftover cleanups, so that the
     expand_eh_region_end that expand_end_eh_spec does will match the
     right expand_eh_region_start, and make sure it comes out before
     the terminate protected region.  */
  if (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (current_function_decl)))
    {
     expand_end_eh_spec (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (current_function_decl)));
     push_to_sequence (insns);

     /* Now expand any new ones.  */
     expand_leftover_cleanups ();

     insns = get_insns ();
     end_sequence ();
    }
#endif

  emit_jump (funcend);

  if (insns)
    {
      /* We cannot protect n regions this way if we must flow into the
	 EH region through the top of the region, as we have to with
	 the setjmp/longjmp approach.  */
      if (exceptions_via_longjmp == 0)
	{
	  /* Is this necessary?  */
	  assemble_external (TREE_OPERAND (Terminate, 0));

	  expand_eh_region_start ();
	}

      emit_insns (insns);

      if (exceptions_via_longjmp == 0)
	expand_eh_region_end (TerminateFunctionCall);

      expand_leftover_cleanups ();
    }

  emit_label (funcend);

  /* Only if we had previous insns do we want to emit the jump around
     them.  If there weren't any, then insns will remain NULL_RTX.  */
  if (insns)
    insns = get_insns ();
  end_sequence ();

  emit_insns (insns);
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
  /* tcf stands for throw clean funciton.  */
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
  rtx label;

  if (! doing_eh (1))
    return;

  if (exp)
    {
      tree throw_type;
      tree cleanup = empty_fndecl, e;

      /* throw expression */
      /* First, decay it.  */
      exp = decay_conversion (exp);

      if (TREE_CODE (TREE_TYPE (exp)) == POINTER_TYPE)
	{
	  throw_type = build_eh_type (exp);
	  exp = build_reinterpret_cast (ptr_type_node, exp);
	}
      else
	{
	  tree object;

	  /* Make a copy of the thrown object.  WP 15.1.5  */
	  exp = build_new (NULL_TREE, TREE_TYPE (exp),
			   build_tree_list (NULL_TREE, exp),
			   0);

	  if (exp == error_mark_node)
	    error ("  in thrown expression");

	  object = build_indirect_ref (exp, NULL_PTR);
	  throw_type = build_eh_type (object);

	  start_sequence ();
	  object = build_reinterpret_cast (TREE_TYPE (exp), saved_throw_value);
	  object = build_indirect_ref (object, NULL_PTR);
	  cleanup = maybe_build_cleanup_and_delete (object);
	  end_sequence ();

	  if (cleanup)
	    {
	      cleanup = start_anon_func ();

	      expand_expr (maybe_build_cleanup_and_delete (object),
			   const0_rtx, VOIDmode, 0);

	      end_anon_func ();

	      mark_addressable (cleanup);
	    }
	  else
	    {
	      cleanup = empty_fndecl;
	    }
	}

      if (cleanup == empty_fndecl)
	assemble_external (empty_fndecl);
	
      e = build_modify_expr (saved_throw_type, NOP_EXPR, throw_type);
      expand_expr (e, const0_rtx, VOIDmode, 0);

      e = build_modify_expr (saved_throw_value, NOP_EXPR, exp);
      e = build1 (CLEANUP_POINT_EXPR, TREE_TYPE (e), e);
      expand_expr (e, const0_rtx, VOIDmode, 0);

      cleanup = build_unary_op (ADDR_EXPR, cleanup, 0);
      cleanup = build_modify_expr (saved_cleanup, NOP_EXPR, cleanup);
      expand_expr (cleanup, const0_rtx, VOIDmode, 0);
    }
  else
    {
      /* rethrow current exception */
      /* This part is easy, as we don't have to do anything else.  */
    }

  if (exceptions_via_longjmp)
    emit_throw ();
  else
    {
      /* This is the label that represents where in the code we were, when
	 we got an exception.  This needs to be updated when we rethrow an
	 exception, so that the matching routine knows to search out.  */
      label = gen_label_rtx ();
      emit_label (label);

      expand_internal_throw (label);
    }
}

/* Build a throw expression.  */

tree
build_throw (e)
     tree e;
{
  if (e != error_mark_node)
    {
      if (processing_template_decl)
	return build_min (THROW_EXPR, void_type_node, e);
      e = build1 (THROW_EXPR, void_type_node, e);
      TREE_SIDE_EFFECTS (e) = 1;
      TREE_USED (e) = 1;
    }
  return e;
}
