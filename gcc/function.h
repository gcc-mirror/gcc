/* Structure for saving state for a nested function.
   Copyright (C) 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_FUNCTION_H
#define GCC_FUNCTION_H

struct var_refs_queue GTY(())
{
  rtx modified;
  enum machine_mode promoted_mode;
  int unsignedp;
  struct var_refs_queue *next;
};

/* Stack of pending (incomplete) sequences saved by `start_sequence'.
   Each element describes one pending sequence.
   The main insn-chain is saved in the last element of the chain,
   unless the chain is empty.  */

struct sequence_stack GTY(())
{
  /* First and last insns in the chain of the saved sequence.  */
  rtx first;
  rtx last;
  tree sequence_rtl_expr;
  struct sequence_stack *next;
};

extern struct sequence_stack *sequence_stack;

/* Stack of single obstacks.  */

struct simple_obstack_stack
{
  struct obstack *obstack;
  struct simple_obstack_stack *next;
};

struct emit_status GTY(())
{
  /* This is reset to LAST_VIRTUAL_REGISTER + 1 at the start of each function.
     After rtl generation, it is 1 plus the largest register number used.  */
  int x_reg_rtx_no;

  /* Lowest label number in current function.  */
  int x_first_label_num;

  /* The ends of the doubly-linked chain of rtl for the current function.
     Both are reset to null at the start of rtl generation for the function.

     start_sequence saves both of these on `sequence_stack' along with
     `sequence_rtl_expr' and then starts a new, nested sequence of insns.  */
  rtx x_first_insn;
  rtx x_last_insn;

  /* RTL_EXPR within which the current sequence will be placed.  Use to
     prevent reuse of any temporaries within the sequence until after the
     RTL_EXPR is emitted.  */
  tree sequence_rtl_expr;

  /* Stack of pending (incomplete) sequences saved by `start_sequence'.
     Each element describes one pending sequence.
     The main insn-chain is saved in the last element of the chain,
     unless the chain is empty.  */
  struct sequence_stack *sequence_stack;

  /* INSN_UID for next insn emitted.
     Reset to 1 for each function compiled.  */
  int x_cur_insn_uid;

  /* Location the last line-number NOTE emitted.
     This is used to avoid generating duplicates.  */
  location_t x_last_location;

  /* The length of the regno_pointer_align, regno_decl, and x_regno_reg_rtx
     vectors.  Since these vectors are needed during the expansion phase when
     the total number of registers in the function is not yet known, the
     vectors are copied and made bigger when necessary.  */
  int regno_pointer_align_length;

  /* Indexed by pseudo register number, if nonzero gives the known alignment
     for that pseudo (if REG_POINTER is set in x_regno_reg_rtx).
     Allocated in parallel with x_regno_reg_rtx.  */
  unsigned char * GTY ((length ("%h.x_reg_rtx_no")))
    regno_pointer_align;

  /* Indexed by pseudo register number, gives the rtx for that pseudo.
     Allocated in parallel with regno_pointer_align.

     Note MEM expressions can appear in this array due to the actions
     of put_var_into_stack.  */
  rtx * GTY ((length ("%h.x_reg_rtx_no"))) x_regno_reg_rtx;
};

/* For backward compatibility... eventually these should all go away.  */
#define reg_rtx_no (cfun->emit->x_reg_rtx_no)
#define seq_rtl_expr (cfun->emit->sequence_rtl_expr)
#define regno_reg_rtx (cfun->emit->x_regno_reg_rtx)
#define seq_stack (cfun->emit->sequence_stack)

#define REGNO_POINTER_ALIGN(REGNO) (cfun->emit->regno_pointer_align[REGNO])

struct expr_status GTY(())
{
  /* Number of units that we should eventually pop off the stack.
     These are the arguments to function calls that have already returned.  */
  int x_pending_stack_adjust;

  /* Under some ABIs, it is the caller's responsibility to pop arguments
     pushed for function calls.  A naive implementation would simply pop
     the arguments immediately after each call.  However, if several
     function calls are made in a row, it is typically cheaper to pop
     all the arguments after all of the calls are complete since a
     single pop instruction can be used.  Therefore, GCC attempts to
     defer popping the arguments until absolutely necessary.  (For
     example, at the end of a conditional, the arguments must be popped,
     since code outside the conditional won't know whether or not the
     arguments need to be popped.)

     When INHIBIT_DEFER_POP is nonzero, however, the compiler does not
     attempt to defer pops.  Instead, the stack is popped immediately
     after each call.  Rather then setting this variable directly, use
     NO_DEFER_POP and OK_DEFER_POP.  */
  int x_inhibit_defer_pop;

  /* If PREFERRED_STACK_BOUNDARY and PUSH_ROUNDING are defined, the stack
     boundary can be momentarily unaligned while pushing the arguments.
     Record the delta since last aligned boundary here in order to get
     stack alignment in the nested function calls working right.  */
  int x_stack_pointer_delta;

  /* Nonzero means __builtin_saveregs has already been done in this function.
     The value is the pseudoreg containing the value __builtin_saveregs
     returned.  */
  rtx x_saveregs_value;

  /* Similarly for __builtin_apply_args.  */
  rtx x_apply_args_value;

  /* List of labels that must never be deleted.  */
  rtx x_forced_labels;

  /* Postincrements that still need to be expanded.  */
  rtx x_pending_chain;
};

#define pending_stack_adjust (cfun->expr->x_pending_stack_adjust)
#define inhibit_defer_pop (cfun->expr->x_inhibit_defer_pop)
#define saveregs_value (cfun->expr->x_saveregs_value)
#define apply_args_value (cfun->expr->x_apply_args_value)
#define forced_labels (cfun->expr->x_forced_labels)
#define pending_chain (cfun->expr->x_pending_chain)
#define stack_pointer_delta (cfun->expr->x_stack_pointer_delta)

/* This structure can save all the important global and static variables
   describing the status of the current function.  */

struct function GTY(())
{
  struct eh_status *eh;
  struct stmt_status *stmt;
  struct expr_status *expr;
  struct emit_status *emit;
  struct varasm_status *varasm;

  /* For function.c.  */

  /* Points to the FUNCTION_DECL of this function.  */
  tree decl;

  /* Function containing this function, if any.  */
  struct function *outer;

  /* Number of bytes of args popped by function being compiled on its return.
     Zero if no bytes are to be popped.
     May affect compilation of return insn or of function epilogue.  */
  int pops_args;

  /* If function's args have a fixed size, this is that size, in bytes.
     Otherwise, it is -1.
     May affect compilation of return insn or of function epilogue.  */
  int args_size;

  /* # bytes the prologue should push and pretend that the caller pushed them.
     The prologue must do this, but only if parms can be passed in
     registers.  */
  int pretend_args_size;

  /* # of bytes of outgoing arguments.  If ACCUMULATE_OUTGOING_ARGS is
     defined, the needed space is pushed by the prologue.  */
  int outgoing_args_size;

  /* This is the offset from the arg pointer to the place where the first
     anonymous arg can be found, if there is one.  */
  rtx arg_offset_rtx;

  /* Quantities of various kinds of registers
     used for the current function's args.  */
  CUMULATIVE_ARGS args_info;

  /* If nonzero, an RTL expression for the location at which the current
     function returns its result.  If the current function returns its
     result in a register, current_function_return_rtx will always be
     the hard register containing the result.  */
  rtx return_rtx;

  /* The arg pointer hard register, or the pseudo into which it was copied.  */
  rtx internal_arg_pointer;

  /* Language-specific reason why the current function cannot be made
     inline.  */
  const char *cannot_inline;

  /* Opaque pointer used by get_hard_reg_initial_val and
     has_hard_reg_initial_val (see integrate.[hc]).  */
  struct initial_value_struct *hard_reg_initial_vals;

  /* Number of function calls seen so far in current function.  */
  int x_function_call_count;

  /* List (chain of TREE_LIST) of LABEL_DECLs for all nonlocal labels
     (labels to which there can be nonlocal gotos from nested functions)
     in this function.  */
  tree x_nonlocal_labels;

  /* List (chain of EXPR_LIST) of stack slots that hold the current handlers
     for nonlocal gotos.  There is one for every nonlocal label in the
     function; this list matches the one in nonlocal_labels.
     Zero when function does not have nonlocal labels.  */
  rtx x_nonlocal_goto_handler_slots;

  /* List (chain of EXPR_LIST) of labels heading the current handlers for
     nonlocal gotos.  */
  rtx x_nonlocal_goto_handler_labels;

  /* RTX for stack slot that holds the stack pointer value to restore
     for a nonlocal goto.
     Zero when function does not have nonlocal labels.  */
  rtx x_nonlocal_goto_stack_level;

  /* Label that will go on parm cleanup code, if any.
     Jumping to this label runs cleanup code for parameters, if
     such code must be run.  Following this code is the logical return
     label.  */
  rtx x_cleanup_label;

  /* Label that will go on function epilogue.
     Jumping to this label serves as a "return" instruction
     on machines which require execution of the epilogue on all returns.  */
  rtx x_return_label;

  /* Label that will go on the end of function epilogue.
     Jumping to this label serves as a "naked return" instruction
     on machines which require execution of the epilogue on all returns.  */
  rtx x_naked_return_label;

  /* Label and register for unswitching computed gotos.  */
  rtx computed_goto_common_label;
  rtx computed_goto_common_reg;

  /* List (chain of EXPR_LISTs) of pseudo-regs of SAVE_EXPRs.
     So we can mark them all live at the end of the function, if nonopt.  */
  rtx x_save_expr_regs;

  /* List (chain of EXPR_LISTs) of all stack slots in this function.
     Made for the sake of unshare_all_rtl.  */
  rtx x_stack_slot_list;

  /* Chain of all RTL_EXPRs that have insns in them.  */
  tree x_rtl_expr_chain;

  /* Label to jump back to for tail recursion, or 0 if we have
     not yet needed one for this function.  */
  rtx x_tail_recursion_label;

  /* Place after which to insert the tail_recursion_label if we need one.  */
  rtx x_tail_recursion_reentry;

  /* Location at which to save the argument pointer if it will need to be
     referenced.  There are two cases where this is done: if nonlocal gotos
     exist, or if vars stored at an offset from the argument pointer will be
     needed by inner routines.  */
  rtx x_arg_pointer_save_area;

  /* If the function returns non-void, we will emit a clobber of the
     return registers just in case the user fell off the end without
     returning a proper value.  This is that insn.  */
  rtx x_clobber_return_insn;

  /* Offset to end of allocated area of stack frame.
     If stack grows down, this is the address of the last stack slot allocated.
     If stack grows up, this is the address for the next slot.  */
  HOST_WIDE_INT x_frame_offset;

  /* List (chain of TREE_LISTs) of static chains for containing functions.
     Each link has a FUNCTION_DECL in the TREE_PURPOSE and a reg rtx
     in an RTL_EXPR in the TREE_VALUE.  */
  tree x_context_display;

  /* List (chain of TREE_LISTs) of trampolines for nested functions.
     The trampoline sets up the static chain and jumps to the function.
     We supply the trampoline's address when the function's address is
     requested.

     Each link has a FUNCTION_DECL in the TREE_PURPOSE and a reg rtx
     in an RTL_EXPR in the TREE_VALUE.  */
  tree x_trampoline_list;

  /* Insn after which register parms and SAVE_EXPRs are born, if nonopt.  */
  rtx x_parm_birth_insn;

  /* Last insn of those whose job was to put parms into their nominal
     homes.  */
  rtx x_last_parm_insn;

  /* 1 + last pseudo register number possibly used for loading a copy
     of a parameter of this function.  */
  unsigned int x_max_parm_reg;

  /* Vector indexed by REGNO, containing location on stack in which
     to put the parm which is nominally in pseudo register REGNO,
     if we discover that that parm must go in the stack.  The highest
     element in this vector is one less than MAX_PARM_REG, above.  */
  rtx * GTY ((length ("%h.x_max_parm_reg"))) x_parm_reg_stack_loc;

  /* List of all temporaries allocated, both available and in use.  */
  struct temp_slot *x_temp_slots;

  /* Current nesting level for temporaries.  */
  int x_temp_slot_level;

  /* Current nesting level for variables in a block.  */
  int x_var_temp_slot_level;

  /* When temporaries are created by TARGET_EXPRs, they are created at
     this level of temp_slot_level, so that they can remain allocated
     until no longer needed.  CLEANUP_POINT_EXPRs define the lifetime
     of TARGET_EXPRs.  */
  int x_target_temp_slot_level;

  /* This slot is initialized as 0 and is added to
     during the nested function.  */
  struct var_refs_queue *fixup_var_refs_queue;

  /* For integrate.c.  */
  int inlinable;
  int no_debugging_symbols;
  rtvec original_arg_vector;
  tree original_decl_initial;
  /* Last insn of those whose job was to put parms into their nominal
     homes.  */
  rtx inl_last_parm_insn;
  /* Highest label number in current function.  */
  int inl_max_label_num;

  /* Function sequence number for profiling, debugging, etc.  */
  int funcdef_no;

  /* For md files.  */

  /* tm.h can use this to store whatever it likes.  */
  struct machine_function * GTY ((maybe_undef (""))) machine;
  /* The largest alignment of slot allocated on the stack.  */
  int stack_alignment_needed;
  /* Preferred alignment of the end of stack frame.  */
  int preferred_stack_boundary;
  /* Set when the call to function itself has been emit.  */
  bool recursive_call_emit;

  /* Language-specific code can use this to store whatever it likes.  */
  struct language_function * language;

  /* For reorg.  */

  /* If some insns can be deferred to the delay slots of the epilogue, the
     delay list for them is recorded here.  */
  rtx epilogue_delay_list;

  /* How commonly executed the function is.  Initialized during branch
     probabilities pass.  */
  enum function_frequency {
    /* This function most likely won't be executed at all.
       (set only when profile feedback is available).  */
    FUNCTION_FREQUENCY_UNLIKELY_EXECUTED,
    /* The default value.  */
    FUNCTION_FREQUENCY_NORMAL,
    /* Optimize this function hard
       (set only when profile feedback is available).  */
    FUNCTION_FREQUENCY_HOT
  } function_frequency;

  /* Maximal number of entities in the single jumptable.  Used to estimate
     final flowgraph size.  */
  int max_jumptable_ents;

  /* Collected bit flags.  */

  /* Nonzero if function being compiled needs to be given an address
     where the value should be stored.  */
  unsigned int returns_struct : 1;

  /* Nonzero if function being compiled needs to
     return the address of where it has put a structure value.  */
  unsigned int returns_pcc_struct : 1;

  /* Nonzero if the current function returns a pointer type.  */
  unsigned int returns_pointer : 1;

  /* Nonzero if function being compiled needs to be passed a static chain.  */
  unsigned int needs_context : 1;

  /* Nonzero if function being compiled can call setjmp.  */
  unsigned int calls_setjmp : 1;

  /* Nonzero if function being compiled can call longjmp.  */
  unsigned int calls_longjmp : 1;

  /* Nonzero if function being compiled can call alloca,
     either as a subroutine or builtin.  */
  unsigned int calls_alloca : 1;

  /* Nonzero if the function calls __builtin_eh_return.  */
  unsigned int calls_eh_return : 1;

  /* Nonzero if the function calls __builtin_constant_p.  */
  unsigned int calls_constant_p : 1;

  /* Nonzero if function being compiled receives nonlocal gotos
     from nested functions.  */
  unsigned int has_nonlocal_label : 1;

  /* Nonzero if function being compiled has nonlocal gotos to parent
     function.  */
  unsigned int has_nonlocal_goto : 1;

  /* Nonzero if function being compiled contains nested functions.  */
  unsigned int contains_functions : 1;

  /* Nonzero if the function being compiled issues a computed jump.  */
  unsigned int has_computed_jump : 1;

  /* Nonzero if the current function is a thunk, i.e., a lightweight
     function implemented by the output_mi_thunk hook) that just
     adjusts one of its arguments and forwards to another
     function.  */
  unsigned int is_thunk : 1;

  /* This bit is used by the exception handling logic.  It is set if all
     calls (if any) are sibling calls.  Such functions do not have to
     have EH tables generated, as they cannot throw.  A call to such a
     function, however, should be treated as throwing if any of its callees
     can throw.  */
  unsigned int all_throwers_are_sibcalls : 1;

  /* Nonzero if instrumentation calls for function entry and exit should be
     generated.  */
  unsigned int instrument_entry_exit : 1;

  /* Nonzero if profiling code should be generated.  */
  unsigned int profile : 1;

  /* Nonzero if stack limit checking should be enabled in the current
     function.  */
  unsigned int limit_stack : 1;

  /* Nonzero if current function uses stdarg.h or equivalent.  */
  unsigned int stdarg : 1;

  /* Nonzero if this function is being processed in function-at-a-time
     mode.  In other words, if all tree structure for this function,
     including the BLOCK tree, is created before RTL generation
     commences.  */
  unsigned int x_whole_function_mode_p : 1;

  /* Nonzero if the back-end should not keep track of expressions that
     determine the size of variable-sized objects.  Normally, such
     expressions are saved away, and then expanded when the next
     function is started.  For example, if a parameter has a
     variable-sized type, then the size of the parameter is computed
     when the function body is entered.  However, some front-ends do
     not desire this behavior.  */
  unsigned int x_dont_save_pending_sizes_p : 1;

  /* Nonzero if the current function uses the constant pool.  */
  unsigned int uses_const_pool : 1;

  /* Nonzero if the current function uses pic_offset_table_rtx.  */
  unsigned int uses_pic_offset_table : 1;

  /* Nonzero if the current function needs an lsda for exception handling.  */
  unsigned int uses_eh_lsda : 1;

  /* Nonzero if code to initialize arg_pointer_save_area has been emitted.  */
  unsigned int arg_pointer_save_area_init : 1;

  /* Flag for use by ther rtl inliner, to tell if the function has been
     processed at least once.  */
  unsigned int rtl_inline_init : 1;

  /* Nonzero if the rtl inliner has saved the function for inlining.  */
  unsigned int saved_for_inline : 1;
};

/* The function currently being compiled.  */
extern GTY(()) struct function *cfun;

/* Pointer to chain of `struct function' for containing functions.  */
extern GTY(()) struct function *outer_function_chain;

/* Nonzero if we've already converted virtual regs to hard regs.  */
extern int virtuals_instantiated;

/* Nonzero if at least one trampoline has been created.  */
extern int trampolines_created;

/* For backward compatibility... eventually these should all go away.  */
#define current_function_pops_args (cfun->pops_args)
#define current_function_returns_struct (cfun->returns_struct)
#define current_function_returns_pcc_struct (cfun->returns_pcc_struct)
#define current_function_returns_pointer (cfun->returns_pointer)
#define current_function_needs_context (cfun->needs_context)
#define current_function_calls_setjmp (cfun->calls_setjmp)
#define current_function_calls_alloca (cfun->calls_alloca)
#define current_function_calls_longjmp (cfun->calls_longjmp)
#define current_function_calls_eh_return (cfun->calls_eh_return)
#define current_function_calls_constant_p (cfun->calls_constant_p)
#define current_function_has_computed_jump (cfun->has_computed_jump)
#define current_function_contains_functions (cfun->contains_functions)
#define current_function_is_thunk (cfun->is_thunk)
#define current_function_args_info (cfun->args_info)
#define current_function_args_size (cfun->args_size)
#define current_function_pretend_args_size (cfun->pretend_args_size)
#define current_function_outgoing_args_size (cfun->outgoing_args_size)
#define current_function_arg_offset_rtx (cfun->arg_offset_rtx)
#define current_function_stdarg (cfun->stdarg)
#define current_function_internal_arg_pointer (cfun->internal_arg_pointer)
#define current_function_return_rtx (cfun->return_rtx)
#define current_function_instrument_entry_exit (cfun->instrument_entry_exit)
#define current_function_profile (cfun->profile)
#define current_function_funcdef_no (cfun->funcdef_no)
#define current_function_limit_stack (cfun->limit_stack)
#define current_function_uses_pic_offset_table (cfun->uses_pic_offset_table)
#define current_function_uses_const_pool (cfun->uses_const_pool)
#define current_function_cannot_inline (cfun->cannot_inline)
#define current_function_epilogue_delay_list (cfun->epilogue_delay_list)
#define current_function_has_nonlocal_label (cfun->has_nonlocal_label)
#define current_function_has_nonlocal_goto (cfun->has_nonlocal_goto)

#define max_parm_reg (cfun->x_max_parm_reg)
#define parm_reg_stack_loc (cfun->x_parm_reg_stack_loc)
#define cleanup_label (cfun->x_cleanup_label)
#define return_label (cfun->x_return_label)
#define naked_return_label (cfun->x_naked_return_label)
#define save_expr_regs (cfun->x_save_expr_regs)
#define stack_slot_list (cfun->x_stack_slot_list)
#define parm_birth_insn (cfun->x_parm_birth_insn)
#define frame_offset (cfun->x_frame_offset)
#define tail_recursion_label (cfun->x_tail_recursion_label)
#define tail_recursion_reentry (cfun->x_tail_recursion_reentry)
#define arg_pointer_save_area (cfun->x_arg_pointer_save_area)
#define rtl_expr_chain (cfun->x_rtl_expr_chain)
#define last_parm_insn (cfun->x_last_parm_insn)
#define context_display (cfun->x_context_display)
#define trampoline_list (cfun->x_trampoline_list)
#define function_call_count (cfun->x_function_call_count)
#define temp_slots (cfun->x_temp_slots)
#define temp_slot_level (cfun->x_temp_slot_level)
#define target_temp_slot_level (cfun->x_target_temp_slot_level)
#define var_temp_slot_level (cfun->x_var_temp_slot_level)
#define nonlocal_labels (cfun->x_nonlocal_labels)
#define nonlocal_goto_handler_slots (cfun->x_nonlocal_goto_handler_slots)
#define nonlocal_goto_handler_labels (cfun->x_nonlocal_goto_handler_labels)
#define nonlocal_goto_stack_level (cfun->x_nonlocal_goto_stack_level)

/* The FUNCTION_DECL for an inline function currently being expanded.  */
extern tree inline_function_decl;

/* Given a function decl for a containing function,
   return the `struct function' for it.  */
struct function *find_function_data (tree);

/* Set NOTE_BLOCK for each block note in the current function.  */
extern void identify_blocks (void);

/* Identify BLOCKs referenced by more than one NOTE_INSN_BLOCK_{BEG,END},
   and create duplicate blocks.  */
extern void reorder_blocks (void);

/* Set BLOCK_NUMBER for all the blocks in FN.  */
extern void number_blocks (tree);

/* Return size needed for stack frame based on slots so far allocated.
   This size counts from zero.  It is not rounded to STACK_BOUNDARY;
   the caller may have to do that.  */
extern HOST_WIDE_INT get_frame_size (void);
/* Likewise, but for a different than the current function.  */
extern HOST_WIDE_INT get_func_frame_size (struct function *);

/* A pointer to a function to create target specific, per-function
   data structures.  */
extern struct machine_function * (*init_machine_status) (void);

/* Save and restore status information for a nested function.  */
extern void restore_emit_status (struct function *);
extern void free_after_parsing (struct function *);
extern void free_after_compilation (struct function *);

extern void init_varasm_status (struct function *);

#ifdef RTX_CODE
extern void diddle_return_value (void (*)(rtx, void*), void*);
extern void clobber_return_register (void);
extern void use_return_register (void);
#endif

extern rtx get_arg_pointer_save_area (struct function *);

extern void init_virtual_regs (struct emit_status *);

/* Returns the name of the current function.  */
extern const char *current_function_name (void);

/* Called once, at initialization, to initialize function.c.  */
extern void init_function_once (void);

extern void do_warn_unused_parameter (tree);

#endif  /* GCC_FUNCTION_H */
