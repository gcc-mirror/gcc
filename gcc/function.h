/* Structure for saving state for a nested function.
   Copyright (C) 1989, 92-97, 1998 Free Software Foundation, Inc.

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


#if !defined(NULL_TREE) && !defined(tree)
typedef union union_node *_function_tree;
#define tree _function_tree
#endif
#if !defined(NULL_RTX) && !defined(rtx)
typedef struct rtx_def *_function_rtx;
#define rtx _function_rtx
#endif

struct var_refs_queue
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

struct sequence_stack
{
  /* First and last insns in the chain of the saved sequence.  */
  rtx first, last;
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

/* This structure can save all the important global and static variables
   describing the status of the current function.  */

struct function
{
  struct function *next;

  /* For function.c.  */
  char *name;
  tree decl;
  int pops_args;
  int returns_struct;
  int returns_pcc_struct;
  int returns_pointer;
  int needs_context;
  int calls_setjmp;
  int calls_longjmp;
  int calls_alloca;
  int has_nonlocal_label;
  int has_nonlocal_goto;
  int contains_functions;
  int has_computed_jump;
  int is_thunk;
  rtx nonlocal_goto_handler_slots;
  rtx nonlocal_goto_handler_labels;
  rtx nonlocal_goto_stack_level;
  tree nonlocal_labels;
  int args_size;
  int pretend_args_size;
  rtx arg_offset_rtx;
  int varargs;
  int stdarg;
  int max_parm_reg;
  rtx *parm_reg_stack_loc;
  int outgoing_args_size;
  rtx return_rtx;
  rtx cleanup_label;
  rtx return_label;
  rtx save_expr_regs;
  rtx stack_slot_list;
  rtx parm_birth_insn;
  HOST_WIDE_INT frame_offset;
  rtx tail_recursion_label;
  rtx tail_recursion_reentry;
  rtx internal_arg_pointer;
  char *cannot_inline;
  rtx arg_pointer_save_area;
  tree rtl_expr_chain;
  rtx last_parm_insn;
  tree context_display;
  tree trampoline_list;
  int function_call_count;
  struct temp_slot *temp_slots;
  int temp_slot_level;
  int target_temp_slot_level;
  int var_temp_slot_level;
  int instrument_entry_exit;
  /* This slot is initialized as 0 and is added to
     during the nested function.  */
  struct var_refs_queue *fixup_var_refs_queue;
  CUMULATIVE_ARGS args_info;

  /* For stmt.c  */
  struct nesting *block_stack;
  struct nesting *stack_block_stack;
  struct nesting *cond_stack;
  struct nesting *loop_stack;
  struct nesting *case_stack;
  struct nesting *nesting_stack;
  int nesting_depth;
  int block_start_count;
  tree last_expr_type;
  rtx last_expr_value;
  int expr_stmts_for_value;
  char *emit_filename;
  int emit_lineno;
  struct goto_fixup *goto_fixup_chain;

  /* For exception handling information.  */
  struct eh_stack ehstack;
  struct eh_stack catchstack;
  struct eh_queue ehqueue;
  rtx catch_clauses;
  struct label_node *false_label_stack;
  struct label_node *caught_return_label_stack;
  tree protect_list;
  rtx ehc;
  rtx eh_return_stub_label;

  /* For expr.c.  */
  rtx pending_chain;
  int pending_stack_adjust;
  int inhibit_defer_pop;
  rtx saveregs_value;
  rtx apply_args_value;
  rtx forced_labels;
  int check_memory_usage;

  /* For emit-rtl.c.  */
  int reg_rtx_no;
  int first_label_num;
  rtx first_insn;
  rtx last_insn;
  tree sequence_rtl_expr;
  struct sequence_stack *sequence_stack;
  int cur_insn_uid;
  int last_linenum;
  char *last_filename;
  char *regno_pointer_flag;
  char *regno_pointer_align;
  int regno_pointer_flag_length;
  rtx *regno_reg_rtx;

  /* For stor-layout.c.  */
  tree permanent_type_chain;
  tree temporary_type_chain;
  tree permanent_type_end;
  tree temporary_type_end;
  tree pending_sizes;
  int immediate_size_expand;

  /* For tree.c.  */
  int all_types_permanent;
  struct momentary_level *momentary_stack;
  char *maybepermanent_firstobj;
  char *temporary_firstobj;
  char *momentary_firstobj;
  char *momentary_function_firstobj;
  struct obstack *current_obstack;
  struct obstack *function_obstack;
  struct obstack *function_maybepermanent_obstack;
  struct obstack *expression_obstack;
  struct obstack *saveable_obstack;
  struct obstack *rtl_obstack;
  struct simple_obstack_stack *inline_obstacks;

  /* For integrate.c.  */
  int uses_const_pool;

  /* For md files.  */
  int uses_pic_offset_table;
  /* tm.h can use this to store whatever it likes.  */
  struct machine_function *machine;

  /* For reorg.  */
  rtx epilogue_delay_list;

  /* For varasm.  */
  struct constant_descriptor **const_rtx_hash_table;
  struct pool_sym **const_rtx_sym_hash_table;
  struct pool_constant *first_pool, *last_pool;
  int pool_offset;
  rtx const_double_chain;
};

/* The FUNCTION_DECL for an inline function currently being expanded.  */
extern tree inline_function_decl;

/* Label that will go on parm cleanup code, if any.
   Jumping to this label runs cleanup code for parameters, if
   such code must be run.  Following this code is the logical return label.  */

extern rtx cleanup_label;

/* Label that will go on function epilogue.
   Jumping to this label serves as a "return" instruction
   on machines which require execution of the epilogue on all returns.  */

extern rtx return_label;

/* Offset to end of allocated area of stack frame.
   If stack grows down, this is the address of the last stack slot allocated.
   If stack grows up, this is the address for the next slot.  */
extern HOST_WIDE_INT frame_offset;

/* Label to jump back to for tail recursion, or 0 if we have
   not yet needed one for this function.  */
extern rtx tail_recursion_label;

/* Place after which to insert the tail_recursion_label if we need one.  */
extern rtx tail_recursion_reentry;

/* Location at which to save the argument pointer if it will need to be
   referenced.  There are two cases where this is done: if nonlocal gotos
   exist, or if vars whose is an offset from the argument pointer will be
   needed by inner routines.  */

extern rtx arg_pointer_save_area;

/* Chain of all RTL_EXPRs that have insns in them.  */
extern tree rtl_expr_chain;

/* List (chain of EXPR_LISTs) of all stack slots in this function.
   Made for the sake of unshare_all_rtl.  */
extern rtx stack_slot_list;

/* Given a function decl for a containing function,
   return the `struct function' for it.  */
struct function *find_function_data PROTO((tree));

/* Pointer to chain of `struct function' for containing functions.  */
extern struct function *outer_function_chain;

/* Put all this function's BLOCK nodes into a vector and return it.
   Also store in each NOTE for the beginning or end of a block
   the index of that block in the vector.  */
extern tree *identify_blocks PROTO((tree, rtx));

/* Return size needed for stack frame based on slots so far allocated.
   This size counts from zero.  It is not rounded to STACK_BOUNDARY;
   the caller may have to do that.  */
extern HOST_WIDE_INT get_frame_size PROTO((void));

/* These variables hold pointers to functions to
   save and restore machine-specific data,
   in push_function_context and pop_function_context.  */
extern void (*save_machine_status) PROTO((struct function *));
extern void (*restore_machine_status) PROTO((struct function *));

/* Save and restore status information for a nested function.  */
extern void save_tree_status		PROTO((struct function *, tree));
extern void restore_tree_status		PROTO((struct function *, tree));
extern void save_varasm_status		PROTO((struct function *, tree));
extern void restore_varasm_status	PROTO((struct function *));
extern void save_eh_status		PROTO((struct function *));
extern void restore_eh_status		PROTO((struct function *));
extern void save_stmt_status		PROTO((struct function *));
extern void restore_stmt_status		PROTO((struct function *));
extern void save_expr_status		PROTO((struct function *));
extern void restore_expr_status		PROTO((struct function *));
extern void save_emit_status		PROTO((struct function *));
extern void restore_emit_status		PROTO((struct function *));
extern void save_storage_status		PROTO((struct function *));
extern void restore_storage_status	PROTO((struct function *));

extern rtx get_first_block_beg		PROTO((void));

#ifdef rtx
#undef rtx
#endif

#ifdef tree
#undef tree
#endif
