/* Expands front end tree to back end RTL for GNU C-Compiler
   Copyright (C) 1987, 88, 89, 92, 93, 94, 1995 Free Software Foundation, Inc.

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


/* This file handles the generation of rtl code from tree structure
   above the level of expressions, using subroutines in exp*.c and emit-rtl.c.
   It also creates the rtl expressions for parameters and auto variables
   and has full responsibility for allocating stack slots.

   The functions whose names start with `expand_' are called by the
   parser to generate RTL instructions for various kinds of constructs.

   Some control and binding constructs require calling several such
   functions at different times.  For example, a simple if-then
   is expanded by calling `expand_start_cond' (with the condition-expression
   as argument) before parsing the then-clause and calling `expand_end_cond'
   after parsing the then-clause.  */

#include "config.h"

#include <stdio.h>
#include <ctype.h>

#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "function.h"
#include "insn-flags.h"
#include "insn-config.h"
#include "insn-codes.h"
#include "expr.h"
#include "hard-reg-set.h"
#include "obstack.h"
#include "loop.h"
#include "recog.h"
#include "machmode.h"

#include "bytecode.h"
#include "bc-typecd.h"
#include "bc-opcode.h"
#include "bc-optab.h"
#include "bc-emit.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free
struct obstack stmt_obstack;

/* Filename and line number of last line-number note,
   whether we actually emitted it or not.  */
char *emit_filename;
int emit_lineno;

/* Nonzero if within a ({...}) grouping, in which case we must
   always compute a value for each expr-stmt in case it is the last one.  */

int expr_stmts_for_value;

/* Each time we expand an expression-statement,
   record the expr's type and its RTL value here.  */

static tree last_expr_type;
static rtx last_expr_value;

/* Each time we expand the end of a binding contour (in `expand_end_bindings')
   and we emit a new NOTE_INSN_BLOCK_END note, we save a pointer to it here.
   This is used by the `remember_end_note' function to record the endpoint
   of each generated block in its associated BLOCK node.  */

static rtx last_block_end_note;

/* Number of binding contours started so far in this function.  */

int block_start_count;

/* Nonzero if function being compiled needs to
   return the address of where it has put a structure value.  */

extern int current_function_returns_pcc_struct;

/* Label that will go on parm cleanup code, if any.
   Jumping to this label runs cleanup code for parameters, if
   such code must be run.  Following this code is the logical return label.  */

extern rtx cleanup_label;

/* Label that will go on function epilogue.
   Jumping to this label serves as a "return" instruction
   on machines which require execution of the epilogue on all returns.  */

extern rtx return_label;

/* List (chain of EXPR_LISTs) of pseudo-regs of SAVE_EXPRs.
   So we can mark them all live at the end of the function, if nonopt.  */
extern rtx save_expr_regs;

/* Offset to end of allocated area of stack frame.
   If stack grows down, this is the address of the last stack slot allocated.
   If stack grows up, this is the address for the next slot.  */
extern int frame_offset;

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

#if 0  /* Turned off because 0 seems to work just as well.  */
/* Cleanup lists are required for binding levels regardless of whether
   that binding level has cleanups or not.  This node serves as the
   cleanup list whenever an empty list is required.  */
static tree empty_cleanup_list;
#endif

extern void (*interim_eh_hook)	PROTO((tree));

/* Functions and data structures for expanding case statements.  */

/* Case label structure, used to hold info on labels within case
   statements.  We handle "range" labels; for a single-value label
   as in C, the high and low limits are the same.

   A chain of case nodes is initially maintained via the RIGHT fields
   in the nodes.  Nodes with higher case values are later in the list.

   Switch statements can be output in one of two forms.  A branch table
   is used if there are more than a few labels and the labels are dense
   within the range between the smallest and largest case value.  If a
   branch table is used, no further manipulations are done with the case
   node chain.

   The alternative to the use of a branch table is to generate a series
   of compare and jump insns.  When that is done, we use the LEFT, RIGHT,
   and PARENT fields to hold a binary tree.  Initially the tree is
   totally unbalanced, with everything on the right.  We balance the tree
   with nodes on the left having lower case values than the parent
   and nodes on the right having higher values.  We then output the tree
   in order.  */

struct case_node
{
  struct case_node	*left;	/* Left son in binary tree */
  struct case_node	*right;	/* Right son in binary tree; also node chain */
  struct case_node	*parent; /* Parent of node in binary tree */
  tree			low;	/* Lowest index value for this label */
  tree			high;	/* Highest index value for this label */
  tree			code_label; /* Label to jump to when node matches */
};

typedef struct case_node case_node;
typedef struct case_node *case_node_ptr;

/* These are used by estimate_case_costs and balance_case_nodes.  */

/* This must be a signed type, and non-ANSI compilers lack signed char.  */
static short *cost_table;
static int use_cost_table;

/* Stack of control and binding constructs we are currently inside.

   These constructs begin when you call `expand_start_WHATEVER'
   and end when you call `expand_end_WHATEVER'.  This stack records
   info about how the construct began that tells the end-function
   what to do.  It also may provide information about the construct
   to alter the behavior of other constructs within the body.
   For example, they may affect the behavior of C `break' and `continue'.

   Each construct gets one `struct nesting' object.
   All of these objects are chained through the `all' field.
   `nesting_stack' points to the first object (innermost construct).
   The position of an entry on `nesting_stack' is in its `depth' field.

   Each type of construct has its own individual stack.
   For example, loops have `loop_stack'.  Each object points to the
   next object of the same type through the `next' field.

   Some constructs are visible to `break' exit-statements and others
   are not.  Which constructs are visible depends on the language.
   Therefore, the data structure allows each construct to be visible
   or not, according to the args given when the construct is started.
   The construct is visible if the `exit_label' field is non-null.
   In that case, the value should be a CODE_LABEL rtx.  */

struct nesting
{
  struct nesting *all;
  struct nesting *next;
  int depth;
  rtx exit_label;
  union
    {
      /* For conds (if-then and if-then-else statements).  */
      struct
	{
	  /* Label for the end of the if construct.
	     There is none if EXITFLAG was not set
	     and no `else' has been seen yet.  */
	  rtx endif_label;
	  /* Label for the end of this alternative.
	     This may be the end of the if or the next else/elseif. */
	  rtx next_label;
	} cond;
      /* For loops.  */
      struct
	{
	  /* Label at the top of the loop; place to loop back to.  */
	  rtx start_label;
	  /* Label at the end of the whole construct.  */
	  rtx end_label;
	  /* Label before a jump that branches to the end of the whole
	     construct.  This is where destructors go if any.  */
	  rtx alt_end_label;
	  /* Label for `continue' statement to jump to;
	     this is in front of the stepper of the loop.  */
	  rtx continue_label;
	} loop;
      /* For variable binding contours.  */
      struct
	{
	  /* Sequence number of this binding contour within the function,
	     in order of entry.  */
	  int block_start_count;
	  /* Nonzero => value to restore stack to on exit.  Complemented by
	     bc_stack_level (see below) when generating bytecodes. */
	  rtx stack_level;
	  /* The NOTE that starts this contour.
	     Used by expand_goto to check whether the destination
	     is within each contour or not.  */
	  rtx first_insn;
	  /* Innermost containing binding contour that has a stack level.  */
	  struct nesting *innermost_stack_block;
	  /* List of cleanups to be run on exit from this contour.
	     This is a list of expressions to be evaluated.
	     The TREE_PURPOSE of each link is the ..._DECL node
	     which the cleanup pertains to.  */
	  tree cleanups;
	  /* List of cleanup-lists of blocks containing this block,
	     as they were at the locus where this block appears.
	     There is an element for each containing block,
	     ordered innermost containing block first.
	     The tail of this list can be 0 (was empty_cleanup_list),
	     if all remaining elements would be empty lists.
	     The element's TREE_VALUE is the cleanup-list of that block,
	     which may be null.  */
	  tree outer_cleanups;
	  /* Chain of labels defined inside this binding contour.
	     For contours that have stack levels or cleanups.  */
	  struct label_chain *label_chain;
	  /* Number of function calls seen, as of start of this block.  */
	  int function_call_count;
	  /* Bytecode specific: stack level to restore stack to on exit.  */
	  int bc_stack_level;
	} block;
      /* For switch (C) or case (Pascal) statements,
	 and also for dummies (see `expand_start_case_dummy').  */
      struct
	{
	  /* The insn after which the case dispatch should finally
	     be emitted.  Zero for a dummy.  */
	  rtx start;
	  /* For bytecodes, the case table is in-lined right in the code.
	     A label is needed for skipping over this block. It is only
	     used when generating bytecodes. */
	  rtx skip_label;
	  /* A list of case labels, kept in ascending order by value
	     as the list is built.
	     During expand_end_case, this list may be rearranged into a
	     nearly balanced binary tree.  */
	  struct case_node *case_list;
	  /* Label to jump to if no case matches.  */
	  tree default_label;
	  /* The expression to be dispatched on.  */
	  tree index_expr;
	  /* Type that INDEX_EXPR should be converted to.  */
	  tree nominal_type;
	  /* Number of range exprs in case statement.  */
	  int num_ranges;
	  /* Name of this kind of statement, for warnings.  */
	  char *printname;
	  /* Nonzero if a case label has been seen in this case stmt.  */
	  char seenlabel;
	} case_stmt;
    } data;
};

/* Chain of all pending binding contours.  */
struct nesting *block_stack;

/* If any new stacks are added here, add them to POPSTACKS too.  */

/* Chain of all pending binding contours that restore stack levels
   or have cleanups.  */
struct nesting *stack_block_stack;

/* Chain of all pending conditional statements.  */
struct nesting *cond_stack;

/* Chain of all pending loops.  */
struct nesting *loop_stack;

/* Chain of all pending case or switch statements.  */
struct nesting *case_stack;

/* Separate chain including all of the above,
   chained through the `all' field.  */
struct nesting *nesting_stack;

/* Number of entries on nesting_stack now.  */
int nesting_depth;

/* Allocate and return a new `struct nesting'.  */

#define ALLOC_NESTING() \
 (struct nesting *) obstack_alloc (&stmt_obstack, sizeof (struct nesting))

/* Pop the nesting stack element by element until we pop off
   the element which is at the top of STACK.
   Update all the other stacks, popping off elements from them
   as we pop them from nesting_stack.  */

#define POPSTACK(STACK)					\
do { struct nesting *target = STACK;			\
     struct nesting *this;				\
     do { this = nesting_stack;				\
	  if (loop_stack == this)			\
	    loop_stack = loop_stack->next;		\
	  if (cond_stack == this)			\
	    cond_stack = cond_stack->next;		\
	  if (block_stack == this)			\
	    block_stack = block_stack->next;		\
	  if (stack_block_stack == this)		\
	    stack_block_stack = stack_block_stack->next; \
	  if (case_stack == this)			\
	    case_stack = case_stack->next;		\
	  nesting_depth = nesting_stack->depth - 1;	\
	  nesting_stack = this->all;			\
	  obstack_free (&stmt_obstack, this); }		\
     while (this != target); } while (0)

/* In some cases it is impossible to generate code for a forward goto
   until the label definition is seen.  This happens when it may be necessary
   for the goto to reset the stack pointer: we don't yet know how to do that.
   So expand_goto puts an entry on this fixup list.
   Each time a binding contour that resets the stack is exited,
   we check each fixup.
   If the target label has now been defined, we can insert the proper code.  */

struct goto_fixup
{
  /* Points to following fixup.  */
  struct goto_fixup *next;
  /* Points to the insn before the jump insn.
     If more code must be inserted, it goes after this insn.  */
  rtx before_jump;
  /* The LABEL_DECL that this jump is jumping to, or 0
     for break, continue or return.  */
  tree target;
  /* The BLOCK for the place where this goto was found.  */
  tree context;
  /* The CODE_LABEL rtx that this is jumping to.  */
  rtx target_rtl;
  /* Number of binding contours started in current function
     before the label reference.  */
  int block_start_count;
  /* The outermost stack level that should be restored for this jump.
     Each time a binding contour that resets the stack is exited,
     if the target label is *not* yet defined, this slot is updated.  */
  rtx stack_level;
  /* List of lists of cleanup expressions to be run by this goto.
     There is one element for each block that this goto is within.
     The tail of this list can be 0 (was empty_cleanup_list),
     if all remaining elements would be empty.
     The TREE_VALUE contains the cleanup list of that block as of the
     time this goto was seen.
     The TREE_ADDRESSABLE flag is 1 for a block that has been exited.  */
  tree cleanup_list_list;

  /* Bytecode specific members follow */

  /* The label that this jump is jumping to, or 0 for break, continue
     or return.  */
  struct bc_label *bc_target;

  /* The label we use for the fixup patch */
  struct bc_label *label;

  /* True (non-0) if fixup has been handled */
  int bc_handled:1;

  /* Like stack_level above, except refers to the interpreter stack */
  int bc_stack_level;
};

static struct goto_fixup *goto_fixup_chain;

/* Within any binding contour that must restore a stack level,
   all labels are recorded with a chain of these structures.  */

struct label_chain
{
  /* Points to following fixup.  */
  struct label_chain *next;
  tree label;
};
static void expand_goto_internal	PROTO((tree, rtx, rtx));
static void bc_expand_goto_internal	PROTO((enum bytecode_opcode,
					       struct bc_label *, tree));
static int expand_fixup			PROTO((tree, rtx, rtx));
static void bc_expand_fixup		PROTO((enum bytecode_opcode,
					       struct bc_label *, int));
static void fixup_gotos			PROTO((struct nesting *, rtx, tree,
					       rtx, int));
static void bc_fixup_gotos		PROTO((struct nesting *, int, tree,
					       rtx, int));
static void bc_expand_start_cond	PROTO((tree, int));
static void bc_expand_end_cond		PROTO((void));
static void bc_expand_start_else	PROTO((void));
static void bc_expand_end_loop		PROTO((void));
static void bc_expand_end_bindings	PROTO((tree, int, int));
static void bc_expand_decl		PROTO((tree, tree));
static void bc_expand_variable_local_init PROTO((tree));
static void bc_expand_decl_init		PROTO((tree));
static void expand_null_return_1	PROTO((rtx, int));
static void expand_value_return		PROTO((rtx));
static int tail_recursion_args		PROTO((tree, tree));
static void expand_cleanups		PROTO((tree, tree, int, int));
static void bc_expand_start_case	PROTO((struct nesting *, tree,
					       tree, char *));
static int bc_pushcase			PROTO((tree, tree));
static void bc_check_for_full_enumeration_handling PROTO((tree));
static void bc_expand_end_case		PROTO((tree));
static void do_jump_if_equal		PROTO((rtx, rtx, rtx, int));
static int estimate_case_costs		PROTO((case_node_ptr));
static void group_case_nodes		PROTO((case_node_ptr));
static void balance_case_nodes		PROTO((case_node_ptr *,
					       case_node_ptr));
static int node_has_low_bound		PROTO((case_node_ptr, tree));
static int node_has_high_bound		PROTO((case_node_ptr, tree));
static int node_is_bounded		PROTO((case_node_ptr, tree));
static void emit_jump_if_reachable	PROTO((rtx));
static void emit_case_nodes		PROTO((rtx, case_node_ptr, rtx, tree));

extern rtx bc_allocate_local ();
extern rtx bc_allocate_variable_array ();

void
init_stmt ()
{
  gcc_obstack_init (&stmt_obstack);
#if 0
  empty_cleanup_list = build_tree_list (NULL_TREE, NULL_TREE);
#endif
}

void
init_stmt_for_function ()
{
  /* We are not currently within any block, conditional, loop or case.  */
  block_stack = 0;
  stack_block_stack = 0;
  loop_stack = 0;
  case_stack = 0;
  cond_stack = 0;
  nesting_stack = 0;
  nesting_depth = 0;

  block_start_count = 0;

  /* No gotos have been expanded yet.  */
  goto_fixup_chain = 0;

  /* We are not processing a ({...}) grouping.  */
  expr_stmts_for_value = 0;
  last_expr_type = 0;
}

void
save_stmt_status (p)
     struct function *p;
{
  p->block_stack = block_stack;
  p->stack_block_stack = stack_block_stack;
  p->cond_stack = cond_stack;
  p->loop_stack = loop_stack;
  p->case_stack = case_stack;
  p->nesting_stack = nesting_stack;
  p->nesting_depth = nesting_depth;
  p->block_start_count = block_start_count;
  p->last_expr_type = last_expr_type;
  p->last_expr_value = last_expr_value;
  p->expr_stmts_for_value = expr_stmts_for_value;
  p->emit_filename = emit_filename;
  p->emit_lineno = emit_lineno;
  p->goto_fixup_chain = goto_fixup_chain;
}

void
restore_stmt_status (p)
     struct function *p;
{
  block_stack = p->block_stack;
  stack_block_stack = p->stack_block_stack;
  cond_stack = p->cond_stack;
  loop_stack = p->loop_stack;
  case_stack = p->case_stack;
  nesting_stack = p->nesting_stack;
  nesting_depth = p->nesting_depth;
  block_start_count = p->block_start_count;
  last_expr_type = p->last_expr_type;
  last_expr_value = p->last_expr_value;
  expr_stmts_for_value = p->expr_stmts_for_value;
  emit_filename = p->emit_filename;
  emit_lineno = p->emit_lineno;
  goto_fixup_chain = p->goto_fixup_chain;
}

/* Emit a no-op instruction.  */

void
emit_nop ()
{
  rtx last_insn;

  if (!output_bytecode)
    {
      last_insn = get_last_insn ();
      if (!optimize
	  && (GET_CODE (last_insn) == CODE_LABEL
	      || (GET_CODE (last_insn) == NOTE
		  && prev_real_insn (last_insn) == 0)))
	emit_insn (gen_nop ());
    }
}

/* Return the rtx-label that corresponds to a LABEL_DECL,
   creating it if necessary.  */

rtx
label_rtx (label)
     tree label;
{
  if (TREE_CODE (label) != LABEL_DECL)
    abort ();

  if (DECL_RTL (label))
    return DECL_RTL (label);

  return DECL_RTL (label) = gen_label_rtx ();
}

/* Add an unconditional jump to LABEL as the next sequential instruction.  */

void
emit_jump (label)
     rtx label;
{
  do_pending_stack_adjust ();
  emit_jump_insn (gen_jump (label));
  emit_barrier ();
}

/* Emit code to jump to the address
   specified by the pointer expression EXP.  */

void
expand_computed_goto (exp)
     tree exp;
{
  if (output_bytecode)
    {
      bc_expand_expr (exp);
      bc_emit_instruction (jumpP);
    }
  else
    {
      rtx x = expand_expr (exp, NULL_RTX, VOIDmode, 0);

#ifdef POINTERS_EXTEND_UNSIGNED
      x = convert_memory_address (Pmode, x);
#endif

      emit_queue ();
      do_pending_stack_adjust ();
      emit_indirect_jump (x);
    }
}

/* Handle goto statements and the labels that they can go to.  */

/* Specify the location in the RTL code of a label LABEL,
   which is a LABEL_DECL tree node.

   This is used for the kind of label that the user can jump to with a
   goto statement, and for alternatives of a switch or case statement.
   RTL labels generated for loops and conditionals don't go through here;
   they are generated directly at the RTL level, by other functions below.

   Note that this has nothing to do with defining label *names*.
   Languages vary in how they do that and what that even means.  */

void
expand_label (label)
     tree label;
{
  struct label_chain *p;

  if (output_bytecode)
    {
      if (! DECL_RTL (label))
	DECL_RTL (label) = bc_gen_rtx ((char *) 0, 0, bc_get_bytecode_label ());
      if (! bc_emit_bytecode_labeldef (BYTECODE_BC_LABEL (DECL_RTL (label))))
	error ("multiply defined label");
      return;
    }

  do_pending_stack_adjust ();
  emit_label (label_rtx (label));
  if (DECL_NAME (label))
    LABEL_NAME (DECL_RTL (label)) = IDENTIFIER_POINTER (DECL_NAME (label));

  if (stack_block_stack != 0)
    {
      p = (struct label_chain *) oballoc (sizeof (struct label_chain));
      p->next = stack_block_stack->data.block.label_chain;
      stack_block_stack->data.block.label_chain = p;
      p->label = label;
    }
}

/* Declare that LABEL (a LABEL_DECL) may be used for nonlocal gotos
   from nested functions.  */

void
declare_nonlocal_label (label)
     tree label;
{
  nonlocal_labels = tree_cons (NULL_TREE, label, nonlocal_labels);
  LABEL_PRESERVE_P (label_rtx (label)) = 1;
  if (nonlocal_goto_handler_slot == 0)
    {
      nonlocal_goto_handler_slot
	= assign_stack_local (Pmode, GET_MODE_SIZE (Pmode), 0);
      emit_stack_save (SAVE_NONLOCAL,
		       &nonlocal_goto_stack_level,
		       PREV_INSN (tail_recursion_reentry));
    }
}

/* Generate RTL code for a `goto' statement with target label LABEL.
   LABEL should be a LABEL_DECL tree node that was or will later be
   defined with `expand_label'.  */

void
expand_goto (label)
     tree label;
{
  tree context;

  if (output_bytecode)
    {
      expand_goto_internal (label, label_rtx (label), NULL_RTX);
      return;
    }

  /* Check for a nonlocal goto to a containing function.  */
  context = decl_function_context (label);
  if (context != 0 && context != current_function_decl)
    {
      struct function *p = find_function_data (context);
      rtx label_ref = gen_rtx (LABEL_REF, Pmode, label_rtx (label));
      rtx temp;

      p->has_nonlocal_label = 1;
      current_function_has_nonlocal_goto = 1;
      LABEL_REF_NONLOCAL_P (label_ref) = 1;

      /* Copy the rtl for the slots so that they won't be shared in
	 case the virtual stack vars register gets instantiated differently
	 in the parent than in the child.  */

#if HAVE_nonlocal_goto
      if (HAVE_nonlocal_goto)
	emit_insn (gen_nonlocal_goto (lookup_static_chain (label),
				      copy_rtx (p->nonlocal_goto_handler_slot),
				      copy_rtx (p->nonlocal_goto_stack_level),
				      label_ref));
      else
#endif
	{
	  rtx addr;

	  /* Restore frame pointer for containing function.
	     This sets the actual hard register used for the frame pointer
	     to the location of the function's incoming static chain info.
	     The non-local goto handler will then adjust it to contain the
	     proper value and reload the argument pointer, if needed.  */
	  emit_move_insn (hard_frame_pointer_rtx, lookup_static_chain (label));

	  /* We have now loaded the frame pointer hardware register with
	     the address of that corresponds to the start of the virtual
	     stack vars.  So replace virtual_stack_vars_rtx in all
	     addresses we use with stack_pointer_rtx.  */

	  /* Get addr of containing function's current nonlocal goto handler,
	     which will do any cleanups and then jump to the label.  */
	  addr = copy_rtx (p->nonlocal_goto_handler_slot);
	  temp = copy_to_reg (replace_rtx (addr, virtual_stack_vars_rtx,
					   hard_frame_pointer_rtx));
	  
	  /* Restore the stack pointer.  Note this uses fp just restored.  */
	  addr = p->nonlocal_goto_stack_level;
	  if (addr)
	    addr = replace_rtx (copy_rtx (addr),
				virtual_stack_vars_rtx,
				hard_frame_pointer_rtx);

	  emit_stack_restore (SAVE_NONLOCAL, addr, NULL_RTX);

	  /* Put in the static chain register the nonlocal label address.  */
	  emit_move_insn (static_chain_rtx, label_ref);
	  /* USE of hard_frame_pointer_rtx added for consistency; not clear if
	     really needed.  */
	  emit_insn (gen_rtx (USE, VOIDmode, hard_frame_pointer_rtx));
	  emit_insn (gen_rtx (USE, VOIDmode, stack_pointer_rtx));
	  emit_insn (gen_rtx (USE, VOIDmode, static_chain_rtx));
	  emit_indirect_jump (temp);
	}
     }
  else
    expand_goto_internal (label, label_rtx (label), NULL_RTX);
}

/* Generate RTL code for a `goto' statement with target label BODY.
   LABEL should be a LABEL_REF.
   LAST_INSN, if non-0, is the rtx we should consider as the last
   insn emitted (for the purposes of cleaning up a return).  */

static void
expand_goto_internal (body, label, last_insn)
     tree body;
     rtx label;
     rtx last_insn;
{
  struct nesting *block;
  rtx stack_level = 0;

  /* NOTICE!  If a bytecode instruction other than `jump' is needed,
     then the caller has to call bc_expand_goto_internal()
     directly. This is rather an exceptional case, and there aren't
     that many places where this is necessary. */
  if (output_bytecode)
    {
      expand_goto_internal (body, label, last_insn);
      return;
    }

  if (GET_CODE (label) != CODE_LABEL)
    abort ();

  /* If label has already been defined, we can tell now
     whether and how we must alter the stack level.  */

  if (PREV_INSN (label) != 0)
    {
      /* Find the innermost pending block that contains the label.
	 (Check containment by comparing insn-uids.)
	 Then restore the outermost stack level within that block,
	 and do cleanups of all blocks contained in it.  */
      for (block = block_stack; block; block = block->next)
	{
	  if (INSN_UID (block->data.block.first_insn) < INSN_UID (label))
	    break;
	  if (block->data.block.stack_level != 0)
	    stack_level = block->data.block.stack_level;
	  /* Execute the cleanups for blocks we are exiting.  */
	  if (block->data.block.cleanups != 0)
	    {
	      expand_cleanups (block->data.block.cleanups, NULL_TREE, 1, 1);
	      do_pending_stack_adjust ();
	    }
	}

      if (stack_level)
	{
	  /* Ensure stack adjust isn't done by emit_jump, as this would clobber
	     the stack pointer.  This one should be deleted as dead by flow. */
	  clear_pending_stack_adjust ();
	  do_pending_stack_adjust ();
	  emit_stack_restore (SAVE_BLOCK, stack_level, NULL_RTX);
	}

      if (body != 0 && DECL_TOO_LATE (body))
	error ("jump to `%s' invalidly jumps into binding contour",
	       IDENTIFIER_POINTER (DECL_NAME (body)));
    }
  /* Label not yet defined: may need to put this goto
     on the fixup list.  */
  else if (! expand_fixup (body, label, last_insn))
    {
      /* No fixup needed.  Record that the label is the target
	 of at least one goto that has no fixup.  */
      if (body != 0)
	TREE_ADDRESSABLE (body) = 1;
    }

  emit_jump (label);
}

/* Generate a jump with OPCODE to the given bytecode LABEL which is
   found within BODY. */

static void
bc_expand_goto_internal (opcode, label, body)
     enum bytecode_opcode opcode;
     struct bc_label *label;
     tree body;
{
  struct nesting *block;
  int stack_level = -1;

  /* If the label is defined, adjust the stack as necessary.
     If it's not defined, we have to push the reference on the
     fixup list. */

  if (label->defined)
    {

      /* Find the innermost pending block that contains the label.
	 (Check containment by comparing bytecode uids.)  Then restore the
	 outermost stack level within that block.  */

      for (block = block_stack; block; block = block->next)
	{
	  if (BYTECODE_BC_LABEL (block->data.block.first_insn)->uid < label->uid)
	    break;
	  if (block->data.block.bc_stack_level)
	    stack_level = block->data.block.bc_stack_level;

	  /* Execute the cleanups for blocks we are exiting.  */
	  if (block->data.block.cleanups != 0)
	    {
	      expand_cleanups (block->data.block.cleanups, NULL_TREE, 1, 1);
	      do_pending_stack_adjust ();
	    }
	}

      /* Restore the stack level. If we need to adjust the stack, we
	 must do so after the jump, since the jump may depend on
	 what's on the stack.  Thus, any stack-modifying conditional
	 jumps (these are the only ones that rely on what's on the
	 stack) go into the fixup list. */

      if (stack_level >= 0
	  && stack_depth != stack_level
	  && opcode != jump)

	bc_expand_fixup (opcode, label, stack_level);
      else
	{
	  if (stack_level >= 0)
	    bc_adjust_stack (stack_depth - stack_level);

	  if (body && DECL_BIT_FIELD (body))
	    error ("jump to `%s' invalidly jumps into binding contour",
		   IDENTIFIER_POINTER (DECL_NAME (body)));
	  
	  /* Emit immediate jump */
	  bc_emit_bytecode (opcode);
	  bc_emit_bytecode_labelref (label);
	  
#ifdef DEBUG_PRINT_CODE
	  fputc ('\n', stderr);
#endif
	}
    }
  else
    /* Put goto in the fixup list */
    bc_expand_fixup (opcode, label, stack_level);
}

/* Generate if necessary a fixup for a goto
   whose target label in tree structure (if any) is TREE_LABEL
   and whose target in rtl is RTL_LABEL.

   If LAST_INSN is nonzero, we pretend that the jump appears
   after insn LAST_INSN instead of at the current point in the insn stream.

   The fixup will be used later to insert insns just before the goto.
   Those insns will restore the stack level as appropriate for the
   target label, and will (in the case of C++) also invoke any object
   destructors which have to be invoked when we exit the scopes which
   are exited by the goto.

   Value is nonzero if a fixup is made.  */

static int
expand_fixup (tree_label, rtl_label, last_insn)
     tree tree_label;
     rtx rtl_label;
     rtx last_insn;
{
  struct nesting *block, *end_block;

  /* See if we can recognize which block the label will be output in.
     This is possible in some very common cases.
     If we succeed, set END_BLOCK to that block.
     Otherwise, set it to 0.  */

  if (cond_stack
      && (rtl_label == cond_stack->data.cond.endif_label
	  || rtl_label == cond_stack->data.cond.next_label))
    end_block = cond_stack;
  /* If we are in a loop, recognize certain labels which
     are likely targets.  This reduces the number of fixups
     we need to create.  */
  else if (loop_stack
      && (rtl_label == loop_stack->data.loop.start_label
	  || rtl_label == loop_stack->data.loop.end_label
	  || rtl_label == loop_stack->data.loop.continue_label))
    end_block = loop_stack;
  else
    end_block = 0;

  /* Now set END_BLOCK to the binding level to which we will return.  */

  if (end_block)
    {
      struct nesting *next_block = end_block->all;
      block = block_stack;

      /* First see if the END_BLOCK is inside the innermost binding level.
	 If so, then no cleanups or stack levels are relevant.  */
      while (next_block && next_block != block)
	next_block = next_block->all;

      if (next_block)
	return 0;

      /* Otherwise, set END_BLOCK to the innermost binding level
	 which is outside the relevant control-structure nesting.  */
      next_block = block_stack->next;
      for (block = block_stack; block != end_block; block = block->all)
	if (block == next_block)
	  next_block = next_block->next;
      end_block = next_block;
    }

  /* Does any containing block have a stack level or cleanups?
     If not, no fixup is needed, and that is the normal case
     (the only case, for standard C).  */
  for (block = block_stack; block != end_block; block = block->next)
    if (block->data.block.stack_level != 0
	|| block->data.block.cleanups != 0)
      break;

  if (block != end_block)
    {
      /* Ok, a fixup is needed.  Add a fixup to the list of such.  */
      struct goto_fixup *fixup
	= (struct goto_fixup *) oballoc (sizeof (struct goto_fixup));
      /* In case an old stack level is restored, make sure that comes
	 after any pending stack adjust.  */
      /* ?? If the fixup isn't to come at the present position,
	 doing the stack adjust here isn't useful.  Doing it with our
	 settings at that location isn't useful either.  Let's hope
	 someone does it!  */
      if (last_insn == 0)
	do_pending_stack_adjust ();
      fixup->target = tree_label;
      fixup->target_rtl = rtl_label;

      /* Create a BLOCK node and a corresponding matched set of
	 NOTE_INSN_BEGIN_BLOCK and NOTE_INSN_END_BLOCK notes at
	 this point.  The notes will encapsulate any and all fixup
	 code which we might later insert at this point in the insn
	 stream.  Also, the BLOCK node will be the parent (i.e. the
	 `SUPERBLOCK') of any other BLOCK nodes which we might create
	 later on when we are expanding the fixup code.  */

      {
        register rtx original_before_jump
          = last_insn ? last_insn : get_last_insn ();

        start_sequence ();
        pushlevel (0);
        fixup->before_jump = emit_note (NULL_PTR, NOTE_INSN_BLOCK_BEG);
        last_block_end_note = emit_note (NULL_PTR, NOTE_INSN_BLOCK_END);
        fixup->context = poplevel (1, 0, 0);  /* Create the BLOCK node now! */
        end_sequence ();
        emit_insns_after (fixup->before_jump, original_before_jump);
      }

      fixup->block_start_count = block_start_count;
      fixup->stack_level = 0;
      fixup->cleanup_list_list
	= (((block->data.block.outer_cleanups
#if 0
	     && block->data.block.outer_cleanups != empty_cleanup_list
#endif
	     )
	    || block->data.block.cleanups)
	   ? tree_cons (NULL_TREE, block->data.block.cleanups,
			block->data.block.outer_cleanups)
	   : 0);
      fixup->next = goto_fixup_chain;
      goto_fixup_chain = fixup;
    }

  return block != 0;
}


/* Generate bytecode jump with OPCODE to a fixup routine that links to LABEL.
   Make the fixup restore the stack level to STACK_LEVEL.  */

static void
bc_expand_fixup (opcode, label, stack_level)
     enum bytecode_opcode opcode;
     struct bc_label *label;
     int stack_level;
{
  struct goto_fixup *fixup
    = (struct goto_fixup *) oballoc (sizeof (struct goto_fixup));

  fixup->label  = bc_get_bytecode_label ();
  fixup->bc_target = label;
  fixup->bc_stack_level = stack_level;
  fixup->bc_handled = FALSE;

  fixup->next = goto_fixup_chain;
  goto_fixup_chain = fixup;

  /* Insert a jump to the fixup code */
  bc_emit_bytecode (opcode);
  bc_emit_bytecode_labelref (fixup->label);

#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif
}

/* Expand any needed fixups in the outputmost binding level of the
   function.  FIRST_INSN is the first insn in the function.  */

void
expand_fixups (first_insn)
     rtx first_insn;
{
  fixup_gotos (NULL_PTR, NULL_RTX, NULL_TREE, first_insn, 0);
}

/* When exiting a binding contour, process all pending gotos requiring fixups.
   THISBLOCK is the structure that describes the block being exited.
   STACK_LEVEL is the rtx for the stack level to restore exiting this contour.
   CLEANUP_LIST is a list of expressions to evaluate on exiting this contour.
   FIRST_INSN is the insn that began this contour.

   Gotos that jump out of this contour must restore the
   stack level and do the cleanups before actually jumping.

   DONT_JUMP_IN nonzero means report error there is a jump into this
   contour from before the beginning of the contour.
   This is also done if STACK_LEVEL is nonzero.  */

static void
fixup_gotos (thisblock, stack_level, cleanup_list, first_insn, dont_jump_in)
     struct nesting *thisblock;
     rtx stack_level;
     tree cleanup_list;
     rtx first_insn;
     int dont_jump_in;
{
  register struct goto_fixup *f, *prev;

  if (output_bytecode)
    {
      /* ??? The second arg is the bc stack level, which is not the same
	 as STACK_LEVEL.  I have no idea what should go here, so I'll
	 just pass 0.  */
      bc_fixup_gotos (thisblock, 0, cleanup_list, first_insn, dont_jump_in);
      return;
    }

  /* F is the fixup we are considering; PREV is the previous one.  */
  /* We run this loop in two passes so that cleanups of exited blocks
     are run first, and blocks that are exited are marked so
     afterwards.  */

  for (prev = 0, f = goto_fixup_chain; f; prev = f, f = f->next)
    {
      /* Test for a fixup that is inactive because it is already handled.  */
      if (f->before_jump == 0)
	{
	  /* Delete inactive fixup from the chain, if that is easy to do.  */
	  if (prev != 0)
	    prev->next = f->next;
	}
      /* Has this fixup's target label been defined?
	 If so, we can finalize it.  */
      else if (PREV_INSN (f->target_rtl) != 0)
	{
	  register rtx cleanup_insns;

	  /* Get the first non-label after the label
	     this goto jumps to.  If that's before this scope begins,
	     we don't have a jump into the scope.  */
	  rtx after_label = f->target_rtl;
	  while (after_label != 0 && GET_CODE (after_label) == CODE_LABEL)
	    after_label = NEXT_INSN (after_label);

	  /* If this fixup jumped into this contour from before the beginning
	     of this contour, report an error.  */
	  /* ??? Bug: this does not detect jumping in through intermediate
	     blocks that have stack levels or cleanups.
	     It detects only a problem with the innermost block
	     around the label.  */
	  if (f->target != 0
	      && (dont_jump_in || stack_level || cleanup_list)
	      /* If AFTER_LABEL is 0, it means the jump goes to the end
		 of the rtl, which means it jumps into this scope.  */
	      && (after_label == 0
		  || INSN_UID (first_insn) < INSN_UID (after_label))
	      && INSN_UID (first_insn) > INSN_UID (f->before_jump)
	      && ! DECL_REGISTER (f->target))
	    {
	      error_with_decl (f->target,
			       "label `%s' used before containing binding contour");
	      /* Prevent multiple errors for one label.  */
	      DECL_REGISTER (f->target) = 1;
	    }

	  /* We will expand the cleanups into a sequence of their own and
	     then later on we will attach this new sequence to the insn
	     stream just ahead of the actual jump insn.  */

	  start_sequence ();

	  /* Temporarily restore the lexical context where we will
	     logically be inserting the fixup code.  We do this for the
	     sake of getting the debugging information right.  */

	  pushlevel (0);
	  set_block (f->context);

	  /* Expand the cleanups for blocks this jump exits.  */
	  if (f->cleanup_list_list)
	    {
	      tree lists;
	      for (lists = f->cleanup_list_list; lists; lists = TREE_CHAIN (lists))
		/* Marked elements correspond to blocks that have been closed.
		   Do their cleanups.  */
		if (TREE_ADDRESSABLE (lists)
		    && TREE_VALUE (lists) != 0)
		  {
		    expand_cleanups (TREE_VALUE (lists), NULL_TREE, 1, 1);
		    /* Pop any pushes done in the cleanups,
		       in case function is about to return.  */
		    do_pending_stack_adjust ();
		  }
	    }

	  /* Restore stack level for the biggest contour that this
	     jump jumps out of.  */
	  if (f->stack_level)
	    emit_stack_restore (SAVE_BLOCK, f->stack_level, f->before_jump);

	  /* Finish up the sequence containing the insns which implement the
	     necessary cleanups, and then attach that whole sequence to the
	     insn stream just ahead of the actual jump insn.  Attaching it
	     at that point insures that any cleanups which are in fact
	     implicit C++ object destructions (which must be executed upon
	     leaving the block) appear (to the debugger) to be taking place
	     in an area of the generated code where the object(s) being
	     destructed are still "in scope".  */

	  cleanup_insns = get_insns ();
	  poplevel (1, 0, 0);

	  end_sequence ();
	  emit_insns_after (cleanup_insns, f->before_jump);


	  f->before_jump = 0;
	}
    }

  /* For any still-undefined labels, do the cleanups for this block now.
     We must do this now since items in the cleanup list may go out
     of scope when the block ends. */
  for (prev = 0, f = goto_fixup_chain; f; prev = f, f = f->next)
    if (f->before_jump != 0
	&& PREV_INSN (f->target_rtl) == 0
	/* Label has still not appeared.  If we are exiting a block with
	   a stack level to restore, that started before the fixup,
	   mark this stack level as needing restoration
	   when the fixup is later finalized.   */
	&& thisblock != 0
	/* Note: if THISBLOCK == 0 and we have a label that hasn't appeared, it
	   means the label is undefined.  That's erroneous, but possible.  */
	&& (thisblock->data.block.block_start_count
	    <= f->block_start_count))
      {
	tree lists = f->cleanup_list_list;
	rtx cleanup_insns;

	for (; lists; lists = TREE_CHAIN (lists))
	  /* If the following elt. corresponds to our containing block
	     then the elt. must be for this block.  */
	  if (TREE_CHAIN (lists) == thisblock->data.block.outer_cleanups)
	    {
	      start_sequence ();
	      pushlevel (0);
	      set_block (f->context);
	      expand_cleanups (TREE_VALUE (lists), NULL_TREE, 1, 1);
	      do_pending_stack_adjust ();
	      cleanup_insns = get_insns ();
	      poplevel (1, 0, 0);
	      end_sequence ();
	      f->before_jump
		= emit_insns_after (cleanup_insns, f->before_jump);

	      f->cleanup_list_list = TREE_CHAIN (lists);
	    }

	if (stack_level)
	  f->stack_level = stack_level;
      }
}


/* When exiting a binding contour, process all pending gotos requiring fixups.
   Note: STACK_DEPTH is not altered.

   The arguments are currently not used in the bytecode compiler, but we may
   need them one day for languages other than C.

   THISBLOCK is the structure that describes the block being exited.
   STACK_LEVEL is the rtx for the stack level to restore exiting this contour.
   CLEANUP_LIST is a list of expressions to evaluate on exiting this contour.
   FIRST_INSN is the insn that began this contour.

   Gotos that jump out of this contour must restore the
   stack level and do the cleanups before actually jumping.

   DONT_JUMP_IN nonzero means report error there is a jump into this
   contour from before the beginning of the contour.
   This is also done if STACK_LEVEL is nonzero.  */

static void
bc_fixup_gotos (thisblock, stack_level, cleanup_list, first_insn, dont_jump_in)
     struct nesting *thisblock;
     int stack_level;
     tree cleanup_list;
     rtx first_insn;
     int dont_jump_in;
{
  register struct goto_fixup *f, *prev;
  int saved_stack_depth;

  /* F is the fixup we are considering; PREV is the previous one.  */

  for (prev = 0, f = goto_fixup_chain; f; prev = f, f = f->next)
    {
      /* Test for a fixup that is inactive because it is already handled.  */
      if (f->before_jump == 0)
	{
	  /* Delete inactive fixup from the chain, if that is easy to do.  */
	  if (prev)
	    prev->next = f->next;
	}

      /* Emit code to restore the stack and continue */
      bc_emit_bytecode_labeldef (f->label);

      /* Save stack_depth across call, since bc_adjust_stack () will alter
         the perceived stack depth via the instructions generated. */

      if (f->bc_stack_level >= 0)
	{
	  saved_stack_depth = stack_depth;
	  bc_adjust_stack (stack_depth - f->bc_stack_level);
	  stack_depth = saved_stack_depth;
	}

      bc_emit_bytecode (jump);
      bc_emit_bytecode_labelref (f->bc_target);

#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif
    }

  goto_fixup_chain = NULL;
}

/* Generate RTL for an asm statement (explicit assembler code).
   BODY is a STRING_CST node containing the assembler code text,
   or an ADDR_EXPR containing a STRING_CST.  */

void
expand_asm (body)
     tree body;
{
  if (output_bytecode)
    {
      error ("`asm' is invalid when generating bytecode");
      return;
    }

  if (TREE_CODE (body) == ADDR_EXPR)
    body = TREE_OPERAND (body, 0);

  emit_insn (gen_rtx (ASM_INPUT, VOIDmode,
		      TREE_STRING_POINTER (body)));
  last_expr_type = 0;
}

/* Generate RTL for an asm statement with arguments.
   STRING is the instruction template.
   OUTPUTS is a list of output arguments (lvalues); INPUTS a list of inputs.
   Each output or input has an expression in the TREE_VALUE and
   a constraint-string in the TREE_PURPOSE.
   CLOBBERS is a list of STRING_CST nodes each naming a hard register
   that is clobbered by this insn.

   Not all kinds of lvalue that may appear in OUTPUTS can be stored directly.
   Some elements of OUTPUTS may be replaced with trees representing temporary
   values.  The caller should copy those temporary values to the originally
   specified lvalues.

   VOL nonzero means the insn is volatile; don't optimize it.  */

void
expand_asm_operands (string, outputs, inputs, clobbers, vol, filename, line)
     tree string, outputs, inputs, clobbers;
     int vol;
     char *filename;
     int line;
{
  rtvec argvec, constraints;
  rtx body;
  int ninputs = list_length (inputs);
  int noutputs = list_length (outputs);
  int nclobbers;
  tree tail;
  register int i;
  /* Vector of RTX's of evaluated output operands.  */
  rtx *output_rtx = (rtx *) alloca (noutputs * sizeof (rtx));
  /* The insn we have emitted.  */
  rtx insn;

  if (output_bytecode)
    {
      error ("`asm' is invalid when generating bytecode");
      return;
    }

  /* Count the number of meaningful clobbered registers, ignoring what
     we would ignore later.  */
  nclobbers = 0;
  for (tail = clobbers; tail; tail = TREE_CHAIN (tail))
    {
      char *regname = TREE_STRING_POINTER (TREE_VALUE (tail));
      i = decode_reg_name (regname);
      if (i >= 0 || i == -4)
	++nclobbers;
      else if (i == -2)
	error ("unknown register name `%s' in `asm'", regname);
    }

  last_expr_type = 0;

  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    {
      tree val = TREE_VALUE (tail);
      tree type = TREE_TYPE (val);
      tree val1;
      int j;
      int found_equal = 0;
      int allows_reg = 0;

      /* If there's an erroneous arg, emit no insn.  */
      if (TREE_TYPE (val) == error_mark_node)
	return;

      /* Make sure constraint has `=' and does not have `+'.  Also, see
	 if it allows any register.  Be liberal on the latter test, since
	 the worst that happens if we get it wrong is we issue an error
	 message.  */

      for (j = 0; j < TREE_STRING_LENGTH (TREE_PURPOSE (tail)) - 1; j++)
	switch (TREE_STRING_POINTER (TREE_PURPOSE (tail))[j])
	  {
	  case '+':
	    error ("output operand constraint contains `+'");
	    return;

	  case '=':
	    found_equal = 1;
	    break;

	  case '?':  case '!':  case '*':  case '%':  case '&':
	  case '0':  case '1':  case '2':  case '3':  case '4':
	  case 'V':  case 'm':  case 'o':  case '<':  case '>':
	  case 'E':  case 'F':  case 'G':  case 'H':  case 'X':
	  case 's':  case 'i':  case 'n':
	  case 'I':  case 'J':  case 'K':  case 'L':  case 'M':
	  case 'N':  case 'O':  case 'P':  case ',':
#ifdef EXTRA_CONSTRAINT
	  case 'Q':  case 'R':  case 'S':  case 'T':  case 'U':
#endif
	    break;

	  case 'p':  case 'g':  case 'r':
	  default:
	    allows_reg = 1;
	    break;
	  }

      if (! found_equal)
	{
	  error ("output operand constraint lacks `='");
	  return;
	}

      /* If an output operand is not a decl or indirect ref and our constraint
	 allows a register, make a temporary to act as an intermediate.
	 Make the asm insn write into that, then our caller will copy it to
	 the real output operand.  Likewise for promoted variables.  */

      if (TREE_CODE (val) == INDIRECT_REF
	  || (TREE_CODE_CLASS (TREE_CODE (val)) == 'd'
	      && ! (GET_CODE (DECL_RTL (val)) == REG
		    && GET_MODE (DECL_RTL (val)) != TYPE_MODE (type)))
	  || ! allows_reg)
	{
	  if (! allows_reg)
	    mark_addressable (TREE_VALUE (tail));

	  output_rtx[i]
	    = expand_expr (TREE_VALUE (tail), NULL_RTX, VOIDmode, 0);

	  if (! allows_reg && GET_CODE (output_rtx[i]) != MEM)
	    error ("output number %d not directly addressable", i);
	}
      else
	{
	  if (TYPE_MODE (type) == BLKmode)
	    {
	      output_rtx[i] = assign_stack_temp (BLKmode,
						 int_size_in_bytes (type), 0);
	      MEM_IN_STRUCT_P (output_rtx[i]) = AGGREGATE_TYPE_P (type);
	    }
	  else
	    output_rtx[i] = gen_reg_rtx (TYPE_MODE (type));

	  TREE_VALUE (tail) = make_tree (type, output_rtx[i]);
	}
    }

  if (ninputs + noutputs > MAX_RECOG_OPERANDS)
    {
      error ("more than %d operands in `asm'", MAX_RECOG_OPERANDS);
      return;
    }

  /* Make vectors for the expression-rtx and constraint strings.  */

  argvec = rtvec_alloc (ninputs);
  constraints = rtvec_alloc (ninputs);

  body = gen_rtx (ASM_OPERANDS, VOIDmode,
		  TREE_STRING_POINTER (string), "", 0, argvec, constraints,
		  filename, line);
  MEM_VOLATILE_P (body) = vol;

  /* Eval the inputs and put them into ARGVEC.
     Put their constraints into ASM_INPUTs and store in CONSTRAINTS.  */

  i = 0;
  for (tail = inputs; tail; tail = TREE_CHAIN (tail))
    {
      int j;
      int allows_reg = 0;

      /* If there's an erroneous arg, emit no insn,
	 because the ASM_INPUT would get VOIDmode
	 and that could cause a crash in reload.  */
      if (TREE_TYPE (TREE_VALUE (tail)) == error_mark_node)
	return;
      if (TREE_PURPOSE (tail) == NULL_TREE)
	{
	  error ("hard register `%s' listed as input operand to `asm'",
		 TREE_STRING_POINTER (TREE_VALUE (tail)) );
	  return;
	}

      /* Make sure constraint has neither `=' nor `+'.  */

      for (j = 0; j < TREE_STRING_LENGTH (TREE_PURPOSE (tail)) - 1; j++)
	switch (TREE_STRING_POINTER (TREE_PURPOSE (tail))[j])
	  {
	  case '+':   case '=':
	    error ("input operand constraint contains `%c'",
		   TREE_STRING_POINTER (TREE_PURPOSE (tail))[j]);
	    return;

	  case '?':  case '!':  case '*':  case '%':  case '&':
	  case 'V':  case 'm':  case 'o':  case '<':  case '>':
	  case 'E':  case 'F':  case 'G':  case 'H':  case 'X':
	  case 's':  case 'i':  case 'n':
	  case 'I':  case 'J':  case 'K':  case 'L':  case 'M':
	  case 'N':  case 'O':  case 'P':  case ',':
#ifdef EXTRA_CONSTRAINT
	  case 'Q':  case 'R':  case 'S':  case 'T':  case 'U':
#endif
	    break;

	  case '0':  case '1':  case '2':  case '3':  case '4':
	  case 'p':  case 'g':  case 'r':
	  default:
	    allows_reg = 1;
	    break;
	  }

      if (! allows_reg)
	mark_addressable (TREE_VALUE (tail));

      XVECEXP (body, 3, i)      /* argvec */
	= expand_expr (TREE_VALUE (tail), NULL_RTX, VOIDmode, 0);
      if (CONSTANT_P (XVECEXP (body, 3, i))
	  && ! general_operand (XVECEXP (body, 3, i),
				TYPE_MODE (TREE_TYPE (TREE_VALUE (tail)))))
	{
	  if (allows_reg)
	    XVECEXP (body, 3, i)
	      = force_reg (TYPE_MODE (TREE_TYPE (TREE_VALUE (tail))),
			   XVECEXP (body, 3, i));
	  else
	    XVECEXP (body, 3, i)
	      = force_const_mem (TYPE_MODE (TREE_TYPE (TREE_VALUE (tail))),
				 XVECEXP (body, 3, i));
	}

      if (! allows_reg
	  && (GET_CODE (XVECEXP (body, 3, i)) == REG
	      || GET_CODE (XVECEXP (body, 3, i)) == SUBREG
	      || GET_CODE (XVECEXP (body, 3, i)) == CONCAT))
	{
	  tree type = TREE_TYPE (TREE_VALUE (tail));
	  rtx memloc = assign_stack_temp (TYPE_MODE (type),
					  int_size_in_bytes (type), 1);

	  MEM_IN_STRUCT_P (memloc) = AGGREGATE_TYPE_P (type);
	  emit_move_insn (memloc, XVECEXP (body, 3, i));
	  XVECEXP (body, 3, i) = memloc;
	}
	  
      XVECEXP (body, 4, i)      /* constraints */
	= gen_rtx (ASM_INPUT, TYPE_MODE (TREE_TYPE (TREE_VALUE (tail))),
		   TREE_STRING_POINTER (TREE_PURPOSE (tail)));
      i++;
    }

  /* Protect all the operands from the queue,
     now that they have all been evaluated.  */

  for (i = 0; i < ninputs; i++)
    XVECEXP (body, 3, i) = protect_from_queue (XVECEXP (body, 3, i), 0);

  for (i = 0; i < noutputs; i++)
    output_rtx[i] = protect_from_queue (output_rtx[i], 1);

  /* Now, for each output, construct an rtx
     (set OUTPUT (asm_operands INSN OUTPUTNUMBER OUTPUTCONSTRAINT
			       ARGVEC CONSTRAINTS))
     If there is more than one, put them inside a PARALLEL.  */

  if (noutputs == 1 && nclobbers == 0)
    {
      XSTR (body, 1) = TREE_STRING_POINTER (TREE_PURPOSE (outputs));
      insn = emit_insn (gen_rtx (SET, VOIDmode, output_rtx[0], body));
    }
  else if (noutputs == 0 && nclobbers == 0)
    {
      /* No output operands: put in a raw ASM_OPERANDS rtx.  */
      insn = emit_insn (body);
    }
  else
    {
      rtx obody = body;
      int num = noutputs;
      if (num == 0) num = 1;
      body = gen_rtx (PARALLEL, VOIDmode, rtvec_alloc (num + nclobbers));

      /* For each output operand, store a SET.  */

      for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
	{
	  XVECEXP (body, 0, i)
	    = gen_rtx (SET, VOIDmode,
		       output_rtx[i],
		       gen_rtx (ASM_OPERANDS, VOIDmode,
				TREE_STRING_POINTER (string),
				TREE_STRING_POINTER (TREE_PURPOSE (tail)),
				i, argvec, constraints,
				filename, line));
	  MEM_VOLATILE_P (SET_SRC (XVECEXP (body, 0, i))) = vol;
	}

      /* If there are no outputs (but there are some clobbers)
	 store the bare ASM_OPERANDS into the PARALLEL.  */

      if (i == 0)
	XVECEXP (body, 0, i++) = obody;

      /* Store (clobber REG) for each clobbered register specified.  */

      for (tail = clobbers; tail; tail = TREE_CHAIN (tail))
	{
	  char *regname = TREE_STRING_POINTER (TREE_VALUE (tail));
	  int j = decode_reg_name (regname);

	  if (j < 0)
	    {
	      if (j == -3)	/* `cc', which is not a register */
		continue;

	      if (j == -4)	/* `memory', don't cache memory across asm */
		{
		  XVECEXP (body, 0, i++)
		    = gen_rtx (CLOBBER, VOIDmode,
			       gen_rtx (MEM, BLKmode,
					gen_rtx (SCRATCH, VOIDmode, 0)));
		  continue;
		}

	      /* Ignore unknown register, error already signalled.  */
	      continue;
	    }

	  /* Use QImode since that's guaranteed to clobber just one reg.  */
	  XVECEXP (body, 0, i++)
	    = gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, QImode, j));
	}

      insn = emit_insn (body);
    }

  free_temp_slots ();
}

/* Generate RTL to evaluate the expression EXP
   and remember it in case this is the VALUE in a ({... VALUE; }) constr.  */

void
expand_expr_stmt (exp)
     tree exp;
{
  if (output_bytecode)
    {
      int org_stack_depth = stack_depth;

      bc_expand_expr (exp);

      /* Restore stack depth */
      if (stack_depth < org_stack_depth)
	abort ();
      
      bc_emit_instruction (drop);

      last_expr_type = TREE_TYPE (exp);
      return;
    }

  /* If -W, warn about statements with no side effects,
     except for an explicit cast to void (e.g. for assert()), and
     except inside a ({...}) where they may be useful.  */
  if (expr_stmts_for_value == 0 && exp != error_mark_node)
    {
      if (! TREE_SIDE_EFFECTS (exp) && (extra_warnings || warn_unused)
	  && !(TREE_CODE (exp) == CONVERT_EXPR
	       && TREE_TYPE (exp) == void_type_node))
	warning_with_file_and_line (emit_filename, emit_lineno,
				    "statement with no effect");
      else if (warn_unused)
	warn_if_unused_value (exp);
    }

  /* If EXP is of function type and we are expanding statements for
     value, convert it to pointer-to-function.  */
  if (expr_stmts_for_value && TREE_CODE (TREE_TYPE (exp)) == FUNCTION_TYPE)
    exp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (exp)), exp);

  last_expr_type = TREE_TYPE (exp);
  if (! flag_syntax_only)
    last_expr_value = expand_expr (exp,
				   (expr_stmts_for_value
				    ? NULL_RTX : const0_rtx),
				   VOIDmode, 0);

  /* If all we do is reference a volatile value in memory,
     copy it to a register to be sure it is actually touched.  */
  if (last_expr_value != 0 && GET_CODE (last_expr_value) == MEM
      && TREE_THIS_VOLATILE (exp))
    {
      if (TYPE_MODE (TREE_TYPE (exp)) == VOIDmode)
	;
      else if (TYPE_MODE (TREE_TYPE (exp)) != BLKmode)
	copy_to_reg (last_expr_value);
      else
	{
	  rtx lab = gen_label_rtx ();
	  
	  /* Compare the value with itself to reference it.  */
	  emit_cmp_insn (last_expr_value, last_expr_value, EQ,
			 expand_expr (TYPE_SIZE (last_expr_type),
				      NULL_RTX, VOIDmode, 0),
			 BLKmode, 0,
			 TYPE_ALIGN (last_expr_type) / BITS_PER_UNIT);
	  emit_jump_insn ((*bcc_gen_fctn[(int) EQ]) (lab));
	  emit_label (lab);
	}
    }

  /* If this expression is part of a ({...}) and is in memory, we may have
     to preserve temporaries.  */
  preserve_temp_slots (last_expr_value);

  /* Free any temporaries used to evaluate this expression.  Any temporary
     used as a result of this expression will already have been preserved
     above.  */
  free_temp_slots ();

  emit_queue ();
}

/* Warn if EXP contains any computations whose results are not used.
   Return 1 if a warning is printed; 0 otherwise.  */

int
warn_if_unused_value (exp)
     tree exp;
{
  if (TREE_USED (exp))
    return 0;

  switch (TREE_CODE (exp))
    {
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case MODIFY_EXPR:
    case INIT_EXPR:
    case TARGET_EXPR:
    case CALL_EXPR:
    case METHOD_CALL_EXPR:
    case RTL_EXPR:
    case WITH_CLEANUP_EXPR:
    case EXIT_EXPR:
      /* We don't warn about COND_EXPR because it may be a useful
	 construct if either arm contains a side effect.  */
    case COND_EXPR:
      return 0;

    case BIND_EXPR:
      /* For a binding, warn if no side effect within it.  */
      return warn_if_unused_value (TREE_OPERAND (exp, 1));

    case SAVE_EXPR:
      return warn_if_unused_value (TREE_OPERAND (exp, 1));

    case TRUTH_ORIF_EXPR:
    case TRUTH_ANDIF_EXPR:
      /* In && or ||, warn if 2nd operand has no side effect.  */
      return warn_if_unused_value (TREE_OPERAND (exp, 1));

    case COMPOUND_EXPR:
      if (TREE_NO_UNUSED_WARNING (exp))
	return 0;
      if (warn_if_unused_value (TREE_OPERAND (exp, 0)))
	return 1;
      /* Let people do `(foo (), 0)' without a warning.  */
      if (TREE_CONSTANT (TREE_OPERAND (exp, 1)))
	return 0;
      return warn_if_unused_value (TREE_OPERAND (exp, 1));

    case NOP_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      /* Don't warn about values cast to void.  */
      if (TREE_TYPE (exp) == void_type_node)
	return 0;
      /* Don't warn about conversions not explicit in the user's program.  */
      if (TREE_NO_UNUSED_WARNING (exp))
	return 0;
      /* Assignment to a cast usually results in a cast of a modify.
	 Don't complain about that.  There can be an arbitrary number of
	 casts before the modify, so we must loop until we find the first
	 non-cast expression and then test to see if that is a modify.  */
      {
	tree tem = TREE_OPERAND (exp, 0);

	while (TREE_CODE (tem) == CONVERT_EXPR || TREE_CODE (tem) == NOP_EXPR)
	  tem = TREE_OPERAND (tem, 0);

	if (TREE_CODE (tem) == MODIFY_EXPR || TREE_CODE (tem) == INIT_EXPR
	    || TREE_CODE (tem) == CALL_EXPR)
	  return 0;
      }
      goto warn;

    case INDIRECT_REF:
      /* Don't warn about automatic dereferencing of references, since
	 the user cannot control it.  */
      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == REFERENCE_TYPE)
	return warn_if_unused_value (TREE_OPERAND (exp, 0));
      /* ... fall through ... */
      
    default:
      /* Referencing a volatile value is a side effect, so don't warn.  */
      if ((TREE_CODE_CLASS (TREE_CODE (exp)) == 'd'
	   || TREE_CODE_CLASS (TREE_CODE (exp)) == 'r')
	  && TREE_THIS_VOLATILE (exp))
	return 0;
    warn:
      warning_with_file_and_line (emit_filename, emit_lineno,
				  "value computed is not used");
      return 1;
    }
}

/* Clear out the memory of the last expression evaluated.  */

void
clear_last_expr ()
{
  last_expr_type = 0;
}

/* Begin a statement which will return a value.
   Return the RTL_EXPR for this statement expr.
   The caller must save that value and pass it to expand_end_stmt_expr.  */

tree
expand_start_stmt_expr ()
{
  int momentary;
  tree t;

  /* When generating bytecode just note down the stack depth */
  if (output_bytecode)
    return (build_int_2 (stack_depth, 0));

  /* Make the RTL_EXPR node temporary, not momentary,
     so that rtl_expr_chain doesn't become garbage.  */
  momentary = suspend_momentary ();
  t = make_node (RTL_EXPR);
  resume_momentary (momentary);
  do_pending_stack_adjust ();
  start_sequence_for_rtl_expr (t);
  NO_DEFER_POP;
  expr_stmts_for_value++;
  return t;
}

/* Restore the previous state at the end of a statement that returns a value.
   Returns a tree node representing the statement's value and the
   insns to compute the value.

   The nodes of that expression have been freed by now, so we cannot use them.
   But we don't want to do that anyway; the expression has already been
   evaluated and now we just want to use the value.  So generate a RTL_EXPR
   with the proper type and RTL value.

   If the last substatement was not an expression,
   return something with type `void'.  */

tree
expand_end_stmt_expr (t)
     tree t;
{
  if (output_bytecode)
    {
      int i;
      tree t;
      
      
      /* At this point, all expressions have been evaluated in order.
	 However, all expression values have been popped when evaluated,
	 which means we have to recover the last expression value.  This is
	 the last value removed by means of a `drop' instruction.  Instead
	 of adding code to inhibit dropping the last expression value, it
	 is here recovered by undoing the `drop'.  Since `drop' is
	 equivalent to `adjustackSI [1]', it can be undone with `adjstackSI
	 [-1]'. */
      
      bc_adjust_stack (-1);
      
      if (!last_expr_type)
	last_expr_type = void_type_node;
      
      t = make_node (RTL_EXPR);
      TREE_TYPE (t) = last_expr_type;
      RTL_EXPR_RTL (t) = NULL;
      RTL_EXPR_SEQUENCE (t) = NULL;
      
      /* Don't consider deleting this expr or containing exprs at tree level.  */
      TREE_THIS_VOLATILE (t) = 1;
      
      last_expr_type = 0;
      return t;
    }

  OK_DEFER_POP;

  if (last_expr_type == 0)
    {
      last_expr_type = void_type_node;
      last_expr_value = const0_rtx;
    }
  else if (last_expr_value == 0)
    /* There are some cases where this can happen, such as when the
       statement is void type.  */
    last_expr_value = const0_rtx;
  else if (GET_CODE (last_expr_value) != REG && ! CONSTANT_P (last_expr_value))
    /* Remove any possible QUEUED.  */
    last_expr_value = protect_from_queue (last_expr_value, 0);

  emit_queue ();

  TREE_TYPE (t) = last_expr_type;
  RTL_EXPR_RTL (t) = last_expr_value;
  RTL_EXPR_SEQUENCE (t) = get_insns ();

  rtl_expr_chain = tree_cons (NULL_TREE, t, rtl_expr_chain);

  end_sequence ();

  /* Don't consider deleting this expr or containing exprs at tree level.  */
  TREE_SIDE_EFFECTS (t) = 1;
  /* Propagate volatility of the actual RTL expr.  */
  TREE_THIS_VOLATILE (t) = volatile_refs_p (last_expr_value);

  last_expr_type = 0;
  expr_stmts_for_value--;

  return t;
}

/* Generate RTL for the start of an if-then.  COND is the expression
   whose truth should be tested.

   If EXITFLAG is nonzero, this conditional is visible to
   `exit_something'.  */

void
expand_start_cond (cond, exitflag)
     tree cond;
     int exitflag;
{
  struct nesting *thiscond = ALLOC_NESTING ();

  /* Make an entry on cond_stack for the cond we are entering.  */

  thiscond->next = cond_stack;
  thiscond->all = nesting_stack;
  thiscond->depth = ++nesting_depth;
  thiscond->data.cond.next_label = gen_label_rtx ();
  /* Before we encounter an `else', we don't need a separate exit label
     unless there are supposed to be exit statements
     to exit this conditional.  */
  thiscond->exit_label = exitflag ? gen_label_rtx () : 0;
  thiscond->data.cond.endif_label = thiscond->exit_label;
  cond_stack = thiscond;
  nesting_stack = thiscond;

  if (output_bytecode)
    bc_expand_start_cond (cond, exitflag);
  else
    do_jump (cond, thiscond->data.cond.next_label, NULL_RTX);
}

/* Generate RTL between then-clause and the elseif-clause
   of an if-then-elseif-....  */

void
expand_start_elseif (cond)
     tree cond;
{
  if (cond_stack->data.cond.endif_label == 0)
    cond_stack->data.cond.endif_label = gen_label_rtx ();
  emit_jump (cond_stack->data.cond.endif_label);
  emit_label (cond_stack->data.cond.next_label);
  cond_stack->data.cond.next_label = gen_label_rtx ();
  do_jump (cond, cond_stack->data.cond.next_label, NULL_RTX);
}

/* Generate RTL between the then-clause and the else-clause
   of an if-then-else.  */

void
expand_start_else ()
{
  if (cond_stack->data.cond.endif_label == 0)
    cond_stack->data.cond.endif_label = gen_label_rtx ();

  if (output_bytecode)
    {
      bc_expand_start_else ();
      return;
    }

  emit_jump (cond_stack->data.cond.endif_label);
  emit_label (cond_stack->data.cond.next_label);
  cond_stack->data.cond.next_label = 0;  /* No more _else or _elseif calls. */
}

/* After calling expand_start_else, turn this "else" into an "else if"
   by providing another condition.  */

void
expand_elseif (cond)
     tree cond;
{
  cond_stack->data.cond.next_label = gen_label_rtx ();
  do_jump (cond, cond_stack->data.cond.next_label, NULL_RTX);
}

/* Generate RTL for the end of an if-then.
   Pop the record for it off of cond_stack.  */

void
expand_end_cond ()
{
  struct nesting *thiscond = cond_stack;

  if (output_bytecode)
    bc_expand_end_cond ();
  else
    {
      do_pending_stack_adjust ();
      if (thiscond->data.cond.next_label)
	emit_label (thiscond->data.cond.next_label);
      if (thiscond->data.cond.endif_label)
	emit_label (thiscond->data.cond.endif_label);
    }

  POPSTACK (cond_stack);
  last_expr_type = 0;
}


/* Generate code for the start of an if-then.  COND is the expression
   whose truth is to be tested; if EXITFLAG is nonzero this conditional
   is to be visible to exit_something.  It is assumed that the caller
   has pushed the previous context on the cond stack. */

static void
bc_expand_start_cond (cond, exitflag)
     tree cond;
     int exitflag;
{
  struct nesting *thiscond = cond_stack;

  thiscond->data.case_stmt.nominal_type = cond;
  if (! exitflag)
    thiscond->exit_label = gen_label_rtx ();
  bc_expand_expr (cond);
  bc_emit_bytecode (xjumpifnot);
  bc_emit_bytecode_labelref (BYTECODE_BC_LABEL (thiscond->exit_label));

#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif
}

/* Generate the label for the end of an if with
   no else- clause.  */

static void
bc_expand_end_cond ()
{
  struct nesting *thiscond = cond_stack;

  bc_emit_bytecode_labeldef (BYTECODE_BC_LABEL (thiscond->exit_label));
}

/* Generate code for the start of the else- clause of
   an if-then-else.  */

static void
bc_expand_start_else ()
{
  struct nesting *thiscond = cond_stack;

  thiscond->data.cond.endif_label = thiscond->exit_label;
  thiscond->exit_label = gen_label_rtx ();
  bc_emit_bytecode (jump);
  bc_emit_bytecode_labelref (BYTECODE_BC_LABEL (thiscond->exit_label));

#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif

  bc_emit_bytecode_labeldef (BYTECODE_BC_LABEL (thiscond->data.cond.endif_label));
}

/* Generate RTL for the start of a loop.  EXIT_FLAG is nonzero if this
   loop should be exited by `exit_something'.  This is a loop for which
   `expand_continue' will jump to the top of the loop.

   Make an entry on loop_stack to record the labels associated with
   this loop.  */

struct nesting *
expand_start_loop (exit_flag)
     int exit_flag;
{
  register struct nesting *thisloop = ALLOC_NESTING ();

  /* Make an entry on loop_stack for the loop we are entering.  */

  thisloop->next = loop_stack;
  thisloop->all = nesting_stack;
  thisloop->depth = ++nesting_depth;
  thisloop->data.loop.start_label = gen_label_rtx ();
  thisloop->data.loop.end_label = gen_label_rtx ();
  thisloop->data.loop.alt_end_label = 0;
  thisloop->data.loop.continue_label = thisloop->data.loop.start_label;
  thisloop->exit_label = exit_flag ? thisloop->data.loop.end_label : 0;
  loop_stack = thisloop;
  nesting_stack = thisloop;

  if (output_bytecode)
    {
      bc_emit_bytecode_labeldef (BYTECODE_BC_LABEL (thisloop->data.loop.start_label));
      return thisloop;
    }

  do_pending_stack_adjust ();
  emit_queue ();
  emit_note (NULL_PTR, NOTE_INSN_LOOP_BEG);
  emit_label (thisloop->data.loop.start_label);

  return thisloop;
}

/* Like expand_start_loop but for a loop where the continuation point
   (for expand_continue_loop) will be specified explicitly.  */

struct nesting *
expand_start_loop_continue_elsewhere (exit_flag)
     int exit_flag;
{
  struct nesting *thisloop = expand_start_loop (exit_flag);
  loop_stack->data.loop.continue_label = gen_label_rtx ();
  return thisloop;
}

/* Specify the continuation point for a loop started with
   expand_start_loop_continue_elsewhere.
   Use this at the point in the code to which a continue statement
   should jump.  */

void
expand_loop_continue_here ()
{
  if (output_bytecode)
    {
      bc_emit_bytecode_labeldef (BYTECODE_BC_LABEL (loop_stack->data.loop.continue_label));
      return;
    }
  do_pending_stack_adjust ();
  emit_note (NULL_PTR, NOTE_INSN_LOOP_CONT);
  emit_label (loop_stack->data.loop.continue_label);
}

/* End a loop.  */

static void
bc_expand_end_loop ()
{
  struct nesting *thisloop = loop_stack;

  bc_emit_bytecode (jump);
  bc_emit_bytecode_labelref (BYTECODE_BC_LABEL (thisloop->data.loop.start_label));

#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif

  bc_emit_bytecode_labeldef (BYTECODE_BC_LABEL (thisloop->exit_label));
  POPSTACK (loop_stack);
  last_expr_type = 0;
}


/* Finish a loop.  Generate a jump back to the top and the loop-exit label.
   Pop the block off of loop_stack.  */

void
expand_end_loop ()
{
  register rtx insn;
  register rtx start_label;
  rtx last_test_insn = 0;
  int num_insns = 0;
    
  if (output_bytecode)
    {
      bc_expand_end_loop ();
      return;
    }

  insn = get_last_insn ();
  start_label = loop_stack->data.loop.start_label;

  /* Mark the continue-point at the top of the loop if none elsewhere.  */
  if (start_label == loop_stack->data.loop.continue_label)
    emit_note_before (NOTE_INSN_LOOP_CONT, start_label);

  do_pending_stack_adjust ();

  /* If optimizing, perhaps reorder the loop.  If the loop
     starts with a conditional exit, roll that to the end
     where it will optimize together with the jump back.

     We look for the last conditional branch to the exit that we encounter
     before hitting 30 insns or a CALL_INSN.  If we see an unconditional
     branch to the exit first, use it.

     We must also stop at NOTE_INSN_BLOCK_BEG and NOTE_INSN_BLOCK_END notes
     because moving them is not valid.  */

  if (optimize
      &&
      ! (GET_CODE (insn) == JUMP_INSN
	 && GET_CODE (PATTERN (insn)) == SET
	 && SET_DEST (PATTERN (insn)) == pc_rtx
	 && GET_CODE (SET_SRC (PATTERN (insn))) == IF_THEN_ELSE))
    {
      /* Scan insns from the top of the loop looking for a qualified
	 conditional exit.  */
      for (insn = NEXT_INSN (loop_stack->data.loop.start_label); insn;
	   insn = NEXT_INSN (insn))
	{
	  if (GET_CODE (insn) == CALL_INSN || GET_CODE (insn) == CODE_LABEL)
	    break;

	  if (GET_CODE (insn) == NOTE
	      && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG
		  || NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END))
	    break;

	  if (GET_CODE (insn) == JUMP_INSN || GET_CODE (insn) == INSN)
	    num_insns++;

	  if (last_test_insn && num_insns > 30)
	    break;

	  if (GET_CODE (insn) == JUMP_INSN && GET_CODE (PATTERN (insn)) == SET
	      && SET_DEST (PATTERN (insn)) == pc_rtx
	      && GET_CODE (SET_SRC (PATTERN (insn))) == IF_THEN_ELSE
	      && ((GET_CODE (XEXP (SET_SRC (PATTERN (insn)), 1)) == LABEL_REF
		   && ((XEXP (XEXP (SET_SRC (PATTERN (insn)), 1), 0)
			== loop_stack->data.loop.end_label)
		       || (XEXP (XEXP (SET_SRC (PATTERN (insn)), 1), 0)
			   == loop_stack->data.loop.alt_end_label)))
		  || (GET_CODE (XEXP (SET_SRC (PATTERN (insn)), 2)) == LABEL_REF
		      && ((XEXP (XEXP (SET_SRC (PATTERN (insn)), 2), 0)
			   == loop_stack->data.loop.end_label)
			  || (XEXP (XEXP (SET_SRC (PATTERN (insn)), 2), 0)
			      == loop_stack->data.loop.alt_end_label)))))
	    last_test_insn = insn;

	  if (last_test_insn == 0 && GET_CODE (insn) == JUMP_INSN
	      && GET_CODE (PATTERN (insn)) == SET
	      && SET_DEST (PATTERN (insn)) == pc_rtx
	      && GET_CODE (SET_SRC (PATTERN (insn))) == LABEL_REF
	      && ((XEXP (SET_SRC (PATTERN (insn)), 0)
		   == loop_stack->data.loop.end_label)
		  || (XEXP (SET_SRC (PATTERN (insn)), 0)
		      == loop_stack->data.loop.alt_end_label)))
	    /* Include BARRIER.  */
	    last_test_insn = NEXT_INSN (insn);
	}

      if (last_test_insn != 0 && last_test_insn != get_last_insn ())
	{
	  /* We found one.  Move everything from there up
	     to the end of the loop, and add a jump into the loop
	     to jump to there.  */
	  register rtx newstart_label = gen_label_rtx ();
	  register rtx start_move = start_label;

	  /* If the start label is preceded by a NOTE_INSN_LOOP_CONT note,
	     then we want to move this note also.  */
	  if (GET_CODE (PREV_INSN (start_move)) == NOTE
	      && (NOTE_LINE_NUMBER (PREV_INSN (start_move))
		  == NOTE_INSN_LOOP_CONT))
	    start_move = PREV_INSN (start_move);

	  emit_label_after (newstart_label, PREV_INSN (start_move));
	  reorder_insns (start_move, last_test_insn, get_last_insn ());
	  emit_jump_insn_after (gen_jump (start_label),
				PREV_INSN (newstart_label));
	  emit_barrier_after (PREV_INSN (newstart_label));
	  start_label = newstart_label;
	}
    }

  emit_jump (start_label);
  emit_note (NULL_PTR, NOTE_INSN_LOOP_END);
  emit_label (loop_stack->data.loop.end_label);

  POPSTACK (loop_stack);

  last_expr_type = 0;
}

/* Generate a jump to the current loop's continue-point.
   This is usually the top of the loop, but may be specified
   explicitly elsewhere.  If not currently inside a loop,
   return 0 and do nothing; caller will print an error message.  */

int
expand_continue_loop (whichloop)
     struct nesting *whichloop;
{
  last_expr_type = 0;
  if (whichloop == 0)
    whichloop = loop_stack;
  if (whichloop == 0)
    return 0;
  expand_goto_internal (NULL_TREE, whichloop->data.loop.continue_label,
			NULL_RTX);
  return 1;
}

/* Generate a jump to exit the current loop.  If not currently inside a loop,
   return 0 and do nothing; caller will print an error message.  */

int
expand_exit_loop (whichloop)
     struct nesting *whichloop;
{
  last_expr_type = 0;
  if (whichloop == 0)
    whichloop = loop_stack;
  if (whichloop == 0)
    return 0;
  expand_goto_internal (NULL_TREE, whichloop->data.loop.end_label, NULL_RTX);
  return 1;
}

/* Generate a conditional jump to exit the current loop if COND
   evaluates to zero.  If not currently inside a loop,
   return 0 and do nothing; caller will print an error message.  */

int
expand_exit_loop_if_false (whichloop, cond)
     struct nesting *whichloop;
     tree cond;
{
  last_expr_type = 0;
  if (whichloop == 0)
    whichloop = loop_stack;
  if (whichloop == 0)
    return 0;
  if (output_bytecode)
    {
      bc_expand_expr (cond);
      bc_expand_goto_internal (xjumpifnot,
			       BYTECODE_BC_LABEL (whichloop->exit_label),
			       NULL_TREE);
    }
  else
    {
      /* In order to handle fixups, we actually create a conditional jump
	 around a unconditional branch to exit the loop.  If fixups are
	 necessary, they go before the unconditional branch.  */

      rtx label = gen_label_rtx ();
      rtx last_insn;

      do_jump (cond, NULL_RTX, label);
      last_insn = get_last_insn ();
      if (GET_CODE (last_insn) == CODE_LABEL)
	whichloop->data.loop.alt_end_label = last_insn;
      expand_goto_internal (NULL_TREE, whichloop->data.loop.end_label,
			    NULL_RTX);
      emit_label (label);
    }

  return 1;
}

/* Return non-zero if we should preserve sub-expressions as separate
   pseudos.  We never do so if we aren't optimizing.  We always do so
   if -fexpensive-optimizations.

   Otherwise, we only do so if we are in the "early" part of a loop.  I.e.,
   the loop may still be a small one.  */

int
preserve_subexpressions_p ()
{
  rtx insn;

  if (flag_expensive_optimizations)
    return 1;

  if (optimize == 0 || loop_stack == 0)
    return 0;

  insn = get_last_insn_anywhere ();

  return (insn
	  && (INSN_UID (insn) - INSN_UID (loop_stack->data.loop.start_label)
	      < n_non_fixed_regs * 3));

}

/* Generate a jump to exit the current loop, conditional, binding contour
   or case statement.  Not all such constructs are visible to this function,
   only those started with EXIT_FLAG nonzero.  Individual languages use
   the EXIT_FLAG parameter to control which kinds of constructs you can
   exit this way.

   If not currently inside anything that can be exited,
   return 0 and do nothing; caller will print an error message.  */

int
expand_exit_something ()
{
  struct nesting *n;
  last_expr_type = 0;
  for (n = nesting_stack; n; n = n->all)
    if (n->exit_label != 0)
      {
	expand_goto_internal (NULL_TREE, n->exit_label, NULL_RTX);
	return 1;
      }

  return 0;
}

/* Generate RTL to return from the current function, with no value.
   (That is, we do not do anything about returning any value.)  */

void
expand_null_return ()
{
  struct nesting *block = block_stack;
  rtx last_insn = 0;

  if (output_bytecode)
    {
      bc_emit_instruction (ret);
      return;
    }

  /* Does any pending block have cleanups?  */

  while (block && block->data.block.cleanups == 0)
    block = block->next;

  /* If yes, use a goto to return, since that runs cleanups.  */

  expand_null_return_1 (last_insn, block != 0);
}

/* Generate RTL to return from the current function, with value VAL.  */

static void
expand_value_return (val)
     rtx val;
{
  struct nesting *block = block_stack;
  rtx last_insn = get_last_insn ();
  rtx return_reg = DECL_RTL (DECL_RESULT (current_function_decl));

  /* Copy the value to the return location
     unless it's already there.  */

  if (return_reg != val)
    {
#ifdef PROMOTE_FUNCTION_RETURN
      tree type = TREE_TYPE (DECL_RESULT (current_function_decl));
      int unsignedp = TREE_UNSIGNED (type);
      enum machine_mode mode
	= promote_mode (type, DECL_MODE (DECL_RESULT (current_function_decl)),
			&unsignedp, 1);

      if (GET_MODE (val) != VOIDmode && GET_MODE (val) != mode)
	convert_move (return_reg, val, unsignedp);
      else
#endif
	emit_move_insn (return_reg, val);
    }
  if (GET_CODE (return_reg) == REG
      && REGNO (return_reg) < FIRST_PSEUDO_REGISTER)
    emit_insn (gen_rtx (USE, VOIDmode, return_reg));

  /* Does any pending block have cleanups?  */

  while (block && block->data.block.cleanups == 0)
    block = block->next;

  /* If yes, use a goto to return, since that runs cleanups.
     Use LAST_INSN to put cleanups *before* the move insn emitted above.  */

  expand_null_return_1 (last_insn, block != 0);
}

/* Output a return with no value.  If LAST_INSN is nonzero,
   pretend that the return takes place after LAST_INSN.
   If USE_GOTO is nonzero then don't use a return instruction;
   go to the return label instead.  This causes any cleanups
   of pending blocks to be executed normally.  */

static void
expand_null_return_1 (last_insn, use_goto)
     rtx last_insn;
     int use_goto;
{
  rtx end_label = cleanup_label ? cleanup_label : return_label;

  clear_pending_stack_adjust ();
  do_pending_stack_adjust ();
  last_expr_type = 0;

  /* PCC-struct return always uses an epilogue.  */
  if (current_function_returns_pcc_struct || use_goto)
    {
      if (end_label == 0)
	end_label = return_label = gen_label_rtx ();
      expand_goto_internal (NULL_TREE, end_label, last_insn);
      return;
    }

  /* Otherwise output a simple return-insn if one is available,
     unless it won't do the job.  */
#ifdef HAVE_return
  if (HAVE_return && use_goto == 0 && cleanup_label == 0)
    {
      emit_jump_insn (gen_return ());
      emit_barrier ();
      return;
    }
#endif

  /* Otherwise jump to the epilogue.  */
  expand_goto_internal (NULL_TREE, end_label, last_insn);
}

/* Generate RTL to evaluate the expression RETVAL and return it
   from the current function.  */

void
expand_return (retval)
     tree retval;
{
  /* If there are any cleanups to be performed, then they will
     be inserted following LAST_INSN.  It is desirable
     that the last_insn, for such purposes, should be the
     last insn before computing the return value.  Otherwise, cleanups
     which call functions can clobber the return value.  */
  /* ??? rms: I think that is erroneous, because in C++ it would
     run destructors on variables that might be used in the subsequent
     computation of the return value.  */
  rtx last_insn = 0;
  register rtx val = 0;
  register rtx op0;
  tree retval_rhs;
  int cleanups;
  struct nesting *block;

  /* Bytecode returns are quite simple, just leave the result on the
     arithmetic stack. */
  if (output_bytecode)
    {
      bc_expand_expr (retval);
      bc_emit_instruction (ret);
      return;
    }
  
  /* If function wants no value, give it none.  */
  if (TREE_CODE (TREE_TYPE (TREE_TYPE (current_function_decl))) == VOID_TYPE)
    {
      expand_expr (retval, NULL_RTX, VOIDmode, 0);
      emit_queue ();
      expand_null_return ();
      return;
    }

  /* Are any cleanups needed?  E.g. C++ destructors to be run?  */
  /* This is not sufficient.  We also need to watch for cleanups of the
     expression we are about to expand.  Unfortunately, we cannot know
     if it has cleanups until we expand it, and we want to change how we
     expand it depending upon if we need cleanups.  We can't win.  */
#if 0
  cleanups = any_pending_cleanups (1);
#else
  cleanups = 1;
#endif

  if (TREE_CODE (retval) == RESULT_DECL)
    retval_rhs = retval;
  else if ((TREE_CODE (retval) == MODIFY_EXPR || TREE_CODE (retval) == INIT_EXPR)
	   && TREE_CODE (TREE_OPERAND (retval, 0)) == RESULT_DECL)
    retval_rhs = TREE_OPERAND (retval, 1);
  else if (TREE_TYPE (retval) == void_type_node)
    /* Recognize tail-recursive call to void function.  */
    retval_rhs = retval;
  else
    retval_rhs = NULL_TREE;

  /* Only use `last_insn' if there are cleanups which must be run.  */
  if (cleanups || cleanup_label != 0)
    last_insn = get_last_insn ();

  /* Distribute return down conditional expr if either of the sides
     may involve tail recursion (see test below).  This enhances the number
     of tail recursions we see.  Don't do this always since it can produce
     sub-optimal code in some cases and we distribute assignments into
     conditional expressions when it would help.  */

  if (optimize && retval_rhs != 0
      && frame_offset == 0
      && TREE_CODE (retval_rhs) == COND_EXPR
      && (TREE_CODE (TREE_OPERAND (retval_rhs, 1)) == CALL_EXPR
	  || TREE_CODE (TREE_OPERAND (retval_rhs, 2)) == CALL_EXPR))
    {
      rtx label = gen_label_rtx ();
      tree expr;

      do_jump (TREE_OPERAND (retval_rhs, 0), label, NULL_RTX);
      expr = build (MODIFY_EXPR, TREE_TYPE (current_function_decl),
		    DECL_RESULT (current_function_decl),
		    TREE_OPERAND (retval_rhs, 1));
      TREE_SIDE_EFFECTS (expr) = 1;
      expand_return (expr);
      emit_label (label);

      expr = build (MODIFY_EXPR, TREE_TYPE (current_function_decl),
		    DECL_RESULT (current_function_decl),
		    TREE_OPERAND (retval_rhs, 2));
      TREE_SIDE_EFFECTS (expr) = 1;
      expand_return (expr);
      return;
    }

  /* For tail-recursive call to current function,
     just jump back to the beginning.
     It's unsafe if any auto variable in this function
     has its address taken; for simplicity,
     require stack frame to be empty.  */
  if (optimize && retval_rhs != 0
      && frame_offset == 0
      && TREE_CODE (retval_rhs) == CALL_EXPR
      && TREE_CODE (TREE_OPERAND (retval_rhs, 0)) == ADDR_EXPR
      && TREE_OPERAND (TREE_OPERAND (retval_rhs, 0), 0) == current_function_decl
      /* Finish checking validity, and if valid emit code
	 to set the argument variables for the new call.  */
      && tail_recursion_args (TREE_OPERAND (retval_rhs, 1),
			      DECL_ARGUMENTS (current_function_decl)))
    {
      if (tail_recursion_label == 0)
	{
	  tail_recursion_label = gen_label_rtx ();
	  emit_label_after (tail_recursion_label,
			    tail_recursion_reentry);
	}
      emit_queue ();
      expand_goto_internal (NULL_TREE, tail_recursion_label, last_insn);
      emit_barrier ();
      return;
    }
#ifdef HAVE_return
  /* This optimization is safe if there are local cleanups
     because expand_null_return takes care of them.
     ??? I think it should also be safe when there is a cleanup label,
     because expand_null_return takes care of them, too.
     Any reason why not?  */
  if (HAVE_return && cleanup_label == 0
      && ! current_function_returns_pcc_struct
      && BRANCH_COST <= 1)
    {
      /* If this is  return x == y;  then generate
	 if (x == y) return 1; else return 0;
	 if we can do it with explicit return insns and
	 branches are cheap.  */
      if (retval_rhs)
	switch (TREE_CODE (retval_rhs))
	  {
	  case EQ_EXPR:
	  case NE_EXPR:
	  case GT_EXPR:
	  case GE_EXPR:
	  case LT_EXPR:
	  case LE_EXPR:
	  case TRUTH_ANDIF_EXPR:
	  case TRUTH_ORIF_EXPR:
	  case TRUTH_AND_EXPR:
	  case TRUTH_OR_EXPR:
	  case TRUTH_NOT_EXPR:
	  case TRUTH_XOR_EXPR:
	    op0 = gen_label_rtx ();
	    jumpifnot (retval_rhs, op0);
	    expand_value_return (const1_rtx);
	    emit_label (op0);
	    expand_value_return (const0_rtx);
	    return;
	  }
    }
#endif /* HAVE_return */

  /* If the result is an aggregate that is being returned in one (or more)
     registers, load the registers here.  The compiler currently can't handle
     copying a BLKmode value into registers.  We could put this code in a
     more general area (for use by everyone instead of just function
     call/return), but until this feature is generally usable it is kept here
     (and in expand_call).  The value must go into a pseudo in case there
     are cleanups that will clobber the real return register.  */

  if (retval_rhs != 0
      && TYPE_MODE (TREE_TYPE (retval_rhs)) == BLKmode
      && GET_CODE (DECL_RTL (DECL_RESULT (current_function_decl))) == REG)
    {
      int i, bitpos, xbitpos;
      int big_endian_correction = 0;
      int bytes = int_size_in_bytes (TREE_TYPE (retval_rhs));
      int n_regs = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
      int bitsize = MIN (TYPE_ALIGN (TREE_TYPE (retval_rhs)),BITS_PER_WORD);
      rtx *result_pseudos = (rtx *) alloca (sizeof (rtx) * n_regs);
      rtx result_reg, src, dst;
      rtx result_val = expand_expr (retval_rhs, NULL_RTX, VOIDmode, 0);
      enum machine_mode tmpmode, result_reg_mode;

      /* Structures whose size is not a multiple of a word are aligned
	 to the least significant byte (to the right).  On a BYTES_BIG_ENDIAN
	 machine, this means we must skip the empty high order bytes when
	 calculating the bit offset.  */
      if (BYTES_BIG_ENDIAN && bytes % UNITS_PER_WORD)
	big_endian_correction = (BITS_PER_WORD - ((bytes % UNITS_PER_WORD)
						  * BITS_PER_UNIT));

      /* Copy the structure BITSIZE bits at a time.  */ 
      for (bitpos = 0, xbitpos = big_endian_correction;
	   bitpos < bytes * BITS_PER_UNIT;
	   bitpos += bitsize, xbitpos += bitsize)
	{
	  /* We need a new destination pseudo each time xbitpos is
	     on a word boundary and when xbitpos == big_endian_correction
	     (the first time through).  */
	  if (xbitpos % BITS_PER_WORD == 0
	      || xbitpos == big_endian_correction)
	    {
	      /* Generate an appropriate register.  */
	      dst = gen_reg_rtx (word_mode);
	      result_pseudos[xbitpos / BITS_PER_WORD] = dst;

	      /* Clobber the destination before we move anything into it.  */
	      emit_insn (gen_rtx (CLOBBER, VOIDmode, dst));
	    }

	  /* We need a new source operand each time bitpos is on a word
	     boundary.  */
	  if (bitpos % BITS_PER_WORD == 0)
	    src = operand_subword_force (result_val,
					 bitpos / BITS_PER_WORD,
					 BLKmode);

	  /* Use bitpos for the source extraction (left justified) and
	     xbitpos for the destination store (right justified).  */
	  store_bit_field (dst, bitsize, xbitpos % BITS_PER_WORD, word_mode,
			   extract_bit_field (src, bitsize,
					      bitpos % BITS_PER_WORD, 1,
					      NULL_RTX, word_mode,
					      word_mode,
					      bitsize / BITS_PER_UNIT,
					      BITS_PER_WORD),
			   bitsize / BITS_PER_UNIT, BITS_PER_WORD);
	}

      /* Find the smallest integer mode large enough to hold the
	 entire structure and use that mode instead of BLKmode
	 on the USE insn for the return register.   */
      bytes = int_size_in_bytes (TREE_TYPE (retval_rhs));
      for (tmpmode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	   tmpmode != MAX_MACHINE_MODE;
	   tmpmode = GET_MODE_WIDER_MODE (tmpmode))
	{
	  /* Have we found a large enough mode?  */
	  if (GET_MODE_SIZE (tmpmode) >= bytes)
	    break;
	}

      /* No suitable mode found.  */
      if (tmpmode == MAX_MACHINE_MODE)
	abort ();

      PUT_MODE (DECL_RTL (DECL_RESULT (current_function_decl)), tmpmode);

      if (GET_MODE_SIZE (tmpmode) < GET_MODE_SIZE (word_mode))
	result_reg_mode = word_mode;
      else
	result_reg_mode = tmpmode;
      result_reg = gen_reg_rtx (result_reg_mode);

      /* Now that the value is in pseudos, copy it to the result reg(s).  */
      emit_queue ();
      free_temp_slots ();
      for (i = 0; i < n_regs; i++)
	emit_move_insn (operand_subword (result_reg, i, 0, result_reg_mode),
			result_pseudos[i]);

      if (tmpmode != result_reg_mode)
	result_reg = gen_lowpart (tmpmode, result_reg);

      expand_value_return (result_reg);
    }
  else if (cleanups
      && retval_rhs != 0
      && TREE_TYPE (retval_rhs) != void_type_node
      && GET_CODE (DECL_RTL (DECL_RESULT (current_function_decl))) == REG)
    {
      /* Calculate the return value into a pseudo reg.  */
      val = expand_expr (retval_rhs, NULL_RTX, VOIDmode, 0);
      emit_queue ();
      /* All temporaries have now been used.  */
      free_temp_slots ();
      /* Return the calculated value, doing cleanups first.  */
      expand_value_return (val);
    }
  else
    {
      /* No cleanups or no hard reg used;
	 calculate value into hard return reg.  */
      expand_expr (retval, const0_rtx, VOIDmode, 0);
      emit_queue ();
      free_temp_slots ();
      expand_value_return (DECL_RTL (DECL_RESULT (current_function_decl)));
    }
}

/* Return 1 if the end of the generated RTX is not a barrier.
   This means code already compiled can drop through.  */

int
drop_through_at_end_p ()
{
  rtx insn = get_last_insn ();
  while (insn && GET_CODE (insn) == NOTE)
    insn = PREV_INSN (insn);
  return insn && GET_CODE (insn) != BARRIER;
}

/* Emit code to alter this function's formal parms for a tail-recursive call.
   ACTUALS is a list of actual parameter expressions (chain of TREE_LISTs).
   FORMALS is the chain of decls of formals.
   Return 1 if this can be done;
   otherwise return 0 and do not emit any code.  */

static int
tail_recursion_args (actuals, formals)
     tree actuals, formals;
{
  register tree a = actuals, f = formals;
  register int i;
  register rtx *argvec;

  /* Check that number and types of actuals are compatible
     with the formals.  This is not always true in valid C code.
     Also check that no formal needs to be addressable
     and that all formals are scalars.  */

  /* Also count the args.  */

  for (a = actuals, f = formals, i = 0; a && f; a = TREE_CHAIN (a), f = TREE_CHAIN (f), i++)
    {
      if (TREE_TYPE (TREE_VALUE (a)) != TREE_TYPE (f))
	return 0;
      if (GET_CODE (DECL_RTL (f)) != REG || DECL_MODE (f) == BLKmode)
	return 0;
    }
  if (a != 0 || f != 0)
    return 0;

  /* Compute all the actuals.  */

  argvec = (rtx *) alloca (i * sizeof (rtx));

  for (a = actuals, i = 0; a; a = TREE_CHAIN (a), i++)
    argvec[i] = expand_expr (TREE_VALUE (a), NULL_RTX, VOIDmode, 0);

  /* Find which actual values refer to current values of previous formals.
     Copy each of them now, before any formal is changed.  */

  for (a = actuals, i = 0; a; a = TREE_CHAIN (a), i++)
    {
      int copy = 0;
      register int j;
      for (f = formals, j = 0; j < i; f = TREE_CHAIN (f), j++)
	if (reg_mentioned_p (DECL_RTL (f), argvec[i]))
	  { copy = 1; break; }
      if (copy)
	argvec[i] = copy_to_reg (argvec[i]);
    }

  /* Store the values of the actuals into the formals.  */

  for (f = formals, a = actuals, i = 0; f;
       f = TREE_CHAIN (f), a = TREE_CHAIN (a), i++)
    {
      if (GET_MODE (DECL_RTL (f)) == GET_MODE (argvec[i]))
	emit_move_insn (DECL_RTL (f), argvec[i]);
      else
	convert_move (DECL_RTL (f), argvec[i],
		      TREE_UNSIGNED (TREE_TYPE (TREE_VALUE (a))));
    }

  free_temp_slots ();
  return 1;
}

/* Generate the RTL code for entering a binding contour.
   The variables are declared one by one, by calls to `expand_decl'.

   EXIT_FLAG is nonzero if this construct should be visible to
   `exit_something'.  */

void
expand_start_bindings (exit_flag)
     int exit_flag;
{
  struct nesting *thisblock = ALLOC_NESTING ();
  rtx note = output_bytecode ? 0 : emit_note (NULL_PTR, NOTE_INSN_BLOCK_BEG);

  /* Make an entry on block_stack for the block we are entering.  */

  thisblock->next = block_stack;
  thisblock->all = nesting_stack;
  thisblock->depth = ++nesting_depth;
  thisblock->data.block.stack_level = 0;
  thisblock->data.block.cleanups = 0;
  thisblock->data.block.function_call_count = 0;
#if 0
  if (block_stack)
    {
      if (block_stack->data.block.cleanups == NULL_TREE
	  && (block_stack->data.block.outer_cleanups == NULL_TREE
	      || block_stack->data.block.outer_cleanups == empty_cleanup_list))
	thisblock->data.block.outer_cleanups = empty_cleanup_list;
      else
	thisblock->data.block.outer_cleanups
	  = tree_cons (NULL_TREE, block_stack->data.block.cleanups,
		       block_stack->data.block.outer_cleanups);
    }
  else
    thisblock->data.block.outer_cleanups = 0;
#endif
#if 1
  if (block_stack
      && !(block_stack->data.block.cleanups == NULL_TREE
	   && block_stack->data.block.outer_cleanups == NULL_TREE))
    thisblock->data.block.outer_cleanups
      = tree_cons (NULL_TREE, block_stack->data.block.cleanups,
		   block_stack->data.block.outer_cleanups);
  else
    thisblock->data.block.outer_cleanups = 0;
#endif
  thisblock->data.block.label_chain = 0;
  thisblock->data.block.innermost_stack_block = stack_block_stack;
  thisblock->data.block.first_insn = note;
  thisblock->data.block.block_start_count = ++block_start_count;
  thisblock->exit_label = exit_flag ? gen_label_rtx () : 0;
  block_stack = thisblock;
  nesting_stack = thisblock;

  if (!output_bytecode)
    {
      /* Make a new level for allocating stack slots.  */
      push_temp_slots ();
    }
}

/* Given a pointer to a BLOCK node, save a pointer to the most recently
   generated NOTE_INSN_BLOCK_END in the BLOCK_END_NOTE field of the given
   BLOCK node.  */

void
remember_end_note (block)
     register tree block;
{
  BLOCK_END_NOTE (block) = last_block_end_note;
  last_block_end_note = NULL_RTX;
}

/* Generate RTL code to terminate a binding contour.
   VARS is the chain of VAR_DECL nodes
   for the variables bound in this contour.
   MARK_ENDS is nonzero if we should put a note at the beginning
   and end of this binding contour.

   DONT_JUMP_IN is nonzero if it is not valid to jump into this contour.
   (That is true automatically if the contour has a saved stack level.)  */

void
expand_end_bindings (vars, mark_ends, dont_jump_in)
     tree vars;
     int mark_ends;
     int dont_jump_in;
{
  register struct nesting *thisblock = block_stack;
  register tree decl;

  if (output_bytecode)
    {
      bc_expand_end_bindings (vars, mark_ends, dont_jump_in);
      return;
    }

  if (warn_unused)
    for (decl = vars; decl; decl = TREE_CHAIN (decl))
      if (! TREE_USED (decl) && TREE_CODE (decl) == VAR_DECL
	  && ! DECL_IN_SYSTEM_HEADER (decl))
	warning_with_decl (decl, "unused variable `%s'");

  if (thisblock->exit_label)
    {
      do_pending_stack_adjust ();
      emit_label (thisblock->exit_label);
    }

  /* If necessary, make a handler for nonlocal gotos taking
     place in the function calls in this block.  */
  if (function_call_count != thisblock->data.block.function_call_count
      && nonlocal_labels
      /* Make handler for outermost block
	 if there were any nonlocal gotos to this function.  */
      && (thisblock->next == 0 ? current_function_has_nonlocal_label
	  /* Make handler for inner block if it has something
	     special to do when you jump out of it.  */
	  : (thisblock->data.block.cleanups != 0
	     || thisblock->data.block.stack_level != 0)))
    {
      tree link;
      rtx afterward = gen_label_rtx ();
      rtx handler_label = gen_label_rtx ();
      rtx save_receiver = gen_reg_rtx (Pmode);
      rtx insns;

      /* Don't let jump_optimize delete the handler.  */
      LABEL_PRESERVE_P (handler_label) = 1;

      /* Record the handler address in the stack slot for that purpose,
	 during this block, saving and restoring the outer value.  */
      if (thisblock->next != 0)
	{
	  emit_move_insn (nonlocal_goto_handler_slot, save_receiver);

	  start_sequence ();
	  emit_move_insn (save_receiver, nonlocal_goto_handler_slot);
	  insns = get_insns ();
	  end_sequence ();
	  emit_insns_before (insns, thisblock->data.block.first_insn);
	}

      start_sequence ();
      emit_move_insn (nonlocal_goto_handler_slot,
		      gen_rtx (LABEL_REF, Pmode, handler_label));
      insns = get_insns ();
      end_sequence ();
      emit_insns_before (insns, thisblock->data.block.first_insn);

      /* Jump around the handler; it runs only when specially invoked.  */
      emit_jump (afterward);
      emit_label (handler_label);

#ifdef HAVE_nonlocal_goto
      if (! HAVE_nonlocal_goto)
#endif
	/* First adjust our frame pointer to its actual value.  It was
	   previously set to the start of the virtual area corresponding to
	   the stacked variables when we branched here and now needs to be
	   adjusted to the actual hardware fp value.

	   Assignments are to virtual registers are converted by
	   instantiate_virtual_regs into the corresponding assignment
	   to the underlying register (fp in this case) that makes
	   the original assignment true.
	   So the following insn will actually be
	   decrementing fp by STARTING_FRAME_OFFSET.  */
	emit_move_insn (virtual_stack_vars_rtx, hard_frame_pointer_rtx);

#if ARG_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
      if (fixed_regs[ARG_POINTER_REGNUM])
	{
#ifdef ELIMINABLE_REGS
	  /* If the argument pointer can be eliminated in favor of the
	     frame pointer, we don't need to restore it.  We assume here
	     that if such an elimination is present, it can always be used.
	     This is the case on all known machines; if we don't make this
	     assumption, we do unnecessary saving on many machines.  */
	  static struct elims {int from, to;} elim_regs[] = ELIMINABLE_REGS;
	  int i;

	  for (i = 0; i < sizeof elim_regs / sizeof elim_regs[0]; i++)
	    if (elim_regs[i].from == ARG_POINTER_REGNUM
		&& elim_regs[i].to == HARD_FRAME_POINTER_REGNUM)
	      break;

	  if (i == sizeof elim_regs / sizeof elim_regs [0])
#endif
	    {
	      /* Now restore our arg pointer from the address at which it
		 was saved in our stack frame.
		 If there hasn't be space allocated for it yet, make
		 some now.  */
	      if (arg_pointer_save_area == 0)
		arg_pointer_save_area
		  = assign_stack_local (Pmode, GET_MODE_SIZE (Pmode), 0);
	      emit_move_insn (virtual_incoming_args_rtx,
			      /* We need a pseudo here, or else
				 instantiate_virtual_regs_1 complains.  */
			      copy_to_reg (arg_pointer_save_area));
	    }
	}
#endif

      /* The handler expects the desired label address in the static chain
	 register.  It tests the address and does an appropriate jump
	 to whatever label is desired.  */
      for (link = nonlocal_labels; link; link = TREE_CHAIN (link))
	/* Skip any labels we shouldn't be able to jump to from here.  */
	if (! DECL_TOO_LATE (TREE_VALUE (link)))
	  {
	    rtx not_this = gen_label_rtx ();
	    rtx this = gen_label_rtx ();
	    do_jump_if_equal (static_chain_rtx,
			      gen_rtx (LABEL_REF, Pmode, DECL_RTL (TREE_VALUE (link))),
			      this, 0);
	    emit_jump (not_this);
	    emit_label (this);
	    expand_goto (TREE_VALUE (link));
	    emit_label (not_this);
	  }
      /* If label is not recognized, abort.  */
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "abort"), 0,
			 VOIDmode, 0);
      emit_barrier ();
      emit_label (afterward);
    }

  /* Don't allow jumping into a block that has cleanups or a stack level.  */
  if (dont_jump_in
      || thisblock->data.block.stack_level != 0
      || thisblock->data.block.cleanups != 0)
    {
      struct label_chain *chain;

      /* Any labels in this block are no longer valid to go to.
	 Mark them to cause an error message.  */
      for (chain = thisblock->data.block.label_chain; chain; chain = chain->next)
	{
	  DECL_TOO_LATE (chain->label) = 1;
	  /* If any goto without a fixup came to this label,
	     that must be an error, because gotos without fixups
	     come from outside all saved stack-levels and all cleanups.  */
	  if (TREE_ADDRESSABLE (chain->label))
	    error_with_decl (chain->label,
			     "label `%s' used before containing binding contour");
	}
    }

  /* Restore stack level in effect before the block
     (only if variable-size objects allocated).  */
  /* Perform any cleanups associated with the block.  */

  if (thisblock->data.block.stack_level != 0
      || thisblock->data.block.cleanups != 0)
    {
      /* Only clean up here if this point can actually be reached.  */
      int reachable = GET_CODE (get_last_insn ()) != BARRIER;

      /* Don't let cleanups affect ({...}) constructs.  */
      int old_expr_stmts_for_value = expr_stmts_for_value;
      rtx old_last_expr_value = last_expr_value;
      tree old_last_expr_type = last_expr_type;
      expr_stmts_for_value = 0;

      /* Do the cleanups.  */
      expand_cleanups (thisblock->data.block.cleanups, NULL_TREE, 0, reachable);
      if (reachable)
	do_pending_stack_adjust ();

      expr_stmts_for_value = old_expr_stmts_for_value;
      last_expr_value = old_last_expr_value;
      last_expr_type = old_last_expr_type;

      /* Restore the stack level.  */

      if (reachable && thisblock->data.block.stack_level != 0)
	{
	  emit_stack_restore (thisblock->next ? SAVE_BLOCK : SAVE_FUNCTION,
			      thisblock->data.block.stack_level, NULL_RTX);
	  if (nonlocal_goto_handler_slot != 0)
	    emit_stack_save (SAVE_NONLOCAL, &nonlocal_goto_stack_level,
			     NULL_RTX);
	}

      /* Any gotos out of this block must also do these things.
	 Also report any gotos with fixups that came to labels in this
	 level.  */
      fixup_gotos (thisblock,
		   thisblock->data.block.stack_level,
		   thisblock->data.block.cleanups,
		   thisblock->data.block.first_insn,
		   dont_jump_in);
    }

  /* Mark the beginning and end of the scope if requested.
     We do this now, after running cleanups on the variables
     just going out of scope, so they are in scope for their cleanups.  */

  if (mark_ends)
    last_block_end_note = emit_note (NULL_PTR, NOTE_INSN_BLOCK_END);
  else
    /* Get rid of the beginning-mark if we don't make an end-mark.  */
    NOTE_LINE_NUMBER (thisblock->data.block.first_insn) = NOTE_INSN_DELETED;

  /* If doing stupid register allocation, make sure lives of all
     register variables declared here extend thru end of scope.  */

  if (obey_regdecls)
    for (decl = vars; decl; decl = TREE_CHAIN (decl))
      {
	rtx rtl = DECL_RTL (decl);
	if (TREE_CODE (decl) == VAR_DECL && rtl != 0)
	  use_variable (rtl);
      }

  /* Restore block_stack level for containing block.  */

  stack_block_stack = thisblock->data.block.innermost_stack_block;
  POPSTACK (block_stack);

  /* Pop the stack slot nesting and free any slots at this level.  */
  pop_temp_slots ();
}


/* End a binding contour.
   VARS is the chain of VAR_DECL nodes for the variables bound
   in this contour.  MARK_ENDS is nonzer if we should put a note
   at the beginning and end of this binding contour.
   DONT_JUMP_IN is nonzero if it is not valid to jump into this
   contour.  */

static void
bc_expand_end_bindings (vars, mark_ends, dont_jump_in)
     tree vars;
     int mark_ends;
     int dont_jump_in;
{
  struct nesting *thisbind = nesting_stack;
  tree decl;

  if (warn_unused)
    for (decl = vars; decl; decl = TREE_CHAIN (decl))
      if (! TREE_USED (TREE_VALUE (decl)) && TREE_CODE (TREE_VALUE (decl)) == VAR_DECL)
	warning_with_decl (decl, "unused variable `%s'");

  if (thisbind->exit_label)
    bc_emit_bytecode_labeldef (BYTECODE_BC_LABEL (thisbind->exit_label));

  /* Pop block/bindings off stack */
  POPSTACK (block_stack);
}

/* Generate RTL for the automatic variable declaration DECL.
   (Other kinds of declarations are simply ignored if seen here.)  */

void
expand_decl (decl)
     register tree decl;
{
  struct nesting *thisblock = block_stack;
  tree type;

  if (output_bytecode)
    {
      bc_expand_decl (decl, 0);
      return;
    }

  type = TREE_TYPE (decl);

  /* Only automatic variables need any expansion done.
     Static and external variables, and external functions,
     will be handled by `assemble_variable' (called from finish_decl).
     TYPE_DECL and CONST_DECL require nothing.
     PARM_DECLs are handled in `assign_parms'.  */

  if (TREE_CODE (decl) != VAR_DECL)
    return;
  if (TREE_STATIC (decl) || DECL_EXTERNAL (decl))
    return;

  /* Create the RTL representation for the variable.  */

  if (type == error_mark_node)
    DECL_RTL (decl) = gen_rtx (MEM, BLKmode, const0_rtx);
  else if (DECL_SIZE (decl) == 0)
    /* Variable with incomplete type.  */
    {
      if (DECL_INITIAL (decl) == 0)
	/* Error message was already done; now avoid a crash.  */
	DECL_RTL (decl) = assign_stack_temp (DECL_MODE (decl), 0, 1);
      else
	/* An initializer is going to decide the size of this array.
	   Until we know the size, represent its address with a reg.  */
	DECL_RTL (decl) = gen_rtx (MEM, BLKmode, gen_reg_rtx (Pmode));
      MEM_IN_STRUCT_P (DECL_RTL (decl)) = AGGREGATE_TYPE_P (type);
    }
  else if (DECL_MODE (decl) != BLKmode
	   /* If -ffloat-store, don't put explicit float vars
	      into regs.  */
	   && !(flag_float_store
		&& TREE_CODE (type) == REAL_TYPE)
	   && ! TREE_THIS_VOLATILE (decl)
	   && ! TREE_ADDRESSABLE (decl)
	   && (DECL_REGISTER (decl) || ! obey_regdecls))
    {
      /* Automatic variable that can go in a register.  */
      int unsignedp = TREE_UNSIGNED (type);
      enum machine_mode reg_mode
	= promote_mode (type, DECL_MODE (decl), &unsignedp, 0);

      if (TREE_CODE (type) == COMPLEX_TYPE)
	{
	  rtx realpart, imagpart;
	  enum machine_mode partmode = TYPE_MODE (TREE_TYPE (type));

	  /* For a complex type variable, make a CONCAT of two pseudos
	     so that the real and imaginary parts
	     can be allocated separately.  */
	  realpart = gen_reg_rtx (partmode);
	  REG_USERVAR_P (realpart) = 1;
	  imagpart = gen_reg_rtx (partmode);
	  REG_USERVAR_P (imagpart) = 1;
	  DECL_RTL (decl) = gen_rtx (CONCAT, reg_mode, realpart, imagpart);
	}
      else
	{
	  DECL_RTL (decl) = gen_reg_rtx (reg_mode);
	  if (TREE_CODE (type) == POINTER_TYPE)
	    mark_reg_pointer (DECL_RTL (decl));
	  REG_USERVAR_P (DECL_RTL (decl)) = 1;
	}
    }
  else if (TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST)
    {
      /* Variable of fixed size that goes on the stack.  */
      rtx oldaddr = 0;
      rtx addr;

      /* If we previously made RTL for this decl, it must be an array
	 whose size was determined by the initializer.
	 The old address was a register; set that register now
	 to the proper address.  */
      if (DECL_RTL (decl) != 0)
	{
	  if (GET_CODE (DECL_RTL (decl)) != MEM
	      || GET_CODE (XEXP (DECL_RTL (decl), 0)) != REG)
	    abort ();
	  oldaddr = XEXP (DECL_RTL (decl), 0);
	}

      DECL_RTL (decl)
	= assign_stack_temp (DECL_MODE (decl),
			     ((TREE_INT_CST_LOW (DECL_SIZE (decl))
			       + BITS_PER_UNIT - 1)
			      / BITS_PER_UNIT),
			     1);
      MEM_IN_STRUCT_P (DECL_RTL (decl)) = AGGREGATE_TYPE_P (TREE_TYPE (decl));

      /* Set alignment we actually gave this decl.  */
      DECL_ALIGN (decl) = (DECL_MODE (decl) == BLKmode ? BIGGEST_ALIGNMENT
			   : GET_MODE_BITSIZE (DECL_MODE (decl)));

      if (oldaddr)
	{
	  addr = force_operand (XEXP (DECL_RTL (decl), 0), oldaddr);
	  if (addr != oldaddr)
	    emit_move_insn (oldaddr, addr);
	}

      /* If this is a memory ref that contains aggregate components,
	 mark it as such for cse and loop optimize.  */
      MEM_IN_STRUCT_P (DECL_RTL (decl)) = AGGREGATE_TYPE_P (TREE_TYPE (decl));
#if 0
      /* If this is in memory because of -ffloat-store,
	 set the volatile bit, to prevent optimizations from
	 undoing the effects.  */
      if (flag_float_store && TREE_CODE (type) == REAL_TYPE)
	MEM_VOLATILE_P (DECL_RTL (decl)) = 1;
#endif
    }
  else
    /* Dynamic-size object: must push space on the stack.  */
    {
      rtx address, size;

      /* Record the stack pointer on entry to block, if have
	 not already done so.  */
      if (thisblock->data.block.stack_level == 0)
	{
	  do_pending_stack_adjust ();
	  emit_stack_save (thisblock->next ? SAVE_BLOCK : SAVE_FUNCTION,
			   &thisblock->data.block.stack_level,
			   thisblock->data.block.first_insn);
	  stack_block_stack = thisblock;
	}

      /* Compute the variable's size, in bytes.  */
      size = expand_expr (size_binop (CEIL_DIV_EXPR,
				      DECL_SIZE (decl),
				      size_int (BITS_PER_UNIT)),
			  NULL_RTX, VOIDmode, 0);
      free_temp_slots ();

      /* Allocate space on the stack for the variable.  */
      address = allocate_dynamic_stack_space (size, NULL_RTX,
					      DECL_ALIGN (decl));

      /* Reference the variable indirect through that rtx.  */
      DECL_RTL (decl) = gen_rtx (MEM, DECL_MODE (decl), address);

      /* If this is a memory ref that contains aggregate components,
	 mark it as such for cse and loop optimize.  */
      MEM_IN_STRUCT_P (DECL_RTL (decl)) = AGGREGATE_TYPE_P (TREE_TYPE (decl));

      /* Indicate the alignment we actually gave this variable.  */
#ifdef STACK_BOUNDARY
      DECL_ALIGN (decl) = STACK_BOUNDARY;
#else
      DECL_ALIGN (decl) = BIGGEST_ALIGNMENT;
#endif
    }

  if (TREE_THIS_VOLATILE (decl))
    MEM_VOLATILE_P (DECL_RTL (decl)) = 1;
#if 0 /* A variable is not necessarily unchanging
	 just because it is const.  RTX_UNCHANGING_P
	 means no change in the function,
	 not merely no change in the variable's scope.
	 It is correct to set RTX_UNCHANGING_P if the variable's scope
	 is the whole function.  There's no convenient way to test that.  */
  if (TREE_READONLY (decl))
    RTX_UNCHANGING_P (DECL_RTL (decl)) = 1;
#endif

  /* If doing stupid register allocation, make sure life of any
     register variable starts here, at the start of its scope.  */

  if (obey_regdecls)
    use_variable (DECL_RTL (decl));
}


/* Generate code for the automatic variable declaration DECL.  For
   most variables this just means we give it a stack offset.  The
   compiler sometimes emits cleanups without variables and we will
   have to deal with those too.  */

static void
bc_expand_decl (decl, cleanup)
     tree decl;
     tree cleanup;
{
  tree type;

  if (!decl)
    {
      /* A cleanup with no variable.  */
      if (!cleanup)
	abort ();

      return;
    }

  /* Only auto variables need any work.  */
  if (TREE_CODE (decl) != VAR_DECL || TREE_STATIC (decl) || DECL_EXTERNAL (decl))
    return;

  type = TREE_TYPE (decl);

  if (type == error_mark_node)
    DECL_RTL (decl) = bc_gen_rtx ((char *) 0, 0, (struct bc_label *) 0);

  else if (DECL_SIZE (decl) == 0)

    /* Variable with incomplete type.  The stack offset herein will be
       fixed later in expand_decl_init ().  */
    DECL_RTL (decl) = bc_gen_rtx ((char *) 0, 0, (struct bc_label *) 0);

  else if (TREE_CONSTANT (DECL_SIZE (decl)))
    {
      DECL_RTL (decl) = bc_allocate_local (TREE_INT_CST_LOW (DECL_SIZE (decl)) / BITS_PER_UNIT,
					   DECL_ALIGN (decl));
    }
  else
    DECL_RTL (decl) = bc_allocate_variable_array (DECL_SIZE (decl));
}

/* Emit code to perform the initialization of a declaration DECL.  */

void
expand_decl_init (decl)
     tree decl;
{
  int was_used = TREE_USED (decl);

  if (output_bytecode)
    {
      bc_expand_decl_init (decl);
      return;
    }

  /* If this is a CONST_DECL, we don't have to generate any code, but
     if DECL_INITIAL is a constant, call expand_expr to force TREE_CST_RTL
     to be set while in the obstack containing the constant.  If we don't
     do this, we can lose if we have functions nested three deep and the middle
     function makes a CONST_DECL whose DECL_INITIAL is a STRING_CST while
     the innermost function is the first to expand that STRING_CST.  */
  if (TREE_CODE (decl) == CONST_DECL)
    {
      if (DECL_INITIAL (decl) && TREE_CONSTANT (DECL_INITIAL (decl)))
	expand_expr (DECL_INITIAL (decl), NULL_RTX, VOIDmode,
		     EXPAND_INITIALIZER);
      return;
    }

  if (TREE_STATIC (decl))
    return;

  /* Compute and store the initial value now.  */

  if (DECL_INITIAL (decl) == error_mark_node)
    {
      enum tree_code code = TREE_CODE (TREE_TYPE (decl));
      if (code == INTEGER_TYPE || code == REAL_TYPE || code == ENUMERAL_TYPE
	  || code == POINTER_TYPE)
	expand_assignment (decl, convert (TREE_TYPE (decl), integer_zero_node),
			   0, 0);
      emit_queue ();
    }
  else if (DECL_INITIAL (decl) && TREE_CODE (DECL_INITIAL (decl)) != TREE_LIST)
    {
      emit_line_note (DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));
      expand_assignment (decl, DECL_INITIAL (decl), 0, 0);
      emit_queue ();
    }

  /* Don't let the initialization count as "using" the variable.  */
  TREE_USED (decl) = was_used;

  /* Free any temporaries we made while initializing the decl.  */
  preserve_temp_slots (NULL_RTX);
  free_temp_slots ();
}

/* Expand initialization for variable-sized types. Allocate array
   using newlocalSI and set local variable, which is a pointer to the
   storage. */

static void
bc_expand_variable_local_init (decl)
     tree decl;
{
  /* Evaluate size expression and coerce to SI */
  bc_expand_expr (DECL_SIZE (decl));

  /* Type sizes are always (?) of TREE_CODE INTEGER_CST, so
     no coercion is necessary (?) */

/*  emit_typecode_conversion (preferred_typecode (TYPE_MODE (DECL_SIZE (decl)),
						TREE_UNSIGNED (DECL_SIZE (decl))), SIcode); */

  /* Emit code to allocate array */
  bc_emit_instruction (newlocalSI);

  /* Store array pointer in local variable. This is the only instance
     where we actually want the address of the pointer to the
     variable-size block, rather than the pointer itself.  We avoid
     using expand_address() since that would cause the pointer to be
     pushed rather than its address. Hence the hard-coded reference;
     notice also that the variable is always local (no global
     variable-size type variables). */

  bc_load_localaddr (DECL_RTL (decl));
  bc_emit_instruction (storeP);
}


/* Emit code to initialize a declaration.  */

static void
bc_expand_decl_init (decl)
     tree decl;
{
  int org_stack_depth;

  /* Statical initializers are handled elsewhere */

  if (TREE_STATIC (decl))
    return;

  /* Memory original stack depth */
  org_stack_depth = stack_depth;

  /* If the type is variable-size, we first create its space (we ASSUME
     it CAN'T be static).  We do this regardless of whether there's an
     initializer assignment or not. */

  if (TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
    bc_expand_variable_local_init (decl);

  /* Expand initializer assignment */
  if (DECL_INITIAL (decl) == error_mark_node)
    {
      enum tree_code code = TREE_CODE (TREE_TYPE (decl));

      if (code == INTEGER_TYPE || code == REAL_TYPE || code == ENUMERAL_TYPE
	  || code == POINTER_TYPE)

	expand_assignment (TREE_TYPE (decl), decl, 0, 0);
    }
  else if (DECL_INITIAL (decl))
    expand_assignment (TREE_TYPE (decl), decl, 0, 0);

  /* Restore stack depth */
  if (org_stack_depth > stack_depth)
    abort ();

  bc_adjust_stack (stack_depth - org_stack_depth);
}
 

/* CLEANUP is an expression to be executed at exit from this binding contour;
   for example, in C++, it might call the destructor for this variable.

   If CLEANUP contains any SAVE_EXPRs, then you must preevaluate them
   either before or after calling `expand_decl_cleanup' but before compiling
   any subsequent expressions.  This is because CLEANUP may be expanded
   more than once, on different branches of execution.
   For the same reason, CLEANUP may not contain a CALL_EXPR
   except as its topmost node--else `preexpand_calls' would get confused.

   If CLEANUP is nonzero and DECL is zero, we record a cleanup
   that is not associated with any particular variable.   */

int
expand_decl_cleanup (decl, cleanup)
     tree decl, cleanup;
{
  struct nesting *thisblock = block_stack;

  /* Error if we are not in any block.  */
  if (thisblock == 0)
    return 0;

  /* Record the cleanup if there is one.  */

  if (cleanup != 0)
    {
      thisblock->data.block.cleanups
	= temp_tree_cons (decl, cleanup, thisblock->data.block.cleanups);
      /* If this block has a cleanup, it belongs in stack_block_stack.  */
      stack_block_stack = thisblock;
      (*interim_eh_hook) (NULL_TREE);
    }
  return 1;
}

/* DECL is an anonymous union.  CLEANUP is a cleanup for DECL.
   DECL_ELTS is the list of elements that belong to DECL's type.
   In each, the TREE_VALUE is a VAR_DECL, and the TREE_PURPOSE a cleanup.  */

void
expand_anon_union_decl (decl, cleanup, decl_elts)
     tree decl, cleanup, decl_elts;
{
  struct nesting *thisblock = block_stack;
  rtx x;

  expand_decl (decl);
  expand_decl_cleanup (decl, cleanup);
  x = DECL_RTL (decl);

  while (decl_elts)
    {
      tree decl_elt = TREE_VALUE (decl_elts);
      tree cleanup_elt = TREE_PURPOSE (decl_elts);
      enum machine_mode mode = TYPE_MODE (TREE_TYPE (decl_elt));

      /* Propagate the union's alignment to the elements.  */
      DECL_ALIGN (decl_elt) = DECL_ALIGN (decl);

      /* If the element has BLKmode and the union doesn't, the union is
         aligned such that the element doesn't need to have BLKmode, so
         change the element's mode to the appropriate one for its size.  */
      if (mode == BLKmode && DECL_MODE (decl) != BLKmode)
	DECL_MODE (decl_elt) = mode
	  = mode_for_size (TREE_INT_CST_LOW (DECL_SIZE (decl_elt)),
			   MODE_INT, 1);

      /* (SUBREG (MEM ...)) at RTL generation time is invalid, so we
         instead create a new MEM rtx with the proper mode.  */
      if (GET_CODE (x) == MEM)
	{
	  if (mode == GET_MODE (x))
	    DECL_RTL (decl_elt) = x;
	  else
	    {
	      DECL_RTL (decl_elt) = gen_rtx (MEM, mode, copy_rtx (XEXP (x, 0)));
	      MEM_IN_STRUCT_P (DECL_RTL (decl_elt)) = MEM_IN_STRUCT_P (x);
	      RTX_UNCHANGING_P (DECL_RTL (decl_elt)) = RTX_UNCHANGING_P (x);
	    }
	}
      else if (GET_CODE (x) == REG)
	{
	  if (mode == GET_MODE (x))
	    DECL_RTL (decl_elt) = x;
	  else
	    DECL_RTL (decl_elt) = gen_rtx (SUBREG, mode, x, 0);
	}
      else
	abort ();

      /* Record the cleanup if there is one.  */

      if (cleanup != 0)
	thisblock->data.block.cleanups
	  = temp_tree_cons (decl_elt, cleanup_elt,
			    thisblock->data.block.cleanups);

      decl_elts = TREE_CHAIN (decl_elts);
    }
}

/* Expand a list of cleanups LIST.
   Elements may be expressions or may be nested lists.

   If DONT_DO is nonnull, then any list-element
   whose TREE_PURPOSE matches DONT_DO is omitted.
   This is sometimes used to avoid a cleanup associated with
   a value that is being returned out of the scope.

   If IN_FIXUP is non-zero, we are generating this cleanup for a fixup
   goto and handle protection regions specially in that case.

   If REACHABLE, we emit code, otherwise just inform the exception handling
   code about this finalization.  */

static void
expand_cleanups (list, dont_do, in_fixup, reachable)
     tree list;
     tree dont_do;
     int in_fixup;
     int reachable;
{
  tree tail;
  for (tail = list; tail; tail = TREE_CHAIN (tail))
    if (dont_do == 0 || TREE_PURPOSE (tail) != dont_do)
      {
	if (TREE_CODE (TREE_VALUE (tail)) == TREE_LIST)
	  expand_cleanups (TREE_VALUE (tail), dont_do, in_fixup, reachable);
	else
	  {
	    if (! in_fixup)
	      (*interim_eh_hook) (TREE_VALUE (tail));

	    if (reachable)
	      {
		/* Cleanups may be run multiple times.  For example,
		   when exiting a binding contour, we expand the
		   cleanups associated with that contour.  When a goto
		   within that binding contour has a target outside that
		   contour, it will expand all cleanups from its scope to
		   the target.  Though the cleanups are expanded multiple
		   times, the control paths are non-overlapping so the
		   cleanups will not be executed twice.  */
		expand_expr (TREE_VALUE (tail), const0_rtx, VOIDmode, 0);
		free_temp_slots ();
	      }
	  }
      }
}

/* Move all cleanups from the current block_stack
   to the containing block_stack, where they are assumed to
   have been created.  If anything can cause a temporary to
   be created, but not expanded for more than one level of
   block_stacks, then this code will have to change.  */

void
move_cleanups_up ()
{
  struct nesting *block = block_stack;
  struct nesting *outer = block->next;

  outer->data.block.cleanups
    = chainon (block->data.block.cleanups,
	       outer->data.block.cleanups);
  block->data.block.cleanups = 0;
}

tree
last_cleanup_this_contour ()
{
  if (block_stack == 0)
    return 0;

  return block_stack->data.block.cleanups;
}

/* Return 1 if there are any pending cleanups at this point.
   If THIS_CONTOUR is nonzero, check the current contour as well.
   Otherwise, look only at the contours that enclose this one.  */

int
any_pending_cleanups (this_contour)
     int this_contour;
{
  struct nesting *block;

  if (block_stack == 0)
    return 0;

  if (this_contour && block_stack->data.block.cleanups != NULL)
    return 1;
  if (block_stack->data.block.cleanups == 0
      && (block_stack->data.block.outer_cleanups == 0
#if 0
	  || block_stack->data.block.outer_cleanups == empty_cleanup_list
#endif
	  ))
    return 0;

  for (block = block_stack->next; block; block = block->next)
    if (block->data.block.cleanups != 0)
      return 1;

  return 0;
}

/* Enter a case (Pascal) or switch (C) statement.
   Push a block onto case_stack and nesting_stack
   to accumulate the case-labels that are seen
   and to record the labels generated for the statement.

   EXIT_FLAG is nonzero if `exit_something' should exit this case stmt.
   Otherwise, this construct is transparent for `exit_something'.

   EXPR is the index-expression to be dispatched on.
   TYPE is its nominal type.  We could simply convert EXPR to this type,
   but instead we take short cuts.  */

void
expand_start_case (exit_flag, expr, type, printname)
     int exit_flag;
     tree expr;
     tree type;
     char *printname;
{
  register struct nesting *thiscase = ALLOC_NESTING ();

  /* Make an entry on case_stack for the case we are entering.  */

  thiscase->next = case_stack;
  thiscase->all = nesting_stack;
  thiscase->depth = ++nesting_depth;
  thiscase->exit_label = exit_flag ? gen_label_rtx () : 0;
  thiscase->data.case_stmt.case_list = 0;
  thiscase->data.case_stmt.index_expr = expr;
  thiscase->data.case_stmt.nominal_type = type;
  thiscase->data.case_stmt.default_label = 0;
  thiscase->data.case_stmt.num_ranges = 0;
  thiscase->data.case_stmt.printname = printname;
  thiscase->data.case_stmt.seenlabel = 0;
  case_stack = thiscase;
  nesting_stack = thiscase;

  if (output_bytecode)
    {
      bc_expand_start_case (thiscase, expr, type, printname);
      return;
    }

  do_pending_stack_adjust ();

  /* Make sure case_stmt.start points to something that won't
     need any transformation before expand_end_case.  */
  if (GET_CODE (get_last_insn ()) != NOTE)
    emit_note (NULL_PTR, NOTE_INSN_DELETED);

  thiscase->data.case_stmt.start = get_last_insn ();
}


/* Enter a case statement. It is assumed that the caller has pushed
   the current context onto the case stack. */

static void
bc_expand_start_case (thiscase, expr, type, printname)
     struct nesting *thiscase;
     tree expr;
     tree type;
     char *printname;
{
  bc_expand_expr (expr);
  bc_expand_conversion (TREE_TYPE (expr), type);

  /* For cases, the skip is a place we jump to that's emitted after
     the size of the jump table is known.  */

  thiscase->data.case_stmt.skip_label = gen_label_rtx ();
  bc_emit_bytecode (jump);
  bc_emit_bytecode_labelref (BYTECODE_BC_LABEL (thiscase->data.case_stmt.skip_label));

#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif
}


/* Start a "dummy case statement" within which case labels are invalid
   and are not connected to any larger real case statement.
   This can be used if you don't want to let a case statement jump
   into the middle of certain kinds of constructs.  */

void
expand_start_case_dummy ()
{
  register struct nesting *thiscase = ALLOC_NESTING ();

  /* Make an entry on case_stack for the dummy.  */

  thiscase->next = case_stack;
  thiscase->all = nesting_stack;
  thiscase->depth = ++nesting_depth;
  thiscase->exit_label = 0;
  thiscase->data.case_stmt.case_list = 0;
  thiscase->data.case_stmt.start = 0;
  thiscase->data.case_stmt.nominal_type = 0;
  thiscase->data.case_stmt.default_label = 0;
  thiscase->data.case_stmt.num_ranges = 0;
  case_stack = thiscase;
  nesting_stack = thiscase;
}

/* End a dummy case statement.  */

void
expand_end_case_dummy ()
{
  POPSTACK (case_stack);
}

/* Return the data type of the index-expression
   of the innermost case statement, or null if none.  */

tree
case_index_expr_type ()
{
  if (case_stack)
    return TREE_TYPE (case_stack->data.case_stmt.index_expr);
  return 0;
}

/* Accumulate one case or default label inside a case or switch statement.
   VALUE is the value of the case (a null pointer, for a default label).
   The function CONVERTER, when applied to arguments T and V,
   converts the value V to the type T.

   If not currently inside a case or switch statement, return 1 and do
   nothing.  The caller will print a language-specific error message.
   If VALUE is a duplicate or overlaps, return 2 and do nothing
   except store the (first) duplicate node in *DUPLICATE.
   If VALUE is out of range, return 3 and do nothing.
   If we are jumping into the scope of a cleaup or var-sized array, return 5.
   Return 0 on success.

   Extended to handle range statements.  */

int
pushcase (value, converter, label, duplicate)
     register tree value;
     tree (*converter) PROTO((tree, tree));
     register tree label;
     tree *duplicate;
{
  register struct case_node **l;
  register struct case_node *n;
  tree index_type;
  tree nominal_type;

  if (output_bytecode)
    return bc_pushcase (value, label);

  /* Fail if not inside a real case statement.  */
  if (! (case_stack && case_stack->data.case_stmt.start))
    return 1;

  if (stack_block_stack
      && stack_block_stack->depth > case_stack->depth)
    return 5;

  index_type = TREE_TYPE (case_stack->data.case_stmt.index_expr);
  nominal_type = case_stack->data.case_stmt.nominal_type;

  /* If the index is erroneous, avoid more problems: pretend to succeed.  */
  if (index_type == error_mark_node)
    return 0;

  /* Convert VALUE to the type in which the comparisons are nominally done.  */
  if (value != 0)
    value = (*converter) (nominal_type, value);

  /* If this is the first label, warn if any insns have been emitted.  */
  if (case_stack->data.case_stmt.seenlabel == 0)
    {
      rtx insn;
      for (insn = case_stack->data.case_stmt.start;
	   insn;
	   insn = NEXT_INSN (insn))
	{
	  if (GET_CODE (insn) == CODE_LABEL)
	    break;
	  if (GET_CODE (insn) != NOTE
	      && (GET_CODE (insn) != INSN || GET_CODE (PATTERN (insn)) != USE))
	    {
	      warning ("unreachable code at beginning of %s",
		       case_stack->data.case_stmt.printname);
	      break;
	    }
	}
    }
  case_stack->data.case_stmt.seenlabel = 1;

  /* Fail if this value is out of range for the actual type of the index
     (which may be narrower than NOMINAL_TYPE).  */
  if (value != 0 && ! int_fits_type_p (value, index_type))
    return 3;

  /* Fail if this is a duplicate or overlaps another entry.  */
  if (value == 0)
    {
      if (case_stack->data.case_stmt.default_label != 0)
	{
	  *duplicate = case_stack->data.case_stmt.default_label;
	  return 2;
	}
      case_stack->data.case_stmt.default_label = label;
    }
  else
    {
      /* Find the elt in the chain before which to insert the new value,
	 to keep the chain sorted in increasing order.
	 But report an error if this element is a duplicate.  */
      for (l = &case_stack->data.case_stmt.case_list;
	   /* Keep going past elements distinctly less than VALUE.  */
	   *l != 0 && tree_int_cst_lt ((*l)->high, value);
	   l = &(*l)->right)
	;
      if (*l)
	{
	  /* Element we will insert before must be distinctly greater;
	     overlap means error.  */
	  if (! tree_int_cst_lt (value, (*l)->low))
	    {
	      *duplicate = (*l)->code_label;
	      return 2;
	    }
	}

      /* Add this label to the chain, and succeed.
	 Copy VALUE so it is on temporary rather than momentary
	 obstack and will thus survive till the end of the case statement.  */
      n = (struct case_node *) oballoc (sizeof (struct case_node));
      n->left = 0;
      n->right = *l;
      n->high = n->low = copy_node (value);
      n->code_label = label;
      *l = n;
    }

  expand_label (label);
  return 0;
}

/* Like pushcase but this case applies to all values
   between VALUE1 and VALUE2 (inclusive).
   The return value is the same as that of pushcase
   but there is one additional error code:
   4 means the specified range was empty.  */

int
pushcase_range (value1, value2, converter, label, duplicate)
     register tree value1, value2;
     tree (*converter) PROTO((tree, tree));
     register tree label;
     tree *duplicate;
{
  register struct case_node **l;
  register struct case_node *n;
  tree index_type;
  tree nominal_type;

  /* Fail if not inside a real case statement.  */
  if (! (case_stack && case_stack->data.case_stmt.start))
    return 1;

  if (stack_block_stack
      && stack_block_stack->depth > case_stack->depth)
    return 5;

  index_type = TREE_TYPE (case_stack->data.case_stmt.index_expr);
  nominal_type = case_stack->data.case_stmt.nominal_type;

  /* If the index is erroneous, avoid more problems: pretend to succeed.  */
  if (index_type == error_mark_node)
    return 0;

  /* If this is the first label, warn if any insns have been emitted.  */
  if (case_stack->data.case_stmt.seenlabel == 0)
    {
      rtx insn;
      for (insn = case_stack->data.case_stmt.start;
	   insn;
	   insn = NEXT_INSN (insn))
	{
	  if (GET_CODE (insn) == CODE_LABEL)
	    break;
	  if (GET_CODE (insn) != NOTE
	      && (GET_CODE (insn) != INSN || GET_CODE (PATTERN (insn)) != USE))
	    {
	      warning ("unreachable code at beginning of %s",
		       case_stack->data.case_stmt.printname);
	      break;
	    }
	}
    }
  case_stack->data.case_stmt.seenlabel = 1;

  /* Convert VALUEs to type in which the comparisons are nominally done.  */
  if (value1 == 0)  /* Negative infinity. */
    value1 = TYPE_MIN_VALUE(index_type);
  value1 = (*converter) (nominal_type, value1);

  if (value2 == 0)  /* Positive infinity. */
    value2 = TYPE_MAX_VALUE(index_type);
  value2 = (*converter) (nominal_type, value2);

  /* Fail if these values are out of range.  */
  if (! int_fits_type_p (value1, index_type))
    return 3;

  if (! int_fits_type_p (value2, index_type))
    return 3;

  /* Fail if the range is empty.  */
  if (tree_int_cst_lt (value2, value1))
    return 4;

  /* If the bounds are equal, turn this into the one-value case.  */
  if (tree_int_cst_equal (value1, value2))
    return pushcase (value1, converter, label, duplicate);

  /* Find the elt in the chain before which to insert the new value,
     to keep the chain sorted in increasing order.
     But report an error if this element is a duplicate.  */
  for (l = &case_stack->data.case_stmt.case_list;
       /* Keep going past elements distinctly less than this range.  */
       *l != 0 && tree_int_cst_lt ((*l)->high, value1);
       l = &(*l)->right)
    ;
  if (*l)
    {
      /* Element we will insert before must be distinctly greater;
	 overlap means error.  */
      if (! tree_int_cst_lt (value2, (*l)->low))
	{
	  *duplicate = (*l)->code_label;
	  return 2;
	}
    }

  /* Add this label to the chain, and succeed.
     Copy VALUE1, VALUE2 so they are on temporary rather than momentary
     obstack and will thus survive till the end of the case statement.  */

  n = (struct case_node *) oballoc (sizeof (struct case_node));
  n->left = 0;
  n->right = *l;
  n->low = copy_node (value1);
  n->high = copy_node (value2);
  n->code_label = label;
  *l = n;

  expand_label (label);

  case_stack->data.case_stmt.num_ranges++;

  return 0;
}


/* Accumulate one case or default label; VALUE is the value of the
   case, or nil for a default label.  If not currently inside a case,
   return 1 and do nothing.  If VALUE is a duplicate or overlaps, return
   2 and do nothing.  If VALUE is out of range, return 3 and do nothing.
   Return 0 on success.  This function is a leftover from the earlier
   bytecode compiler, which was based on gcc 1.37.  It should be
   merged into pushcase. */

static int
bc_pushcase (value, label)
     tree value;
     tree label;
{
  struct nesting *thiscase = case_stack;
  struct case_node *case_label, *new_label;

  if (! thiscase)
    return 1;

  /* Fail if duplicate, overlap, or out of type range.  */
  if (value)
    {
      value = convert (thiscase->data.case_stmt.nominal_type, value);
      if (! int_fits_type_p (value, thiscase->data.case_stmt.nominal_type))
	return 3;

      for (case_label = thiscase->data.case_stmt.case_list;
	   case_label->left; case_label = case_label->left)
	if (! tree_int_cst_lt (case_label->left->high, value))
	  break;

      if (case_label != thiscase->data.case_stmt.case_list
	  && ! tree_int_cst_lt (case_label->high, value)
	  || case_label->left && ! tree_int_cst_lt (value, case_label->left->low))
	return 2;

      new_label = (struct case_node *) oballoc (sizeof (struct case_node));
      new_label->low = new_label->high = copy_node (value);
      new_label->code_label = label;
      new_label->left = case_label->left;

      case_label->left = new_label;
      thiscase->data.case_stmt.num_ranges++;
    }
  else
    {
      if (thiscase->data.case_stmt.default_label)
	return 2;
      thiscase->data.case_stmt.default_label = label;
    }

  expand_label (label);
  return 0;
}

/* Returns the number of possible values of TYPE.
   Returns -1 if the number is unknown or variable.
   Returns -2 if the number does not fit in a HOST_WIDE_INT.
   Sets *SPARENESS to 2 if TYPE is an ENUMERAL_TYPE whose values
   do not increase monotonically (there may be duplicates);
   to 1 if the values increase monotonically, but not always by 1;
   otherwise sets it to 0.  */

HOST_WIDE_INT
all_cases_count (type, spareness)
     tree type;
     int *spareness;
{
  HOST_WIDE_INT count, count_high = 0;
  *spareness = 0;

  switch (TREE_CODE (type))
    {
      tree t;
    case BOOLEAN_TYPE:
      count = 2;
      break;
    case CHAR_TYPE:
      count = 1 << BITS_PER_UNIT;
      break;
    default:
    case INTEGER_TYPE:
      if (TREE_CODE (TYPE_MIN_VALUE (type)) != INTEGER_CST
	  || TREE_CODE (TYPE_MAX_VALUE (type)) != INTEGER_CST)
	return -1;
      else
	{
	  /* count
	     = TREE_INT_CST_LOW (TYPE_MAX_VALUE (type))
	     - TREE_INT_CST_LOW (TYPE_MIN_VALUE (type)) + 1
	     but with overflow checking. */
	  tree mint = TYPE_MIN_VALUE (type);
	  tree maxt = TYPE_MAX_VALUE (type);
	  HOST_WIDE_INT lo, hi;
	  neg_double(TREE_INT_CST_LOW (mint), TREE_INT_CST_HIGH (mint),
		     &lo, &hi);
	  add_double(TREE_INT_CST_LOW (maxt), TREE_INT_CST_HIGH (maxt),
		     lo, hi, &lo, &hi);
	  add_double (lo, hi, 1, 0, &lo, &hi);
	  if (hi != 0 || lo < 0)
	    return -2;
	  count = lo;
	}
      break;
    case ENUMERAL_TYPE:
      count = 0;
      for (t = TYPE_VALUES (type); t != NULL_TREE; t = TREE_CHAIN (t))
	{
	  if (TREE_CODE (TYPE_MIN_VALUE (type)) != INTEGER_CST
	      || TREE_CODE (TREE_VALUE (t)) != INTEGER_CST
	      || TREE_INT_CST_LOW (TYPE_MIN_VALUE (type)) + count
	      != TREE_INT_CST_LOW (TREE_VALUE (t)))
	    *spareness = 1;
	  count++;
	}
      if (*spareness == 1)
	{
	  tree prev = TREE_VALUE (TYPE_VALUES (type));
	  for (t = TYPE_VALUES (type); t = TREE_CHAIN (t), t != NULL_TREE; )
	    {
	      if (! tree_int_cst_lt (prev, TREE_VALUE (t)))
		{
		  *spareness = 2;
		  break;
		}
	      prev = TREE_VALUE (t);
	    }
	  
	}
    }
  return count;
}


#define BITARRAY_TEST(ARRAY, INDEX) \
  ((ARRAY)[(unsigned)(INDEX) / HOST_BITS_PER_CHAR]\
			  & (1 << ((unsigned)(INDEX) % HOST_BITS_PER_CHAR)))
#define BITARRAY_SET(ARRAY, INDEX) \
  ((ARRAY)[(unsigned)(INDEX) / HOST_BITS_PER_CHAR]\
			  |= 1 << ((unsigned)(INDEX) % HOST_BITS_PER_CHAR))

/* Set the elements of the bitstring CASES_SEEN (which has length COUNT),
   with the case values we have seen, assuming the case expression
   has the given TYPE.
   SPARSENESS is as determined by all_cases_count.

   The time needed is proportional to COUNT, unless
   SPARSENESS is 2, in which case quadratic time is needed.  */

void
mark_seen_cases (type, cases_seen, count, sparseness)
     tree type;
     unsigned char *cases_seen;
     long count;
     int sparseness;
{
  long i;

  tree next_node_to_try = NULL_TREE;
  long next_node_offset = 0;

  register struct case_node *n;
  tree val = make_node (INTEGER_CST);
  TREE_TYPE (val) = type;
  for (n = case_stack->data.case_stmt.case_list; n;
       n = n->right)
    {
      TREE_INT_CST_LOW (val) = TREE_INT_CST_LOW (n->low);
      TREE_INT_CST_HIGH (val) = TREE_INT_CST_HIGH (n->low);
      while ( ! tree_int_cst_lt (n->high, val))
	{
	  /* Calculate (into xlo) the "offset" of the integer (val).
	     The element with lowest value has offset 0, the next smallest
	     element has offset 1, etc.  */

	  HOST_WIDE_INT xlo, xhi;
	  tree t;
	  if (sparseness == 2)
	    {
	      /* This less efficient loop is only needed to handle
		 duplicate case values (multiple enum constants
		 with the same value).  */
	      for (t = TYPE_VALUES (type), xlo = 0;  t != NULL_TREE;
		   t = TREE_CHAIN (t), xlo++)
		{
		  if (tree_int_cst_equal (val, TREE_VALUE (t)))
		    BITARRAY_SET (cases_seen, xlo);
		}
	    }
	  else
	    {
	      if (sparseness && TYPE_VALUES (type) != NULL_TREE)
		{
		  /* The TYPE_VALUES will be in increasing order, so
		     starting searching where we last ended.  */
		  t = next_node_to_try;
		  xlo = next_node_offset;
		  xhi = 0;
		  for (;;)
		    {
		      if (t == NULL_TREE)
			{
			  t = TYPE_VALUES (type);
			  xlo = 0;
			}
		      if (tree_int_cst_equal (val, TREE_VALUE (t)))
			{
			  next_node_to_try = TREE_CHAIN (t);
			  next_node_offset = xlo + 1;
			  break;
			}
		      xlo++;
		      t = TREE_CHAIN (t);
		      if (t == next_node_to_try)
			break;
		    }
		}
	      else
		{
		  t = TYPE_MIN_VALUE (type);
		  if (t)
		    neg_double (TREE_INT_CST_LOW (t), TREE_INT_CST_HIGH (t),
				&xlo, &xhi);
		  else
		    xlo = xhi = 0;
		  add_double (xlo, xhi,
			      TREE_INT_CST_LOW (val), TREE_INT_CST_HIGH (val),
			      &xlo, &xhi);
		}
	      
	      if (xhi == 0 && xlo >= 0 && xlo < count)
		BITARRAY_SET (cases_seen, xlo);
	    }
	  add_double (TREE_INT_CST_LOW (val), TREE_INT_CST_HIGH (val),
		      1, 0,
		      &TREE_INT_CST_LOW (val), &TREE_INT_CST_HIGH (val));
	}
    }
}

/* Called when the index of a switch statement is an enumerated type
   and there is no default label.

   Checks that all enumeration literals are covered by the case
   expressions of a switch.  Also, warn if there are any extra
   switch cases that are *not* elements of the enumerated type.

   If all enumeration literals were covered by the case expressions,
   turn one of the expressions into the default expression since it should
   not be possible to fall through such a switch.  */

void
check_for_full_enumeration_handling (type)
     tree type;
{
  register struct case_node *n;
  register struct case_node **l;
  register tree chain;
  int all_values = 1;

  /* True iff the selector type is a numbered set mode. */
  int sparseness = 0;

  /* The number of possible selector values. */
  HOST_WIDE_INT size;

  /* For each possible selector value. a one iff it has been matched
     by a case value alternative. */
  unsigned char *cases_seen;

  /* The allocated size of cases_seen, in chars. */
  long bytes_needed;
  tree t;

  if (output_bytecode)
    {
      bc_check_for_full_enumeration_handling (type);
      return;
    }

  if (! warn_switch)
    return;

  size = all_cases_count (type, &sparseness);
  bytes_needed = (size + HOST_BITS_PER_CHAR) / HOST_BITS_PER_CHAR;

  if (size > 0 && size < 600000
      /* We deliberately use malloc here - not xmalloc. */
      && (cases_seen = (unsigned char *) malloc (bytes_needed)) != NULL)
    {
      long i;
      tree v = TYPE_VALUES (type);
      bzero (cases_seen, bytes_needed);

      /* The time complexity of this code is normally O(N), where
	 N being the number of members in the enumerated type.
	 However, if type is a ENUMERAL_TYPE whose values do not
	 increase monotonically, quadratic time may be needed. */

      mark_seen_cases (type, cases_seen, size, sparseness);

      for (i = 0;  v != NULL_TREE && i < size; i++, v = TREE_CHAIN (v))
	{
	  if (BITARRAY_TEST(cases_seen, i) == 0)
	    warning ("enumeration value `%s' not handled in switch",
		     IDENTIFIER_POINTER (TREE_PURPOSE (v)));
	}

      free (cases_seen);
    }

  /* Now we go the other way around; we warn if there are case
     expressions that don't correspond to enumerators.  This can
     occur since C and C++ don't enforce type-checking of
     assignments to enumeration variables. */

  if (warn_switch)
    for (n = case_stack->data.case_stmt.case_list; n; n = n->right)
      {
	for (chain = TYPE_VALUES (type);
	     chain && !tree_int_cst_equal (n->low, TREE_VALUE (chain));
	     chain = TREE_CHAIN (chain))
	  ;

	if (!chain)
	  {
	    if (TYPE_NAME (type) == 0)
	      warning ("case value `%d' not in enumerated type",
		       TREE_INT_CST_LOW (n->low));
	    else
	      warning ("case value `%d' not in enumerated type `%s'",
		       TREE_INT_CST_LOW (n->low),
		       IDENTIFIER_POINTER ((TREE_CODE (TYPE_NAME (type))
					    == IDENTIFIER_NODE)
					   ? TYPE_NAME (type)
					   : DECL_NAME (TYPE_NAME (type))));
	  }
	if (!tree_int_cst_equal (n->low, n->high))
	  {
	    for (chain = TYPE_VALUES (type);
		 chain && !tree_int_cst_equal (n->high, TREE_VALUE (chain));
		 chain = TREE_CHAIN (chain))
	      ;

	    if (!chain)
	      {
		if (TYPE_NAME (type) == 0)
		  warning ("case value `%d' not in enumerated type",
			   TREE_INT_CST_LOW (n->high));
		else
		  warning ("case value `%d' not in enumerated type `%s'",
			   TREE_INT_CST_LOW (n->high),
			   IDENTIFIER_POINTER ((TREE_CODE (TYPE_NAME (type))
						== IDENTIFIER_NODE)
					       ? TYPE_NAME (type)
					       : DECL_NAME (TYPE_NAME (type))));
	      }
	  }
      }

#if 0
  /* ??? This optimization is disabled because it causes valid programs to
     fail.  ANSI C does not guarantee that an expression with enum type
     will have a value that is the same as one of the enumeration literals.  */

  /* If all values were found as case labels, make one of them the default
     label.  Thus, this switch will never fall through.  We arbitrarily pick
     the last one to make the default since this is likely the most
     efficient choice.  */

  if (all_values)
    {
      for (l = &case_stack->data.case_stmt.case_list;
	   (*l)->right != 0;
	   l = &(*l)->right)
	;

      case_stack->data.case_stmt.default_label = (*l)->code_label;
      *l = 0;
    }
#endif /* 0 */
}


/* Check that all enumeration literals are covered by the case
   expressions of a switch.  Also warn if there are any cases
   that are not elements of the enumerated type.  */

static void
bc_check_for_full_enumeration_handling (type)
     tree type;
{
  struct nesting *thiscase = case_stack;
  struct case_node *c;
  tree e;

  /* Check for enums not handled.  */
  for (e = TYPE_VALUES (type); e; e = TREE_CHAIN (e))
    {
      for (c = thiscase->data.case_stmt.case_list->left;
	   c && tree_int_cst_lt (c->high, TREE_VALUE (e));
	   c = c->left)
	;
      if (! (c && tree_int_cst_equal (c->low, TREE_VALUE (e))))
	warning ("enumerated value `%s' not handled in switch",
		 IDENTIFIER_POINTER (TREE_PURPOSE (e)));
    }

  /* Check for cases not in the enumeration.  */
  for (c = thiscase->data.case_stmt.case_list->left; c; c = c->left)
    {
      for (e = TYPE_VALUES (type);
	   e && !tree_int_cst_equal (c->low, TREE_VALUE (e));
	   e = TREE_CHAIN (e))
	;
      if (! e)
	warning ("case value `%d' not in enumerated type `%s'",
		 TREE_INT_CST_LOW (c->low),
		 IDENTIFIER_POINTER (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE
				     ? TYPE_NAME (type)
				     : DECL_NAME (TYPE_NAME (type))));
    }
}

/* Terminate a case (Pascal) or switch (C) statement
   in which ORIG_INDEX is the expression to be tested.
   Generate the code to test it and jump to the right place.  */

void
expand_end_case (orig_index)
     tree orig_index;
{
  tree minval, maxval, range, orig_minval;
  rtx default_label = 0;
  register struct case_node *n;
  int count;
  rtx index;
  rtx table_label;
  int ncases;
  rtx *labelvec;
  register int i;
  rtx before_case;
  register struct nesting *thiscase = case_stack;
  tree index_expr, index_type;
  int unsignedp;

  if (output_bytecode)
    {
      bc_expand_end_case (orig_index);
      return;
    }

  table_label = gen_label_rtx ();
  index_expr = thiscase->data.case_stmt.index_expr;
  index_type = TREE_TYPE (index_expr);
  unsignedp = TREE_UNSIGNED (index_type);

  do_pending_stack_adjust ();

  /* An ERROR_MARK occurs for various reasons including invalid data type.  */
  if (index_type != error_mark_node)
    {
      /* If switch expression was an enumerated type, check that all
	 enumeration literals are covered by the cases.
	 No sense trying this if there's a default case, however.  */

      if (!thiscase->data.case_stmt.default_label
	  && TREE_CODE (TREE_TYPE (orig_index)) == ENUMERAL_TYPE
	  && TREE_CODE (index_expr) != INTEGER_CST)
	check_for_full_enumeration_handling (TREE_TYPE (orig_index));

      /* If this is the first label, warn if any insns have been emitted.  */
      if (thiscase->data.case_stmt.seenlabel == 0)
	{
	  rtx insn;
	  for (insn = get_last_insn ();
	       insn != case_stack->data.case_stmt.start;
	       insn = PREV_INSN (insn))
	    if (GET_CODE (insn) != NOTE
	        && (GET_CODE (insn) != INSN || GET_CODE (PATTERN (insn))!= USE))
	      {
		warning ("unreachable code at beginning of %s",
			 case_stack->data.case_stmt.printname);
		break;
	      }
	}

      /* If we don't have a default-label, create one here,
	 after the body of the switch.  */
      if (thiscase->data.case_stmt.default_label == 0)
	{
	  thiscase->data.case_stmt.default_label
	    = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
	  expand_label (thiscase->data.case_stmt.default_label);
	}
      default_label = label_rtx (thiscase->data.case_stmt.default_label);

      before_case = get_last_insn ();

      /* Simplify the case-list before we count it.  */
      group_case_nodes (thiscase->data.case_stmt.case_list);

      /* Get upper and lower bounds of case values.
	 Also convert all the case values to the index expr's data type.  */

      count = 0;
      for (n = thiscase->data.case_stmt.case_list; n; n = n->right)
	{
	  /* Check low and high label values are integers.  */
	  if (TREE_CODE (n->low) != INTEGER_CST)
	    abort ();
	  if (TREE_CODE (n->high) != INTEGER_CST)
	    abort ();

	  n->low = convert (index_type, n->low);
	  n->high = convert (index_type, n->high);

	  /* Count the elements and track the largest and smallest
	     of them (treating them as signed even if they are not).  */
	  if (count++ == 0)
	    {
	      minval = n->low;
	      maxval = n->high;
	    }
	  else
	    {
	      if (INT_CST_LT (n->low, minval))
		minval = n->low;
	      if (INT_CST_LT (maxval, n->high))
		maxval = n->high;
	    }
	  /* A range counts double, since it requires two compares.  */
	  if (! tree_int_cst_equal (n->low, n->high))
	    count++;
	}

      orig_minval = minval;

      /* Compute span of values.  */
      if (count != 0)
	range = fold (build (MINUS_EXPR, index_type, maxval, minval));

      if (count == 0)
	{
	  expand_expr (index_expr, const0_rtx, VOIDmode, 0);
	  emit_queue ();
	  emit_jump (default_label);
	}

      /* If range of values is much bigger than number of values,
	 make a sequence of conditional branches instead of a dispatch.
	 If the switch-index is a constant, do it this way
	 because we can optimize it.  */

#ifndef CASE_VALUES_THRESHOLD
#ifdef HAVE_casesi
#define CASE_VALUES_THRESHOLD (HAVE_casesi ? 4 : 5)
#else
      /* If machine does not have a case insn that compares the
	 bounds, this means extra overhead for dispatch tables
	 which raises the threshold for using them.  */
#define CASE_VALUES_THRESHOLD 5
#endif /* HAVE_casesi */
#endif /* CASE_VALUES_THRESHOLD */

      else if (TREE_INT_CST_HIGH (range) != 0
	       || count < CASE_VALUES_THRESHOLD
	       || ((unsigned HOST_WIDE_INT) (TREE_INT_CST_LOW (range))
		   > 10 * count)
	       || TREE_CODE (index_expr) == INTEGER_CST
	       /* These will reduce to a constant.  */
	       || (TREE_CODE (index_expr) == CALL_EXPR
		   && TREE_CODE (TREE_OPERAND (index_expr, 0)) == ADDR_EXPR
		   && TREE_CODE (TREE_OPERAND (TREE_OPERAND (index_expr, 0), 0)) == FUNCTION_DECL
		   && DECL_FUNCTION_CODE (TREE_OPERAND (TREE_OPERAND (index_expr, 0), 0)) == BUILT_IN_CLASSIFY_TYPE)
	       || (TREE_CODE (index_expr) == COMPOUND_EXPR
		   && TREE_CODE (TREE_OPERAND (index_expr, 1)) == INTEGER_CST))
	{
	  index = expand_expr (index_expr, NULL_RTX, VOIDmode, 0);

	  /* If the index is a short or char that we do not have
	     an insn to handle comparisons directly, convert it to
	     a full integer now, rather than letting each comparison
	     generate the conversion.  */

	  if (GET_MODE_CLASS (GET_MODE (index)) == MODE_INT
	      && (cmp_optab->handlers[(int) GET_MODE(index)].insn_code
		  == CODE_FOR_nothing))
	    {
	      enum machine_mode wider_mode;
	      for (wider_mode = GET_MODE (index); wider_mode != VOIDmode;
		   wider_mode = GET_MODE_WIDER_MODE (wider_mode))
		if (cmp_optab->handlers[(int) wider_mode].insn_code
		    != CODE_FOR_nothing)
		  {
		    index = convert_to_mode (wider_mode, index, unsignedp);
		    break;
		  }
	    }

	  emit_queue ();
	  do_pending_stack_adjust ();

	  index = protect_from_queue (index, 0);
	  if (GET_CODE (index) == MEM)
	    index = copy_to_reg (index);
	  if (GET_CODE (index) == CONST_INT
	      || TREE_CODE (index_expr) == INTEGER_CST)
	    {
	      /* Make a tree node with the proper constant value
		 if we don't already have one.  */
	      if (TREE_CODE (index_expr) != INTEGER_CST)
		{
		  index_expr
		    = build_int_2 (INTVAL (index),
				   unsignedp || INTVAL (index) >= 0 ? 0 : -1);
		  index_expr = convert (index_type, index_expr);
		}

	      /* For constant index expressions we need only
		 issue a unconditional branch to the appropriate
		 target code.  The job of removing any unreachable
		 code is left to the optimisation phase if the
		 "-O" option is specified.  */
	      for (n = thiscase->data.case_stmt.case_list; n; n = n->right)
		if (! tree_int_cst_lt (index_expr, n->low)
		    && ! tree_int_cst_lt (n->high, index_expr))
		  break;

	      if (n)
		emit_jump (label_rtx (n->code_label));
	      else
		emit_jump (default_label);
	    }
	  else
	    {
	      /* If the index expression is not constant we generate
		 a binary decision tree to select the appropriate
		 target code.  This is done as follows:

		 The list of cases is rearranged into a binary tree,
		 nearly optimal assuming equal probability for each case.

		 The tree is transformed into RTL, eliminating
		 redundant test conditions at the same time.

		 If program flow could reach the end of the
		 decision tree an unconditional jump to the
		 default code is emitted.  */

	      use_cost_table
		= (TREE_CODE (TREE_TYPE (orig_index)) != ENUMERAL_TYPE
		   && estimate_case_costs (thiscase->data.case_stmt.case_list));
	      balance_case_nodes (&thiscase->data.case_stmt.case_list, 
				  NULL_PTR);
	      emit_case_nodes (index, thiscase->data.case_stmt.case_list,
			       default_label, index_type);
	      emit_jump_if_reachable (default_label);
	    }
	}
      else
	{
	  int win = 0;
#ifdef HAVE_casesi
	  if (HAVE_casesi)
	    {
	      enum machine_mode index_mode = SImode;
	      int index_bits = GET_MODE_BITSIZE (index_mode);
	      rtx op1, op2;
	      enum machine_mode op_mode;

	      /* Convert the index to SImode.  */
	      if (GET_MODE_BITSIZE (TYPE_MODE (index_type))
		  > GET_MODE_BITSIZE (index_mode))
		{
		  enum machine_mode omode = TYPE_MODE (index_type);
		  rtx rangertx = expand_expr (range, NULL_RTX, VOIDmode, 0);

		  /* We must handle the endpoints in the original mode.  */
		  index_expr = build (MINUS_EXPR, index_type,
				      index_expr, minval);
		  minval = integer_zero_node;
		  index = expand_expr (index_expr, NULL_RTX, VOIDmode, 0);
		  emit_cmp_insn (rangertx, index, LTU, NULL_RTX, omode, 1, 0);
		  emit_jump_insn (gen_bltu (default_label));
		  /* Now we can safely truncate.  */
		  index = convert_to_mode (index_mode, index, 0);
		}
	      else
		{
		  if (TYPE_MODE (index_type) != index_mode)
		    {
		      index_expr = convert (type_for_size (index_bits, 0),
					    index_expr);
		      index_type = TREE_TYPE (index_expr);
		    }

		  index = expand_expr (index_expr, NULL_RTX, VOIDmode, 0);
		}
	      emit_queue ();
	      index = protect_from_queue (index, 0);
	      do_pending_stack_adjust ();

	      op_mode = insn_operand_mode[(int)CODE_FOR_casesi][0];
	      if (! (*insn_operand_predicate[(int)CODE_FOR_casesi][0])
		  (index, op_mode))
		index = copy_to_mode_reg (op_mode, index);

	      op1 = expand_expr (minval, NULL_RTX, VOIDmode, 0);

	      op_mode = insn_operand_mode[(int)CODE_FOR_casesi][1];
	      if (! (*insn_operand_predicate[(int)CODE_FOR_casesi][1])
		  (op1, op_mode))
		op1 = copy_to_mode_reg (op_mode, op1);

	      op2 = expand_expr (range, NULL_RTX, VOIDmode, 0);

	      op_mode = insn_operand_mode[(int)CODE_FOR_casesi][2];
	      if (! (*insn_operand_predicate[(int)CODE_FOR_casesi][2])
		  (op2, op_mode))
		op2 = copy_to_mode_reg (op_mode, op2);

	      emit_jump_insn (gen_casesi (index, op1, op2,
					  table_label, default_label));
	      win = 1;
	    }
#endif
#ifdef HAVE_tablejump
	  if (! win && HAVE_tablejump)
	    {
	      index_expr = convert (thiscase->data.case_stmt.nominal_type,
				    fold (build (MINUS_EXPR, index_type,
						 index_expr, minval)));
	      index_type = TREE_TYPE (index_expr);
	      index = expand_expr (index_expr, NULL_RTX, VOIDmode, 0);
	      emit_queue ();
	      index = protect_from_queue (index, 0);
	      do_pending_stack_adjust ();

	      do_tablejump (index, TYPE_MODE (index_type),
			    expand_expr (range, NULL_RTX, VOIDmode, 0),
			    table_label, default_label);
	      win = 1;
	    }
#endif
	  if (! win)
	    abort ();

	  /* Get table of labels to jump to, in order of case index.  */

	  ncases = TREE_INT_CST_LOW (range) + 1;
	  labelvec = (rtx *) alloca (ncases * sizeof (rtx));
	  bzero ((char *) labelvec, ncases * sizeof (rtx));

	  for (n = thiscase->data.case_stmt.case_list; n; n = n->right)
	    {
	      register HOST_WIDE_INT i
		= TREE_INT_CST_LOW (n->low) - TREE_INT_CST_LOW (orig_minval);

	      while (1)
		{
		  labelvec[i]
		    = gen_rtx (LABEL_REF, Pmode, label_rtx (n->code_label));
		  if (i + TREE_INT_CST_LOW (orig_minval)
		      == TREE_INT_CST_LOW (n->high))
		    break;
		  i++;
		}
	    }

	  /* Fill in the gaps with the default.  */
	  for (i = 0; i < ncases; i++)
	    if (labelvec[i] == 0)
	      labelvec[i] = gen_rtx (LABEL_REF, Pmode, default_label);

	  /* Output the table */
	  emit_label (table_label);

	  /* This would be a lot nicer if CASE_VECTOR_PC_RELATIVE
	     were an expression, instead of an #ifdef/#ifndef.  */
	  if (
#ifdef CASE_VECTOR_PC_RELATIVE
	      1 ||
#endif
	      flag_pic)
	    emit_jump_insn (gen_rtx (ADDR_DIFF_VEC, CASE_VECTOR_MODE,
				     gen_rtx (LABEL_REF, Pmode, table_label),
				     gen_rtvec_v (ncases, labelvec)));
	  else
	    emit_jump_insn (gen_rtx (ADDR_VEC, CASE_VECTOR_MODE,
				     gen_rtvec_v (ncases, labelvec)));

	  /* If the case insn drops through the table,
	     after the table we must jump to the default-label.
	     Otherwise record no drop-through after the table.  */
#ifdef CASE_DROPS_THROUGH
	  emit_jump (default_label);
#else
	  emit_barrier ();
#endif
	}

      before_case = squeeze_notes (NEXT_INSN (before_case), get_last_insn ());
      reorder_insns (before_case, get_last_insn (),
		     thiscase->data.case_stmt.start);
    }

  if (thiscase->exit_label)
    emit_label (thiscase->exit_label);

  POPSTACK (case_stack);

  free_temp_slots ();
}


/* Terminate a case statement.  EXPR is the original index
   expression.  */

static void
bc_expand_end_case (expr)
     tree expr;
{
  struct nesting *thiscase = case_stack;
  enum bytecode_opcode opcode;
  struct bc_label *jump_label;
  struct case_node *c;

  bc_emit_bytecode (jump);
  bc_emit_bytecode_labelref (BYTECODE_BC_LABEL (thiscase->exit_label));

#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif

  /* Now that the size of the jump table is known, emit the actual
     indexed jump instruction.  */
  bc_emit_bytecode_labeldef (BYTECODE_BC_LABEL (thiscase->data.case_stmt.skip_label));

  opcode = TYPE_MODE (thiscase->data.case_stmt.nominal_type) == SImode
    ? TREE_UNSIGNED (thiscase->data.case_stmt.nominal_type) ? caseSU : caseSI
      : TREE_UNSIGNED (thiscase->data.case_stmt.nominal_type) ? caseDU : caseDI;

  bc_emit_bytecode (opcode);

  /* Now emit the case instructions literal arguments, in order.
     In addition to the value on the stack, it uses:
     1.  The address of the jump table.
     2.  The size of the jump table.
     3.  The default label.  */

  jump_label = bc_get_bytecode_label ();
  bc_emit_bytecode_labelref (jump_label);
  bc_emit_bytecode_const ((char *) &thiscase->data.case_stmt.num_ranges,
			  sizeof thiscase->data.case_stmt.num_ranges);

  if (thiscase->data.case_stmt.default_label)
    bc_emit_bytecode_labelref (BYTECODE_BC_LABEL (DECL_RTL (thiscase->data.case_stmt.default_label)));
  else
    bc_emit_bytecode_labelref (BYTECODE_BC_LABEL (thiscase->exit_label));

  /* Output the jump table.  */

  bc_align_bytecode (3 /* PTR_ALIGN */);
  bc_emit_bytecode_labeldef (jump_label);

  if (TYPE_MODE (thiscase->data.case_stmt.nominal_type) == SImode)
    for (c = thiscase->data.case_stmt.case_list->left; c; c = c->left)
      {
	opcode = TREE_INT_CST_LOW (c->low);
	bc_emit_bytecode_const ((char *) &opcode, sizeof opcode);

	opcode = TREE_INT_CST_LOW (c->high);
	bc_emit_bytecode_const ((char *) &opcode, sizeof opcode);

	bc_emit_bytecode_labelref (BYTECODE_BC_LABEL (DECL_RTL (c->code_label)));
      }
  else
    if (TYPE_MODE (thiscase->data.case_stmt.nominal_type) == DImode)
      for (c = thiscase->data.case_stmt.case_list->left; c; c = c->left)
	{
	  bc_emit_bytecode_DI_const (c->low);
	  bc_emit_bytecode_DI_const (c->high);

	  bc_emit_bytecode_labelref (BYTECODE_BC_LABEL (DECL_RTL (c->code_label)));
	}
    else
      /* Bad mode */
      abort ();

    
  bc_emit_bytecode_labeldef (BYTECODE_BC_LABEL (thiscase->exit_label));

  /* Possibly issue enumeration warnings.  */

  if (!thiscase->data.case_stmt.default_label
      && TREE_CODE (TREE_TYPE (expr)) == ENUMERAL_TYPE
      && TREE_CODE (expr) != INTEGER_CST
      && warn_switch)
    check_for_full_enumeration_handling (TREE_TYPE (expr));


#ifdef DEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif

  POPSTACK (case_stack);
}


/* Return unique bytecode ID. */

int 
bc_new_uid ()
{
  static int bc_uid = 0;

  return (++bc_uid);
}

/* Generate code to jump to LABEL if OP1 and OP2 are equal.  */

static void
do_jump_if_equal (op1, op2, label, unsignedp)
     rtx op1, op2, label;
     int unsignedp;
{
  if (GET_CODE (op1) == CONST_INT
      && GET_CODE (op2) == CONST_INT)
    {
      if (INTVAL (op1) == INTVAL (op2))
	emit_jump (label);
    }
  else
    {
      enum machine_mode mode = GET_MODE (op1);
      if (mode == VOIDmode)
	mode = GET_MODE (op2);
      emit_cmp_insn (op1, op2, EQ, NULL_RTX, mode, unsignedp, 0);
      emit_jump_insn (gen_beq (label));
    }
}

/* Not all case values are encountered equally.  This function
   uses a heuristic to weight case labels, in cases where that
   looks like a reasonable thing to do.

   Right now, all we try to guess is text, and we establish the
   following weights:

	chars above space:	16
	digits:			16
	default:		12
	space, punct:		8
	tab:			4
	newline:		2
	other "\" chars:	1
	remaining chars:	0

   If we find any cases in the switch that are not either -1 or in the range
   of valid ASCII characters, or are control characters other than those
   commonly used with "\", don't treat this switch scanning text.

   Return 1 if these nodes are suitable for cost estimation, otherwise
   return 0.  */

static int
estimate_case_costs (node)
     case_node_ptr node;
{
  tree min_ascii = build_int_2 (-1, -1);
  tree max_ascii = convert (TREE_TYPE (node->high), build_int_2 (127, 0));
  case_node_ptr n;
  int i;

  /* If we haven't already made the cost table, make it now.  Note that the
     lower bound of the table is -1, not zero.  */

  if (cost_table == NULL)
    {
      cost_table = ((short *) xmalloc (129 * sizeof (short))) + 1;
      bzero ((char *) (cost_table - 1), 129 * sizeof (short));

      for (i = 0; i < 128; i++)
	{
	  if (isalnum (i))
	    cost_table[i] = 16;
	  else if (ispunct (i))
	    cost_table[i] = 8;
	  else if (iscntrl (i))
	    cost_table[i] = -1;
	}

      cost_table[' '] = 8;
      cost_table['\t'] = 4;
      cost_table['\0'] = 4;
      cost_table['\n'] = 2;
      cost_table['\f'] = 1;
      cost_table['\v'] = 1;
      cost_table['\b'] = 1;
    }

  /* See if all the case expressions look like text.  It is text if the
     constant is >= -1 and the highest constant is <= 127.  Do all comparisons
     as signed arithmetic since we don't want to ever access cost_table with a
     value less than -1.  Also check that none of the constants in a range
     are strange control characters.  */

  for (n = node; n; n = n->right)
    {
      if ((INT_CST_LT (n->low, min_ascii)) || INT_CST_LT (max_ascii, n->high))
	return 0;

      for (i = TREE_INT_CST_LOW (n->low); i <= TREE_INT_CST_LOW (n->high); i++)
	if (cost_table[i] < 0)
	  return 0;
    }

  /* All interesting values are within the range of interesting
     ASCII characters.  */
  return 1;
}

/* Scan an ordered list of case nodes
   combining those with consecutive values or ranges.

   Eg. three separate entries 1: 2: 3: become one entry 1..3:  */

static void
group_case_nodes (head)
     case_node_ptr head;
{
  case_node_ptr node = head;

  while (node)
    {
      rtx lb = next_real_insn (label_rtx (node->code_label));
      case_node_ptr np = node;

      /* Try to group the successors of NODE with NODE.  */
      while (((np = np->right) != 0)
	     /* Do they jump to the same place?  */
	     && next_real_insn (label_rtx (np->code_label)) == lb
	     /* Are their ranges consecutive?  */
	     && tree_int_cst_equal (np->low,
				    fold (build (PLUS_EXPR,
						 TREE_TYPE (node->high),
						 node->high,
						 integer_one_node)))
	     /* An overflow is not consecutive.  */
	     && tree_int_cst_lt (node->high,
				 fold (build (PLUS_EXPR,
					      TREE_TYPE (node->high),
					      node->high,
					      integer_one_node))))
	{
	  node->high = np->high;
	}
      /* NP is the first node after NODE which can't be grouped with it.
	 Delete the nodes in between, and move on to that node.  */
      node->right = np;
      node = np;
    }
}

/* Take an ordered list of case nodes
   and transform them into a near optimal binary tree,
   on the assumption that any target code selection value is as
   likely as any other.

   The transformation is performed by splitting the ordered
   list into two equal sections plus a pivot.  The parts are
   then attached to the pivot as left and right branches.  Each
   branch is is then transformed recursively.  */

static void
balance_case_nodes (head, parent)
     case_node_ptr *head;
     case_node_ptr parent;
{
  register case_node_ptr np;

  np = *head;
  if (np)
    {
      int cost = 0;
      int i = 0;
      int ranges = 0;
      register case_node_ptr *npp;
      case_node_ptr left;

      /* Count the number of entries on branch.  Also count the ranges.  */

      while (np)
	{
	  if (!tree_int_cst_equal (np->low, np->high))
	    {
	      ranges++;
	      if (use_cost_table)
		cost += cost_table[TREE_INT_CST_LOW (np->high)];
	    }

	  if (use_cost_table)
	    cost += cost_table[TREE_INT_CST_LOW (np->low)];

	  i++;
	  np = np->right;
	}

      if (i > 2)
	{
	  /* Split this list if it is long enough for that to help.  */
	  npp = head;
	  left = *npp;
	  if (use_cost_table)
	    {
	      /* Find the place in the list that bisects the list's total cost,
		 Here I gets half the total cost.  */
	      int n_moved = 0;
	      i = (cost + 1) / 2;
	      while (1)
		{
		  /* Skip nodes while their cost does not reach that amount.  */
		  if (!tree_int_cst_equal ((*npp)->low, (*npp)->high))
		    i -= cost_table[TREE_INT_CST_LOW ((*npp)->high)];
		  i -= cost_table[TREE_INT_CST_LOW ((*npp)->low)];
		  if (i <= 0)
		    break;
		  npp = &(*npp)->right;
		  n_moved += 1;
		}
	      if (n_moved == 0)
		{
		  /* Leave this branch lopsided, but optimize left-hand
		     side and fill in `parent' fields for right-hand side.  */
		  np = *head;
		  np->parent = parent;
		  balance_case_nodes (&np->left, np);
		  for (; np->right; np = np->right)
		    np->right->parent = np;
		  return;
		}
	    }
	  /* If there are just three nodes, split at the middle one.  */
	  else if (i == 3)
	    npp = &(*npp)->right;
	  else
	    {
	      /* Find the place in the list that bisects the list's total cost,
		 where ranges count as 2.
		 Here I gets half the total cost.  */
	      i = (i + ranges + 1) / 2;
	      while (1)
		{
		  /* Skip nodes while their cost does not reach that amount.  */
		  if (!tree_int_cst_equal ((*npp)->low, (*npp)->high))
		    i--;
		  i--;
		  if (i <= 0)
		    break;
		  npp = &(*npp)->right;
		}
	    }
	  *head = np = *npp;
	  *npp = 0;
	  np->parent = parent;
	  np->left = left;

	  /* Optimize each of the two split parts.  */
	  balance_case_nodes (&np->left, np);
	  balance_case_nodes (&np->right, np);
	}
      else
	{
	  /* Else leave this branch as one level,
	     but fill in `parent' fields.  */
	  np = *head;
	  np->parent = parent;
	  for (; np->right; np = np->right)
	    np->right->parent = np;
	}
    }
}

/* Search the parent sections of the case node tree
   to see if a test for the lower bound of NODE would be redundant.
   INDEX_TYPE is the type of the index expression.

   The instructions to generate the case decision tree are
   output in the same order as nodes are processed so it is
   known that if a parent node checks the range of the current
   node minus one that the current node is bounded at its lower
   span.  Thus the test would be redundant.  */

static int
node_has_low_bound (node, index_type)
     case_node_ptr node;
     tree index_type;
{
  tree low_minus_one;
  case_node_ptr pnode;

  /* If the lower bound of this node is the lowest value in the index type,
     we need not test it.  */

  if (tree_int_cst_equal (node->low, TYPE_MIN_VALUE (index_type)))
    return 1;

  /* If this node has a left branch, the value at the left must be less
     than that at this node, so it cannot be bounded at the bottom and
     we need not bother testing any further.  */

  if (node->left)
    return 0;

  low_minus_one = fold (build (MINUS_EXPR, TREE_TYPE (node->low),
			       node->low, integer_one_node));

  /* If the subtraction above overflowed, we can't verify anything.
     Otherwise, look for a parent that tests our value - 1.  */

  if (! tree_int_cst_lt (low_minus_one, node->low))
    return 0;

  for (pnode = node->parent; pnode; pnode = pnode->parent)
    if (tree_int_cst_equal (low_minus_one, pnode->high))
      return 1;

  return 0;
}

/* Search the parent sections of the case node tree
   to see if a test for the upper bound of NODE would be redundant.
   INDEX_TYPE is the type of the index expression.

   The instructions to generate the case decision tree are
   output in the same order as nodes are processed so it is
   known that if a parent node checks the range of the current
   node plus one that the current node is bounded at its upper
   span.  Thus the test would be redundant.  */

static int
node_has_high_bound (node, index_type)
     case_node_ptr node;
     tree index_type;
{
  tree high_plus_one;
  case_node_ptr pnode;

  /* If the upper bound of this node is the highest value in the type
     of the index expression, we need not test against it.  */

  if (tree_int_cst_equal (node->high, TYPE_MAX_VALUE (index_type)))
    return 1;

  /* If this node has a right branch, the value at the right must be greater
     than that at this node, so it cannot be bounded at the top and
     we need not bother testing any further.  */

  if (node->right)
    return 0;

  high_plus_one = fold (build (PLUS_EXPR, TREE_TYPE (node->high),
			       node->high, integer_one_node));

  /* If the addition above overflowed, we can't verify anything.
     Otherwise, look for a parent that tests our value + 1.  */

  if (! tree_int_cst_lt (node->high, high_plus_one))
    return 0;

  for (pnode = node->parent; pnode; pnode = pnode->parent)
    if (tree_int_cst_equal (high_plus_one, pnode->low))
      return 1;

  return 0;
}

/* Search the parent sections of the
   case node tree to see if both tests for the upper and lower
   bounds of NODE would be redundant.  */

static int
node_is_bounded (node, index_type)
     case_node_ptr node;
     tree index_type;
{
  return (node_has_low_bound (node, index_type)
	  && node_has_high_bound (node, index_type));
}

/*  Emit an unconditional jump to LABEL unless it would be dead code.  */

static void
emit_jump_if_reachable (label)
     rtx label;
{
  if (GET_CODE (get_last_insn ()) != BARRIER)
    emit_jump (label);
}

/* Emit step-by-step code to select a case for the value of INDEX.
   The thus generated decision tree follows the form of the
   case-node binary tree NODE, whose nodes represent test conditions.
   INDEX_TYPE is the type of the index of the switch.

   Care is taken to prune redundant tests from the decision tree
   by detecting any boundary conditions already checked by
   emitted rtx.  (See node_has_high_bound, node_has_low_bound
   and node_is_bounded, above.)

   Where the test conditions can be shown to be redundant we emit
   an unconditional jump to the target code.  As a further
   optimization, the subordinates of a tree node are examined to
   check for bounded nodes.  In this case conditional and/or
   unconditional jumps as a result of the boundary check for the
   current node are arranged to target the subordinates associated
   code for out of bound conditions on the current node node.

   We can assume that when control reaches the code generated here,
   the index value has already been compared with the parents
   of this node, and determined to be on the same side of each parent
   as this node is.  Thus, if this node tests for the value 51,
   and a parent tested for 52, we don't need to consider
   the possibility of a value greater than 51.  If another parent
   tests for the value 50, then this node need not test anything.  */

static void
emit_case_nodes (index, node, default_label, index_type)
     rtx index;
     case_node_ptr node;
     rtx default_label;
     tree index_type;
{
  /* If INDEX has an unsigned type, we must make unsigned branches.  */
  int unsignedp = TREE_UNSIGNED (index_type);
  typedef rtx rtx_function ();
  rtx_function *gen_bgt_pat = unsignedp ? gen_bgtu : gen_bgt;
  rtx_function *gen_bge_pat = unsignedp ? gen_bgeu : gen_bge;
  rtx_function *gen_blt_pat = unsignedp ? gen_bltu : gen_blt;
  rtx_function *gen_ble_pat = unsignedp ? gen_bleu : gen_ble;
  enum machine_mode mode = GET_MODE (index);

  /* See if our parents have already tested everything for us.
     If they have, emit an unconditional jump for this node.  */
  if (node_is_bounded (node, index_type))
    emit_jump (label_rtx (node->code_label));

  else if (tree_int_cst_equal (node->low, node->high))
    {
      /* Node is single valued.  First see if the index expression matches
	 this node and then check our children, if any. */

      do_jump_if_equal (index, expand_expr (node->low, NULL_RTX, VOIDmode, 0),
			label_rtx (node->code_label), unsignedp);

      if (node->right != 0 && node->left != 0)
	{
	  /* This node has children on both sides.
	     Dispatch to one side or the other
	     by comparing the index value with this node's value.
	     If one subtree is bounded, check that one first,
	     so we can avoid real branches in the tree.  */

	  if (node_is_bounded (node->right, index_type))
	    {
	      emit_cmp_insn (index, expand_expr (node->high, NULL_RTX,
						 VOIDmode, 0),
			     GT, NULL_RTX, mode, unsignedp, 0);

	      emit_jump_insn ((*gen_bgt_pat) (label_rtx (node->right->code_label)));
	      emit_case_nodes (index, node->left, default_label, index_type);
	    }

	  else if (node_is_bounded (node->left, index_type))
	    {
	      emit_cmp_insn (index, expand_expr (node->high, NULL_RTX,
						 VOIDmode, 0),
			     LT, NULL_RTX, mode, unsignedp, 0);
	      emit_jump_insn ((*gen_blt_pat) (label_rtx (node->left->code_label)));
	      emit_case_nodes (index, node->right, default_label, index_type);
	    }

	  else
	    {
	      /* Neither node is bounded.  First distinguish the two sides;
		 then emit the code for one side at a time.  */

	      tree test_label
		= build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

	      /* See if the value is on the right.  */
	      emit_cmp_insn (index, expand_expr (node->high, NULL_RTX,
						 VOIDmode, 0),
			     GT, NULL_RTX, mode, unsignedp, 0);
	      emit_jump_insn ((*gen_bgt_pat) (label_rtx (test_label)));

	      /* Value must be on the left.
		 Handle the left-hand subtree.  */
	      emit_case_nodes (index, node->left, default_label, index_type);
	      /* If left-hand subtree does nothing,
		 go to default.  */
	      emit_jump_if_reachable (default_label);

	      /* Code branches here for the right-hand subtree.  */
	      expand_label (test_label);
	      emit_case_nodes (index, node->right, default_label, index_type);
	    }
	}

      else if (node->right != 0 && node->left == 0)
	{
	  /* Here we have a right child but no left so we issue conditional
	     branch to default and process the right child.

	     Omit the conditional branch to default if we it avoid only one
	     right child; it costs too much space to save so little time.  */

	  if (node->right->right || node->right->left
	      || !tree_int_cst_equal (node->right->low, node->right->high))
	    {
	      if (!node_has_low_bound (node, index_type))
		{
		  emit_cmp_insn (index, expand_expr (node->high, NULL_RTX,
						     VOIDmode, 0),
				 LT, NULL_RTX, mode, unsignedp, 0);
		  emit_jump_insn ((*gen_blt_pat) (default_label));
		}

	      emit_case_nodes (index, node->right, default_label, index_type);
	    }
	  else
	    /* We cannot process node->right normally
	       since we haven't ruled out the numbers less than
	       this node's value.  So handle node->right explicitly.  */
	    do_jump_if_equal (index,
			      expand_expr (node->right->low, NULL_RTX,
					   VOIDmode, 0),
			      label_rtx (node->right->code_label), unsignedp);
	}

      else if (node->right == 0 && node->left != 0)
	{
	  /* Just one subtree, on the left.  */

#if 0 /* The following code and comment were formerly part
	 of the condition here, but they didn't work
	 and I don't understand what the idea was.  -- rms.  */
	  /* If our "most probable entry" is less probable
	     than the default label, emit a jump to
	     the default label using condition codes
	     already lying around.  With no right branch,
	     a branch-greater-than will get us to the default
	     label correctly.  */
	  if (use_cost_table
	       && cost_table[TREE_INT_CST_LOW (node->high)] < 12)
	    ;
#endif /* 0 */
 	  if (node->left->left || node->left->right
	      || !tree_int_cst_equal (node->left->low, node->left->high))
	    {
	      if (!node_has_high_bound (node, index_type))
		{
		  emit_cmp_insn (index, expand_expr (node->high, NULL_RTX,
						     VOIDmode, 0),
				 GT, NULL_RTX, mode, unsignedp, 0);
		  emit_jump_insn ((*gen_bgt_pat) (default_label));
		}

	      emit_case_nodes (index, node->left, default_label, index_type);
	    }
	  else
	    /* We cannot process node->left normally
	       since we haven't ruled out the numbers less than
	       this node's value.  So handle node->left explicitly.  */
	    do_jump_if_equal (index,
			      expand_expr (node->left->low, NULL_RTX,
					   VOIDmode, 0),
			      label_rtx (node->left->code_label), unsignedp);
	}
    }
  else
    {
      /* Node is a range.  These cases are very similar to those for a single
	 value, except that we do not start by testing whether this node
	 is the one to branch to.  */

      if (node->right != 0 && node->left != 0)
	{
	  /* Node has subtrees on both sides.
	     If the right-hand subtree is bounded,
	     test for it first, since we can go straight there.
	     Otherwise, we need to make a branch in the control structure,
	     then handle the two subtrees.  */
	  tree test_label = 0;

	  emit_cmp_insn (index, expand_expr (node->high, NULL_RTX,
					     VOIDmode, 0),
			 GT, NULL_RTX, mode, unsignedp, 0);

	  if (node_is_bounded (node->right, index_type))
	    /* Right hand node is fully bounded so we can eliminate any
	       testing and branch directly to the target code.  */
	    emit_jump_insn ((*gen_bgt_pat) (label_rtx (node->right->code_label)));
	  else
	    {
	      /* Right hand node requires testing.
		 Branch to a label where we will handle it later.  */

	      test_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
	      emit_jump_insn ((*gen_bgt_pat) (label_rtx (test_label)));
	    }

	  /* Value belongs to this node or to the left-hand subtree.  */

	  emit_cmp_insn (index, expand_expr (node->low, NULL_RTX, VOIDmode, 0),
			 GE, NULL_RTX, mode, unsignedp, 0);
	  emit_jump_insn ((*gen_bge_pat) (label_rtx (node->code_label)));

	  /* Handle the left-hand subtree.  */
	  emit_case_nodes (index, node->left, default_label, index_type);

	  /* If right node had to be handled later, do that now.  */

	  if (test_label)
	    {
	      /* If the left-hand subtree fell through,
		 don't let it fall into the right-hand subtree.  */
	      emit_jump_if_reachable (default_label);

	      expand_label (test_label);
	      emit_case_nodes (index, node->right, default_label, index_type);
	    }
	}

      else if (node->right != 0 && node->left == 0)
	{
	  /* Deal with values to the left of this node,
	     if they are possible.  */
	  if (!node_has_low_bound (node, index_type))
	    {
	      emit_cmp_insn (index, expand_expr (node->low, NULL_RTX,
						 VOIDmode, 0),
			     LT, NULL_RTX, mode, unsignedp, 0);
	      emit_jump_insn ((*gen_blt_pat) (default_label));
	    }

	  /* Value belongs to this node or to the right-hand subtree.  */

	  emit_cmp_insn (index, expand_expr (node->high, NULL_RTX,
					     VOIDmode, 0),
			 LE, NULL_RTX, mode, unsignedp, 0);
	  emit_jump_insn ((*gen_ble_pat) (label_rtx (node->code_label)));

	  emit_case_nodes (index, node->right, default_label, index_type);
	}

      else if (node->right == 0 && node->left != 0)
	{
	  /* Deal with values to the right of this node,
	     if they are possible.  */
	  if (!node_has_high_bound (node, index_type))
	    {
	      emit_cmp_insn (index, expand_expr (node->high, NULL_RTX,
						 VOIDmode, 0),
			     GT, NULL_RTX, mode, unsignedp, 0);
	      emit_jump_insn ((*gen_bgt_pat) (default_label));
	    }

	  /* Value belongs to this node or to the left-hand subtree.  */

	  emit_cmp_insn (index, expand_expr (node->low, NULL_RTX, VOIDmode, 0),
			 GE, NULL_RTX, mode, unsignedp, 0);
	  emit_jump_insn ((*gen_bge_pat) (label_rtx (node->code_label)));

	  emit_case_nodes (index, node->left, default_label, index_type);
	}

      else
	{
	  /* Node has no children so we check low and high bounds to remove
	     redundant tests.  Only one of the bounds can exist,
	     since otherwise this node is bounded--a case tested already.  */

	  if (!node_has_high_bound (node, index_type))
	    {
	      emit_cmp_insn (index, expand_expr (node->high, NULL_RTX,
						 VOIDmode, 0),
			     GT, NULL_RTX, mode, unsignedp, 0);
	      emit_jump_insn ((*gen_bgt_pat) (default_label));
	    }

	  if (!node_has_low_bound (node, index_type))
	    {
	      emit_cmp_insn (index, expand_expr (node->low, NULL_RTX,
						 VOIDmode, 0),
			     LT, NULL_RTX, mode, unsignedp, 0);
	      emit_jump_insn ((*gen_blt_pat) (default_label));
	    }

	  emit_jump (label_rtx (node->code_label));
	}
    }
}

/* These routines are used by the loop unrolling code.  They copy BLOCK trees
   so that the debugging info will be correct for the unrolled loop.  */

/* Indexed by block number, contains a pointer to the N'th block node.  */

static tree *block_vector;

void
find_loop_tree_blocks ()
{
  tree block = DECL_INITIAL (current_function_decl);

  /* There first block is for the function body, and does not have
     corresponding block notes.  Don't include it in the block vector.  */
  block = BLOCK_SUBBLOCKS (block);

  block_vector = identify_blocks (block, get_insns ());
}

void
unroll_block_trees ()
{
  tree block = DECL_INITIAL (current_function_decl);

  reorder_blocks (block_vector, block, get_insns ());
}

