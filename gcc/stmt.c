/* Expands front end tree to back end RTL for GNU C-Compiler
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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
#include "system.h"

#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "flags.h"
#include "except.h"
#include "function.h"
#include "insn-config.h"
#include "expr.h"
#include "libfuncs.h"
#include "hard-reg-set.h"
#include "loop.h"
#include "recog.h"
#include "machmode.h"
#include "toplev.h"
#include "output.h"
#include "ggc.h"
#include "langhooks.h"
#include "predict.h"

/* Assume that case vectors are not pc-relative.  */
#ifndef CASE_VECTOR_PC_RELATIVE
#define CASE_VECTOR_PC_RELATIVE 0
#endif

/* Functions and data structures for expanding case statements.  */

/* Case label structure, used to hold info on labels within case
   statements.  We handle "range" labels; for a single-value label
   as in C, the high and low limits are the same.

   An AVL tree of case nodes is initially created, and later transformed
   to a list linked via the RIGHT fields in the nodes.  Nodes with
   higher case values are later in the list.

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

struct case_node GTY(())
{
  struct case_node	*left;	/* Left son in binary tree */
  struct case_node	*right;	/* Right son in binary tree; also node chain */
  struct case_node	*parent; /* Parent of node in binary tree */
  tree			low;	/* Lowest index value for this label */
  tree			high;	/* Highest index value for this label */
  tree			code_label; /* Label to jump to when node matches */
  int			balance;
};

typedef struct case_node case_node;
typedef struct case_node *case_node_ptr;

/* These are used by estimate_case_costs and balance_case_nodes.  */

/* This must be a signed type, and non-ANSI compilers lack signed char.  */
static short cost_table_[129];
static int use_cost_table;
static int cost_table_initialized;

/* Special care is needed because we allow -1, but TREE_INT_CST_LOW
   is unsigned.  */
#define COST_TABLE(I)  cost_table_[(unsigned HOST_WIDE_INT) ((I) + 1)]

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

struct nesting GTY(())
{
  struct nesting *all;
  struct nesting *next;
  int depth;
  rtx exit_label;
  enum nesting_desc {
    COND_NESTING,
    LOOP_NESTING,
    BLOCK_NESTING,
    CASE_NESTING
  } desc;
  union nesting_u
    {
      /* For conds (if-then and if-then-else statements).  */
      struct nesting_cond
	{
	  /* Label for the end of the if construct.
	     There is none if EXITFLAG was not set
	     and no `else' has been seen yet.  */
	  rtx endif_label;
	  /* Label for the end of this alternative.
	     This may be the end of the if or the next else/elseif.  */
	  rtx next_label;
	} GTY ((tag ("COND_NESTING"))) cond;
      /* For loops.  */
      struct nesting_loop
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
	} GTY ((tag ("LOOP_NESTING"))) loop;
      /* For variable binding contours.  */
      struct nesting_block
	{
	  /* Sequence number of this binding contour within the function,
	     in order of entry.  */
	  int block_start_count;
	  /* Nonzero => value to restore stack to on exit.  */
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
	     The tail of this list can be 0,
	     if all remaining elements would be empty lists.
	     The element's TREE_VALUE is the cleanup-list of that block,
	     which may be null.  */
	  tree outer_cleanups;
	  /* Chain of labels defined inside this binding contour.
	     For contours that have stack levels or cleanups.  */
	  struct label_chain *label_chain;
	  /* Number of function calls seen, as of start of this block.  */
	  int n_function_calls;
	  /* Nonzero if this is associated with an EH region.  */
	  int exception_region;
	  /* The saved target_temp_slot_level from our outer block.
	     We may reset target_temp_slot_level to be the level of
	     this block, if that is done, target_temp_slot_level
	     reverts to the saved target_temp_slot_level at the very
	     end of the block.  */
	  int block_target_temp_slot_level;
	  /* True if we are currently emitting insns in an area of
	     output code that is controlled by a conditional
	     expression.  This is used by the cleanup handling code to
	     generate conditional cleanup actions.  */
	  int conditional_code;
	  /* A place to move the start of the exception region for any
	     of the conditional cleanups, must be at the end or after
	     the start of the last unconditional cleanup, and before any
	     conditional branch points.  */
	  rtx last_unconditional_cleanup;
	} GTY ((tag ("BLOCK_NESTING"))) block;
      /* For switch (C) or case (Pascal) statements,
	 and also for dummies (see `expand_start_case_dummy').  */
      struct nesting_case
	{
	  /* The insn after which the case dispatch should finally
	     be emitted.  Zero for a dummy.  */
	  rtx start;
	  /* A list of case labels; it is first built as an AVL tree.
	     During expand_end_case, this is converted to a list, and may be
	     rearranged into a nearly balanced binary tree.  */
	  struct case_node *case_list;
	  /* Label to jump to if no case matches.  */
	  tree default_label;
	  /* The expression to be dispatched on.  */
	  tree index_expr;
	  /* Type that INDEX_EXPR should be converted to.  */
	  tree nominal_type;
	  /* Name of this kind of statement, for warnings.  */
	  const char *printname;
	  /* Used to save no_line_numbers till we see the first case label.
	     We set this to -1 when we see the first case label in this
	     case statement.  */
	  int line_number_status;
	} GTY ((tag ("CASE_NESTING"))) case_stmt;
    } GTY ((desc ("%1.desc"))) data;
};

/* Allocate and return a new `struct nesting'.  */

#define ALLOC_NESTING() \
 (struct nesting *) ggc_alloc (sizeof (struct nesting))

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
	  nesting_stack = this->all; }			\
     while (this != target); } while (0)

/* In some cases it is impossible to generate code for a forward goto
   until the label definition is seen.  This happens when it may be necessary
   for the goto to reset the stack pointer: we don't yet know how to do that.
   So expand_goto puts an entry on this fixup list.
   Each time a binding contour that resets the stack is exited,
   we check each fixup.
   If the target label has now been defined, we can insert the proper code.  */

struct goto_fixup GTY(())
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
     The tail of this list can be 0,
     if all remaining elements would be empty.
     The TREE_VALUE contains the cleanup list of that block as of the
     time this goto was seen.
     The TREE_ADDRESSABLE flag is 1 for a block that has been exited.  */
  tree cleanup_list_list;
};

/* Within any binding contour that must restore a stack level,
   all labels are recorded with a chain of these structures.  */

struct label_chain GTY(())
{
  /* Points to following fixup.  */
  struct label_chain *next;
  tree label;
};

struct stmt_status GTY(())
{
  /* Chain of all pending binding contours.  */
  struct nesting * x_block_stack;

  /* If any new stacks are added here, add them to POPSTACKS too.  */

  /* Chain of all pending binding contours that restore stack levels
     or have cleanups.  */
  struct nesting * x_stack_block_stack;

  /* Chain of all pending conditional statements.  */
  struct nesting * x_cond_stack;

  /* Chain of all pending loops.  */
  struct nesting * x_loop_stack;

  /* Chain of all pending case or switch statements.  */
  struct nesting * x_case_stack;

  /* Separate chain including all of the above,
     chained through the `all' field.  */
  struct nesting * x_nesting_stack;

  /* Number of entries on nesting_stack now.  */
  int x_nesting_depth;

  /* Number of binding contours started so far in this function.  */
  int x_block_start_count;

  /* Each time we expand an expression-statement,
     record the expr's type and its RTL value here.  */
  tree x_last_expr_type;
  rtx x_last_expr_value;

  /* Nonzero if within a ({...}) grouping, in which case we must
     always compute a value for each expr-stmt in case it is the last one.  */
  int x_expr_stmts_for_value;

  /* Filename and line number of last line-number note,
     whether we actually emitted it or not.  */
  const char *x_emit_filename;
  int x_emit_lineno;

  struct goto_fixup *x_goto_fixup_chain;
};

#define block_stack (cfun->stmt->x_block_stack)
#define stack_block_stack (cfun->stmt->x_stack_block_stack)
#define cond_stack (cfun->stmt->x_cond_stack)
#define loop_stack (cfun->stmt->x_loop_stack)
#define case_stack (cfun->stmt->x_case_stack)
#define nesting_stack (cfun->stmt->x_nesting_stack)
#define nesting_depth (cfun->stmt->x_nesting_depth)
#define current_block_start_count (cfun->stmt->x_block_start_count)
#define last_expr_type (cfun->stmt->x_last_expr_type)
#define last_expr_value (cfun->stmt->x_last_expr_value)
#define expr_stmts_for_value (cfun->stmt->x_expr_stmts_for_value)
#define emit_filename (cfun->stmt->x_emit_filename)
#define emit_lineno (cfun->stmt->x_emit_lineno)
#define goto_fixup_chain (cfun->stmt->x_goto_fixup_chain)

/* Non-zero if we are using EH to handle cleanups.  */
static int using_eh_for_cleanups_p = 0;

static int n_occurrences		PARAMS ((int, const char *));
static bool parse_input_constraint	PARAMS ((const char **, int, int, int,
						 int, const char * const *,
						 bool *, bool *));
static bool decl_conflicts_with_clobbers_p PARAMS ((tree, const HARD_REG_SET));
static void expand_goto_internal	PARAMS ((tree, rtx, rtx));
static int expand_fixup			PARAMS ((tree, rtx, rtx));
static rtx expand_nl_handler_label	PARAMS ((rtx, rtx));
static void expand_nl_goto_receiver	PARAMS ((void));
static void expand_nl_goto_receivers	PARAMS ((struct nesting *));
static void fixup_gotos			PARAMS ((struct nesting *, rtx, tree,
					       rtx, int));
static bool check_operand_nalternatives	PARAMS ((tree, tree));
static bool check_unique_operand_names	PARAMS ((tree, tree));
static tree resolve_operand_names	PARAMS ((tree, tree, tree,
						 const char **));
static char *resolve_operand_name_1	PARAMS ((char *, tree, tree));
static void expand_null_return_1	PARAMS ((rtx));
static enum br_predictor return_prediction PARAMS ((rtx));
static void expand_value_return		PARAMS ((rtx));
static int tail_recursion_args		PARAMS ((tree, tree));
static void expand_cleanups		PARAMS ((tree, tree, int, int));
static void check_seenlabel		PARAMS ((void));
static void do_jump_if_equal		PARAMS ((rtx, rtx, rtx, int));
static int estimate_case_costs		PARAMS ((case_node_ptr));
static void group_case_nodes		PARAMS ((case_node_ptr));
static void balance_case_nodes		PARAMS ((case_node_ptr *,
					       case_node_ptr));
static int node_has_low_bound		PARAMS ((case_node_ptr, tree));
static int node_has_high_bound		PARAMS ((case_node_ptr, tree));
static int node_is_bounded		PARAMS ((case_node_ptr, tree));
static void emit_jump_if_reachable	PARAMS ((rtx));
static void emit_case_nodes		PARAMS ((rtx, case_node_ptr, rtx, tree));
static struct case_node *case_tree2list	PARAMS ((case_node *, case_node *));

void
using_eh_for_cleanups ()
{
  using_eh_for_cleanups_p = 1;
}

void
init_stmt_for_function ()
{
  cfun->stmt = ((struct stmt_status *)ggc_alloc (sizeof (struct stmt_status)));

  /* We are not currently within any block, conditional, loop or case.  */
  block_stack = 0;
  stack_block_stack = 0;
  loop_stack = 0;
  case_stack = 0;
  cond_stack = 0;
  nesting_stack = 0;
  nesting_depth = 0;

  current_block_start_count = 0;

  /* No gotos have been expanded yet.  */
  goto_fixup_chain = 0;

  /* We are not processing a ({...}) grouping.  */
  expr_stmts_for_value = 0;
  clear_last_expr ();
}

/* Return nonzero if anything is pushed on the loop, condition, or case
   stack.  */
int
in_control_zone_p ()
{
  return cond_stack || loop_stack || case_stack;
}

/* Record the current file and line.  Called from emit_line_note.  */
void
set_file_and_line_for_stmt (file, line)
     const char *file;
     int line;
{
  /* If we're outputting an inline function, and we add a line note,
     there may be no CFUN->STMT information.  So, there's no need to
     update it.  */
  if (cfun->stmt)
    {
      emit_filename = file;
      emit_lineno = line;
    }
}

/* Emit a no-op instruction.  */

void
emit_nop ()
{
  rtx last_insn;

  last_insn = get_last_insn ();
  if (!optimize
      && (GET_CODE (last_insn) == CODE_LABEL
	  || (GET_CODE (last_insn) == NOTE
	      && prev_real_insn (last_insn) == 0)))
    emit_insn (gen_nop ());
}

/* Return the rtx-label that corresponds to a LABEL_DECL,
   creating it if necessary.  */

rtx
label_rtx (label)
     tree label;
{
  if (TREE_CODE (label) != LABEL_DECL)
    abort ();

  if (!DECL_RTL_SET_P (label))
    SET_DECL_RTL (label, gen_label_rtx ());

  return DECL_RTL (label);
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
  rtx x = expand_expr (exp, NULL_RTX, VOIDmode, 0);

#ifdef POINTERS_EXTEND_UNSIGNED
  if (GET_MODE (x) != Pmode)
    x = convert_memory_address (Pmode, x);
#endif

  emit_queue ();

  if (! cfun->computed_goto_common_label)
    {
      cfun->computed_goto_common_reg = copy_to_mode_reg (Pmode, x);
      cfun->computed_goto_common_label = gen_label_rtx ();
  
      do_pending_stack_adjust ();
      emit_label (cfun->computed_goto_common_label);
      emit_indirect_jump (cfun->computed_goto_common_reg);

      current_function_has_computed_jump = 1;
    }
  else
    {
      emit_move_insn (cfun->computed_goto_common_reg, x);
      emit_jump (cfun->computed_goto_common_label);
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

  do_pending_stack_adjust ();
  emit_label (label_rtx (label));
  if (DECL_NAME (label))
    LABEL_NAME (DECL_RTL (label)) = IDENTIFIER_POINTER (DECL_NAME (label));

  if (stack_block_stack != 0)
    {
      p = (struct label_chain *) ggc_alloc (sizeof (struct label_chain));
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
  rtx slot = assign_stack_local (Pmode, GET_MODE_SIZE (Pmode), 0);

  nonlocal_labels = tree_cons (NULL_TREE, label, nonlocal_labels);
  LABEL_PRESERVE_P (label_rtx (label)) = 1;
  if (nonlocal_goto_handler_slots == 0)
    {
      emit_stack_save (SAVE_NONLOCAL,
		       &nonlocal_goto_stack_level,
		       PREV_INSN (tail_recursion_reentry));
    }
  nonlocal_goto_handler_slots
    = gen_rtx_EXPR_LIST (VOIDmode, slot, nonlocal_goto_handler_slots);
}

/* Generate RTL code for a `goto' statement with target label LABEL.
   LABEL should be a LABEL_DECL tree node that was or will later be
   defined with `expand_label'.  */

void
expand_goto (label)
     tree label;
{
  tree context;

  /* Check for a nonlocal goto to a containing function.  */
  context = decl_function_context (label);
  if (context != 0 && context != current_function_decl)
    {
      struct function *p = find_function_data (context);
      rtx label_ref = gen_rtx_LABEL_REF (Pmode, label_rtx (label));
      rtx handler_slot, static_chain, save_area, insn;
      tree link;

      /* Find the corresponding handler slot for this label.  */
      handler_slot = p->x_nonlocal_goto_handler_slots;
      for (link = p->x_nonlocal_labels; TREE_VALUE (link) != label;
	   link = TREE_CHAIN (link))
	handler_slot = XEXP (handler_slot, 1);
      handler_slot = XEXP (handler_slot, 0);

      p->has_nonlocal_label = 1;
      current_function_has_nonlocal_goto = 1;
      LABEL_REF_NONLOCAL_P (label_ref) = 1;

      /* Copy the rtl for the slots so that they won't be shared in
	 case the virtual stack vars register gets instantiated differently
	 in the parent than in the child.  */

      static_chain = copy_to_reg (lookup_static_chain (label));

      /* Get addr of containing function's current nonlocal goto handler,
	 which will do any cleanups and then jump to the label.  */
      handler_slot = copy_to_reg (replace_rtx (copy_rtx (handler_slot),
					       virtual_stack_vars_rtx,
					       static_chain));

      /* Get addr of containing function's nonlocal save area.  */
      save_area = p->x_nonlocal_goto_stack_level;
      if (save_area)
	save_area = replace_rtx (copy_rtx (save_area),
				 virtual_stack_vars_rtx, static_chain);

#if HAVE_nonlocal_goto
      if (HAVE_nonlocal_goto)
	emit_insn (gen_nonlocal_goto (static_chain, handler_slot,
				      save_area, label_ref));
      else
#endif
	{
	  /* Restore frame pointer for containing function.
	     This sets the actual hard register used for the frame pointer
	     to the location of the function's incoming static chain info.
	     The non-local goto handler will then adjust it to contain the
	     proper value and reload the argument pointer, if needed.  */
	  emit_move_insn (hard_frame_pointer_rtx, static_chain);
	  emit_stack_restore (SAVE_NONLOCAL, save_area, NULL_RTX);

	  /* USE of hard_frame_pointer_rtx added for consistency;
	     not clear if really needed.  */
	  emit_insn (gen_rtx_USE (VOIDmode, hard_frame_pointer_rtx));
	  emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
	  emit_indirect_jump (handler_slot);
	}

      /* Search backwards to the jump insn and mark it as a
	 non-local goto.  */
      for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
	{
	  if (GET_CODE (insn) == JUMP_INSN)
	    {
	      REG_NOTES (insn) = alloc_EXPR_LIST (REG_NON_LOCAL_GOTO,
						  const0_rtx, REG_NOTES (insn));
	      break;
	    }
	  else if (GET_CODE (insn) == CALL_INSN)
	      break;
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
	  /* Ensure stack adjust isn't done by emit_jump, as this
	     would clobber the stack pointer.  This one should be
	     deleted as dead by flow.  */
	  clear_pending_stack_adjust ();
	  do_pending_stack_adjust ();

	  /* Don't do this adjust if it's to the end label and this function
	     is to return with a depressed stack pointer.  */
	  if (label == return_label
	      && (((TREE_CODE (TREE_TYPE (current_function_decl))
		   == FUNCTION_TYPE)
		   && (TYPE_RETURNS_STACK_DEPRESSED
		       (TREE_TYPE (current_function_decl))))))
	    ;
	  else
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
	= (struct goto_fixup *) ggc_alloc (sizeof (struct goto_fixup));
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
	 NOTE_INSN_BLOCK_BEG and NOTE_INSN_BLOCK_END notes at
	 this point.  The notes will encapsulate any and all fixup
	 code which we might later insert at this point in the insn
	 stream.  Also, the BLOCK node will be the parent (i.e. the
	 `SUPERBLOCK') of any other BLOCK nodes which we might create
	 later on when we are expanding the fixup code.

	 Note that optimization passes (including expand_end_loop)
	 might move the *_BLOCK notes away, so we use a NOTE_INSN_DELETED
	 as a placeholder.  */

      {
	rtx original_before_jump
	  = last_insn ? last_insn : get_last_insn ();
	rtx start;
	rtx end;
	tree block;

	block = make_node (BLOCK);
	TREE_USED (block) = 1;

	if (!cfun->x_whole_function_mode_p)
	  (*lang_hooks.decls.insert_block) (block);
	else
	  {
	    BLOCK_CHAIN (block)
	      = BLOCK_CHAIN (DECL_INITIAL (current_function_decl));
	    BLOCK_CHAIN (DECL_INITIAL (current_function_decl))
	      = block;
	  }

	start_sequence ();
	start = emit_note (NULL, NOTE_INSN_BLOCK_BEG);
	if (cfun->x_whole_function_mode_p)
	  NOTE_BLOCK (start) = block;
	fixup->before_jump = emit_note (NULL, NOTE_INSN_DELETED);
	end = emit_note (NULL, NOTE_INSN_BLOCK_END);
	if (cfun->x_whole_function_mode_p)
	  NOTE_BLOCK (end) = block;
	fixup->context = block;
	end_sequence ();
	emit_insn_after (start, original_before_jump);
      }

      fixup->block_start_count = current_block_start_count;
      fixup->stack_level = 0;
      fixup->cleanup_list_list
	= ((block->data.block.outer_cleanups
	    || block->data.block.cleanups)
	   ? tree_cons (NULL_TREE, block->data.block.cleanups,
			block->data.block.outer_cleanups)
	   : 0);
      fixup->next = goto_fixup_chain;
      goto_fixup_chain = fixup;
    }

  return block != 0;
}

/* Expand any needed fixups in the outputmost binding level of the
   function.  FIRST_INSN is the first insn in the function.  */

void
expand_fixups (first_insn)
     rtx first_insn;
{
  fixup_gotos (NULL, NULL_RTX, NULL_TREE, first_insn, 0);
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
  struct goto_fixup *f, *prev;

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
	  rtx cleanup_insns;

	  /* If this fixup jumped into this contour from before the beginning
	     of this contour, report an error.   This code used to use
	     the first non-label insn after f->target_rtl, but that's
	     wrong since such can be added, by things like put_var_into_stack
	     and have INSN_UIDs that are out of the range of the block.  */
	  /* ??? Bug: this does not detect jumping in through intermediate
	     blocks that have stack levels or cleanups.
	     It detects only a problem with the innermost block
	     around the label.  */
	  if (f->target != 0
	      && (dont_jump_in || stack_level || cleanup_list)
	      && INSN_UID (first_insn) < INSN_UID (f->target_rtl)
	      && INSN_UID (first_insn) > INSN_UID (f->before_jump)
	      && ! DECL_ERROR_ISSUED (f->target))
	    {
	      error_with_decl (f->target,
			       "label `%s' used before containing binding contour");
	      /* Prevent multiple errors for one label.  */
	      DECL_ERROR_ISSUED (f->target) = 1;
	    }

	  /* We will expand the cleanups into a sequence of their own and
	     then later on we will attach this new sequence to the insn
	     stream just ahead of the actual jump insn.  */

	  start_sequence ();

	  /* Temporarily restore the lexical context where we will
	     logically be inserting the fixup code.  We do this for the
	     sake of getting the debugging information right.  */

	  (*lang_hooks.decls.pushlevel) (0);
	  (*lang_hooks.decls.set_block) (f->context);

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
	  if (f->stack_level
	      && ! (f->target_rtl == return_label
		    && ((TREE_CODE (TREE_TYPE (current_function_decl))
			 == FUNCTION_TYPE)
			&& (TYPE_RETURNS_STACK_DEPRESSED
			    (TREE_TYPE (current_function_decl))))))
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
	  (*lang_hooks.decls.poplevel) (1, 0, 0);

	  end_sequence ();
	  emit_insn_after (cleanup_insns, f->before_jump);

	  f->before_jump = 0;
	}
    }

  /* For any still-undefined labels, do the cleanups for this block now.
     We must do this now since items in the cleanup list may go out
     of scope when the block ends.  */
  for (prev = 0, f = goto_fixup_chain; f; prev = f, f = f->next)
    if (f->before_jump != 0
	&& PREV_INSN (f->target_rtl) == 0
	/* Label has still not appeared.  If we are exiting a block with
	   a stack level to restore, that started before the fixup,
	   mark this stack level as needing restoration
	   when the fixup is later finalized.  */
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
	      (*lang_hooks.decls.pushlevel) (0);
	      (*lang_hooks.decls.set_block) (f->context);
	      expand_cleanups (TREE_VALUE (lists), NULL_TREE, 1, 1);
	      do_pending_stack_adjust ();
	      cleanup_insns = get_insns ();
	      (*lang_hooks.decls.poplevel) (1, 0, 0);
	      end_sequence ();
	      if (cleanup_insns != 0)
		f->before_jump
		  = emit_insn_after (cleanup_insns, f->before_jump);

	      f->cleanup_list_list = TREE_CHAIN (lists);
	    }

	if (stack_level)
	  f->stack_level = stack_level;
      }
}

/* Return the number of times character C occurs in string S.  */
static int
n_occurrences (c, s)
     int c;
     const char *s;
{
  int n = 0;
  while (*s)
    n += (*s++ == c);
  return n;
}

/* Generate RTL for an asm statement (explicit assembler code).
   STRING is a STRING_CST node containing the assembler code text,
   or an ADDR_EXPR containing a STRING_CST.  VOL nonzero means the
   insn is volatile; don't optimize it.  */

void
expand_asm (string, vol)
     tree string;
     int vol;
{
  rtx body;

  if (TREE_CODE (string) == ADDR_EXPR)
    string = TREE_OPERAND (string, 0);

  body = gen_rtx_ASM_INPUT (VOIDmode, TREE_STRING_POINTER (string));

  MEM_VOLATILE_P (body) = vol;

  emit_insn (body);
  
  clear_last_expr ();
}

/* Parse the output constraint pointed to by *CONSTRAINT_P.  It is the
   OPERAND_NUMth output operand, indexed from zero.  There are NINPUTS
   inputs and NOUTPUTS outputs to this extended-asm.  Upon return,
   *ALLOWS_MEM will be TRUE iff the constraint allows the use of a
   memory operand.  Similarly, *ALLOWS_REG will be TRUE iff the
   constraint allows the use of a register operand.  And, *IS_INOUT
   will be true if the operand is read-write, i.e., if it is used as
   an input as well as an output.  If *CONSTRAINT_P is not in
   canonical form, it will be made canonical.  (Note that `+' will be
   rpelaced with `=' as part of this process.)

   Returns TRUE if all went well; FALSE if an error occurred.  */

bool
parse_output_constraint (constraint_p, operand_num, ninputs, noutputs,
			 allows_mem, allows_reg, is_inout)
     const char **constraint_p;
     int operand_num;
     int ninputs;
     int noutputs;
     bool *allows_mem;
     bool *allows_reg;
     bool *is_inout;
{
  const char *constraint = *constraint_p;
  const char *p;

  /* Assume the constraint doesn't allow the use of either a register
     or memory.  */
  *allows_mem = false;
  *allows_reg = false;

  /* Allow the `=' or `+' to not be at the beginning of the string,
     since it wasn't explicitly documented that way, and there is a
     large body of code that puts it last.  Swap the character to
     the front, so as not to uglify any place else.  */
  p = strchr (constraint, '=');
  if (!p)
    p = strchr (constraint, '+');

  /* If the string doesn't contain an `=', issue an error
     message.  */
  if (!p)
    {
      error ("output operand constraint lacks `='");
      return false;
    }

  /* If the constraint begins with `+', then the operand is both read
     from and written to.  */
  *is_inout = (*p == '+');

  /* Canonicalize the output constraint so that it begins with `='.  */
  if (p != constraint || is_inout)
    {
      char *buf;
      size_t c_len = strlen (constraint);

      if (p != constraint)
	warning ("output constraint `%c' for operand %d is not at the beginning",
		 *p, operand_num);

      /* Make a copy of the constraint.  */
      buf = alloca (c_len + 1);
      strcpy (buf, constraint);
      /* Swap the first character and the `=' or `+'.  */
      buf[p - constraint] = buf[0];
      /* Make sure the first character is an `='.  (Until we do this,
	 it might be a `+'.)  */
      buf[0] = '=';
      /* Replace the constraint with the canonicalized string.  */
      *constraint_p = ggc_alloc_string (buf, c_len);
      constraint = *constraint_p;
    }

  /* Loop through the constraint string.  */
  for (p = constraint + 1; *p; ++p)
    switch (*p)
      {
      case '+':
      case '=':
	error ("operand constraint contains incorrectly positioned '+' or '='");
	return false;

      case '%':
	if (operand_num + 1 == ninputs + noutputs)
	  {
	    error ("`%%' constraint used with last operand");
	    return false;
	  }
	break;

      case 'V':  case 'm':  case 'o':
	*allows_mem = true;
	break;

      case '?':  case '!':  case '*':  case '&':  case '#':
      case 'E':  case 'F':  case 'G':  case 'H':
      case 's':  case 'i':  case 'n':
      case 'I':  case 'J':  case 'K':  case 'L':  case 'M':
      case 'N':  case 'O':  case 'P':  case ',':
	break;

      case '0':  case '1':  case '2':  case '3':  case '4':
      case '5':  case '6':  case '7':  case '8':  case '9':
      case '[':
	error ("matching constraint not valid in output operand");
	return false;

      case '<':  case '>':
	/* ??? Before flow, auto inc/dec insns are not supposed to exist,
	   excepting those that expand_call created.  So match memory
	   and hope.  */
	*allows_mem = true;
	break;

      case 'g':  case 'X':
	*allows_reg = true;
	*allows_mem = true;
	break;

      case 'p': case 'r':
	*allows_reg = true;
	break;

      default:
	if (!ISALPHA (*p))
	  break;
	if (REG_CLASS_FROM_LETTER (*p) != NO_REGS)
	  *allows_reg = true;
#ifdef EXTRA_CONSTRAINT
	else if (EXTRA_ADDRESS_CONSTRAINT (*p))
	  *allows_reg = true;
	else if (EXTRA_MEMORY_CONSTRAINT (*p))
	  *allows_mem = true;
	else
	  {
	    /* Otherwise we can't assume anything about the nature of
	       the constraint except that it isn't purely registers.
	       Treat it like "g" and hope for the best.  */
	    *allows_reg = true;
	    *allows_mem = true;
	  }
#endif
	break;
      }

  return true;
}

/* Similar, but for input constraints.  */

static bool
parse_input_constraint (constraint_p, input_num, ninputs, noutputs, ninout,
			constraints, allows_mem, allows_reg)
     const char **constraint_p;
     int input_num;
     int ninputs;
     int noutputs;
     int ninout;
     const char * const * constraints;
     bool *allows_mem;
     bool *allows_reg;
{
  const char *constraint = *constraint_p;
  const char *orig_constraint = constraint;
  size_t c_len = strlen (constraint);
  size_t j;

  /* Assume the constraint doesn't allow the use of either
     a register or memory.  */
  *allows_mem = false;
  *allows_reg = false;

  /* Make sure constraint has neither `=', `+', nor '&'.  */

  for (j = 0; j < c_len; j++)
    switch (constraint[j])
      {
      case '+':  case '=':  case '&':
	if (constraint == orig_constraint)
	  {
	    error ("input operand constraint contains `%c'", constraint[j]);
	    return false;
	  }
	break;

      case '%':
	if (constraint == orig_constraint
	    && input_num + 1 == ninputs - ninout)
	  {
	    error ("`%%' constraint used with last operand");
	    return false;
	  }
	break;

      case 'V':  case 'm':  case 'o':
	*allows_mem = true;
	break;

      case '<':  case '>':
      case '?':  case '!':  case '*':  case '#':
      case 'E':  case 'F':  case 'G':  case 'H':
      case 's':  case 'i':  case 'n':
      case 'I':  case 'J':  case 'K':  case 'L':  case 'M':
      case 'N':  case 'O':  case 'P':  case ',':
	break;

	/* Whether or not a numeric constraint allows a register is
	   decided by the matching constraint, and so there is no need
	   to do anything special with them.  We must handle them in
	   the default case, so that we don't unnecessarily force
	   operands to memory.  */
      case '0':  case '1':  case '2':  case '3':  case '4':
      case '5':  case '6':  case '7':  case '8':  case '9':
	{
	  char *end;
	  unsigned long match;

	  match = strtoul (constraint + j, &end, 10);
	  if (match >= (unsigned long) noutputs)
	    {
	      error ("matching constraint references invalid operand number");
	      return false;
	    }

	  /* Try and find the real constraint for this dup.  Only do this
	     if the matching constraint is the only alternative.  */
	  if (*end == '\0'
	      && (j == 0 || (j == 1 && constraint[0] == '%')))
	    {
	      constraint = constraints[match];
	      *constraint_p = constraint;
	      c_len = strlen (constraint);
	      j = 0;
	      break;
	    }
	  else
	    j = end - constraint;
	}
	/* Fall through.  */

      case 'p':  case 'r':
	*allows_reg = true;
	break;

      case 'g':  case 'X':
	*allows_reg = true;
	*allows_mem = true;
	break;

      default:
	if (! ISALPHA (constraint[j]))
	  {
	    error ("invalid punctuation `%c' in constraint", constraint[j]);
	    return false;
	  }
	if (REG_CLASS_FROM_LETTER (constraint[j]) != NO_REGS)
	  *allows_reg = true;
#ifdef EXTRA_CONSTRAINT
	else if (EXTRA_ADDRESS_CONSTRAINT (constraint[j]))
	  *allows_reg = true;
	else if (EXTRA_MEMORY_CONSTRAINT (constraint[j]))
	  *allows_mem = true;
	else
	  {
	    /* Otherwise we can't assume anything about the nature of
	       the constraint except that it isn't purely registers.
	       Treat it like "g" and hope for the best.  */
	    *allows_reg = true;
	    *allows_mem = true;
	  }
#endif
	break;
      }

  return true;
}

/* Check for overlap between registers marked in CLOBBERED_REGS and
   anything inappropriate in DECL.  Emit error and return TRUE for error,
   FALSE for ok.  */

static bool
decl_conflicts_with_clobbers_p (decl, clobbered_regs)
     tree decl;
     const HARD_REG_SET clobbered_regs;
{
  /* Conflicts between asm-declared register variables and the clobber
     list are not allowed.  */
  if ((TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == PARM_DECL)
      && DECL_REGISTER (decl)
      && REG_P (DECL_RTL (decl))
      && REGNO (DECL_RTL (decl)) < FIRST_PSEUDO_REGISTER)
    {
      rtx reg = DECL_RTL (decl);
      unsigned int regno;

      for (regno = REGNO (reg);
	   regno < (REGNO (reg)
		    + HARD_REGNO_NREGS (REGNO (reg), GET_MODE (reg)));
	   regno++)
	if (TEST_HARD_REG_BIT (clobbered_regs, regno))
	  {
	    error ("asm-specifier for variable `%s' conflicts with asm clobber list",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));

	    /* Reset registerness to stop multiple errors emitted for a
	       single variable.  */
	    DECL_REGISTER (decl) = 0;
	    return true;
	  }
    }
  return false;
}

/* Generate RTL for an asm statement with arguments.
   STRING is the instruction template.
   OUTPUTS is a list of output arguments (lvalues); INPUTS a list of inputs.
   Each output or input has an expression in the TREE_VALUE and
   and a tree list in TREE_PURPOSE which in turn contains a constraint
   name in TREE_VALUE (or NULL_TREE) and a constraint string
   in TREE_PURPOSE.
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
     const char *filename;
     int line;
{
  rtvec argvec, constraintvec;
  rtx body;
  int ninputs = list_length (inputs);
  int noutputs = list_length (outputs);
  int ninout;
  int nclobbers;
  HARD_REG_SET clobbered_regs;
  int clobber_conflict_found = 0;
  tree tail;
  int i;
  /* Vector of RTX's of evaluated output operands.  */
  rtx *output_rtx = (rtx *) alloca (noutputs * sizeof (rtx));
  int *inout_opnum = (int *) alloca (noutputs * sizeof (int));
  rtx *real_output_rtx = (rtx *) alloca (noutputs * sizeof (rtx));
  enum machine_mode *inout_mode
    = (enum machine_mode *) alloca (noutputs * sizeof (enum machine_mode));
  const char **constraints
    = (const char **) alloca ((noutputs + ninputs) * sizeof (const char *));
  /* The insn we have emitted.  */
  rtx insn;
  int old_generating_concat_p = generating_concat_p;

  /* An ASM with no outputs needs to be treated as volatile, for now.  */
  if (noutputs == 0)
    vol = 1;

  if (! check_operand_nalternatives (outputs, inputs))
    return;

  if (! check_unique_operand_names (outputs, inputs))
    return;

  string = resolve_operand_names (string, outputs, inputs, constraints);

#ifdef MD_ASM_CLOBBERS
  /* Sometimes we wish to automatically clobber registers across an asm.
     Case in point is when the i386 backend moved from cc0 to a hard reg --
     maintaining source-level compatibility means automatically clobbering
     the flags register.  */
  MD_ASM_CLOBBERS (clobbers);
#endif

  /* Count the number of meaningful clobbered registers, ignoring what
     we would ignore later.  */
  nclobbers = 0;
  CLEAR_HARD_REG_SET (clobbered_regs);
  for (tail = clobbers; tail; tail = TREE_CHAIN (tail))
    {
      const char *regname = TREE_STRING_POINTER (TREE_VALUE (tail));

      i = decode_reg_name (regname);
      if (i >= 0 || i == -4)
	++nclobbers;
      else if (i == -2)
	error ("unknown register name `%s' in `asm'", regname);

      /* Mark clobbered registers.  */
      if (i >= 0)
	SET_HARD_REG_BIT (clobbered_regs, i);
    }

  clear_last_expr ();

  /* First pass over inputs and outputs checks validity and sets
     mark_addressable if needed.  */

  ninout = 0;
  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    {
      tree val = TREE_VALUE (tail);
      tree type = TREE_TYPE (val);
      const char *constraint;
      bool is_inout;
      bool allows_reg;
      bool allows_mem;

      /* If there's an erroneous arg, emit no insn.  */
      if (type == error_mark_node)
	return;

      /* Try to parse the output constraint.  If that fails, there's
	 no point in going further.  */
      constraint = constraints[i];
      if (!parse_output_constraint (&constraint, i, ninputs, noutputs,
				    &allows_mem, &allows_reg, &is_inout))
	return;

      if (! allows_reg
	  && (allows_mem
	      || is_inout
	      || (DECL_P (val)
		  && GET_CODE (DECL_RTL (val)) == REG
		  && GET_MODE (DECL_RTL (val)) != TYPE_MODE (type))))
	(*lang_hooks.mark_addressable) (val);

      if (is_inout)
	ninout++;
    }

  ninputs += ninout;
  if (ninputs + noutputs > MAX_RECOG_OPERANDS)
    {
      error ("more than %d operands in `asm'", MAX_RECOG_OPERANDS);
      return;
    }

  for (i = 0, tail = inputs; tail; i++, tail = TREE_CHAIN (tail))
    {
      bool allows_reg, allows_mem;
      const char *constraint;

      /* If there's an erroneous arg, emit no insn, because the ASM_INPUT
	 would get VOIDmode and that could cause a crash in reload.  */
      if (TREE_TYPE (TREE_VALUE (tail)) == error_mark_node)
	return;

      constraint = constraints[i + noutputs];
      if (! parse_input_constraint (&constraint, i, ninputs, noutputs, ninout,
				    constraints, &allows_mem, &allows_reg))
	return;

      if (! allows_reg && allows_mem)
	(*lang_hooks.mark_addressable) (TREE_VALUE (tail));
    }

  /* Second pass evaluates arguments.  */

  ninout = 0;
  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    {
      tree val = TREE_VALUE (tail);
      tree type = TREE_TYPE (val);
      bool is_inout;
      bool allows_reg;
      bool allows_mem;
      rtx op;

      if (!parse_output_constraint (&constraints[i], i, ninputs,
				    noutputs, &allows_mem, &allows_reg,
				    &is_inout))
	abort ();

      /* If an output operand is not a decl or indirect ref and our constraint
	 allows a register, make a temporary to act as an intermediate.
	 Make the asm insn write into that, then our caller will copy it to
	 the real output operand.  Likewise for promoted variables.  */

      generating_concat_p = 0;

      real_output_rtx[i] = NULL_RTX;
      if ((TREE_CODE (val) == INDIRECT_REF
	   && allows_mem)
	  || (DECL_P (val)
	      && (allows_mem || GET_CODE (DECL_RTL (val)) == REG)
	      && ! (GET_CODE (DECL_RTL (val)) == REG
		    && GET_MODE (DECL_RTL (val)) != TYPE_MODE (type)))
	  || ! allows_reg
	  || is_inout)
	{
	  op = expand_expr (val, NULL_RTX, VOIDmode, EXPAND_WRITE);
	  if (GET_CODE (op) == MEM)
	    op = validize_mem (op);

	  if (! allows_reg && GET_CODE (op) != MEM)
	    error ("output number %d not directly addressable", i);
	  if ((! allows_mem && GET_CODE (op) == MEM)
	      || GET_CODE (op) == CONCAT)
	    {
	      real_output_rtx[i] = protect_from_queue (op, 1);
	      op = gen_reg_rtx (GET_MODE (op));
	      if (is_inout)
		emit_move_insn (op, real_output_rtx[i]);
	    }
	}
      else
	{
	  op = assign_temp (type, 0, 0, 1);
	  op = validize_mem (op);
	  TREE_VALUE (tail) = make_tree (type, op);
	}
      output_rtx[i] = op;

      generating_concat_p = old_generating_concat_p;

      if (is_inout)
	{
	  inout_mode[ninout] = TYPE_MODE (type);
	  inout_opnum[ninout++] = i;
	}

      if (decl_conflicts_with_clobbers_p (val, clobbered_regs))
	clobber_conflict_found = 1;
    }

  /* Make vectors for the expression-rtx, constraint strings,
     and named operands.  */

  argvec = rtvec_alloc (ninputs);
  constraintvec = rtvec_alloc (ninputs);

  body = gen_rtx_ASM_OPERANDS ((noutputs == 0 ? VOIDmode
				: GET_MODE (output_rtx[0])),
			       TREE_STRING_POINTER (string),
			       empty_string, 0, argvec, constraintvec,
			       filename, line);

  MEM_VOLATILE_P (body) = vol;

  /* Eval the inputs and put them into ARGVEC.
     Put their constraints into ASM_INPUTs and store in CONSTRAINTS.  */

  for (i = 0, tail = inputs; tail; tail = TREE_CHAIN (tail), ++i)
    {
      bool allows_reg, allows_mem;
      const char *constraint;
      tree val, type;
      rtx op;

      constraint = constraints[i + noutputs];
      if (! parse_input_constraint (&constraint, i, ninputs, noutputs, ninout,
				    constraints, &allows_mem, &allows_reg))
	abort ();

      generating_concat_p = 0;

      val = TREE_VALUE (tail);
      type = TREE_TYPE (val);
      op = expand_expr (val, NULL_RTX, VOIDmode,
			(allows_mem && !allows_reg
			 ? EXPAND_MEMORY : EXPAND_NORMAL));

      /* Never pass a CONCAT to an ASM.  */
      if (GET_CODE (op) == CONCAT)
	op = force_reg (GET_MODE (op), op);
      else if (GET_CODE (op) == MEM)
	op = validize_mem (op);

      if (asm_operand_ok (op, constraint) <= 0)
	{
	  if (allows_reg)
	    op = force_reg (TYPE_MODE (type), op);
	  else if (!allows_mem)
	    warning ("asm operand %d probably doesn't match constraints",
		     i + noutputs);
	  else if (GET_CODE (op) == MEM)
	    {
	      /* We won't recognize either volatile memory or memory
		 with a queued address as available a memory_operand
		 at this point.  Ignore it: clearly this *is* a memory.  */
	    }
	  else
	    {
	      warning ("use of memory input without lvalue in asm operand %d is deprecated",
		       i + noutputs);

	      if (CONSTANT_P (op))
		{
		  op = force_const_mem (TYPE_MODE (type), op);
		  op = validize_mem (op);
		}
	      else if (GET_CODE (op) == REG
		       || GET_CODE (op) == SUBREG
		       || GET_CODE (op) == ADDRESSOF
		       || GET_CODE (op) == CONCAT)
		{
		  tree qual_type = build_qualified_type (type,
							 (TYPE_QUALS (type)
							  | TYPE_QUAL_CONST));
		  rtx memloc = assign_temp (qual_type, 1, 1, 1);
		  memloc = validize_mem (memloc);
		  emit_move_insn (memloc, op);
		  op = memloc;
		}
	    }
	}

      generating_concat_p = old_generating_concat_p;
      ASM_OPERANDS_INPUT (body, i) = op;

      ASM_OPERANDS_INPUT_CONSTRAINT_EXP (body, i)
	= gen_rtx_ASM_INPUT (TYPE_MODE (type), constraints[i + noutputs]);

      if (decl_conflicts_with_clobbers_p (val, clobbered_regs))
	clobber_conflict_found = 1;
    }

  /* Protect all the operands from the queue now that they have all been
     evaluated.  */

  generating_concat_p = 0;

  for (i = 0; i < ninputs - ninout; i++)
    ASM_OPERANDS_INPUT (body, i)
      = protect_from_queue (ASM_OPERANDS_INPUT (body, i), 0);

  for (i = 0; i < noutputs; i++)
    output_rtx[i] = protect_from_queue (output_rtx[i], 1);

  /* For in-out operands, copy output rtx to input rtx.  */
  for (i = 0; i < ninout; i++)
    {
      int j = inout_opnum[i];
      char buffer[16];

      ASM_OPERANDS_INPUT (body, ninputs - ninout + i)
	= output_rtx[j];

      sprintf (buffer, "%d", j);
      ASM_OPERANDS_INPUT_CONSTRAINT_EXP (body, ninputs - ninout + i)
	= gen_rtx_ASM_INPUT (inout_mode[i], ggc_alloc_string (buffer, -1));
    }

  generating_concat_p = old_generating_concat_p;

  /* Now, for each output, construct an rtx
     (set OUTPUT (asm_operands INSN OUTPUTCONSTRAINT OUTPUTNUMBER
			       ARGVEC CONSTRAINTS OPNAMES))
     If there is more than one, put them inside a PARALLEL.  */

  if (noutputs == 1 && nclobbers == 0)
    {
      ASM_OPERANDS_OUTPUT_CONSTRAINT (body) = constraints[0];
      insn = emit_insn (gen_rtx_SET (VOIDmode, output_rtx[0], body));
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

      if (num == 0)
	num = 1;

      body = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (num + nclobbers));

      /* For each output operand, store a SET.  */
      for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
	{
	  XVECEXP (body, 0, i)
	    = gen_rtx_SET (VOIDmode,
			   output_rtx[i],
			   gen_rtx_ASM_OPERANDS
			   (GET_MODE (output_rtx[i]),
			    TREE_STRING_POINTER (string),
			    constraints[i], i, argvec, constraintvec,
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
	  const char *regname = TREE_STRING_POINTER (TREE_VALUE (tail));
	  int j = decode_reg_name (regname);
	  rtx clobbered_reg;

	  if (j < 0)
	    {
	      if (j == -3)	/* `cc', which is not a register */
		continue;

	      if (j == -4)	/* `memory', don't cache memory across asm */
		{
		  XVECEXP (body, 0, i++)
		    = gen_rtx_CLOBBER (VOIDmode,
				       gen_rtx_MEM
				       (BLKmode,
					gen_rtx_SCRATCH (VOIDmode)));
		  continue;
		}

	      /* Ignore unknown register, error already signaled.  */
	      continue;
	    }

	  /* Use QImode since that's guaranteed to clobber just one reg.  */
	  clobbered_reg = gen_rtx_REG (QImode, j);

	  /* Do sanity check for overlap between clobbers and respectively
	     input and outputs that hasn't been handled.  Such overlap
	     should have been detected and reported above.  */
	  if (!clobber_conflict_found)
	    {
	      int opno;

	      /* We test the old body (obody) contents to avoid tripping
		 over the under-construction body.  */
	      for (opno = 0; opno < noutputs; opno++)
		if (reg_overlap_mentioned_p (clobbered_reg, output_rtx[opno]))
		  internal_error ("asm clobber conflict with output operand");

	      for (opno = 0; opno < ninputs - ninout; opno++)
		if (reg_overlap_mentioned_p (clobbered_reg,
					     ASM_OPERANDS_INPUT (obody, opno)))
		  internal_error ("asm clobber conflict with input operand");
	    }

	  XVECEXP (body, 0, i++)
	    = gen_rtx_CLOBBER (VOIDmode, clobbered_reg);
	}

      insn = emit_insn (body);
    }

  /* For any outputs that needed reloading into registers, spill them
     back to where they belong.  */
  for (i = 0; i < noutputs; ++i)
    if (real_output_rtx[i])
      emit_move_insn (real_output_rtx[i], output_rtx[i]);

  free_temp_slots ();
}

/* A subroutine of expand_asm_operands.  Check that all operands have
   the same number of alternatives.  Return true if so.  */

static bool
check_operand_nalternatives (outputs, inputs)
     tree outputs, inputs;
{
  if (outputs || inputs)
    {
      tree tmp = TREE_PURPOSE (outputs ? outputs : inputs);
      int nalternatives
	= n_occurrences (',', TREE_STRING_POINTER (TREE_VALUE (tmp)));
      tree next = inputs;

      if (nalternatives + 1 > MAX_RECOG_ALTERNATIVES)
	{
	  error ("too many alternatives in `asm'");
	  return false;
	}

      tmp = outputs;
      while (tmp)
	{
	  const char *constraint
	    = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (tmp)));

	  if (n_occurrences (',', constraint) != nalternatives)
	    {
	      error ("operand constraints for `asm' differ in number of alternatives");
	      return false;
	    }

	  if (TREE_CHAIN (tmp))
	    tmp = TREE_CHAIN (tmp);
	  else
	    tmp = next, next = 0;
	}
    }

  return true;
}

/* A subroutine of expand_asm_operands.  Check that all operand names
   are unique.  Return true if so.  We rely on the fact that these names
   are identifiers, and so have been canonicalized by get_identifier,
   so all we need are pointer comparisons.  */

static bool
check_unique_operand_names (outputs, inputs)
     tree outputs, inputs;
{
  tree i, j;

  for (i = outputs; i ; i = TREE_CHAIN (i))
    {
      tree i_name = TREE_PURPOSE (TREE_PURPOSE (i));
      if (! i_name)
	continue;

      for (j = TREE_CHAIN (i); j ; j = TREE_CHAIN (j))
	if (simple_cst_equal (i_name, TREE_PURPOSE (TREE_PURPOSE (j))))
	  goto failure;
    }

  for (i = inputs; i ; i = TREE_CHAIN (i))
    {
      tree i_name = TREE_PURPOSE (TREE_PURPOSE (i));
      if (! i_name)
	continue;

      for (j = TREE_CHAIN (i); j ; j = TREE_CHAIN (j))
	if (simple_cst_equal (i_name, TREE_PURPOSE (TREE_PURPOSE (j))))
	  goto failure;
      for (j = outputs; j ; j = TREE_CHAIN (j))
	if (simple_cst_equal (i_name, TREE_PURPOSE (TREE_PURPOSE (j))))
	  goto failure;
    }

  return true;

 failure:
  error ("duplicate asm operand name '%s'",
	 TREE_STRING_POINTER (TREE_PURPOSE (TREE_PURPOSE (i))));
  return false;
}

/* A subroutine of expand_asm_operands.  Resolve the names of the operands
   in *POUTPUTS and *PINPUTS to numbers, and replace the name expansions in
   STRING and in the constraints to those numbers.  */

static tree
resolve_operand_names (string, outputs, inputs, pconstraints)
     tree string;
     tree outputs, inputs;
     const char **pconstraints;
{
  char *buffer = xstrdup (TREE_STRING_POINTER (string));
  char *p;
  tree t;

  /* Assume that we will not need extra space to perform the substitution.
     This because we get to remove '[' and ']', which means we cannot have
     a problem until we have more than 999 operands.  */

  p = buffer;
  while ((p = strchr (p, '%')) != NULL)
    {
      if (p[1] == '[')
	p += 1;
      else if (ISALPHA (p[1]) && p[2] == '[')
	p += 2;
      else
	{
	  p += 1;
	  continue;
	}

      p = resolve_operand_name_1 (p, outputs, inputs);
    }

  string = build_string (strlen (buffer), buffer);
  free (buffer);

  /* Collect output constraints here because it's convenient.
     There should be no named operands here; this is verified
     in expand_asm_operand.  */
  for (t = outputs; t ; t = TREE_CHAIN (t), pconstraints++)
    *pconstraints = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));

  /* Substitute [<name>] in input constraint strings.  */
  for (t = inputs; t ; t = TREE_CHAIN (t), pconstraints++)
    {
      const char *c = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));
      if (strchr (c, '[') == NULL)
	*pconstraints = c;
      else
	{
	  p = buffer = xstrdup (c);
	  while ((p = strchr (p, '[')) != NULL)
	    p = resolve_operand_name_1 (p, outputs, inputs);

	  *pconstraints = ggc_alloc_string (buffer, -1);
	  free (buffer);
	}
    }

  return string;
}

/* A subroutine of resolve_operand_names.  P points to the '[' for a
   potential named operand of the form [<name>].  In place, replace
   the name and brackets with a number.  Return a pointer to the
   balance of the string after substitution.  */

static char *
resolve_operand_name_1 (p, outputs, inputs)
     char *p;
     tree outputs, inputs;
{
  char *q;
  int op;
  tree t;
  size_t len;

  /* Collect the operand name.  */
  q = strchr (p, ']');
  if (!q)
    {
      error ("missing close brace for named operand");
      return strchr (p, '\0');
    }
  len = q - p - 1;

  /* Resolve the name to a number.  */
  for (op = 0, t = outputs; t ; t = TREE_CHAIN (t), op++)
    {
      tree name = TREE_PURPOSE (TREE_PURPOSE (t));
      if (name)
	{
	  const char *c = TREE_STRING_POINTER (name);
	  if (strncmp (c, p + 1, len) == 0 && c[len] == '\0')
	    goto found;
	}
    }
  for (t = inputs; t ; t = TREE_CHAIN (t), op++)
    {
      tree name = TREE_PURPOSE (TREE_PURPOSE (t));
      if (name)
	{
	  const char *c = TREE_STRING_POINTER (name);
	  if (strncmp (c, p + 1, len) == 0 && c[len] == '\0')
	    goto found;
	}
    }

  *q = '\0';
  error ("undefined named operand '%s'", p + 1);
  op = 0;
 found:

  /* Replace the name with the number.  Unfortunately, not all libraries
     get the return value of sprintf correct, so search for the end of the
     generated string by hand.  */
  sprintf (p, "%d", op);
  p = strchr (p, '\0');

  /* Verify the no extra buffer space assumption.  */
  if (p > q)
    abort ();

  /* Shift the rest of the buffer down to fill the gap.  */
  memmove (p, q + 1, strlen (q + 1) + 1);

  return p;
}

/* Generate RTL to evaluate the expression EXP
   and remember it in case this is the VALUE in a ({... VALUE; }) constr.
   Provided just for backward-compatibility.  expand_expr_stmt_value()
   should be used for new code.  */

void
expand_expr_stmt (exp)
     tree exp;
{
  expand_expr_stmt_value (exp, -1, 1);
}

/* Generate RTL to evaluate the expression EXP.  WANT_VALUE tells
   whether to (1) save the value of the expression, (0) discard it or
   (-1) use expr_stmts_for_value to tell.  The use of -1 is
   deprecated, and retained only for backward compatibility.  */

void
expand_expr_stmt_value (exp, want_value, maybe_last)
     tree exp;
     int want_value, maybe_last;
{
  rtx value;
  tree type;

  if (want_value == -1)
    want_value = expr_stmts_for_value != 0;

  /* If -W, warn about statements with no side effects,
     except for an explicit cast to void (e.g. for assert()), and
     except for last statement in ({...}) where they may be useful.  */
  if (! want_value
      && (expr_stmts_for_value == 0 || ! maybe_last)
      && exp != error_mark_node)
    {
      if (! TREE_SIDE_EFFECTS (exp))
	{
	  if ((extra_warnings || warn_unused_value)
	      && !(TREE_CODE (exp) == CONVERT_EXPR
		   && VOID_TYPE_P (TREE_TYPE (exp))))
	    warning_with_file_and_line (emit_filename, emit_lineno,
				        "statement with no effect");
	}
      else if (warn_unused_value)
	warn_if_unused_value (exp);
    }

  /* If EXP is of function type and we are expanding statements for
     value, convert it to pointer-to-function.  */
  if (want_value && TREE_CODE (TREE_TYPE (exp)) == FUNCTION_TYPE)
    exp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (exp)), exp);

  /* The call to `expand_expr' could cause last_expr_type and
     last_expr_value to get reset.  Therefore, we set last_expr_value
     and last_expr_type *after* calling expand_expr.  */
  value = expand_expr (exp, want_value ? NULL_RTX : const0_rtx,
		       VOIDmode, 0);
  type = TREE_TYPE (exp);

  /* If all we do is reference a volatile value in memory,
     copy it to a register to be sure it is actually touched.  */
  if (value && GET_CODE (value) == MEM && TREE_THIS_VOLATILE (exp))
    {
      if (TYPE_MODE (type) == VOIDmode)
	;
      else if (TYPE_MODE (type) != BLKmode)
	value = copy_to_reg (value);
      else
	{
	  rtx lab = gen_label_rtx ();

	  /* Compare the value with itself to reference it.  */
	  emit_cmp_and_jump_insns (value, value, EQ,
				   expand_expr (TYPE_SIZE (type),
						NULL_RTX, VOIDmode, 0),
				   BLKmode, 0, lab);
	  emit_label (lab);
	}
    }

  /* If this expression is part of a ({...}) and is in memory, we may have
     to preserve temporaries.  */
  preserve_temp_slots (value);

  /* Free any temporaries used to evaluate this expression.  Any temporary
     used as a result of this expression will already have been preserved
     above.  */
  free_temp_slots ();

  if (want_value)
    {
      last_expr_value = value;
      last_expr_type = type;
    }

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

  /* Don't warn about void constructs.  This includes casting to void,
     void function calls, and statement expressions with a final cast
     to void.  */
  if (VOID_TYPE_P (TREE_TYPE (exp)))
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
    case TRY_CATCH_EXPR:
    case WITH_CLEANUP_EXPR:
    case EXIT_EXPR:
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
      goto maybe_warn;

    case INDIRECT_REF:
      /* Don't warn about automatic dereferencing of references, since
	 the user cannot control it.  */
      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == REFERENCE_TYPE)
	return warn_if_unused_value (TREE_OPERAND (exp, 0));
      /* Fall through.  */

    default:
      /* Referencing a volatile value is a side effect, so don't warn.  */
      if ((DECL_P (exp)
	   || TREE_CODE_CLASS (TREE_CODE (exp)) == 'r')
	  && TREE_THIS_VOLATILE (exp))
	return 0;

      /* If this is an expression which has no operands, there is no value
	 to be unused.  There are no such language-independent codes,
	 but front ends may define such.  */
      if (TREE_CODE_CLASS (TREE_CODE (exp)) == 'e'
	  && TREE_CODE_LENGTH (TREE_CODE (exp)) == 0)
	return 0;

    maybe_warn:
      /* If this is an expression with side effects, don't warn.  */
      if (TREE_SIDE_EFFECTS (exp))
	return 0;

      warning_with_file_and_line (emit_filename, emit_lineno,
				  "value computed is not used");
      return 1;
    }
}

/* Clear out the memory of the last expression evaluated.  */

void
clear_last_expr ()
{
  last_expr_type = NULL_TREE;
  last_expr_value = NULL_RTX;
}

/* Begin a statement-expression, i.e., a series of statements which
   may return a value.  Return the RTL_EXPR for this statement expr.
   The caller must save that value and pass it to
   expand_end_stmt_expr.  If HAS_SCOPE is nonzero, temporaries created
   in the statement-expression are deallocated at the end of the
   expression.  */

tree
expand_start_stmt_expr (has_scope)
     int has_scope;
{
  tree t;

  /* Make the RTL_EXPR node temporary, not momentary,
     so that rtl_expr_chain doesn't become garbage.  */
  t = make_node (RTL_EXPR);
  do_pending_stack_adjust ();
  if (has_scope)
    start_sequence_for_rtl_expr (t);
  else
    start_sequence ();
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
  OK_DEFER_POP;

  if (! last_expr_value || ! last_expr_type)
    {
      last_expr_value = const0_rtx;
      last_expr_type = void_type_node;
    }
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

  clear_last_expr ();
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

  thiscond->desc = COND_NESTING;
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

  emit_jump (cond_stack->data.cond.endif_label);
  emit_label (cond_stack->data.cond.next_label);
  cond_stack->data.cond.next_label = 0;  /* No more _else or _elseif calls.  */
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

  do_pending_stack_adjust ();
  if (thiscond->data.cond.next_label)
    emit_label (thiscond->data.cond.next_label);
  if (thiscond->data.cond.endif_label)
    emit_label (thiscond->data.cond.endif_label);

  POPSTACK (cond_stack);
  clear_last_expr ();
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
  struct nesting *thisloop = ALLOC_NESTING ();

  /* Make an entry on loop_stack for the loop we are entering.  */

  thisloop->desc = LOOP_NESTING;
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

  do_pending_stack_adjust ();
  emit_queue ();
  emit_note (NULL, NOTE_INSN_LOOP_BEG);
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

/* Begin a null, aka do { } while (0) "loop".  But since the contents
   of said loop can still contain a break, we must frob the loop nest.  */

struct nesting *
expand_start_null_loop ()
{
  struct nesting *thisloop = ALLOC_NESTING ();

  /* Make an entry on loop_stack for the loop we are entering.  */

  thisloop->desc = LOOP_NESTING;
  thisloop->next = loop_stack;
  thisloop->all = nesting_stack;
  thisloop->depth = ++nesting_depth;
  thisloop->data.loop.start_label = emit_note (NULL, NOTE_INSN_DELETED);
  thisloop->data.loop.end_label = gen_label_rtx ();
  thisloop->data.loop.alt_end_label = NULL_RTX;
  thisloop->data.loop.continue_label = thisloop->data.loop.end_label;
  thisloop->exit_label = thisloop->data.loop.end_label;
  loop_stack = thisloop;
  nesting_stack = thisloop;

  return thisloop;
}

/* Specify the continuation point for a loop started with
   expand_start_loop_continue_elsewhere.
   Use this at the point in the code to which a continue statement
   should jump.  */

void
expand_loop_continue_here ()
{
  do_pending_stack_adjust ();
  emit_note (NULL, NOTE_INSN_LOOP_CONT);
  emit_label (loop_stack->data.loop.continue_label);
}

/* Finish a loop.  Generate a jump back to the top and the loop-exit label.
   Pop the block off of loop_stack.  */

void
expand_end_loop ()
{
  rtx start_label = loop_stack->data.loop.start_label;
  rtx etc_note;
  int eh_regions, debug_blocks;

  /* Mark the continue-point at the top of the loop if none elsewhere.  */
  if (start_label == loop_stack->data.loop.continue_label)
    emit_note_before (NOTE_INSN_LOOP_CONT, start_label);

  do_pending_stack_adjust ();

  /* If the loop starts with a loop exit, roll that to the end where
     it will optimize together with the jump back.

     If the loop presently looks like this (in pseudo-C):

	LOOP_BEG
	start_label:
	  if (test) goto end_label;
	LOOP_END_TOP_COND
	  body;
	  goto start_label;
	end_label:

     transform it to look like:

	LOOP_BEG
	  goto start_label;
	top_label:
	  body;
	start_label:
	  if (test) goto end_label;
	  goto top_label;
	end_label:

     We rely on the presence of NOTE_INSN_LOOP_END_TOP_COND to mark
     the end of the entry condtional.  Without this, our lexical scan
     can't tell the difference between an entry conditional and a
     body conditional that exits the loop.  Mistaking the two means
     that we can misplace the NOTE_INSN_LOOP_CONT note, which can
     screw up loop unrolling.

     Things will be oh so much better when loop optimization is done
     off of a proper control flow graph...  */

  /* Scan insns from the top of the loop looking for the END_TOP_COND note.  */

  eh_regions = debug_blocks = 0;
  for (etc_note = start_label; etc_note ; etc_note = NEXT_INSN (etc_note))
    if (GET_CODE (etc_note) == NOTE)
      {
	if (NOTE_LINE_NUMBER (etc_note) == NOTE_INSN_LOOP_END_TOP_COND)
	  break;

	/* We must not walk into a nested loop.  */
	else if (NOTE_LINE_NUMBER (etc_note) == NOTE_INSN_LOOP_BEG)
	  {
	    etc_note = NULL_RTX;
	    break;
	  }

	/* At the same time, scan for EH region notes, as we don't want
	   to scrog region nesting.  This shouldn't happen, but...  */
	else if (NOTE_LINE_NUMBER (etc_note) == NOTE_INSN_EH_REGION_BEG)
	  eh_regions++;
	else if (NOTE_LINE_NUMBER (etc_note) == NOTE_INSN_EH_REGION_END)
	  {
	    if (--eh_regions < 0)
	      /* We've come to the end of an EH region, but never saw the
		 beginning of that region.  That means that an EH region
		 begins before the top of the loop, and ends in the middle
		 of it.  The existence of such a situation violates a basic
		 assumption in this code, since that would imply that even
		 when EH_REGIONS is zero, we might move code out of an
		 exception region.  */
	      abort ();
	  }

	/* Likewise for debug scopes.  In this case we'll either (1) move
	   all of the notes if they are properly nested or (2) leave the
	   notes alone and only rotate the loop at high optimization
	   levels when we expect to scrog debug info.  */
	else if (NOTE_LINE_NUMBER (etc_note) == NOTE_INSN_BLOCK_BEG)
	  debug_blocks++;
	else if (NOTE_LINE_NUMBER (etc_note) == NOTE_INSN_BLOCK_END)
	  debug_blocks--;
      }

  if (etc_note
      && optimize
      && eh_regions == 0
      && (debug_blocks == 0 || optimize >= 2)
      && NEXT_INSN (etc_note) != NULL_RTX
      && ! any_condjump_p (get_last_insn ()))
    {
      /* We found one.  Move everything from START to ETC to the end
	 of the loop, and add a jump from the top of the loop.  */
      rtx top_label = gen_label_rtx ();
      rtx start_move = start_label;

      /* If the start label is preceded by a NOTE_INSN_LOOP_CONT note,
	 then we want to move this note also.  */
      if (GET_CODE (PREV_INSN (start_move)) == NOTE
	  && NOTE_LINE_NUMBER (PREV_INSN (start_move)) == NOTE_INSN_LOOP_CONT)
	start_move = PREV_INSN (start_move);

      emit_label_before (top_label, start_move);

      /* Actually move the insns.  If the debug scopes are nested, we
	 can move everything at once.  Otherwise we have to move them
	 one by one and squeeze out the block notes.  */
      if (debug_blocks == 0)
	reorder_insns (start_move, etc_note, get_last_insn ());
      else
	{
	  rtx insn, next_insn;
	  for (insn = start_move; insn; insn = next_insn)
	    {
	      /* Figure out which insn comes after this one.  We have
		 to do this before we move INSN.  */
	      next_insn = (insn == etc_note ? NULL : NEXT_INSN (insn));

	      if (GET_CODE (insn) == NOTE
		  && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG
		      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END))
		continue;

	      reorder_insns (insn, insn, get_last_insn ());
	    }
	}

      /* Add the jump from the top of the loop.  */
      emit_jump_insn_before (gen_jump (start_label), top_label);
      emit_barrier_before (top_label);
      start_label = top_label;
    }

  emit_jump (start_label);
  emit_note (NULL, NOTE_INSN_LOOP_END);
  emit_label (loop_stack->data.loop.end_label);

  POPSTACK (loop_stack);

  clear_last_expr ();
}

/* Finish a null loop, aka do { } while (0).  */

void
expand_end_null_loop ()
{
  do_pending_stack_adjust ();
  emit_label (loop_stack->data.loop.end_label);

  POPSTACK (loop_stack);

  clear_last_expr ();
}

/* Generate a jump to the current loop's continue-point.
   This is usually the top of the loop, but may be specified
   explicitly elsewhere.  If not currently inside a loop,
   return 0 and do nothing; caller will print an error message.  */

int
expand_continue_loop (whichloop)
     struct nesting *whichloop;
{
  /* Emit information for branch prediction.  */
  rtx note;

  if (flag_guess_branch_prob)
    {
      note = emit_note (NULL, NOTE_INSN_PREDICTION);
      NOTE_PREDICTION (note) = NOTE_PREDICT (PRED_CONTINUE, IS_TAKEN);
    }
  clear_last_expr ();
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
  clear_last_expr ();
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
  rtx label = gen_label_rtx ();
  rtx last_insn;
  clear_last_expr ();

  if (whichloop == 0)
    whichloop = loop_stack;
  if (whichloop == 0)
    return 0;
  /* In order to handle fixups, we actually create a conditional jump
     around an unconditional branch to exit the loop.  If fixups are
     necessary, they go before the unconditional branch.  */

  do_jump (cond, NULL_RTX, label);
  last_insn = get_last_insn ();
  if (GET_CODE (last_insn) == CODE_LABEL)
    whichloop->data.loop.alt_end_label = last_insn;
  expand_goto_internal (NULL_TREE, whichloop->data.loop.end_label,
			NULL_RTX);
  emit_label (label);

  return 1;
}

/* Like expand_exit_loop_if_false except also emit a note marking
   the end of the conditional.  Should only be used immediately
   after expand_loop_start.  */

int
expand_exit_loop_top_cond (whichloop, cond)
     struct nesting *whichloop;
     tree cond;
{
  if (! expand_exit_loop_if_false (whichloop, cond))
    return 0;

  emit_note (NULL, NOTE_INSN_LOOP_END_TOP_COND);
  return 1;
}

/* Return nonzero if the loop nest is empty.  Else return zero.  */

int
stmt_loop_nest_empty ()
{
  /* cfun->stmt can be NULL if we are building a call to get the
     EH context for a setjmp/longjmp EH target and the current
     function was a deferred inline function.  */
  return (cfun->stmt == NULL || loop_stack == NULL);
}

/* Return nonzero if we should preserve sub-expressions as separate
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

  if (optimize == 0 || cfun == 0 || cfun->stmt == 0 || loop_stack == 0)
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
  clear_last_expr ();
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
  rtx last_insn;

  last_insn = get_last_insn ();

  /* If this function was declared to return a value, but we
     didn't, clobber the return registers so that they are not
     propagated live to the rest of the function.  */
  clobber_return_register ();

  expand_null_return_1 (last_insn);
}

/* Try to guess whether the value of return means error code.  */
static enum br_predictor
return_prediction (val)
     rtx val;
{
  /* Different heuristics for pointers and scalars.  */
  if (POINTER_TYPE_P (TREE_TYPE (DECL_RESULT (current_function_decl))))
    {
      /* NULL is usually not returned.  */
      if (val == const0_rtx)
	return PRED_NULL_RETURN;
    }
  else
    {
      /* Negative return values are often used to indicate
         errors.  */
      if (GET_CODE (val) == CONST_INT
	  && INTVAL (val) < 0)
	return PRED_NEGATIVE_RETURN;
      /* Constant return values are also usually erors,
         zero/one often mean booleans so exclude them from the
	 heuristics.  */
      if (CONSTANT_P (val)
	  && (val != const0_rtx && val != const1_rtx))
	return PRED_CONST_RETURN;
    }
  return PRED_NO_PREDICTION;
}

/* Generate RTL to return from the current function, with value VAL.  */

static void
expand_value_return (val)
     rtx val;
{
  rtx last_insn;
  rtx return_reg;
  enum br_predictor pred;

  if (flag_guess_branch_prob
      && (pred = return_prediction (val)) != PRED_NO_PREDICTION)
    {
      /* Emit information for branch prediction.  */
      rtx note;

      note = emit_note (NULL, NOTE_INSN_PREDICTION);

      NOTE_PREDICTION (note) = NOTE_PREDICT (pred, NOT_TAKEN);

    }

  last_insn = get_last_insn ();
  return_reg = DECL_RTL (DECL_RESULT (current_function_decl));

  /* Copy the value to the return location
     unless it's already there.  */

  if (return_reg != val)
    {
      tree type = TREE_TYPE (DECL_RESULT (current_function_decl));
#ifdef PROMOTE_FUNCTION_RETURN
      int unsignedp = TREE_UNSIGNED (type);
      enum machine_mode old_mode
	= DECL_MODE (DECL_RESULT (current_function_decl));
      enum machine_mode mode
	= promote_mode (type, old_mode, &unsignedp, 1);

      if (mode != old_mode)
	val = convert_modes (mode, old_mode, val, unsignedp);
#endif
      if (GET_CODE (return_reg) == PARALLEL)
	emit_group_load (return_reg, val, int_size_in_bytes (type));
      else
	emit_move_insn (return_reg, val);
    }

  expand_null_return_1 (last_insn);
}

/* Output a return with no value.  If LAST_INSN is nonzero,
   pretend that the return takes place after LAST_INSN.  */

static void
expand_null_return_1 (last_insn)
     rtx last_insn;
{
  rtx end_label = cleanup_label ? cleanup_label : return_label;

  clear_pending_stack_adjust ();
  do_pending_stack_adjust ();
  clear_last_expr ();

  if (end_label == 0)
     end_label = return_label = gen_label_rtx ();
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
  rtx result_rtl;
  rtx val = 0;
  tree retval_rhs;

  /* If function wants no value, give it none.  */
  if (TREE_CODE (TREE_TYPE (TREE_TYPE (current_function_decl))) == VOID_TYPE)
    {
      expand_expr (retval, NULL_RTX, VOIDmode, 0);
      emit_queue ();
      expand_null_return ();
      return;
    }

  if (retval == error_mark_node)
    {
      /* Treat this like a return of no value from a function that
	 returns a value.  */
      expand_null_return ();
      return;
    }
  else if (TREE_CODE (retval) == RESULT_DECL)
    retval_rhs = retval;
  else if ((TREE_CODE (retval) == MODIFY_EXPR || TREE_CODE (retval) == INIT_EXPR)
	   && TREE_CODE (TREE_OPERAND (retval, 0)) == RESULT_DECL)
    retval_rhs = TREE_OPERAND (retval, 1);
  else if (VOID_TYPE_P (TREE_TYPE (retval)))
    /* Recognize tail-recursive call to void function.  */
    retval_rhs = retval;
  else
    retval_rhs = NULL_TREE;

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
      start_cleanup_deferral ();
      expr = build (MODIFY_EXPR, TREE_TYPE (TREE_TYPE (current_function_decl)),
		    DECL_RESULT (current_function_decl),
		    TREE_OPERAND (retval_rhs, 1));
      TREE_SIDE_EFFECTS (expr) = 1;
      expand_return (expr);
      emit_label (label);

      expr = build (MODIFY_EXPR, TREE_TYPE (TREE_TYPE (current_function_decl)),
		    DECL_RESULT (current_function_decl),
		    TREE_OPERAND (retval_rhs, 2));
      TREE_SIDE_EFFECTS (expr) = 1;
      expand_return (expr);
      end_cleanup_deferral ();
      return;
    }

  result_rtl = DECL_RTL (DECL_RESULT (current_function_decl));

  /* If the result is an aggregate that is being returned in one (or more)
     registers, load the registers here.  The compiler currently can't handle
     copying a BLKmode value into registers.  We could put this code in a
     more general area (for use by everyone instead of just function
     call/return), but until this feature is generally usable it is kept here
     (and in expand_call).  The value must go into a pseudo in case there
     are cleanups that will clobber the real return register.  */

  if (retval_rhs != 0
      && TYPE_MODE (TREE_TYPE (retval_rhs)) == BLKmode
      && GET_CODE (result_rtl) == REG)
    {
      int i;
      unsigned HOST_WIDE_INT bitpos, xbitpos;
      unsigned HOST_WIDE_INT big_endian_correction = 0;
      unsigned HOST_WIDE_INT bytes
	= int_size_in_bytes (TREE_TYPE (retval_rhs));
      int n_regs = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
      unsigned int bitsize
	= MIN (TYPE_ALIGN (TREE_TYPE (retval_rhs)), BITS_PER_WORD);
      rtx *result_pseudos = (rtx *) alloca (sizeof (rtx) * n_regs);
      rtx result_reg, src = NULL_RTX, dst = NULL_RTX;
      rtx result_val = expand_expr (retval_rhs, NULL_RTX, VOIDmode, 0);
      enum machine_mode tmpmode, result_reg_mode;

      if (bytes == 0)
	{
	  expand_null_return ();
	  return;
	}

      /* Structures whose size is not a multiple of a word are aligned
	 to the least significant byte (to the right).  On a BYTES_BIG_ENDIAN
	 machine, this means we must skip the empty high order bytes when
	 calculating the bit offset.  */
      if (BYTES_BIG_ENDIAN
	  && bytes % UNITS_PER_WORD)
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

	      /* Clear the destination before we move anything into it.  */
	      emit_move_insn (dst, CONST0_RTX (GET_MODE (dst)));
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
					      NULL_RTX, word_mode, word_mode,
					      BITS_PER_WORD),
			   BITS_PER_WORD);
	}

      /* Find the smallest integer mode large enough to hold the
	 entire structure and use that mode instead of BLKmode
	 on the USE insn for the return register.  */
      for (tmpmode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	   tmpmode != VOIDmode;
	   tmpmode = GET_MODE_WIDER_MODE (tmpmode))
	/* Have we found a large enough mode?  */
	if (GET_MODE_SIZE (tmpmode) >= bytes)
	  break;

      /* No suitable mode found.  */
      if (tmpmode == VOIDmode)
	abort ();

      PUT_MODE (result_rtl, tmpmode);

      if (GET_MODE_SIZE (tmpmode) < GET_MODE_SIZE (word_mode))
	result_reg_mode = word_mode;
      else
	result_reg_mode = tmpmode;
      result_reg = gen_reg_rtx (result_reg_mode);

      emit_queue ();
      for (i = 0; i < n_regs; i++)
	emit_move_insn (operand_subword (result_reg, i, 0, result_reg_mode),
			result_pseudos[i]);

      if (tmpmode != result_reg_mode)
	result_reg = gen_lowpart (tmpmode, result_reg);

      expand_value_return (result_reg);
    }
  else if (retval_rhs != 0
	   && !VOID_TYPE_P (TREE_TYPE (retval_rhs))
	   && (GET_CODE (result_rtl) == REG
	       || (GET_CODE (result_rtl) == PARALLEL)))
    {
      /* Calculate the return value into a temporary (usually a pseudo
         reg).  */
      tree ot = TREE_TYPE (DECL_RESULT (current_function_decl));
      tree nt = build_qualified_type (ot, TYPE_QUALS (ot) | TYPE_QUAL_CONST);

      val = assign_temp (nt, 0, 0, 1);
      val = expand_expr (retval_rhs, val, GET_MODE (val), 0);
      val = force_not_mem (val);
      emit_queue ();
      /* Return the calculated value, doing cleanups first.  */
      expand_value_return (val);
    }
  else
    {
      /* No cleanups or no hard reg used;
	 calculate value into hard return reg.  */
      expand_expr (retval, const0_rtx, VOIDmode, 0);
      emit_queue ();
      expand_value_return (result_rtl);
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

/* Attempt to optimize a potential tail recursion call into a goto.
   ARGUMENTS are the arguments to a CALL_EXPR; LAST_INSN indicates
   where to place the jump to the tail recursion label.

   Return TRUE if the call was optimized into a goto.  */

int
optimize_tail_recursion (arguments, last_insn)
     tree arguments;
     rtx last_insn;
{
  /* Finish checking validity, and if valid emit code to set the
     argument variables for the new call.  */
  if (tail_recursion_args (arguments, DECL_ARGUMENTS (current_function_decl)))
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
      return 1;
    }
  return 0;
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
  tree a = actuals, f = formals;
  int i;
  rtx *argvec;

  /* Check that number and types of actuals are compatible
     with the formals.  This is not always true in valid C code.
     Also check that no formal needs to be addressable
     and that all formals are scalars.  */

  /* Also count the args.  */

  for (a = actuals, f = formals, i = 0; a && f; a = TREE_CHAIN (a), f = TREE_CHAIN (f), i++)
    {
      if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_VALUE (a)))
	  != TYPE_MAIN_VARIANT (TREE_TYPE (f)))
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
      int j;
      for (f = formals, j = 0; j < i; f = TREE_CHAIN (f), j++)
	if (reg_mentioned_p (DECL_RTL (f), argvec[i]))
	  {
	    copy = 1;
	    break;
	  }
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
	{
	  rtx tmp = argvec[i];
	  int unsignedp = TREE_UNSIGNED (TREE_TYPE (TREE_VALUE (a)));
	  promote_mode(TREE_TYPE (TREE_VALUE (a)), GET_MODE (tmp),
		       &unsignedp, 0);
	  if (DECL_MODE (f) != GET_MODE (DECL_RTL (f)))
	    {
	      tmp = gen_reg_rtx (DECL_MODE (f));
	      convert_move (tmp, argvec[i], unsignedp);
	    }
	  convert_move (DECL_RTL (f), tmp, unsignedp);
	}
    }

  free_temp_slots ();
  return 1;
}

/* Generate the RTL code for entering a binding contour.
   The variables are declared one by one, by calls to `expand_decl'.

   FLAGS is a bitwise or of the following flags:

     1 - Nonzero if this construct should be visible to
         `exit_something'.

     2 - Nonzero if this contour does not require a
	 NOTE_INSN_BLOCK_BEG note.  Virtually all calls from
	 language-independent code should set this flag because they
	 will not create corresponding BLOCK nodes.  (There should be
	 a one-to-one correspondence between NOTE_INSN_BLOCK_BEG notes
	 and BLOCKs.)  If this flag is set, MARK_ENDS should be zero
	 when expand_end_bindings is called.

    If we are creating a NOTE_INSN_BLOCK_BEG note, a BLOCK may
    optionally be supplied.  If so, it becomes the NOTE_BLOCK for the
    note.  */

void
expand_start_bindings_and_block (flags, block)
     int flags;
     tree block;
{
  struct nesting *thisblock = ALLOC_NESTING ();
  rtx note;
  int exit_flag = ((flags & 1) != 0);
  int block_flag = ((flags & 2) == 0);

  /* If a BLOCK is supplied, then the caller should be requesting a
     NOTE_INSN_BLOCK_BEG note.  */
  if (!block_flag && block)
    abort ();

  /* Create a note to mark the beginning of the block.  */
  if (block_flag)
    {
      note = emit_note (NULL, NOTE_INSN_BLOCK_BEG);
      NOTE_BLOCK (note) = block;
    }
  else
    note = emit_note (NULL, NOTE_INSN_DELETED);

  /* Make an entry on block_stack for the block we are entering.  */

  thisblock->desc = BLOCK_NESTING;
  thisblock->next = block_stack;
  thisblock->all = nesting_stack;
  thisblock->depth = ++nesting_depth;
  thisblock->data.block.stack_level = 0;
  thisblock->data.block.cleanups = 0;
  thisblock->data.block.n_function_calls = 0;
  thisblock->data.block.exception_region = 0;
  thisblock->data.block.block_target_temp_slot_level = target_temp_slot_level;

  thisblock->data.block.conditional_code = 0;
  thisblock->data.block.last_unconditional_cleanup = note;
  /* When we insert instructions after the last unconditional cleanup,
     we don't adjust last_insn.  That means that a later add_insn will
     clobber the instructions we've just added.  The easiest way to
     fix this is to just insert another instruction here, so that the
     instructions inserted after the last unconditional cleanup are
     never the last instruction.  */
  emit_note (NULL, NOTE_INSN_DELETED);

  if (block_stack
      && !(block_stack->data.block.cleanups == NULL_TREE
	   && block_stack->data.block.outer_cleanups == NULL_TREE))
    thisblock->data.block.outer_cleanups
      = tree_cons (NULL_TREE, block_stack->data.block.cleanups,
		   block_stack->data.block.outer_cleanups);
  else
    thisblock->data.block.outer_cleanups = 0;
  thisblock->data.block.label_chain = 0;
  thisblock->data.block.innermost_stack_block = stack_block_stack;
  thisblock->data.block.first_insn = note;
  thisblock->data.block.block_start_count = ++current_block_start_count;
  thisblock->exit_label = exit_flag ? gen_label_rtx () : 0;
  block_stack = thisblock;
  nesting_stack = thisblock;

  /* Make a new level for allocating stack slots.  */
  push_temp_slots ();
}

/* Specify the scope of temporaries created by TARGET_EXPRs.  Similar
   to CLEANUP_POINT_EXPR, but handles cases when a series of calls to
   expand_expr are made.  After we end the region, we know that all
   space for all temporaries that were created by TARGET_EXPRs will be
   destroyed and their space freed for reuse.  */

void
expand_start_target_temps ()
{
  /* This is so that even if the result is preserved, the space
     allocated will be freed, as we know that it is no longer in use.  */
  push_temp_slots ();

  /* Start a new binding layer that will keep track of all cleanup
     actions to be performed.  */
  expand_start_bindings (2);

  target_temp_slot_level = temp_slot_level;
}

void
expand_end_target_temps ()
{
  expand_end_bindings (NULL_TREE, 0, 0);

  /* This is so that even if the result is preserved, the space
     allocated will be freed, as we know that it is no longer in use.  */
  pop_temp_slots ();
}

/* Given a pointer to a BLOCK node return nonzero if (and only if) the node
   in question represents the outermost pair of curly braces (i.e. the "body
   block") of a function or method.

   For any BLOCK node representing a "body block" of a function or method, the
   BLOCK_SUPERCONTEXT of the node will point to another BLOCK node which
   represents the outermost (function) scope for the function or method (i.e.
   the one which includes the formal parameters).  The BLOCK_SUPERCONTEXT of
   *that* node in turn will point to the relevant FUNCTION_DECL node.  */

int
is_body_block (stmt)
     tree stmt;
{
  if (TREE_CODE (stmt) == BLOCK)
    {
      tree parent = BLOCK_SUPERCONTEXT (stmt);

      if (parent && TREE_CODE (parent) == BLOCK)
	{
	  tree grandparent = BLOCK_SUPERCONTEXT (parent);

	  if (grandparent && TREE_CODE (grandparent) == FUNCTION_DECL)
	    return 1;
	}
    }

  return 0;
}

/* True if we are currently emitting insns in an area of output code
   that is controlled by a conditional expression.  This is used by
   the cleanup handling code to generate conditional cleanup actions.  */

int
conditional_context ()
{
  return block_stack && block_stack->data.block.conditional_code;
}

/* Return an opaque pointer to the current nesting level, so frontend code
   can check its own sanity.  */

struct nesting *
current_nesting_level ()
{
  return cfun ? block_stack : 0;
}

/* Emit a handler label for a nonlocal goto handler.
   Also emit code to store the handler label in SLOT before BEFORE_INSN.  */

static rtx
expand_nl_handler_label (slot, before_insn)
     rtx slot, before_insn;
{
  rtx insns;
  rtx handler_label = gen_label_rtx ();

  /* Don't let cleanup_cfg delete the handler.  */
  LABEL_PRESERVE_P (handler_label) = 1;

  start_sequence ();
  emit_move_insn (slot, gen_rtx_LABEL_REF (Pmode, handler_label));
  insns = get_insns ();
  end_sequence ();
  emit_insn_before (insns, before_insn);

  emit_label (handler_label);

  return handler_label;
}

/* Emit code to restore vital registers at the beginning of a nonlocal goto
   handler.  */
static void
expand_nl_goto_receiver ()
{
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
      static const struct elims {const int from, to;} elim_regs[] = ELIMINABLE_REGS;
      size_t i;

      for (i = 0; i < ARRAY_SIZE (elim_regs); i++)
	if (elim_regs[i].from == ARG_POINTER_REGNUM
	    && elim_regs[i].to == HARD_FRAME_POINTER_REGNUM)
	  break;

      if (i == ARRAY_SIZE (elim_regs))
#endif
	{
	  /* Now restore our arg pointer from the address at which it
	     was saved in our stack frame.  */
	  emit_move_insn (virtual_incoming_args_rtx,
			  copy_to_reg (get_arg_pointer_save_area (cfun)));
	}
    }
#endif

#ifdef HAVE_nonlocal_goto_receiver
  if (HAVE_nonlocal_goto_receiver)
    emit_insn (gen_nonlocal_goto_receiver ());
#endif
}

/* Make handlers for nonlocal gotos taking place in the function calls in
   block THISBLOCK.  */

static void
expand_nl_goto_receivers (thisblock)
     struct nesting *thisblock;
{
  tree link;
  rtx afterward = gen_label_rtx ();
  rtx insns, slot;
  rtx label_list;
  int any_invalid;

  /* Record the handler address in the stack slot for that purpose,
     during this block, saving and restoring the outer value.  */
  if (thisblock->next != 0)
    for (slot = nonlocal_goto_handler_slots; slot; slot = XEXP (slot, 1))
      {
	rtx save_receiver = gen_reg_rtx (Pmode);
	emit_move_insn (XEXP (slot, 0), save_receiver);

	start_sequence ();
	emit_move_insn (save_receiver, XEXP (slot, 0));
	insns = get_insns ();
	end_sequence ();
	emit_insn_before (insns, thisblock->data.block.first_insn);
      }

  /* Jump around the handlers; they run only when specially invoked.  */
  emit_jump (afterward);

  /* Make a separate handler for each label.  */
  link = nonlocal_labels;
  slot = nonlocal_goto_handler_slots;
  label_list = NULL_RTX;
  for (; link; link = TREE_CHAIN (link), slot = XEXP (slot, 1))
    /* Skip any labels we shouldn't be able to jump to from here,
       we generate one special handler for all of them below which just calls
       abort.  */
    if (! DECL_TOO_LATE (TREE_VALUE (link)))
      {
	rtx lab;
	lab = expand_nl_handler_label (XEXP (slot, 0),
				       thisblock->data.block.first_insn);
	label_list = gen_rtx_EXPR_LIST (VOIDmode, lab, label_list);

	expand_nl_goto_receiver ();

	/* Jump to the "real" nonlocal label.  */
	expand_goto (TREE_VALUE (link));
      }

  /* A second pass over all nonlocal labels; this time we handle those
     we should not be able to jump to at this point.  */
  link = nonlocal_labels;
  slot = nonlocal_goto_handler_slots;
  any_invalid = 0;
  for (; link; link = TREE_CHAIN (link), slot = XEXP (slot, 1))
    if (DECL_TOO_LATE (TREE_VALUE (link)))
      {
	rtx lab;
	lab = expand_nl_handler_label (XEXP (slot, 0),
				       thisblock->data.block.first_insn);
	label_list = gen_rtx_EXPR_LIST (VOIDmode, lab, label_list);
	any_invalid = 1;
      }

  if (any_invalid)
    {
      expand_nl_goto_receiver ();
      expand_builtin_trap ();
    }

  nonlocal_goto_handler_labels = label_list;
  emit_label (afterward);
}

/* Warn about any unused VARS (which may contain nodes other than
   VAR_DECLs, but such nodes are ignored).  The nodes are connected
   via the TREE_CHAIN field.  */

void
warn_about_unused_variables (vars)
     tree vars;
{
  tree decl;

  if (warn_unused_variable)
    for (decl = vars; decl; decl = TREE_CHAIN (decl))
      if (TREE_CODE (decl) == VAR_DECL
	  && ! TREE_USED (decl)
	  && ! DECL_IN_SYSTEM_HEADER (decl)
	  && DECL_NAME (decl) && ! DECL_ARTIFICIAL (decl))
	warning_with_decl (decl, "unused variable `%s'");
}

/* Generate RTL code to terminate a binding contour.

   VARS is the chain of VAR_DECL nodes for the variables bound in this
   contour.  There may actually be other nodes in this chain, but any
   nodes other than VAR_DECLS are ignored.

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
  struct nesting *thisblock = block_stack;

  /* If any of the variables in this scope were not used, warn the
     user.  */
  warn_about_unused_variables (vars);

  if (thisblock->exit_label)
    {
      do_pending_stack_adjust ();
      emit_label (thisblock->exit_label);
    }

  /* If necessary, make handlers for nonlocal gotos taking
     place in the function calls in this block.  */
  if (function_call_count != thisblock->data.block.n_function_calls
      && nonlocal_labels
      /* Make handler for outermost block
	 if there were any nonlocal gotos to this function.  */
      && (thisblock->next == 0 ? current_function_has_nonlocal_label
	  /* Make handler for inner block if it has something
	     special to do when you jump out of it.  */
	  : (thisblock->data.block.cleanups != 0
	     || thisblock->data.block.stack_level != 0)))
    expand_nl_goto_receivers (thisblock);

  /* Don't allow jumping into a block that has a stack level.
     Cleanups are allowed, though.  */
  if (dont_jump_in
      || thisblock->data.block.stack_level != 0)
    {
      struct label_chain *chain;

      /* Any labels in this block are no longer valid to go to.
	 Mark them to cause an error message.  */
      for (chain = thisblock->data.block.label_chain; chain; chain = chain->next)
	{
	  DECL_TOO_LATE (chain->label) = 1;
	  /* If any goto without a fixup came to this label,
	     that must be an error, because gotos without fixups
	     come from outside all saved stack-levels.  */
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
      int reachable;
      rtx insn;

      /* Don't let cleanups affect ({...}) constructs.  */
      int old_expr_stmts_for_value = expr_stmts_for_value;
      rtx old_last_expr_value = last_expr_value;
      tree old_last_expr_type = last_expr_type;
      expr_stmts_for_value = 0;

      /* Only clean up here if this point can actually be reached.  */
      insn = get_last_insn ();
      if (GET_CODE (insn) == NOTE)
	insn = prev_nonnote_insn (insn);
      reachable = (! insn || GET_CODE (insn) != BARRIER);

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
	  if (nonlocal_goto_handler_slots != 0)
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
    {
      rtx note = emit_note (NULL, NOTE_INSN_BLOCK_END);
      NOTE_BLOCK (note) = NOTE_BLOCK (thisblock->data.block.first_insn);
    }
  else
    /* Get rid of the beginning-mark if we don't make an end-mark.  */
    NOTE_LINE_NUMBER (thisblock->data.block.first_insn) = NOTE_INSN_DELETED;

  /* Restore the temporary level of TARGET_EXPRs.  */
  target_temp_slot_level = thisblock->data.block.block_target_temp_slot_level;

  /* Restore block_stack level for containing block.  */

  stack_block_stack = thisblock->data.block.innermost_stack_block;
  POPSTACK (block_stack);

  /* Pop the stack slot nesting and free any slots at this level.  */
  pop_temp_slots ();
}

/* Generate code to save the stack pointer at the start of the current block
   and set up to restore it on exit.  */

void
save_stack_pointer ()
{
  struct nesting *thisblock = block_stack;

  if (thisblock->data.block.stack_level == 0)
    {
      emit_stack_save (thisblock->next ? SAVE_BLOCK : SAVE_FUNCTION,
		       &thisblock->data.block.stack_level,
		       thisblock->data.block.first_insn);
      stack_block_stack = thisblock;
    }
}

/* Generate RTL for the automatic variable declaration DECL.
   (Other kinds of declarations are simply ignored if seen here.)  */

void
expand_decl (decl)
     tree decl;
{
  struct nesting *thisblock;
  tree type;

  type = TREE_TYPE (decl);

  /* For a CONST_DECL, set mode, alignment, and sizes from those of the
     type in case this node is used in a reference.  */
  if (TREE_CODE (decl) == CONST_DECL)
    {
      DECL_MODE (decl) = TYPE_MODE (type);
      DECL_ALIGN (decl) = TYPE_ALIGN (type);
      DECL_SIZE (decl) = TYPE_SIZE (type);
      DECL_SIZE_UNIT (decl) = TYPE_SIZE_UNIT (type);
      return;
    }

  /* Otherwise, only automatic variables need any expansion done.  Static and
     external variables, and external functions, will be handled by
     `assemble_variable' (called from finish_decl).  TYPE_DECL requires
     nothing.  PARM_DECLs are handled in `assign_parms'.  */
  if (TREE_CODE (decl) != VAR_DECL)
    return;

  if (TREE_STATIC (decl) || DECL_EXTERNAL (decl))
    return;

  thisblock = block_stack;

  /* Create the RTL representation for the variable.  */

  if (type == error_mark_node)
    SET_DECL_RTL (decl, gen_rtx_MEM (BLKmode, const0_rtx));

  else if (DECL_SIZE (decl) == 0)
    /* Variable with incomplete type.  */
    {
      rtx x;
      if (DECL_INITIAL (decl) == 0)
	/* Error message was already done; now avoid a crash.  */
	x = gen_rtx_MEM (BLKmode, const0_rtx);
      else
	/* An initializer is going to decide the size of this array.
	   Until we know the size, represent its address with a reg.  */
	x = gen_rtx_MEM (BLKmode, gen_reg_rtx (Pmode));

      set_mem_attributes (x, decl, 1);
      SET_DECL_RTL (decl, x);
    }
  else if (DECL_MODE (decl) != BLKmode
	   /* If -ffloat-store, don't put explicit float vars
	      into regs.  */
	   && !(flag_float_store
		&& TREE_CODE (type) == REAL_TYPE)
	   && ! TREE_THIS_VOLATILE (decl)
	   && ! DECL_NONLOCAL (decl)
	   && (DECL_REGISTER (decl) || optimize))
    {
      /* Automatic variable that can go in a register.  */
      int unsignedp = TREE_UNSIGNED (type);
      enum machine_mode reg_mode
	= promote_mode (type, DECL_MODE (decl), &unsignedp, 0);

      SET_DECL_RTL (decl, gen_reg_rtx (reg_mode));

      if (GET_CODE (DECL_RTL (decl)) == REG)
	REGNO_DECL (REGNO (DECL_RTL (decl))) = decl;
      else if (GET_CODE (DECL_RTL (decl)) == CONCAT)
	{
	  REGNO_DECL (REGNO (XEXP (DECL_RTL (decl), 0))) = decl;
	  REGNO_DECL (REGNO (XEXP (DECL_RTL (decl), 1))) = decl;
	}

      mark_user_reg (DECL_RTL (decl));

      if (POINTER_TYPE_P (type))
	mark_reg_pointer (DECL_RTL (decl),
			  TYPE_ALIGN (TREE_TYPE (TREE_TYPE (decl))));

      maybe_set_unchanging (DECL_RTL (decl), decl);

      /* If something wants our address, try to use ADDRESSOF.  */
      if (TREE_ADDRESSABLE (decl))
	put_var_into_stack (decl, /*rescan=*/false);
    }

  else if (TREE_CODE (DECL_SIZE_UNIT (decl)) == INTEGER_CST
	   && ! (flag_stack_check && ! STACK_CHECK_BUILTIN
		 && 0 < compare_tree_int (DECL_SIZE_UNIT (decl),
					  STACK_CHECK_MAX_VAR_SIZE)))
    {
      /* Variable of fixed size that goes on the stack.  */
      rtx oldaddr = 0;
      rtx addr;
      rtx x;

      /* If we previously made RTL for this decl, it must be an array
	 whose size was determined by the initializer.
	 The old address was a register; set that register now
	 to the proper address.  */
      if (DECL_RTL_SET_P (decl))
	{
	  if (GET_CODE (DECL_RTL (decl)) != MEM
	      || GET_CODE (XEXP (DECL_RTL (decl), 0)) != REG)
	    abort ();
	  oldaddr = XEXP (DECL_RTL (decl), 0);
	}

      /* Set alignment we actually gave this decl.  */
      DECL_ALIGN (decl) = (DECL_MODE (decl) == BLKmode ? BIGGEST_ALIGNMENT
			   : GET_MODE_BITSIZE (DECL_MODE (decl)));
      DECL_USER_ALIGN (decl) = 0;

      x = assign_temp (decl, 1, 1, 1);
      set_mem_attributes (x, decl, 1);
      SET_DECL_RTL (decl, x);

      if (oldaddr)
	{
	  addr = force_operand (XEXP (DECL_RTL (decl), 0), oldaddr);
	  if (addr != oldaddr)
	    emit_move_insn (oldaddr, addr);
	}
    }
  else
    /* Dynamic-size object: must push space on the stack.  */
    {
      rtx address, size, x;

      /* Record the stack pointer on entry to block, if have
	 not already done so.  */
      do_pending_stack_adjust ();
      save_stack_pointer ();

      /* In function-at-a-time mode, variable_size doesn't expand this,
	 so do it now.  */
      if (TREE_CODE (type) == ARRAY_TYPE && TYPE_DOMAIN (type))
	expand_expr (TYPE_MAX_VALUE (TYPE_DOMAIN (type)),
		     const0_rtx, VOIDmode, 0);

      /* Compute the variable's size, in bytes.  */
      size = expand_expr (DECL_SIZE_UNIT (decl), NULL_RTX, VOIDmode, 0);
      free_temp_slots ();

      /* Allocate space on the stack for the variable.  Note that
	 DECL_ALIGN says how the variable is to be aligned and we
	 cannot use it to conclude anything about the alignment of
	 the size.  */
      address = allocate_dynamic_stack_space (size, NULL_RTX,
					      TYPE_ALIGN (TREE_TYPE (decl)));

      /* Reference the variable indirect through that rtx.  */
      x = gen_rtx_MEM (DECL_MODE (decl), address);
      set_mem_attributes (x, decl, 1);
      SET_DECL_RTL (decl, x);


      /* Indicate the alignment we actually gave this variable.  */
#ifdef STACK_BOUNDARY
      DECL_ALIGN (decl) = STACK_BOUNDARY;
#else
      DECL_ALIGN (decl) = BIGGEST_ALIGNMENT;
#endif
      DECL_USER_ALIGN (decl) = 0;
    }
}

/* Emit code to perform the initialization of a declaration DECL.  */

void
expand_decl_init (decl)
     tree decl;
{
  int was_used = TREE_USED (decl);

  /* If this is a CONST_DECL, we don't have to generate any code.  Likewise
     for static decls.  */
  if (TREE_CODE (decl) == CONST_DECL
      || TREE_STATIC (decl))
    return;

  /* Compute and store the initial value now.  */

  push_temp_slots ();

  if (DECL_INITIAL (decl) == error_mark_node)
    {
      enum tree_code code = TREE_CODE (TREE_TYPE (decl));

      if (code == INTEGER_TYPE || code == REAL_TYPE || code == ENUMERAL_TYPE
	  || code == POINTER_TYPE || code == REFERENCE_TYPE)
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
  pop_temp_slots ();
}

/* CLEANUP is an expression to be executed at exit from this binding contour;
   for example, in C++, it might call the destructor for this variable.

   We wrap CLEANUP in an UNSAVE_EXPR node, so that we can expand the
   CLEANUP multiple times, and have the correct semantics.  This
   happens in exception handling, for gotos, returns, breaks that
   leave the current scope.

   If CLEANUP is nonzero and DECL is zero, we record a cleanup
   that is not associated with any particular variable.  */

int
expand_decl_cleanup (decl, cleanup)
     tree decl, cleanup;
{
  struct nesting *thisblock;

  /* Error if we are not in any block.  */
  if (cfun == 0 || block_stack == 0)
    return 0;

  thisblock = block_stack;

  /* Record the cleanup if there is one.  */

  if (cleanup != 0)
    {
      tree t;
      rtx seq;
      tree *cleanups = &thisblock->data.block.cleanups;
      int cond_context = conditional_context ();

      if (cond_context)
	{
	  rtx flag = gen_reg_rtx (word_mode);
	  rtx set_flag_0;
	  tree cond;

	  start_sequence ();
	  emit_move_insn (flag, const0_rtx);
	  set_flag_0 = get_insns ();
	  end_sequence ();

	  thisblock->data.block.last_unconditional_cleanup
	    = emit_insn_after (set_flag_0,
				thisblock->data.block.last_unconditional_cleanup);

	  emit_move_insn (flag, const1_rtx);

	  cond = build_decl (VAR_DECL, NULL_TREE,
			     (*lang_hooks.types.type_for_mode) (word_mode, 1));
	  SET_DECL_RTL (cond, flag);

	  /* Conditionalize the cleanup.  */
	  cleanup = build (COND_EXPR, void_type_node,
			   (*lang_hooks.truthvalue_conversion) (cond),
			   cleanup, integer_zero_node);
	  cleanup = fold (cleanup);

	  cleanups = &thisblock->data.block.cleanups;
	}

      cleanup = unsave_expr (cleanup);

      t = *cleanups = tree_cons (decl, cleanup, *cleanups);

      if (! cond_context)
	/* If this block has a cleanup, it belongs in stack_block_stack.  */
	stack_block_stack = thisblock;

      if (cond_context)
	{
	  start_sequence ();
	}

      if (! using_eh_for_cleanups_p)
	TREE_ADDRESSABLE (t) = 1;
      else
	expand_eh_region_start ();

      if (cond_context)
	{
	  seq = get_insns ();
	  end_sequence ();
	  if (seq)
	    thisblock->data.block.last_unconditional_cleanup
	      = emit_insn_after (seq,
				 thisblock->data.block.last_unconditional_cleanup);
	}
      else
	{
	  thisblock->data.block.last_unconditional_cleanup
	    = get_last_insn ();
	  /* When we insert instructions after the last unconditional cleanup,
	     we don't adjust last_insn.  That means that a later add_insn will
	     clobber the instructions we've just added.  The easiest way to
	     fix this is to just insert another instruction here, so that the
	     instructions inserted after the last unconditional cleanup are
	     never the last instruction.  */
	  emit_note (NULL, NOTE_INSN_DELETED);
	}
    }
  return 1;
}

/* Like expand_decl_cleanup, but maybe only run the cleanup if an exception
   is thrown.  */

int
expand_decl_cleanup_eh (decl, cleanup, eh_only)
     tree decl, cleanup;
     int eh_only;
{
  int ret = expand_decl_cleanup (decl, cleanup);
  if (cleanup && ret)
    {
      tree node = block_stack->data.block.cleanups;
      CLEANUP_EH_ONLY (node) = eh_only;
    }
  return ret;
}

/* DECL is an anonymous union.  CLEANUP is a cleanup for DECL.
   DECL_ELTS is the list of elements that belong to DECL's type.
   In each, the TREE_VALUE is a VAR_DECL, and the TREE_PURPOSE a cleanup.  */

void
expand_anon_union_decl (decl, cleanup, decl_elts)
     tree decl, cleanup, decl_elts;
{
  struct nesting *thisblock = cfun == 0 ? 0 : block_stack;
  rtx x;
  tree t;

  /* If any of the elements are addressable, so is the entire union.  */
  for (t = decl_elts; t; t = TREE_CHAIN (t))
    if (TREE_ADDRESSABLE (TREE_VALUE (t)))
      {
	TREE_ADDRESSABLE (decl) = 1;
	break;
      }

  expand_decl (decl);
  expand_decl_cleanup (decl, cleanup);
  x = DECL_RTL (decl);

  /* Go through the elements, assigning RTL to each.  */
  for (t = decl_elts; t; t = TREE_CHAIN (t))
    {
      tree decl_elt = TREE_VALUE (t);
      tree cleanup_elt = TREE_PURPOSE (t);
      enum machine_mode mode = TYPE_MODE (TREE_TYPE (decl_elt));

      /* If any of the elements are addressable, so is the entire
	 union.  */
      if (TREE_USED (decl_elt))
	TREE_USED (decl) = 1;

      /* Propagate the union's alignment to the elements.  */
      DECL_ALIGN (decl_elt) = DECL_ALIGN (decl);
      DECL_USER_ALIGN (decl_elt) = DECL_USER_ALIGN (decl);

      /* If the element has BLKmode and the union doesn't, the union is
         aligned such that the element doesn't need to have BLKmode, so
         change the element's mode to the appropriate one for its size.  */
      if (mode == BLKmode && DECL_MODE (decl) != BLKmode)
	DECL_MODE (decl_elt) = mode
	  = mode_for_size_tree (DECL_SIZE (decl_elt), MODE_INT, 1);

      /* (SUBREG (MEM ...)) at RTL generation time is invalid, so we
         instead create a new MEM rtx with the proper mode.  */
      if (GET_CODE (x) == MEM)
	{
	  if (mode == GET_MODE (x))
	    SET_DECL_RTL (decl_elt, x);
	  else
	    SET_DECL_RTL (decl_elt, adjust_address_nv (x, mode, 0));
	}
      else if (GET_CODE (x) == REG)
	{
	  if (mode == GET_MODE (x))
	    SET_DECL_RTL (decl_elt, x);
	  else
	    SET_DECL_RTL (decl_elt, gen_lowpart_SUBREG (mode, x));
	}
      else
	abort ();

      /* Record the cleanup if there is one.  */

      if (cleanup != 0)
	thisblock->data.block.cleanups
	  = tree_cons (decl_elt, cleanup_elt,
		       thisblock->data.block.cleanups);
    }
}

/* Expand a list of cleanups LIST.
   Elements may be expressions or may be nested lists.

   If DONT_DO is nonnull, then any list-element
   whose TREE_PURPOSE matches DONT_DO is omitted.
   This is sometimes used to avoid a cleanup associated with
   a value that is being returned out of the scope.

   If IN_FIXUP is nonzero, we are generating this cleanup for a fixup
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
	    if (! in_fixup && using_eh_for_cleanups_p)
	      expand_eh_region_end_cleanup (TREE_VALUE (tail));

	    if (reachable && !CLEANUP_EH_ONLY (tail))
	      {
		/* Cleanups may be run multiple times.  For example,
		   when exiting a binding contour, we expand the
		   cleanups associated with that contour.  When a goto
		   within that binding contour has a target outside that
		   contour, it will expand all cleanups from its scope to
		   the target.  Though the cleanups are expanded multiple
		   times, the control paths are non-overlapping so the
		   cleanups will not be executed twice.  */

		/* We may need to protect from outer cleanups.  */
		if (in_fixup && using_eh_for_cleanups_p)
		  {
		    expand_eh_region_start ();

		    expand_expr (TREE_VALUE (tail), const0_rtx, VOIDmode, 0);

		    expand_eh_region_end_fixup (TREE_VALUE (tail));
		  }
		else
		  expand_expr (TREE_VALUE (tail), const0_rtx, VOIDmode, 0);

		free_temp_slots ();
	      }
	  }
      }
}

/* Mark when the context we are emitting RTL for as a conditional
   context, so that any cleanup actions we register with
   expand_decl_init will be properly conditionalized when those
   cleanup actions are later performed.  Must be called before any
   expression (tree) is expanded that is within a conditional context.  */

void
start_cleanup_deferral ()
{
  /* block_stack can be NULL if we are inside the parameter list.  It is
     OK to do nothing, because cleanups aren't possible here.  */
  if (block_stack)
    ++block_stack->data.block.conditional_code;
}

/* Mark the end of a conditional region of code.  Because cleanup
   deferrals may be nested, we may still be in a conditional region
   after we end the currently deferred cleanups, only after we end all
   deferred cleanups, are we back in unconditional code.  */

void
end_cleanup_deferral ()
{
  /* block_stack can be NULL if we are inside the parameter list.  It is
     OK to do nothing, because cleanups aren't possible here.  */
  if (block_stack)
    --block_stack->data.block.conditional_code;
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

  if (cfun == NULL || cfun->stmt == NULL || block_stack == 0)
    return 0;

  if (this_contour && block_stack->data.block.cleanups != NULL)
    return 1;
  if (block_stack->data.block.cleanups == 0
      && block_stack->data.block.outer_cleanups == 0)
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
     const char *printname;
{
  struct nesting *thiscase = ALLOC_NESTING ();

  /* Make an entry on case_stack for the case we are entering.  */

  thiscase->desc = CASE_NESTING;
  thiscase->next = case_stack;
  thiscase->all = nesting_stack;
  thiscase->depth = ++nesting_depth;
  thiscase->exit_label = exit_flag ? gen_label_rtx () : 0;
  thiscase->data.case_stmt.case_list = 0;
  thiscase->data.case_stmt.index_expr = expr;
  thiscase->data.case_stmt.nominal_type = type;
  thiscase->data.case_stmt.default_label = 0;
  thiscase->data.case_stmt.printname = printname;
  thiscase->data.case_stmt.line_number_status = force_line_numbers ();
  case_stack = thiscase;
  nesting_stack = thiscase;

  do_pending_stack_adjust ();
  emit_queue ();

  /* Make sure case_stmt.start points to something that won't
     need any transformation before expand_end_case.  */
  if (GET_CODE (get_last_insn ()) != NOTE)
    emit_note (NULL, NOTE_INSN_DELETED);

  thiscase->data.case_stmt.start = get_last_insn ();

  start_cleanup_deferral ();
}

/* Start a "dummy case statement" within which case labels are invalid
   and are not connected to any larger real case statement.
   This can be used if you don't want to let a case statement jump
   into the middle of certain kinds of constructs.  */

void
expand_start_case_dummy ()
{
  struct nesting *thiscase = ALLOC_NESTING ();

  /* Make an entry on case_stack for the dummy.  */

  thiscase->desc = CASE_NESTING;
  thiscase->next = case_stack;
  thiscase->all = nesting_stack;
  thiscase->depth = ++nesting_depth;
  thiscase->exit_label = 0;
  thiscase->data.case_stmt.case_list = 0;
  thiscase->data.case_stmt.start = 0;
  thiscase->data.case_stmt.nominal_type = 0;
  thiscase->data.case_stmt.default_label = 0;
  case_stack = thiscase;
  nesting_stack = thiscase;
  start_cleanup_deferral ();
}

/* End a dummy case statement.  */

void
expand_end_case_dummy ()
{
  end_cleanup_deferral ();
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

static void
check_seenlabel ()
{
  /* If this is the first label, warn if any insns have been emitted.  */
  if (case_stack->data.case_stmt.line_number_status >= 0)
    {
      rtx insn;

      restore_line_number_status
	(case_stack->data.case_stmt.line_number_status);
      case_stack->data.case_stmt.line_number_status = -1;

      for (insn = case_stack->data.case_stmt.start;
	   insn;
	   insn = NEXT_INSN (insn))
	{
	  if (GET_CODE (insn) == CODE_LABEL)
	    break;
	  if (GET_CODE (insn) != NOTE
	      && (GET_CODE (insn) != INSN || GET_CODE (PATTERN (insn)) != USE))
	    {
	      do
		insn = PREV_INSN (insn);
	      while (insn && (GET_CODE (insn) != NOTE || NOTE_LINE_NUMBER (insn) < 0));

	      /* If insn is zero, then there must have been a syntax error.  */
	      if (insn)
		warning_with_file_and_line (NOTE_SOURCE_FILE (insn),
					    NOTE_LINE_NUMBER (insn),
					    "unreachable code at beginning of %s",
					    case_stack->data.case_stmt.printname);
	      break;
	    }
	}
    }
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
   If we are jumping into the scope of a cleanup or var-sized array, return 5.
   Return 0 on success.

   Extended to handle range statements.  */

int
pushcase (value, converter, label, duplicate)
     tree value;
     tree (*converter) PARAMS ((tree, tree));
     tree label;
     tree *duplicate;
{
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

  /* Convert VALUE to the type in which the comparisons are nominally done.  */
  if (value != 0)
    value = (*converter) (nominal_type, value);

  check_seenlabel ();

  /* Fail if this value is out of range for the actual type of the index
     (which may be narrower than NOMINAL_TYPE).  */
  if (value != 0
      && (TREE_CONSTANT_OVERFLOW (value)
	  || ! int_fits_type_p (value, index_type)))
    return 3;

  return add_case_node (value, value, label, duplicate);
}

/* Like pushcase but this case applies to all values between VALUE1 and
   VALUE2 (inclusive).  If VALUE1 is NULL, the range starts at the lowest
   value of the index type and ends at VALUE2.  If VALUE2 is NULL, the range
   starts at VALUE1 and ends at the highest value of the index type.
   If both are NULL, this case applies to all values.

   The return value is the same as that of pushcase but there is one
   additional error code: 4 means the specified range was empty.  */

int
pushcase_range (value1, value2, converter, label, duplicate)
     tree value1, value2;
     tree (*converter) PARAMS ((tree, tree));
     tree label;
     tree *duplicate;
{
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

  check_seenlabel ();

  /* Convert VALUEs to type in which the comparisons are nominally done
     and replace any unspecified value with the corresponding bound.  */
  if (value1 == 0)
    value1 = TYPE_MIN_VALUE (index_type);
  if (value2 == 0)
    value2 = TYPE_MAX_VALUE (index_type);

  /* Fail if the range is empty.  Do this before any conversion since
     we want to allow out-of-range empty ranges.  */
  if (value2 != 0 && tree_int_cst_lt (value2, value1))
    return 4;

  /* If the max was unbounded, use the max of the nominal_type we are
     converting to.  Do this after the < check above to suppress false
     positives.  */
  if (value2 == 0)
    value2 = TYPE_MAX_VALUE (nominal_type);

  value1 = (*converter) (nominal_type, value1);
  value2 = (*converter) (nominal_type, value2);

  /* Fail if these values are out of range.  */
  if (TREE_CONSTANT_OVERFLOW (value1)
      || ! int_fits_type_p (value1, index_type))
    return 3;

  if (TREE_CONSTANT_OVERFLOW (value2)
      || ! int_fits_type_p (value2, index_type))
    return 3;

  return add_case_node (value1, value2, label, duplicate);
}

/* Do the actual insertion of a case label for pushcase and pushcase_range
   into case_stack->data.case_stmt.case_list.  Use an AVL tree to avoid
   slowdown for large switch statements.  */

int
add_case_node (low, high, label, duplicate)
     tree low, high;
     tree label;
     tree *duplicate;
{
  struct case_node *p, **q, *r;

  /* If there's no HIGH value, then this is not a case range; it's
     just a simple case label.  But that's just a degenerate case
     range.  */
  if (!high)
    high = low;

  /* Handle default labels specially.  */
  if (!high && !low)
    {
      if (case_stack->data.case_stmt.default_label != 0)
	{
	  *duplicate = case_stack->data.case_stmt.default_label;
	  return 2;
	}
      case_stack->data.case_stmt.default_label = label;
      expand_label (label);
      return 0;
    }

  q = &case_stack->data.case_stmt.case_list;
  p = *q;

  while ((r = *q))
    {
      p = r;

      /* Keep going past elements distinctly greater than HIGH.  */
      if (tree_int_cst_lt (high, p->low))
	q = &p->left;

      /* or distinctly less than LOW.  */
      else if (tree_int_cst_lt (p->high, low))
	q = &p->right;

      else
	{
	  /* We have an overlap; this is an error.  */
	  *duplicate = p->code_label;
	  return 2;
	}
    }

  /* Add this label to the chain, and succeed.  */

  r = (struct case_node *) ggc_alloc (sizeof (struct case_node));
  r->low = low;

  /* If the bounds are equal, turn this into the one-value case.  */
  if (tree_int_cst_equal (low, high))
    r->high = r->low;
  else
    r->high = high;

  r->code_label = label;
  expand_label (label);

  *q = r;
  r->parent = p;
  r->left = 0;
  r->right = 0;
  r->balance = 0;

  while (p)
    {
      struct case_node *s;

      if (r == p->left)
	{
	  int b;

	  if (! (b = p->balance))
	    /* Growth propagation from left side.  */
	    p->balance = -1;
	  else if (b < 0)
	    {
	      if (r->balance < 0)
		{
		  /* R-Rotation */
		  if ((p->left = s = r->right))
		    s->parent = p;

		  r->right = p;
		  p->balance = 0;
		  r->balance = 0;
		  s = p->parent;
		  p->parent = r;

		  if ((r->parent = s))
		    {
		      if (s->left == p)
			s->left = r;
		      else
			s->right = r;
		    }
		  else
		    case_stack->data.case_stmt.case_list = r;
		}
	      else
		/* r->balance == +1 */
		{
		  /* LR-Rotation */

		  int b2;
		  struct case_node *t = r->right;

		  if ((p->left = s = t->right))
		    s->parent = p;

		  t->right = p;
		  if ((r->right = s = t->left))
		    s->parent = r;

		  t->left = r;
		  b = t->balance;
		  b2 = b < 0;
		  p->balance = b2;
		  b2 = -b2 - b;
		  r->balance = b2;
		  t->balance = 0;
		  s = p->parent;
		  p->parent = t;
		  r->parent = t;

		  if ((t->parent = s))
		    {
		      if (s->left == p)
			s->left = t;
		      else
			s->right = t;
		    }
		  else
		    case_stack->data.case_stmt.case_list = t;
		}
	      break;
	    }

	  else
	    {
	      /* p->balance == +1; growth of left side balances the node.  */
	      p->balance = 0;
	      break;
	    }
	}
      else
	/* r == p->right */
	{
	  int b;

	  if (! (b = p->balance))
	    /* Growth propagation from right side.  */
	    p->balance++;
	  else if (b > 0)
	    {
	      if (r->balance > 0)
		{
		  /* L-Rotation */

		  if ((p->right = s = r->left))
		    s->parent = p;

		  r->left = p;
		  p->balance = 0;
		  r->balance = 0;
		  s = p->parent;
		  p->parent = r;
		  if ((r->parent = s))
		    {
		      if (s->left == p)
			s->left = r;
		      else
			s->right = r;
		    }

		  else
		    case_stack->data.case_stmt.case_list = r;
		}

	      else
		/* r->balance == -1 */
		{
		  /* RL-Rotation */
		  int b2;
		  struct case_node *t = r->left;

		  if ((p->right = s = t->left))
		    s->parent = p;

		  t->left = p;

		  if ((r->left = s = t->right))
		    s->parent = r;

		  t->right = r;
		  b = t->balance;
		  b2 = b < 0;
		  r->balance = b2;
		  b2 = -b2 - b;
		  p->balance = b2;
		  t->balance = 0;
		  s = p->parent;
		  p->parent = t;
		  r->parent = t;

		  if ((t->parent = s))
		    {
		      if (s->left == p)
			s->left = t;
		      else
			s->right = t;
		    }

		  else
		    case_stack->data.case_stmt.case_list = t;
		}
	      break;
	    }
	  else
	    {
	      /* p->balance == -1; growth of right side balances the node.  */
	      p->balance = 0;
	      break;
	    }
	}

      r = p;
      p = p->parent;
    }

  return 0;
}

/* Returns the number of possible values of TYPE.
   Returns -1 if the number is unknown, variable, or if the number does not
   fit in a HOST_WIDE_INT.
   Sets *SPARSENESS to 2 if TYPE is an ENUMERAL_TYPE whose values
   do not increase monotonically (there may be duplicates);
   to 1 if the values increase monotonically, but not always by 1;
   otherwise sets it to 0.  */

HOST_WIDE_INT
all_cases_count (type, sparseness)
     tree type;
     int *sparseness;
{
  tree t;
  HOST_WIDE_INT count, minval, lastval;

  *sparseness = 0;

  switch (TREE_CODE (type))
    {
    case BOOLEAN_TYPE:
      count = 2;
      break;

    case CHAR_TYPE:
      count = 1 << BITS_PER_UNIT;
      break;

    default:
    case INTEGER_TYPE:
      if (TYPE_MAX_VALUE (type) != 0
	  && 0 != (t = fold (build (MINUS_EXPR, type, TYPE_MAX_VALUE (type),
				    TYPE_MIN_VALUE (type))))
	  && 0 != (t = fold (build (PLUS_EXPR, type, t,
				    convert (type, integer_zero_node))))
	  && host_integerp (t, 1))
	count = tree_low_cst (t, 1);
      else
	return -1;
      break;

    case ENUMERAL_TYPE:
      /* Don't waste time with enumeral types with huge values.  */
      if (! host_integerp (TYPE_MIN_VALUE (type), 0)
	  || TYPE_MAX_VALUE (type) == 0
	  || ! host_integerp (TYPE_MAX_VALUE (type), 0))
	return -1;

      lastval = minval = tree_low_cst (TYPE_MIN_VALUE (type), 0);
      count = 0;

      for (t = TYPE_VALUES (type); t != NULL_TREE; t = TREE_CHAIN (t))
	{
	  HOST_WIDE_INT thisval = tree_low_cst (TREE_VALUE (t), 0);

	  if (*sparseness == 2 || thisval <= lastval)
	    *sparseness = 2;
	  else if (thisval != minval + count)
	    *sparseness = 1;

	  lastval = thisval;
	  count++;
	}
    }

  return count;
}

#define BITARRAY_TEST(ARRAY, INDEX) \
  ((ARRAY)[(unsigned) (INDEX) / HOST_BITS_PER_CHAR]\
			  & (1 << ((unsigned) (INDEX) % HOST_BITS_PER_CHAR)))
#define BITARRAY_SET(ARRAY, INDEX) \
  ((ARRAY)[(unsigned) (INDEX) / HOST_BITS_PER_CHAR]\
			  |= 1 << ((unsigned) (INDEX) % HOST_BITS_PER_CHAR))

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
     HOST_WIDE_INT count;
     int sparseness;
{
  tree next_node_to_try = NULL_TREE;
  HOST_WIDE_INT next_node_offset = 0;

  struct case_node *n, *root = case_stack->data.case_stmt.case_list;
  tree val = make_node (INTEGER_CST);

  TREE_TYPE (val) = type;
  if (! root)
    /* Do nothing.  */
    ;
  else if (sparseness == 2)
    {
      tree t;
      unsigned HOST_WIDE_INT xlo;

      /* This less efficient loop is only needed to handle
	 duplicate case values (multiple enum constants
	 with the same value).  */
      TREE_TYPE (val) = TREE_TYPE (root->low);
      for (t = TYPE_VALUES (type), xlo = 0; t != NULL_TREE;
	   t = TREE_CHAIN (t), xlo++)
	{
	  TREE_INT_CST_LOW (val) = TREE_INT_CST_LOW (TREE_VALUE (t));
	  TREE_INT_CST_HIGH (val) = TREE_INT_CST_HIGH (TREE_VALUE (t));
	  n = root;
	  do
	    {
	      /* Keep going past elements distinctly greater than VAL.  */
	      if (tree_int_cst_lt (val, n->low))
		n = n->left;

	      /* or distinctly less than VAL.  */
	      else if (tree_int_cst_lt (n->high, val))
		n = n->right;

	      else
		{
		  /* We have found a matching range.  */
		  BITARRAY_SET (cases_seen, xlo);
		  break;
		}
	    }
	  while (n);
	}
    }
  else
    {
      if (root->left)
	case_stack->data.case_stmt.case_list = root = case_tree2list (root, 0);

      for (n = root; n; n = n->right)
	{
	  TREE_INT_CST_LOW (val) = TREE_INT_CST_LOW (n->low);
	  TREE_INT_CST_HIGH (val) = TREE_INT_CST_HIGH (n->low);
	  while (! tree_int_cst_lt (n->high, val))
	    {
	      /* Calculate (into xlo) the "offset" of the integer (val).
		 The element with lowest value has offset 0, the next smallest
		 element has offset 1, etc.  */

	      unsigned HOST_WIDE_INT xlo;
	      HOST_WIDE_INT xhi;
	      tree t;

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
			{
			  xlo = -1;
			  break;
			}
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

	      if (xhi == 0 && xlo < (unsigned HOST_WIDE_INT) count)
		BITARRAY_SET (cases_seen, xlo);

	      add_double (TREE_INT_CST_LOW (val), TREE_INT_CST_HIGH (val),
			  1, 0,
			  &TREE_INT_CST_LOW (val), &TREE_INT_CST_HIGH (val));
	    }
	}
    }
}

/* Given a switch statement with an expression that is an enumeration
   type, warn if any of the enumeration type's literals are not
   covered by the case expressions of the switch.  Also, warn if there
   are any extra switch cases that are *not* elements of the
   enumerated type.

   Historical note:

   At one stage this function would: ``If all enumeration literals
   were covered by the case expressions, turn one of the expressions
   into the default expression since it should not be possible to fall
   through such a switch.''

   That code has since been removed as: ``This optimization is
   disabled because it causes valid programs to fail.  ANSI C does not
   guarantee that an expression with enum type will have a value that
   is the same as one of the enumeration literals.''  */

void
check_for_full_enumeration_handling (type)
     tree type;
{
  struct case_node *n;
  tree chain;

  /* True iff the selector type is a numbered set mode.  */
  int sparseness = 0;

  /* The number of possible selector values.  */
  HOST_WIDE_INT size;

  /* For each possible selector value. a one iff it has been matched
     by a case value alternative.  */
  unsigned char *cases_seen;

  /* The allocated size of cases_seen, in chars.  */
  HOST_WIDE_INT bytes_needed;

  size = all_cases_count (type, &sparseness);
  bytes_needed = (size + HOST_BITS_PER_CHAR) / HOST_BITS_PER_CHAR;

  if (size > 0 && size < 600000
      /* We deliberately use calloc here, not cmalloc, so that we can suppress
	 this optimization if we don't have enough memory rather than
	 aborting, as xmalloc would do.  */
      && (cases_seen =
	  (unsigned char *) really_call_calloc (bytes_needed, 1)) != NULL)
    {
      HOST_WIDE_INT i;
      tree v = TYPE_VALUES (type);

      /* The time complexity of this code is normally O(N), where
	 N being the number of members in the enumerated type.
	 However, if type is an ENUMERAL_TYPE whose values do not
	 increase monotonically, O(N*log(N)) time may be needed.  */

      mark_seen_cases (type, cases_seen, size, sparseness);

      for (i = 0; v != NULL_TREE && i < size; i++, v = TREE_CHAIN (v))
	if (BITARRAY_TEST (cases_seen, i) == 0)
	  warning ("enumeration value `%s' not handled in switch",
		   IDENTIFIER_POINTER (TREE_PURPOSE (v)));

      free (cases_seen);
    }

  /* Now we go the other way around; we warn if there are case
     expressions that don't correspond to enumerators.  This can
     occur since C and C++ don't enforce type-checking of
     assignments to enumeration variables.  */

  if (case_stack->data.case_stmt.case_list
      && case_stack->data.case_stmt.case_list->left)
    case_stack->data.case_stmt.case_list
      = case_tree2list (case_stack->data.case_stmt.case_list, 0);
  for (n = case_stack->data.case_stmt.case_list; n; n = n->right)
    {
      for (chain = TYPE_VALUES (type);
	   chain && !tree_int_cst_equal (n->low, TREE_VALUE (chain));
	   chain = TREE_CHAIN (chain))
	;

      if (!chain)
	{
	  if (TYPE_NAME (type) == 0)
	    warning ("case value `%ld' not in enumerated type",
		     (long) TREE_INT_CST_LOW (n->low));
	  else
	    warning ("case value `%ld' not in enumerated type `%s'",
		     (long) TREE_INT_CST_LOW (n->low),
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
		warning ("case value `%ld' not in enumerated type",
			 (long) TREE_INT_CST_LOW (n->high));
	      else
		warning ("case value `%ld' not in enumerated type `%s'",
			 (long) TREE_INT_CST_LOW (n->high),
			 IDENTIFIER_POINTER ((TREE_CODE (TYPE_NAME (type))
					      == IDENTIFIER_NODE)
					     ? TYPE_NAME (type)
					     : DECL_NAME (TYPE_NAME (type))));
	    }
	}
    }
}



/* Terminate a case (Pascal) or switch (C) statement
   in which ORIG_INDEX is the expression to be tested.
   If ORIG_TYPE is not NULL, it is the original ORIG_INDEX
   type as given in the source before any compiler conversions.
   Generate the code to test it and jump to the right place.  */

void
expand_end_case_type (orig_index, orig_type)
     tree orig_index, orig_type;
{
  tree minval = NULL_TREE, maxval = NULL_TREE, range = NULL_TREE;
  rtx default_label = 0;
  struct case_node *n;
  unsigned int count;
  rtx index;
  rtx table_label;
  int ncases;
  rtx *labelvec;
  int i;
  rtx before_case, end;
  struct nesting *thiscase = case_stack;
  tree index_expr, index_type;
  int unsignedp;

  /* Don't crash due to previous errors.  */
  if (thiscase == NULL)
    return;

  table_label = gen_label_rtx ();
  index_expr = thiscase->data.case_stmt.index_expr;
  index_type = TREE_TYPE (index_expr);
  unsignedp = TREE_UNSIGNED (index_type);
  if (orig_type == NULL)
    orig_type = TREE_TYPE (orig_index);

  do_pending_stack_adjust ();

  /* This might get a spurious warning in the presence of a syntax error;
     it could be fixed by moving the call to check_seenlabel after the
     check for error_mark_node, and copying the code of check_seenlabel that
     deals with case_stack->data.case_stmt.line_number_status /
     restore_line_number_status in front of the call to end_cleanup_deferral;
     However, this might miss some useful warnings in the presence of
     non-syntax errors.  */
  check_seenlabel ();

  /* An ERROR_MARK occurs for various reasons including invalid data type.  */
  if (index_type != error_mark_node)
    {
      /* If the switch expression was an enumerated type, check that
	 exactly all enumeration literals are covered by the cases.
	 The check is made when -Wswitch was specified and there is no
	 default case, or when -Wswitch-enum was specified.  */
      if (((warn_switch && !thiscase->data.case_stmt.default_label)
	   || warn_switch_enum)
	  && TREE_CODE (orig_type) == ENUMERAL_TYPE
	  && TREE_CODE (index_expr) != INTEGER_CST)
	check_for_full_enumeration_handling (orig_type);

      if (warn_switch_default && !thiscase->data.case_stmt.default_label)
	warning ("switch missing default case");

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

      if (thiscase->data.case_stmt.case_list
	  && thiscase->data.case_stmt.case_list->left)
	thiscase->data.case_stmt.case_list
	  = case_tree2list (thiscase->data.case_stmt.case_list, 0);

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

      /* Compute span of values.  */
      if (count != 0)
	range = fold (build (MINUS_EXPR, index_type, maxval, minval));

      end_cleanup_deferral ();

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

      else if (count < case_values_threshold ()
	       || compare_tree_int (range, 10 * count) > 0
	       /* RANGE may be signed, and really large ranges will show up
		  as negative numbers.  */
	       || compare_tree_int (range, 0) < 0
#ifndef ASM_OUTPUT_ADDR_DIFF_ELT
	       || flag_pic
#endif
	       || TREE_CODE (index_expr) == INTEGER_CST
	       || (TREE_CODE (index_expr) == COMPOUND_EXPR
		   && TREE_CODE (TREE_OPERAND (index_expr, 1)) == INTEGER_CST))
	{
	  index = expand_expr (index_expr, NULL_RTX, VOIDmode, 0);

	  /* If the index is a short or char that we do not have
	     an insn to handle comparisons directly, convert it to
	     a full integer now, rather than letting each comparison
	     generate the conversion.  */

	  if (GET_MODE_CLASS (GET_MODE (index)) == MODE_INT
	      && ! have_insn_for (COMPARE, GET_MODE (index)))
	    {
	      enum machine_mode wider_mode;
	      for (wider_mode = GET_MODE (index); wider_mode != VOIDmode;
		   wider_mode = GET_MODE_WIDER_MODE (wider_mode))
		if (have_insn_for (COMPARE, wider_mode))
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
		 issue an unconditional branch to the appropriate
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
		= (TREE_CODE (orig_type) != ENUMERAL_TYPE
		   && estimate_case_costs (thiscase->data.case_stmt.case_list));
	      balance_case_nodes (&thiscase->data.case_stmt.case_list, NULL);
	      emit_case_nodes (index, thiscase->data.case_stmt.case_list,
			       default_label, index_type);
	      emit_jump_if_reachable (default_label);
	    }
	}
      else
	{
	  if (! try_casesi (index_type, index_expr, minval, range,
			    table_label, default_label))
	    {
	      index_type = thiscase->data.case_stmt.nominal_type;

	      /* Index jumptables from zero for suitable values of
                 minval to avoid a subtraction.  */
	      if (! optimize_size
		  && compare_tree_int (minval, 0) > 0
		  && compare_tree_int (minval, 3) < 0)
		{
		  minval = integer_zero_node;
		  range = maxval;
		}

	      if (! try_tablejump (index_type, index_expr, minval, range,
				   table_label, default_label))
		abort ();
	    }

	  /* Get table of labels to jump to, in order of case index.  */

	  ncases = tree_low_cst (range, 0) + 1;
	  labelvec = (rtx *) alloca (ncases * sizeof (rtx));
	  memset ((char *) labelvec, 0, ncases * sizeof (rtx));

	  for (n = thiscase->data.case_stmt.case_list; n; n = n->right)
	    {
	      /* Compute the low and high bounds relative to the minimum
		 value since that should fit in a HOST_WIDE_INT while the
		 actual values may not.  */
	      HOST_WIDE_INT i_low
		= tree_low_cst (fold (build (MINUS_EXPR, index_type,
					     n->low, minval)), 1);
	      HOST_WIDE_INT i_high
		= tree_low_cst (fold (build (MINUS_EXPR, index_type,
					     n->high, minval)), 1);
	      HOST_WIDE_INT i;

	      for (i = i_low; i <= i_high; i ++)
		labelvec[i]
		  = gen_rtx_LABEL_REF (Pmode, label_rtx (n->code_label));
	    }

	  /* Fill in the gaps with the default.  */
	  for (i = 0; i < ncases; i++)
	    if (labelvec[i] == 0)
	      labelvec[i] = gen_rtx_LABEL_REF (Pmode, default_label);

	  /* Output the table */
	  emit_label (table_label);

	  if (CASE_VECTOR_PC_RELATIVE || flag_pic)
	    emit_jump_insn (gen_rtx_ADDR_DIFF_VEC (CASE_VECTOR_MODE,
						   gen_rtx_LABEL_REF (Pmode, table_label),
						   gen_rtvec_v (ncases, labelvec),
						   const0_rtx, const0_rtx));
	  else
	    emit_jump_insn (gen_rtx_ADDR_VEC (CASE_VECTOR_MODE,
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

      before_case = NEXT_INSN (before_case);
      end = get_last_insn ();
      if (squeeze_notes (&before_case, &end))
	abort ();
      reorder_insns (before_case, end,
		     thiscase->data.case_stmt.start);
    }
  else
    end_cleanup_deferral ();

  if (thiscase->exit_label)
    emit_label (thiscase->exit_label);

  POPSTACK (case_stack);

  free_temp_slots ();
}

/* Convert the tree NODE into a list linked by the right field, with the left
   field zeroed.  RIGHT is used for recursion; it is a list to be placed
   rightmost in the resulting list.  */

static struct case_node *
case_tree2list (node, right)
     struct case_node *node, *right;
{
  struct case_node *left;

  if (node->right)
    right = case_tree2list (node->right, right);

  node->right = right;
  if ((left = node->left))
    {
      node->left = 0;
      return case_tree2list (left, node);
    }

  return node;
}

/* Generate code to jump to LABEL if OP1 and OP2 are equal.  */

static void
do_jump_if_equal (op1, op2, label, unsignedp)
     rtx op1, op2, label;
     int unsignedp;
{
  if (GET_CODE (op1) == CONST_INT && GET_CODE (op2) == CONST_INT)
    {
      if (INTVAL (op1) == INTVAL (op2))
	emit_jump (label);
    }
  else
    emit_cmp_and_jump_insns (op1, op2, EQ, NULL_RTX,
			     (GET_MODE (op1) == VOIDmode
			     ? GET_MODE (op2) : GET_MODE (op1)),
			     unsignedp, label);
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
  tree min_ascii = integer_minus_one_node;
  tree max_ascii = convert (TREE_TYPE (node->high), build_int_2 (127, 0));
  case_node_ptr n;
  int i;

  /* If we haven't already made the cost table, make it now.  Note that the
     lower bound of the table is -1, not zero.  */

  if (! cost_table_initialized)
    {
      cost_table_initialized = 1;

      for (i = 0; i < 128; i++)
	{
	  if (ISALNUM (i))
	    COST_TABLE (i) = 16;
	  else if (ISPUNCT (i))
	    COST_TABLE (i) = 8;
	  else if (ISCNTRL (i))
	    COST_TABLE (i) = -1;
	}

      COST_TABLE (' ') = 8;
      COST_TABLE ('\t') = 4;
      COST_TABLE ('\0') = 4;
      COST_TABLE ('\n') = 2;
      COST_TABLE ('\f') = 1;
      COST_TABLE ('\v') = 1;
      COST_TABLE ('\b') = 1;
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

      for (i = (HOST_WIDE_INT) TREE_INT_CST_LOW (n->low);
	   i <= (HOST_WIDE_INT) TREE_INT_CST_LOW (n->high); i++)
	if (COST_TABLE (i) < 0)
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
      rtx lb2;
      case_node_ptr np = node;

      /* Try to group the successors of NODE with NODE.  */
      while (((np = np->right) != 0)
	     /* Do they jump to the same place?  */
	     && ((lb2 = next_real_insn (label_rtx (np->code_label))) == lb
		 || (lb != 0 && lb2 != 0
		     && simplejump_p (lb)
		     && simplejump_p (lb2)
		     && rtx_equal_p (SET_SRC (PATTERN (lb)),
				     SET_SRC (PATTERN (lb2)))))
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
   branch is then transformed recursively.  */

static void
balance_case_nodes (head, parent)
     case_node_ptr *head;
     case_node_ptr parent;
{
  case_node_ptr np;

  np = *head;
  if (np)
    {
      int cost = 0;
      int i = 0;
      int ranges = 0;
      case_node_ptr *npp;
      case_node_ptr left;

      /* Count the number of entries on branch.  Also count the ranges.  */

      while (np)
	{
	  if (!tree_int_cst_equal (np->low, np->high))
	    {
	      ranges++;
	      if (use_cost_table)
		cost += COST_TABLE (TREE_INT_CST_LOW (np->high));
	    }

	  if (use_cost_table)
	    cost += COST_TABLE (TREE_INT_CST_LOW (np->low));

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
		    i -= COST_TABLE (TREE_INT_CST_LOW ((*npp)->high));
		  i -= COST_TABLE (TREE_INT_CST_LOW ((*npp)->low));
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

  /* If there is no upper bound, obviously no test is needed.  */

  if (TYPE_MAX_VALUE (index_type) == NULL)
    return 1;

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
   code for out of bound conditions on the current node.

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
  enum machine_mode mode = GET_MODE (index);
  enum machine_mode imode = TYPE_MODE (index_type);

  /* See if our parents have already tested everything for us.
     If they have, emit an unconditional jump for this node.  */
  if (node_is_bounded (node, index_type))
    emit_jump (label_rtx (node->code_label));

  else if (tree_int_cst_equal (node->low, node->high))
    {
      /* Node is single valued.  First see if the index expression matches
	 this node and then check our children, if any.  */

      do_jump_if_equal (index,
			convert_modes (mode, imode,
				       expand_expr (node->low, NULL_RTX,
						    VOIDmode, 0),
				       unsignedp),
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
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_expr (node->high, NULL_RTX,
						     VOIDmode, 0),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       label_rtx (node->right->code_label));
	      emit_case_nodes (index, node->left, default_label, index_type);
	    }

	  else if (node_is_bounded (node->left, index_type))
	    {
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_expr (node->high, NULL_RTX,
						     VOIDmode, 0),
					unsignedp),
				       LT, NULL_RTX, mode, unsignedp,
				       label_rtx (node->left->code_label));
	      emit_case_nodes (index, node->right, default_label, index_type);
	    }

	  else
	    {
	      /* Neither node is bounded.  First distinguish the two sides;
		 then emit the code for one side at a time.  */

	      tree test_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

	      /* See if the value is on the right.  */
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_expr (node->high, NULL_RTX,
						     VOIDmode, 0),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       label_rtx (test_label));

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
		  emit_cmp_and_jump_insns (index,
					   convert_modes
					   (mode, imode,
					    expand_expr (node->high, NULL_RTX,
							 VOIDmode, 0),
					    unsignedp),
					   LT, NULL_RTX, mode, unsignedp,
					   default_label);
		}

	      emit_case_nodes (index, node->right, default_label, index_type);
	    }
	  else
	    /* We cannot process node->right normally
	       since we haven't ruled out the numbers less than
	       this node's value.  So handle node->right explicitly.  */
	    do_jump_if_equal (index,
			      convert_modes
			      (mode, imode,
			       expand_expr (node->right->low, NULL_RTX,
					    VOIDmode, 0),
			       unsignedp),
			      label_rtx (node->right->code_label), unsignedp);
	}

      else if (node->right == 0 && node->left != 0)
	{
	  /* Just one subtree, on the left.  */
	  if (node->left->left || node->left->right
	      || !tree_int_cst_equal (node->left->low, node->left->high))
	    {
	      if (!node_has_high_bound (node, index_type))
		{
		  emit_cmp_and_jump_insns (index,
					   convert_modes
					   (mode, imode,
					    expand_expr (node->high, NULL_RTX,
							 VOIDmode, 0),
					    unsignedp),
					   GT, NULL_RTX, mode, unsignedp,
					   default_label);
		}

	      emit_case_nodes (index, node->left, default_label, index_type);
	    }
	  else
	    /* We cannot process node->left normally
	       since we haven't ruled out the numbers less than
	       this node's value.  So handle node->left explicitly.  */
	    do_jump_if_equal (index,
			      convert_modes
			      (mode, imode,
			       expand_expr (node->left->low, NULL_RTX,
					    VOIDmode, 0),
			       unsignedp),
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

	  if (node_is_bounded (node->right, index_type))
	    /* Right hand node is fully bounded so we can eliminate any
	       testing and branch directly to the target code.  */
	    emit_cmp_and_jump_insns (index,
				     convert_modes
				     (mode, imode,
				      expand_expr (node->high, NULL_RTX,
						   VOIDmode, 0),
				      unsignedp),
				     GT, NULL_RTX, mode, unsignedp,
				     label_rtx (node->right->code_label));
	  else
	    {
	      /* Right hand node requires testing.
		 Branch to a label where we will handle it later.  */

	      test_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_expr (node->high, NULL_RTX,
						     VOIDmode, 0),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       label_rtx (test_label));
	    }

	  /* Value belongs to this node or to the left-hand subtree.  */

	  emit_cmp_and_jump_insns (index,
				   convert_modes
				   (mode, imode,
				    expand_expr (node->low, NULL_RTX,
						 VOIDmode, 0),
				    unsignedp),
				   GE, NULL_RTX, mode, unsignedp,
				   label_rtx (node->code_label));

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
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_expr (node->low, NULL_RTX,
						     VOIDmode, 0),
					unsignedp),
				       LT, NULL_RTX, mode, unsignedp,
				       default_label);
	    }

	  /* Value belongs to this node or to the right-hand subtree.  */

	  emit_cmp_and_jump_insns (index,
				   convert_modes
				   (mode, imode,
				    expand_expr (node->high, NULL_RTX,
						 VOIDmode, 0),
				    unsignedp),
				   LE, NULL_RTX, mode, unsignedp,
				   label_rtx (node->code_label));

	  emit_case_nodes (index, node->right, default_label, index_type);
	}

      else if (node->right == 0 && node->left != 0)
	{
	  /* Deal with values to the right of this node,
	     if they are possible.  */
	  if (!node_has_high_bound (node, index_type))
	    {
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_expr (node->high, NULL_RTX,
						     VOIDmode, 0),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       default_label);
	    }

	  /* Value belongs to this node or to the left-hand subtree.  */

	  emit_cmp_and_jump_insns (index,
				   convert_modes
				   (mode, imode,
				    expand_expr (node->low, NULL_RTX,
						 VOIDmode, 0),
				    unsignedp),
				   GE, NULL_RTX, mode, unsignedp,
				   label_rtx (node->code_label));

	  emit_case_nodes (index, node->left, default_label, index_type);
	}

      else
	{
	  /* Node has no children so we check low and high bounds to remove
	     redundant tests.  Only one of the bounds can exist,
	     since otherwise this node is bounded--a case tested already.  */
	  int high_bound = node_has_high_bound (node, index_type);
	  int low_bound = node_has_low_bound (node, index_type);

	  if (!high_bound && low_bound)
	    {
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_expr (node->high, NULL_RTX,
						     VOIDmode, 0),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       default_label);
	    }

	  else if (!low_bound && high_bound)
	    {
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_expr (node->low, NULL_RTX,
						     VOIDmode, 0),
					unsignedp),
				       LT, NULL_RTX, mode, unsignedp,
				       default_label);
	    }
	  else if (!low_bound && !high_bound)
	    {
	      /* Widen LOW and HIGH to the same width as INDEX.  */
	      tree type = (*lang_hooks.types.type_for_mode) (mode, unsignedp);
	      tree low = build1 (CONVERT_EXPR, type, node->low);
	      tree high = build1 (CONVERT_EXPR, type, node->high);
	      rtx low_rtx, new_index, new_bound;

	      /* Instead of doing two branches, emit one unsigned branch for
		 (index-low) > (high-low).  */
	      low_rtx = expand_expr (low, NULL_RTX, mode, 0);
	      new_index = expand_simple_binop (mode, MINUS, index, low_rtx,
					       NULL_RTX, unsignedp,
					       OPTAB_WIDEN);
	      new_bound = expand_expr (fold (build (MINUS_EXPR, type,
						    high, low)),
				       NULL_RTX, mode, 0);

	      emit_cmp_and_jump_insns (new_index, new_bound, GT, NULL_RTX,
				       mode, 1, default_label);
	    }

	  emit_jump (label_rtx (node->code_label));
	}
    }
}

#include "gt-stmt.h"
