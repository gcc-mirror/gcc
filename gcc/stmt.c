/* Expands front end tree to back end RTL for GNU C-Compiler
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000 Free Software Foundation, Inc.

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
#include "system.h"

#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "flags.h"
#include "except.h"
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
#include "toplev.h"
#include "output.h"
#include "ggc.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free
struct obstack stmt_obstack;

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

struct case_node
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
	     This may be the end of the if or the next else/elseif.  */
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
	  /* Nonzero if this is associated with a EH region.  */
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
	  /* When in a conditional context, this is the specific
	     cleanup list associated with last_unconditional_cleanup,
	     where we place the conditionalized cleanups.  */
	  tree *cleanup_ptr;
	} block;
      /* For switch (C) or case (Pascal) statements,
	 and also for dummies (see `expand_start_case_dummy').  */
      struct
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
	  /* Number of range exprs in case statement.  */
	  int num_ranges;
	  /* Name of this kind of statement, for warnings.  */
	  const char *printname;
	  /* Used to save no_line_numbers till we see the first case label.
	     We set this to -1 when we see the first case label in this
	     case statement.  */
	  int line_number_status;
	} case_stmt;
    } data;
};

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
     The tail of this list can be 0,
     if all remaining elements would be empty.
     The TREE_VALUE contains the cleanup list of that block as of the
     time this goto was seen.
     The TREE_ADDRESSABLE flag is 1 for a block that has been exited.  */
  tree cleanup_list_list;
};

/* Within any binding contour that must restore a stack level,
   all labels are recorded with a chain of these structures.  */

struct label_chain
{
  /* Points to following fixup.  */
  struct label_chain *next;
  tree label;
};

struct stmt_status
{
  /* Chain of all pending binding contours.  */
  struct nesting *x_block_stack;

  /* If any new stacks are added here, add them to POPSTACKS too.  */

  /* Chain of all pending binding contours that restore stack levels
     or have cleanups.  */
  struct nesting *x_stack_block_stack;

  /* Chain of all pending conditional statements.  */
  struct nesting *x_cond_stack;

  /* Chain of all pending loops.  */
  struct nesting *x_loop_stack;

  /* Chain of all pending case or switch statements.  */
  struct nesting *x_case_stack;

  /* Separate chain including all of the above,
     chained through the `all' field.  */
  struct nesting *x_nesting_stack;

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

/* Non-zero if we are using EH to handle cleanus.  */
static int using_eh_for_cleanups_p = 0;

/* Character strings, each containing a single decimal digit.  */
static char *digit_strings[10];


static int n_occurrences		PARAMS ((int, const char *));
static void expand_goto_internal	PARAMS ((tree, rtx, rtx));
static int expand_fixup			PARAMS ((tree, rtx, rtx));
static rtx expand_nl_handler_label	PARAMS ((rtx, rtx));
static void expand_nl_goto_receiver	PARAMS ((void));
static void expand_nl_goto_receivers	PARAMS ((struct nesting *));
static void fixup_gotos			PARAMS ((struct nesting *, rtx, tree,
					       rtx, int));
static void expand_null_return_1	PARAMS ((rtx, int));
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
static int add_case_node		PARAMS ((tree, tree, tree, tree *));
static struct case_node *case_tree2list	PARAMS ((case_node *, case_node *));
static void mark_cond_nesting           PARAMS ((struct nesting *));
static void mark_loop_nesting           PARAMS ((struct nesting *));
static void mark_block_nesting          PARAMS ((struct nesting *));
static void mark_case_nesting           PARAMS ((struct nesting *));
static void mark_goto_fixup             PARAMS ((struct goto_fixup *));


void
using_eh_for_cleanups ()
{
  using_eh_for_cleanups_p = 1;
}

/* Mark N (known to be a cond-nesting) for GC.  */

static void
mark_cond_nesting (n)
     struct nesting *n;
{
  while (n)
    {
      ggc_mark_rtx (n->exit_label);
      ggc_mark_rtx (n->data.cond.endif_label);
      ggc_mark_rtx (n->data.cond.next_label);

      n = n->next;
    }
}

/* Mark N (known to be a loop-nesting) for GC.  */

static void
mark_loop_nesting (n)
     struct nesting *n;
{

  while (n)
    {
      ggc_mark_rtx (n->exit_label);
      ggc_mark_rtx (n->data.loop.start_label);
      ggc_mark_rtx (n->data.loop.end_label);
      ggc_mark_rtx (n->data.loop.alt_end_label);
      ggc_mark_rtx (n->data.loop.continue_label);

      n = n->next;
    }
}

/* Mark N (known to be a block-nesting) for GC.  */

static void
mark_block_nesting (n)
     struct nesting *n;
{
  while (n)
    {
      struct label_chain *l;

      ggc_mark_rtx (n->exit_label);
      ggc_mark_rtx (n->data.block.stack_level);
      ggc_mark_rtx (n->data.block.first_insn);
      ggc_mark_tree (n->data.block.cleanups);
      ggc_mark_tree (n->data.block.outer_cleanups);

      for (l = n->data.block.label_chain; l != NULL; l = l->next)
	ggc_mark_tree (l->label);

      ggc_mark_rtx (n->data.block.last_unconditional_cleanup);

      /* ??? cleanup_ptr never points outside the stack, does it?  */

      n = n->next;
    }
}

/* Mark N (known to be a case-nesting) for GC.  */

static void
mark_case_nesting (n)
     struct nesting *n;
{
  while (n)
    {
      struct case_node *node;

      ggc_mark_rtx (n->exit_label);
      ggc_mark_rtx (n->data.case_stmt.start);

      node = n->data.case_stmt.case_list;
      while (node)
	{
	  ggc_mark_tree (node->low);
	  ggc_mark_tree (node->high);
	  ggc_mark_tree (node->code_label);
	  node = node->right;
	}

      ggc_mark_tree (n->data.case_stmt.default_label);
      ggc_mark_tree (n->data.case_stmt.index_expr);
      ggc_mark_tree (n->data.case_stmt.nominal_type);

      n = n->next;
    }
}

/* Mark G for GC.  */

static void
mark_goto_fixup (g)
     struct goto_fixup *g;
{
  while (g)
    {
      ggc_mark_rtx (g->before_jump);
      ggc_mark_tree (g->target);
      ggc_mark_tree (g->context);
      ggc_mark_rtx (g->target_rtl);
      ggc_mark_rtx (g->stack_level);
      ggc_mark_tree (g->cleanup_list_list);

      g = g->next;
    }
}

/* Clear out all parts of the state in F that can safely be discarded
   after the function has been compiled, to let garbage collection
   reclaim the memory.  */

void
free_stmt_status (f)
     struct function *f;
{
  /* We're about to free the function obstack.  If we hold pointers to
     things allocated there, then we'll try to mark them when we do
     GC.  So, we clear them out here explicitly.  */
  if (f->stmt)
    free (f->stmt);
  f->stmt = NULL;
}

/* Mark P for GC.  */

void
mark_stmt_status (p)
     struct stmt_status *p;
{
  if (p == 0)
    return;

  mark_block_nesting (p->x_block_stack);
  mark_cond_nesting (p->x_cond_stack);
  mark_loop_nesting (p->x_loop_stack);
  mark_case_nesting (p->x_case_stack);

  ggc_mark_tree (p->x_last_expr_type);
  /* last_epxr_value is only valid if last_expr_type is nonzero.  */
  if (p->x_last_expr_type)
    ggc_mark_rtx (p->x_last_expr_value);

  mark_goto_fixup (p->x_goto_fixup_chain);
}

void
init_stmt ()
{
  int i;

  gcc_obstack_init (&stmt_obstack);

  for (i = 0; i < 10; i++)
    {
      digit_strings[i] = ggc_alloc_string (NULL, 1);
      digit_strings[i][0] = '0' + i;
    }
  ggc_add_string_root (digit_strings, 10);
}

void
init_stmt_for_function ()
{
  cfun->stmt = (struct stmt_status *) xmalloc (sizeof (struct stmt_status));

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
  last_expr_type = 0;
  last_expr_value = NULL_RTX;
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
  rtx x = expand_expr (exp, NULL_RTX, VOIDmode, 0);

#ifdef POINTERS_EXTEND_UNSIGNED
  x = convert_memory_address (Pmode, x);
#endif

  emit_queue ();
  /* Be sure the function is executable.  */
  if (current_function_check_memory_usage)
    emit_library_call (chkr_check_exec_libfunc, 1,
		       VOIDmode, 1, x, ptr_mode);

  do_pending_stack_adjust ();
  emit_indirect_jump (x);

  current_function_has_computed_jump = 1;
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
      rtx temp, handler_slot;
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

#if HAVE_nonlocal_goto
      if (HAVE_nonlocal_goto)
	emit_insn (gen_nonlocal_goto (lookup_static_chain (label),
				      copy_rtx (handler_slot),
				      copy_rtx (p->x_nonlocal_goto_stack_level),
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
	  addr = copy_rtx (handler_slot);
	  temp = copy_to_reg (replace_rtx (addr, virtual_stack_vars_rtx,
					   hard_frame_pointer_rtx));
	  
	  /* Restore the stack pointer.  Note this uses fp just restored.  */
	  addr = p->x_nonlocal_goto_stack_level;
	  if (addr)
	    addr = replace_rtx (copy_rtx (addr),
				virtual_stack_vars_rtx,
				hard_frame_pointer_rtx);

	  emit_stack_restore (SAVE_NONLOCAL, addr, NULL_RTX);

	  /* USE of hard_frame_pointer_rtx added for consistency; not clear if
	     really needed.  */
	  emit_insn (gen_rtx_USE (VOIDmode, hard_frame_pointer_rtx));
	  emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
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
        register rtx original_before_jump
          = last_insn ? last_insn : get_last_insn ();
	rtx start;
	rtx end;
	tree block;

	block = make_node (BLOCK);
	TREE_USED (block) = 1;

	if (!cfun->x_whole_function_mode_p)
	  insert_block (block);
	else
	  {
	    BLOCK_CHAIN (block) 
	      = BLOCK_CHAIN (DECL_INITIAL (current_function_decl));
	    BLOCK_CHAIN (DECL_INITIAL (current_function_decl))
	      = block;
	  }

        start_sequence ();
        start = emit_note (NULL_PTR, NOTE_INSN_BLOCK_BEG);
	if (cfun->x_whole_function_mode_p)
	  NOTE_BLOCK (start) = block;
	fixup->before_jump = emit_note (NULL_PTR, NOTE_INSN_DELETED);
	end = emit_note (NULL_PTR, NOTE_INSN_BLOCK_END);
	if (cfun->x_whole_function_mode_p)
	  NOTE_BLOCK (end) = block;
        fixup->context = block;
        end_sequence ();
        emit_insns_after (start, original_before_jump);
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
     of scope when the block ends.  */
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
	      if (cleanup_insns != 0)
		f->before_jump
		  = emit_insns_after (cleanup_insns, f->before_jump);

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
   BODY is a STRING_CST node containing the assembler code text,
   or an ADDR_EXPR containing a STRING_CST.  */

void
expand_asm (body)
     tree body;
{
  if (current_function_check_memory_usage)
    {
      error ("`asm' cannot be used in function where memory usage is checked");
      return;
    }

  if (TREE_CODE (body) == ADDR_EXPR)
    body = TREE_OPERAND (body, 0);

  emit_insn (gen_rtx_ASM_INPUT (VOIDmode,
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
  int ninout = 0;
  int nclobbers;
  tree tail;
  register int i;
  /* Vector of RTX's of evaluated output operands.  */
  rtx *output_rtx = (rtx *) alloca (noutputs * sizeof (rtx));
  int *inout_opnum = (int *) alloca (noutputs * sizeof (int));
  rtx *real_output_rtx = (rtx *) alloca (noutputs * sizeof (rtx));
  enum machine_mode *inout_mode
    = (enum machine_mode *) alloca (noutputs * sizeof (enum machine_mode));
  /* The insn we have emitted.  */
  rtx insn;

  /* An ASM with no outputs needs to be treated as volatile, for now.  */
  if (noutputs == 0)
    vol = 1;

  if (current_function_check_memory_usage)
    {
      error ("`asm' cannot be used with `-fcheck-memory-usage'");
      return;
    }

#ifdef MD_ASM_CLOBBERS
  /* Sometimes we wish to automatically clobber registers across an asm.
     Case in point is when the i386 backend moved from cc0 to a hard reg --
     maintaining source-level compatability means automatically clobbering
     the flags register.  */
  MD_ASM_CLOBBERS (clobbers);
#endif

  if (current_function_check_memory_usage)
    {
      error ("`asm' cannot be used in function where memory usage is checked");
      return;
    }

  /* Count the number of meaningful clobbered registers, ignoring what
     we would ignore later.  */
  nclobbers = 0;
  for (tail = clobbers; tail; tail = TREE_CHAIN (tail))
    {
      const char *regname = TREE_STRING_POINTER (TREE_VALUE (tail));

      i = decode_reg_name (regname);
      if (i >= 0 || i == -4)
	++nclobbers;
      else if (i == -2)
	error ("unknown register name `%s' in `asm'", regname);
    }

  last_expr_type = 0;

  /* Check that the number of alternatives is constant across all
     operands.  */
  if (outputs || inputs)
    {
      tree tmp = TREE_PURPOSE (outputs ? outputs : inputs);
      int nalternatives = n_occurrences (',', TREE_STRING_POINTER (tmp));
      tree next = inputs;

      if (nalternatives + 1 > MAX_RECOG_ALTERNATIVES)
	{
	  error ("too many alternatives in `asm'");
	  return;
	}
      
      tmp = outputs;
      while (tmp)
	{
	  const char *constraint = TREE_STRING_POINTER (TREE_PURPOSE (tmp));

	  if (n_occurrences (',', constraint) != nalternatives)
	    {
	      error ("operand constraints for `asm' differ in number of alternatives");
	      return;
	    }

	  if (TREE_CHAIN (tmp))
	    tmp = TREE_CHAIN (tmp);
	  else
	    tmp = next, next = 0;
	}
    }

  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    {
      tree val = TREE_VALUE (tail);
      tree type = TREE_TYPE (val);
      char *constraint;
      char *p;
      int c_len;
      int j;
      int is_inout = 0;
      int allows_reg = 0;
      int allows_mem = 0;

      /* If there's an erroneous arg, emit no insn.  */
      if (TREE_TYPE (val) == error_mark_node)
	return;

      /* Make sure constraint has `=' and does not have `+'.  Also, see
	 if it allows any register.  Be liberal on the latter test, since
	 the worst that happens if we get it wrong is we issue an error
	 message.  */

      c_len = strlen (TREE_STRING_POINTER (TREE_PURPOSE (tail)));
      constraint = TREE_STRING_POINTER (TREE_PURPOSE (tail));

      /* Allow the `=' or `+' to not be at the beginning of the string,
	 since it wasn't explicitly documented that way, and there is a
	 large body of code that puts it last.  Swap the character to
	 the front, so as not to uglify any place else.  */
      switch (c_len)
	{
	default:
	  if ((p = strchr (constraint, '=')) != NULL)
	    break;
	  if ((p = strchr (constraint, '+')) != NULL)
	    break;
	case 0:
	  error ("output operand constraint lacks `='");
	  return;
	}

      if (p != constraint)
	{
	  j = *p;
	  bcopy (constraint, constraint+1, p-constraint);
	  *constraint = j;

	  warning ("output constraint `%c' for operand %d is not at the beginning", j, i);
	}

      is_inout = constraint[0] == '+';
      /* Replace '+' with '='.  */
      constraint[0] = '=';
      /* Make sure we can specify the matching operand.  */
      if (is_inout && i > 9)
	{
	  error ("output operand constraint %d contains `+'", i);
	  return;
	}

      for (j = 1; j < c_len; j++)
	switch (constraint[j])
	  {
	  case '+':
	  case '=':
	    error ("operand constraint contains '+' or '=' at illegal position.");
	    return;

	  case '%':
	    if (i + 1 == ninputs + noutputs)
	      {
		error ("`%%' constraint used with last operand");
		return;
	      }
	    break;

	  case '?':  case '!':  case '*':  case '&':
	  case 'E':  case 'F':  case 'G':  case 'H':
	  case 's':  case 'i':  case 'n':
	  case 'I':  case 'J':  case 'K':  case 'L':  case 'M':
	  case 'N':  case 'O':  case 'P':  case ',':
#ifdef EXTRA_CONSTRAINT
	  case 'Q':  case 'R':  case 'S':  case 'T':  case 'U':
#endif
	    break;

	  case '0':  case '1':  case '2':  case '3':  case '4':
	  case '5':  case '6':  case '7':  case '8':  case '9':
	    error ("matching constraint not valid in output operand");
	    break;

	  case 'V':  case 'm':  case 'o':
	    allows_mem = 1;
	    break;

	  case '<':  case '>':
          /* ??? Before flow, auto inc/dec insns are not supposed to exist,
             excepting those that expand_call created.  So match memory
	     and hope.  */
	    allows_mem = 1;
	    break;

	  case 'g':  case 'X':
	    allows_reg = 1;
	    allows_mem = 1;
	    break;

	  case 'p': case 'r':
	  default:
	    allows_reg = 1;
	    break;
	  }

      /* If an output operand is not a decl or indirect ref and our constraint
	 allows a register, make a temporary to act as an intermediate.
	 Make the asm insn write into that, then our caller will copy it to
	 the real output operand.  Likewise for promoted variables.  */

      real_output_rtx[i] = NULL_RTX;
      if ((TREE_CODE (val) == INDIRECT_REF
	   && allows_mem)
	  || (TREE_CODE_CLASS (TREE_CODE (val)) == 'd'
	      && (allows_mem || GET_CODE (DECL_RTL (val)) == REG)
	      && ! (GET_CODE (DECL_RTL (val)) == REG
		    && GET_MODE (DECL_RTL (val)) != TYPE_MODE (type)))
	  || ! allows_reg
	  || is_inout)
	{
	  if (! allows_reg)
	    mark_addressable (TREE_VALUE (tail));

	  output_rtx[i]
	    = expand_expr (TREE_VALUE (tail), NULL_RTX, VOIDmode,
			   EXPAND_MEMORY_USE_WO);

	  if (! allows_reg && GET_CODE (output_rtx[i]) != MEM)
	    error ("output number %d not directly addressable", i);
	  if (! allows_mem && GET_CODE (output_rtx[i]) == MEM)
	    {
    	      real_output_rtx[i] = protect_from_queue (output_rtx[i], 1);
	      output_rtx[i] = gen_reg_rtx (GET_MODE (output_rtx[i]));
	      if (is_inout)
		emit_move_insn (output_rtx[i], real_output_rtx[i]);
	    }
	}
      else
	{
	  output_rtx[i] = assign_temp (type, 0, 0, 0);
	  TREE_VALUE (tail) = make_tree (type, output_rtx[i]);
	}

      if (is_inout)
	{
	  inout_mode[ninout] = TYPE_MODE (TREE_TYPE (TREE_VALUE (tail)));
	  inout_opnum[ninout++] = i;
	}
    }

  ninputs += ninout;
  if (ninputs + noutputs > MAX_RECOG_OPERANDS)
    {
      error ("more than %d operands in `asm'", MAX_RECOG_OPERANDS);
      return;
    }

  /* Make vectors for the expression-rtx and constraint strings.  */

  argvec = rtvec_alloc (ninputs);
  constraints = rtvec_alloc (ninputs);

  body = gen_rtx_ASM_OPERANDS (VOIDmode, TREE_STRING_POINTER (string), 
			       empty_string, 0, argvec, constraints, 
			       filename, line);

  MEM_VOLATILE_P (body) = vol;

  /* Eval the inputs and put them into ARGVEC.
     Put their constraints into ASM_INPUTs and store in CONSTRAINTS.  */

  i = 0;
  for (tail = inputs; tail; tail = TREE_CHAIN (tail))
    {
      int j;
      int allows_reg = 0, allows_mem = 0;
      char *constraint, *orig_constraint;
      int c_len;
      rtx op;

      /* If there's an erroneous arg, emit no insn,
	 because the ASM_INPUT would get VOIDmode
	 and that could cause a crash in reload.  */
      if (TREE_TYPE (TREE_VALUE (tail)) == error_mark_node)
	return;

      /* ??? Can this happen, and does the error message make any sense? */
      if (TREE_PURPOSE (tail) == NULL_TREE)
	{
	  error ("hard register `%s' listed as input operand to `asm'",
		 TREE_STRING_POINTER (TREE_VALUE (tail)) );
	  return;
	}

      c_len = strlen (TREE_STRING_POINTER (TREE_PURPOSE (tail)));
      constraint = TREE_STRING_POINTER (TREE_PURPOSE (tail));
      orig_constraint = constraint;

      /* Make sure constraint has neither `=', `+', nor '&'.  */

      for (j = 0; j < c_len; j++)
	switch (constraint[j])
	  {
	  case '+':  case '=':  case '&':
	    if (constraint == orig_constraint)
	      {
	        error ("input operand constraint contains `%c'",
		       constraint[j]);
	        return;
	      }
	    break;

	  case '%':
	    if (constraint == orig_constraint
		&& i + 1 == ninputs - ninout)
	      {
		error ("`%%' constraint used with last operand");
		return;
	      }
	    break;

	  case 'V':  case 'm':  case 'o':
	    allows_mem = 1;
	    break;

	  case '<':  case '>':
	  case '?':  case '!':  case '*':
	  case 'E':  case 'F':  case 'G':  case 'H':  case 'X':
	  case 's':  case 'i':  case 'n':
	  case 'I':  case 'J':  case 'K':  case 'L':  case 'M':
	  case 'N':  case 'O':  case 'P':  case ',':
#ifdef EXTRA_CONSTRAINT
	  case 'Q':  case 'R':  case 'S':  case 'T':  case 'U':
#endif
	    break;

	    /* Whether or not a numeric constraint allows a register is
	       decided by the matching constraint, and so there is no need
	       to do anything special with them.  We must handle them in
	       the default case, so that we don't unnecessarily force
	       operands to memory.  */
	  case '0':  case '1':  case '2':  case '3':  case '4':
	  case '5':  case '6':  case '7':  case '8':  case '9':
	    if (constraint[j] >= '0' + noutputs)
	      {
		error
		  ("matching constraint references invalid operand number");
		return;
	      }

	    /* Try and find the real constraint for this dup.  */
	    if ((j == 0 && c_len == 1)
		|| (j == 1 && c_len == 2 && constraint[0] == '%'))
	      {
		tree o = outputs;

		for (j = constraint[j] - '0'; j > 0; --j)
		  o = TREE_CHAIN (o);
	
		c_len = strlen (TREE_STRING_POINTER (TREE_PURPOSE (o)));
		constraint = TREE_STRING_POINTER (TREE_PURPOSE (o));
		j = 0;
		break;
	      }

	    /* ... fall through ... */

	  case 'p':  case 'r':
	  default:
	    allows_reg = 1;
	    break;

	  case 'g':
	    allows_reg = 1;
	    allows_mem = 1;
	    break;
	  }

      if (! allows_reg && allows_mem)
	mark_addressable (TREE_VALUE (tail));

      op = expand_expr (TREE_VALUE (tail), NULL_RTX, VOIDmode, 0);

      if (asm_operand_ok (op, constraint) <= 0)
	{
	  if (allows_reg)
	    op = force_reg (TYPE_MODE (TREE_TYPE (TREE_VALUE (tail))), op);
	  else if (!allows_mem)
	    warning ("asm operand %d probably doesn't match constraints", i);
	  else if (CONSTANT_P (op))
	    op = force_const_mem (TYPE_MODE (TREE_TYPE (TREE_VALUE (tail))),
				  op);
	  else if (GET_CODE (op) == REG
		   || GET_CODE (op) == SUBREG
		   || GET_CODE (op) == CONCAT)
	    {
	      tree type = TREE_TYPE (TREE_VALUE (tail));
	      rtx memloc = assign_temp (type, 1, 1, 1);

	      emit_move_insn (memloc, op);
	      op = memloc;
	    }

	  else if (GET_CODE (op) == MEM && MEM_VOLATILE_P (op))
	    /* We won't recognize volatile memory as available a
	       memory_operand at this point.  Ignore it.  */
	    ;
	  else if (queued_subexp_p (op))
	    ;
	  else
	    /* ??? Leave this only until we have experience with what
	       happens in combine and elsewhere when constraints are
	       not satisfied.  */
	    warning ("asm operand %d probably doesn't match constraints", i);
	}
      XVECEXP (body, 3, i) = op;

      XVECEXP (body, 4, i)      /* constraints */
	= gen_rtx_ASM_INPUT (TYPE_MODE (TREE_TYPE (TREE_VALUE (tail))),
			     orig_constraint);
      i++;
    }

  /* Protect all the operands from the queue now that they have all been
     evaluated.  */

  for (i = 0; i < ninputs - ninout; i++)
    XVECEXP (body, 3, i) = protect_from_queue (XVECEXP (body, 3, i), 0);

  for (i = 0; i < noutputs; i++)
    output_rtx[i] = protect_from_queue (output_rtx[i], 1);

  /* For in-out operands, copy output rtx to input rtx. */
  for (i = 0; i < ninout; i++)
    {
      int j = inout_opnum[i];

      XVECEXP (body, 3, ninputs - ninout + i)      /* argvec */
	= output_rtx[j];
      XVECEXP (body, 4, ninputs - ninout + i)      /* constraints */
	= gen_rtx_ASM_INPUT (inout_mode[i], digit_strings[j]);
    }

  /* Now, for each output, construct an rtx
     (set OUTPUT (asm_operands INSN OUTPUTNUMBER OUTPUTCONSTRAINT
			       ARGVEC CONSTRAINTS))
     If there is more than one, put them inside a PARALLEL.  */

  if (noutputs == 1 && nclobbers == 0)
    {
      XSTR (body, 1) = TREE_STRING_POINTER (TREE_PURPOSE (outputs));
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
			   (VOIDmode,
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
	  const char *regname = TREE_STRING_POINTER (TREE_VALUE (tail));
	  int j = decode_reg_name (regname);

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
	  XVECEXP (body, 0, i++)
	    = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (QImode, j));
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

/* Generate RTL to evaluate the expression EXP
   and remember it in case this is the VALUE in a ({... VALUE; }) constr.  */

void
expand_expr_stmt (exp)
     tree exp;
{
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
	  emit_cmp_and_jump_insns (last_expr_value, last_expr_value, EQ,
				   expand_expr (TYPE_SIZE (last_expr_type),
						NULL_RTX, VOIDmode, 0),
				   BLKmode, 0,
				   TYPE_ALIGN (last_expr_type) / BITS_PER_UNIT,
				   lab);
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
    case TRY_CATCH_EXPR:
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
      /* ... fall through ...  */
      
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
  last_expr_type = 0;
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
  do_pending_stack_adjust ();
  emit_note (NULL_PTR, NOTE_INSN_LOOP_CONT);
  emit_label (loop_stack->data.loop.continue_label);
}

/* Finish a loop.  Generate a jump back to the top and the loop-exit label.
   Pop the block off of loop_stack.  */

void
expand_end_loop ()
{
  rtx start_label = loop_stack->data.loop.start_label;
  rtx insn = get_last_insn ();
  int needs_end_jump = 1;

  /* Mark the continue-point at the top of the loop if none elsewhere.  */
  if (start_label == loop_stack->data.loop.continue_label)
    emit_note_before (NOTE_INSN_LOOP_CONT, start_label);

  do_pending_stack_adjust ();

  /* If optimizing, perhaps reorder the loop.
     First, try to use a condjump near the end.
     expand_exit_loop_if_false ends loops with unconditional jumps,
     like this:

     if (test) goto label;
     optional: cleanup
     goto loop_stack->data.loop.end_label
     barrier
     label:

     If we find such a pattern, we can end the loop earlier.  */

  if (optimize
      && GET_CODE (insn) == CODE_LABEL
      && LABEL_NAME (insn) == NULL
      && GET_CODE (PREV_INSN (insn)) == BARRIER)
    {
      rtx label = insn;
      rtx jump = PREV_INSN (PREV_INSN (label));

      if (GET_CODE (jump) == JUMP_INSN
	  && GET_CODE (PATTERN (jump)) == SET
	  && SET_DEST (PATTERN (jump)) == pc_rtx
	  && GET_CODE (SET_SRC (PATTERN (jump))) == LABEL_REF
	  && (XEXP (SET_SRC (PATTERN (jump)), 0)
	      == loop_stack->data.loop.end_label))
	{
	  rtx prev;

	  /* The test might be complex and reference LABEL multiple times,
	     like the loop in loop_iterations to set vtop.  To handle this,
	     we move LABEL.  */
	  insn = PREV_INSN (label);
	  reorder_insns (label, label, start_label);

	  for (prev = PREV_INSN (jump); ; prev = PREV_INSN (prev))
	   {
	      /* We ignore line number notes, but if we see any other note,
		 in particular NOTE_INSN_BLOCK_*, NOTE_INSN_EH_REGION_*,
		 NOTE_INSN_LOOP_*, we disable this optimization.  */
	      if (GET_CODE (prev) == NOTE)
		{
		  if (NOTE_LINE_NUMBER (prev) < 0)
		    break;
		  continue;
		}
	      if (GET_CODE (prev) == CODE_LABEL)
		break;
	      if (GET_CODE (prev) == JUMP_INSN)
		{
		  if (GET_CODE (PATTERN (prev)) == SET
		      && SET_DEST (PATTERN (prev)) == pc_rtx
		      && GET_CODE (SET_SRC (PATTERN (prev))) == IF_THEN_ELSE
		      && (GET_CODE (XEXP (SET_SRC (PATTERN (prev)), 1))
			  == LABEL_REF)
		      && XEXP (XEXP (SET_SRC (PATTERN (prev)), 1), 0) == label)
		    {
		      XEXP (XEXP (SET_SRC (PATTERN (prev)), 1), 0)
			= start_label;
		      emit_note_after (NOTE_INSN_LOOP_END, prev);
		      needs_end_jump = 0;
		    }
		  break;
		}
	   }
	}
    }

     /* If the loop starts with a loop exit, roll that to the end where
     it will optimize together with the jump back.

     We look for the conditional branch to the exit, except that once
     we find such a branch, we don't look past 30 instructions.

     In more detail, if the loop presently looks like this (in pseudo-C):

         start_label:
         if (test) goto end_label;
	 body;
	 goto start_label;
	 end_label:
	 
     transform it to look like:

         goto start_label;
         newstart_label:
	 body;
	 start_label:
	 if (test) goto end_label;
	 goto newstart_label;
	 end_label:

     Here, the `test' may actually consist of some reasonably complex
     code, terminating in a test.  */

  if (optimize
      && needs_end_jump
      &&
      ! (GET_CODE (insn) == JUMP_INSN
	 && GET_CODE (PATTERN (insn)) == SET
	 && SET_DEST (PATTERN (insn)) == pc_rtx
	 && GET_CODE (SET_SRC (PATTERN (insn))) == IF_THEN_ELSE))
    {
      int eh_regions = 0;
      int num_insns = 0;
      rtx last_test_insn = NULL_RTX;

      /* Scan insns from the top of the loop looking for a qualified
	 conditional exit.  */
      for (insn = NEXT_INSN (loop_stack->data.loop.start_label); insn;
	   insn = NEXT_INSN (insn))
	{
	  if (GET_CODE (insn) == NOTE) 
	    {
	      if (optimize < 2
		  && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG
		      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END))
		/* The code that actually moves the exit test will
		   carefully leave BLOCK notes in their original
		   location.  That means, however, that we can't debug
		   the exit test itself.  So, we refuse to move code
		   containing BLOCK notes at low optimization levels.  */
		break;

	      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG)
		++eh_regions;
	      else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END)
		{
		  --eh_regions;
		  if (eh_regions < 0) 
		    /* We've come to the end of an EH region, but
		       never saw the beginning of that region.  That
		       means that an EH region begins before the top
		       of the loop, and ends in the middle of it.  The
		       existence of such a situation violates a basic
		       assumption in this code, since that would imply
		       that even when EH_REGIONS is zero, we might
		       move code out of an exception region.  */
		    abort ();
		}

	      /* We must not walk into a nested loop.  */
	      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
		break;

	      /* We already know this INSN is a NOTE, so there's no
		 point in looking at it to see if it's a JUMP.  */
	      continue;
	    }

	  if (GET_CODE (insn) == JUMP_INSN || GET_CODE (insn) == INSN)
	    num_insns++;

	  if (last_test_insn && num_insns > 30)
	    break;

	  if (eh_regions > 0) 
	    /* We don't want to move a partial EH region.  Consider:

		  while ( ( { try {
				if (cond ()) 0;	
				else {
				  bar();
				  1;
				}
			      } catch (...) { 
				1;
			      } )) {
		     body;
		  } 

	        This isn't legal C++, but here's what it's supposed to
	        mean: if cond() is true, stop looping.  Otherwise,
	        call bar, and keep looping.  In addition, if cond
	        throws an exception, catch it and keep looping. Such
	        constructs are certainy legal in LISP.  

		We should not move the `if (cond()) 0' test since then
		the EH-region for the try-block would be broken up.
		(In this case we would the EH_BEG note for the `try'
		and `if cond()' but not the call to bar() or the
		EH_END note.)  

	        So we don't look for tests within an EH region.  */
	    continue;

	  if (GET_CODE (insn) == JUMP_INSN 
	      && GET_CODE (PATTERN (insn)) == SET
	      && SET_DEST (PATTERN (insn)) == pc_rtx)
	    {
	      /* This is indeed a jump.  */
	      rtx dest1 = NULL_RTX;
	      rtx dest2 = NULL_RTX;
	      rtx potential_last_test;
	      if (GET_CODE (SET_SRC (PATTERN (insn))) == IF_THEN_ELSE)
		{
		  /* A conditional jump.  */
		  dest1 = XEXP (SET_SRC (PATTERN (insn)), 1);
		  dest2 = XEXP (SET_SRC (PATTERN (insn)), 2);
		  potential_last_test = insn;
		}
	      else
		{
		  /* An unconditional jump.  */
		  dest1 = SET_SRC (PATTERN (insn));
		  /* Include the BARRIER after the JUMP.  */
		  potential_last_test = NEXT_INSN (insn);
		}

	      do {
		if (dest1 && GET_CODE (dest1) == LABEL_REF
		    && ((XEXP (dest1, 0) 
			 == loop_stack->data.loop.alt_end_label)
			|| (XEXP (dest1, 0) 
			    == loop_stack->data.loop.end_label)))
		  {
		    last_test_insn = potential_last_test;
		    break;
		  }

		/* If this was a conditional jump, there may be
		   another label at which we should look.  */
		dest1 = dest2;
		dest2 = NULL_RTX;
	      } while (dest1);
	    }
	}

      if (last_test_insn != 0 && last_test_insn != get_last_insn ())
	{
	  /* We found one.  Move everything from there up
	     to the end of the loop, and add a jump into the loop
	     to jump to there.  */
	  register rtx newstart_label = gen_label_rtx ();
	  register rtx start_move = start_label;
	  rtx next_insn;

	  /* If the start label is preceded by a NOTE_INSN_LOOP_CONT note,
	     then we want to move this note also.  */
	  if (GET_CODE (PREV_INSN (start_move)) == NOTE
	      && (NOTE_LINE_NUMBER (PREV_INSN (start_move))
		  == NOTE_INSN_LOOP_CONT))
	    start_move = PREV_INSN (start_move);

	  emit_label_after (newstart_label, PREV_INSN (start_move));

	  /* Actually move the insns.  Start at the beginning, and
	     keep copying insns until we've copied the
	     last_test_insn.  */
	  for (insn = start_move; insn; insn = next_insn)
	    {
	      /* Figure out which insn comes after this one.  We have
		 to do this before we move INSN.  */
	      if (insn == last_test_insn)
		/* We've moved all the insns.  */
		next_insn = NULL_RTX;
	      else
		next_insn = NEXT_INSN (insn);

	      if (GET_CODE (insn) == NOTE
		  && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG
		      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END))
		/* We don't want to move NOTE_INSN_BLOCK_BEGs or
		   NOTE_INSN_BLOCK_ENDs because the correct generation
		   of debugging information depends on these appearing
		   in the same order in the RTL and in the tree
		   structure, where they are represented as BLOCKs.
		   So, we don't move block notes.  Of course, moving
		   the code inside the block is likely to make it
		   impossible to debug the instructions in the exit
		   test, but such is the price of optimization.  */
		continue;

	      /* Move the INSN.  */
	      reorder_insns (insn, insn, get_last_insn ());
	    }

	  emit_jump_insn_after (gen_jump (start_label),
				PREV_INSN (newstart_label));
	  emit_barrier_after (PREV_INSN (newstart_label));
	  start_label = newstart_label;
	}
    }

  if (needs_end_jump)
    {
      emit_jump (start_label);
      emit_note (NULL_PTR, NOTE_INSN_LOOP_END);
    }
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
  rtx label = gen_label_rtx ();
  rtx last_insn;
  last_expr_type = 0;

  if (whichloop == 0)
    whichloop = loop_stack;
  if (whichloop == 0)
    return 0;
  /* In order to handle fixups, we actually create a conditional jump
     around a unconditional branch to exit the loop.  If fixups are
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

/* Return nonzero if the loop nest is empty.  Else return zero.  */

int
stmt_loop_nest_empty ()
{
  return (loop_stack == NULL);
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
  rtx last_insn = get_last_insn ();

  /* If this function was declared to return a value, but we 
     didn't, clobber the return registers so that they are not
     propogated live to the rest of the function.  */
  clobber_return_register ();

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
	emit_group_load (return_reg, val, int_size_in_bytes (type),
			 TYPE_ALIGN (type) / BITS_PER_UNIT);
      else
	emit_move_insn (return_reg, val);
    }

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
  rtx result_rtl = DECL_RTL (DECL_RESULT (current_function_decl));
  register rtx val = 0;
#ifdef HAVE_return
  register rtx op0;
#endif
  tree retval_rhs;
  int cleanups;

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

  /* Attempt to optimize the call if it is tail recursive.  */
  if (optimize_tail_recursion (retval_rhs, last_insn))
    return;

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
	 if we can do it with explicit return insns and branches are cheap,
	 but not if we have the corresponding scc insn.  */
      int has_scc = 0;
      if (retval_rhs)
	switch (TREE_CODE (retval_rhs))
	  {
	  case EQ_EXPR:
#ifdef HAVE_seq
	    has_scc = HAVE_seq;
#endif
	  case NE_EXPR:
#ifdef HAVE_sne
	    has_scc = HAVE_sne;
#endif
	  case GT_EXPR:
#ifdef HAVE_sgt
	    has_scc = HAVE_sgt;
#endif
	  case GE_EXPR:
#ifdef HAVE_sge
	    has_scc = HAVE_sge;
#endif
	  case LT_EXPR:
#ifdef HAVE_slt
	    has_scc = HAVE_slt;
#endif
	  case LE_EXPR:
#ifdef HAVE_sle
	    has_scc = HAVE_sle;
#endif
	  case TRUTH_ANDIF_EXPR:
	  case TRUTH_ORIF_EXPR:
	  case TRUTH_AND_EXPR:
	  case TRUTH_OR_EXPR:
	  case TRUTH_NOT_EXPR:
	  case TRUTH_XOR_EXPR:
	    if (! has_scc)
	      {
		op0 = gen_label_rtx ();
		jumpifnot (retval_rhs, op0);
		expand_value_return (const1_rtx);
		emit_label (op0);
		expand_value_return (const0_rtx);
		return;
	      }
	    break;

	  default:
	    break;
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
      && GET_CODE (result_rtl) == REG)
    {
      int i, bitpos, xbitpos;
      int big_endian_correction = 0;
      int bytes = int_size_in_bytes (TREE_TYPE (retval_rhs));
      int n_regs = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
      int bitsize = MIN (TYPE_ALIGN (TREE_TYPE (retval_rhs)),
			 (unsigned int)BITS_PER_WORD);
      rtx *result_pseudos = (rtx *) alloca (sizeof (rtx) * n_regs);
      rtx result_reg, src = NULL_RTX, dst = NULL_RTX;
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
	      emit_insn (gen_rtx_CLOBBER (VOIDmode, dst));
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
	   tmpmode != VOIDmode;
	   tmpmode = GET_MODE_WIDER_MODE (tmpmode))
	{
	  /* Have we found a large enough mode?  */
	  if (GET_MODE_SIZE (tmpmode) >= bytes)
	    break;
	}

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
  else if (cleanups
      && retval_rhs != 0
      && TREE_TYPE (retval_rhs) != void_type_node
      && (GET_CODE (result_rtl) == REG
	  || (GET_CODE (result_rtl) == PARALLEL)))
    {
      /* Calculate the return value into a temporary (usually a pseudo
         reg).  */
      val = assign_temp (TREE_TYPE (DECL_RESULT (current_function_decl)),
			 0, 0, 1);
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

/* Test CALL_EXPR to determine if it is a potential tail recursion call
   and emit code to optimize the tail recursion.  LAST_INSN indicates where
   to place the jump to the tail recursion label.  Return TRUE if the
   call was optimized into a goto.

   This is only used by expand_return, but expand_call is expected to
   use it soon.  */

int
optimize_tail_recursion (call_expr, last_insn)
     tree call_expr;
     rtx last_insn;
{
  /* For tail-recursive call to current function,
     just jump back to the beginning.
     It's unsafe if any auto variable in this function
     has its address taken; for simplicity,
     require stack frame to be empty.  */
  if (optimize && call_expr != 0
      && frame_offset == 0
      && TREE_CODE (call_expr) == CALL_EXPR
      && TREE_CODE (TREE_OPERAND (call_expr, 0)) == ADDR_EXPR
      && TREE_OPERAND (TREE_OPERAND (call_expr, 0), 0) == current_function_decl
      /* Finish checking validity, and if valid emit code
	 to set the argument variables for the new call.  */
      && tail_recursion_args (TREE_OPERAND (call_expr, 1),
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
      note = emit_note (NULL_PTR, NOTE_INSN_BLOCK_BEG);
      NOTE_BLOCK (note) = block;
    }
  else
    note = emit_note (NULL_PTR, NOTE_INSN_DELETED);
    
  /* Make an entry on block_stack for the block we are entering.  */

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
  emit_note (NULL_PTR, NOTE_INSN_DELETED);
  thisblock->data.block.cleanup_ptr = &thisblock->data.block.cleanups;

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

/* Given a pointer to a BLOCK node return non-zero if (and only if) the node
   in question represents the outermost pair of curly braces (i.e. the "body
   block") of a function or method.

   For any BLOCK node representing a "body block" of a function or method, the
   BLOCK_SUPERCONTEXT of the node will point to another BLOCK node which
   represents the outermost (function) scope for the function or method (i.e.
   the one which includes the formal parameters).  The BLOCK_SUPERCONTEXT of
   *that* node in turn will point to the relevant FUNCTION_DECL node. */

int
is_body_block (stmt)
     register tree stmt;
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

/* Mark top block of block_stack as an implicit binding for an
   exception region.  This is used to prevent infinite recursion when
   ending a binding with expand_end_bindings.  It is only ever called
   by expand_eh_region_start, as that it the only way to create a
   block stack for a exception region.  */

void
mark_block_as_eh_region ()
{
  block_stack->data.block.exception_region = 1;
  if (block_stack->next
      && block_stack->next->data.block.conditional_code)
    {
      block_stack->data.block.conditional_code
	= block_stack->next->data.block.conditional_code;
      block_stack->data.block.last_unconditional_cleanup
	= block_stack->next->data.block.last_unconditional_cleanup;
      block_stack->data.block.cleanup_ptr
	= block_stack->next->data.block.cleanup_ptr;
    }
}

/* True if we are currently emitting insns in an area of output code
   that is controlled by a conditional expression.  This is used by
   the cleanup handling code to generate conditional cleanup actions.  */

int
conditional_context ()
{
  return block_stack && block_stack->data.block.conditional_code;
}

/* Mark top block of block_stack as not for an implicit binding for an
   exception region.  This is only ever done by expand_eh_region_end
   to let expand_end_bindings know that it is being called explicitly
   to end the binding layer for just the binding layer associated with
   the exception region, otherwise expand_end_bindings would try and
   end all implicit binding layers for exceptions regions, and then
   one normal binding layer.  */

void
mark_block_as_not_eh_region ()
{
  block_stack->data.block.exception_region = 0;
}

/* True if the top block of block_stack was marked as for an exception
   region by mark_block_as_eh_region.  */

int
is_eh_region ()
{
  return cfun && block_stack && block_stack->data.block.exception_region;
}

/* Emit a handler label for a nonlocal goto handler.
   Also emit code to store the handler label in SLOT before BEFORE_INSN.  */

static rtx
expand_nl_handler_label (slot, before_insn)
     rtx slot, before_insn;
{
  rtx insns;
  rtx handler_label = gen_label_rtx ();

  /* Don't let jump_optimize delete the handler.  */
  LABEL_PRESERVE_P (handler_label) = 1;

  start_sequence ();
  emit_move_insn (slot, gen_rtx_LABEL_REF (Pmode, handler_label));
  insns = get_insns ();
  end_sequence ();
  emit_insns_before (insns, before_insn);

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
      static struct elims {int from, to;} elim_regs[] = ELIMINABLE_REGS;
      size_t i;

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
	emit_insns_before (insns, thisblock->data.block.first_insn);
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
      emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "abort"), 0,
			 VOIDmode, 0);
      emit_barrier ();
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

  if (warn_unused)
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
  register struct nesting *thisblock;

  while (block_stack->data.block.exception_region)
    {
      /* Because we don't need or want a new temporary level and
	 because we didn't create one in expand_eh_region_start,
	 create a fake one now to avoid removing one in
	 expand_end_bindings.  */
      push_temp_slots ();

      block_stack->data.block.exception_region = 0;

      expand_end_bindings (NULL_TREE, 0, 0);
    }

  /* Since expand_eh_region_start does an expand_start_bindings, we
     have to first end all the bindings that were created by
     expand_eh_region_start.  */
     
  thisblock = block_stack;

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
      rtx note = emit_note (NULL_PTR, NOTE_INSN_BLOCK_END);
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

/* Generate RTL for the automatic variable declaration DECL.
   (Other kinds of declarations are simply ignored if seen here.)  */

void
expand_decl (decl)
     register tree decl;
{
  struct nesting *thisblock;
  tree type;

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

  thisblock = block_stack;

  /* Create the RTL representation for the variable.  */

  if (type == error_mark_node)
    DECL_RTL (decl) = gen_rtx_MEM (BLKmode, const0_rtx);
  else if (DECL_SIZE (decl) == 0)
    /* Variable with incomplete type.  */
    {
      if (DECL_INITIAL (decl) == 0)
	/* Error message was already done; now avoid a crash.  */
	DECL_RTL (decl) = assign_stack_temp (DECL_MODE (decl), 0, 1);
      else
	/* An initializer is going to decide the size of this array.
	   Until we know the size, represent its address with a reg.  */
	DECL_RTL (decl) = gen_rtx_MEM (BLKmode, gen_reg_rtx (Pmode));
      MEM_SET_IN_STRUCT_P (DECL_RTL (decl), AGGREGATE_TYPE_P (type));
    }
  else if (DECL_MODE (decl) != BLKmode
	   /* If -ffloat-store, don't put explicit float vars
	      into regs.  */
	   && !(flag_float_store
		&& TREE_CODE (type) == REAL_TYPE)
	   && ! TREE_THIS_VOLATILE (decl)
	   && ! TREE_ADDRESSABLE (decl)
	   && (DECL_REGISTER (decl) || optimize)
	   /* if -fcheck-memory-usage, check all variables.  */
	   && ! current_function_check_memory_usage)
    {
      /* Automatic variable that can go in a register.  */
      int unsignedp = TREE_UNSIGNED (type);
      enum machine_mode reg_mode
	= promote_mode (type, DECL_MODE (decl), &unsignedp, 0);

      DECL_RTL (decl) = gen_reg_rtx (reg_mode);
      mark_user_reg (DECL_RTL (decl));

      if (POINTER_TYPE_P (type))
	mark_reg_pointer (DECL_RTL (decl),
			  (TYPE_ALIGN (TREE_TYPE (TREE_TYPE (decl)))
			   / BITS_PER_UNIT));
    }

  else if (TREE_CODE (DECL_SIZE_UNIT (decl)) == INTEGER_CST
	   && ! (flag_stack_check && ! STACK_CHECK_BUILTIN
		 && 0 < compare_tree_int (DECL_SIZE_UNIT (decl),
					  STACK_CHECK_MAX_VAR_SIZE)))
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

      DECL_RTL (decl) = assign_temp (TREE_TYPE (decl), 1, 1, 1);
      MEM_SET_IN_STRUCT_P (DECL_RTL (decl),
			   AGGREGATE_TYPE_P (TREE_TYPE (decl)));

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
      MEM_SET_IN_STRUCT_P (DECL_RTL (decl),
			   AGGREGATE_TYPE_P (TREE_TYPE (decl)));
#if 0
      /* If this is in memory because of -ffloat-store,
	 set the volatile bit, to prevent optimizations from
	 undoing the effects.  */
      if (flag_float_store && TREE_CODE (type) == REAL_TYPE)
	MEM_VOLATILE_P (DECL_RTL (decl)) = 1;
#endif

      MEM_ALIAS_SET (DECL_RTL (decl)) = get_alias_set (decl);
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
      DECL_RTL (decl) = gen_rtx_MEM (DECL_MODE (decl), address);

      /* If this is a memory ref that contains aggregate components,
	 mark it as such for cse and loop optimize.  */
      MEM_SET_IN_STRUCT_P (DECL_RTL (decl),
			   AGGREGATE_TYPE_P (TREE_TYPE (decl)));

      /* Indicate the alignment we actually gave this variable.  */
#ifdef STACK_BOUNDARY
      DECL_ALIGN (decl) = STACK_BOUNDARY;
#else
      DECL_ALIGN (decl) = BIGGEST_ALIGNMENT;
#endif
    }

  if (TREE_THIS_VOLATILE (decl))
    MEM_VOLATILE_P (DECL_RTL (decl)) = 1;

  if (TREE_READONLY (decl))
    RTX_UNCHANGING_P (DECL_RTL (decl)) = 1;
}

/* Emit code to perform the initialization of a declaration DECL.  */

void
expand_decl_init (decl)
     tree decl;
{
  int was_used = TREE_USED (decl);

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
}

/* CLEANUP is an expression to be executed at exit from this binding contour;
   for example, in C++, it might call the destructor for this variable.

   We wrap CLEANUP in an UNSAVE_EXPR node, so that we can expand the
   CLEANUP multiple times, and have the correct semantics.  This
   happens in exception handling, for gotos, returns, breaks that
   leave the current scope.

   If CLEANUP is nonzero and DECL is zero, we record a cleanup
   that is not associated with any particular variable.   */

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
	    = emit_insns_after (set_flag_0,
				thisblock->data.block.last_unconditional_cleanup);

	  emit_move_insn (flag, const1_rtx);

	  /* All cleanups must be on the function_obstack.  */
	  push_obstacks_nochange ();
	  resume_temporary_allocation ();

	  cond = build_decl (VAR_DECL, NULL_TREE, type_for_mode (word_mode, 1));
	  DECL_RTL (cond) = flag;

	  /* Conditionalize the cleanup.  */
	  cleanup = build (COND_EXPR, void_type_node,
			   truthvalue_conversion (cond),
			   cleanup, integer_zero_node);
	  cleanup = fold (cleanup);

	  pop_obstacks ();

	  cleanups = thisblock->data.block.cleanup_ptr;
	}

      /* All cleanups must be on the function_obstack.  */
      push_obstacks_nochange ();
      resume_temporary_allocation ();
      cleanup = unsave_expr (cleanup);
      pop_obstacks ();

      t = *cleanups = temp_tree_cons (decl, cleanup, *cleanups);

      if (! cond_context)
	/* If this block has a cleanup, it belongs in stack_block_stack.  */
	stack_block_stack = thisblock;

      if (cond_context)
	{
	  start_sequence ();
	}

      /* If this was optimized so that there is no exception region for the
	 cleanup, then mark the TREE_LIST node, so that we can later tell
	 if we need to call expand_eh_region_end.  */
      if (! using_eh_for_cleanups_p
	  || expand_eh_region_start_tree (decl, cleanup))
	TREE_ADDRESSABLE (t) = 1;
      /* If that started a new EH region, we're in a new block.  */
      thisblock = block_stack;

      if (cond_context)
	{
	  seq = get_insns ();
	  end_sequence ();
	  if (seq)
	    thisblock->data.block.last_unconditional_cleanup
	      = emit_insns_after (seq,
				  thisblock->data.block.last_unconditional_cleanup);
	}
      else
	{
	  thisblock->data.block.last_unconditional_cleanup
	    = get_last_insn ();
	  thisblock->data.block.cleanup_ptr = &thisblock->data.block.cleanups;
	}
    }
  return 1;
}

/* Like expand_decl_cleanup, but suppress generating an exception handler
   to perform the cleanup.  */

#if 0
int
expand_decl_cleanup_no_eh (decl, cleanup)
     tree decl, cleanup;
{
  int save_eh = using_eh_for_cleanups_p;
  int result;

  using_eh_for_cleanups_p = 0;
  result = expand_decl_cleanup (decl, cleanup);
  using_eh_for_cleanups_p = save_eh;

  return result;
}
#endif

/* Arrange for the top element of the dynamic cleanup chain to be
   popped if we exit the current binding contour.  DECL is the
   associated declaration, if any, otherwise NULL_TREE.  If the
   current contour is left via an exception, then __sjthrow will pop
   the top element off the dynamic cleanup chain.  The code that
   avoids doing the action we push into the cleanup chain in the
   exceptional case is contained in expand_cleanups.

   This routine is only used by expand_eh_region_start, and that is
   the only way in which an exception region should be started.  This
   routine is only used when using the setjmp/longjmp codegen method
   for exception handling.  */

int
expand_dcc_cleanup (decl)
     tree decl;
{
  struct nesting *thisblock;
  tree cleanup;

  /* Error if we are not in any block.  */
  if (cfun == 0 || block_stack == 0)
    return 0;
  thisblock = block_stack;

  /* Record the cleanup for the dynamic handler chain.  */

  /* All cleanups must be on the function_obstack.  */
  push_obstacks_nochange ();
  resume_temporary_allocation ();
  cleanup = make_node (POPDCC_EXPR);
  pop_obstacks ();

  /* Add the cleanup in a manner similar to expand_decl_cleanup.  */
  thisblock->data.block.cleanups
    = temp_tree_cons (decl, cleanup, thisblock->data.block.cleanups);

  /* If this block has a cleanup, it belongs in stack_block_stack.  */
  stack_block_stack = thisblock;
  return 1;
}

/* Arrange for the top element of the dynamic handler chain to be
   popped if we exit the current binding contour.  DECL is the
   associated declaration, if any, otherwise NULL_TREE.  If the current
   contour is left via an exception, then __sjthrow will pop the top
   element off the dynamic handler chain.  The code that avoids doing
   the action we push into the handler chain in the exceptional case
   is contained in expand_cleanups.

   This routine is only used by expand_eh_region_start, and that is
   the only way in which an exception region should be started.  This
   routine is only used when using the setjmp/longjmp codegen method
   for exception handling.  */

int
expand_dhc_cleanup (decl)
     tree decl;
{
  struct nesting *thisblock;
  tree cleanup;

  /* Error if we are not in any block.  */
  if (cfun == 0 || block_stack == 0)
    return 0;
  thisblock = block_stack;

  /* Record the cleanup for the dynamic handler chain.  */

  /* All cleanups must be on the function_obstack.  */
  push_obstacks_nochange ();
  resume_temporary_allocation ();
  cleanup = make_node (POPDHC_EXPR);
  pop_obstacks ();

  /* Add the cleanup in a manner similar to expand_decl_cleanup.  */
  thisblock->data.block.cleanups
    = temp_tree_cons (decl, cleanup, thisblock->data.block.cleanups);

  /* If this block has a cleanup, it belongs in stack_block_stack.  */
  stack_block_stack = thisblock;
  return 1;
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

      /* Propagate the union's alignment to the elements.  */
      DECL_ALIGN (decl_elt) = DECL_ALIGN (decl);

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
	    DECL_RTL (decl_elt) = x;
	  else
	    {
	      DECL_RTL (decl_elt) = gen_rtx_MEM (mode, copy_rtx (XEXP (x, 0)));
	      MEM_COPY_ATTRIBUTES (DECL_RTL (decl_elt), x);
	      RTX_UNCHANGING_P (DECL_RTL (decl_elt)) = RTX_UNCHANGING_P (x);
	    }
	}
      else if (GET_CODE (x) == REG)
	{
	  if (mode == GET_MODE (x))
	    DECL_RTL (decl_elt) = x;
	  else
	    DECL_RTL (decl_elt) = gen_rtx_SUBREG (mode, x, 0);
	}
      else
	abort ();

      /* Record the cleanup if there is one.  */

      if (cleanup != 0)
	thisblock->data.block.cleanups
	  = temp_tree_cons (decl_elt, cleanup_elt,
			    thisblock->data.block.cleanups);
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
	      {
		tree cleanup = TREE_VALUE (tail);

		/* See expand_d{h,c}c_cleanup for why we avoid this.  */
		if (TREE_CODE (cleanup) != POPDHC_EXPR
		    && TREE_CODE (cleanup) != POPDCC_EXPR
		    /* See expand_eh_region_start_tree for this case.  */
		    && ! TREE_ADDRESSABLE (tail))
		  {
		    cleanup = protect_with_terminate (cleanup);
		    expand_eh_region_end (cleanup);
		  }
	      }

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

		/* We may need to protect fixups with rethrow regions.  */
		int protect = (in_fixup && ! TREE_ADDRESSABLE (tail));

		if (protect)
		  expand_fixup_region_start ();

		/* The cleanup might contain try-blocks, so we have to
		   preserve our current queue.  */
		push_ehqueue ();
		expand_expr (TREE_VALUE (tail), const0_rtx, VOIDmode, 0);
		pop_ehqueue ();
		if (protect)
		  expand_fixup_region_end (TREE_VALUE (tail));
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
  thiscase->data.case_stmt.line_number_status = force_line_numbers ();
  case_stack = thiscase;
  nesting_stack = thiscase;

  do_pending_stack_adjust ();

  /* Make sure case_stmt.start points to something that won't
     need any transformation before expand_end_case.  */
  if (GET_CODE (get_last_insn ()) != NOTE)
    emit_note (NULL_PTR, NOTE_INSN_DELETED);

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
		warning_with_file_and_line (NOTE_SOURCE_FILE(insn),
					    NOTE_LINE_NUMBER(insn),
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
     register tree value;
     tree (*converter) PARAMS ((tree, tree));
     register tree label;
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
    return add_case_node (value, value, label, duplicate);

  expand_label (label);
  return 0;
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
     register tree value1, value2;
     tree (*converter) PARAMS ((tree, tree));
     register tree label;
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

static int
add_case_node (low, high, label, duplicate)
     tree low, high;
     tree label;
     tree *duplicate;
{
  struct case_node *p, **q, *r;

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

  /* Add this label to the chain, and succeed.
     Copy LOW, HIGH so they are on temporary rather than momentary
     obstack and will thus survive till the end of the case statement.  */

  r = (struct case_node *) oballoc (sizeof (struct case_node));
  r->low = copy_node (low);

  /* If the bounds are equal, turn this into the one-value case.  */

  if (tree_int_cst_equal (low, high))
    r->high = r->low;
  else
    {
      r->high = copy_node (high);
      case_stack->data.case_stmt.num_ranges++;
    }

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
  HOST_WIDE_INT count;
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
	  || TYPE_MAX_VALUE (type) == NULL
	  || TREE_CODE (TYPE_MAX_VALUE (type)) != INTEGER_CST)
	return -1;
      else
	{
	  /* count
	     = TREE_INT_CST_LOW (TYPE_MAX_VALUE (type))
	     - TREE_INT_CST_LOW (TYPE_MIN_VALUE (type)) + 1
	     but with overflow checking.  */
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
	      || (TREE_INT_CST_LOW (TYPE_MIN_VALUE (type)) + count
		  != TREE_INT_CST_LOW (TREE_VALUE (t))))
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
     long count;
     int sparseness;
{
  tree next_node_to_try = NULL_TREE;
  long next_node_offset = 0;

  register struct case_node *n, *root = case_stack->data.case_stmt.case_list;
  tree val = make_node (INTEGER_CST);
  TREE_TYPE (val) = type;
  if (! root)
    ; /* Do nothing */
  else if (sparseness == 2)
    {
      tree t;
      HOST_WIDE_INT xlo;

      /* This less efficient loop is only needed to handle
	 duplicate case values (multiple enum constants
	 with the same value).  */
      TREE_TYPE (val) = TREE_TYPE (root->low);
      for (t = TYPE_VALUES (type), xlo = 0;  t != NULL_TREE;
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
	  while ( ! tree_int_cst_lt (n->high, val))
	    {
	      /* Calculate (into xlo) the "offset" of the integer (val).
		 The element with lowest value has offset 0, the next smallest
		 element has offset 1, etc.  */

	      HOST_WIDE_INT xlo, xhi;
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
	      
	      if (xhi == 0 && xlo >= 0 && xlo < count)
		BITARRAY_SET (cases_seen, xlo);
	      add_double (TREE_INT_CST_LOW (val), TREE_INT_CST_HIGH (val),
			  1, 0,
			  &TREE_INT_CST_LOW (val), &TREE_INT_CST_HIGH (val));
	    }
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
  register tree chain;
#if 0  /* variable used by 'if 0'ed  code below. */
  register struct case_node **l;
  int all_values = 1;
#endif

  /* True iff the selector type is a numbered set mode.  */
  int sparseness = 0;

  /* The number of possible selector values.  */
  HOST_WIDE_INT size;

  /* For each possible selector value. a one iff it has been matched
     by a case value alternative.  */
  unsigned char *cases_seen;

  /* The allocated size of cases_seen, in chars.  */
  long bytes_needed;

  if (! warn_switch)
    return;

  size = all_cases_count (type, &sparseness);
  bytes_needed = (size + HOST_BITS_PER_CHAR) / HOST_BITS_PER_CHAR;

  if (size > 0 && size < 600000
      /* We deliberately use calloc here, not cmalloc, so that we can suppress
	 this optimization if we don't have enough memory rather than 
	 aborting, as xmalloc would do.  */
      && (cases_seen = (unsigned char *) calloc (bytes_needed, 1)) != NULL)
    {
      long i;
      tree v = TYPE_VALUES (type);

      /* The time complexity of this code is normally O(N), where
	 N being the number of members in the enumerated type.
	 However, if type is a ENUMERAL_TYPE whose values do not
	 increase monotonically, O(N*log(N)) time may be needed.  */

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
     assignments to enumeration variables.  */

  if (case_stack->data.case_stmt.case_list
      && case_stack->data.case_stmt.case_list->left)
    case_stack->data.case_stmt.case_list
      = case_tree2list (case_stack->data.case_stmt.case_list, 0);
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


/* Terminate a case (Pascal) or switch (C) statement
   in which ORIG_INDEX is the expression to be tested.
   Generate the code to test it and jump to the right place.  */

void
expand_end_case (orig_index)
     tree orig_index;
{
  tree minval = NULL_TREE, maxval = NULL_TREE, range = NULL_TREE, orig_minval;
  rtx default_label = 0;
  register struct case_node *n;
  unsigned int count;
  rtx index;
  rtx table_label;
  int ncases;
  rtx *labelvec;
  register int i;
  rtx before_case;
  register struct nesting *thiscase = case_stack;
  tree index_expr, index_type;
  int unsignedp;

  /* Don't crash due to previous errors.  */
  if (thiscase == NULL)
    return;

  table_label = gen_label_rtx ();
  index_expr = thiscase->data.case_stmt.index_expr;
  index_type = TREE_TYPE (index_expr);
  unsignedp = TREE_UNSIGNED (index_type);

  do_pending_stack_adjust ();

  /* This might get an spurious warning in the presence of a syntax error;
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
      /* If switch expression was an enumerated type, check that all
	 enumeration literals are covered by the cases.
	 No sense trying this if there's a default case, however.  */

      if (!thiscase->data.case_stmt.default_label
	  && TREE_CODE (TREE_TYPE (orig_index)) == ENUMERAL_TYPE
	  && TREE_CODE (index_expr) != INTEGER_CST)
	check_for_full_enumeration_handling (TREE_TYPE (orig_index));

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
	  = case_tree2list(thiscase->data.case_stmt.case_list, 0);

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

      else if (count < CASE_VALUES_THRESHOLD
	       || compare_tree_int (range, 10 * count) > 0
	       /* RANGE may be signed, and really large ranges will show up
		  as negative numbers.  */
	       || compare_tree_int (range, 0) < 0
#ifndef ASM_OUTPUT_ADDR_DIFF_ELT
	       || flag_pic
#endif
	       || TREE_CODE (index_expr) == INTEGER_CST
	       /* These will reduce to a constant.  */
	       || (TREE_CODE (index_expr) == CALL_EXPR
		   && TREE_CODE (TREE_OPERAND (index_expr, 0)) == ADDR_EXPR
		   && TREE_CODE (TREE_OPERAND (TREE_OPERAND (index_expr, 0), 0)) == FUNCTION_DECL
		   && DECL_BUILT_IN_CLASS (TREE_OPERAND (TREE_OPERAND (index_expr, 0), 0)) == BUILT_IN_NORMAL
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
		  emit_cmp_and_jump_insns (rangertx, index, LTU, NULL_RTX,
					   omode, 1, 0, default_label);
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

	      op_mode = insn_data[(int)CODE_FOR_casesi].operand[0].mode;
	      if (! (*insn_data[(int)CODE_FOR_casesi].operand[0].predicate)
		  (index, op_mode))
		index = copy_to_mode_reg (op_mode, index);

	      op1 = expand_expr (minval, NULL_RTX, VOIDmode, 0);

	      op_mode = insn_data[(int)CODE_FOR_casesi].operand[1].mode;
	      if (! (*insn_data[(int)CODE_FOR_casesi].operand[1].predicate)
		  (op1, op_mode))
		op1 = copy_to_mode_reg (op_mode, op1);

	      op2 = expand_expr (range, NULL_RTX, VOIDmode, 0);

	      op_mode = insn_data[(int)CODE_FOR_casesi].operand[2].mode;
	      if (! (*insn_data[(int)CODE_FOR_casesi].operand[2].predicate)
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
		    = gen_rtx_LABEL_REF (Pmode, label_rtx (n->code_label));
		  if (i + TREE_INT_CST_LOW (orig_minval)
		      == TREE_INT_CST_LOW (n->high))
		    break;
		  i++;
		}
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

      before_case = squeeze_notes (NEXT_INSN (before_case), get_last_insn ());
      reorder_insns (before_case, get_last_insn (),
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
      emit_cmp_and_jump_insns (op1, op2, EQ, NULL_RTX, mode, unsignedp,
			       0, label);
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
      cost_table = cost_table_ + 1;

      for (i = 0; i < 128; i++)
	{
	  if (ISALNUM (i))
	    cost_table[i] = 16;
	  else if (ISPUNCT (i))
	    cost_table[i] = 8;
	  else if (ISCNTRL (i))
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

      for (i = (HOST_WIDE_INT) TREE_INT_CST_LOW (n->low);
	   i <= (HOST_WIDE_INT) TREE_INT_CST_LOW (n->high); i++)
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

  /* See if our parents have already tested everything for us.
     If they have, emit an unconditional jump for this node.  */
  if (node_is_bounded (node, index_type))
    emit_jump (label_rtx (node->code_label));

  else if (tree_int_cst_equal (node->low, node->high))
    {
      /* Node is single valued.  First see if the index expression matches
	 this node and then check our children, if any.  */

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
	      emit_cmp_and_jump_insns (index, expand_expr (node->high, NULL_RTX,
							   VOIDmode, 0),
				        GT, NULL_RTX, mode, unsignedp, 0,
					label_rtx (node->right->code_label));
	      emit_case_nodes (index, node->left, default_label, index_type);
	    }

	  else if (node_is_bounded (node->left, index_type))
	    {
	      emit_cmp_and_jump_insns (index, expand_expr (node->high, NULL_RTX,
							   VOIDmode, 0),
				       LT, NULL_RTX, mode, unsignedp, 0,
				       label_rtx (node->left->code_label));
	      emit_case_nodes (index, node->right, default_label, index_type);
	    }

	  else
	    {
	      /* Neither node is bounded.  First distinguish the two sides;
		 then emit the code for one side at a time.  */

	      tree test_label
		= build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

	      /* See if the value is on the right.  */
	      emit_cmp_and_jump_insns (index, expand_expr (node->high, NULL_RTX,
							   VOIDmode, 0),
				       GT, NULL_RTX, mode, unsignedp, 0,
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
		  emit_cmp_and_jump_insns (index, expand_expr (node->high,
							       NULL_RTX,
							       VOIDmode, 0),
					   LT, NULL_RTX, mode, unsignedp, 0,
					   default_label);
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
		  emit_cmp_and_jump_insns (index, expand_expr (node->high,
							       NULL_RTX,
							       VOIDmode, 0),
					   GT, NULL_RTX, mode, unsignedp, 0,
					   default_label);
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


	  if (node_is_bounded (node->right, index_type))
	    /* Right hand node is fully bounded so we can eliminate any
	       testing and branch directly to the target code.  */
	    emit_cmp_and_jump_insns (index, expand_expr (node->high, NULL_RTX,
							 VOIDmode, 0),
				     GT, NULL_RTX, mode, unsignedp, 0,
				     label_rtx (node->right->code_label));
	  else
	    {
	      /* Right hand node requires testing.
		 Branch to a label where we will handle it later.  */

	      test_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
	      emit_cmp_and_jump_insns (index, expand_expr (node->high, NULL_RTX,
							   VOIDmode, 0),
				       GT, NULL_RTX, mode, unsignedp, 0,
				       label_rtx (test_label));
	    }

	  /* Value belongs to this node or to the left-hand subtree.  */

	  emit_cmp_and_jump_insns (index, expand_expr (node->low, NULL_RTX,
						       VOIDmode, 0),
				   GE, NULL_RTX, mode, unsignedp, 0,
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
	      emit_cmp_and_jump_insns (index, expand_expr (node->low, NULL_RTX,
							   VOIDmode, 0),
				       LT, NULL_RTX, mode, unsignedp, 0,
				       default_label);
	    }

	  /* Value belongs to this node or to the right-hand subtree.  */

	  emit_cmp_and_jump_insns (index, expand_expr (node->high, NULL_RTX,
						       VOIDmode, 0),
				   LE, NULL_RTX, mode, unsignedp, 0,
				   label_rtx (node->code_label));

	  emit_case_nodes (index, node->right, default_label, index_type);
	}

      else if (node->right == 0 && node->left != 0)
	{
	  /* Deal with values to the right of this node,
	     if they are possible.  */
	  if (!node_has_high_bound (node, index_type))
	    {
	      emit_cmp_and_jump_insns (index, expand_expr (node->high, NULL_RTX,
							   VOIDmode, 0),
				       GT, NULL_RTX, mode, unsignedp, 0,
				       default_label);
	    }

	  /* Value belongs to this node or to the left-hand subtree.  */

	  emit_cmp_and_jump_insns (index, expand_expr (node->low, NULL_RTX,
						       VOIDmode, 0),
				   GE, NULL_RTX, mode, unsignedp, 0,
				   label_rtx (node->code_label));

	  emit_case_nodes (index, node->left, default_label, index_type);
	}

      else
	{
	  /* Node has no children so we check low and high bounds to remove
	     redundant tests.  Only one of the bounds can exist,
	     since otherwise this node is bounded--a case tested already.  */

	  if (!node_has_high_bound (node, index_type))
	    {
	      emit_cmp_and_jump_insns (index, expand_expr (node->high, NULL_RTX,
							   VOIDmode, 0),
				       GT, NULL_RTX, mode, unsignedp, 0,
				       default_label);
	    }

	  if (!node_has_low_bound (node, index_type))
	    {
	      emit_cmp_and_jump_insns (index, expand_expr (node->low, NULL_RTX,
							   VOIDmode, 0),
				       LT, NULL_RTX, mode, unsignedp, 0,
				       default_label);
	    }

	  emit_jump (label_rtx (node->code_label));
	}
    }
}

/* These routines are used by the loop unrolling code.  They copy BLOCK trees
   so that the debugging info will be correct for the unrolled loop.  */

void
find_loop_tree_blocks ()
{
  identify_blocks (DECL_INITIAL (current_function_decl), get_insns ());
}

void
unroll_block_trees ()
{
  tree block = DECL_INITIAL (current_function_decl);

  reorder_blocks (block, get_insns ());
}
