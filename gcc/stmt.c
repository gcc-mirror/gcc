/* Expands front end tree to back end RTL for GCC
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This file handles the generation of rtl code from tree structure
   above the level of expressions, using subroutines in exp*.c and emit-rtl.c.
   The functions whose names start with `expand_' are called by the
   expander to generate RTL instructions for various kinds of constructs.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "alloc-pool.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "pretty-print.h"
#include "diagnostic-core.h"

#include "fold-const.h"
#include "varasm.h"
#include "stor-layout.h"
#include "dojump.h"
#include "explow.h"
#include "stmt.h"
#include "expr.h"
#include "langhooks.h"
#include "cfganal.h"
#include "params.h"
#include "dumpfile.h"
#include "builtins.h"


/* Functions and data structures for expanding case statements.  */

/* Case label structure, used to hold info on labels within case
   statements.  We handle "range" labels; for a single-value label
   as in C, the high and low limits are the same.

   We start with a vector of case nodes sorted in ascending order, and
   the default label as the last element in the vector.  Before expanding
   to RTL, we transform this vector into a list linked via the RIGHT
   fields in the case_node struct.  Nodes with higher case values are
   later in the list.

   Switch statements can be output in three forms.  A branch table is
   used if there are more than a few labels and the labels are dense
   within the range between the smallest and largest case value.  If a
   branch table is used, no further manipulations are done with the case
   node chain.

   The alternative to the use of a branch table is to generate a series
   of compare and jump insns.  When that is done, we use the LEFT, RIGHT,
   and PARENT fields to hold a binary tree.  Initially the tree is
   totally unbalanced, with everything on the right.  We balance the tree
   with nodes on the left having lower case values than the parent
   and nodes on the right having higher values.  We then output the tree
   in order.

   For very small, suitable switch statements, we can generate a series
   of simple bit test and branches instead.  */

struct case_node
{
  struct case_node	*left;	/* Left son in binary tree */
  struct case_node	*right;	/* Right son in binary tree; also node chain */
  struct case_node	*parent; /* Parent of node in binary tree */
  tree			low;	/* Lowest index value for this label */
  tree			high;	/* Highest index value for this label */
  tree			code_label; /* Label to jump to when node matches */
  int                   prob; /* Probability of taking this case.  */
  /* Probability of reaching subtree rooted at this node */
  int                   subtree_prob;
};

typedef struct case_node *case_node_ptr;

extern basic_block label_to_block_fn (struct function *, tree);

static bool check_unique_operand_names (tree, tree, tree);
static char *resolve_operand_name_1 (char *, tree, tree, tree);
static void balance_case_nodes (case_node_ptr *, case_node_ptr);
static int node_has_low_bound (case_node_ptr, tree);
static int node_has_high_bound (case_node_ptr, tree);
static int node_is_bounded (case_node_ptr, tree);
static void emit_case_nodes (rtx, case_node_ptr, rtx_code_label *, int, tree);

/* Return the rtx-label that corresponds to a LABEL_DECL,
   creating it if necessary.  */

rtx_insn *
label_rtx (tree label)
{
  gcc_assert (TREE_CODE (label) == LABEL_DECL);

  if (!DECL_RTL_SET_P (label))
    {
      rtx_code_label *r = gen_label_rtx ();
      SET_DECL_RTL (label, r);
      if (FORCED_LABEL (label) || DECL_NONLOCAL (label))
	LABEL_PRESERVE_P (r) = 1;
    }

  return as_a <rtx_insn *> (DECL_RTL (label));
}

/* As above, but also put it on the forced-reference list of the
   function that contains it.  */
rtx_insn *
force_label_rtx (tree label)
{
  rtx_insn *ref = label_rtx (label);
  tree function = decl_function_context (label);

  gcc_assert (function);

  vec_safe_push (forced_labels, ref);
  return ref;
}

/* As label_rtx, but ensures (in check build), that returned value is
   an existing label (i.e. rtx with code CODE_LABEL).  */
rtx_code_label *
jump_target_rtx (tree label)
{
  return as_a <rtx_code_label *> (label_rtx (label));
}

/* Add an unconditional jump to LABEL as the next sequential instruction.  */

void
emit_jump (rtx label)
{
  do_pending_stack_adjust ();
  emit_jump_insn (targetm.gen_jump (label));
  emit_barrier ();
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
expand_label (tree label)
{
  rtx_code_label *label_r = jump_target_rtx (label);

  do_pending_stack_adjust ();
  emit_label (label_r);
  if (DECL_NAME (label))
    LABEL_NAME (DECL_RTL (label)) = IDENTIFIER_POINTER (DECL_NAME (label));

  if (DECL_NONLOCAL (label))
    {
      expand_builtin_setjmp_receiver (NULL);
      nonlocal_goto_handler_labels
	= gen_rtx_INSN_LIST (VOIDmode, label_r,
			     nonlocal_goto_handler_labels);
    }

  if (FORCED_LABEL (label))
    vec_safe_push<rtx_insn *> (forced_labels, label_r);

  if (DECL_NONLOCAL (label) || FORCED_LABEL (label))
    maybe_set_first_label_num (label_r);
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
   replaced with `=' as part of this process.)

   Returns TRUE if all went well; FALSE if an error occurred.  */

bool
parse_output_constraint (const char **constraint_p, int operand_num,
			 int ninputs, int noutputs, bool *allows_mem,
			 bool *allows_reg, bool *is_inout)
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
      error ("output operand constraint lacks %<=%>");
      return false;
    }

  /* If the constraint begins with `+', then the operand is both read
     from and written to.  */
  *is_inout = (*p == '+');

  /* Canonicalize the output constraint so that it begins with `='.  */
  if (p != constraint || *is_inout)
    {
      char *buf;
      size_t c_len = strlen (constraint);

      if (p != constraint)
	warning (0, "output constraint %qc for operand %d "
		 "is not at the beginning",
		 *p, operand_num);

      /* Make a copy of the constraint.  */
      buf = XALLOCAVEC (char, c_len + 1);
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
  for (p = constraint + 1; *p; p += CONSTRAINT_LEN (*p, p))
    switch (*p)
      {
      case '+':
      case '=':
	error ("operand constraint contains incorrectly positioned "
	       "%<+%> or %<=%>");
	return false;

      case '%':
	if (operand_num + 1 == ninputs + noutputs)
	  {
	    error ("%<%%%> constraint used with last operand");
	    return false;
	  }
	break;

      case '?':  case '!':  case '*':  case '&':  case '#':
      case '$':  case '^':
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

      default:
	if (!ISALPHA (*p))
	  break;
	enum constraint_num cn = lookup_constraint (p);
	if (reg_class_for_constraint (cn) != NO_REGS
	    || insn_extra_address_constraint (cn))
	  *allows_reg = true;
	else if (insn_extra_memory_constraint (cn))
	  *allows_mem = true;
	else
	  insn_extra_constraint_allows_reg_mem (cn, allows_reg, allows_mem);
	break;
      }

  return true;
}

/* Similar, but for input constraints.  */

bool
parse_input_constraint (const char **constraint_p, int input_num,
			int ninputs, int noutputs, int ninout,
			const char * const * constraints,
			bool *allows_mem, bool *allows_reg)
{
  const char *constraint = *constraint_p;
  const char *orig_constraint = constraint;
  size_t c_len = strlen (constraint);
  size_t j;
  bool saw_match = false;

  /* Assume the constraint doesn't allow the use of either
     a register or memory.  */
  *allows_mem = false;
  *allows_reg = false;

  /* Make sure constraint has neither `=', `+', nor '&'.  */

  for (j = 0; j < c_len; j += CONSTRAINT_LEN (constraint[j], constraint+j))
    switch (constraint[j])
      {
      case '+':  case '=':  case '&':
	if (constraint == orig_constraint)
	  {
	    error ("input operand constraint contains %qc", constraint[j]);
	    return false;
	  }
	break;

      case '%':
	if (constraint == orig_constraint
	    && input_num + 1 == ninputs - ninout)
	  {
	    error ("%<%%%> constraint used with last operand");
	    return false;
	  }
	break;

      case '<':  case '>':
      case '?':  case '!':  case '*':  case '#':
      case '$':  case '^':
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

	  saw_match = true;

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
	      /* ??? At the end of the loop, we will skip the first part of
		 the matched constraint.  This assumes not only that the
		 other constraint is an output constraint, but also that
		 the '=' or '+' come first.  */
	      break;
	    }
	  else
	    j = end - constraint;
	  /* Anticipate increment at end of loop.  */
	  j--;
	}
	/* Fall through.  */

      case 'g':  case 'X':
	*allows_reg = true;
	*allows_mem = true;
	break;

      default:
	if (! ISALPHA (constraint[j]))
	  {
	    error ("invalid punctuation %qc in constraint", constraint[j]);
	    return false;
	  }
	enum constraint_num cn = lookup_constraint (constraint + j);
	if (reg_class_for_constraint (cn) != NO_REGS
	    || insn_extra_address_constraint (cn))
	  *allows_reg = true;
	else if (insn_extra_memory_constraint (cn)
		 || insn_extra_special_memory_constraint (cn))
	  *allows_mem = true;
	else
	  insn_extra_constraint_allows_reg_mem (cn, allows_reg, allows_mem);
	break;
      }

  if (saw_match && !*allows_reg)
    warning (0, "matching constraint does not allow a register");

  return true;
}

/* Return DECL iff there's an overlap between *REGS and DECL, where DECL
   can be an asm-declared register.  Called via walk_tree.  */

static tree
decl_overlaps_hard_reg_set_p (tree *declp, int *walk_subtrees ATTRIBUTE_UNUSED,
			      void *data)
{
  tree decl = *declp;
  const HARD_REG_SET *const regs = (const HARD_REG_SET *) data;

  if (VAR_P (decl))
    {
      if (DECL_HARD_REGISTER (decl)
	  && REG_P (DECL_RTL (decl))
	  && REGNO (DECL_RTL (decl)) < FIRST_PSEUDO_REGISTER)
	{
	  rtx reg = DECL_RTL (decl);

	  if (overlaps_hard_reg_set_p (*regs, GET_MODE (reg), REGNO (reg)))
	    return decl;
	}
      walk_subtrees = 0;
    }
  else if (TYPE_P (decl) || TREE_CODE (decl) == PARM_DECL)
    walk_subtrees = 0;
  return NULL_TREE;
}

/* If there is an overlap between *REGS and DECL, return the first overlap
   found.  */
tree
tree_overlaps_hard_reg_set (tree decl, HARD_REG_SET *regs)
{
  return walk_tree (&decl, decl_overlaps_hard_reg_set_p, regs, NULL);
}


/* A subroutine of expand_asm_operands.  Check that all operand names
   are unique.  Return true if so.  We rely on the fact that these names
   are identifiers, and so have been canonicalized by get_identifier,
   so all we need are pointer comparisons.  */

static bool
check_unique_operand_names (tree outputs, tree inputs, tree labels)
{
  tree i, j, i_name = NULL_TREE;

  for (i = outputs; i ; i = TREE_CHAIN (i))
    {
      i_name = TREE_PURPOSE (TREE_PURPOSE (i));
      if (! i_name)
	continue;

      for (j = TREE_CHAIN (i); j ; j = TREE_CHAIN (j))
	if (simple_cst_equal (i_name, TREE_PURPOSE (TREE_PURPOSE (j))))
	  goto failure;
    }

  for (i = inputs; i ; i = TREE_CHAIN (i))
    {
      i_name = TREE_PURPOSE (TREE_PURPOSE (i));
      if (! i_name)
	continue;

      for (j = TREE_CHAIN (i); j ; j = TREE_CHAIN (j))
	if (simple_cst_equal (i_name, TREE_PURPOSE (TREE_PURPOSE (j))))
	  goto failure;
      for (j = outputs; j ; j = TREE_CHAIN (j))
	if (simple_cst_equal (i_name, TREE_PURPOSE (TREE_PURPOSE (j))))
	  goto failure;
    }

  for (i = labels; i ; i = TREE_CHAIN (i))
    {
      i_name = TREE_PURPOSE (i);
      if (! i_name)
	continue;

      for (j = TREE_CHAIN (i); j ; j = TREE_CHAIN (j))
	if (simple_cst_equal (i_name, TREE_PURPOSE (j)))
	  goto failure;
      for (j = inputs; j ; j = TREE_CHAIN (j))
	if (simple_cst_equal (i_name, TREE_PURPOSE (TREE_PURPOSE (j))))
	  goto failure;
    }

  return true;

 failure:
  error ("duplicate asm operand name %qs", TREE_STRING_POINTER (i_name));
  return false;
}

/* Resolve the names of the operands in *POUTPUTS and *PINPUTS to numbers,
   and replace the name expansions in STRING and in the constraints to
   those numbers.  This is generally done in the front end while creating
   the ASM_EXPR generic tree that eventually becomes the GIMPLE_ASM.  */

tree
resolve_asm_operand_names (tree string, tree outputs, tree inputs, tree labels)
{
  char *buffer;
  char *p;
  const char *c;
  tree t;

  check_unique_operand_names (outputs, inputs, labels);

  /* Substitute [<name>] in input constraint strings.  There should be no
     named operands in output constraints.  */
  for (t = inputs; t ; t = TREE_CHAIN (t))
    {
      c = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));
      if (strchr (c, '[') != NULL)
	{
	  p = buffer = xstrdup (c);
	  while ((p = strchr (p, '[')) != NULL)
	    p = resolve_operand_name_1 (p, outputs, inputs, NULL);
	  TREE_VALUE (TREE_PURPOSE (t))
	    = build_string (strlen (buffer), buffer);
	  free (buffer);
	}
    }

  /* Now check for any needed substitutions in the template.  */
  c = TREE_STRING_POINTER (string);
  while ((c = strchr (c, '%')) != NULL)
    {
      if (c[1] == '[')
	break;
      else if (ISALPHA (c[1]) && c[2] == '[')
	break;
      else
	{
	  c += 1 + (c[1] == '%');
	  continue;
	}
    }

  if (c)
    {
      /* OK, we need to make a copy so we can perform the substitutions.
	 Assume that we will not need extra space--we get to remove '['
	 and ']', which means we cannot have a problem until we have more
	 than 999 operands.  */
      buffer = xstrdup (TREE_STRING_POINTER (string));
      p = buffer + (c - TREE_STRING_POINTER (string));

      while ((p = strchr (p, '%')) != NULL)
	{
	  if (p[1] == '[')
	    p += 1;
	  else if (ISALPHA (p[1]) && p[2] == '[')
	    p += 2;
	  else
	    {
	      p += 1 + (p[1] == '%');
	      continue;
	    }

	  p = resolve_operand_name_1 (p, outputs, inputs, labels);
	}

      string = build_string (strlen (buffer), buffer);
      free (buffer);
    }

  return string;
}

/* A subroutine of resolve_operand_names.  P points to the '[' for a
   potential named operand of the form [<name>].  In place, replace
   the name and brackets with a number.  Return a pointer to the
   balance of the string after substitution.  */

static char *
resolve_operand_name_1 (char *p, tree outputs, tree inputs, tree labels)
{
  char *q;
  int op;
  tree t;

  /* Collect the operand name.  */
  q = strchr (++p, ']');
  if (!q)
    {
      error ("missing close brace for named operand");
      return strchr (p, '\0');
    }
  *q = '\0';

  /* Resolve the name to a number.  */
  for (op = 0, t = outputs; t ; t = TREE_CHAIN (t), op++)
    {
      tree name = TREE_PURPOSE (TREE_PURPOSE (t));
      if (name && strcmp (TREE_STRING_POINTER (name), p) == 0)
	goto found;
    }
  for (t = inputs; t ; t = TREE_CHAIN (t), op++)
    {
      tree name = TREE_PURPOSE (TREE_PURPOSE (t));
      if (name && strcmp (TREE_STRING_POINTER (name), p) == 0)
	goto found;
    }
  for (t = labels; t ; t = TREE_CHAIN (t), op++)
    {
      tree name = TREE_PURPOSE (t);
      if (name && strcmp (TREE_STRING_POINTER (name), p) == 0)
	goto found;
    }

  error ("undefined named operand %qs", identifier_to_locale (p));
  op = 0;

 found:
  /* Replace the name with the number.  Unfortunately, not all libraries
     get the return value of sprintf correct, so search for the end of the
     generated string by hand.  */
  sprintf (--p, "%d", op);
  p = strchr (p, '\0');

  /* Verify the no extra buffer space assumption.  */
  gcc_assert (p <= q);

  /* Shift the rest of the buffer down to fill the gap.  */
  memmove (p, q + 1, strlen (q + 1) + 1);

  return p;
}


/* Generate RTL to return directly from the current function.
   (That is, we bypass any return value.)  */

void
expand_naked_return (void)
{
  rtx_code_label *end_label;

  clear_pending_stack_adjust ();
  do_pending_stack_adjust ();

  end_label = naked_return_label;
  if (end_label == 0)
    end_label = naked_return_label = gen_label_rtx ();

  emit_jump (end_label);
}

/* Generate code to jump to LABEL if OP0 and OP1 are equal in mode MODE. PROB
   is the probability of jumping to LABEL.  */
static void
do_jump_if_equal (machine_mode mode, rtx op0, rtx op1, rtx_code_label *label,
		  int unsignedp, int prob)
{
  gcc_assert (prob <= REG_BR_PROB_BASE);
  do_compare_rtx_and_jump (op0, op1, EQ, unsignedp, mode,
			   NULL_RTX, NULL, label, prob);
}

/* Do the insertion of a case label into case_list.  The labels are
   fed to us in descending order from the sorted vector of case labels used
   in the tree part of the middle end.  So the list we construct is
   sorted in ascending order.
   
   LABEL is the case label to be inserted. LOW and HIGH are the bounds
   against which the index is compared to jump to LABEL and PROB is the
   estimated probability LABEL is reached from the switch statement.  */

static struct case_node *
add_case_node (struct case_node *head, tree low, tree high,
	       tree label, int prob,
	       object_allocator<case_node> &case_node_pool)
{
  struct case_node *r;

  gcc_checking_assert (low);
  gcc_checking_assert (high && (TREE_TYPE (low) == TREE_TYPE (high)));

  /* Add this label to the chain.  */
  r = case_node_pool.allocate ();
  r->low = low;
  r->high = high;
  r->code_label = label;
  r->parent = r->left = NULL;
  r->prob = prob;
  r->subtree_prob = prob;
  r->right = head;
  return r;
}

/* Dump ROOT, a list or tree of case nodes, to file.  */

static void
dump_case_nodes (FILE *f, struct case_node *root,
		 int indent_step, int indent_level)
{
  if (root == 0)
    return;
  indent_level++;

  dump_case_nodes (f, root->left, indent_step, indent_level);

  fputs (";; ", f);
  fprintf (f, "%*s", indent_step * indent_level, "");
  print_dec (root->low, f, TYPE_SIGN (TREE_TYPE (root->low)));
  if (!tree_int_cst_equal (root->low, root->high))
    {
      fprintf (f, " ... ");
      print_dec (root->high, f, TYPE_SIGN (TREE_TYPE (root->high)));
    }
  fputs ("\n", f);

  dump_case_nodes (f, root->right, indent_step, indent_level);
}

/* Return the smallest number of different values for which it is best to use a
   jump-table instead of a tree of conditional branches.  */

static unsigned int
case_values_threshold (void)
{
  unsigned int threshold = PARAM_VALUE (PARAM_CASE_VALUES_THRESHOLD);

  if (threshold == 0)
    threshold = targetm.case_values_threshold ();

  return threshold;
}

/* Return true if a switch should be expanded as a decision tree.
   RANGE is the difference between highest and lowest case.
   UNIQ is number of unique case node targets, not counting the default case.
   COUNT is the number of comparisons needed, not counting the default case.  */

static bool
expand_switch_as_decision_tree_p (tree range,
				  unsigned int uniq ATTRIBUTE_UNUSED,
				  unsigned int count)
{
  int max_ratio;

  /* If neither casesi or tablejump is available, or flag_jump_tables
     over-ruled us, we really have no choice.  */
  if (!targetm.have_casesi () && !targetm.have_tablejump ())
    return true;
  if (!flag_jump_tables)
    return true;
#ifndef ASM_OUTPUT_ADDR_DIFF_ELT
  if (flag_pic)
    return true;
#endif

  /* If the switch is relatively small such that the cost of one
     indirect jump on the target are higher than the cost of a
     decision tree, go with the decision tree.

     If range of values is much bigger than number of values,
     or if it is too large to represent in a HOST_WIDE_INT,
     make a sequence of conditional branches instead of a dispatch.

     The definition of "much bigger" depends on whether we are
     optimizing for size or for speed.  If the former, the maximum
     ratio range/count = 3, because this was found to be the optimal
     ratio for size on i686-pc-linux-gnu, see PR11823.  The ratio
     10 is much older, and was probably selected after an extensive
     benchmarking investigation on numerous platforms.  Or maybe it
     just made sense to someone at some point in the history of GCC,
     who knows...  */
  max_ratio = optimize_insn_for_size_p () ? 3 : 10;
  if (count < case_values_threshold ()
      || ! tree_fits_uhwi_p (range)
      || compare_tree_int (range, max_ratio * count) > 0)
    return true;

  return false;
}

/* Generate a decision tree, switching on INDEX_EXPR and jumping to
   one of the labels in CASE_LIST or to the DEFAULT_LABEL.
   DEFAULT_PROB is the estimated probability that it jumps to
   DEFAULT_LABEL.
   
   We generate a binary decision tree to select the appropriate target
   code.  This is done as follows:

     If the index is a short or char that we do not have
     an insn to handle comparisons directly, convert it to
     a full integer now, rather than letting each comparison
     generate the conversion.

     Load the index into a register.

     The list of cases is rearranged into a binary tree,
     nearly optimal assuming equal probability for each case.

     The tree is transformed into RTL, eliminating redundant
     test conditions at the same time.

     If program flow could reach the end of the decision tree
     an unconditional jump to the default code is emitted.

   The above process is unaware of the CFG.  The caller has to fix up
   the CFG itself.  This is done in cfgexpand.c.  */     

static void
emit_case_decision_tree (tree index_expr, tree index_type,
			 case_node_ptr case_list, rtx_code_label *default_label,
			 int default_prob)
{
  rtx index = expand_normal (index_expr);

  if (GET_MODE_CLASS (GET_MODE (index)) == MODE_INT
      && ! have_insn_for (COMPARE, GET_MODE (index)))
    {
      int unsignedp = TYPE_UNSIGNED (index_type);
      machine_mode wider_mode;
      for (wider_mode = GET_MODE (index); wider_mode != VOIDmode;
	   wider_mode = GET_MODE_WIDER_MODE (wider_mode))
	if (have_insn_for (COMPARE, wider_mode))
	  {
	    index = convert_to_mode (wider_mode, index, unsignedp);
	    break;
	  }
    }

  do_pending_stack_adjust ();

  if (MEM_P (index))
    {
      index = copy_to_reg (index);
      if (TREE_CODE (index_expr) == SSA_NAME)
	set_reg_attrs_for_decl_rtl (index_expr, index);
    }

  balance_case_nodes (&case_list, NULL);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      int indent_step = ceil_log2 (TYPE_PRECISION (index_type)) + 2;
      fprintf (dump_file, ";; Expanding GIMPLE switch as decision tree:\n");
      dump_case_nodes (dump_file, case_list, indent_step, 0);
    }

  emit_case_nodes (index, case_list, default_label, default_prob, index_type);
  if (default_label)
    emit_jump (default_label);
}

/* Return the sum of probabilities of outgoing edges of basic block BB.  */

static int
get_outgoing_edge_probs (basic_block bb)
{
  edge e;
  edge_iterator ei;
  int prob_sum = 0;
  if (!bb)
    return 0;
  FOR_EACH_EDGE (e, ei, bb->succs)
    prob_sum += e->probability;
  return prob_sum;
}

/* Computes the conditional probability of jumping to a target if the branch
   instruction is executed.
   TARGET_PROB is the estimated probability of jumping to a target relative
   to some basic block BB.
   BASE_PROB is the probability of reaching the branch instruction relative
   to the same basic block BB.  */

static inline int
conditional_probability (int target_prob, int base_prob)
{
  if (base_prob > 0)
    {
      gcc_assert (target_prob >= 0);
      gcc_assert (target_prob <= base_prob);
      return GCOV_COMPUTE_SCALE (target_prob, base_prob);
    }
  return -1;
}

/* Generate a dispatch tabler, switching on INDEX_EXPR and jumping to
   one of the labels in CASE_LIST or to the DEFAULT_LABEL.
   MINVAL, MAXVAL, and RANGE are the extrema and range of the case
   labels in CASE_LIST. STMT_BB is the basic block containing the statement.

   First, a jump insn is emitted.  First we try "casesi".  If that
   fails, try "tablejump".   A target *must* have one of them (or both).

   Then, a table with the target labels is emitted.

   The process is unaware of the CFG.  The caller has to fix up
   the CFG itself.  This is done in cfgexpand.c.  */     

static void
emit_case_dispatch_table (tree index_expr, tree index_type,
			  struct case_node *case_list, rtx default_label,
			  tree minval, tree maxval, tree range,
                          basic_block stmt_bb)
{
  int i, ncases;
  struct case_node *n;
  rtx *labelvec;
  rtx_insn *fallback_label = label_rtx (case_list->code_label);
  rtx_code_label *table_label = gen_label_rtx ();
  bool has_gaps = false;
  edge default_edge = stmt_bb ? EDGE_SUCC (stmt_bb, 0) : NULL;
  int default_prob = default_edge ? default_edge->probability : 0;
  int base = get_outgoing_edge_probs (stmt_bb);
  bool try_with_tablejump = false;

  int new_default_prob = conditional_probability (default_prob,
                                                  base);

  if (! try_casesi (index_type, index_expr, minval, range,
		    table_label, default_label, fallback_label,
                    new_default_prob))
    {
      /* Index jumptables from zero for suitable values of minval to avoid
	 a subtraction.  For the rationale see:
	 "http://gcc.gnu.org/ml/gcc-patches/2001-10/msg01234.html".  */
      if (optimize_insn_for_speed_p ()
	  && compare_tree_int (minval, 0) > 0
	  && compare_tree_int (minval, 3) < 0)
	{
	  minval = build_int_cst (index_type, 0);
	  range = maxval;
          has_gaps = true;
	}
      try_with_tablejump = true;
    }

  /* Get table of labels to jump to, in order of case index.  */

  ncases = tree_to_shwi (range) + 1;
  labelvec = XALLOCAVEC (rtx, ncases);
  memset (labelvec, 0, ncases * sizeof (rtx));

  for (n = case_list; n; n = n->right)
    {
      /* Compute the low and high bounds relative to the minimum
	 value since that should fit in a HOST_WIDE_INT while the
	 actual values may not.  */
      HOST_WIDE_INT i_low
	= tree_to_uhwi (fold_build2 (MINUS_EXPR, index_type,
				     n->low, minval));
      HOST_WIDE_INT i_high
	= tree_to_uhwi (fold_build2 (MINUS_EXPR, index_type,
				     n->high, minval));
      HOST_WIDE_INT i;

      for (i = i_low; i <= i_high; i ++)
	labelvec[i]
	  = gen_rtx_LABEL_REF (Pmode, label_rtx (n->code_label));
    }

  /* Fill in the gaps with the default.  We may have gaps at
     the beginning if we tried to avoid the minval subtraction,
     so substitute some label even if the default label was
     deemed unreachable.  */
  if (!default_label)
    default_label = fallback_label;
  for (i = 0; i < ncases; i++)
    if (labelvec[i] == 0)
      {
        has_gaps = true;
        labelvec[i] = gen_rtx_LABEL_REF (Pmode, default_label);
      }

  if (has_gaps)
    {
      /* There is at least one entry in the jump table that jumps
         to default label. The default label can either be reached
         through the indirect jump or the direct conditional jump
         before that. Split the probability of reaching the
         default label among these two jumps.  */
      new_default_prob = conditional_probability (default_prob/2,
                                                  base);
      default_prob /= 2;
      base -= default_prob;
    }
  else
    {
      base -= default_prob;
      default_prob = 0;
    }

  if (default_edge)
    default_edge->probability = default_prob;

  /* We have altered the probability of the default edge. So the probabilities
     of all other edges need to be adjusted so that it sums up to
     REG_BR_PROB_BASE.  */
  if (base)
    {
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, stmt_bb->succs)
        e->probability = GCOV_COMPUTE_SCALE (e->probability,  base);
    }

  if (try_with_tablejump)
    {
      bool ok = try_tablejump (index_type, index_expr, minval, range,
                               table_label, default_label, new_default_prob);
      gcc_assert (ok);
    }
  /* Output the table.  */
  emit_label (table_label);

  if (CASE_VECTOR_PC_RELATIVE || flag_pic)
    emit_jump_table_data (gen_rtx_ADDR_DIFF_VEC (CASE_VECTOR_MODE,
						 gen_rtx_LABEL_REF (Pmode,
								    table_label),
						 gen_rtvec_v (ncases, labelvec),
						 const0_rtx, const0_rtx));
  else
    emit_jump_table_data (gen_rtx_ADDR_VEC (CASE_VECTOR_MODE,
					    gen_rtvec_v (ncases, labelvec)));

  /* Record no drop-through after the table.  */
  emit_barrier ();
}

/* Reset the aux field of all outgoing edges of basic block BB.  */

static inline void
reset_out_edges_aux (basic_block bb)
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->succs)
    e->aux = (void *)0;
}

/* Compute the number of case labels that correspond to each outgoing edge of
   STMT. Record this information in the aux field of the edge.  */

static inline void
compute_cases_per_edge (gswitch *stmt)
{
  basic_block bb = gimple_bb (stmt);
  reset_out_edges_aux (bb);
  int ncases = gimple_switch_num_labels (stmt);
  for (int i = ncases - 1; i >= 1; --i)
    {
      tree elt = gimple_switch_label (stmt, i);
      tree lab = CASE_LABEL (elt);
      basic_block case_bb = label_to_block_fn (cfun, lab);
      edge case_edge = find_edge (bb, case_bb);
      case_edge->aux = (void *)((intptr_t)(case_edge->aux) + 1);
    }
}

/* Terminate a case (Pascal/Ada) or switch (C) statement
   in which ORIG_INDEX is the expression to be tested.
   If ORIG_TYPE is not NULL, it is the original ORIG_INDEX
   type as given in the source before any compiler conversions.
   Generate the code to test it and jump to the right place.  */

void
expand_case (gswitch *stmt)
{
  tree minval = NULL_TREE, maxval = NULL_TREE, range = NULL_TREE;
  rtx_code_label *default_label = NULL;
  unsigned int count, uniq;
  int i;
  int ncases = gimple_switch_num_labels (stmt);
  tree index_expr = gimple_switch_index (stmt);
  tree index_type = TREE_TYPE (index_expr);
  tree elt;
  basic_block bb = gimple_bb (stmt);

  /* A list of case labels; it is first built as a list and it may then
     be rearranged into a nearly balanced binary tree.  */
  struct case_node *case_list = 0;

  /* A pool for case nodes.  */
  object_allocator<case_node> case_node_pool ("struct case_node pool");

  /* An ERROR_MARK occurs for various reasons including invalid data type.
     ??? Can this still happen, with GIMPLE and all?  */
  if (index_type == error_mark_node)
    return;

  /* cleanup_tree_cfg removes all SWITCH_EXPR with their index
     expressions being INTEGER_CST.  */
  gcc_assert (TREE_CODE (index_expr) != INTEGER_CST);
  

  do_pending_stack_adjust ();

  /* Find the default case target label.  */
  default_label = jump_target_rtx
      (CASE_LABEL (gimple_switch_default_label (stmt)));
  edge default_edge = EDGE_SUCC (bb, 0);
  int default_prob = default_edge->probability;

  /* Get upper and lower bounds of case values.  */
  elt = gimple_switch_label (stmt, 1);
  minval = fold_convert (index_type, CASE_LOW (elt));
  elt = gimple_switch_label (stmt, ncases - 1);
  if (CASE_HIGH (elt))
    maxval = fold_convert (index_type, CASE_HIGH (elt));
  else
    maxval = fold_convert (index_type, CASE_LOW (elt));

  /* Compute span of values.  */
  range = fold_build2 (MINUS_EXPR, index_type, maxval, minval);

  /* Listify the labels queue and gather some numbers to decide
     how to expand this switch().  */
  uniq = 0;
  count = 0;
  hash_set<tree> seen_labels;
  compute_cases_per_edge (stmt);

  for (i = ncases - 1; i >= 1; --i)
    {
      elt = gimple_switch_label (stmt, i);
      tree low = CASE_LOW (elt);
      gcc_assert (low);
      tree high = CASE_HIGH (elt);
      gcc_assert (! high || tree_int_cst_lt (low, high));
      tree lab = CASE_LABEL (elt);

      /* Count the elements.
	 A range counts double, since it requires two compares.  */
      count++;
      if (high)
	count++;

      /* If we have not seen this label yet, then increase the
	 number of unique case node targets seen.  */
      if (!seen_labels.add (lab))
	uniq++;

      /* The bounds on the case range, LOW and HIGH, have to be converted
	 to case's index type TYPE.  Note that the original type of the
	 case index in the source code is usually "lost" during
	 gimplification due to type promotion, but the case labels retain the
	 original type.  Make sure to drop overflow flags.  */
      low = fold_convert (index_type, low);
      if (TREE_OVERFLOW (low))
	low = wide_int_to_tree (index_type, low);

      /* The canonical from of a case label in GIMPLE is that a simple case
	 has an empty CASE_HIGH.  For the casesi and tablejump expanders,
	 the back ends want simple cases to have high == low.  */
      if (! high)
	high = low;
      high = fold_convert (index_type, high);
      if (TREE_OVERFLOW (high))
	high = wide_int_to_tree (index_type, high);

      basic_block case_bb = label_to_block_fn (cfun, lab);
      edge case_edge = find_edge (bb, case_bb);
      case_list = add_case_node (
          case_list, low, high, lab,
          case_edge->probability / (intptr_t)(case_edge->aux),
          case_node_pool);
    }
  reset_out_edges_aux (bb);

  /* cleanup_tree_cfg removes all SWITCH_EXPR with a single
     destination, such as one with a default case only.
     It also removes cases that are out of range for the switch
     type, so we should never get a zero here.  */
  gcc_assert (count > 0);

  rtx_insn *before_case = get_last_insn ();

  /* Decide how to expand this switch.
     The two options at this point are a dispatch table (casesi or
     tablejump) or a decision tree.  */

  if (expand_switch_as_decision_tree_p (range, uniq, count))
    emit_case_decision_tree (index_expr, index_type,
                             case_list, default_label,
                             default_prob);
  else
    emit_case_dispatch_table (index_expr, index_type,
			      case_list, default_label,
			      minval, maxval, range, bb);

  reorder_insns (NEXT_INSN (before_case), get_last_insn (), before_case);

  free_temp_slots ();
}

/* Expand the dispatch to a short decrement chain if there are few cases
   to dispatch to.  Likewise if neither casesi nor tablejump is available,
   or if flag_jump_tables is set.  Otherwise, expand as a casesi or a
   tablejump.  The index mode is always the mode of integer_type_node.
   Trap if no case matches the index.

   DISPATCH_INDEX is the index expression to switch on.  It should be a
   memory or register operand.
   
   DISPATCH_TABLE is a set of case labels.  The set should be sorted in
   ascending order, be contiguous, starting with value 0, and contain only
   single-valued case labels.  */

void
expand_sjlj_dispatch_table (rtx dispatch_index,
			    vec<tree> dispatch_table)
{
  tree index_type = integer_type_node;
  machine_mode index_mode = TYPE_MODE (index_type);

  int ncases = dispatch_table.length ();

  do_pending_stack_adjust ();
  rtx_insn *before_case = get_last_insn ();

  /* Expand as a decrement-chain if there are 5 or fewer dispatch
     labels.  This covers more than 98% of the cases in libjava,
     and seems to be a reasonable compromise between the "old way"
     of expanding as a decision tree or dispatch table vs. the "new
     way" with decrement chain or dispatch table.  */
  if (dispatch_table.length () <= 5
      || (!targetm.have_casesi () && !targetm.have_tablejump ())
      || !flag_jump_tables)
    {
      /* Expand the dispatch as a decrement chain:

	 "switch(index) {case 0: do_0; case 1: do_1; ...; case N: do_N;}"

	 ==>

	 if (index == 0) do_0; else index--;
	 if (index == 0) do_1; else index--;
	 ...
	 if (index == 0) do_N; else index--;

	 This is more efficient than a dispatch table on most machines.
	 The last "index--" is redundant but the code is trivially dead
	 and will be cleaned up by later passes.  */
      rtx index = copy_to_mode_reg (index_mode, dispatch_index);
      rtx zero = CONST0_RTX (index_mode);
      for (int i = 0; i < ncases; i++)
        {
	  tree elt = dispatch_table[i];
	  rtx_code_label *lab = jump_target_rtx (CASE_LABEL (elt));
	  do_jump_if_equal (index_mode, index, zero, lab, 0, -1);
	  force_expand_binop (index_mode, sub_optab,
			      index, CONST1_RTX (index_mode),
			      index, 0, OPTAB_DIRECT);
	}
    }
  else
    {
      /* Similar to expand_case, but much simpler.  */
      struct case_node *case_list = 0;
      object_allocator<case_node> case_node_pool ("struct sjlj_case pool");
      tree index_expr = make_tree (index_type, dispatch_index);
      tree minval = build_int_cst (index_type, 0);
      tree maxval = CASE_LOW (dispatch_table.last ());
      tree range = maxval;
      rtx_code_label *default_label = gen_label_rtx ();

      for (int i = ncases - 1; i >= 0; --i)
	{
	  tree elt = dispatch_table[i];
	  tree low = CASE_LOW (elt);
	  tree lab = CASE_LABEL (elt);
	  case_list = add_case_node (case_list, low, low, lab, 0, case_node_pool);
	}

      emit_case_dispatch_table (index_expr, index_type,
				case_list, default_label,
				minval, maxval, range,
                                BLOCK_FOR_INSN (before_case));
      emit_label (default_label);
    }

  /* Dispatching something not handled?  Trap!  */
  expand_builtin_trap ();

  reorder_insns (NEXT_INSN (before_case), get_last_insn (), before_case);

  free_temp_slots ();
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
balance_case_nodes (case_node_ptr *head, case_node_ptr parent)
{
  case_node_ptr np;

  np = *head;
  if (np)
    {
      int i = 0;
      int ranges = 0;
      case_node_ptr *npp;
      case_node_ptr left;

      /* Count the number of entries on branch.  Also count the ranges.  */

      while (np)
	{
	  if (!tree_int_cst_equal (np->low, np->high))
	    ranges++;

	  i++;
	  np = np->right;
	}

      if (i > 2)
	{
	  /* Split this list if it is long enough for that to help.  */
	  npp = head;
	  left = *npp;

	  /* If there are just three nodes, split at the middle one.  */
	  if (i == 3)
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
          np->subtree_prob = np->prob;
          np->subtree_prob += np->left->subtree_prob;
          np->subtree_prob += np->right->subtree_prob;
	}
      else
	{
	  /* Else leave this branch as one level,
	     but fill in `parent' fields.  */
	  np = *head;
	  np->parent = parent;
          np->subtree_prob = np->prob;
	  for (; np->right; np = np->right)
            {
	      np->right->parent = np;
              (*head)->subtree_prob += np->right->subtree_prob;
            }
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
node_has_low_bound (case_node_ptr node, tree index_type)
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

  low_minus_one = fold_build2 (MINUS_EXPR, TREE_TYPE (node->low),
			       node->low,
			       build_int_cst (TREE_TYPE (node->low), 1));

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
node_has_high_bound (case_node_ptr node, tree index_type)
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

  high_plus_one = fold_build2 (PLUS_EXPR, TREE_TYPE (node->high),
			       node->high,
			       build_int_cst (TREE_TYPE (node->high), 1));

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
node_is_bounded (case_node_ptr node, tree index_type)
{
  return (node_has_low_bound (node, index_type)
	  && node_has_high_bound (node, index_type));
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
emit_case_nodes (rtx index, case_node_ptr node, rtx_code_label *default_label,
		 int default_prob, tree index_type)
{
  /* If INDEX has an unsigned type, we must make unsigned branches.  */
  int unsignedp = TYPE_UNSIGNED (index_type);
  int probability;
  int prob = node->prob, subtree_prob = node->subtree_prob;
  machine_mode mode = GET_MODE (index);
  machine_mode imode = TYPE_MODE (index_type);

  /* Handle indices detected as constant during RTL expansion.  */
  if (mode == VOIDmode)
    mode = imode;

  /* See if our parents have already tested everything for us.
     If they have, emit an unconditional jump for this node.  */
  if (node_is_bounded (node, index_type))
    emit_jump (label_rtx (node->code_label));

  else if (tree_int_cst_equal (node->low, node->high))
    {
      probability = conditional_probability (prob, subtree_prob + default_prob);
      /* Node is single valued.  First see if the index expression matches
	 this node and then check our children, if any.  */
      do_jump_if_equal (mode, index,
			convert_modes (mode, imode,
				       expand_normal (node->low),
				       unsignedp),
			jump_target_rtx (node->code_label),
			unsignedp, probability);
      /* Since this case is taken at this point, reduce its weight from
         subtree_weight.  */
      subtree_prob -= prob;
      if (node->right != 0 && node->left != 0)
	{
	  /* This node has children on both sides.
	     Dispatch to one side or the other
	     by comparing the index value with this node's value.
	     If one subtree is bounded, check that one first,
	     so we can avoid real branches in the tree.  */

	  if (node_is_bounded (node->right, index_type))
	    {
              probability = conditional_probability (
                  node->right->prob,
                  subtree_prob + default_prob);
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_normal (node->high),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       label_rtx (node->right->code_label),
                                       probability);
	      emit_case_nodes (index, node->left, default_label, default_prob,
                               index_type);
	    }

	  else if (node_is_bounded (node->left, index_type))
	    {
              probability = conditional_probability (
                  node->left->prob,
                  subtree_prob + default_prob);
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_normal (node->high),
					unsignedp),
				       LT, NULL_RTX, mode, unsignedp,
				       label_rtx (node->left->code_label),
                                       probability);
	      emit_case_nodes (index, node->right, default_label, default_prob,
			       index_type);
	    }

	  /* If both children are single-valued cases with no
	     children, finish up all the work.  This way, we can save
	     one ordered comparison.  */
	  else if (tree_int_cst_equal (node->right->low, node->right->high)
		   && node->right->left == 0
		   && node->right->right == 0
		   && tree_int_cst_equal (node->left->low, node->left->high)
		   && node->left->left == 0
		   && node->left->right == 0)
	    {
	      /* Neither node is bounded.  First distinguish the two sides;
		 then emit the code for one side at a time.  */

	      /* See if the value matches what the right hand side
		 wants.  */
              probability = conditional_probability (
                  node->right->prob,
                  subtree_prob + default_prob);
	      do_jump_if_equal (mode, index,
				convert_modes (mode, imode,
					       expand_normal (node->right->low),
					       unsignedp),
				jump_target_rtx (node->right->code_label),
				unsignedp, probability);

	      /* See if the value matches what the left hand side
		 wants.  */
              probability = conditional_probability (
                  node->left->prob,
                  subtree_prob + default_prob);
	      do_jump_if_equal (mode, index,
				convert_modes (mode, imode,
					       expand_normal (node->left->low),
					       unsignedp),
				jump_target_rtx (node->left->code_label),
				unsignedp, probability);
	    }

	  else
	    {
	      /* Neither node is bounded.  First distinguish the two sides;
		 then emit the code for one side at a time.  */

	      tree test_label
		= build_decl (curr_insn_location (),
			      LABEL_DECL, NULL_TREE, void_type_node);

              /* The default label could be reached either through the right
                 subtree or the left subtree. Divide the probability
                 equally.  */
              probability = conditional_probability (
                  node->right->subtree_prob + default_prob/2,
                  subtree_prob + default_prob);
	      /* See if the value is on the right.  */
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_normal (node->high),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       label_rtx (test_label),
                                       probability);
              default_prob /= 2;

	      /* Value must be on the left.
		 Handle the left-hand subtree.  */
	      emit_case_nodes (index, node->left, default_label, default_prob, index_type);
	      /* If left-hand subtree does nothing,
		 go to default.  */
	      if (default_label)
	        emit_jump (default_label);

	      /* Code branches here for the right-hand subtree.  */
	      expand_label (test_label);
	      emit_case_nodes (index, node->right, default_label, default_prob, index_type);
	    }
	}

      else if (node->right != 0 && node->left == 0)
	{
	  /* Here we have a right child but no left so we issue a conditional
	     branch to default and process the right child.

	     Omit the conditional branch to default if the right child
	     does not have any children and is single valued; it would
	     cost too much space to save so little time.  */

	  if (node->right->right || node->right->left
	      || !tree_int_cst_equal (node->right->low, node->right->high))
	    {
	      if (!node_has_low_bound (node, index_type))
		{
                  probability = conditional_probability (
                      default_prob/2,
                      subtree_prob + default_prob);
		  emit_cmp_and_jump_insns (index,
					   convert_modes
					   (mode, imode,
					    expand_normal (node->high),
					    unsignedp),
					   LT, NULL_RTX, mode, unsignedp,
					   default_label,
                                           probability);
                  default_prob /= 2;
		}

	      emit_case_nodes (index, node->right, default_label, default_prob, index_type);
	    }
	  else
            {
              probability = conditional_probability (
                  node->right->subtree_prob,
                  subtree_prob + default_prob);
	      /* We cannot process node->right normally
	         since we haven't ruled out the numbers less than
	         this node's value.  So handle node->right explicitly.  */
	      do_jump_if_equal (mode, index,
			        convert_modes
			        (mode, imode,
			         expand_normal (node->right->low),
			         unsignedp),
				jump_target_rtx (node->right->code_label),
				unsignedp, probability);
            }
	  }

      else if (node->right == 0 && node->left != 0)
	{
	  /* Just one subtree, on the left.  */
	  if (node->left->left || node->left->right
	      || !tree_int_cst_equal (node->left->low, node->left->high))
	    {
	      if (!node_has_high_bound (node, index_type))
		{
                  probability = conditional_probability (
                      default_prob/2,
                      subtree_prob + default_prob);
		  emit_cmp_and_jump_insns (index,
					   convert_modes
					   (mode, imode,
					    expand_normal (node->high),
					    unsignedp),
					   GT, NULL_RTX, mode, unsignedp,
					   default_label,
                                           probability);
                  default_prob /= 2;
		}

	      emit_case_nodes (index, node->left, default_label,
                               default_prob, index_type);
	    }
	  else
            {
              probability = conditional_probability (
                  node->left->subtree_prob,
                  subtree_prob + default_prob);
	      /* We cannot process node->left normally
	         since we haven't ruled out the numbers less than
	         this node's value.  So handle node->left explicitly.  */
	      do_jump_if_equal (mode, index,
			        convert_modes
			        (mode, imode,
			         expand_normal (node->left->low),
			         unsignedp),
				jump_target_rtx (node->left->code_label),
				unsignedp, probability);
            }
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
            {
	      /* Right hand node is fully bounded so we can eliminate any
	         testing and branch directly to the target code.  */
              probability = conditional_probability (
                  node->right->subtree_prob,
                  subtree_prob + default_prob);
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
				        expand_normal (node->high),
				        unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       label_rtx (node->right->code_label),
                                       probability);
            }
	  else
	    {
	      /* Right hand node requires testing.
		 Branch to a label where we will handle it later.  */

	      test_label = build_decl (curr_insn_location (),
				       LABEL_DECL, NULL_TREE, void_type_node);
              probability = conditional_probability (
                  node->right->subtree_prob + default_prob/2,
                  subtree_prob + default_prob);
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_normal (node->high),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       label_rtx (test_label),
                                       probability);
              default_prob /= 2;
	    }

	  /* Value belongs to this node or to the left-hand subtree.  */

          probability = conditional_probability (
              prob,
              subtree_prob + default_prob);
	  emit_cmp_and_jump_insns (index,
				   convert_modes
				   (mode, imode,
				    expand_normal (node->low),
				    unsignedp),
				   GE, NULL_RTX, mode, unsignedp,
				   label_rtx (node->code_label),
                                   probability);

	  /* Handle the left-hand subtree.  */
	  emit_case_nodes (index, node->left, default_label, default_prob, index_type);

	  /* If right node had to be handled later, do that now.  */

	  if (test_label)
	    {
	      /* If the left-hand subtree fell through,
		 don't let it fall into the right-hand subtree.  */
	      if (default_label)
		emit_jump (default_label);

	      expand_label (test_label);
	      emit_case_nodes (index, node->right, default_label, default_prob, index_type);
	    }
	}

      else if (node->right != 0 && node->left == 0)
	{
	  /* Deal with values to the left of this node,
	     if they are possible.  */
	  if (!node_has_low_bound (node, index_type))
	    {
              probability = conditional_probability (
                  default_prob/2,
                  subtree_prob + default_prob);
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_normal (node->low),
					unsignedp),
				       LT, NULL_RTX, mode, unsignedp,
				       default_label,
                                       probability);
              default_prob /= 2;
	    }

	  /* Value belongs to this node or to the right-hand subtree.  */

          probability = conditional_probability (
              prob,
              subtree_prob + default_prob);
	  emit_cmp_and_jump_insns (index,
				   convert_modes
				   (mode, imode,
				    expand_normal (node->high),
				    unsignedp),
				   LE, NULL_RTX, mode, unsignedp,
				   label_rtx (node->code_label),
                                   probability);

	  emit_case_nodes (index, node->right, default_label, default_prob, index_type);
	}

      else if (node->right == 0 && node->left != 0)
	{
	  /* Deal with values to the right of this node,
	     if they are possible.  */
	  if (!node_has_high_bound (node, index_type))
	    {
              probability = conditional_probability (
                  default_prob/2,
                  subtree_prob + default_prob);
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_normal (node->high),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       default_label,
                                       probability);
              default_prob /= 2;
	    }

	  /* Value belongs to this node or to the left-hand subtree.  */

          probability = conditional_probability (
              prob,
              subtree_prob + default_prob);
	  emit_cmp_and_jump_insns (index,
				   convert_modes
				   (mode, imode,
				    expand_normal (node->low),
				    unsignedp),
				   GE, NULL_RTX, mode, unsignedp,
				   label_rtx (node->code_label),
                                   probability);

	  emit_case_nodes (index, node->left, default_label, default_prob, index_type);
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
              probability = conditional_probability (
                  default_prob,
                  subtree_prob + default_prob);
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_normal (node->high),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       default_label,
                                       probability);
	    }

	  else if (!low_bound && high_bound)
	    {
              probability = conditional_probability (
                  default_prob,
                  subtree_prob + default_prob);
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_normal (node->low),
					unsignedp),
				       LT, NULL_RTX, mode, unsignedp,
				       default_label,
                                       probability);
	    }
	  else if (!low_bound && !high_bound)
	    {
	      /* Widen LOW and HIGH to the same width as INDEX.  */
	      tree type = lang_hooks.types.type_for_mode (mode, unsignedp);
	      tree low = build1 (CONVERT_EXPR, type, node->low);
	      tree high = build1 (CONVERT_EXPR, type, node->high);
	      rtx low_rtx, new_index, new_bound;

	      /* Instead of doing two branches, emit one unsigned branch for
		 (index-low) > (high-low).  */
	      low_rtx = expand_expr (low, NULL_RTX, mode, EXPAND_NORMAL);
	      new_index = expand_simple_binop (mode, MINUS, index, low_rtx,
					       NULL_RTX, unsignedp,
					       OPTAB_WIDEN);
	      new_bound = expand_expr (fold_build2 (MINUS_EXPR, type,
						    high, low),
				       NULL_RTX, mode, EXPAND_NORMAL);

              probability = conditional_probability (
                  default_prob,
                  subtree_prob + default_prob);
	      emit_cmp_and_jump_insns (new_index, new_bound, GT, NULL_RTX,
				       mode, 1, default_label, probability);
	    }

	  emit_jump (jump_target_rtx (node->code_label));
	}
    }
}
