/* Expands front end tree to back end RTL for GCC
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
   2010, 2011, 2012 Free Software Foundation, Inc.

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
#include "tm.h"

#include "rtl.h"
#include "hard-reg-set.h"
#include "tree.h"
#include "tm_p.h"
#include "flags.h"
#include "except.h"
#include "function.h"
#include "insn-config.h"
#include "expr.h"
#include "libfuncs.h"
#include "recog.h"
#include "machmode.h"
#include "diagnostic-core.h"
#include "output.h"
#include "ggc.h"
#include "langhooks.h"
#include "predict.h"
#include "optabs.h"
#include "target.h"
#include "gimple.h"
#include "regs.h"
#include "alloc-pool.h"
#include "pretty-print.h"
#include "bitmap.h"
#include "params.h"
#include "dumpfile.h"


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
};

typedef struct case_node case_node;
typedef struct case_node *case_node_ptr;


static int n_occurrences (int, const char *);
static bool tree_conflicts_with_clobbers_p (tree, HARD_REG_SET *);
static void expand_nl_goto_receiver (void);
static bool check_operand_nalternatives (tree, tree);
static bool check_unique_operand_names (tree, tree, tree);
static char *resolve_operand_name_1 (char *, tree, tree, tree);
static void expand_null_return_1 (void);
static void expand_value_return (rtx);
static void balance_case_nodes (case_node_ptr *, case_node_ptr);
static int node_has_low_bound (case_node_ptr, tree);
static int node_has_high_bound (case_node_ptr, tree);
static int node_is_bounded (case_node_ptr, tree);
static void emit_case_nodes (rtx, case_node_ptr, rtx, tree);
static struct case_node *add_case_node (struct case_node *, tree,
                                        tree, tree, tree, alloc_pool);


/* Return the rtx-label that corresponds to a LABEL_DECL,
   creating it if necessary.  */

rtx
label_rtx (tree label)
{
  gcc_assert (TREE_CODE (label) == LABEL_DECL);

  if (!DECL_RTL_SET_P (label))
    {
      rtx r = gen_label_rtx ();
      SET_DECL_RTL (label, r);
      if (FORCED_LABEL (label) || DECL_NONLOCAL (label))
	LABEL_PRESERVE_P (r) = 1;
    }

  return DECL_RTL (label);
}

/* As above, but also put it on the forced-reference list of the
   function that contains it.  */
rtx
force_label_rtx (tree label)
{
  rtx ref = label_rtx (label);
  tree function = decl_function_context (label);

  gcc_assert (function);

  forced_labels = gen_rtx_EXPR_LIST (VOIDmode, ref, forced_labels);
  return ref;
}

/* Add an unconditional jump to LABEL as the next sequential instruction.  */

void
emit_jump (rtx label)
{
  do_pending_stack_adjust ();
  emit_jump_insn (gen_jump (label));
  emit_barrier ();
}

/* Emit code to jump to the address
   specified by the pointer expression EXP.  */

void
expand_computed_goto (tree exp)
{
  rtx x = expand_normal (exp);

  x = convert_memory_address (Pmode, x);

  do_pending_stack_adjust ();
  emit_indirect_jump (x);
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
  rtx label_r = label_rtx (label);

  do_pending_stack_adjust ();
  emit_label (label_r);
  if (DECL_NAME (label))
    LABEL_NAME (DECL_RTL (label)) = IDENTIFIER_POINTER (DECL_NAME (label));

  if (DECL_NONLOCAL (label))
    {
      expand_nl_goto_receiver ();
      nonlocal_goto_handler_labels
	= gen_rtx_EXPR_LIST (VOIDmode, label_r,
			     nonlocal_goto_handler_labels);
    }

  if (FORCED_LABEL (label))
    forced_labels = gen_rtx_EXPR_LIST (VOIDmode, label_r, forced_labels);

  if (DECL_NONLOCAL (label) || FORCED_LABEL (label))
    maybe_set_first_label_num (label_r);
}

/* Generate RTL code for a `goto' statement with target label LABEL.
   LABEL should be a LABEL_DECL tree node that was or will later be
   defined with `expand_label'.  */

void
expand_goto (tree label)
{
#ifdef ENABLE_CHECKING
  /* Check for a nonlocal goto to a containing function.  Should have
     gotten translated to __builtin_nonlocal_goto.  */
  tree context = decl_function_context (label);
  gcc_assert (!context || context == current_function_decl);
#endif

  emit_jump (label_rtx (label));
}

/* Return the number of times character C occurs in string S.  */
static int
n_occurrences (int c, const char *s)
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

static void
expand_asm_loc (tree string, int vol, location_t locus)
{
  rtx body;

  if (TREE_CODE (string) == ADDR_EXPR)
    string = TREE_OPERAND (string, 0);

  body = gen_rtx_ASM_INPUT_loc (VOIDmode,
				ggc_strdup (TREE_STRING_POINTER (string)),
				locus);

  MEM_VOLATILE_P (body) = vol;

  emit_insn (body);
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

      case 'V':  case TARGET_MEM_CONSTRAINT:  case 'o':
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
	if (REG_CLASS_FROM_CONSTRAINT (*p, p) != NO_REGS)
	  *allows_reg = true;
#ifdef EXTRA_CONSTRAINT_STR
	else if (EXTRA_ADDRESS_CONSTRAINT (*p, p))
	  *allows_reg = true;
	else if (EXTRA_MEMORY_CONSTRAINT (*p, p))
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

      case 'V':  case TARGET_MEM_CONSTRAINT:  case 'o':
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
	    error ("invalid punctuation %qc in constraint", constraint[j]);
	    return false;
	  }
	if (REG_CLASS_FROM_CONSTRAINT (constraint[j], constraint + j)
	    != NO_REGS)
	  *allows_reg = true;
#ifdef EXTRA_CONSTRAINT_STR
	else if (EXTRA_ADDRESS_CONSTRAINT (constraint[j], constraint + j))
	  *allows_reg = true;
	else if (EXTRA_MEMORY_CONSTRAINT (constraint[j], constraint + j))
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

  if (TREE_CODE (decl) == VAR_DECL)
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

/* Check for overlap between registers marked in CLOBBERED_REGS and
   anything inappropriate in T.  Emit error and return the register
   variable definition for error, NULL_TREE for ok.  */

static bool
tree_conflicts_with_clobbers_p (tree t, HARD_REG_SET *clobbered_regs)
{
  /* Conflicts between asm-declared register variables and the clobber
     list are not allowed.  */
  tree overlap = tree_overlaps_hard_reg_set (t, clobbered_regs);

  if (overlap)
    {
      error ("asm-specifier for variable %qE conflicts with asm clobber list",
	     DECL_NAME (overlap));

      /* Reset registerness to stop multiple errors emitted for a single
	 variable.  */
      DECL_REGISTER (overlap) = 0;
      return true;
    }

  return false;
}

/* Generate RTL for an asm statement with arguments.
   STRING is the instruction template.
   OUTPUTS is a list of output arguments (lvalues); INPUTS a list of inputs.
   Each output or input has an expression in the TREE_VALUE and
   a tree list in TREE_PURPOSE which in turn contains a constraint
   name in TREE_VALUE (or NULL_TREE) and a constraint string
   in TREE_PURPOSE.
   CLOBBERS is a list of STRING_CST nodes each naming a hard register
   that is clobbered by this insn.

   Not all kinds of lvalue that may appear in OUTPUTS can be stored directly.
   Some elements of OUTPUTS may be replaced with trees representing temporary
   values.  The caller should copy those temporary values to the originally
   specified lvalues.

   VOL nonzero means the insn is volatile; don't optimize it.  */

static void
expand_asm_operands (tree string, tree outputs, tree inputs,
		     tree clobbers, tree labels, int vol, location_t locus)
{
  rtvec argvec, constraintvec, labelvec;
  rtx body;
  int ninputs = list_length (inputs);
  int noutputs = list_length (outputs);
  int nlabels = list_length (labels);
  int ninout;
  int nclobbers;
  HARD_REG_SET clobbered_regs;
  int clobber_conflict_found = 0;
  tree tail;
  tree t;
  int i;
  /* Vector of RTX's of evaluated output operands.  */
  rtx *output_rtx = XALLOCAVEC (rtx, noutputs);
  int *inout_opnum = XALLOCAVEC (int, noutputs);
  rtx *real_output_rtx = XALLOCAVEC (rtx, noutputs);
  enum machine_mode *inout_mode = XALLOCAVEC (enum machine_mode, noutputs);
  const char **constraints = XALLOCAVEC (const char *, noutputs + ninputs);
  int old_generating_concat_p = generating_concat_p;

  /* An ASM with no outputs needs to be treated as volatile, for now.  */
  if (noutputs == 0)
    vol = 1;

  if (! check_operand_nalternatives (outputs, inputs))
    return;

  string = resolve_asm_operand_names (string, outputs, inputs, labels);

  /* Collect constraints.  */
  i = 0;
  for (t = outputs; t ; t = TREE_CHAIN (t), i++)
    constraints[i] = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));
  for (t = inputs; t ; t = TREE_CHAIN (t), i++)
    constraints[i] = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));

  /* Sometimes we wish to automatically clobber registers across an asm.
     Case in point is when the i386 backend moved from cc0 to a hard reg --
     maintaining source-level compatibility means automatically clobbering
     the flags register.  */
  clobbers = targetm.md_asm_clobbers (outputs, inputs, clobbers);

  /* Count the number of meaningful clobbered registers, ignoring what
     we would ignore later.  */
  nclobbers = 0;
  CLEAR_HARD_REG_SET (clobbered_regs);
  for (tail = clobbers; tail; tail = TREE_CHAIN (tail))
    {
      const char *regname;
      int nregs;

      if (TREE_VALUE (tail) == error_mark_node)
	return;
      regname = TREE_STRING_POINTER (TREE_VALUE (tail));

      i = decode_reg_name_and_count (regname, &nregs);
      if (i == -4)
	++nclobbers;
      else if (i == -2)
	error ("unknown register name %qs in %<asm%>", regname);

      /* Mark clobbered registers.  */
      if (i >= 0)
        {
	  int reg;

	  for (reg = i; reg < i + nregs; reg++)
	    {
	      ++nclobbers;

	      /* Clobbering the PIC register is an error.  */
	      if (reg == (int) PIC_OFFSET_TABLE_REGNUM)
		{
		  error ("PIC register clobbered by %qs in %<asm%>", regname);
		  return;
		}

	      SET_HARD_REG_BIT (clobbered_regs, reg);
	    }
	}
    }

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
		  && REG_P (DECL_RTL (val))
		  && GET_MODE (DECL_RTL (val)) != TYPE_MODE (type))))
	mark_addressable (val);

      if (is_inout)
	ninout++;
    }

  ninputs += ninout;
  if (ninputs + noutputs > MAX_RECOG_OPERANDS)
    {
      error ("more than %d operands in %<asm%>", MAX_RECOG_OPERANDS);
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
	mark_addressable (TREE_VALUE (tail));
    }

  /* Second pass evaluates arguments.  */

  /* Make sure stack is consistent for asm goto.  */
  if (nlabels > 0)
    do_pending_stack_adjust ();

  ninout = 0;
  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    {
      tree val = TREE_VALUE (tail);
      tree type = TREE_TYPE (val);
      bool is_inout;
      bool allows_reg;
      bool allows_mem;
      rtx op;
      bool ok;

      ok = parse_output_constraint (&constraints[i], i, ninputs,
				    noutputs, &allows_mem, &allows_reg,
				    &is_inout);
      gcc_assert (ok);

      /* If an output operand is not a decl or indirect ref and our constraint
	 allows a register, make a temporary to act as an intermediate.
	 Make the asm insn write into that, then our caller will copy it to
	 the real output operand.  Likewise for promoted variables.  */

      generating_concat_p = 0;

      real_output_rtx[i] = NULL_RTX;
      if ((TREE_CODE (val) == INDIRECT_REF
	   && allows_mem)
	  || (DECL_P (val)
	      && (allows_mem || REG_P (DECL_RTL (val)))
	      && ! (REG_P (DECL_RTL (val))
		    && GET_MODE (DECL_RTL (val)) != TYPE_MODE (type)))
	  || ! allows_reg
	  || is_inout)
	{
	  op = expand_expr (val, NULL_RTX, VOIDmode, EXPAND_WRITE);
	  if (MEM_P (op))
	    op = validize_mem (op);

	  if (! allows_reg && !MEM_P (op))
	    error ("output number %d not directly addressable", i);
	  if ((! allows_mem && MEM_P (op))
	      || GET_CODE (op) == CONCAT)
	    {
	      real_output_rtx[i] = op;
	      op = gen_reg_rtx (GET_MODE (op));
	      if (is_inout)
		emit_move_insn (op, real_output_rtx[i]);
	    }
	}
      else
	{
	  op = assign_temp (type, 0, 1);
	  op = validize_mem (op);
	  if (!MEM_P (op) && TREE_CODE (TREE_VALUE (tail)) == SSA_NAME)
	    set_reg_attrs_for_decl_rtl (SSA_NAME_VAR (TREE_VALUE (tail)), op);
	  TREE_VALUE (tail) = make_tree (type, op);
	}
      output_rtx[i] = op;

      generating_concat_p = old_generating_concat_p;

      if (is_inout)
	{
	  inout_mode[ninout] = TYPE_MODE (type);
	  inout_opnum[ninout++] = i;
	}

      if (tree_conflicts_with_clobbers_p (val, &clobbered_regs))
	clobber_conflict_found = 1;
    }

  /* Make vectors for the expression-rtx, constraint strings,
     and named operands.  */

  argvec = rtvec_alloc (ninputs);
  constraintvec = rtvec_alloc (ninputs);
  labelvec = rtvec_alloc (nlabels);

  body = gen_rtx_ASM_OPERANDS ((noutputs == 0 ? VOIDmode
				: GET_MODE (output_rtx[0])),
			       ggc_strdup (TREE_STRING_POINTER (string)),
			       empty_string, 0, argvec, constraintvec,
			       labelvec, locus);

  MEM_VOLATILE_P (body) = vol;

  /* Eval the inputs and put them into ARGVEC.
     Put their constraints into ASM_INPUTs and store in CONSTRAINTS.  */

  for (i = 0, tail = inputs; tail; tail = TREE_CHAIN (tail), ++i)
    {
      bool allows_reg, allows_mem;
      const char *constraint;
      tree val, type;
      rtx op;
      bool ok;

      constraint = constraints[i + noutputs];
      ok = parse_input_constraint (&constraint, i, ninputs, noutputs, ninout,
				   constraints, &allows_mem, &allows_reg);
      gcc_assert (ok);

      generating_concat_p = 0;

      val = TREE_VALUE (tail);
      type = TREE_TYPE (val);
      /* EXPAND_INITIALIZER will not generate code for valid initializer
	 constants, but will still generate code for other types of operand.
	 This is the behavior we want for constant constraints.  */
      op = expand_expr (val, NULL_RTX, VOIDmode,
			allows_reg ? EXPAND_NORMAL
			: allows_mem ? EXPAND_MEMORY
			: EXPAND_INITIALIZER);

      /* Never pass a CONCAT to an ASM.  */
      if (GET_CODE (op) == CONCAT)
	op = force_reg (GET_MODE (op), op);
      else if (MEM_P (op))
	op = validize_mem (op);

      if (asm_operand_ok (op, constraint, NULL) <= 0)
	{
	  if (allows_reg && TYPE_MODE (type) != BLKmode)
	    op = force_reg (TYPE_MODE (type), op);
	  else if (!allows_mem)
	    warning (0, "asm operand %d probably doesn%'t match constraints",
		     i + noutputs);
	  else if (MEM_P (op))
	    {
	      /* We won't recognize either volatile memory or memory
		 with a queued address as available a memory_operand
		 at this point.  Ignore it: clearly this *is* a memory.  */
	    }
	  else
	    gcc_unreachable ();
	}

      generating_concat_p = old_generating_concat_p;
      ASM_OPERANDS_INPUT (body, i) = op;

      ASM_OPERANDS_INPUT_CONSTRAINT_EXP (body, i)
	= gen_rtx_ASM_INPUT (TYPE_MODE (type),
			     ggc_strdup (constraints[i + noutputs]));

      if (tree_conflicts_with_clobbers_p (val, &clobbered_regs))
	clobber_conflict_found = 1;
    }

  /* Protect all the operands from the queue now that they have all been
     evaluated.  */

  generating_concat_p = 0;

  /* For in-out operands, copy output rtx to input rtx.  */
  for (i = 0; i < ninout; i++)
    {
      int j = inout_opnum[i];
      char buffer[16];

      ASM_OPERANDS_INPUT (body, ninputs - ninout + i)
	= output_rtx[j];

      sprintf (buffer, "%d", j);
      ASM_OPERANDS_INPUT_CONSTRAINT_EXP (body, ninputs - ninout + i)
	= gen_rtx_ASM_INPUT (inout_mode[i], ggc_strdup (buffer));
    }

  /* Copy labels to the vector.  */
  for (i = 0, tail = labels; i < nlabels; ++i, tail = TREE_CHAIN (tail))
    ASM_OPERANDS_LABEL (body, i)
      = gen_rtx_LABEL_REF (Pmode, label_rtx (TREE_VALUE (tail)));

  generating_concat_p = old_generating_concat_p;

  /* Now, for each output, construct an rtx
     (set OUTPUT (asm_operands INSN OUTPUTCONSTRAINT OUTPUTNUMBER
			       ARGVEC CONSTRAINTS OPNAMES))
     If there is more than one, put them inside a PARALLEL.  */

  if (nlabels > 0 && nclobbers == 0)
    {
      gcc_assert (noutputs == 0);
      emit_jump_insn (body);
    }
  else if (noutputs == 0 && nclobbers == 0)
    {
      /* No output operands: put in a raw ASM_OPERANDS rtx.  */
      emit_insn (body);
    }
  else if (noutputs == 1 && nclobbers == 0)
    {
      ASM_OPERANDS_OUTPUT_CONSTRAINT (body) = ggc_strdup (constraints[0]);
      emit_insn (gen_rtx_SET (VOIDmode, output_rtx[0], body));
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
			    ggc_strdup (TREE_STRING_POINTER (string)),
			    ggc_strdup (constraints[i]),
			    i, argvec, constraintvec, labelvec, locus));

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
	  int reg, nregs;
	  int j = decode_reg_name_and_count (regname, &nregs);
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

	  for (reg = j; reg < j + nregs; reg++)
	    {
	      /* Use QImode since that's guaranteed to clobber just
	       * one reg.  */
	      clobbered_reg = gen_rtx_REG (QImode, reg);

	      /* Do sanity check for overlap between clobbers and
		 respectively input and outputs that hasn't been
		 handled.  Such overlap should have been detected and
		 reported above.  */
	      if (!clobber_conflict_found)
		{
		  int opno;

		  /* We test the old body (obody) contents to avoid
		     tripping over the under-construction body.  */
		  for (opno = 0; opno < noutputs; opno++)
		    if (reg_overlap_mentioned_p (clobbered_reg,
						 output_rtx[opno]))
		      internal_error
			("asm clobber conflict with output operand");

		  for (opno = 0; opno < ninputs - ninout; opno++)
		    if (reg_overlap_mentioned_p (clobbered_reg,
						 ASM_OPERANDS_INPUT (obody,
								     opno)))
		      internal_error
			("asm clobber conflict with input operand");
		}

	      XVECEXP (body, 0, i++)
		= gen_rtx_CLOBBER (VOIDmode, clobbered_reg);
	    }
	}

      if (nlabels > 0)
	emit_jump_insn (body);
      else
	emit_insn (body);
    }

  /* For any outputs that needed reloading into registers, spill them
     back to where they belong.  */
  for (i = 0; i < noutputs; ++i)
    if (real_output_rtx[i])
      emit_move_insn (real_output_rtx[i], output_rtx[i]);

  crtl->has_asm_statement = 1;
  free_temp_slots ();
}

void
expand_asm_stmt (gimple stmt)
{
  int noutputs;
  tree outputs, tail, t;
  tree *o;
  size_t i, n;
  const char *s;
  tree str, out, in, cl, labels;
  location_t locus = gimple_location (stmt);

  /* Meh... convert the gimple asm operands into real tree lists.
     Eventually we should make all routines work on the vectors instead
     of relying on TREE_CHAIN.  */
  out = NULL_TREE;
  n = gimple_asm_noutputs (stmt);
  if (n > 0)
    {
      t = out = gimple_asm_output_op (stmt, 0);
      for (i = 1; i < n; i++)
	t = TREE_CHAIN (t) = gimple_asm_output_op (stmt, i);
    }

  in = NULL_TREE;
  n = gimple_asm_ninputs (stmt);
  if (n > 0)
    {
      t = in = gimple_asm_input_op (stmt, 0);
      for (i = 1; i < n; i++)
	t = TREE_CHAIN (t) = gimple_asm_input_op (stmt, i);
    }

  cl = NULL_TREE;
  n = gimple_asm_nclobbers (stmt);
  if (n > 0)
    {
      t = cl = gimple_asm_clobber_op (stmt, 0);
      for (i = 1; i < n; i++)
	t = TREE_CHAIN (t) = gimple_asm_clobber_op (stmt, i);
    }

  labels = NULL_TREE;
  n = gimple_asm_nlabels (stmt);
  if (n > 0)
    {
      t = labels = gimple_asm_label_op (stmt, 0);
      for (i = 1; i < n; i++)
	t = TREE_CHAIN (t) = gimple_asm_label_op (stmt, i);
    }

  s = gimple_asm_string (stmt);
  str = build_string (strlen (s), s);

  if (gimple_asm_input_p (stmt))
    {
      expand_asm_loc (str, gimple_asm_volatile_p (stmt), locus);
      return;
    }

  outputs = out;
  noutputs = gimple_asm_noutputs (stmt);
  /* o[I] is the place that output number I should be written.  */
  o = (tree *) alloca (noutputs * sizeof (tree));

  /* Record the contents of OUTPUTS before it is modified.  */
  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    o[i] = TREE_VALUE (tail);

  /* Generate the ASM_OPERANDS insn; store into the TREE_VALUEs of
     OUTPUTS some trees for where the values were actually stored.  */
  expand_asm_operands (str, outputs, in, cl, labels,
		       gimple_asm_volatile_p (stmt), locus);

  /* Copy all the intermediate outputs into the specified outputs.  */
  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    {
      if (o[i] != TREE_VALUE (tail))
	{
	  expand_assignment (o[i], TREE_VALUE (tail), false);
	  free_temp_slots ();

	  /* Restore the original value so that it's correct the next
	     time we expand this function.  */
	  TREE_VALUE (tail) = o[i];
	}
    }
}

/* A subroutine of expand_asm_operands.  Check that all operands have
   the same number of alternatives.  Return true if so.  */

static bool
check_operand_nalternatives (tree outputs, tree inputs)
{
  if (outputs || inputs)
    {
      tree tmp = TREE_PURPOSE (outputs ? outputs : inputs);
      int nalternatives
	= n_occurrences (',', TREE_STRING_POINTER (TREE_VALUE (tmp)));
      tree next = inputs;

      if (nalternatives + 1 > MAX_RECOG_ALTERNATIVES)
	{
	  error ("too many alternatives in %<asm%>");
	  return false;
	}

      tmp = outputs;
      while (tmp)
	{
	  const char *constraint
	    = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (tmp)));

	  if (n_occurrences (',', constraint) != nalternatives)
	    {
	      error ("operand constraints for %<asm%> differ "
		     "in number of alternatives");
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

/* A subroutine of expand_asm_operands.  Resolve the names of the operands
   in *POUTPUTS and *PINPUTS to numbers, and replace the name expansions in
   STRING and in the constraints to those numbers.  */

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

/* Generate RTL to return from the current function, with no value.
   (That is, we do not do anything about returning any value.)  */

void
expand_null_return (void)
{
  /* If this function was declared to return a value, but we
     didn't, clobber the return registers so that they are not
     propagated live to the rest of the function.  */
  clobber_return_register ();

  expand_null_return_1 ();
}

/* Generate RTL to return directly from the current function.
   (That is, we bypass any return value.)  */

void
expand_naked_return (void)
{
  rtx end_label;

  clear_pending_stack_adjust ();
  do_pending_stack_adjust ();

  end_label = naked_return_label;
  if (end_label == 0)
    end_label = naked_return_label = gen_label_rtx ();

  emit_jump (end_label);
}

/* Generate RTL to return from the current function, with value VAL.  */

static void
expand_value_return (rtx val)
{
  /* Copy the value to the return location unless it's already there.  */

  tree decl = DECL_RESULT (current_function_decl);
  rtx return_reg = DECL_RTL (decl);
  if (return_reg != val)
    {
      tree funtype = TREE_TYPE (current_function_decl);
      tree type = TREE_TYPE (decl);
      int unsignedp = TYPE_UNSIGNED (type);
      enum machine_mode old_mode = DECL_MODE (decl);
      enum machine_mode mode;
      if (DECL_BY_REFERENCE (decl))
        mode = promote_function_mode (type, old_mode, &unsignedp, funtype, 2);
      else
        mode = promote_function_mode (type, old_mode, &unsignedp, funtype, 1);

      if (mode != old_mode)
	val = convert_modes (mode, old_mode, val, unsignedp);

      if (GET_CODE (return_reg) == PARALLEL)
	emit_group_load (return_reg, val, type, int_size_in_bytes (type));
      else
	emit_move_insn (return_reg, val);
    }

  expand_null_return_1 ();
}

/* Output a return with no value.  */

static void
expand_null_return_1 (void)
{
  clear_pending_stack_adjust ();
  do_pending_stack_adjust ();
  emit_jump (return_label);
}

/* Generate RTL to evaluate the expression RETVAL and return it
   from the current function.  */

void
expand_return (tree retval)
{
  rtx result_rtl;
  rtx val = 0;
  tree retval_rhs;

  /* If function wants no value, give it none.  */
  if (TREE_CODE (TREE_TYPE (TREE_TYPE (current_function_decl))) == VOID_TYPE)
    {
      expand_normal (retval);
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
  else if ((TREE_CODE (retval) == MODIFY_EXPR
	    || TREE_CODE (retval) == INIT_EXPR)
	   && TREE_CODE (TREE_OPERAND (retval, 0)) == RESULT_DECL)
    retval_rhs = TREE_OPERAND (retval, 1);
  else
    retval_rhs = retval;

  result_rtl = DECL_RTL (DECL_RESULT (current_function_decl));

  /* If we are returning the RESULT_DECL, then the value has already
     been stored into it, so we don't have to do anything special.  */
  if (TREE_CODE (retval_rhs) == RESULT_DECL)
    expand_value_return (result_rtl);

  /* If the result is an aggregate that is being returned in one (or more)
     registers, load the registers here.  */

  else if (retval_rhs != 0
	   && TYPE_MODE (TREE_TYPE (retval_rhs)) == BLKmode
	   && REG_P (result_rtl))
    {
      val = copy_blkmode_to_reg (GET_MODE (result_rtl), retval_rhs);
      if (val)
	{
	  /* Use the mode of the result value on the return register.  */
	  PUT_MODE (result_rtl, GET_MODE (val));
	  expand_value_return (val);
	}
      else
	expand_null_return ();
    }
  else if (retval_rhs != 0
	   && !VOID_TYPE_P (TREE_TYPE (retval_rhs))
	   && (REG_P (result_rtl)
	       || (GET_CODE (result_rtl) == PARALLEL)))
    {
      /* Calculate the return value into a temporary (usually a pseudo
         reg).  */
      tree ot = TREE_TYPE (DECL_RESULT (current_function_decl));
      tree nt = build_qualified_type (ot, TYPE_QUALS (ot) | TYPE_QUAL_CONST);

      val = assign_temp (nt, 0, 1);
      val = expand_expr (retval_rhs, val, GET_MODE (val), EXPAND_NORMAL);
      val = force_not_mem (val);
      /* Return the calculated value.  */
      expand_value_return (val);
    }
  else
    {
      /* No hard reg used; calculate value into hard return reg.  */
      expand_expr (retval, const0_rtx, VOIDmode, EXPAND_NORMAL);
      expand_value_return (result_rtl);
    }
}

/* Emit code to restore vital registers at the beginning of a nonlocal goto
   handler.  */
static void
expand_nl_goto_receiver (void)
{
  rtx chain;

  /* Clobber the FP when we get here, so we have to make sure it's
     marked as used by this function.  */
  emit_use (hard_frame_pointer_rtx);

  /* Mark the static chain as clobbered here so life information
     doesn't get messed up for it.  */
  chain = targetm.calls.static_chain (current_function_decl, true);
  if (chain && REG_P (chain))
    emit_clobber (chain);

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

#if !HARD_FRAME_POINTER_IS_ARG_POINTER
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
	  emit_move_insn (crtl->args.internal_arg_pointer,
			  copy_to_reg (get_arg_pointer_save_area ()));
	}
    }
#endif

#ifdef HAVE_nonlocal_goto_receiver
  if (HAVE_nonlocal_goto_receiver)
    emit_insn (gen_nonlocal_goto_receiver ());
#endif

  /* We must not allow the code we just generated to be reordered by
     scheduling.  Specifically, the update of the frame pointer must
     happen immediately, not later.  */
  emit_insn (gen_blockage ());
}

/* Emit code to save the current value of stack.  */
rtx
expand_stack_save (void)
{
  rtx ret = NULL_RTX;

  do_pending_stack_adjust ();
  emit_stack_save (SAVE_BLOCK, &ret);
  return ret;
}

/* Emit code to restore the current value of stack.  */
void
expand_stack_restore (tree var)
{
  rtx prev, sa = expand_normal (var);

  sa = convert_memory_address (Pmode, sa);

  prev = get_last_insn ();
  emit_stack_restore (SAVE_BLOCK, sa);
  fixup_args_size_notes (prev, get_last_insn (), 0);
}

/* Do the insertion of a case label into case_list.  The labels are
   fed to us in descending order from the sorted vector of case labels used
   in the tree part of the middle end.  So the list we construct is
   sorted in ascending order.  The bounds on the case range, LOW and HIGH,
   are converted to case's index type TYPE.  Note that the original type
   of the case index in the source code is usually "lost" during
   gimplification due to type promotion, but the case labels retain the
   original type.  */

static struct case_node *
add_case_node (struct case_node *head, tree type, tree low, tree high,
               tree label, alloc_pool case_node_pool)
{
  struct case_node *r;

  gcc_checking_assert (low);
  gcc_checking_assert (! high || (TREE_TYPE (low) == TREE_TYPE (high)));

  /* Add this label to the chain.  Make sure to drop overflow flags.  */
  r = (struct case_node *) pool_alloc (case_node_pool);
  r->low = build_int_cst_wide (type, TREE_INT_CST_LOW (low),
			       TREE_INT_CST_HIGH (low));
  r->high = build_int_cst_wide (type, TREE_INT_CST_LOW (high),
				TREE_INT_CST_HIGH (high));
  r->code_label = label;
  r->parent = r->left = NULL;
  r->right = head;
  return r;
}

/* Dump ROOT, a list or tree of case nodes, to file.  */

static void
dump_case_nodes (FILE *f, struct case_node *root,
		 int indent_step, int indent_level)
{
  HOST_WIDE_INT low, high;

  if (root == 0)
    return;
  indent_level++;

  dump_case_nodes (f, root->left, indent_step, indent_level);

  low = tree_low_cst (root->low, 0);
  high = tree_low_cst (root->high, 0);

  fputs (";; ", f);
  if (high == low)
    fprintf(f, "%*s" HOST_WIDE_INT_PRINT_DEC,
	    indent_step * indent_level, "", low);
  else
    fprintf(f, "%*s" HOST_WIDE_INT_PRINT_DEC " ... " HOST_WIDE_INT_PRINT_DEC,
	    indent_step * indent_level, "", low, high);
  fputs ("\n", f);

  dump_case_nodes (f, root->right, indent_step, indent_level);
}

#ifndef HAVE_casesi
#define HAVE_casesi 0
#endif

#ifndef HAVE_tablejump
#define HAVE_tablejump 0
#endif

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
  if (!HAVE_casesi && !HAVE_tablejump)
    return true;
  if (!flag_jump_tables)
    return true;

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
      || ! host_integerp (range, /*pos=*/1)
      || compare_tree_int (range, max_ratio * count) > 0)
    return true;

  return false;
}

/* Generate a decision tree, switching on INDEX_EXPR and jumping to
   one of the labels in CASE_LIST or to the DEFAULT_LABEL.
   
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
			 struct case_node *case_list, rtx default_label)
{
  rtx index = expand_normal (index_expr);

  if (GET_MODE_CLASS (GET_MODE (index)) == MODE_INT
      && ! have_insn_for (COMPARE, GET_MODE (index)))
    {
      int unsignedp = TYPE_UNSIGNED (index_type);
      enum machine_mode wider_mode;
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
	set_reg_attrs_for_decl_rtl (SSA_NAME_VAR (index_expr), index);
    }

  balance_case_nodes (&case_list, NULL);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      int indent_step = ceil_log2 (TYPE_PRECISION (index_type)) + 2;
      fprintf (dump_file, ";; Expanding GIMPLE switch as decision tree:\n");
      dump_case_nodes (dump_file, case_list, indent_step, 0);
    }

  emit_case_nodes (index, case_list, default_label, index_type);
  if (default_label)
    emit_jump (default_label);
}

/* Generate a dispatch tabler, switching on INDEX_EXPR and jumping to
   one of the labels in CASE_LIST or to the DEFAULT_LABEL.
   MINVAL, MAXVAL, and RANGE are the extrema and range of the case
   labels in CASE_LIST.

   First, a jump insn is emitted.  First we try "casesi".  If that
   fails, try "tablejump".   A target *must* have one of them (or both).

   Then, a table with the target labels is emitted.

   The process is unaware of the CFG.  The caller has to fix up
   the CFG itself.  This is done in cfgexpand.c.  */     

static void
emit_case_dispatch_table (tree index_expr, tree index_type,
			  struct case_node *case_list, rtx default_label,
			  tree minval, tree maxval, tree range)
{
  int i, ncases;
  struct case_node *n;
  rtx *labelvec;
  rtx fallback_label = label_rtx (case_list->code_label);
  rtx table_label = gen_label_rtx ();

  if (! try_casesi (index_type, index_expr, minval, range,
		    table_label, default_label, fallback_label))
    {
      bool ok;

      /* Index jumptables from zero for suitable values of minval to avoid
	 a subtraction.  For the rationale see:
	 "http://gcc.gnu.org/ml/gcc-patches/2001-10/msg01234.html".  */
      if (optimize_insn_for_speed_p ()
	  && compare_tree_int (minval, 0) > 0
	  && compare_tree_int (minval, 3) < 0)
	{
	  minval = build_int_cst (index_type, 0);
	  range = maxval;
	}

      ok = try_tablejump (index_type, index_expr, minval, range,
			  table_label, default_label);
      gcc_assert (ok);
    }

  /* Get table of labels to jump to, in order of case index.  */

  ncases = tree_low_cst (range, 0) + 1;
  labelvec = XALLOCAVEC (rtx, ncases);
  memset (labelvec, 0, ncases * sizeof (rtx));

  for (n = case_list; n; n = n->right)
    {
      /* Compute the low and high bounds relative to the minimum
	 value since that should fit in a HOST_WIDE_INT while the
	 actual values may not.  */
      HOST_WIDE_INT i_low
	= tree_low_cst (fold_build2 (MINUS_EXPR, index_type,
				     n->low, minval), 1);
      HOST_WIDE_INT i_high
	= tree_low_cst (fold_build2 (MINUS_EXPR, index_type,
				     n->high, minval), 1);
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
      labelvec[i] = gen_rtx_LABEL_REF (Pmode, default_label);

  /* Output the table.  */
  emit_label (table_label);

  if (CASE_VECTOR_PC_RELATIVE || flag_pic)
    emit_jump_insn (gen_rtx_ADDR_DIFF_VEC (CASE_VECTOR_MODE,
					   gen_rtx_LABEL_REF (Pmode, table_label),
					   gen_rtvec_v (ncases, labelvec),
					   const0_rtx, const0_rtx));
  else
    emit_jump_insn (gen_rtx_ADDR_VEC (CASE_VECTOR_MODE,
				      gen_rtvec_v (ncases, labelvec)));

  /* Record no drop-through after the table.  */
  emit_barrier ();
}

/* Terminate a case (Pascal/Ada) or switch (C) statement
   in which ORIG_INDEX is the expression to be tested.
   If ORIG_TYPE is not NULL, it is the original ORIG_INDEX
   type as given in the source before any compiler conversions.
   Generate the code to test it and jump to the right place.  */

void
expand_case (gimple stmt)
{
  tree minval = NULL_TREE, maxval = NULL_TREE, range = NULL_TREE;
  rtx default_label = NULL_RTX;
  unsigned int count, uniq;
  int i, stopi = 0;
  rtx before_case, end;
  int ncases = gimple_switch_num_labels (stmt);
  tree index_expr = gimple_switch_index (stmt);
  tree index_type = TREE_TYPE (index_expr);

  tree elt;
  bitmap label_bitmap;

  /* The insn after which the case dispatch should finally
     be emitted.  Zero for a dummy.  */
  rtx start;

  /* A list of case labels; it is first built as a list and it may then
     be rearranged into a nearly balanced binary tree.  */
  struct case_node *case_list = 0;

  /* A pool for case nodes.  */
  alloc_pool case_node_pool;

  /* An ERROR_MARK occurs for various reasons including invalid data type.
     ??? Can this still happen, with GIMPLE and all?  */
  if (index_type == error_mark_node)
    return;

  /* cleanup_tree_cfg removes all SWITCH_EXPR with their index
     expressions being INTEGER_CST.  */
  gcc_assert (TREE_CODE (index_expr) != INTEGER_CST);
  
  case_node_pool = create_alloc_pool ("struct case_node pool",
				      sizeof (struct case_node),
				      100);

  do_pending_stack_adjust ();

  /* The default case, if ever taken, is the first element.  */
  elt = gimple_switch_label (stmt, 0);
  if (!CASE_LOW (elt) && !CASE_HIGH (elt))
    {
      default_label = label_rtx (CASE_LABEL (elt));
      stopi = 1;
    }

  /* Get upper and lower bounds of case values.  */
  elt = gimple_switch_label (stmt, stopi);
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
  label_bitmap = BITMAP_ALLOC (NULL);
  for (i = gimple_switch_num_labels (stmt) - 1; i >= stopi; --i)
    {
      tree low, high;
      rtx lab;

      elt = gimple_switch_label (stmt, i);
      low = CASE_LOW (elt);
      gcc_assert (low);
      high = CASE_HIGH (elt);
      gcc_assert (! high || tree_int_cst_lt (low, high));

      /* Count the elements.
	 A range counts double, since it requires two compares.  */
      count++;
      if (high)
	count++;

      /* If we have not seen this label yet, then increase the
	 number of unique case node targets seen.  */
      lab = label_rtx (CASE_LABEL (elt));
      if (bitmap_set_bit (label_bitmap, CODE_LABEL_NUMBER (lab)))
	uniq++;

      /* The canonical from of a case label in GIMPLE is that a simple case
	 has an empty CASE_HIGH.  For the casesi and tablejump expanders,
	 the back ends want simple cases to have high == low.  */
      if (! high)
	high = low;

      case_list = add_case_node (case_list, index_type, low, high,
				 CASE_LABEL (elt), case_node_pool);
    }
  BITMAP_FREE (label_bitmap);

  /* cleanup_tree_cfg removes all SWITCH_EXPR with a single
     destination, such as one with a default case only.
     It also removes cases that are out of range for the switch
     type, so we should never get a zero here.  */
  gcc_assert (count > 0);

  before_case = start = get_last_insn ();

  /* Decide how to expand this switch.
     The two options at this point are a dispatch table (casesi or
     tablejump) or a decision tree.  */

  if (expand_switch_as_decision_tree_p (range, uniq, count))
    emit_case_decision_tree (index_expr, index_type,
			     case_list, default_label);
  else
    emit_case_dispatch_table (index_expr, index_type,
			      case_list, default_label,
			      minval, maxval, range);

  before_case = NEXT_INSN (before_case);
  end = get_last_insn ();
  reorder_insns (before_case, end, start);

  free_temp_slots ();
  free_alloc_pool (case_node_pool);
}

/* Generate code to jump to LABEL if OP0 and OP1 are equal in mode MODE.  */

static void
do_jump_if_equal (enum machine_mode mode, rtx op0, rtx op1, rtx label,
		  int unsignedp)
{
  do_compare_rtx_and_jump (op0, op1, EQ, unsignedp, mode,
			   NULL_RTX, NULL_RTX, label, -1);
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
emit_case_nodes (rtx index, case_node_ptr node, rtx default_label,
		 tree index_type)
{
  /* If INDEX has an unsigned type, we must make unsigned branches.  */
  int unsignedp = TYPE_UNSIGNED (index_type);
  enum machine_mode mode = GET_MODE (index);
  enum machine_mode imode = TYPE_MODE (index_type);

  /* Handle indices detected as constant during RTL expansion.  */
  if (mode == VOIDmode)
    mode = imode;

  /* See if our parents have already tested everything for us.
     If they have, emit an unconditional jump for this node.  */
  if (node_is_bounded (node, index_type))
    emit_jump (label_rtx (node->code_label));

  else if (tree_int_cst_equal (node->low, node->high))
    {
      /* Node is single valued.  First see if the index expression matches
	 this node and then check our children, if any.  */

      do_jump_if_equal (mode, index,
			convert_modes (mode, imode,
				       expand_normal (node->low),
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
					expand_normal (node->high),
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
					expand_normal (node->high),
					unsignedp),
				       LT, NULL_RTX, mode, unsignedp,
				       label_rtx (node->left->code_label));
	      emit_case_nodes (index, node->right, default_label, index_type);
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
	      do_jump_if_equal (mode, index,
				convert_modes (mode, imode,
					       expand_normal (node->right->low),
					       unsignedp),
				label_rtx (node->right->code_label),
				unsignedp);

	      /* See if the value matches what the left hand side
		 wants.  */
	      do_jump_if_equal (mode, index,
				convert_modes (mode, imode,
					       expand_normal (node->left->low),
					       unsignedp),
				label_rtx (node->left->code_label),
				unsignedp);
	    }

	  else
	    {
	      /* Neither node is bounded.  First distinguish the two sides;
		 then emit the code for one side at a time.  */

	      tree test_label
		= build_decl (CURR_INSN_LOCATION,
			      LABEL_DECL, NULL_TREE, NULL_TREE);

	      /* See if the value is on the right.  */
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_normal (node->high),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       label_rtx (test_label));

	      /* Value must be on the left.
		 Handle the left-hand subtree.  */
	      emit_case_nodes (index, node->left, default_label, index_type);
	      /* If left-hand subtree does nothing,
		 go to default.  */
	      if (default_label)
	        emit_jump (default_label);

	      /* Code branches here for the right-hand subtree.  */
	      expand_label (test_label);
	      emit_case_nodes (index, node->right, default_label, index_type);
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
		  emit_cmp_and_jump_insns (index,
					   convert_modes
					   (mode, imode,
					    expand_normal (node->high),
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
	    do_jump_if_equal (mode, index,
			      convert_modes
			      (mode, imode,
			       expand_normal (node->right->low),
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
					    expand_normal (node->high),
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
	    do_jump_if_equal (mode, index,
			      convert_modes
			      (mode, imode,
			       expand_normal (node->left->low),
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
				      expand_normal (node->high),
				      unsignedp),
				     GT, NULL_RTX, mode, unsignedp,
				     label_rtx (node->right->code_label));
	  else
	    {
	      /* Right hand node requires testing.
		 Branch to a label where we will handle it later.  */

	      test_label = build_decl (CURR_INSN_LOCATION,
				       LABEL_DECL, NULL_TREE, NULL_TREE);
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_normal (node->high),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       label_rtx (test_label));
	    }

	  /* Value belongs to this node or to the left-hand subtree.  */

	  emit_cmp_and_jump_insns (index,
				   convert_modes
				   (mode, imode,
				    expand_normal (node->low),
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
	      if (default_label)
		emit_jump (default_label);

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
					expand_normal (node->low),
					unsignedp),
				       LT, NULL_RTX, mode, unsignedp,
				       default_label);
	    }

	  /* Value belongs to this node or to the right-hand subtree.  */

	  emit_cmp_and_jump_insns (index,
				   convert_modes
				   (mode, imode,
				    expand_normal (node->high),
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
					expand_normal (node->high),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       default_label);
	    }

	  /* Value belongs to this node or to the left-hand subtree.  */

	  emit_cmp_and_jump_insns (index,
				   convert_modes
				   (mode, imode,
				    expand_normal (node->low),
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
					expand_normal (node->high),
					unsignedp),
				       GT, NULL_RTX, mode, unsignedp,
				       default_label);
	    }

	  else if (!low_bound && high_bound)
	    {
	      emit_cmp_and_jump_insns (index,
				       convert_modes
				       (mode, imode,
					expand_normal (node->low),
					unsignedp),
				       LT, NULL_RTX, mode, unsignedp,
				       default_label);
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

	      emit_cmp_and_jump_insns (new_index, new_bound, GT, NULL_RTX,
				       mode, 1, default_label);
	    }

	  emit_jump (label_rtx (node->code_label));
	}
    }
}
