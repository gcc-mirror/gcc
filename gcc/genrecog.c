/* Generate code from machine description to recognize rtl as insns.
   Copyright (C) 1987, 88, 92, 93, 94, 95, 1997 Free Software Foundation, Inc.

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


/* This program is used to produce insn-recog.c, which contains
   a function called `recog' plus its subroutines.
   These functions contain a decision tree
   that recognizes whether an rtx, the argument given to recog,
   is a valid instruction.

   recog returns -1 if the rtx is not valid.
   If the rtx is valid, recog returns a nonnegative number
   which is the insn code number for the pattern that matched.
   This is the same as the order in the machine description of the
   entry that matched.  This number can be used as an index into various
   insn_* tables, such as insn_template, insn_outfun, and insn_n_operands
   (found in insn-output.c).

   The third argument to recog is an optional pointer to an int.
   If present, recog will accept a pattern if it matches except for
   missing CLOBBER expressions at the end.  In that case, the value
   pointed to by the optional pointer will be set to the number of
   CLOBBERs that need to be added (it should be initialized to zero by
   the caller).  If it is set nonzero, the caller should allocate a
   PARALLEL of the appropriate size, copy the initial entries, and call
   add_clobbers (found in insn-emit.c) to fill in the CLOBBERs.

   This program also generates the function `split_insns',
   which returns 0 if the rtl could not be split, or
   it returns the split rtl in a SEQUENCE.  */

#include <stdio.h>
#include "hconfig.h"
#include "rtl.h"
#include "obstack.h"

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern void free ();
extern rtx read_rtx ();

/* Data structure for a listhead of decision trees.  The alternatives
   to a node are kept in a doublely-linked list so we can easily add nodes
   to the proper place when merging.  */

struct decision_head { struct decision *first, *last; };

/* Data structure for decision tree for recognizing
   legitimate instructions.  */

struct decision
{
  int number;			/* Node number, used for labels */
  char *position;		/* String denoting position in pattern */
  RTX_CODE code;		/* Code to test for or UNKNOWN to suppress */
  char ignore_code;		/* If non-zero, need not test code */
  char ignore_mode;		/* If non-zero, need not test mode */
  int veclen;			/* Length of vector, if nonzero */
  enum machine_mode mode;	/* Machine mode of node */
  char enforce_mode;		/* If non-zero, test `mode' */
  char retest_code, retest_mode; /* See write_tree_1 */
  int test_elt_zero_int;	/* Nonzero if should test XINT (rtl, 0) */
  int elt_zero_int;		/* Required value for XINT (rtl, 0) */
  int test_elt_one_int;		/* Nonzero if should test XINT (rtl, 1) */
  int elt_one_int;		/* Required value for XINT (rtl, 1) */
  int test_elt_zero_wide;	/* Nonzero if should test XWINT (rtl, 0) */
  HOST_WIDE_INT elt_zero_wide;	/* Required value for XWINT (rtl, 0) */
  char *tests;			/* If nonzero predicate to call */
  int pred;			/* `preds' index of predicate or -1 */
  char *c_test;			/* Additional test to perform */
  struct decision_head success;	/* Nodes to test on success */
  int insn_code_number;		/* Insn number matched, if success */
  int num_clobbers_to_add;	/* Number of CLOBBERs to be added to pattern */
  struct decision *next;	/* Node to test on failure */
  struct decision *prev;	/* Node whose failure tests us */
  struct decision *afterward;	/* Node to test on success, but failure of
				   successor nodes */
  int opno;			/* Operand number, if >= 0 */
  int dupno;			/* Number of operand to compare against */
  int label_needed;		/* Nonzero if label needed when writing tree */
  int subroutine_number;	/* Number of subroutine this node starts */
};

#define SUBROUTINE_THRESHOLD 50

static int next_subroutine_number;

/* We can write two types of subroutines: One for insn recognition and
   one to split insns.  This defines which type is being written.  */

enum routine_type {RECOG, SPLIT};

/* Next available node number for tree nodes.  */

static int next_number;

/* Next number to use as an insn_code.  */

static int next_insn_code;

/* Similar, but counts all expressions in the MD file; used for
   error messages.  */

static int next_index;

/* Record the highest depth we ever have so we know how many variables to
   allocate in each subroutine we make.  */

static int max_depth;

/* This table contains a list of the rtl codes that can possibly match a
   predicate defined in recog.c.  The function `not_both_true' uses it to
   deduce that there are no expressions that can be matches by certain pairs
   of tree nodes.  Also, if a predicate can match only one code, we can
   hardwire that code into the node testing the predicate.  */

static struct pred_table
{
  char *name;
  RTX_CODE codes[NUM_RTX_CODE];
} preds[]
  = {{"general_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,
			  LABEL_REF, SUBREG, REG, MEM}},
#ifdef PREDICATE_CODES
     PREDICATE_CODES
#endif
     {"address_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,
			  LABEL_REF, SUBREG, REG, MEM, PLUS, MINUS, MULT}},
     {"register_operand", {SUBREG, REG}},
     {"scratch_operand", {SCRATCH, REG}},
     {"immediate_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,
			    LABEL_REF}},
     {"const_int_operand", {CONST_INT}},
     {"const_double_operand", {CONST_INT, CONST_DOUBLE}},
     {"nonimmediate_operand", {SUBREG, REG, MEM}},
     {"nonmemory_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,
			    LABEL_REF, SUBREG, REG}},
     {"push_operand", {MEM}},
     {"memory_operand", {SUBREG, MEM}},
     {"indirect_operand", {SUBREG, MEM}},
     {"comparison_operator", {EQ, NE, LE, LT, GE, GT, LEU, LTU, GEU, GTU}},
     {"mode_independent_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,
				   LABEL_REF, SUBREG, REG, MEM}}};

#define NUM_KNOWN_PREDS (sizeof preds / sizeof preds[0])

static struct decision_head make_insn_sequence PROTO((rtx, enum routine_type));
static struct decision *add_to_sequence PROTO((rtx, struct decision_head *,
					       char *));
static int not_both_true	PROTO((struct decision *, struct decision *,
				       int));
static int position_merit	PROTO((struct decision *, enum machine_mode,
				       enum rtx_code));
static struct decision_head merge_trees PROTO((struct decision_head,
					       struct decision_head));
static int break_out_subroutines PROTO((struct decision_head,
					enum routine_type, int));
static void write_subroutine	PROTO((struct decision *, enum routine_type));
static void write_tree_1	PROTO((struct decision *, char *,
				       struct decision *, enum routine_type));
static void print_code		PROTO((enum rtx_code));
static int same_codes		PROTO((struct decision *, enum rtx_code));
static void clear_codes		PROTO((struct decision *));
static int same_modes		PROTO((struct decision *, enum machine_mode));
static void clear_modes		PROTO((struct decision *));
static void write_tree		PROTO((struct decision *, char *,
				       struct decision *, int,
				       enum routine_type));
static void change_state	PROTO((char *, char *, int));
static char *copystr		PROTO((char *));
static void mybzero		PROTO((char *, unsigned));
static void mybcopy		PROTO((char *, char *, unsigned));
static char *concat		PROTO((char *, char *));
static void fatal		PROTO((char *));
char *xrealloc			PROTO((char *, unsigned));
char *xmalloc			PROTO((unsigned));
void fancy_abort		PROTO((void));

/* Construct and return a sequence of decisions
   that will recognize INSN.

   TYPE says what type of routine we are recognizing (RECOG or SPLIT).  */

static struct decision_head
make_insn_sequence (insn, type)
     rtx insn;
     enum routine_type type;
{
  rtx x;
  char *c_test = XSTR (insn, type == RECOG ? 2 : 1);
  struct decision *last;
  struct decision_head head;

  if (XVECLEN (insn, type == RECOG) == 1)
    x = XVECEXP (insn, type == RECOG, 0);
  else
    {
      x = rtx_alloc (PARALLEL);
      XVEC (x, 0) = XVEC (insn, type == RECOG);
      PUT_MODE (x, VOIDmode);
    }

  last = add_to_sequence (x, &head, "");

  if (c_test[0])
    last->c_test = c_test;
  last->insn_code_number = next_insn_code;
  last->num_clobbers_to_add = 0;

  /* If this is not a DEFINE_SPLIT and X is a PARALLEL, see if it ends with a
     group of CLOBBERs of (hard) registers or MATCH_SCRATCHes.  If so, set up
     to recognize the pattern without these CLOBBERs.  */

  if (type == RECOG && GET_CODE (x) == PARALLEL)
    {
      int i;

      for (i = XVECLEN (x, 0); i > 0; i--)
	if (GET_CODE (XVECEXP (x, 0, i - 1)) != CLOBBER
	    || (GET_CODE (XEXP (XVECEXP (x, 0, i - 1), 0)) != REG
		&& GET_CODE (XEXP (XVECEXP (x, 0, i - 1), 0)) != MATCH_SCRATCH))
	  break;

      if (i != XVECLEN (x, 0))
	{
	  rtx new;
	  struct decision_head clobber_head;

	  if (i == 1)
	    new = XVECEXP (x, 0, 0);
	  else
	    {
	      int j;

	      new = rtx_alloc (PARALLEL);
	      XVEC (new, 0) = rtvec_alloc (i);
	      for (j = i - 1; j >= 0; j--)
		XVECEXP (new, 0, j) = XVECEXP (x, 0, j);
	    }

	  last = add_to_sequence (new, &clobber_head, "");

	  if (c_test[0])
	    last->c_test = c_test;
	  last->insn_code_number = next_insn_code;
	  last->num_clobbers_to_add = XVECLEN (x, 0) - i;

	  head = merge_trees (head, clobber_head);
	}
    }

  next_insn_code++;

  if (type == SPLIT)
    /* Define the subroutine we will call below and emit in genemit.  */
    printf ("extern rtx gen_split_%d ();\n", last->insn_code_number);

  return head;
}

/* Create a chain of nodes to verify that an rtl expression matches
   PATTERN.

   LAST is a pointer to the listhead in the previous node in the chain (or
   in the calling function, for the first node).

   POSITION is the string representing the current position in the insn.

   A pointer to the final node in the chain is returned.  */

static struct decision *
add_to_sequence (pattern, last, position)
     rtx pattern;
     struct decision_head *last;
     char *position;
{
  register RTX_CODE code;
  register struct decision *new
    = (struct decision *) xmalloc (sizeof (struct decision));
  struct decision *this;
  char *newpos;
  register char *fmt;
  register int i;
  int depth = strlen (position);
  int len;

  if (depth > max_depth)
    max_depth = depth;

  new->number = next_number++;
  new->position = copystr (position);
  new->ignore_code = 0;
  new->ignore_mode = 0;
  new->enforce_mode = 1;
  new->retest_code = new->retest_mode = 0;
  new->veclen = 0;
  new->test_elt_zero_int = 0;
  new->test_elt_one_int = 0;
  new->test_elt_zero_wide = 0;
  new->elt_zero_int = 0;
  new->elt_one_int = 0;
  new->elt_zero_wide = 0;
  new->tests = 0;
  new->pred = -1;
  new->c_test = 0;
  new->success.first = new->success.last = 0;
  new->insn_code_number = -1;
  new->num_clobbers_to_add = 0;
  new->next = 0;
  new->prev = 0;
  new->afterward = 0;
  new->opno = -1;
  new->dupno = -1;
  new->label_needed = 0;
  new->subroutine_number = 0;

  this = new;

  last->first = last->last = new;

  newpos = (char *) alloca (depth + 2);
  strcpy (newpos, position);
  newpos[depth + 1] = 0;

 restart:

  new->mode = GET_MODE (pattern);
  new->code = code = GET_CODE (pattern);

  switch (code)
    {
    case MATCH_OPERAND:
    case MATCH_SCRATCH:
    case MATCH_OPERATOR:
    case MATCH_PARALLEL:
      new->opno = XINT (pattern, 0);
      new->code = (code == MATCH_PARALLEL ? PARALLEL : UNKNOWN);
      new->enforce_mode = 0;

      if (code == MATCH_SCRATCH)
	new->tests = "scratch_operand";
      else
	new->tests = XSTR (pattern, 1);

      if (*new->tests == 0)
	new->tests = 0;

      /* See if we know about this predicate and save its number.  If we do,
	 and it only accepts one code, note that fact.  The predicate
	 `const_int_operand' only tests for a CONST_INT, so if we do so we
	 can avoid calling it at all.

	 Finally, if we know that the predicate does not allow CONST_INT, we
	 know that the only way the predicate can match is if the modes match
	 (here we use the kludge of relying on the fact that "address_operand"
	 accepts CONST_INT; otherwise, it would have to be a special case),
	 so we can test the mode (but we need not).  This fact should
	 considerably simplify the generated code.  */

      if (new->tests)
	{
	  for (i = 0; i < NUM_KNOWN_PREDS; i++)
	    if (! strcmp (preds[i].name, new->tests))
	      {
		int j;
		int allows_const_int = 0;

		new->pred = i;

		if (preds[i].codes[1] == 0 && new->code == UNKNOWN)
		  {
		    new->code = preds[i].codes[0];
		    if (! strcmp ("const_int_operand", new->tests))
		      new->tests = 0, new->pred = -1;
		  }

		for (j = 0; j < NUM_RTX_CODE && preds[i].codes[j] != 0; j++)
		  if (preds[i].codes[j] == CONST_INT)
		    allows_const_int = 1;

		if (! allows_const_int)
		  new->enforce_mode = new->ignore_mode= 1;

		break;
	      }

#ifdef PREDICATE_CODES
	  /* If the port has a list of the predicates it uses but omits
	     one, warn.  */
	  if (i == NUM_KNOWN_PREDS)
	    fprintf (stderr, "Warning: `%s' not in PREDICATE_CODES\n",
		     new->tests);
#endif
	}

      if (code == MATCH_OPERATOR || code == MATCH_PARALLEL)
	{
	  for (i = 0; i < XVECLEN (pattern, 2); i++)
	    {
	      newpos[depth] = i + (code == MATCH_OPERATOR ? '0': 'a');
	      new = add_to_sequence (XVECEXP (pattern, 2, i),
				     &new->success, newpos);
	    }
	}

      return new;

    case MATCH_OP_DUP:
      new->opno = XINT (pattern, 0);
      new->dupno = XINT (pattern, 0);
      new->code = UNKNOWN;
      new->tests = 0;
      for (i = 0; i < XVECLEN (pattern, 1); i++)
	{
	  newpos[depth] = i + '0';
	  new = add_to_sequence (XVECEXP (pattern, 1, i),
				 &new->success, newpos);
	}
      return new;

    case MATCH_DUP:
    case MATCH_PAR_DUP:
      new->dupno = XINT (pattern, 0);
      new->code = UNKNOWN;
      new->enforce_mode = 0;
      return new;

    case ADDRESS:
      pattern = XEXP (pattern, 0);
      goto restart;

    case SET:
      newpos[depth] = '0';
      new = add_to_sequence (SET_DEST (pattern), &new->success, newpos);
      this->success.first->enforce_mode = 1;
      newpos[depth] = '1';
      new = add_to_sequence (SET_SRC (pattern), &new->success, newpos);

      /* If set are setting CC0 from anything other than a COMPARE, we
	 must enforce the mode so that we do not produce ambiguous insns.  */
      if (GET_CODE (SET_DEST (pattern)) == CC0
	  && GET_CODE (SET_SRC (pattern)) != COMPARE)
	this->success.first->enforce_mode = 1;
      return new;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
    case STRICT_LOW_PART:
      newpos[depth] = '0';
      new = add_to_sequence (XEXP (pattern, 0), &new->success, newpos);
      this->success.first->enforce_mode = 1;
      return new;

    case SUBREG:
      this->test_elt_one_int = 1;
      this->elt_one_int = XINT (pattern, 1);
      newpos[depth] = '0';
      new = add_to_sequence (XEXP (pattern, 0), &new->success, newpos);
      this->success.first->enforce_mode = 1;
      return new;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      newpos[depth] = '0';
      new = add_to_sequence (XEXP (pattern, 0), &new->success, newpos);
      this->success.first->enforce_mode = 1;
      newpos[depth] = '1';
      new = add_to_sequence (XEXP (pattern, 1), &new->success, newpos);
      newpos[depth] = '2';
      new = add_to_sequence (XEXP (pattern, 2), &new->success, newpos);
      return new;

    case EQ:   case NE:   case LE:   case LT:   case GE:  case GT:
    case LEU:  case LTU:  case GEU:  case GTU:
      /* If the first operand is (cc0), we don't have to do anything
	 special.  */
      if (GET_CODE (XEXP (pattern, 0)) == CC0)
	break;

      /* ... fall through ...  */
      
    case COMPARE:
      /* Enforce the mode on the first operand to avoid ambiguous insns.  */
      newpos[depth] = '0';
      new = add_to_sequence (XEXP (pattern, 0), &new->success, newpos);
      this->success.first->enforce_mode = 1;
      newpos[depth] = '1';
      new = add_to_sequence (XEXP (pattern, 1), &new->success, newpos);
      return new;
      
    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      newpos[depth] = '0' + i;
      if (fmt[i] == 'e' || fmt[i] == 'u')
	new = add_to_sequence (XEXP (pattern, i), &new->success, newpos);
      else if (fmt[i] == 'i' && i == 0)
	{
	  this->test_elt_zero_int = 1;
	  this->elt_zero_int = XINT (pattern, i);
	}
      else if (fmt[i] == 'i' && i == 1)
	{
	  this->test_elt_one_int = 1;
	  this->elt_one_int = XINT (pattern, i);
	}
      else if (fmt[i] == 'w' && i == 0)
	{
	  this->test_elt_zero_wide = 1;
	  this->elt_zero_wide = XWINT (pattern, i);
	}
      else if (fmt[i] == 'E')
	{
	  register int j;
	  /* We do not handle a vector appearing as other than
	     the first item, just because nothing uses them
	     and by handling only the special case
	     we can use one element in newpos for either
	     the item number of a subexpression
	     or the element number in a vector.  */
	  if (i != 0)
	    abort ();
	  this->veclen = XVECLEN (pattern, i);
	  for (j = 0; j < XVECLEN (pattern, i); j++)
	    {
	      newpos[depth] = 'a' + j;
	      new = add_to_sequence (XVECEXP (pattern, i, j),
				     &new->success, newpos);
	    }
	}
      else if (fmt[i] != '0')
	abort ();
    }
  return new;
}

/* Return 1 if we can prove that there is no RTL that can match both
   D1 and D2.  Otherwise, return 0 (it may be that there is an RTL that
   can match both or just that we couldn't prove there wasn't such an RTL).

   TOPLEVEL is non-zero if we are to only look at the top level and not
   recursively descend.  */

static int
not_both_true (d1, d2, toplevel)
     struct decision *d1, *d2;
     int toplevel;
{
  struct decision *p1, *p2;

  /* If they are both to test modes and the modes are different, they aren't
     both true.  Similarly for codes, integer elements, and vector lengths.  */

  if ((d1->enforce_mode && d2->enforce_mode
       && d1->mode != VOIDmode && d2->mode != VOIDmode && d1->mode != d2->mode)
      || (d1->code != UNKNOWN && d2->code != UNKNOWN && d1->code != d2->code)
      || (d1->test_elt_zero_int && d2->test_elt_zero_int
	  && d1->elt_zero_int != d2->elt_zero_int)
      || (d1->test_elt_one_int && d2->test_elt_one_int
	  && d1->elt_one_int != d2->elt_one_int)
      || (d1->test_elt_zero_wide && d2->test_elt_zero_wide
	  && d1->elt_zero_wide != d2->elt_zero_wide)
      || (d1->veclen && d2->veclen && d1->veclen != d2->veclen))
    return 1;

  /* If either is a wild-card MATCH_OPERAND without a predicate, it can match
     absolutely anything, so we can't say that no intersection is possible.
     This case is detected by having a zero TESTS field with a code of
     UNKNOWN.  */

  if ((d1->tests == 0 && d1->code == UNKNOWN)
      || (d2->tests == 0 && d2->code == UNKNOWN))
    return 0;

  /* If either has a predicate that we know something about, set things up so
     that D1 is the one that always has a known predicate.  Then see if they
     have any codes in common.  */

  if (d1->pred >= 0 || d2->pred >= 0)
    {
      int i, j;

      if (d2->pred >= 0)
	p1 = d1, d1 = d2, d2 = p1;

      /* If D2 tests an explicit code, see if it is in the list of valid codes
	 for D1's predicate.  */
      if (d2->code != UNKNOWN)
	{
	  for (i = 0; i < NUM_RTX_CODE && preds[d1->pred].codes[i] != 0; i++)
	    if (preds[d1->pred].codes[i] == d2->code)
	      break;

	  if (preds[d1->pred].codes[i] == 0)
	    return 1;
	}

      /* Otherwise see if the predicates have any codes in common.  */

      else if (d2->pred >= 0)
	{
	  for (i = 0; i < NUM_RTX_CODE && preds[d1->pred].codes[i] != 0; i++)
	    {
	      for (j = 0; j < NUM_RTX_CODE; j++)
		if (preds[d2->pred].codes[j] == 0
		    || preds[d2->pred].codes[j] == preds[d1->pred].codes[i])
		  break;

	      if (preds[d2->pred].codes[j] != 0)
		break;
	    }

	  if (preds[d1->pred].codes[i] == 0)
	    return 1;
	}
    }

  /* If we got here, we can't prove that D1 and D2 cannot both be true.
     If we are only to check the top level, return 0.  Otherwise, see if
     we can prove that all choices in both successors are mutually
     exclusive.  If either does not have any successors, we can't prove
     they can't both be true.  */

  if (toplevel || d1->success.first == 0 || d2->success.first == 0)
    return 0;

  for (p1 = d1->success.first; p1; p1 = p1->next)
    for (p2 = d2->success.first; p2; p2 = p2->next)
      if (! not_both_true (p1, p2, 0))
	return 0;

  return 1;
}

/* Assuming that we can reorder all the alternatives at a specific point in
   the tree (see discussion in merge_trees), we would prefer an ordering of
   nodes where groups of consecutive nodes test the same mode and, within each
   mode, groups of nodes test the same code.  With this order, we can
   construct nested switch statements, the inner one to test the code and
   the outer one to test the mode.

   We would like to list nodes testing for specific codes before those
   that test predicates to avoid unnecessary function calls.  Similarly,
   tests for specific modes should precede nodes that allow any mode.

   This function returns the merit (with 0 being the best) of inserting
   a test involving the specified MODE and CODE after node P.  If P is
   zero, we are to determine the merit of inserting the test at the front
   of the list.  */

static int
position_merit (p, mode, code)
     struct decision *p;
     enum machine_mode mode;
     enum rtx_code code;
{
  enum machine_mode p_mode;

  /* The only time the front of the list is anything other than the worst
     position is if we are testing a mode that isn't VOIDmode.  */
  if (p == 0)
    return mode == VOIDmode ? 3 : 2;

  p_mode = p->enforce_mode ? p->mode : VOIDmode;

  /* The best case is if the codes and modes both match.  */
  if (p_mode == mode && p->code== code)
    return 0;

  /* If the codes don't match, the next best case is if the modes match.
     In that case, the best position for this node depends on whether
     we are testing for a specific code or not.  If we are, the best place
     is after some other test for an explicit code and our mode or after
     the last test in the previous mode if every test in our mode is for
     an unknown code.

     If we are testing for UNKNOWN, then the next best case is at the end of
     our mode.  */

  if ((code != UNKNOWN
       && ((p_mode == mode && p->code != UNKNOWN)
	   || (p_mode != mode && p->next
	       && (p->next->enforce_mode ? p->next->mode : VOIDmode) == mode
	       && (p->next->code == UNKNOWN))))
      || (code == UNKNOWN && p_mode == mode
	  && (p->next == 0
	      || (p->next->enforce_mode ? p->next->mode : VOIDmode) != mode)))
    return 1;

  /* The third best case occurs when nothing is testing MODE.  If MODE
     is not VOIDmode, then the third best case is after something of any
     mode that is not VOIDmode.  If we are testing VOIDmode, the third best
     place is the end of the list.  */

  if (p_mode != mode
      && ((mode != VOIDmode && p_mode != VOIDmode)
	  || (mode == VOIDmode && p->next == 0)))
    return 2;

  /* Otherwise, we have the worst case.  */
  return 3;
}

/* Merge two decision tree listheads OLDH and ADDH,
   modifying OLDH destructively, and return the merged tree.  */

static struct decision_head
merge_trees (oldh, addh)
     register struct decision_head oldh, addh;
{
  struct decision *add, *next;

  if (oldh.first == 0)
    return addh;

  if (addh.first == 0)
    return oldh;

  /* If we are adding things at different positions, something is wrong.  */
  if (strcmp (oldh.first->position, addh.first->position))
    abort ();

  for (add = addh.first; add; add = next)
    {
      enum machine_mode add_mode = add->enforce_mode ? add->mode : VOIDmode;
      struct decision *best_position = 0;
      int best_merit = 4;
      struct decision *old;

      next = add->next;

      /* The semantics of pattern matching state that the tests are done in
	 the order given in the MD file so that if an insn matches two
	 patterns, the first one will be used.  However, in practice, most,
	 if not all, patterns are unambiguous so that their order is 
	 independent.  In that case, we can merge identical tests and
	 group all similar modes and codes together.

	 Scan starting from the end of OLDH until we reach a point
	 where we reach the head of the list or where we pass a pattern
	 that could also be true if NEW is true.  If we find an identical
	 pattern, we can merge them.  Also, record the last node that tests
	 the same code and mode and the last one that tests just the same mode.

	 If we have no match, place NEW after the closest match we found.  */
	 
      for (old = oldh.last; old; old = old->prev)
	{
	  int our_merit;

	  /* If we don't have anything to test except an additional test,
	     do not consider the two nodes equal.  If we did, the test below
	     would cause an infinite recursion.  */
	  if (old->tests == 0 && old->test_elt_zero_int == 0
	      && old->test_elt_one_int == 0 && old->veclen == 0
	      && old->test_elt_zero_wide == 0
	      && old->dupno == -1 && old->mode == VOIDmode
	      && old->code == UNKNOWN
	      && (old->c_test != 0 || add->c_test != 0))
	    ;

	  else if ((old->tests == add->tests
		    || (old->pred >= 0 && old->pred == add->pred)
		    || (old->tests && add->tests
			&& !strcmp (old->tests, add->tests)))
		   && old->test_elt_zero_int == add->test_elt_zero_int
		   && old->elt_zero_int == add->elt_zero_int
		   && old->test_elt_one_int == add->test_elt_one_int
		   && old->elt_one_int == add->elt_one_int
		   && old->test_elt_zero_wide == add->test_elt_zero_wide
		   && old->elt_zero_wide == add->elt_zero_wide
		   && old->veclen == add->veclen
		   && old->dupno == add->dupno
		   && old->opno == add->opno
		   && old->code == add->code
		   && old->enforce_mode == add->enforce_mode
		   && old->mode == add->mode)
	    {
	      /* If the additional test is not the same, split both nodes
		 into nodes that just contain all things tested before the
		 additional test and nodes that contain the additional test
		 and actions when it is true.  This optimization is important
		 because of the case where we have almost identical patterns
		 with different tests on target flags.  */

	      if (old->c_test != add->c_test
		  && ! (old->c_test && add->c_test
			&& !strcmp (old->c_test, add->c_test)))
		{
		  if (old->insn_code_number >= 0 || old->opno >= 0)
		    {
		      struct decision *split
			= (struct decision *) xmalloc (sizeof (struct decision));

		      mybcopy ((char *) old, (char *) split,
			       sizeof (struct decision));

		      old->success.first = old->success.last = split;
		      old->c_test = 0;
		      old->opno = -1;
		      old->insn_code_number = -1;
		      old->num_clobbers_to_add = 0;

		      split->number = next_number++;
		      split->next = split->prev = 0;
		      split->mode = VOIDmode;
		      split->code = UNKNOWN;
		      split->veclen = 0;
		      split->test_elt_zero_int = 0;
		      split->test_elt_one_int = 0;
		      split->test_elt_zero_wide = 0;
		      split->tests = 0;
		      split->pred = -1;
		      split->dupno = -1;
		    }

		  if (add->insn_code_number >= 0 || add->opno >= 0)
		    {
		      struct decision *split
			= (struct decision *) xmalloc (sizeof (struct decision));

		      mybcopy ((char *) add, (char *) split,
			       sizeof (struct decision));

		      add->success.first = add->success.last = split;
		      add->c_test = 0;
		      add->opno = -1;
		      add->insn_code_number = -1;
		      add->num_clobbers_to_add = 0;

		      split->number = next_number++;
		      split->next = split->prev = 0;
		      split->mode = VOIDmode;
		      split->code = UNKNOWN;
		      split->veclen = 0;
		      split->test_elt_zero_int = 0;
		      split->test_elt_one_int = 0;
		      split->test_elt_zero_wide = 0;
		      split->tests = 0;
		      split->pred = -1;
		      split->dupno = -1;
		    }
		}

	      if (old->insn_code_number >= 0 && add->insn_code_number >= 0)
		{
		  /* If one node is for a normal insn and the second is
		     for the base insn with clobbers stripped off, the
		     second node should be ignored.  */

		  if (old->num_clobbers_to_add == 0
		      && add->num_clobbers_to_add > 0)
		    /* Nothing to do here.  */
		    ;
		  else if (old->num_clobbers_to_add > 0
			   && add->num_clobbers_to_add == 0)
		    {
		      /* In this case, replace OLD with ADD.  */
		      old->insn_code_number = add->insn_code_number;
		      old->num_clobbers_to_add = 0;
		    }
		  else
		    fatal ("Two actions at one point in tree");
		}

	      if (old->insn_code_number == -1)
		old->insn_code_number = add->insn_code_number;
	      old->success = merge_trees (old->success, add->success);
	      add = 0;
	      break;
	    }

	  /* Unless we have already found the best possible insert point,
	     see if this position is better.  If so, record it.  */

	  if (best_merit != 0
	      && ((our_merit = position_merit (old, add_mode, add->code))
		  < best_merit))
	    best_merit = our_merit, best_position = old;

	  if (! not_both_true (old, add, 0))
	    break;
	}

      /* If ADD was duplicate, we are done.  */
      if (add == 0)
	continue;

      /* Otherwise, find the best place to insert ADD.  Normally this is
	 BEST_POSITION.  However, if we went all the way to the top of
	 the list, it might be better to insert at the top.  */

      if (best_position == 0)
	abort ();

      if (old == 0
	  && position_merit (NULL_PTR, add_mode, add->code) < best_merit)
	{
	  add->prev = 0;
	  add->next = oldh.first;
	  oldh.first->prev = add;
	  oldh.first = add;
	}

      else
	{
	  add->prev = best_position;
	  add->next = best_position->next;
	  best_position->next = add;
	  if (best_position == oldh.last)
	    oldh.last = add;
	  else
	    add->next->prev = add;
	}
    }

  return oldh;
}

/* Count the number of subnodes of HEAD.  If the number is high enough,
   make the first node in HEAD start a separate subroutine in the C code
   that is generated.

   TYPE gives the type of routine we are writing.

   INITIAL is non-zero if this is the highest-level node.  We never write
   it out here.  */

static int
break_out_subroutines (head, type, initial)
     struct decision_head head;
     enum routine_type type;
     int initial;
{
  int size = 0;
  struct decision *sub;

  for (sub = head.first; sub; sub = sub->next)
    size += 1 + break_out_subroutines (sub->success, type, 0);

  if (size > SUBROUTINE_THRESHOLD && ! initial)
    {
      head.first->subroutine_number = ++next_subroutine_number;
      write_subroutine (head.first, type);
      size = 1;
    }
  return size;
}

/* Write out a subroutine of type TYPE to do comparisons starting at node
   TREE.  */

static void
write_subroutine (tree, type)
     struct decision *tree;
     enum routine_type type;
{
  int i;

  if (type == SPLIT)
    printf ("rtx\nsplit");
  else
    printf ("int\nrecog");

  if (tree != 0 && tree->subroutine_number > 0)
    printf ("_%d", tree->subroutine_number);
  else if (type == SPLIT)
    printf ("_insns");

  printf (" (x0, insn");
  if (type == RECOG)
    printf (", pnum_clobbers");

  printf (")\n");
  printf ("     register rtx x0;\n     rtx insn;\n");
  if (type == RECOG)
    printf ("     int *pnum_clobbers;\n");

  printf ("{\n");
  printf ("  register rtx *ro = &recog_operand[0];\n");

  printf ("  register rtx ");
  for (i = 1; i < max_depth; i++)
    printf ("x%d, ", i);

  printf ("x%d;\n", max_depth);
  printf ("  %s tem;\n", type == SPLIT ? "rtx" : "int");
  write_tree (tree, "", NULL_PTR, 1, type);
  printf (" ret0: return %d;\n}\n\n", type == SPLIT ? 0 : -1);
}

/* This table is used to indent the recog_* functions when we are inside
   conditions or switch statements.  We only support small indentations
   and always indent at least two spaces.  */

static char *indents[]
  = {"  ", "  ", "  ", "   ", "    ", "     ", "      ", "       ",
     "\t", "\t ", "\t  ", "\t   ", "\t    ", "\t     ", "\t      ",
     "\t\t", "\t\t ", "\t\t  ", "\t\t   ", "\t\t    ", "\t\t     "};

/* Write out C code to perform the decisions in TREE for a subroutine of
   type TYPE.  If all of the choices fail, branch to node AFTERWARD, if
   non-zero, otherwise return.  PREVPOS is the position of the node that
   branched to this test.

   When we merged all alternatives, we tried to set up a convenient order.
   Specifically, tests involving the same mode are all grouped together,
   followed by a group that does not contain a mode test.  Within each group
   of the same mode, we also group tests with the same code, followed by a
   group that does not test a code.

   Occasionally, we cannot arbitrarily reorder the tests so that multiple
   sequence of groups as described above are present.

   We generate two nested switch statements, the outer statement for
   testing modes, and the inner switch for testing RTX codes.  It is
   not worth optimizing cases when only a small number of modes or 
   codes is tested, since the compiler can do that when compiling the
   resulting function.   We do check for when every test is the same mode
   or code.  */

static void
write_tree_1 (tree, prevpos, afterward, type)
     struct decision *tree;
     char *prevpos;
     struct decision *afterward;
     enum routine_type type;
{
  register struct decision *p, *p1;
  register int depth = tree ? strlen (tree->position) : 0;
  enum machine_mode switch_mode = VOIDmode;
  RTX_CODE switch_code = UNKNOWN;
  int uncond = 0;
  char modemap[NUM_MACHINE_MODES];
  char codemap[NUM_RTX_CODE];
  int indent = 2;
  int i;

  /* One tricky area is what is the exact state when we branch to a
     node's label.  There are two cases where we branch: when looking at
     successors to a node, or when a set of tests fails.

     In the former case, we are always branching to the first node in a
     decision list and we want all required tests to be performed.  We
     put the labels for such nodes in front of any switch or test statements.
     These branches are done without updating the position to that of the
     target node.

     In the latter case, we are branching to a node that is not the first
     node in a decision list.  We have already checked that it is possible
     for both the node we originally tested at this level and the node we
     are branching to to be both match some pattern.  That means that they
     usually will be testing the same mode and code.  So it is normally safe
     for such labels to be inside switch statements, since the tests done
     by virtue of arriving at that label will usually already have been
     done.  The exception is a branch from a node that does not test a
     mode or code to one that does.  In such cases, we set the `retest_mode'
     or `retest_code' flags.  That will ensure that we start a new switch
     at that position and put the label before the switch. 

     The branches in the latter case must set the position to that of the
     target node.  */


  printf ("\n");
  if (tree && tree->subroutine_number == 0)
    {
      printf ("  L%d:\n", tree->number);
      tree->label_needed = 0;
    }

  if (tree)
    {
      change_state (prevpos, tree->position, 2);
      prevpos = tree->position;
    }

  for (p = tree; p; p = p->next)
    {
      enum machine_mode mode = p->enforce_mode ? p->mode : VOIDmode;
      int need_bracket;
      int wrote_bracket = 0;
      int inner_indent;

      if (p->success.first == 0 && p->insn_code_number < 0)
	abort ();

      /* Find the next alternative to p that might be true when p is true.
	 Test that one next if p's successors fail.  */

      for (p1 = p->next; p1 && not_both_true (p, p1, 1); p1 = p1->next)
	;
      p->afterward = p1;

      if (p1)
	{
	  if (mode == VOIDmode && p1->enforce_mode && p1->mode != VOIDmode)
	    p1->retest_mode = 1;
	  if (p->code == UNKNOWN && p1->code != UNKNOWN)
	    p1->retest_code = 1;
	  p1->label_needed = 1;
	}

      /* If we have a different code or mode than the last node and
	 are in a switch on codes, we must either end the switch or
	 go to another case.  We must also end the switch if this
	 node needs a label and to retest either the mode or code.  */

      if (switch_code != UNKNOWN
	  && (switch_code != p->code || switch_mode != mode
	      || (p->label_needed && (p->retest_mode || p->retest_code))))
	{
	  enum rtx_code code = p->code;

	  /* If P is testing a predicate that we know about and we haven't
	     seen any of the codes that are valid for the predicate, we
	     can write a series of "case" statement, one for each possible
	     code.  Since we are already in a switch, these redundant tests
	     are very cheap and will reduce the number of predicate called.  */

	  if (p->pred >= 0)
	    {
	      for (i = 0; i < NUM_RTX_CODE && preds[p->pred].codes[i] != 0; i++)
		if (codemap[(int) preds[p->pred].codes[i]])
		  break;

	      if (preds[p->pred].codes[i] == 0)
		code = MATCH_OPERAND;
	    }

	  if (code == UNKNOWN || codemap[(int) code]
	      || switch_mode != mode
	      || (p->label_needed && (p->retest_mode || p->retest_code)))
	    {
	      printf ("%s}\n", indents[indent - 2]);
	      switch_code = UNKNOWN;
	      indent -= 4;
	    }
	  else
	    {
	      if (! uncond)
		printf ("%sbreak;\n", indents[indent]);

	      if (code == MATCH_OPERAND)
		{
		  for (i = 0; i < NUM_RTX_CODE && preds[p->pred].codes[i] != 0; i++)
		    {
		      printf ("%scase ", indents[indent - 2]);
		      print_code (preds[p->pred].codes[i]);
		      printf (":\n");
		      codemap[(int) preds[p->pred].codes[i]] = 1;
		    }
		}
	      else
		{
		  printf ("%scase ", indents[indent - 2]);
		  print_code (code);
		  printf (":\n");
		  codemap[(int) p->code] = 1;
		}

	      switch_code = code;
	    }

	  uncond = 0;
	}

      /* If we were previously in a switch on modes and now have a different
	 mode, end at least the case, and maybe end the switch if we are
	 not testing a mode or testing a mode whose case we already saw.  */

      if (switch_mode != VOIDmode
	  && (switch_mode != mode || (p->label_needed && p->retest_mode)))
	{
	  if (mode == VOIDmode || modemap[(int) mode]
	      || (p->label_needed && p->retest_mode))
	    {
	      printf ("%s}\n", indents[indent - 2]);
	      switch_mode = VOIDmode;
	      indent -= 4;
	    }
	  else
	    {
	      if (! uncond)
		printf ("      break;\n");
	      printf ("    case %smode:\n", GET_MODE_NAME (mode));
	      switch_mode = mode;
	      modemap[(int) mode] = 1;
	    }

	  uncond = 0;
	}

      /* If we are about to write dead code, something went wrong.  */
      if (! p->label_needed && uncond)
	abort ();

      /* If we need a label and we will want to retest the mode or code at
	 that label, write the label now.  We have already ensured that
	 things will be valid for the test.  */

      if (p->label_needed && (p->retest_mode || p->retest_code))
	{
	  printf ("%sL%d:\n", indents[indent - 2], p->number);
	  p->label_needed = 0;
	}

      uncond = 0;

      /* If we are not in any switches, see if we can shortcut things
	 by checking for identical modes and codes.  */

      if (switch_mode == VOIDmode && switch_code == UNKNOWN)
	{
	  /* If p and its alternatives all want the same mode,
	     reject all others at once, first, then ignore the mode.  */

	  if (mode != VOIDmode && p->next && same_modes (p, mode))
	    {
	      printf ("  if (GET_MODE (x%d) != %smode)\n",
		      depth, GET_MODE_NAME (p->mode));
	      if (afterward)
		{
		  printf ("    {\n");
		  change_state (p->position, afterward->position, 6);
		  printf ("      goto L%d;\n    }\n", afterward->number);
		}
	      else
		printf ("    goto ret0;\n");
	      clear_modes (p);
	      mode = VOIDmode;
	    }

	  /* If p and its alternatives all want the same code,
	     reject all others at once, first, then ignore the code.  */

	  if (p->code != UNKNOWN && p->next && same_codes (p, p->code))
	    {
	      printf ("  if (GET_CODE (x%d) != ", depth);
	      print_code (p->code);
	      printf (")\n");
	      if (afterward)
		{
		  printf ("    {\n");
		  change_state (p->position, afterward->position, indent + 4);
		  printf ("    goto L%d;\n    }\n", afterward->number);
		}
	      else
		printf ("    goto ret0;\n");
	      clear_codes (p);
	    }
	}

      /* If we are not in a mode switch and we are testing for a specific
	 mode, start a mode switch unless we have just one node or the next
	 node is not testing a mode (we have already tested for the case of
	 more than one mode, but all of the same mode).  */

      if (switch_mode == VOIDmode && mode != VOIDmode && p->next != 0
	  && p->next->enforce_mode && p->next->mode != VOIDmode)
	{
	  mybzero (modemap, sizeof modemap);
	  printf ("%sswitch (GET_MODE (x%d))\n", indents[indent], depth);
	  printf ("%s{\n", indents[indent + 2]);
	  indent += 4;
	  printf ("%sdefault:\n%sbreak;\n", indents[indent - 2],
		  indents[indent]);
	  printf ("%scase %smode:\n", indents[indent - 2],
		  GET_MODE_NAME (mode));
	  modemap[(int) mode] = 1;
	  switch_mode = mode;
	}

      /* Similarly for testing codes.  */

      if (switch_code == UNKNOWN && p->code != UNKNOWN && ! p->ignore_code
	  && p->next != 0 && p->next->code != UNKNOWN)
	{
	  mybzero (codemap, sizeof codemap);
	  printf ("%sswitch (GET_CODE (x%d))\n", indents[indent], depth);
	  printf ("%s{\n", indents[indent + 2]);
	  indent += 4;
	  printf ("%sdefault:\n%sbreak;\n", indents[indent - 2],
		  indents[indent]);
	  printf ("%scase ", indents[indent - 2]);
	  print_code (p->code);
	  printf (":\n");
	  codemap[(int) p->code] = 1;
	  switch_code = p->code;
	}

      /* Now that most mode and code tests have been done, we can write out
	 a label for an inner node, if we haven't already.  */
      if (p->label_needed)
	printf ("%sL%d:\n", indents[indent - 2], p->number);

      inner_indent = indent;

      /* The only way we can have to do a mode or code test here is if
	 this node needs such a test but is the only node to be tested.
	 In that case, we won't have started a switch.  Note that this is
	 the only way the switch and test modes can disagree.  */

      if ((mode != switch_mode && ! p->ignore_mode)
	  || (p->code != switch_code && p->code != UNKNOWN && ! p->ignore_code)
	  || p->test_elt_zero_int || p->test_elt_one_int
	  || p->test_elt_zero_wide || p->veclen
	  || p->dupno >= 0 || p->tests || p->num_clobbers_to_add)
	{
	  printf ("%sif (", indents[indent]);

	  if (mode != switch_mode && ! p->ignore_mode)
	    printf ("GET_MODE (x%d) == %smode && ",
		    depth, GET_MODE_NAME (mode));
	  if (p->code != switch_code && p->code != UNKNOWN && ! p->ignore_code)
	    {
	      printf ("GET_CODE (x%d) == ", depth);
	      print_code (p->code);
	      printf (" && ");
	    }

	  if (p->test_elt_zero_int)
	    printf ("XINT (x%d, 0) == %d && ", depth, p->elt_zero_int);
	  if (p->test_elt_one_int)
	    printf ("XINT (x%d, 1) == %d && ", depth, p->elt_one_int);
	  if (p->test_elt_zero_wide)
	    {
	      /* Set offset to 1 iff the number might get propagated to
	         unsigned long by ANSI C rules, else 0.
	         Prospective hosts are required to have at least 32 bit
	         ints, and integer constants in machine descriptions
	         must fit in 32 bit, thus it suffices to check only
	         for 1 << 31 .  */
	      HOST_WIDE_INT offset = p->elt_zero_wide == -2147483647 - 1;
	      printf (
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		       "XWINT (x%d, 0) == %d%s && ",
#else
		       "XWINT (x%d, 0) == %ld%s && ",
#endif
		       depth, p->elt_zero_wide + offset, offset ? "-1" : "");
	    }
	  if (p->veclen)
	    printf ("XVECLEN (x%d, 0) == %d && ", depth, p->veclen);
	  if (p->dupno >= 0)
	    printf ("rtx_equal_p (x%d, ro[%d]) && ", depth, p->dupno);
	  if (p->num_clobbers_to_add)
	    printf ("pnum_clobbers != 0 && ");
	  if (p->tests)
	    printf ("%s (x%d, %smode)", p->tests, depth,
		    GET_MODE_NAME (p->mode));
	  else
	    printf ("1");

	  printf (")\n");
	  inner_indent += 2;
	}
      else
	uncond = 1;

      need_bracket = ! uncond;

      if (p->opno >= 0)
	{
	  if (need_bracket)
	    {
	      printf ("%s{\n", indents[inner_indent]);
	      inner_indent += 2;
	      wrote_bracket = 1;
	      need_bracket = 0;
	    }

	  printf ("%sro[%d] = x%d;\n", indents[inner_indent], p->opno, depth);
	}

      if (p->c_test)
	{
	  printf ("%sif (%s)\n", indents[inner_indent], p->c_test);
	  inner_indent += 2;
	  uncond = 0;
	  need_bracket = 1;
	}

      if (p->insn_code_number >= 0)
	{
	  if (type == SPLIT)
	    printf ("%sreturn gen_split_%d (operands);\n",
		    indents[inner_indent], p->insn_code_number);
	  else
	    {
	      if (p->num_clobbers_to_add)
		{
		  if (need_bracket)
		    {
		      printf ("%s{\n", indents[inner_indent]);
		      inner_indent += 2;
		    }

		  printf ("%s*pnum_clobbers = %d;\n",
			  indents[inner_indent], p->num_clobbers_to_add);
		  printf ("%sreturn %d;\n",
			  indents[inner_indent], p->insn_code_number);

		  if (need_bracket)
		    {
		      inner_indent -= 2;
		      printf ("%s}\n", indents[inner_indent]);
		    }
		}
	      else
		printf ("%sreturn %d;\n",
			indents[inner_indent], p->insn_code_number);
	    }
	}
      else
	printf ("%sgoto L%d;\n", indents[inner_indent],
		p->success.first->number);

      if (wrote_bracket)
	printf ("%s}\n", indents[inner_indent - 2]);
    }

  /* We have now tested all alternatives.  End any switches we have open
     and branch to the alternative node unless we know that we can't fall
     through to the branch.  */

  if (switch_code != UNKNOWN)
    {
      printf ("%s}\n", indents[indent - 2]);
      indent -= 4;
      uncond = 0;
    }

  if (switch_mode != VOIDmode)
    {
      printf ("%s}\n", indents[indent - 2]);
      indent -= 4;
      uncond = 0;
    }

  if (indent != 2)
    abort ();

  if (uncond)
    return;

  if (afterward)
    {
      change_state (prevpos, afterward->position, 2);
      printf ("  goto L%d;\n", afterward->number);
    }
  else
    printf ("  goto ret0;\n");
}

static void
print_code (code)
     enum rtx_code code;
{
  register char *p1;
  for (p1 = GET_RTX_NAME (code); *p1; p1++)
    {
      if (*p1 >= 'a' && *p1 <= 'z')
	putchar (*p1 + 'A' - 'a');
      else
	putchar (*p1);
    }
}

static int
same_codes (p, code)
     register struct decision *p;
     register enum rtx_code code;
{
  for (; p; p = p->next)
    if (p->code != code)
      return 0;

  return 1;
}

static void
clear_codes (p)
     register struct decision *p;
{
  for (; p; p = p->next)
    p->ignore_code = 1;
}

static int
same_modes (p, mode)
     register struct decision *p;
     register enum machine_mode mode;
{
  for (; p; p = p->next)
    if ((p->enforce_mode ? p->mode : VOIDmode) != mode)
      return 0;

  return 1;
}

static void
clear_modes (p)
     register struct decision *p;
{
  for (; p; p = p->next)
    p->enforce_mode = 0;
}

/* Write out the decision tree starting at TREE for a subroutine of type TYPE.

   PREVPOS is the position at the node that branched to this node.

   INITIAL is nonzero if this is the first node we are writing in a subroutine.

   If all nodes are false, branch to the node AFTERWARD.  */

static void
write_tree (tree, prevpos, afterward, initial, type)
     struct decision *tree;
     char *prevpos;
     struct decision *afterward;
     int initial;
     enum routine_type type;
{
  register struct decision *p;
  char *name_prefix = (type == SPLIT ? "split" : "recog");
  char *call_suffix = (type == SPLIT ? "" : ", pnum_clobbers");

  if (! initial && tree->subroutine_number > 0)
    {
      printf (" L%d:\n", tree->number);

      if (afterward)
	{
	  printf ("  tem = %s_%d (x0, insn%s);\n",
		  name_prefix, tree->subroutine_number, call_suffix);
	  if (type == SPLIT)
	    printf ("  if (tem != 0) return tem;\n");
	  else
	    printf ("  if (tem >= 0) return tem;\n");
	  change_state (tree->position, afterward->position, 2);
	  printf ("  goto L%d;\n", afterward->number);
	}
      else
	printf ("  return %s_%d (x0, insn%s);\n",
		name_prefix, tree->subroutine_number, call_suffix);
      return;
    }

  write_tree_1 (tree, prevpos, afterward, type);

  for (p = tree; p; p = p->next)
    if (p->success.first)
      write_tree (p->success.first, p->position,
		  p->afterward ? p->afterward : afterward, 0, type);
}


/* Assuming that the state of argument is denoted by OLDPOS, take whatever
   actions are necessary to move to NEWPOS.

   INDENT says how many blanks to place at the front of lines.  */

static void
change_state (oldpos, newpos, indent)
     char *oldpos;
     char *newpos;
     int indent;
{
  int odepth = strlen (oldpos);
  int depth = odepth;
  int ndepth = strlen (newpos);

  /* Pop up as many levels as necessary.  */

  while (strncmp (oldpos, newpos, depth))
    --depth;

  /* Go down to desired level.  */

  while (depth < ndepth)
    {
      if (newpos[depth] >= 'a' && newpos[depth] <= 'z')
	printf ("%sx%d = XVECEXP (x%d, 0, %d);\n",
		indents[indent], depth + 1, depth, newpos[depth] - 'a');
      else
	printf ("%sx%d = XEXP (x%d, %c);\n",
		indents[indent], depth + 1, depth, newpos[depth]);
      ++depth;
    }
}

static char *
copystr (s1)
     char *s1;
{
  register char *tem;

  if (s1 == 0)
    return 0;

  tem = (char *) xmalloc (strlen (s1) + 1);
  strcpy (tem, s1);

  return tem;
}

static void
mybzero (b, length)
     register char *b;
     register unsigned length;
{
  while (length-- > 0)
    *b++ = 0;
}

static void
mybcopy (in, out, length)
     register char *in, *out;
     register unsigned length;
{
  while (length-- > 0)
    *out++ = *in++;
}

static char *
concat (s1, s2)
     char *s1, *s2;
{
  register char *tem;

  if (s1 == 0)
    return s2;
  if (s2 == 0)
    return s1;

  tem = (char *) xmalloc (strlen (s1) + strlen (s2) + 2);
  strcpy (tem, s1);
  strcat (tem, " ");
  strcat (tem, s2);

  return tem;
}

char *
xrealloc (ptr, size)
     char *ptr;
     unsigned size;
{
  char *result = (char *) realloc (ptr, size);
  if (!result)
    fatal ("virtual memory exhausted");
  return result;
}

char *
xmalloc (size)
     unsigned size;
{
  register char *val = (char *) malloc (size);

  if (val == 0)
    fatal ("virtual memory exhausted");
  return val;
}

static void
fatal (s)
     char *s;
{
  fprintf (stderr, "genrecog: ");
  fprintf (stderr, s);
  fprintf (stderr, "\n");
  fprintf (stderr, "after %d definitions\n", next_index);
  exit (FATAL_EXIT_CODE);
}

/* More 'friendly' abort that prints the line and file.
   config.h can #define abort fancy_abort if you like that sort of thing.  */

void
fancy_abort ()
{
  fatal ("Internal gcc abort.");
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  struct decision_head recog_tree;
  struct decision_head split_tree;
  FILE *infile;
  register int c;

  obstack_init (rtl_obstack);
  recog_tree.first = recog_tree.last = split_tree.first = split_tree.last = 0;

  if (argc <= 1)
    fatal ("No input file name.");

  infile = fopen (argv[1], "r");
  if (infile == 0)
    {
      perror (argv[1]);
      exit (FATAL_EXIT_CODE);
    }

  init_rtl ();
  next_insn_code = 0;
  next_index = 0;

  printf ("/* Generated automatically by the program `genrecog'\n\
from the machine description file `md'.  */\n\n");

  printf ("#include \"config.h\"\n");
  printf ("#include <stdio.h>\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"insn-config.h\"\n");
  printf ("#include \"recog.h\"\n");
  printf ("#include \"real.h\"\n");
  printf ("#include \"output.h\"\n");
  printf ("#include \"flags.h\"\n");
  printf ("\n");

  /* Read the machine description.  */

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == EOF)
	break;
      ungetc (c, infile);

      desc = read_rtx (infile);
      if (GET_CODE (desc) == DEFINE_INSN)
	recog_tree = merge_trees (recog_tree,
				  make_insn_sequence (desc, RECOG));
      else if (GET_CODE (desc) == DEFINE_SPLIT)
	split_tree = merge_trees (split_tree,
				  make_insn_sequence (desc, SPLIT));
      if (GET_CODE (desc) == DEFINE_PEEPHOLE
	  || GET_CODE (desc) == DEFINE_EXPAND)
	next_insn_code++;
      next_index++;
    }

  printf ("\n\
/* `recog' contains a decision tree\n\
   that recognizes whether the rtx X0 is a valid instruction.\n\
\n\
   recog returns -1 if the rtx is not valid.\n\
   If the rtx is valid, recog returns a nonnegative number\n\
   which is the insn code number for the pattern that matched.\n");
  printf ("   This is the same as the order in the machine description of\n\
   the entry that matched.  This number can be used as an index into\n\
   entry that matched.  This number can be used as an index into various\n\
   insn_* tables, such as insn_templates, insn_outfun, and insn_n_operands\n\
   (found in insn-output.c).\n\n");
  printf ("   The third argument to recog is an optional pointer to an int.\n\
   If present, recog will accept a pattern if it matches except for\n\
   missing CLOBBER expressions at the end.  In that case, the value\n\
   pointed to by the optional pointer will be set to the number of\n\
   CLOBBERs that need to be added (it should be initialized to zero by\n\
   the caller).  If it is set nonzero, the caller should allocate a\n\
   PARALLEL of the appropriate size, copy the initial entries, and call\n\
   add_clobbers (found in insn-emit.c) to fill in the CLOBBERs.");

  if (split_tree.first)
    printf ("\n\n   The function split_insns returns 0 if the rtl could not\n\
   be split or the split rtl in a SEQUENCE if it can be.");

  printf ("*/\n\n");

  printf ("rtx recog_operand[MAX_RECOG_OPERANDS];\n\n");
  printf ("rtx *recog_operand_loc[MAX_RECOG_OPERANDS];\n\n");
  printf ("rtx *recog_dup_loc[MAX_DUP_OPERANDS];\n\n");
  printf ("char recog_dup_num[MAX_DUP_OPERANDS];\n\n");
  printf ("#define operands recog_operand\n\n");

  next_subroutine_number = 0;
  break_out_subroutines (recog_tree, RECOG, 1);
  write_subroutine (recog_tree.first, RECOG);

  next_subroutine_number = 0;
  break_out_subroutines (split_tree, SPLIT, 1);
  write_subroutine (split_tree.first, SPLIT);

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
  /* NOTREACHED */
  return 0;
}
