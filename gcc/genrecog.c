/* Generate code from machine description to recognize rtl as insns.
   Copyright (C) 1987-1991 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


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
   insn_* tables, such as insn_templates, insn_outfun, and insn_n_operands
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
#include "config.h"
#include "rtl.h"
#include "obstack.h"

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern void free ();

/* Data structure for decision tree for recognizing
   legitimate instructions.  */

struct decision
{
  int number;
  char *position;
  RTX_CODE code;
  char *exact;
  enum machine_mode mode;
  char *tests;
  int insn_code_number;
  int num_clobbers_to_add;
  struct decision *next;
  struct decision *success;
  int opno;
  int dupno;
  int test_elt_zero_int;
  int elt_zero_int;
  int test_elt_one_int;
  int elt_one_int;
  int ignmode;
  struct decision *afterward;
  int label_needed;
  char *c_test;
  char enforce_mode;
  int veclen;
  int subroutine_number;
  /* Used for DEFINE_SPLITs.  */
  char *c_hook;
  rtx split_sequence;
};

#define SUBROUTINE_THRESHOLD 50

static int next_subroutine_number;

/* We can write two types of subroutines: One for insn recognition and
   one to split insns.  This defines which type is being written.  */

enum routine_type {RECOG, SPLIT};

static int try_merge_1 ();
static int no_same_mode ();
static int same_codes ();
static int same_modes ();

/*
static int
recognize (top)
{
 staten:
  x = XVECEXP (top, 0, 3);
  if (test_code (GET_CODE (x))
      && test_mode (MODE (x))
      && whatever_else)
    goto statep;
  else if (next one...)
    goto statem:
  goto stater;

 statep:
  actions...;
  return 1;

 statem:
  x = stack[depth--];
  more tests...;

 stateq:
  stack[++depth] = x;
  x = XEXP (stack[depth], 0);
  more tests...;

 stater:
  x = XEXP (stack[depth], 1);
}

*/

static int next_number;

static int next_insn_code;

static int next_index;

char *xmalloc ();
static struct decision *add_to_sequence ();
static struct decision *merge_trees ();
static struct decision *try_merge_2 ();
static void write_subroutine ();
static void print_code ();
static void clear_codes ();
static void clear_modes ();
static void change_state ();
static void write_tree ();
static char *copystr ();
static char *concat ();
static void fatal ();
void fancy_abort ();
static void mybzero ();

static struct decision *first;

/* Construct and return a sequence of decisions
   that will recognize INSN.  */

static struct decision *
make_insn_sequence (insn)
     rtx insn;
{
  rtx x;
  char *c_test = XSTR (insn, 2);
  struct decision *last;

  if (XVECLEN (insn, 1) == 1)
    x = XVECEXP (insn, 1, 0);
  else
    {
      x = rtx_alloc (PARALLEL);
      XVEC (x, 0) = XVEC (insn, 1);
      PUT_MODE (x, VOIDmode);
    }

  last = add_to_sequence (x, 0, "");

  if (c_test[0])
    last->c_test = c_test;
  last->insn_code_number = next_insn_code;
  last->num_clobbers_to_add = 0;

  /* If X is a PARALLEL, see if it ends with a group of CLOBBERs of (hard)
     registers or MATCH_SCRATCHes.  If so, set up to recognize the pattern
     without these CLOBBERs.  */

  if (GET_CODE (x) == PARALLEL)
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
	  struct decision *previous_first = first;

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

	  last = add_to_sequence (new, 0, "");

	  if (c_test[0])
	    last->c_test = c_test;
	  last->insn_code_number = next_insn_code;
	  last->num_clobbers_to_add = XVECLEN (x, 0) - i;
	  first = merge_trees (previous_first, first);
	}
    }

  next_insn_code++;
  return first;
}

static struct decision *
make_split_sequence (insn)
     rtx insn;
{
  rtx x;
  char *c_test = XSTR (insn, 1);
  char *c_hook = XSTR (insn, 3);
  struct decision *last;

  if (XVECLEN (insn, 0) == 1)
    x = XVECEXP (insn, 0, 0);
  else
    {
      x = rtx_alloc (PARALLEL);
      XVEC (x, 0) = XVEC (insn, 0);
      PUT_MODE (x, VOIDmode);
    }

  last = add_to_sequence (x, 0, "");

  if (c_test[0])
    last->c_test = c_test;
  if (c_hook != 0 && c_hook[0] != 0)
    last->c_hook = c_hook;
  last->split_sequence = XEXP (insn, 2);
  last->insn_code_number = next_insn_code++;

  /* Define the subroutine we will call below and emit in genemit.  */
  printf ("extern rtx gen_split_%d ();\n", last->insn_code_number);

  return first;
}

static struct decision *
add_to_sequence (pattern, last, position)
     rtx pattern;
     struct decision *last;
     char *position;
{
  register RTX_CODE code;
  register struct decision *new
    = (struct decision *) xmalloc (sizeof (struct decision));
  struct decision *this;
  char *newpos;
  register char *fmt;
  register int i;
  int depth;
  int len;

  new->number = next_number++;
  new->position = copystr (position);
  new->exact = 0;
  new->next = 0;
  new->success = 0;
  new->insn_code_number = -1;
  new->num_clobbers_to_add = 0;
  new->tests = 0;
  new->opno = -1;
  new->dupno = -1;
  new->test_elt_zero_int = 0;
  new->test_elt_one_int = 0;
  new->elt_zero_int = 0;
  new->elt_one_int = 0;
  new->enforce_mode = 0;
  new->ignmode = 0;
  new->afterward = 0;
  new->label_needed = 0;
  new->c_test = 0;
  new->c_hook = 0;
  new->split_sequence = 0;
  new->veclen = 0;
  new->subroutine_number = 0;

  this = new;

  if (last == 0)
    first = new;
  else
    last->success = new;

  depth = strlen (position);
  newpos = (char *) alloca (depth + 2);
  strcpy (newpos, position);
  newpos[depth + 1] = 0;

 restart:

  if (pattern == 0)
    {
      new->exact = "0";
      new->code = UNKNOWN;
      new->mode = VOIDmode;
      return new;
    }

  new->mode = GET_MODE (pattern);
  new->code = code = GET_CODE (pattern);

  switch (code)
    {
    case MATCH_OPERAND:
      new->opno = XINT (pattern, 0);
      new->code = UNKNOWN;
      new->tests = XSTR (pattern, 1);
      if (*new->tests == 0)
	new->tests = 0;
      return new;

    case MATCH_SCRATCH:
      new->opno = XINT (pattern, 0);
      new->code = UNKNOWN;
      new->tests = "scratch_operand";
      if (*new->tests == 0)
	new->tests = 0;
      return new;

    case MATCH_OPERATOR:
      new->opno = XINT (pattern, 0);
      new->code = UNKNOWN;
      new->tests = XSTR (pattern, 1);
      if (*new->tests == 0)
	new->tests = 0;
      for (i = 0; i < XVECLEN (pattern, 2); i++)
	{
	  newpos[depth] = i + '0';
	  new = add_to_sequence (XVECEXP (pattern, 2, i), new, newpos);
	}
      this->success->enforce_mode = 0;
      return new;

    case MATCH_PARALLEL:
      new->opno = XINT (pattern, 0);
      new->code = PARALLEL;
      new->tests = XSTR (pattern, 1);
      if (*new->tests == 0)
	new->tests = 0;
      for (i = 0; i < XVECLEN (pattern, 2); i++)
	{
	  newpos[depth] = i + 'a';
	  new = add_to_sequence (XVECEXP (pattern, 2, i), new, newpos);
	}
      this->success->enforce_mode = 0;
      return new;

    case MATCH_OP_DUP:
      new->opno = XINT (pattern, 0);
      new->dupno = XINT (pattern, 0);
      new->code = UNKNOWN;
      new->tests = 0;
      for (i = 0; i < XVECLEN (pattern, 1); i++)
	{
	  newpos[depth] = i + '0';
	  new = add_to_sequence (XVECEXP (pattern, 1, i), new, newpos);
	}
      this->success->enforce_mode = 0;
      return new;

    case MATCH_DUP:
      new->dupno = XINT (pattern, 0);
      new->code = UNKNOWN;
      return new;

    case ADDRESS:
      pattern = XEXP (pattern, 0);
      goto restart;

    case PC:
      new->exact = "pc_rtx";
      return new;

    case CC0:
      new->exact = "cc0_rtx";
      return new;

    case CONST_INT:
      if (INTVAL (pattern) == 0)
	{
	  new->exact = "const0_rtx";
	  return new;
	}
      if (INTVAL (pattern) == 1)
	{
	  new->exact = "const1_rtx";
	  return new;
	}
      if (INTVAL (pattern) == -1)
	{
	  new->exact = "constm1_rtx";
	  return new;
	}
      if (INTVAL (pattern) == STORE_FLAG_VALUE)
	{
	  new->exact = "const_true_rtx";
	  return new;
	}
      break;

    case SET:
      newpos[depth] = '0';
      new = add_to_sequence (SET_DEST (pattern), new, newpos);
      this->success->enforce_mode = 1;
      newpos[depth] = '1';
      new = add_to_sequence (SET_SRC (pattern), new, newpos);
      return new;

    case STRICT_LOW_PART:
      newpos[depth] = '0';
      new = add_to_sequence (XEXP (pattern, 0), new, newpos);
      this->success->enforce_mode = 1;
      return new;

    case SUBREG:
      this->test_elt_one_int = 1;
      this->elt_one_int = XINT (pattern, 1);
      newpos[depth] = '0';
      new = add_to_sequence (XEXP (pattern, 0), new, newpos);
      this->success->enforce_mode = 1;
      return new;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      newpos[depth] = '0';
      new = add_to_sequence (XEXP (pattern, 0), new, newpos);
      this->success->enforce_mode = 1;
      newpos[depth] = '1';
      new = add_to_sequence (XEXP (pattern, 1), new, newpos);
      newpos[depth] = '2';
      new = add_to_sequence (XEXP (pattern, 2), new, newpos);
      return new;
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      newpos[depth] = '0' + i;
      if (fmt[i] == 'e' || fmt[i] == 'u')
	new = add_to_sequence (XEXP (pattern, i), new, newpos);
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
				     new, newpos);
	    }
	}
      else if (fmt[i] != '0')
	abort ();
    }
  return new;
}

/* Merge two decision trees OLD and ADD,
   modifying OLD destructively,
   and return the merged tree.  */

static struct decision *
merge_trees (old, add)
     register struct decision *old, *add;
{
  while (add)
    {
      register struct decision *next = add->next;
      add->next = 0;
      if (!try_merge_1 (old, add))
	old = try_merge_2 (old, add);
      add = next;
    }
  return old;
}

/* Merge ADD into the next-chain starting with OLD
   only if it overlaps a condition already tested in OLD.
   Returns 1 if successful (OLD is modified),
   0 if nothing has been done.  */

static int
try_merge_1 (old, add)
     register struct decision *old, *add;
{
  while (old)
    {
      if ((old->position == add->position
	   || (old->position && add->position
	       && !strcmp (old->position, add->position)))
	  && (old->tests == add->tests
	      || (old->tests && add->tests && !strcmp (old->tests, add->tests)))
	  && (old->c_test == add->c_test
	      || (old->c_test && add->c_test && !strcmp (old->c_test, add->c_test)))
	  && (old->c_hook == add->c_hook
	      || (old->c_hook && add->c_hook && !strcmp (old->c_hook, add->c_hook)))
	  && old->test_elt_zero_int == add->test_elt_zero_int
	  && old->elt_zero_int == add->elt_zero_int
	  && old->test_elt_one_int == add->test_elt_one_int
	  && old->elt_one_int == add->elt_one_int
	  && old->veclen == add->veclen
	  && old->dupno == add->dupno
	  && old->opno == add->opno
/* In a collection of nodes that don't have predicates,
   we can always merge a new one with any node that matches it.
   This is because we know that two different nodes can't possibly match
   the same RTL object.  So we can reorder the tests to simplify the
   whole collection of them.

   But when predicates are involved, we have to preserve the order of
   testing them.  This means that a new node can only be merged with the
   last existing node.

   enforce_mode indicates that at this level each of the nodes
   requires a particular mode.  When this is true, then we know
   that two nodes with different modes can't possibly both match.
   Therefore, it is ok to merge a new node with the last node
   that wants the same mode, even if other nodes for different modes
   appear after it.  no_same_mode tests for this condition.  */
	  && (old->tests == 0
	      || (add->enforce_mode ? no_same_mode (old) : old->next == 0))
	  && old->code == add->code
	  && old->mode == add->mode
	  && (old->exact == add->exact
	      || (old->exact && add->exact && ! strcmp (old->exact, add->exact))))
	{
	  old->success = merge_trees (old->success, add->success);
	  if (old->insn_code_number >= 0 && add->insn_code_number >= 0)
	    fatal ("Two actions at one point in tree");
	  if (old->insn_code_number == -1)
	    old->insn_code_number = add->insn_code_number;
	  return 1;
	}
      old = old->next;
    }
  return 0;
}

/* Merge ADD into the next-chain that starts with OLD,
   preferably after something that tests the same place
   that ADD does.
   The next-chain of ADD itself is ignored, and it is set
   up for entering ADD into the new chain.
   Returns the new chain.  */

static struct decision *
try_merge_2 (old, add)
     struct decision *old, *add;
{
  register struct decision *p;
  struct decision *last = 0;
  struct decision *last_same_place = 0;

  /* Put this in after the others that test the same place,
     if there are any.  If not, find the last chain element
     and insert there.

     One modification: if this one is NOT a MATCH_OPERAND,
     put it before any MATCH_OPERANDS that test the same place.

     Another: if enforce_mode (i.e. this is first operand of a SET),
     put this after the last thing that tests the same place for
     the same mode.  */

#if 0
  int operand = 0 != add->tests;
#endif

  for (p = old; p; p = p->next)
    {
      if (p->position == add->position
	  || (p->position && add->position
	      && !strcmp (p->position, add->position)))
	{
	  last_same_place = p;
	  /* If enforce_mode, segregate the modes in numerical order.  */
	  if (p->enforce_mode && (int) add->mode < (int) p->mode)
	    break;
#if 0
	  /* Keep explicit decompositions before those that test predicates.
	     If enforce_mode, do this separately within each mode.  */
	  if (! p->enforce_mode || p->mode == add->mode)
	    if (!operand && p->tests)
	      break;
#endif
	}
      /* If this is past the end of the decisions at the same place as ADD,
	 stop looking now; add ADD before here.  */
      else if (last_same_place)
	break;
      last = p;
    }

  /* Insert before P, which means after LAST.  */

  if (last)
    {
      add->next = last->next;
      last->next = add;
      return old;
    }

  add->next = old;
  return add;
}

static int
no_same_mode (node)
     struct decision *node;
{
  register struct decision *p;
  register enum machine_mode mode = node->mode;

  for (p = node->next; p; p = p->next)
    if (p->mode == mode)
      return 0;

  return 1;
}

/* Count the number of subnodes of node NODE, assumed to be the start
   of a next-chain.  If the number is high enough, make NODE start
   a separate subroutine in the C code that is generated.

   TYPE gives the type of routine we are writing.  */

static int
break_out_subroutines (node, type)
     struct decision *node;
     enum routine_type type;
{
  int size = 0;
  struct decision *sub;
  for (sub = node; sub; sub = sub->next)
    size += 1 + break_out_subroutines (sub->success, type);
  if (size > SUBROUTINE_THRESHOLD)
    {
      node->subroutine_number = ++next_subroutine_number;
      write_subroutine (node, type);
      size = 1;
    }
  return size;
}

static void
write_subroutine (tree, type)
     struct decision *tree;
     enum routine_type type;
{
  char *return_type = (type == SPLIT ? "rtx" : "int");

  if (type == SPLIT)
    {
      printf ("rtx\nsplit_%d (x0, insn)\n", tree->subroutine_number);
      printf ("     register rtx x0;\n     rtx insn;\n");
    }
  else
    {
      printf ("int\nrecog_%d (x0, insn, pnum_clobbers)\n",
	      tree->subroutine_number);
      printf ("     register rtx x0;\n     rtx insn;\n");
      printf ("     int *pnum_clobbers;\n");
    }

  printf ("{\n");
  printf ("  register rtx *ro = &recog_operand[0];\n");
  printf ("  register rtx x1, x2, x3, x4, x5;\n  rtx x6, x7, x8, x9, x10, x11;\n");
  printf ("  %s tem;\n", return_type);
  write_tree (tree, "", 0, "", 1, type);
  printf (" ret0: return %d;\n}\n\n", type == SPLIT ? 0 : -1);
}

/* Write out C code to perform the decisions in the tree.  */

static char *
write_tree_1 (tree, prevpos, afterward, afterpos, initial, type)
     struct decision *tree;
     char *prevpos;
     int afterward;
     char *afterpos;
     int initial;
     enum routine_type type;
{
  register struct decision *p, *p1;
  char *pos;
  register int depth;
  int ignmode;
  enum anon1 { NO_SWITCH, CODE_SWITCH, MODE_SWITCH } in_switch = NO_SWITCH;
  char modemap[NUM_MACHINE_MODES];
  char codemap[NUM_RTX_CODE];

  pos = prevpos;

  tree->label_needed = 1;
  for (p = tree; p; p = p->next)
    {
      /* Find the next alternative to p
	 that might be true when p is true.
	 Test that one next if p's successors fail.
	 Note that when the `tests' field is nonzero
	 it is up to the specified test-function to compare machine modes
	 and some (such as general_operand) don't always do so.
	 But when inside a switch-on-modes we ignore this and
	 consider all modes mutually exclusive.  */
      for (p1 = p->next; p1; p1 = p1->next)
	if (((p->code == UNKNOWN || p1->code == UNKNOWN || p->code == p1->code)
	     && (p->mode == VOIDmode || p1->mode == VOIDmode
		 || p->mode == p1->mode
		 || (in_switch != MODE_SWITCH && (p->tests || p1->tests))))
	    || strcmp (p1->position, p->position))
	  break;
      p->afterward = p1;
      if (p1) p1->label_needed = 1;

      if (in_switch == MODE_SWITCH
	  && (p->mode == VOIDmode || (! p->enforce_mode && p->tests != 0)))
	{
	  in_switch = NO_SWITCH;
	  printf ("  }\n");
	}
      if (in_switch == CODE_SWITCH && p->code == UNKNOWN)
	{
	  in_switch = NO_SWITCH;
	  printf ("  }\n");
	}

      if (p->label_needed)
	printf (" L%d:\n", p->number);

      if (p->success == 0 && p->insn_code_number < 0)
	abort ();

      change_state (pos, p->position);
      pos = p->position;
      depth = strlen (pos);

      ignmode = (p->ignmode || p->tests);

      if (in_switch == NO_SWITCH)
	{
	  /* If p and its alternatives all want the same mode,
	     reject all others at once, first, then ignore the mode.  */
	  if (!ignmode && p->mode != VOIDmode && p->next && same_modes (p, p->mode))
	    {
	      printf ("  if (GET_MODE (x%d) != %smode)\n",
		      depth, GET_MODE_NAME (p->mode));
	      if (afterward)
		{
		  printf ("    {\n    ");
		  change_state (pos, afterpos);
		  printf ("      goto L%d;\n    }\n", afterward);
		}
	      else
		printf ("    goto ret0;\n");
	      clear_modes (p);
	      ignmode = 1;
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
		  printf ("    {");
		  change_state (pos, afterpos);
		  printf ("    goto L%d; }\n", afterward);
		}
	      else
		printf ("    goto ret0;\n");
	      clear_codes (p);
	    }
	}

      /* If p and its alternatives all have different modes
	 and there are at least 4 of them, make a switch.  */
      if (in_switch == NO_SWITCH)
	{
	  register int i;
	  int lose = 0;

	  mybzero (modemap, sizeof modemap);
	  for (p1 = p, i = 0;
	       (p1 && p1->mode != VOIDmode
		&& (p1->tests == 0 || p1->enforce_mode));
	       p1 = p1->next, i++)
	    {
	      if (! p->enforce_mode && modemap[(int) p1->mode])
		{
		  lose = 1;
		  break;
		}
	      modemap[(int) p1->mode] = 1;
	    }
	  if (!lose && i >= 4)
	    {
	      in_switch = MODE_SWITCH;
	      printf (" switch (GET_MODE (x%d))\n  {\n", depth);
	    }
	}

      if (in_switch == NO_SWITCH)
	{
	  register int i;
	  mybzero (codemap, sizeof codemap);
	  for (p1 = p, i = 0; p1 && p1->code != UNKNOWN; p1 = p1->next, i++)
	    {
	      if (codemap[(int) p1->code])
		break;
	      codemap[(int) p1->code] = 1;
	    }
	  if ((p1 == 0 || p1->code == UNKNOWN) && i >= 4)
	    {
	      in_switch = CODE_SWITCH;
	      printf (" switch (GET_CODE (x%d))\n  {\n", depth);
	    }
	}

      if (in_switch == MODE_SWITCH)
	{
	  if (modemap[(int) p->mode])
	    {
	      printf ("  case %smode:\n", GET_MODE_NAME (p->mode));
	      modemap[(int) p->mode] = 0;
	    }
	}
      if (in_switch == CODE_SWITCH)
	{
	  if (codemap[(int) p->code])
	    {
	      printf ("  case ");
	      print_code (p->code);
	      printf (":\n");
	      codemap[(int) p->code] = 0;
	    }
	}

      printf ("  if (");
      if (p->exact || (p->code != UNKNOWN && in_switch != CODE_SWITCH))
	{
	  if (p->exact)
	    printf ("x%d == %s", depth, p->exact);
	  else
	    {
	      printf ("GET_CODE (x%d) == ", depth);
	      print_code (p->code);
	    }
	  printf (" && ");
	}
      if (p->mode != VOIDmode && !ignmode && in_switch != MODE_SWITCH)
	printf ("GET_MODE (x%d) == %smode && ",
		depth, GET_MODE_NAME (p->mode));
      if (p->test_elt_zero_int)
	printf ("XINT (x%d, 0) == %d && ", depth, p->elt_zero_int);
      if (p->veclen)
	printf ("XVECLEN (x%d, 0) == %d && ", depth, p->veclen);
      if (p->test_elt_one_int)
	printf ("XINT (x%d, 1) == %d && ", depth, p->elt_one_int);
      if (p->dupno >= 0)
	printf ("rtx_equal_p (x%d, ro[%d]) && ", depth, p->dupno);
      if (p->tests)
	printf ("%s (x%d, %smode)", p->tests, depth,
		GET_MODE_NAME (p->mode));
      else
	printf ("1");

      if (p->opno >= 0)
	printf (")\n    { ro[%d] = x%d; ",
		p->opno, depth);
      else
	printf (")\n    ");

      if (p->c_test)
	printf ("if (%s) ", p->c_test);

      if (p->insn_code_number >= 0)
	{
	  if (type == SPLIT)
	    printf ("return gen_split_%d (operands);", p->insn_code_number);
	  else
	    {
	      if (p->num_clobbers_to_add)
		{
		  printf ("\n      {\n");
		  printf ("\tif (pnum_clobbers == 0) goto ret0; ");
		  printf ("*pnum_clobbers = %d; ", p->num_clobbers_to_add);
		  printf ("return %d;\n      }", p->insn_code_number);
		}
	      else
		printf ("return %d;", p->insn_code_number);
	    }
	}
      else
	printf ("goto L%d;", p->success->number);

      if (p->opno >= 0)
	printf (" }\n");
      else
	printf ("\n");

      /* Now, if inside a switch, branch to next switch member
	 that might also need to be tested if this one fails.  */

      if (in_switch == CODE_SWITCH)
	{
	  /* Find the next alternative to p
	     that might be applicable if p was applicable.  */
	  for (p1 = p->next; p1; p1 = p1->next)
	    if (p1->code == UNKNOWN || p->code == p1->code)
	      break;
	  if (p1 == 0 || p1->code == UNKNOWN)
	    printf ("  break;\n");
	  else if (p1 != p->next)
	    {
	      printf (" goto L%d;\n", p1->number);
	      p1->label_needed = 1;
	    }
	}

      if (in_switch == MODE_SWITCH)
	{
	  /* Find the next alternative to p
	     that might be applicable if p was applicable.  */
	  for (p1 = p->next; p1; p1 = p1->next)
	    if (p1->mode == VOIDmode || p->mode == p1->mode)
	      break;
	  if (p1 == 0 || p1->mode == VOIDmode)
	    printf ("  break;\n");
	  else if (p1 != p->next)
	    {
	      printf (" goto L%d;\n", p1->number);
	      p1->label_needed = 1;
	    }
	}
    }

  if (in_switch != NO_SWITCH)
    printf ("  }\n");

  if (afterward)
    {
      change_state (pos, afterpos);
      printf ("  goto L%d;\n", afterward);
    }
  else
    printf ("  goto ret0;\n");
  return pos;
}

static void
write_tree (tree, prevpos, afterward, afterpos, initial, type)
     struct decision *tree;
     char *prevpos;
     int afterward;
     char *afterpos;
     int initial;
     enum routine_type type;
{
  register struct decision *p;
  char *pos = prevpos;
  char *name_prefix = (type == SPLIT ? "split" : "recog");
  char *call_suffix = (type == SPLIT ? "" : ", pnum_clobbers");

  if (tree->subroutine_number > 0 && ! initial)
    {
      printf (" L%d:\n", tree->number);

      if (afterward)
	{
	  printf ("  tem = %s_%d (x0, insn%s);\n",
		  name_prefix, tree->subroutine_number, call_suffix);
	  printf ("  if (tem >= 0) return tem;\n");
	  change_state (pos, afterpos);
	  printf ("  goto L%d;\n", afterward);
	}
      else
	printf ("  return %s_%d (x0, insn%s);\n",
		name_prefix, tree->subroutine_number, call_suffix);
      return;
    }

  pos = write_tree_1 (tree, prevpos, afterward, afterpos, initial, type);

  for (p = tree; p; p = p->next)
    if (p->success)
      {
	pos = p->position;
	write_tree (p->success, pos,
		    p->afterward ? p->afterward->number : afterward,
		    p->afterward ? pos : afterpos, 0, type);
      }
}

static void
print_code (code)
     RTX_CODE code;
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
     register RTX_CODE code;
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
    p->code = UNKNOWN;
}

static int
same_modes (p, mode)
     register struct decision *p;
     register enum machine_mode mode;
{
  for (; p; p = p->next)
    if (p->mode != mode || p->tests)
      return 0;

  return 1;
}

static void
clear_modes (p)
     register struct decision *p;
{
  for (; p; p = p->next)
    p->ignmode = 1;
}

static void
change_state (oldpos, newpos)
     char *oldpos;
     char *newpos;
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
	printf ("  x%d = XVECEXP (x%d, 0, %d);\n",
		depth + 1, depth, newpos[depth] - 'a');
      else
	printf ("  x%d = XEXP (x%d, %c);\n",
		depth + 1, depth, newpos[depth]);
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
fatal (s, a1, a2)
     char *s;
{
  fprintf (stderr, "genrecog: ");
  fprintf (stderr, s, a1, a2);
  fprintf (stderr, "\n");
  fprintf (stderr, "after %d instruction definitions\n", next_index);
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
  struct decision *tree = 0;
  struct decision *split_tree = 0;
  FILE *infile;
  extern rtx read_rtx ();
  register int c;

  obstack_init (rtl_obstack);

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
	tree = merge_trees (tree, make_insn_sequence (desc));
      else if (GET_CODE (desc) == DEFINE_SPLIT)
	split_tree = merge_trees (split_tree, make_split_sequence (desc));
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

  if (split_tree)
    printf ("\n\n   The function split_insns returns 0 if the rtl could not\n\
   be split or the split rtl in a SEQUENCE if it can be.");

  printf ("*/\n\n");

  printf ("rtx recog_operand[MAX_RECOG_OPERANDS];\n\n");
  printf ("rtx *recog_operand_loc[MAX_RECOG_OPERANDS];\n\n");
  printf ("rtx *recog_dup_loc[MAX_DUP_OPERANDS];\n\n");
  printf ("char recog_dup_num[MAX_DUP_OPERANDS];\n\n");
  printf ("#define operands recog_operand\n\n");

  next_subroutine_number = 0;
  break_out_subroutines (tree, RECOG);

  printf ("int\nrecog (x0, insn, pnum_clobbers)\n");
  printf ("     register rtx x0;\n     rtx insn;\n");
  printf ("     int *pnum_clobbers;\n{\n");
  printf ("  register rtx *ro = &recog_operand[0];\n");
  printf ("  register rtx x1, x2, x3, x4, x5;\n  rtx x6, x7, x8, x9, x10, x11;\n");
  printf ("  int tem;\n");

  if (tree)
    write_tree (tree, "", 0, "", 1, RECOG);
  printf (" ret0: return -1;\n}\n");

  next_subroutine_number = 0;
  break_out_subroutines (split_tree, SPLIT);

  printf ("rtx\nsplit_insns (x0, insn)\n     register rtx x0;\n     rtx insn;\n{\n");
  printf ("  register rtx *ro = &recog_operand[0];\n");
  printf ("  register rtx x1, x2, x3, x4, x5;\n  rtx x6, x7, x8, x9, x10, x11;\n");
  printf ("  rtx tem;\n");

  if (split_tree)
    write_tree (split_tree, "", 0, "", 1, SPLIT);
  printf (" ret0: return 0;\n}\n");

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
  /* NOTREACHED */
  return 0;
}
