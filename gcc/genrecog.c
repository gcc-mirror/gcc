/* Generate code from machine description to recognize rtl as insns.
   Copyright (C) 1987, 88, 92-95, 97-99, 2000 Free Software Foundation, Inc.

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


/* This program is used to produce insn-recog.c, which contains a
   function called `recog' plus its subroutines.  These functions
   contain a decision tree that recognizes whether an rtx, the
   argument given to recog, is a valid instruction.

   recog returns -1 if the rtx is not valid.  If the rtx is valid,
   recog returns a nonnegative number which is the insn code number
   for the pattern that matched.  This is the same as the order in the
   machine description of the entry that matched.  This number can be
   used as an index into various insn_* tables, such as insn_template,
   insn_outfun, and insn_n_operands (found in insn-output.c).

   The third argument to recog is an optional pointer to an int.  If
   present, recog will accept a pattern if it matches except for
   missing CLOBBER expressions at the end.  In that case, the value
   pointed to by the optional pointer will be set to the number of
   CLOBBERs that need to be added (it should be initialized to zero by
   the caller).  If it is set nonzero, the caller should allocate a
   PARALLEL of the appropriate size, copy the initial entries, and
   call add_clobbers (found in insn-emit.c) to fill in the CLOBBERs.

   This program also generates the function `split_insns', which
   returns 0 if the rtl could not be split, or it returns the split
   rtl in a SEQUENCE.

   This program also generates the function `peephole2_insns', which
   returns 0 if the rtl could not be matched.  If there was a match,
   the new rtl is returned in a SEQUENCE, and LAST_INSN will point
   to the last recognized insn in the old sequence.  */

#include "hconfig.h"
#include "system.h"
#include "rtl.h"
#include "obstack.h"
#include "errors.h"

#define OUTPUT_LABEL(INDENT_STRING, LABEL_NUMBER) \
  printf("%sL%d: ATTRIBUTE_UNUSED_LABEL\n", (INDENT_STRING), (LABEL_NUMBER))

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* Holds an array of names indexed by insn_code_number.  */
static char **insn_name_ptr = 0;
static int insn_name_ptr_size = 0;

/* A listhead of decision trees.  The alternatives to a node are kept
   in a doublely-linked list so we can easily add nodes to the proper
   place when merging.  */

struct decision_head
{
  struct decision *first;
  struct decision *last;
};
    
/* A single test.  The two accept types aren't tests per-se, but
   their equality (or lack thereof) does affect tree merging so
   it is convenient to keep them here.  */

struct decision_test
{
  /* A linked list through the tests attached to a node.  */
  struct decision_test *next;

  /* These types are roughly in the order in which we'd like to test them.  */
  enum decision_type {
    DT_mode, DT_code, DT_veclen,
    DT_elt_zero_int, DT_elt_one_int, DT_elt_zero_wide,
    DT_dup, DT_pred, DT_c_test, 
    DT_accept_op, DT_accept_insn
  } type;

  union
  {
    enum machine_mode mode;	/* Machine mode of node.  */
    RTX_CODE code;		/* Code to test.  */

    struct
    {
      const char *name;		/* Predicate to call.  */
      int index;		/* Index into `preds' or -1.  */
      enum machine_mode mode;	/* Machine mode for node.  */
    } pred;

    const char *c_test;		/* Additional test to perform.  */
    int veclen;			/* Length of vector.  */
    int dup;			/* Number of operand to compare against.  */
    HOST_WIDE_INT intval;	/* Value for XINT for XWINT.  */
    int opno;			/* Operand number matched.  */

    struct {
      int code_number;		/* Insn number matched.  */
      int lineno;		/* Line number of the insn.  */
      int num_clobbers_to_add;	/* Number of CLOBBERs to be added.  */
    } insn;
  } u;
};

/* Data structure for decision tree for recognizing legitimate insns.  */

struct decision
{
  struct decision_head success;	/* Nodes to test on success.  */
  struct decision *next;	/* Node to test on failure.  */
  struct decision *prev;	/* Node whose failure tests us.  */
  struct decision *afterward;	/* Node to test on success,
				   but failure of successor nodes.  */

  const char *position;		/* String denoting position in pattern.  */

  struct decision_test *tests;	/* The tests for this node.  */

  int number;			/* Node number, used for labels */
  int subroutine_number;	/* Number of subroutine this node starts */
  int need_label;		/* Label needs to be output.  */
};

#define SUBROUTINE_THRESHOLD	100

static int next_subroutine_number;

/* We can write three types of subroutines: One for insn recognition,
   one to split insns, and one for peephole-type optimizations.  This
   defines which type is being written.  */

enum routine_type {
  RECOG, SPLIT, PEEPHOLE2
};

#define IS_SPLIT(X) ((X) != RECOG)

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

/* The line number of the start of the pattern currently being processed.  */
static int pattern_lineno;

/* Count of errors.  */
static int error_count;

/* This table contains a list of the rtl codes that can possibly match a
   predicate defined in recog.c.  The function `maybe_both_true' uses it to
   deduce that there are no expressions that can be matches by certain pairs
   of tree nodes.  Also, if a predicate can match only one code, we can
   hardwire that code into the node testing the predicate.  */

static struct pred_table
{
  const char *name;
  RTX_CODE codes[NUM_RTX_CODE];
} preds[] = {
  {"general_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,
		       LABEL_REF, SUBREG, REG, MEM}},
#ifdef PREDICATE_CODES
  PREDICATE_CODES
#endif
  {"address_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,
		       LABEL_REF, SUBREG, REG, MEM, PLUS, MINUS, MULT}},
  {"register_operand", {SUBREG, REG}},
  {"pmode_register_operand", {SUBREG, REG}},
  {"scratch_operand", {SCRATCH, REG}},
  {"immediate_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,
			 LABEL_REF}},
  {"const_int_operand", {CONST_INT}},
  {"const_double_operand", {CONST_INT, CONST_DOUBLE}},
  {"nonimmediate_operand", {SUBREG, REG, MEM}},
  {"nonmemory_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,
			 LABEL_REF, SUBREG, REG}},
  {"push_operand", {MEM}},
  {"pop_operand", {MEM}},
  {"memory_operand", {SUBREG, MEM}},
  {"indirect_operand", {SUBREG, MEM}},
  {"comparison_operator", {EQ, NE, LE, LT, GE, GT, LEU, LTU, GEU, GTU}},
  {"mode_independent_operand", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,
				LABEL_REF, SUBREG, REG, MEM}}
};

#define NUM_KNOWN_PREDS (sizeof preds / sizeof preds[0])

static const char * special_mode_pred_table[] = {
#ifdef SPECIAL_MODE_PREDICATES
  SPECIAL_MODE_PREDICATES
#endif
  "pmode_register_operand"
};

#define NUM_SPECIAL_MODE_PREDS \
  (sizeof (special_mode_pred_table) / sizeof (special_mode_pred_table[0]))

static void message_with_line
  PARAMS ((int, const char *, ...)) ATTRIBUTE_PRINTF_2;

static struct decision *new_decision
  PARAMS ((const char *, struct decision_head *));
static struct decision_test *new_decision_test
  PARAMS ((enum decision_type, struct decision_test ***));
static rtx find_operand
  PARAMS ((rtx, int));
static void validate_pattern
  PARAMS ((rtx, rtx, rtx));
static struct decision *add_to_sequence
  PARAMS ((rtx, struct decision_head *, const char *, enum routine_type, int));

static int maybe_both_true_2
  PARAMS ((struct decision_test *, struct decision_test *));
static int maybe_both_true_1
  PARAMS ((struct decision_test *, struct decision_test *));
static int maybe_both_true
  PARAMS ((struct decision *, struct decision *, int));

static int nodes_identical_1
  PARAMS ((struct decision_test *, struct decision_test *));
static int nodes_identical
  PARAMS ((struct decision *, struct decision *));
static void merge_accept_insn
  PARAMS ((struct decision *, struct decision *));
static void merge_trees
  PARAMS ((struct decision_head *, struct decision_head *));

static void factor_tests
  PARAMS ((struct decision_head *));
static void simplify_tests
  PARAMS ((struct decision_head *));
static int break_out_subroutines
  PARAMS ((struct decision_head *, int));
static void find_afterward
  PARAMS ((struct decision_head *, struct decision *));

static void change_state
  PARAMS ((const char *, const char *, struct decision *, const char *));
static void print_code
  PARAMS ((enum rtx_code));
static void write_afterward
  PARAMS ((struct decision *, struct decision *, const char *));
static struct decision *write_switch
  PARAMS ((struct decision *, int));
static void write_cond
  PARAMS ((struct decision_test *, int, enum routine_type));
static void write_action
  PARAMS ((struct decision_test *, int, int, struct decision *,
	 enum routine_type));
static int is_unconditional
  PARAMS ((struct decision_test *, enum routine_type));
static int write_node
  PARAMS ((struct decision *, int, enum routine_type));
static void write_tree_1
  PARAMS ((struct decision_head *, int, enum routine_type));
static void write_tree
  PARAMS ((struct decision_head *, const char *, enum routine_type, int));
static void write_subroutine
  PARAMS ((struct decision_head *, enum routine_type));
static void write_subroutines
  PARAMS ((struct decision_head *, enum routine_type));
static void write_header
  PARAMS ((void));

static struct decision_head make_insn_sequence
  PARAMS ((rtx, enum routine_type));
static void process_tree
  PARAMS ((struct decision_head *, enum routine_type));
  
static void record_insn_name
  PARAMS ((int, const char *));

static void debug_decision_0
  PARAMS ((struct decision *, int, int));
static void debug_decision_1
  PARAMS ((struct decision *, int));
static void debug_decision_2
  PARAMS ((struct decision_test *));
extern void debug_decision
  PARAMS ((struct decision *));
extern void debug_decision_list
  PARAMS ((struct decision *));

static void
message_with_line VPARAMS ((int lineno, const char *msg, ...))
{
#ifndef ANSI_PROTOTYPES
  int lineno;
  const char *msg;
#endif
  va_list ap;

  VA_START (ap, msg);

#ifndef ANSI_PROTOTYPES
  lineno = va_arg (ap, int);
  msg = va_arg (ap, const char *);
#endif

  fprintf (stderr, "%s:%d: ", read_rtx_filename, lineno);
  vfprintf (stderr, msg, ap);
  fputc ('\n', stderr);

  va_end (ap);
}

/* Create a new node in sequence after LAST.  */

static struct decision *
new_decision (position, last)
     const char *position;
     struct decision_head *last;
{
  register struct decision *new
    = (struct decision *) xmalloc (sizeof (struct decision));

  memset (new, 0, sizeof (*new));
  new->success = *last;
  new->position = xstrdup (position);
  new->number = next_number++;

  last->first = last->last = new;
  return new;
}

/* Create a new test and link it in at PLACE.  */

static struct decision_test *
new_decision_test (type, pplace)
     enum decision_type type;
     struct decision_test ***pplace;
{
  struct decision_test **place = *pplace;
  struct decision_test *test;

  test = (struct decision_test *) xmalloc (sizeof (*test));
  test->next = *place;
  test->type = type;
  *place = test;

  place = &test->next;
  *pplace = place;

  return test;
}

/* Search for and return operand N.  */

static rtx
find_operand (pattern, n)
     rtx pattern;
     int n;
{
  const char *fmt;
  RTX_CODE code;
  int i, j, len;
  rtx r;

  code = GET_CODE (pattern);
  if ((code == MATCH_SCRATCH
       || code == MATCH_INSN
       || code == MATCH_OPERAND
       || code == MATCH_OPERATOR
       || code == MATCH_PARALLEL)
      && XINT (pattern, 0) == n)
    return pattern;

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      switch (fmt[i])
	{
	case 'e': case 'u':
	  if ((r = find_operand (XEXP (pattern, i), n)) != NULL_RTX)
	    return r;
	  break;

	case 'E':
	  for (j = 0; j < XVECLEN (pattern, i); j++)
	    if ((r = find_operand (XVECEXP (pattern, i, j), n)) != NULL_RTX)
	      return r;
	  break;

	case 'i': case 'w': case '0': case 's':
	  break;

	default:
	  abort ();
	}
    }

  return NULL;
}

/* Check for various errors in patterns.  SET is nonnull for a destination,
   and is the complete set pattern.  */

static void
validate_pattern (pattern, insn, set)
     rtx pattern;
     rtx insn;
     rtx set;
{
  const char *fmt;
  RTX_CODE code;
  size_t i, len;
  int j;

  code = GET_CODE (pattern);
  switch (code)
    {
    case MATCH_SCRATCH:
      return;

    case MATCH_INSN:
    case MATCH_OPERAND:
    case MATCH_OPERATOR:
      {
	const char *pred_name = XSTR (pattern, 1);
	int allows_non_lvalue = 1, allows_non_const = 1;
	int special_mode_pred = 0;
	const char *c_test;

	if (GET_CODE (insn) == DEFINE_INSN)
	  c_test = XSTR (insn, 2);
	else
	  c_test = XSTR (insn, 1);

	if (pred_name[0] != 0)
	  {
	    for (i = 0; i < NUM_KNOWN_PREDS; i++)
	      if (! strcmp (preds[i].name, pred_name))
		break;

	    if (i < NUM_KNOWN_PREDS)
	      {
		int j;

		allows_non_lvalue = allows_non_const = 0;
		for (j = 0; preds[i].codes[j] != 0; j++)
		  {
		    RTX_CODE c = preds[i].codes[j];
		    if (c != LABEL_REF
			&& c != SYMBOL_REF
			&& c != CONST_INT
			&& c != CONST_DOUBLE
			&& c != CONST
			&& c != HIGH
			&& c != CONSTANT_P_RTX)
		      allows_non_const = 1;

		    if (c != REG
			&& c != SUBREG
			&& c != MEM
			&& c != CONCAT
			&& c != PARALLEL
			&& c != STRICT_LOW_PART)
		      allows_non_lvalue = 1;
		  }
	      }
	    else
	      {
#ifdef PREDICATE_CODES
		/* If the port has a list of the predicates it uses but
		   omits one, warn.  */
		message_with_line (pattern_lineno,
				   "warning: `%s' not in PREDICATE_CODES",
				   pred_name);
#endif
	      }

	    for (i = 0; i < NUM_SPECIAL_MODE_PREDS; ++i)
	      if (strcmp (pred_name, special_mode_pred_table[i]) == 0)
		{
		  special_mode_pred = 1;
		  break;
		}
	  }

	/* A MATCH_OPERAND that is a SET should have an output reload.  */
	if (set
	    && code == MATCH_OPERAND
	    && XSTR (pattern, 2)[0] != '\0'
	    && XSTR (pattern, 2)[0] != '='
	    && XSTR (pattern, 2)[0] != '+')
	  {
	    message_with_line (pattern_lineno,
			       "operand %d missing output reload", 
			       XINT (pattern, 0));
	    error_count++;
	  }

	/* Allowing non-lvalues in destinations -- particularly CONST_INT --
	   while not likely to occur at runtime, results in less efficient
	   code from insn-recog.c.  */
	if (set
	    && pred_name[0] != '\0'
	    && allows_non_lvalue)
	  {
	    message_with_line (pattern_lineno,
			"warning: destination operand %d allows non-lvalue",
			XINT (pattern, 0));
	  }

	/* A modeless MATCH_OPERAND can be handy when we can
	   check for multiple modes in the c_test.  In most other cases,
	   it is a mistake.  Only DEFINE_INSN is eligible, since SPLIT
	   and PEEP2 can FAIL within the output pattern.  Exclude 
	   address_operand, since its mode is related to the mode of
	   the memory not the operand.  Exclude the SET_DEST of a call
	   instruction, as that is a common idiom.  */

	if (GET_MODE (pattern) == VOIDmode
	    && code == MATCH_OPERAND
	    && GET_CODE (insn) == DEFINE_INSN
	    && allows_non_const
	    && ! special_mode_pred
	    && pred_name[0] != '\0'
	    && strcmp (pred_name, "address_operand") != 0
	    && strstr (c_test, "operands") == NULL
	    && ! (set
		  && GET_CODE (set) == SET
		  && GET_CODE (SET_SRC (set)) == CALL))
	  {
	    message_with_line (pattern_lineno,
			       "warning: operand %d missing mode?",
			       XINT (pattern, 0));
	  }
	return;
      }

    case SET:
      {
	enum machine_mode dmode, smode;
	rtx dest, src;

	dest = SET_DEST (pattern);
	src = SET_SRC (pattern);

	/* Find the referant for a DUP.  */

	if (GET_CODE (dest) == MATCH_DUP
	    || GET_CODE (dest) == MATCH_OP_DUP
	    || GET_CODE (dest) == MATCH_PAR_DUP)
	  dest = find_operand (insn, XINT (dest, 0));

	if (GET_CODE (src) == MATCH_DUP
	    || GET_CODE (src) == MATCH_OP_DUP
	    || GET_CODE (src) == MATCH_PAR_DUP)
	  src = find_operand (insn, XINT (src, 0));

	/* STRICT_LOW_PART is a wrapper.  Its argument is the real
	   destination, and it's mode should match the source.  */
	if (GET_CODE (dest) == STRICT_LOW_PART)
	  dest = XEXP (dest, 0);

	dmode = GET_MODE (dest);
	smode = GET_MODE (src);

	/* The mode of an ADDRESS_OPERAND is the mode of the memory
	   reference, not the mode of the address.  */
	if (GET_CODE (src) == MATCH_OPERAND
	    && ! strcmp (XSTR (src, 1), "address_operand"))
	  ;

        /* The operands of a SET must have the same mode unless one
	   is VOIDmode.  */
        else if (dmode != VOIDmode && smode != VOIDmode && dmode != smode)
	  {
	    message_with_line (pattern_lineno,
			       "mode mismatch in set: %smode vs %smode",
			       GET_MODE_NAME (dmode), GET_MODE_NAME (smode));
	    error_count++;
	  }

	/* If only one of the operands is VOIDmode, and PC or CC0 is 
	   not involved, it's probably a mistake.  */
	else if (dmode != smode
		 && GET_CODE (dest) != PC
		 && GET_CODE (dest) != CC0
		 && GET_CODE (src) != PC
		 && GET_CODE (src) != CC0
		 && GET_CODE (src) != CONST_INT)
	  {
	    const char *which;
	    which = (dmode == VOIDmode ? "destination" : "source");
	    message_with_line (pattern_lineno,
			       "warning: %s missing a mode?", which);
	  }

	if (dest != SET_DEST (pattern))
	  validate_pattern (dest, insn, pattern);
	validate_pattern (SET_DEST (pattern), insn, pattern);
        validate_pattern (SET_SRC (pattern), insn, NULL_RTX);
        return;
      }

    case CLOBBER:
      validate_pattern (SET_DEST (pattern), insn, pattern);
      return;

    case LABEL_REF:
      if (GET_MODE (XEXP (pattern, 0)) != VOIDmode)
	{
	  message_with_line (pattern_lineno,
			     "operand to label_ref %smode not VOIDmode",
			     GET_MODE_NAME (GET_MODE (XEXP (pattern, 0))));
	  error_count++;
	}
      break;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      switch (fmt[i])
	{
	case 'e': case 'u':
	  validate_pattern (XEXP (pattern, i), insn, NULL_RTX);
	  break;

	case 'E':
	  for (j = 0; j < XVECLEN (pattern, i); j++)
	    validate_pattern (XVECEXP (pattern, i, j), insn, NULL_RTX);
	  break;

	case 'i': case 'w': case '0': case 's':
	  break;

	default:
	  abort ();
	}
    }
}

/* Create a chain of nodes to verify that an rtl expression matches
   PATTERN.

   LAST is a pointer to the listhead in the previous node in the chain (or
   in the calling function, for the first node).

   POSITION is the string representing the current position in the insn.

   INSN_TYPE is the type of insn for which we are emitting code.

   A pointer to the final node in the chain is returned.  */

static struct decision *
add_to_sequence (pattern, last, position, insn_type, top)
     rtx pattern;
     struct decision_head *last;
     const char *position;
     enum routine_type insn_type;
     int top;
{
  RTX_CODE code;
  struct decision *this, *sub;
  struct decision_test *test;
  struct decision_test **place;
  char *subpos;
  register size_t i;
  register const char *fmt;
  int depth = strlen (position);
  int len;
  enum machine_mode mode;

  if (depth > max_depth)
    max_depth = depth;

  subpos = (char *) alloca (depth + 2);
  strcpy (subpos, position);
  subpos[depth + 1] = 0;

  sub = this = new_decision (position, last);
  place = &this->tests;

 restart:
  mode = GET_MODE (pattern);
  code = GET_CODE (pattern);

  switch (code)
    {
    case PARALLEL:
      /* Toplevel peephole pattern. */
      if (insn_type == PEEPHOLE2 && top)
	{
	  /* We don't need the node we just created -- unlink it.  */
	  last->first = last->last = NULL;

	  for (i = 0; i < (size_t) XVECLEN (pattern, 0); i++)
	    {
	      /* Which insn we're looking at is represented by A-Z. We don't
	         ever use 'A', however; it is always implied. */

	      subpos[depth] = (i > 0 ? 'A' + i : 0);
	      sub = add_to_sequence (XVECEXP (pattern, 0, i),
				     last, subpos, insn_type, 0);
	      last = &sub->success;
	    }
	  return sub;
	}

      /* Else nothing special.  */
      break;

    case MATCH_OPERAND:
    case MATCH_SCRATCH:
    case MATCH_OPERATOR:
    case MATCH_PARALLEL:
    case MATCH_INSN:
      {
	const char *pred_name;
	RTX_CODE was_code = code;
	int allows_const_int = 1;

	if (code == MATCH_SCRATCH)
	  {
	    pred_name = "scratch_operand";
	    code = UNKNOWN;
	  }
	else
	  {
	    pred_name = XSTR (pattern, 1);
	    if (code == MATCH_PARALLEL)
	      code = PARALLEL;
	    else
	      code = UNKNOWN;
	  }

	/* We know exactly what const_int_operand matches -- any CONST_INT.  */
	if (strcmp ("const_int_operand", pred_name) == 0)
	  {
	    code = CONST_INT;
	    mode = VOIDmode;
	  }
	else if (pred_name[0] != 0)
	  {
	    test = new_decision_test (DT_pred, &place);
	    test->u.pred.name = pred_name;
	    test->u.pred.mode = mode;

	    /* See if we know about this predicate and save its number.  If
	       we do, and it only accepts one code, note that fact.  The
	       predicate `const_int_operand' only tests for a CONST_INT, so
	       if we do so we can avoid calling it at all.

	       Finally, if we know that the predicate does not allow
	       CONST_INT, we know that the only way the predicate can match
	       is if the modes match (here we use the kludge of relying on
	       the fact that "address_operand" accepts CONST_INT; otherwise,
	       it would have to be a special case), so we can test the mode
	       (but we need not).  This fact should considerably simplify the
	       generated code.  */

	    for (i = 0; i < NUM_KNOWN_PREDS; i++)
	      if (! strcmp (preds[i].name, pred_name))
		break;

	    if (i < NUM_KNOWN_PREDS)
	      {
		int j;

		test->u.pred.index = i;

		if (preds[i].codes[1] == 0 && code == UNKNOWN)
		  code = preds[i].codes[0];

		allows_const_int = 0;
		for (j = 0; preds[i].codes[j] != 0; j++)
		  if (preds[i].codes[j] == CONST_INT)
		    {
		      allows_const_int = 1;
		      break;
		    }
	      }
	    else
	      test->u.pred.index = -1;
	  }

	/* Can't enforce a mode if we allow const_int.  */
	if (allows_const_int)
	  mode = VOIDmode;

	/* Accept the operand, ie. record it in `operands'.  */
	test = new_decision_test (DT_accept_op, &place);
	test->u.opno = XINT (pattern, 0);

	if (was_code == MATCH_OPERATOR || was_code == MATCH_PARALLEL)
	  {
	    char base = (was_code == MATCH_OPERATOR ? '0' : 'a');
	    for (i = 0; i < (size_t) XVECLEN (pattern, 2); i++)
	      {
		subpos[depth] = i + base;
		sub = add_to_sequence (XVECEXP (pattern, 2, i),
				       &sub->success, subpos, insn_type, 0);
	      }
	  }
	goto fini;
      }

    case MATCH_OP_DUP:
      code = UNKNOWN;

      test = new_decision_test (DT_dup, &place);
      test->u.dup = XINT (pattern, 0);

      test = new_decision_test (DT_accept_op, &place);
      test->u.opno = XINT (pattern, 0);

      for (i = 0; i < (size_t) XVECLEN (pattern, 1); i++)
	{
	  subpos[depth] = i + '0';
	  sub = add_to_sequence (XVECEXP (pattern, 1, i),
				 &sub->success, subpos, insn_type, 0);
	}
      goto fini;

    case MATCH_DUP:
    case MATCH_PAR_DUP:
      code = UNKNOWN;

      test = new_decision_test (DT_dup, &place);
      test->u.dup = XINT (pattern, 0);
      goto fini;

    case ADDRESS:
      pattern = XEXP (pattern, 0);
      goto restart;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);

  /* Do tests against the current node first.  */
  for (i = 0; i < (size_t) len; i++)
    {
      if (fmt[i] == 'i')
	{
	  if (i == 0)
	    {
	      test = new_decision_test (DT_elt_zero_int, &place);
	      test->u.intval = XINT (pattern, i);
	    }
	  else if (i == 1)
	    {
	      test = new_decision_test (DT_elt_one_int, &place);
	      test->u.intval = XINT (pattern, i);
	    }
	  else
	    abort ();
	}
      else if (fmt[i] == 'w')
	{
	  if (i != 0)
	    abort ();

	  test = new_decision_test (DT_elt_zero_wide, &place);
	  test->u.intval = XWINT (pattern, i);
	}
      else if (fmt[i] == 'E')
	{
	  if (i != 0)
	    abort ();

	  test = new_decision_test (DT_veclen, &place);
	  test->u.veclen = XVECLEN (pattern, i);
	}
    }

  /* Now test our sub-patterns.  */
  for (i = 0; i < (size_t) len; i++)
    {
      switch (fmt[i])
	{
	case 'e': case 'u':
	  subpos[depth] = '0' + i;
	  sub = add_to_sequence (XEXP (pattern, i), &sub->success,
				 subpos, insn_type, 0);
	  break;

	case 'E':
	  {
	    register int j;
	    for (j = 0; j < XVECLEN (pattern, i); j++)
	      {
		subpos[depth] = 'a' + j;
		sub = add_to_sequence (XVECEXP (pattern, i, j),
				       &sub->success, subpos, insn_type, 0);
	      }
	    break;
	  }

	case 'i': case 'w':
	  /* Handled above.  */
	  break;
	case '0':
	  break;

	default:
	  abort ();
	}
    }

 fini:
  /* Insert nodes testing mode and code, if they're still relevant,
     before any of the nodes we may have added above.  */
  if (code != UNKNOWN)
    {
      place = &this->tests;
      test = new_decision_test (DT_code, &place);
      test->u.code = code;
    }

  if (mode != VOIDmode)
    {
      place = &this->tests;
      test = new_decision_test (DT_mode, &place);
      test->u.mode = mode;
    }

  /* If we didn't insert any tests or accept nodes, hork.  */
  if (this->tests == NULL)
    abort ();

  return sub;
}

/* A subroutine of maybe_both_true; examines only one test.
   Returns > 0 for "definitely both true" and < 0 for "maybe both true".  */

static int
maybe_both_true_2 (d1, d2)
     struct decision_test *d1, *d2;
{
  if (d1->type == d2->type)
    {
      switch (d1->type)
	{
	case DT_mode:
	  return d1->u.mode == d2->u.mode;

	case DT_code:
	  return d1->u.code == d2->u.code;

	case DT_veclen:
	  return d1->u.veclen == d2->u.veclen;

	case DT_elt_zero_int:
	case DT_elt_one_int:
	case DT_elt_zero_wide:
	  return d1->u.intval == d2->u.intval;

	default:
	  break;
	}
    }

  /* If either has a predicate that we know something about, set
     things up so that D1 is the one that always has a known
     predicate.  Then see if they have any codes in common.  */

  if (d1->type == DT_pred || d2->type == DT_pred)
    {
      if (d2->type == DT_pred)
	{
	  struct decision_test *tmp;
	  tmp = d1, d1 = d2, d2 = tmp;
	}

      /* If D2 tests a mode, see if it matches D1.  */
      if (d1->u.pred.mode != VOIDmode)
	{
	  if (d2->type == DT_mode)
	    {
	      if (d1->u.pred.mode != d2->u.mode
		  /* The mode of an address_operand predicate is the
		     mode of the memory, not the operand.  It can only
		     be used for testing the predicate, so we must
		     ignore it here.  */
		  && strcmp (d1->u.pred.name, "address_operand") != 0)
		return 0;
	    }
	  /* Don't check two predicate modes here, because if both predicates
	     accept CONST_INT, then both can still be true even if the modes
	     are different.  If they don't accept CONST_INT, there will be a
	     separate DT_mode that will make maybe_both_true_1 return 0.  */
	}

      if (d1->u.pred.index >= 0)
	{
	  /* If D2 tests a code, see if it is in the list of valid
	     codes for D1's predicate.  */
	  if (d2->type == DT_code)
	    {
	      const RTX_CODE *c = &preds[d1->u.pred.index].codes[0];
	      while (*c != 0)
		{
		  if (*c == d2->u.code)
		    break;
		  ++c;
		}
	      if (*c == 0)
		return 0;
	    }

	  /* Otherwise see if the predicates have any codes in common.  */
	  else if (d2->type == DT_pred && d2->u.pred.index >= 0)
	    {
	      const RTX_CODE *c1 = &preds[d1->u.pred.index].codes[0];
	      int common = 0;

	      while (*c1 != 0 && !common)
		{
		  const RTX_CODE *c2 = &preds[d2->u.pred.index].codes[0];
		  while (*c2 != 0 && !common)
		    {
		      common = (*c1 == *c2);
		      ++c2;
		    }
		  ++c1;
		}

	      if (!common)
		return 0;
	    }
	}
    }

  return -1;
}

/* A subroutine of maybe_both_true; examines all the tests for a given node.
   Returns > 0 for "definitely both true" and < 0 for "maybe both true".  */

static int
maybe_both_true_1 (d1, d2)
     struct decision_test *d1, *d2;
{
  struct decision_test *t1, *t2;

  /* A match_operand with no predicate can match anything.  Recognize
     this by the existance of a lone DT_accept_op test.  */
  if (d1->type == DT_accept_op || d2->type == DT_accept_op)
    return 1;

  /* Eliminate pairs of tests while they can exactly match.  */
  while (d1 && d2 && d1->type == d2->type)
    {
      if (maybe_both_true_2 (d1, d2) == 0)
	return 0;
      d1 = d1->next, d2 = d2->next;
    }

  /* After that, consider all pairs.  */
  for (t1 = d1; t1 ; t1 = t1->next)
    for (t2 = d2; t2 ; t2 = t2->next)
      if (maybe_both_true_2 (t1, t2) == 0)
	return 0;

  return -1;
}

/* Return 0 if we can prove that there is no RTL that can match both
   D1 and D2.  Otherwise, return 1 (it may be that there is an RTL that
   can match both or just that we couldn't prove there wasn't such an RTL).

   TOPLEVEL is non-zero if we are to only look at the top level and not
   recursively descend.  */

static int
maybe_both_true (d1, d2, toplevel)
     struct decision *d1, *d2;
     int toplevel;
{
  struct decision *p1, *p2;
  int cmp;

  /* Don't compare strings on the different positions in insn.  Doing so
     is incorrect and results in false matches from constructs like

	[(set (subreg:HI (match_operand:SI "register_operand" "r") 0)
	      (subreg:HI (match_operand:SI "register_operand" "r") 0))]
     vs
	[(set (match_operand:HI "register_operand" "r")
	      (match_operand:HI "register_operand" "r"))]

     If we are presented with such, we are recursing through the remainder
     of a node's success nodes (from the loop at the end of this function).
     Skip forward until we come to a position that matches.

     Due to the way position strings are constructed, we know that iterating
     forward from the lexically lower position (e.g. "00") will run into
     the lexically higher position (e.g. "1") and not the other way around.
     This saves a bit of effort.  */

  cmp = strcmp (d1->position, d2->position);
  if (cmp != 0)
    {
      if (toplevel)
	abort();

      /* If the d2->position was lexically lower, swap.  */
      if (cmp > 0)
	p1 = d1, d1 = d2, d2 = p1;

      if (d1->success.first == 0)
	return 0;
      for (p1 = d1->success.first; p1; p1 = p1->next)
	if (maybe_both_true (p1, d2, 0))
	  return 1;

      return 0;
    }

  /* Test the current level.  */
  cmp = maybe_both_true_1 (d1->tests, d2->tests);
  if (cmp >= 0)
    return cmp;

  /* We can't prove that D1 and D2 cannot both be true.  If we are only
     to check the top level, return 1.  Otherwise, see if we can prove
     that all choices in both successors are mutually exclusive.  If
     either does not have any successors, we can't prove they can't both
     be true.  */

  if (toplevel || d1->success.first == 0 || d2->success.first == 0)
    return 1;

  for (p1 = d1->success.first; p1; p1 = p1->next)
    for (p2 = d2->success.first; p2; p2 = p2->next)
      if (maybe_both_true (p1, p2, 0))
	return 1;

  return 0;
}

/* A subroutine of nodes_identical.  Examine two tests for equivalence.  */

static int
nodes_identical_1 (d1, d2)
     struct decision_test *d1, *d2;
{
  switch (d1->type)
    {
    case DT_mode:
      return d1->u.mode == d2->u.mode;

    case DT_code:
      return d1->u.code == d2->u.code;

    case DT_pred:
      return (d1->u.pred.mode == d2->u.pred.mode
	      && strcmp (d1->u.pred.name, d2->u.pred.name) == 0);

    case DT_c_test:
      return strcmp (d1->u.c_test, d2->u.c_test) == 0;

    case DT_veclen:
      return d1->u.veclen == d2->u.veclen;

    case DT_dup:
      return d1->u.dup == d2->u.dup;

    case DT_elt_zero_int:
    case DT_elt_one_int:
    case DT_elt_zero_wide:
      return d1->u.intval == d2->u.intval;

    case DT_accept_op:
      return d1->u.opno == d2->u.opno;

    case DT_accept_insn:
      /* Differences will be handled in merge_accept_insn.  */
      return 1;

    default:
      abort ();
    }
}

/* True iff the two nodes are identical (on one level only).  Due
   to the way these lists are constructed, we shouldn't have to 
   consider different orderings on the tests.  */

static int
nodes_identical (d1, d2)
     struct decision *d1, *d2;
{
  struct decision_test *t1, *t2;

  for (t1 = d1->tests, t2 = d2->tests; t1 && t2; t1 = t1->next, t2 = t2->next)
    {
      if (t1->type != t2->type)
	return 0;
      if (! nodes_identical_1 (t1, t2))
	return 0;
    }

  /* For success, they should now both be null.  */
  if (t1 != t2)
    return 0;

  /* Check that their subnodes are at the same position, as any one set
     of sibling decisions must be at the same position.  */
  if (d1->success.first
      && d2->success.first
      && strcmp (d1->success.first->position, d2->success.first->position))
    return 0;

  return 1;
}

/* A subroutine of merge_trees; given two nodes that have been declared
   identical, cope with two insn accept states.  If they differ in the
   number of clobbers, then the conflict was created by make_insn_sequence
   and we can drop the with-clobbers version on the floor.  If both 
   nodes have no additional clobbers, we have found an ambiguity in the
   source machine description.  */

static void
merge_accept_insn (oldd, addd)
     struct decision *oldd, *addd;
{
  struct decision_test *old, *add;

  for (old = oldd->tests; old; old = old->next)
    if (old->type == DT_accept_insn)
      break;
  if (old == NULL)
    return;

  for (add = addd->tests; add; add = add->next)
    if (add->type == DT_accept_insn)
      break;
  if (add == NULL)
    return;

  /* If one node is for a normal insn and the second is for the base
     insn with clobbers stripped off, the second node should be ignored.  */

  if (old->u.insn.num_clobbers_to_add == 0
      && add->u.insn.num_clobbers_to_add > 0)
    {
      /* Nothing to do here.  */
    }
  else if (old->u.insn.num_clobbers_to_add > 0
	   && add->u.insn.num_clobbers_to_add == 0)
    {
      /* In this case, replace OLD with ADD.  */
      old->u.insn = add->u.insn;
    }
  else
    {
      message_with_line (add->u.insn.lineno, "`%s' matches `%s'",
			 get_insn_name (add->u.insn.code_number),
			 get_insn_name (old->u.insn.code_number));
      message_with_line (old->u.insn.lineno, "previous definition of `%s'",
			 get_insn_name (old->u.insn.code_number));
      error_count++;
    }
}

/* Merge two decision trees OLDH and ADDH, modifying OLDH destructively.  */

static void
merge_trees (oldh, addh)
     struct decision_head *oldh, *addh;
{
  struct decision *next, *add;

  if (addh->first == 0)
    return;
  if (oldh->first == 0)
    {
      *oldh = *addh;
      return;
    }

  /* Trying to merge bits at different positions isn't possible.  */
  if (strcmp (oldh->first->position, addh->first->position))
    abort ();

  for (add = addh->first; add ; add = next)
    {
      struct decision *old, *insert_before = NULL;

      next = add->next;

      /* The semantics of pattern matching state that the tests are
	 done in the order given in the MD file so that if an insn
	 matches two patterns, the first one will be used.  However,
	 in practice, most, if not all, patterns are unambiguous so
	 that their order is independent.  In that case, we can merge
	 identical tests and group all similar modes and codes together.

	 Scan starting from the end of OLDH until we reach a point
	 where we reach the head of the list or where we pass a
	 pattern that could also be true if NEW is true.  If we find
	 an identical pattern, we can merge them.  Also, record the
	 last node that tests the same code and mode and the last one
	 that tests just the same mode.

	 If we have no match, place NEW after the closest match we found.  */
	 
      for (old = oldh->last; old; old = old->prev)
	{
	  if (nodes_identical (old, add))
	    {
	      merge_accept_insn (old, add);
	      merge_trees (&old->success, &add->success);
	      goto merged_nodes;
	    }

	  if (maybe_both_true (old, add, 0))
	    break;

	  /* Insert the nodes in DT test type order, which is roughly
	     how expensive/important the test is.  Given that the tests
	     are also ordered within the list, examining the first is
	     sufficient.  */
	  if (add->tests->type < old->tests->type)
	    insert_before = old;
	}

      if (insert_before == NULL)
	{
	  add->next = NULL;
	  add->prev = oldh->last;
	  oldh->last->next = add;
	  oldh->last = add;
	}
      else
	{
	  if ((add->prev = insert_before->prev) != NULL)
	    add->prev->next = add;
	  else
	    oldh->first = add;
	  add->next = insert_before;
	  insert_before->prev = add;
	}

    merged_nodes:;
    }
}

/* Walk the tree looking for sub-nodes that perform common tests.  
   Factor out the common test into a new node.  This enables us
   (depending on the test type) to emit switch statements later.  */

static void
factor_tests (head)
     struct decision_head *head;
{
  struct decision *first, *next;

  for (first = head->first; first && first->next; first = next)
    {
      enum decision_type type;
      struct decision *new, *old_last;

      type = first->tests->type;
      next = first->next;

      /* Want at least two compatible sequential nodes.  */
      if (next->tests->type != type)
	continue;

      /* Don't want all node types, just those we can turn into 
	 switch statements.  */
      if (type != DT_mode
	  && type != DT_code
	  && type != DT_veclen
	  && type != DT_elt_zero_int
	  && type != DT_elt_one_int
	  && type != DT_elt_zero_wide)
	continue;

      /* If we'd been performing more than one test, create a new node
         below our first test.  */
      if (first->tests->next != NULL)
	{
	  new = new_decision (first->position, &first->success);
	  new->tests = first->tests->next;
	  first->tests->next = NULL;
	}
	
      /* Crop the node tree off after our first test.  */
      first->next = NULL;
      old_last = head->last;
      head->last = first;

      /* For each compatible test, adjust to perform only one test in
	 the top level node, then merge the node back into the tree.  */
      do
	{
	  struct decision_head h;

	  if (next->tests->next != NULL)
	    {
	      new = new_decision (next->position, &next->success);
	      new->tests = next->tests->next;
	      next->tests->next = NULL;
	    }
	  new = next;
	  next = next->next;
	  new->next = NULL;
	  h.first = h.last = new;

	  merge_trees (head, &h);
	}
      while (next && next->tests->type == type);

      /* After we run out of compatible tests, graft the remaining nodes
	 back onto the tree.  */
      if (next)
	{
	  next->prev = head->last;
	  head->last->next = next;
	  head->last = old_last;
	}
    }

  /* Recurse.  */
  for (first = head->first; first; first = first->next)
    factor_tests (&first->success);
}

/* After factoring, try to simplify the tests on any one node.
   Tests that are useful for switch statements are recognizable
   by having only a single test on a node -- we'll be manipulating
   nodes with multiple tests:

   If we have mode tests or code tests that are redundant with
   predicates, remove them.  */

static void
simplify_tests (head)
     struct decision_head *head;
{
  struct decision *tree;

  for (tree = head->first; tree; tree = tree->next)
    {
      struct decision_test *a, *b;

      a = tree->tests;
      b = a->next;
      if (b == NULL)
	continue;

      /* Find a predicate node.  */
      while (b && b->type != DT_pred)
	b = b->next;
      if (b)
	{
	  /* Due to how these tests are constructed, we don't even need
	     to check that the mode and code are compatible -- they were
	     generated from the predicate in the first place.  */
	  while (a->type == DT_mode || a->type == DT_code)
	    a = a->next;
	  tree->tests = a;
	}
    }

  /* Recurse.  */
  for (tree = head->first; tree; tree = tree->next)
    simplify_tests (&tree->success);
}

/* Count the number of subnodes of HEAD.  If the number is high enough,
   make the first node in HEAD start a separate subroutine in the C code
   that is generated.  */

static int
break_out_subroutines (head, initial)
     struct decision_head *head;
     int initial;
{
  int size = 0;
  struct decision *sub;

  for (sub = head->first; sub; sub = sub->next)
    size += 1 + break_out_subroutines (&sub->success, 0);

  if (size > SUBROUTINE_THRESHOLD && ! initial)
    {
      head->first->subroutine_number = ++next_subroutine_number;
      size = 1;
    }
  return size;
}

/* For each node p, find the next alternative that might be true
   when p is true.  */

static void
find_afterward (head, real_afterward)
     struct decision_head *head;
     struct decision *real_afterward;
{
  struct decision *p, *q, *afterward;

  /* We can't propogate alternatives across subroutine boundaries. 
     This is not incorrect, merely a minor optimization loss.  */

  p = head->first;
  afterward = (p->subroutine_number > 0 ? NULL : real_afterward);

  for ( ; p ; p = p->next)
    {
      /* Find the next node that might be true if this one fails.  */
      for (q = p->next; q ; q = q->next)
	if (maybe_both_true (p, q, 1))
	  break;

      /* If we reached the end of the list without finding one, 
	 use the incoming afterward position.  */
      if (!q)
	q = afterward;
      p->afterward = q;
      if (q)
	q->need_label = 1;
    }

  /* Recurse.  */
  for (p = head->first; p ; p = p->next)
    if (p->success.first)
      find_afterward (&p->success, p->afterward);

  /* When we are generating a subroutine, record the real afterward
     position in the first node where write_tree can find it, and we
     can do the right thing at the subroutine call site.  */
  p = head->first;
  if (p->subroutine_number > 0)
    p->afterward = real_afterward;
}

/* Assuming that the state of argument is denoted by OLDPOS, take whatever
   actions are necessary to move to NEWPOS.  If we fail to move to the
   new state, branch to node AFTERWARD if non-zero, otherwise return.

   Failure to move to the new state can only occur if we are trying to
   match multiple insns and we try to step past the end of the stream. */

static void
change_state (oldpos, newpos, afterward, indent)
     const char *oldpos;
     const char *newpos;
     struct decision *afterward;
     const char *indent;
{
  int odepth = strlen (oldpos);
  int ndepth = strlen (newpos);
  int depth;
  int old_has_insn, new_has_insn;

  /* Pop up as many levels as necessary.  */
  for (depth = odepth; strncmp (oldpos, newpos, depth) != 0; --depth)
    continue;

  /* Hunt for the last [A-Z] in both strings.  */
  for (old_has_insn = odepth - 1; old_has_insn >= 0; --old_has_insn)
    if (oldpos[old_has_insn] >= 'A' && oldpos[old_has_insn] <= 'Z')
      break;
  for (new_has_insn = ndepth - 1; new_has_insn >= 0; --new_has_insn)
    if (newpos[new_has_insn] >= 'A' && newpos[new_has_insn] <= 'Z')
      break;

  /* Make sure to reset the _last_insn pointer when popping back up.  */
  if (old_has_insn >= 0 && new_has_insn < 0)
    printf ("%s_last_insn = insn;\n", indent);

  /* Go down to desired level.  */
  while (depth < ndepth)
    {
      /* It's a different insn from the first one. */
      if (newpos[depth] >= 'A' && newpos[depth] <= 'Z')
	{
	  /* We can only fail if we're moving down the tree.  */
	  if (old_has_insn >= 0 && oldpos[old_has_insn] >= newpos[depth])
	    {
	      printf ("%s_last_insn = recog_next_insn (insn, %d);\n", 
		      indent, newpos[depth] - 'A');
	    }
	  else
	    {
	      printf ("%stem = recog_next_insn (insn, %d);\n", 
		      indent, newpos[depth] - 'A');
	      printf ("%sif (tem == NULL_RTX)\n", indent);
	      if (afterward)
		printf ("%s  goto L%d;\n", indent, afterward->number);
	      else
		printf ("%s  goto ret0;\n", indent);
	      printf ("%s_last_insn = tem;\n", indent);
	    }
	  printf ("%sx%d = PATTERN (_last_insn);\n", indent, depth + 1);
	}
      else if (newpos[depth] >= 'a' && newpos[depth] <= 'z')
	printf ("%sx%d = XVECEXP (x%d, 0, %d);\n",
		indent, depth + 1, depth, newpos[depth] - 'a');
      else
	printf ("%sx%d = XEXP (x%d, %c);\n",
		indent, depth + 1, depth, newpos[depth]);
      ++depth;
    }
}

/* Print the enumerator constant for CODE -- the upcase version of
   the name.  */

static void
print_code (code)
     enum rtx_code code;
{
  register const char *p;
  for (p = GET_RTX_NAME (code); *p; p++)
    putchar (TOUPPER (*p));
}

/* Emit code to cross an afterward link -- change state and branch.  */

static void
write_afterward (start, afterward, indent)
     struct decision *start;
     struct decision *afterward;
     const char *indent;
{
  if (!afterward || start->subroutine_number > 0)
    printf("%sgoto ret0;\n", indent);
  else
    {
      change_state (start->position, afterward->position, NULL, indent);
      printf ("%sgoto L%d;\n", indent, afterward->number);
    }
}

/* Emit a switch statement, if possible, for an initial sequence of 
   nodes at START.  Return the first node yet untested.  */

static struct decision *
write_switch (start, depth)
     struct decision *start;
     int depth;
{
  struct decision *p = start;
  enum decision_type type = p->tests->type;

  /* If we have two or more nodes in sequence that test the same one
     thing, we may be able to use a switch statement.  */

  if (!p->next
      || p->tests->next
      || p->next->tests->type != type
      || p->next->tests->next)
    return p;

  /* DT_code is special in that we can do interesting things with
     known predicates at the same time.  */
  if (type == DT_code)
    {
      char codemap[NUM_RTX_CODE];
      struct decision *ret;
      RTX_CODE code;

      memset (codemap, 0, sizeof(codemap));

      printf ("  switch (GET_CODE (x%d))\n    {\n", depth);
      code = p->tests->u.code;
      do 
	{
	  printf ("    case ");
	  print_code (code);
	  printf (":\n      goto L%d;\n", p->success.first->number);
	  p->success.first->need_label = 1;

	  codemap[code] = 1;
	  p = p->next;
	}
      while (p
	     && ! p->tests->next
	     && p->tests->type == DT_code
	     && ! codemap[code = p->tests->u.code]);

      /* If P is testing a predicate that we know about and we haven't
	 seen any of the codes that are valid for the predicate, we can
	 write a series of "case" statement, one for each possible code.
	 Since we are already in a switch, these redundant tests are very
	 cheap and will reduce the number of predicates called.  */

      /* Note that while we write out cases for these predicates here,
	 we don't actually write the test here, as it gets kinda messy.
	 It is trivial to leave this to later by telling our caller that
	 we only processed the CODE tests.  */
      ret = p;

      while (p && p->tests->type == DT_pred
	     && p->tests->u.pred.index >= 0)
	{
	  const RTX_CODE *c;

	  for (c = &preds[p->tests->u.pred.index].codes[0]; *c ; ++c)
	    if (codemap[(int) *c] != 0)
	      goto pred_done;

	  for (c = &preds[p->tests->u.pred.index].codes[0]; *c ; ++c)
	    {
	      printf ("    case ");
	      print_code (*c);
	      printf (":\n");
	      codemap[(int) *c] = 1;
	    }

	  printf ("      goto L%d;\n", p->number);
	  p->need_label = 1;
	  p = p->next;
	}

    pred_done:
      /* Make the default case skip the predicates we managed to match.  */

      printf ("    default:\n");
      if (p != ret)
	{
	  if (p)
	    {
	      printf ("      goto L%d;\n", p->number);
	      p->need_label = 1;
	    }
	  else
	    write_afterward (start, start->afterward, "      ");
	}
      else
	printf ("     break;\n");
      printf ("   }\n");

      return ret;
    }
  else if (type == DT_mode
	   || type == DT_veclen
	   || type == DT_elt_zero_int
	   || type == DT_elt_one_int
	   || type == DT_elt_zero_wide)
    {
      printf ("  switch (");
      switch (type)
	{
	case DT_mode:
	  printf("GET_MODE (x%d)", depth);
	  break;
	case DT_veclen:
	  printf("XVECLEN (x%d, 0)", depth);
	  break;
	case DT_elt_zero_int:
	  printf("XINT (x%d, 0)", depth);
	  break;
	case DT_elt_one_int:
	  printf("XINT (x%d, 1)", depth);
	  break;
	case DT_elt_zero_wide:
	  printf("XWINT (x%d, 0)", depth);
	  break;
	default:
	  abort ();
	}
      printf (")\n    {\n");

      do
	{
	  printf ("    case ");
	  switch (type)
	    {
	    case DT_mode:
	      printf ("%smode", GET_MODE_NAME (p->tests->u.mode));
	      break;
	    case DT_veclen:
	      printf ("%d", p->tests->u.veclen);
	      break;
	    case DT_elt_zero_int:
	    case DT_elt_one_int:
	    case DT_elt_zero_wide:
	      printf (HOST_WIDE_INT_PRINT_DEC, p->tests->u.intval);
	      break;
	    default:
	      abort ();
	    }
	  printf (":\n      goto L%d;\n", p->success.first->number);
	  p->success.first->need_label = 1;

	  p = p->next;
	}
      while (p && p->tests->type == type && !p->tests->next);
      
      printf ("    default:\n      break;\n    }\n");

      return p;
    }
  else
    {
      /* None of the other tests are ameanable.  */
      return p;
    }
}

/* Emit code for one test.  */

static void
write_cond (p, depth, subroutine_type)
     struct decision_test *p;
     int depth;
     enum routine_type subroutine_type;
{
  switch (p->type)
    {
    case DT_mode:
      printf ("GET_MODE (x%d) == %smode", depth, GET_MODE_NAME (p->u.mode));
      break;

    case DT_code:
      printf ("GET_CODE (x%d) == ", depth);
      print_code (p->u.code);
      break;

    case DT_veclen:
      printf ("XVECLEN (x%d, 0) == %d", depth, p->u.veclen);
      break;

    case DT_elt_zero_int:
      printf ("XINT (x%d, 0) == %d", depth, (int) p->u.intval);
      break;

    case DT_elt_one_int:
      printf ("XINT (x%d, 1) == %d", depth, (int) p->u.intval);
      break;

    case DT_elt_zero_wide:
      printf ("XWINT (x%d, 0) == ", depth);
      printf (HOST_WIDE_INT_PRINT_DEC, p->u.intval);
      break;

    case DT_dup:
      printf ("rtx_equal_p (x%d, operands[%d])", depth, p->u.dup);
      break;

    case DT_pred:
      printf ("%s (x%d, %smode)", p->u.pred.name, depth,
	      GET_MODE_NAME (p->u.pred.mode));
      break;

    case DT_c_test:
      printf ("(%s)", p->u.c_test);
      break;

    case DT_accept_insn:
      switch (subroutine_type)
	{
	case RECOG:
	  if (p->u.insn.num_clobbers_to_add == 0)
	    abort ();
	  printf ("pnum_clobbers != NULL");
	  break;

	default:
	  abort ();
	}
      break;

    default:
      abort ();
    }
}

/* Emit code for one action.  The previous tests have succeeded;
   TEST is the last of the chain.  In the normal case we simply
   perform a state change.  For the `accept' tests we must do more work.  */

static void
write_action (test, depth, uncond, success, subroutine_type)
     struct decision_test *test;
     int depth, uncond;
     struct decision *success;
     enum routine_type subroutine_type;
{
  const char *indent;
  int want_close = 0;

  if (uncond)
    indent = "  ";
  else if (test->type == DT_accept_op || test->type == DT_accept_insn)
    {
      fputs ("    {\n", stdout);
      indent = "      ";
      want_close = 1;
    }
  else
    indent = "    ";

  if (test->type == DT_accept_op)
    {
      printf("%soperands[%d] = x%d;\n", indent, test->u.opno, depth);

      /* Only allow DT_accept_insn to follow.  */
      if (test->next)
	{
	  test = test->next;
	  if (test->type != DT_accept_insn)
	    abort ();
	}
    }

  /* Sanity check that we're now at the end of the list of tests.  */
  if (test->next)
    abort ();

  if (test->type == DT_accept_insn)
    {
      switch (subroutine_type)
	{
	case RECOG:
	  if (test->u.insn.num_clobbers_to_add != 0)
	    printf ("%s*pnum_clobbers = %d;\n",
		    indent, test->u.insn.num_clobbers_to_add);
	  printf ("%sreturn %d;\n", indent, test->u.insn.code_number);
	  break;

	case SPLIT:
	  printf ("%sreturn gen_split_%d (operands);\n",
		  indent, test->u.insn.code_number);
	  break;

	case PEEPHOLE2:
	  printf ("%stem = gen_peephole2_%d (insn, operands);\n",
		  indent, test->u.insn.code_number);
	  printf ("%sif (tem != 0)\n%s  goto ret1;\n", indent, indent);
	  break;

	default:
	  abort ();
	}
    }
  else
    {
      printf("%sgoto L%d;\n", indent, success->number);
      success->need_label = 1;
    }

  if (want_close)
    fputs ("    }\n", stdout);
}

/* Return 1 if the test is always true and has no fallthru path.  Return -1
   if the test does have a fallthru path, but requires that the condition be
   terminated.  Otherwise return 0 for a normal test.  */
/* ??? is_unconditional is a stupid name for a tri-state function.  */

static int
is_unconditional (t, subroutine_type)
     struct decision_test *t;
     enum routine_type subroutine_type;
{
  if (t->type == DT_accept_op)
    return 1;

  if (t->type == DT_accept_insn)
    {
      switch (subroutine_type)
	{
	case RECOG:
	  return (t->u.insn.num_clobbers_to_add == 0);
	case SPLIT:
	  return 1;
	case PEEPHOLE2:
	  return -1;
	default:
	  abort ();
	}
    }

  return 0;
}

/* Emit code for one node -- the conditional and the accompanying action.
   Return true if there is no fallthru path.  */

static int
write_node (p, depth, subroutine_type)
     struct decision *p;
     int depth;
     enum routine_type subroutine_type;
{
  struct decision_test *test, *last_test;
  int uncond;

  last_test = test = p->tests;
  uncond = is_unconditional (test, subroutine_type);
  if (uncond == 0)
    {
      printf ("  if (");
      write_cond (test, depth, subroutine_type);

      while ((test = test->next) != NULL)
	{
	  int uncond2;

	  last_test = test;
	  uncond2 = is_unconditional (test, subroutine_type);
	  if (uncond2 != 0)
	    break;

	  printf ("\n      && ");
	  write_cond (test, depth, subroutine_type);
	}

      printf (")\n");
    }

  write_action (last_test, depth, uncond, p->success.first, subroutine_type);

  return uncond > 0;
}

/* Emit code for all of the sibling nodes of HEAD.  */

static void
write_tree_1 (head, depth, subroutine_type)
     struct decision_head *head;
     int depth;
     enum routine_type subroutine_type;
{
  struct decision *p, *next;
  int uncond = 0;

  for (p = head->first; p ; p = next)
    {
      /* The label for the first element was printed in write_tree.  */
      if (p != head->first && p->need_label)
	OUTPUT_LABEL (" ", p->number);

      /* Attempt to write a switch statement for a whole sequence.  */
      next = write_switch (p, depth);
      if (p != next)
	uncond = 0;
      else
	{
	  /* Failed -- fall back and write one node.  */
	  uncond = write_node (p, depth, subroutine_type);
	  next = p->next;
	}
    }

  /* Finished with this chain.  Close a fallthru path by branching
     to the afterward node.  */
  if (! uncond)
    write_afterward (head->last, head->last->afterward, "  ");
}

/* Write out the decision tree starting at HEAD.  PREVPOS is the
   position at the node that branched to this node.  */

static void
write_tree (head, prevpos, type, initial)
     struct decision_head *head;
     const char *prevpos;
     enum routine_type type;
     int initial;
{
  register struct decision *p = head->first;

  putchar ('\n');
  if (p->need_label)
    OUTPUT_LABEL (" ", p->number);

  if (! initial && p->subroutine_number > 0)
    {
      static const char * const name_prefix[] = {
	  "recog", "split", "peephole2"
      };

      static const char * const call_suffix[] = {
	  ", pnum_clobbers", "", ", _plast_insn"
      };

      /* This node has been broken out into a separate subroutine.
	 Call it, test the result, and branch accordingly.  */

      if (p->afterward)
	{
	  printf ("  tem = %s_%d (x0, insn%s);\n",
		  name_prefix[type], p->subroutine_number, call_suffix[type]);
	  if (IS_SPLIT (type))
	    printf ("  if (tem != 0)\n    return tem;\n");
	  else
	    printf ("  if (tem >= 0)\n    return tem;\n");

	  change_state (p->position, p->afterward->position, NULL, "  ");
	  printf ("  goto L%d;\n", p->afterward->number);
	}
      else
	{
	  printf ("  return %s_%d (x0, insn%s);\n",
		  name_prefix[type], p->subroutine_number, call_suffix[type]);
	}
    }
  else
    {
      int depth = strlen (p->position);

      change_state (prevpos, p->position, head->last->afterward, "  ");
      write_tree_1 (head, depth, type);

      for (p = head->first; p; p = p->next)
        if (p->success.first)
          write_tree (&p->success, p->position, type, 0);
    }
}

/* Write out a subroutine of type TYPE to do comparisons starting at
   node TREE.  */

static void
write_subroutine (head, type)
     struct decision_head *head;
     enum routine_type type;
{
  int subfunction = head->first ? head->first->subroutine_number : 0;
  const char *s_or_e;
  char extension[32];
  int i;
  
  s_or_e = subfunction ? "static " : "";

  if (subfunction)
    sprintf (extension, "_%d", subfunction);
  else if (type == RECOG)
    extension[0] = '\0';
  else
    strcpy (extension, "_insns");

  switch (type)
    {
    case RECOG:
      printf ("%sint recog%s PARAMS ((rtx, rtx, int *));\n", s_or_e, extension);
      printf ("%sint\n\
recog%s (x0, insn, pnum_clobbers)\n\
     register rtx x0;\n\
     rtx insn ATTRIBUTE_UNUSED;\n\
     int *pnum_clobbers ATTRIBUTE_UNUSED;\n", s_or_e, extension);
      break;
    case SPLIT:
      printf ("%srtx split%s PARAMS ((rtx, rtx));\n", s_or_e, extension);
      printf ("%srtx\n\
split%s (x0, insn)\n\
     register rtx x0;\n\
     rtx insn ATTRIBUTE_UNUSED;\n", s_or_e, extension);
      break;
    case PEEPHOLE2:
      printf ("%srtx peephole2%s PARAMS ((rtx, rtx, rtx *));\n", s_or_e, extension);
      printf ("%srtx\n\
peephole2%s (x0, insn, _plast_insn)\n\
     register rtx x0;\n\
     rtx insn ATTRIBUTE_UNUSED;\n\
     rtx *_plast_insn ATTRIBUTE_UNUSED;\n", s_or_e, extension);
      break;
    }

  printf ("{\n  register rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];\n");
  for (i = 1; i <= max_depth; i++)
    printf ("  register rtx x%d ATTRIBUTE_UNUSED;\n", i);

  if (type == PEEPHOLE2)
    printf ("  register rtx _last_insn = insn;\n");
  printf ("  %s tem ATTRIBUTE_UNUSED;\n", IS_SPLIT (type) ? "rtx" : "int");

  if (head->first)
    write_tree (head, "", type, 1);
  else
    printf ("  goto ret0;\n");

  if (type == PEEPHOLE2)
    printf (" ret1:\n  *_plast_insn = _last_insn;\n  return tem;\n");
  printf (" ret0:\n  return %d;\n}\n\n", IS_SPLIT (type) ? 0 : -1);
}

/* In break_out_subroutines, we discovered the boundaries for the
   subroutines, but did not write them out.  Do so now.  */

static void
write_subroutines (head, type)
     struct decision_head *head;
     enum routine_type type;
{
  struct decision *p;

  for (p = head->first; p ; p = p->next)
    if (p->success.first)
      write_subroutines (&p->success, type);

  if (head->first->subroutine_number > 0)
    write_subroutine (head, type);
}

/* Begin the output file.  */

static void
write_header ()
{
  puts ("\
/* Generated automatically by the program `genrecog' from the target\n\
   machine description file.  */\n\
\n\
#include \"config.h\"\n\
#include \"system.h\"\n\
#include \"rtl.h\"\n\
#include \"tm_p.h\"\n\
#include \"function.h\"\n\
#include \"insn-config.h\"\n\
#include \"recog.h\"\n\
#include \"real.h\"\n\
#include \"output.h\"\n\
#include \"flags.h\"\n\
#include \"hard-reg-set.h\"\n\
#include \"resource.h\"\n\
\n");

  puts ("\n\
/* `recog' contains a decision tree that recognizes whether the rtx\n\
   X0 is a valid instruction.\n\
\n\
   recog returns -1 if the rtx is not valid.  If the rtx is valid, recog\n\
   returns a nonnegative number which is the insn code number for the\n\
   pattern that matched.  This is the same as the order in the machine\n\
   description of the entry that matched.  This number can be used as an\n\
   index into `insn_data' and other tables.\n\
\n\
   The third argument to recog is an optional pointer to an int.  If\n\
   present, recog will accept a pattern if it matches except for missing\n\
   CLOBBER expressions at the end.  In that case, the value pointed to by\n\
   the optional pointer will be set to the number of CLOBBERs that need\n\
   to be added (it should be initialized to zero by the caller).  If it\n\
   is set nonzero, the caller should allocate a PARALLEL of the\n\
   appropriate size, copy the initial entries, and call add_clobbers\n\
   (found in insn-emit.c) to fill in the CLOBBERs.\n\
");

  puts ("\n\
   The function split_insns returns 0 if the rtl could not\n\
   be split or the split rtl in a SEQUENCE if it can be.\n\
\n\
   The function peephole2_insns returns 0 if the rtl could not\n\
   be matched. If there was a match, the new rtl is returned in a SEQUENCE,\n\
   and LAST_INSN will point to the last recognized insn in the old sequence.\n\
*/\n\n");
}


/* Construct and return a sequence of decisions
   that will recognize INSN.

   TYPE says what type of routine we are recognizing (RECOG or SPLIT).  */

static struct decision_head
make_insn_sequence (insn, type)
     rtx insn;
     enum routine_type type;
{
  rtx x;
  const char *c_test = XSTR (insn, type == RECOG ? 2 : 1);
  struct decision *last;
  struct decision_test *test, **place;
  struct decision_head head;

  record_insn_name (next_insn_code, (type == RECOG ? XSTR (insn, 0) : NULL));

  if (type == PEEPHOLE2)
    {
      int i, j;

      /* peephole2 gets special treatment:
	 - X always gets an outer parallel even if it's only one entry
	 - we remove all traces of outer-level match_scratch and match_dup
           expressions here.  */
      x = rtx_alloc (PARALLEL);
      PUT_MODE (x, VOIDmode);
      XVEC (x, 0) = rtvec_alloc (XVECLEN (insn, 0));
      for (i = j = 0; i < XVECLEN (insn, 0); i++)
	{
	  rtx tmp = XVECEXP (insn, 0, i);
	  if (GET_CODE (tmp) != MATCH_SCRATCH && GET_CODE (tmp) != MATCH_DUP)
	    {
	      XVECEXP (x, 0, j) = tmp;
	      j++;
	    }
	}
      XVECLEN (x, 0) = j;
    }
  else if (XVECLEN (insn, type == RECOG) == 1)
    x = XVECEXP (insn, type == RECOG, 0);
  else
    {
      x = rtx_alloc (PARALLEL);
      XVEC (x, 0) = XVEC (insn, type == RECOG);
      PUT_MODE (x, VOIDmode);
    }

  validate_pattern (x, insn, NULL_RTX);

  memset(&head, 0, sizeof(head));
  last = add_to_sequence (x, &head, "", type, 1);

  /* Find the end of the test chain on the last node.  */
  for (test = last->tests; test->next; test = test->next)
    continue;
  place = &test->next;

  if (c_test[0])
    {
      /* Need a new node if we have another test to add.  */
      if (test->type == DT_accept_op)
	{
	  last = new_decision ("", &last->success);
	  place = &last->tests;
	}
      test = new_decision_test (DT_c_test, &place);
      test->u.c_test = c_test;
    }

  test = new_decision_test (DT_accept_insn, &place);
  test->u.insn.code_number = next_insn_code;
  test->u.insn.lineno = pattern_lineno;
  test->u.insn.num_clobbers_to_add = 0;

  switch (type)
    {
    case RECOG:
      /* If this is an DEFINE_INSN and X is a PARALLEL, see if it ends
	 with a group of CLOBBERs of (hard) registers or MATCH_SCRATCHes.
	 If so, set up to recognize the pattern without these CLOBBERs.  */

      if (GET_CODE (x) == PARALLEL)
	{
	  int i;

	  /* Find the last non-clobber in the parallel.  */
	  for (i = XVECLEN (x, 0); i > 0; i--)
	    {
	      rtx y = XVECEXP (x, 0, i - 1);
	      if (GET_CODE (y) != CLOBBER
		  || (GET_CODE (XEXP (y, 0)) != REG
		      && GET_CODE (XEXP (y, 0)) != MATCH_SCRATCH))
		break;
	    }

	  if (i != XVECLEN (x, 0))
	    {
	      rtx new;
	      struct decision_head clobber_head;

	      /* Build a similar insn without the clobbers.  */
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

	      /* Recognize it.  */
	      memset (&clobber_head, 0, sizeof(clobber_head));
	      last = add_to_sequence (new, &clobber_head, "", type, 1);

	      /* Find the end of the test chain on the last node.  */
	      for (test = last->tests; test->next; test = test->next)
		continue;

	      /* We definitely have a new test to add -- create a new
		 node if needed.  */
	      place = &test->next;
	      if (test->type == DT_accept_op)
		{
		  last = new_decision ("", &last->success);
		  place = &last->tests;
		}

	      if (c_test[0])
		{
		  test = new_decision_test (DT_c_test, &place);
		  test->u.c_test = c_test;
		}

	      test = new_decision_test (DT_accept_insn, &place);
	      test->u.insn.code_number = next_insn_code;
	      test->u.insn.lineno = pattern_lineno;
	      test->u.insn.num_clobbers_to_add = XVECLEN (x, 0) - i;

	      merge_trees (&head, &clobber_head);
	    }
	}
      break;

    case SPLIT:
      /* Define the subroutine we will call below and emit in genemit.  */
      printf ("extern rtx gen_split_%d PARAMS ((rtx *));\n", next_insn_code);
      break;

    case PEEPHOLE2:
      /* Define the subroutine we will call below and emit in genemit.  */
      printf ("extern rtx gen_peephole2_%d PARAMS ((rtx, rtx *));\n",
	      next_insn_code);
      break;
    }
  next_insn_code++;

  return head;
}

static void
process_tree (head, subroutine_type)
     struct decision_head *head;
     enum routine_type subroutine_type;
{
  if (head->first == NULL)
    {
      /* We can elide peephole2_insns, but not recog or split_insns.  */
      if (subroutine_type == PEEPHOLE2)
	return;
    }
  else
    {
      factor_tests (head);

      next_subroutine_number = 0;
      break_out_subroutines (head, 1);
      find_afterward (head, NULL);

      /* We run this after find_afterward, because find_afterward needs
	 the redundant DT_mode tests on predicates to determine whether
	 two tests can both be true or not.  */
      simplify_tests(head);

      write_subroutines (head, subroutine_type);
    }

  write_subroutine (head, subroutine_type);
}

extern int main PARAMS ((int, char **));

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  struct decision_head recog_tree, split_tree, peephole2_tree, h;
  FILE *infile;
  register int c;

  progname = "genrecog";
  obstack_init (rtl_obstack);

  memset (&recog_tree, 0, sizeof recog_tree);
  memset (&split_tree, 0, sizeof split_tree);
  memset (&peephole2_tree, 0, sizeof peephole2_tree);

  if (argc <= 1)
    fatal ("No input file name.");

  infile = fopen (argv[1], "r");
  if (infile == 0)
    {
      perror (argv[1]);
      return FATAL_EXIT_CODE;
    }
  read_rtx_filename = argv[1];

  next_insn_code = 0;
  next_index = 0;

  write_header ();

  /* Read the machine description.  */

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == EOF)
	break;
      ungetc (c, infile);
      pattern_lineno = read_rtx_lineno;

      desc = read_rtx (infile);
      if (GET_CODE (desc) == DEFINE_INSN)
	{
	  h = make_insn_sequence (desc, RECOG);
	  merge_trees (&recog_tree, &h);
	}
      else if (GET_CODE (desc) == DEFINE_SPLIT)
	{
	  h = make_insn_sequence (desc, SPLIT);
	  merge_trees (&split_tree, &h);
	}
      else if (GET_CODE (desc) == DEFINE_PEEPHOLE2)
	{
	  h = make_insn_sequence (desc, PEEPHOLE2);
	  merge_trees (&peephole2_tree, &h);
	}
	
      if (GET_CODE (desc) == DEFINE_PEEPHOLE
	  || GET_CODE (desc) == DEFINE_EXPAND)
	next_insn_code++;
      next_index++;
    }

  if (error_count)
    return FATAL_EXIT_CODE;

  puts ("\n\n");

  process_tree (&recog_tree, RECOG);
  process_tree (&split_tree, SPLIT);
  process_tree (&peephole2_tree, PEEPHOLE2);

  fflush (stdout);
  return (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}

/* Define this so we can link with print-rtl.o to get debug_rtx function.  */
const char *
get_insn_name (code)
     int code;
{
  if (code < insn_name_ptr_size)
    return insn_name_ptr[code];
  else
    return NULL;
}

static void
record_insn_name (code, name)
     int code;
     const char *name;
{
  static const char *last_real_name = "insn";
  static int last_real_code = 0;
  char *new;

  if (insn_name_ptr_size <= code)
    {
      int new_size;
      new_size = (insn_name_ptr_size ? insn_name_ptr_size * 2 : 512);
      insn_name_ptr =
	(char **) xrealloc (insn_name_ptr, sizeof(char *) * new_size);
      memset (insn_name_ptr + insn_name_ptr_size, 0, 
	      sizeof(char *) * (new_size - insn_name_ptr_size));
      insn_name_ptr_size = new_size;
    }

  if (!name || name[0] == '\0')
    {
      new = xmalloc (strlen (last_real_name) + 10);
      sprintf (new, "%s+%d", last_real_name, code - last_real_code);
    }
  else
    {
      last_real_name = new = xstrdup (name);
      last_real_code = code;
    }
  
  insn_name_ptr[code] = new;
}  

char *
xstrdup (input)
  const char *input;
{
  register size_t len = strlen (input) + 1;
  register char *output = xmalloc (len);
  memcpy (output, input, len);
  return output;
}

PTR
xrealloc (old, size)
  PTR old;
  size_t size;
{
  register PTR ptr;
  if (old)
    ptr = (PTR) realloc (old, size);
  else
    ptr = (PTR) malloc (size);
  if (!ptr)
    fatal ("virtual memory exhausted");
  return ptr;
}

PTR
xmalloc (size)
  size_t size;
{
  register PTR val = (PTR) malloc (size);

  if (val == 0)
    fatal ("virtual memory exhausted");
  return val;
}

static void
debug_decision_2 (test)
     struct decision_test *test;
{
  switch (test->type)
    {
    case DT_mode:
      fprintf (stderr, "mode=%s", GET_MODE_NAME (test->u.mode));
      break;
    case DT_code:
      fprintf (stderr, "code=%s", GET_RTX_NAME (test->u.code));
      break;
    case DT_veclen:
      fprintf (stderr, "veclen=%d", test->u.veclen);
      break;
    case DT_elt_zero_int:
      fprintf (stderr, "elt0_i=%d", (int) test->u.intval);
      break;
    case DT_elt_one_int:
      fprintf (stderr, "elt1_i=%d", (int) test->u.intval);
      break;
    case DT_elt_zero_wide:
      fprintf (stderr, "elt0_w=");
      fprintf (stderr, HOST_WIDE_INT_PRINT_DEC, test->u.intval);
      break;
    case DT_dup:
      fprintf (stderr, "dup=%d", test->u.dup);
      break;
    case DT_pred:
      fprintf (stderr, "pred=(%s,%s)",
	       test->u.pred.name, GET_MODE_NAME(test->u.pred.mode));
      break;
    case DT_c_test:
      {
	char sub[16+4];
	strncpy (sub, test->u.c_test, sizeof(sub));
	memcpy (sub+16, "...", 4);
	fprintf (stderr, "c_test=\"%s\"", sub);
      }
      break;
    case DT_accept_op:
      fprintf (stderr, "A_op=%d", test->u.opno);
      break;
    case DT_accept_insn:
      fprintf (stderr, "A_insn=(%d,%d)", 
	       test->u.insn.code_number, test->u.insn.num_clobbers_to_add);
      break;

    default:
      abort ();
    }
}

static void
debug_decision_1 (d, indent)
     struct decision *d;
     int indent;
{
  int i;
  struct decision_test *test;

  if (d == NULL)
    {
      for (i = 0; i < indent; ++i)
	putc (' ', stderr);
      fputs ("(nil)\n", stderr);
      return;
    }

  for (i = 0; i < indent; ++i)
    putc (' ', stderr);

  putc ('{', stderr);
  test = d->tests;
  if (test)
    {
      debug_decision_2 (test);
      while ((test = test->next) != NULL)
	{
	  fputs (" + ", stderr);
	  debug_decision_2 (test);
	}
    }
  fprintf (stderr, "} %d n %d a %d\n", d->number,
	   (d->next ? d->next->number : -1),
	   (d->afterward ? d->afterward->number : -1));
}

static void
debug_decision_0 (d, indent, maxdepth)
     struct decision *d;
     int indent, maxdepth;
{
  struct decision *n;
  int i;

  if (maxdepth < 0)
    return;
  if (d == NULL)
    {
      for (i = 0; i < indent; ++i)
	putc (' ', stderr);
      fputs ("(nil)\n", stderr);
      return;
    }

  debug_decision_1 (d, indent);
  for (n = d->success.first; n ; n = n->next)
    debug_decision_0 (n, indent + 2, maxdepth - 1);
}

void
debug_decision (d)
     struct decision *d;
{
  debug_decision_0 (d, 0, 1000000);
}

void
debug_decision_list (d)
     struct decision *d;
{
  while (d)
    {
      debug_decision_0 (d, 0, 0);
      d = d->next;
    }
}
