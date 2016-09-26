/* Generate code from machine description to recognize rtl as insns.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


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
   rtl as an INSN list.

   This program also generates the function `peephole2_insns', which
   returns 0 if the rtl could not be matched.  If there was a match,
   the new rtl is returned in an INSN list, and LAST_INSN will point
   to the last recognized insn in the old sequence.


   At a high level, the algorithm used in this file is as follows:

   1. Build up a decision tree for each routine, using the following
      approach to matching an rtx:

      - First determine the "shape" of the rtx, based on GET_CODE,
	XVECLEN and XINT.  This phase examines SET_SRCs before SET_DESTs
	since SET_SRCs tend to be more distinctive.  It examines other
	operands in numerical order, since the canonicalization rules
	prefer putting complex operands of commutative operators first.

      - Next check modes and predicates.  This phase examines all
	operands in numerical order, even for SETs, since the mode of a
	SET_DEST is exact while the mode of a SET_SRC can be VOIDmode
	for constant integers.

      - Next check match_dups.

      - Finally check the C condition and (where appropriate) pnum_clobbers.

   2. Try to optimize the tree by removing redundant tests, CSEing tests,
      folding tests together, etc.

   3. Look for common subtrees and split them out into "pattern" routines.
      These common subtrees can be identical or they can differ in mode,
      code, or integer (usually an UNSPEC or UNSPEC_VOLATILE code).
      In the latter case the users of the pattern routine pass the
      appropriate mode, etc., as argument.  For example, if two patterns
      contain:

         (plus:SI (match_operand:SI 1 "register_operand")
	          (match_operand:SI 2 "register_operand"))

      we can split the associated matching code out into a subroutine.
      If a pattern contains:

         (minus:DI (match_operand:DI 1 "register_operand")
	           (match_operand:DI 2 "register_operand"))

      then we can consider using the same matching routine for both
      the plus and minus expressions, passing PLUS and SImode in the
      former case and MINUS and DImode in the latter case.

      The main aim of this phase is to reduce the compile time of the
      insn-recog.c code and to reduce the amount of object code in
      insn-recog.o.

   4. Split the matching trees into functions, trying to limit the
      size of each function to a sensible amount.

      Again, the main aim of this phase is to reduce the compile time
      of insn-recog.c.  (It doesn't help with the size of insn-recog.o.)

   5. Write out C++ code for each function.  */

#include "bconfig.h"
#define INCLUDE_ALGORITHM
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "errors.h"
#include "read-md.h"
#include "gensupport.h"

#undef GENERATOR_FILE
enum true_rtx_doe {
#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS) TRUE_##ENUM,
#include "rtl.def"
#undef DEF_RTL_EXPR
  FIRST_GENERATOR_RTX_CODE
};
#define NUM_TRUE_RTX_CODE ((int) FIRST_GENERATOR_RTX_CODE)
#define GENERATOR_FILE 1

/* Debugging variables to control which optimizations are performed.
   Note that disabling merge_states_p leads to very large output.  */
static const bool merge_states_p = true;
static const bool collapse_optional_decisions_p = true;
static const bool cse_tests_p = true;
static const bool simplify_tests_p = true;
static const bool use_operand_variables_p = true;
static const bool use_subroutines_p = true;
static const bool use_pattern_routines_p = true;

/* Whether to add comments for optional tests that we decided to keep.
   Can be useful when debugging the generator itself but is noise when
   debugging the generated code.  */
static const bool mark_optional_transitions_p = false;

/* Whether pattern routines should calculate positions relative to their
   rtx parameter rather than use absolute positions.  This e.g. allows
   a pattern routine to be shared between a plain SET and a PARALLEL
   that includes a SET.

   In principle it sounds like this should be useful, especially for
   recog_for_combine, where the plain SET form is generated automatically
   from a PARALLEL of a single SET and some CLOBBERs.  In practice it doesn't
   seem to help much and leads to slightly bigger object files.  */
static const bool relative_patterns_p = false;

/* Whether pattern routines should be allowed to test whether pnum_clobbers
   is null.  This requires passing pnum_clobbers around as a parameter.  */
static const bool pattern_have_num_clobbers_p = true;

/* Whether pattern routines should be allowed to test .md file C conditions.
   This requires passing insn around as a parameter, in case the C
   condition refers to it.  In practice this tends to lead to bigger
   object files.  */
static const bool pattern_c_test_p = false;

/* Whether to require each parameter passed to a pattern routine to be
   unique.  Disabling this check for example allows unary operators with
   matching modes (like NEG) and unary operators with mismatched modes
   (like ZERO_EXTEND) to be matched by a single pattern.  However, we then
   often have cases where the same value is passed too many times.  */
static const bool force_unique_params_p = true;

/* The maximum (approximate) depth of block nesting that an individual
   routine or subroutine should have.  This limit is about keeping the
   output readable rather than reducing compile time.  */
static const unsigned int MAX_DEPTH = 6;

/* The minimum number of pseudo-statements that a state must have before
   we split it out into a subroutine.  */
static const unsigned int MIN_NUM_STATEMENTS = 5;

/* The number of pseudo-statements a state can have before we consider
   splitting out substates into subroutines.  This limit is about avoiding
   compile-time problems with very big functions (and also about keeping
   functions within --param optimization limits, etc.).  */
static const unsigned int MAX_NUM_STATEMENTS = 200;

/* The minimum number of pseudo-statements that can be used in a pattern
   routine.  */
static const unsigned int MIN_COMBINE_COST = 4;

/* The maximum number of arguments that a pattern routine can have.
   The idea is to prevent one pattern getting a ridiculous number of
   arguments when it would be more beneficial to have a separate pattern
   routine instead.  */
static const unsigned int MAX_PATTERN_PARAMS = 5;

/* The maximum operand number plus one.  */
int num_operands;

/* Ways of obtaining an rtx to be tested.  */
enum position_type {
  /* PATTERN (peep2_next_insn (ARG)).  */
  POS_PEEP2_INSN,

  /* XEXP (BASE, ARG).  */
  POS_XEXP,

  /* XVECEXP (BASE, 0, ARG).  */
  POS_XVECEXP0
};

/* The position of an rtx relative to X0.  Each useful position is
   represented by exactly one instance of this structure.  */
struct position
{
  /* The parent rtx.  This is the root position for POS_PEEP2_INSNs.  */
  struct position *base;

  /* A position with the same BASE and TYPE, but with the next value
     of ARG.  */
  struct position *next;

  /* A list of all POS_XEXP positions that use this one as their base,
     chained by NEXT fields.  The first entry represents XEXP (this, 0),
     the second represents XEXP (this, 1), and so on.  */
  struct position *xexps;

  /* A list of POS_XVECEXP0 positions that use this one as their base,
     chained by NEXT fields.  The first entry represents XVECEXP (this, 0, 0),
     the second represents XVECEXP (this, 0, 1), and so on.  */
  struct position *xvecexp0s;

  /* The type of position.  */
  enum position_type type;

  /* The argument to TYPE (shown as ARG in the position_type comments).  */
  int arg;

  /* The instruction to which the position belongs.  */
  unsigned int insn_id;

  /* The depth of this position relative to the instruction pattern.
     E.g. if the instruction pattern is a SET, the SET itself has a
     depth of 0 while the SET_DEST and SET_SRC have depths of 1.  */
  unsigned int depth;

  /* A unique identifier for this position.  */
  unsigned int id;
};

enum routine_type {
  SUBPATTERN, RECOG, SPLIT, PEEPHOLE2
};

/* The root position (x0).  */
static struct position root_pos;

/* The number of positions created.  Also one higher than the maximum
   position id.  */
static unsigned int num_positions = 1;

/* A list of all POS_PEEP2_INSNs.  The entry for insn 0 is the root position,
   since we are given that instruction's pattern as x0.  */
static struct position *peep2_insn_pos_list = &root_pos;

/* Return a position with the given BASE, TYPE and ARG.  NEXT_PTR
   points to where the unique object that represents the position
   should be stored.  Create the object if it doesn't already exist,
   otherwise reuse the object that is already there.  */

static struct position *
next_position (struct position **next_ptr, struct position *base,
	       enum position_type type, int arg)
{
  struct position *pos;

  pos = *next_ptr;
  if (!pos)
    {
      pos = XCNEW (struct position);
      pos->type = type;
      pos->arg = arg;
      if (type == POS_PEEP2_INSN)
	{
	  pos->base = 0;
	  pos->insn_id = arg;
	  pos->depth = base->depth;
	}
      else
	{
	  pos->base = base;
	  pos->insn_id = base->insn_id;
	  pos->depth = base->depth + 1;
	}
      pos->id = num_positions++;
      *next_ptr = pos;
    }
  return pos;
}

/* Compare positions POS1 and POS2 lexicographically.  */

static int
compare_positions (struct position *pos1, struct position *pos2)
{
  int diff;

  diff = pos1->depth - pos2->depth;
  if (diff < 0)
    do
      pos2 = pos2->base;
    while (pos1->depth != pos2->depth);
  else if (diff > 0)
    do
      pos1 = pos1->base;
    while (pos1->depth != pos2->depth);
  while (pos1 != pos2)
    {
      diff = (int) pos1->type - (int) pos2->type;
      if (diff == 0)
	diff = pos1->arg - pos2->arg;
      pos1 = pos1->base;
      pos2 = pos2->base;
    }
  return diff;
}

/* Return the most deeply-nested position that is common to both
   POS1 and POS2.  If the positions are from different instructions,
   return the one with the lowest insn_id.  */

static struct position *
common_position (struct position *pos1, struct position *pos2)
{
  if (pos1->insn_id != pos2->insn_id)
    return pos1->insn_id < pos2->insn_id ? pos1 : pos2;
  if (pos1->depth > pos2->depth)
    std::swap (pos1, pos2);
  while (pos1->depth != pos2->depth)
    pos2 = pos2->base;
  while (pos1 != pos2)
    {
      pos1 = pos1->base;
      pos2 = pos2->base;
    }
  return pos1;
}

/* Search for and return operand N, stop when reaching node STOP.  */

static rtx
find_operand (rtx pattern, int n, rtx stop)
{
  const char *fmt;
  RTX_CODE code;
  int i, j, len;
  rtx r;

  if (pattern == stop)
    return stop;

  code = GET_CODE (pattern);
  if ((code == MATCH_SCRATCH
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
	  if ((r = find_operand (XEXP (pattern, i), n, stop)) != NULL_RTX)
	    return r;
	  break;

	case 'V':
	  if (! XVEC (pattern, i))
	    break;
	  /* Fall through.  */

	case 'E':
	  for (j = 0; j < XVECLEN (pattern, i); j++)
	    if ((r = find_operand (XVECEXP (pattern, i, j), n, stop))
		!= NULL_RTX)
	      return r;
	  break;

	case 'i': case 'r': case 'w': case '0': case 's':
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  return NULL;
}

/* Search for and return operand M, such that it has a matching
   constraint for operand N.  */

static rtx
find_matching_operand (rtx pattern, int n)
{
  const char *fmt;
  RTX_CODE code;
  int i, j, len;
  rtx r;

  code = GET_CODE (pattern);
  if (code == MATCH_OPERAND
      && (XSTR (pattern, 2)[0] == '0' + n
	  || (XSTR (pattern, 2)[0] == '%'
	      && XSTR (pattern, 2)[1] == '0' + n)))
    return pattern;

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      switch (fmt[i])
	{
	case 'e': case 'u':
	  if ((r = find_matching_operand (XEXP (pattern, i), n)))
	    return r;
	  break;

	case 'V':
	  if (! XVEC (pattern, i))
	    break;
	  /* Fall through.  */

	case 'E':
	  for (j = 0; j < XVECLEN (pattern, i); j++)
	    if ((r = find_matching_operand (XVECEXP (pattern, i, j), n)))
	      return r;
	  break;

	case 'i': case 'r': case 'w': case '0': case 's':
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  return NULL;
}

/* In DEFINE_EXPAND, DEFINE_SPLIT, and DEFINE_PEEPHOLE2, we
   don't use the MATCH_OPERAND constraint, only the predicate.
   This is confusing to folks doing new ports, so help them
   not make the mistake.  */

static bool
constraints_supported_in_insn_p (rtx insn)
{
  return !(GET_CODE (insn) == DEFINE_EXPAND
	   || GET_CODE (insn) == DEFINE_SPLIT
	   || GET_CODE (insn) == DEFINE_PEEPHOLE2);
}

/* Return the name of the predicate matched by MATCH_RTX.  */

static const char *
predicate_name (rtx match_rtx)
{
  if (GET_CODE (match_rtx) == MATCH_SCRATCH)
    return "scratch_operand";
  else
    return XSTR (match_rtx, 1);
}

/* Return true if OPERAND is a MATCH_OPERAND using a special predicate
   function.  */

static bool
special_predicate_operand_p (rtx operand)
{
  if (GET_CODE (operand) == MATCH_OPERAND)
    {
      const char *pred_name = predicate_name (operand);
      if (pred_name[0] != 0)
	{
	  const struct pred_data *pred;

	  pred = lookup_predicate (pred_name);
	  return pred != NULL && pred->special;
	}
    }

  return false;
}

/* Check for various errors in PATTERN, which is part of INFO.
   SET is nonnull for a destination, and is the complete set pattern.
   SET_CODE is '=' for normal sets, and '+' within a context that
   requires in-out constraints.  */

static void
validate_pattern (rtx pattern, md_rtx_info *info, rtx set, int set_code)
{
  const char *fmt;
  RTX_CODE code;
  size_t i, len;
  int j;

  code = GET_CODE (pattern);
  switch (code)
    {
    case MATCH_SCRATCH:
      {
	const char constraints0 = XSTR (pattern, 1)[0];

	if (!constraints_supported_in_insn_p (info->def))
	  {
	    if (constraints0)
	      {
		error_at (info->loc, "constraints not supported in %s",
			  GET_RTX_NAME (GET_CODE (info->def)));
	      }
	    return;
	  }

	/* If a MATCH_SCRATCH is used in a context requiring an write-only
	   or read/write register, validate that.  */
	if (set_code == '='
	    && constraints0
	    && constraints0 != '='
	    && constraints0 != '+')
	  {
	    error_at (info->loc, "operand %d missing output reload",
		      XINT (pattern, 0));
	  }
	return;
      }
    case MATCH_DUP:
    case MATCH_OP_DUP:
    case MATCH_PAR_DUP:
      if (find_operand (info->def, XINT (pattern, 0), pattern) == pattern)
	error_at (info->loc, "operand %i duplicated before defined",
		  XINT (pattern, 0));
      break;
    case MATCH_OPERAND:
    case MATCH_OPERATOR:
      {
	const char *pred_name = XSTR (pattern, 1);
	const struct pred_data *pred;
	const char *c_test;

	c_test = get_c_test (info->def);

	if (pred_name[0] != 0)
	  {
	    pred = lookup_predicate (pred_name);
	    if (!pred)
	      error_at (info->loc, "unknown predicate '%s'", pred_name);
	  }
	else
	  pred = 0;

	if (code == MATCH_OPERAND)
	  {
	    const char *constraints = XSTR (pattern, 2);
	    const char constraints0 = constraints[0];

	    if (!constraints_supported_in_insn_p (info->def))
	      {
		if (constraints0)
		  {
		    error_at (info->loc, "constraints not supported in %s",
			      GET_RTX_NAME (GET_CODE (info->def)));
		  }
	      }

	    /* A MATCH_OPERAND that is a SET should have an output reload.  */
	    else if (set && constraints0)
	      {
		if (set_code == '+')
		  {
		    if (constraints0 == '+')
		      ;
		    /* If we've only got an output reload for this operand,
		       we'd better have a matching input operand.  */
		    else if (constraints0 == '='
			     && find_matching_operand (info->def,
						       XINT (pattern, 0)))
		      ;
		    else
		      error_at (info->loc, "operand %d missing in-out reload",
				XINT (pattern, 0));
		  }
		else if (constraints0 != '=' && constraints0 != '+')
		  error_at (info->loc, "operand %d missing output reload",
			    XINT (pattern, 0));
	      }

	    /* For matching constraint in MATCH_OPERAND, the digit must be a
	       smaller number than the number of the operand that uses it in the
	       constraint.  */
	    while (1)
	      {
		while (constraints[0]
		       && (constraints[0] == ' ' || constraints[0] == ','))
		  constraints++;
		if (!constraints[0])
		  break;

		if (constraints[0] >= '0' && constraints[0] <= '9')
		  {
		    int val;

		    sscanf (constraints, "%d", &val);
		    if (val >= XINT (pattern, 0))
		      error_at (info->loc, "constraint digit %d is not"
				" smaller than operand %d",
				val, XINT (pattern, 0));
		  }

		while (constraints[0] && constraints[0] != ',')
		  constraints++;
	      }
	  }

	/* Allowing non-lvalues in destinations -- particularly CONST_INT --
	   while not likely to occur at runtime, results in less efficient
	   code from insn-recog.c.  */
	if (set && pred && pred->allows_non_lvalue)
	  error_at (info->loc, "destination operand %d allows non-lvalue",
		    XINT (pattern, 0));

	/* A modeless MATCH_OPERAND can be handy when we can check for
	   multiple modes in the c_test.  In most other cases, it is a
	   mistake.  Only DEFINE_INSN is eligible, since SPLIT and
	   PEEP2 can FAIL within the output pattern.  Exclude special
	   predicates, which check the mode themselves.  Also exclude
	   predicates that allow only constants.  Exclude the SET_DEST
	   of a call instruction, as that is a common idiom.  */

	if (GET_MODE (pattern) == VOIDmode
	    && code == MATCH_OPERAND
	    && GET_CODE (info->def) == DEFINE_INSN
	    && pred
	    && !pred->special
	    && pred->allows_non_const
	    && strstr (c_test, "operands") == NULL
	    && ! (set
		  && GET_CODE (set) == SET
		  && GET_CODE (SET_SRC (set)) == CALL))
	  message_at (info->loc, "warning: operand %d missing mode?",
		      XINT (pattern, 0));
	return;
      }

    case SET:
      {
	machine_mode dmode, smode;
	rtx dest, src;

	dest = SET_DEST (pattern);
	src = SET_SRC (pattern);

	/* STRICT_LOW_PART is a wrapper.  Its argument is the real
	   destination, and it's mode should match the source.  */
	if (GET_CODE (dest) == STRICT_LOW_PART)
	  dest = XEXP (dest, 0);

	/* Find the referent for a DUP.  */

	if (GET_CODE (dest) == MATCH_DUP
	    || GET_CODE (dest) == MATCH_OP_DUP
	    || GET_CODE (dest) == MATCH_PAR_DUP)
	  dest = find_operand (info->def, XINT (dest, 0), NULL);

	if (GET_CODE (src) == MATCH_DUP
	    || GET_CODE (src) == MATCH_OP_DUP
	    || GET_CODE (src) == MATCH_PAR_DUP)
	  src = find_operand (info->def, XINT (src, 0), NULL);

	dmode = GET_MODE (dest);
	smode = GET_MODE (src);

	/* Mode checking is not performed for special predicates.  */
	if (special_predicate_operand_p (src)
	    || special_predicate_operand_p (dest))
	  ;

        /* The operands of a SET must have the same mode unless one
	   is VOIDmode.  */
        else if (dmode != VOIDmode && smode != VOIDmode && dmode != smode)
	  error_at (info->loc, "mode mismatch in set: %smode vs %smode",
		    GET_MODE_NAME (dmode), GET_MODE_NAME (smode));

	/* If only one of the operands is VOIDmode, and PC or CC0 is
	   not involved, it's probably a mistake.  */
	else if (dmode != smode
		 && GET_CODE (dest) != PC
		 && GET_CODE (dest) != CC0
		 && GET_CODE (src) != PC
		 && GET_CODE (src) != CC0
		 && !CONST_INT_P (src)
		 && !CONST_WIDE_INT_P (src)
		 && GET_CODE (src) != CALL)
	  {
	    const char *which;
	    which = (dmode == VOIDmode ? "destination" : "source");
	    message_at (info->loc, "warning: %s missing a mode?", which);
	  }

	if (dest != SET_DEST (pattern))
	  validate_pattern (dest, info, pattern, '=');
	validate_pattern (SET_DEST (pattern), info, pattern, '=');
        validate_pattern (SET_SRC (pattern), info, NULL_RTX, 0);
        return;
      }

    case CLOBBER:
      validate_pattern (SET_DEST (pattern), info, pattern, '=');
      return;

    case ZERO_EXTRACT:
      validate_pattern (XEXP (pattern, 0), info, set, set ? '+' : 0);
      validate_pattern (XEXP (pattern, 1), info, NULL_RTX, 0);
      validate_pattern (XEXP (pattern, 2), info, NULL_RTX, 0);
      return;

    case STRICT_LOW_PART:
      validate_pattern (XEXP (pattern, 0), info, set, set ? '+' : 0);
      return;

    case LABEL_REF:
      if (GET_MODE (LABEL_REF_LABEL (pattern)) != VOIDmode)
	error_at (info->loc, "operand to label_ref %smode not VOIDmode",
		  GET_MODE_NAME (GET_MODE (LABEL_REF_LABEL (pattern))));
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
	  validate_pattern (XEXP (pattern, i), info, NULL_RTX, 0);
	  break;

	case 'E':
	  for (j = 0; j < XVECLEN (pattern, i); j++)
	    validate_pattern (XVECEXP (pattern, i, j), info, NULL_RTX, 0);
	  break;

	case 'i': case 'r': case 'w': case '0': case 's':
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

/* Simple list structure for items of type T, for use when being part
   of a list is an inherent property of T.  T must have members equivalent
   to "T *prev, *next;" and a function "void set_parent (list_head <T> *)"
   to set the parent list.  */
template <typename T>
struct list_head
{
  /* A range of linked items.  */
  struct range
  {
    range (T *);
    range (T *, T *);

    T *start, *end;
    void set_parent (list_head *);
  };

  list_head ();
  range release ();
  void push_back (range);
  range remove (range);
  void replace (range, range);
  T *singleton () const;

  T *first, *last;
};

/* Create a range [START_IN, START_IN].  */

template <typename T>
list_head <T>::range::range (T *start_in) : start (start_in), end (start_in) {}

/* Create a range [START_IN, END_IN], linked by next and prev fields.  */

template <typename T>
list_head <T>::range::range (T *start_in, T *end_in)
  : start (start_in), end (end_in) {}

template <typename T>
void
list_head <T>::range::set_parent (list_head <T> *owner)
{
  for (T *item = start; item != end; item = item->next)
    item->set_parent (owner);
  end->set_parent (owner);
}

template <typename T>
list_head <T>::list_head () : first (0), last (0) {}

/* Add R to the end of the list.  */

template <typename T>
void
list_head <T>::push_back (range r)
{
  if (last)
    last->next = r.start;
  else
    first = r.start;
  r.start->prev = last;
  last = r.end;
  r.set_parent (this);
}

/* Remove R from the list.  R remains valid and can be inserted into
   other lists.  */

template <typename T>
typename list_head <T>::range
list_head <T>::remove (range r)
{
  if (r.start->prev)
    r.start->prev->next = r.end->next;
  else
    first = r.end->next;
  if (r.end->next)
    r.end->next->prev = r.start->prev;
  else
    last = r.start->prev;
  r.start->prev = 0;
  r.end->next = 0;
  r.set_parent (0);
  return r;
}

/* Replace OLDR with NEWR.  OLDR remains valid and can be inserted into
   other lists.  */

template <typename T>
void
list_head <T>::replace (range oldr, range newr)
{
  newr.start->prev = oldr.start->prev;
  newr.end->next = oldr.end->next;

  oldr.start->prev = 0;
  oldr.end->next = 0;
  oldr.set_parent (0);

  if (newr.start->prev)
    newr.start->prev->next = newr.start;
  else
    first = newr.start;
  if (newr.end->next)
    newr.end->next->prev = newr.end;
  else
    last = newr.end;
  newr.set_parent (this);
}

/* Empty the list and return the previous contents as a range that can
   be inserted into other lists.  */

template <typename T>
typename list_head <T>::range
list_head <T>::release ()
{
  range r (first, last);
  first = 0;
  last = 0;
  r.set_parent (0);
  return r;
}

/* If the list contains a single item, return that item, otherwise return
   null.  */

template <typename T>
T *
list_head <T>::singleton () const
{
  return first == last ? first : 0;
}

struct state;

/* Describes a possible successful return from a routine.  */
struct acceptance_type
{
  /* The type of routine we're returning from.  */
  routine_type type : 16;

  /* True if this structure only really represents a partial match,
     and if we must call a subroutine of type TYPE to complete the match.
     In this case we'll call the subroutine and, if it succeeds, return
     whatever the subroutine returned.

     False if this structure presents a full match.  */
  unsigned int partial_p : 1;

  union
  {
    /* If PARTIAL_P, this is the number of the subroutine to call.  */
    int subroutine_id;

    /* Valid if !PARTIAL_P.  */
    struct
    {
      /* The identifier of the matching pattern.  For SUBPATTERNs this
	 value belongs to an ad-hoc routine-specific enum.  For the
	 others it's the number of an .md file pattern.  */
      int code;
      union
      {
	/* For RECOG, the number of clobbers that must be added to the
	   pattern in order for it to match CODE.  */
	int num_clobbers;

	/* For PEEPHOLE2, the number of additional instructions that were
	   included in the optimization.  */
	int match_len;
      } u;
    } full;
  } u;
};

bool
operator == (const acceptance_type &a, const acceptance_type &b)
{
  if (a.partial_p != b.partial_p)
    return false;
  if (a.partial_p)
    return a.u.subroutine_id == b.u.subroutine_id;
  else
    return a.u.full.code == b.u.full.code;
}

bool
operator != (const acceptance_type &a, const acceptance_type &b)
{
  return !operator == (a, b);
}

/* Represents a parameter to a pattern routine.  */
struct parameter
{
  /* The C type of parameter.  */
  enum type_enum {
    /* Represents an invalid parameter.  */
    UNSET,

    /* A machine_mode parameter.  */
    MODE,

    /* An rtx_code parameter.  */
    CODE,

    /* An int parameter.  */
    INT,

    /* An unsigned int parameter.  */
    UINT,

    /* A HOST_WIDE_INT parameter.  */
    WIDE_INT
  };

  parameter ();
  parameter (type_enum, bool, uint64_t);

  /* The type of the parameter.  */
  type_enum type;

  /* True if the value passed is variable, false if it is constant.  */
  bool is_param;

  /* If IS_PARAM, this is the number of the variable passed, for an "i%d"
     format string.  If !IS_PARAM, this is the constant value passed.  */
  uint64_t value;
};

parameter::parameter ()
  : type (UNSET), is_param (false), value (0) {}

parameter::parameter (type_enum type_in, bool is_param_in, uint64_t value_in)
  : type (type_in), is_param (is_param_in), value (value_in) {}

bool
operator == (const parameter &param1, const parameter &param2)
{
  return (param1.type == param2.type
	  && param1.is_param == param2.is_param
	  && param1.value == param2.value);
}

bool
operator != (const parameter &param1, const parameter &param2)
{
  return !operator == (param1, param2);
}

/* Represents a routine that matches a partial rtx pattern, returning
   an ad-hoc enum value on success and -1 on failure.  The routine can
   be used by any subroutine type.  The match can be parameterized by
   things like mode, code and UNSPEC number.  */
struct pattern_routine
{
  /* The state that implements the pattern.  */
  state *s;

  /* The deepest root position from which S can access all the rtxes it needs.
     This is NULL if the pattern doesn't need an rtx input, usually because
     all matching is done on operands[] instead.  */
  position *pos;

  /* A unique identifier for the routine.  */
  unsigned int pattern_id;

  /* True if the routine takes pnum_clobbers as argument.  */
  bool pnum_clobbers_p;

  /* True if the routine takes the enclosing instruction as argument.  */
  bool insn_p;

  /* The types of the other parameters to the routine, if any.  */
  auto_vec <parameter::type_enum, MAX_PATTERN_PARAMS> param_types;
};

/* All defined patterns.  */
static vec <pattern_routine *> patterns;

/* Represents one use of a pattern routine.  */
struct pattern_use
{
  /* The pattern routine to use.  */
  pattern_routine *routine;

  /* The values to pass as parameters.  This vector has the same length
     as ROUTINE->PARAM_TYPES.  */
  auto_vec <parameter, MAX_PATTERN_PARAMS> params;
};

/* Represents a test performed by a decision.  */
struct rtx_test
{
  rtx_test ();

  /* The types of test that can be performed.  Most of them take as input
     an rtx X.  Some also take as input a transition label LABEL; the others
     are booleans for which the transition label is always "true".

     The order of the enum isn't important.  */
  enum kind_enum {
    /* Check GET_CODE (X) == LABEL.  */
    CODE,

    /* Check GET_MODE (X) == LABEL.  */
    MODE,

    /* Check REGNO (X) == LABEL.  */
    REGNO_FIELD,

    /* Check XINT (X, u.opno) == LABEL.  */
    INT_FIELD,

    /* Check XWINT (X, u.opno) == LABEL.  */
    WIDE_INT_FIELD,

    /* Check XVECLEN (X, 0) == LABEL.  */
    VECLEN,

    /* Check peep2_current_count >= u.min_len.  */
    PEEP2_COUNT,

    /* Check XVECLEN (X, 0) >= u.min_len.  */
    VECLEN_GE,

    /* Check whether X is a cached const_int with value u.integer.  */
    SAVED_CONST_INT,

    /* Check u.predicate.data (X, u.predicate.mode).  */
    PREDICATE,

    /* Check rtx_equal_p (X, operands[u.opno]).  */
    DUPLICATE,

    /* Check whether X matches pattern u.pattern.  */
    PATTERN,

    /* Check whether pnum_clobbers is nonnull (RECOG only).  */
    HAVE_NUM_CLOBBERS,

    /* Check whether general C test u.string holds.  In general the condition
       needs access to "insn" and the full operand list.  */
    C_TEST,

    /* Execute operands[u.opno] = X.  (Always succeeds.)  */
    SET_OP,

    /* Accept u.acceptance.  Always succeeds for SUBPATTERN, RECOG and SPLIT.
       May fail for PEEPHOLE2 if the define_peephole2 C code executes FAIL.  */
    ACCEPT
  };

  /* The position of rtx X in the above description, relative to the
     incoming instruction "insn".  The position is null if the test
     doesn't take an X as input.  */
  position *pos;

  /* Which element of operands[] already contains POS, or -1 if no element
     is known to hold POS.  */
  int pos_operand;

  /* The type of test and its parameters, as described above.  */
  kind_enum kind;
  union
  {
    int opno;
    int min_len;
    struct
    {
      bool is_param;
      int value;
    } integer;
    struct
    {
      const struct pred_data *data;
      /* True if the mode is taken from a machine_mode parameter
	 to the routine rather than a constant machine_mode.  If true,
	 MODE is the number of the parameter (for an "i%d" format string),
	 otherwise it is the mode itself.  */
      bool mode_is_param;
      unsigned int mode;
    } predicate;
    pattern_use *pattern;
    const char *string;
    acceptance_type acceptance;
  } u;

  static rtx_test code (position *);
  static rtx_test mode (position *);
  static rtx_test regno_field (position *);
  static rtx_test int_field (position *, int);
  static rtx_test wide_int_field (position *, int);
  static rtx_test veclen (position *);
  static rtx_test peep2_count (int);
  static rtx_test veclen_ge (position *, int);
  static rtx_test predicate (position *, const pred_data *, machine_mode);
  static rtx_test duplicate (position *, int);
  static rtx_test pattern (position *, pattern_use *);
  static rtx_test have_num_clobbers ();
  static rtx_test c_test (const char *);
  static rtx_test set_op (position *, int);
  static rtx_test accept (const acceptance_type &);

  bool terminal_p () const;
  bool single_outcome_p () const;

private:
  rtx_test (position *, kind_enum);
};

rtx_test::rtx_test () {}

rtx_test::rtx_test (position *pos_in, kind_enum kind_in)
  : pos (pos_in), pos_operand (-1), kind (kind_in) {}

rtx_test
rtx_test::code (position *pos)
{
  return rtx_test (pos, rtx_test::CODE);
}

rtx_test
rtx_test::mode (position *pos)
{
  return rtx_test (pos, rtx_test::MODE);
}

rtx_test
rtx_test::regno_field (position *pos)
{
  rtx_test res (pos, rtx_test::REGNO_FIELD);
  return res;
}

rtx_test
rtx_test::int_field (position *pos, int opno)
{
  rtx_test res (pos, rtx_test::INT_FIELD);
  res.u.opno = opno;
  return res;
}

rtx_test
rtx_test::wide_int_field (position *pos, int opno)
{
  rtx_test res (pos, rtx_test::WIDE_INT_FIELD);
  res.u.opno = opno;
  return res;
}

rtx_test
rtx_test::veclen (position *pos)
{
  return rtx_test (pos, rtx_test::VECLEN);
}

rtx_test
rtx_test::peep2_count (int min_len)
{
  rtx_test res (0, rtx_test::PEEP2_COUNT);
  res.u.min_len = min_len;
  return res;
}

rtx_test
rtx_test::veclen_ge (position *pos, int min_len)
{
  rtx_test res (pos, rtx_test::VECLEN_GE);
  res.u.min_len = min_len;
  return res;
}

rtx_test
rtx_test::predicate (position *pos, const struct pred_data *data,
		     machine_mode mode)
{
  rtx_test res (pos, rtx_test::PREDICATE);
  res.u.predicate.data = data;
  res.u.predicate.mode_is_param = false;
  res.u.predicate.mode = mode;
  return res;
}

rtx_test
rtx_test::duplicate (position *pos, int opno)
{
  rtx_test res (pos, rtx_test::DUPLICATE);
  res.u.opno = opno;
  return res;
}

rtx_test
rtx_test::pattern (position *pos, pattern_use *pattern)
{
  rtx_test res (pos, rtx_test::PATTERN);
  res.u.pattern = pattern;
  return res;
}

rtx_test
rtx_test::have_num_clobbers ()
{
  return rtx_test (0, rtx_test::HAVE_NUM_CLOBBERS);
}

rtx_test
rtx_test::c_test (const char *string)
{
  rtx_test res (0, rtx_test::C_TEST);
  res.u.string = string;
  return res;
}

rtx_test
rtx_test::set_op (position *pos, int opno)
{
  rtx_test res (pos, rtx_test::SET_OP);
  res.u.opno = opno;
  return res;
}

rtx_test
rtx_test::accept (const acceptance_type &acceptance)
{
  rtx_test res (0, rtx_test::ACCEPT);
  res.u.acceptance = acceptance;
  return res;
}

/* Return true if the test represents an unconditionally successful match.  */

bool
rtx_test::terminal_p () const
{
  return kind == rtx_test::ACCEPT && u.acceptance.type != PEEPHOLE2;
}

/* Return true if the test is a boolean that is always true.  */

bool
rtx_test::single_outcome_p () const
{
  return terminal_p () || kind == rtx_test::SET_OP;
}

bool
operator == (const rtx_test &a, const rtx_test &b)
{
  if (a.pos != b.pos || a.kind != b.kind)
    return false;
  switch (a.kind)
    {
    case rtx_test::CODE:
    case rtx_test::MODE:
    case rtx_test::REGNO_FIELD:
    case rtx_test::VECLEN:
    case rtx_test::HAVE_NUM_CLOBBERS:
      return true;

    case rtx_test::PEEP2_COUNT:
    case rtx_test::VECLEN_GE:
      return a.u.min_len == b.u.min_len;

    case rtx_test::INT_FIELD:
    case rtx_test::WIDE_INT_FIELD:
    case rtx_test::DUPLICATE:
    case rtx_test::SET_OP:
      return a.u.opno == b.u.opno;

    case rtx_test::SAVED_CONST_INT:
      return (a.u.integer.is_param == b.u.integer.is_param
	      && a.u.integer.value == b.u.integer.value);

    case rtx_test::PREDICATE:
      return (a.u.predicate.data == b.u.predicate.data
	      && a.u.predicate.mode_is_param == b.u.predicate.mode_is_param
	      && a.u.predicate.mode == b.u.predicate.mode);

    case rtx_test::PATTERN:
      return (a.u.pattern->routine == b.u.pattern->routine
	      && a.u.pattern->params == b.u.pattern->params);

    case rtx_test::C_TEST:
      return strcmp (a.u.string, b.u.string) == 0;

    case rtx_test::ACCEPT:
      return a.u.acceptance == b.u.acceptance;
    }
  gcc_unreachable ();
}

bool
operator != (const rtx_test &a, const rtx_test &b)
{
  return !operator == (a, b);
}

/* A simple set of transition labels.  Most transitions have a singleton
   label, so try to make that case as efficient as possible.  */
struct int_set : public auto_vec <uint64_t, 1>
{
  typedef uint64_t *iterator;

  int_set ();
  int_set (uint64_t);
  int_set (const int_set &);

  int_set &operator = (const int_set &);

  iterator begin ();
  iterator end ();
};

int_set::int_set () {}

int_set::int_set (uint64_t label)
{
  safe_push (label);
}

int_set::int_set (const int_set &other)
{
  safe_splice (other);
}

int_set &
int_set::operator = (const int_set &other)
{
  truncate (0);
  safe_splice (other);
  return *this;
}

int_set::iterator
int_set::begin ()
{
  return address ();
}

int_set::iterator
int_set::end ()
{
  return address () + length ();
}

bool
operator == (const int_set &a, const int_set &b)
{
  if (a.length () != b.length ())
    return false;
  for (unsigned int i = 0; i < a.length (); ++i)
    if (a[i] != b[i])
      return false;
  return true;
}

bool
operator != (const int_set &a, const int_set &b)
{
  return !operator == (a, b);
}

struct decision;

/* Represents a transition between states, dependent on the result of
   a test T.  */
struct transition
{
  transition (const int_set &, state *, bool);

  void set_parent (list_head <transition> *);

  /* Links to other transitions for T.  Always null for boolean tests.  */
  transition *prev, *next;

  /* The transition should be taken when T has one of these values.
     E.g. for rtx_test::CODE this is a set of codes, while for booleans like
     rtx_test::PREDICATE it is always a singleton "true".  The labels are
     sorted in ascending order.  */
  int_set labels;

  /* The source decision.  */
  decision *from;

  /* The target state.  */
  state *to;

  /* True if TO would function correctly even if TEST wasn't performed.
     E.g. it isn't necessary to check whether GET_MODE (x1) is SImode
     before calling register_operand (x1, SImode), since register_operand
     performs its own mode check.  However, checking GET_MODE can be a cheap
     way of disambiguating SImode and DImode register operands.  */
  bool optional;

  /* True if LABELS contains parameter numbers rather than constants.
     E.g. if this is true for a rtx_test::CODE, the label is the number
     of an rtx_code parameter rather than an rtx_code itself.
     LABELS is always a singleton when this variable is true.  */
  bool is_param;
};

/* Represents a test and the action that should be taken on the result.
   If a transition exists for the test outcome, the machine switches
   to the transition's target state.  If no suitable transition exists,
   the machine either falls through to the next decision or, if there are no
   more decisions to try, fails the match.  */
struct decision : list_head <transition>
{
  decision (const rtx_test &);

  void set_parent (list_head <decision> *s);
  bool if_statement_p (uint64_t * = 0) const;

  /* The state to which this decision belongs.  */
  state *s;

  /* Links to other decisions in the same state.  */
  decision *prev, *next;

  /* The test to perform.  */
  rtx_test test;
};

/* Represents one machine state.  For each state the machine tries a list
   of decisions, in order, and acts on the first match.  It fails without
   further backtracking if no decisions match.  */
struct state : list_head <decision>
{
  void set_parent (list_head <state> *) {}
};

transition::transition (const int_set &labels_in, state *to_in,
			bool optional_in)
  : prev (0), next (0), labels (labels_in), from (0), to (to_in),
    optional (optional_in), is_param (false) {}

/* Set the source decision of the transition.  */

void
transition::set_parent (list_head <transition> *from_in)
{
  from = static_cast <decision *> (from_in);
}

decision::decision (const rtx_test &test_in)
  : prev (0), next (0), test (test_in) {}

/* Set the state to which this decision belongs.  */

void
decision::set_parent (list_head <decision> *s_in)
{
  s = static_cast <state *> (s_in);
}

/* Return true if the decision has a single transition with a single label.
   If so, return the label in *LABEL if nonnull.  */

inline bool
decision::if_statement_p (uint64_t *label) const
{
  if (singleton () && first->labels.length () == 1)
    {
      if (label)
	*label = first->labels[0];
      return true;
    }
  return false;
}

/* Add to FROM a decision that performs TEST and has a single transition
   TRANS.  */

static void
add_decision (state *from, const rtx_test &test, transition *trans)
{
  decision *d = new decision (test);
  from->push_back (d);
  d->push_back (trans);
}

/* Add a transition from FROM to a new, empty state that is taken
   when TEST == LABELS.  OPTIONAL says whether the new transition
   should be optional.  Return the new state.  */

static state *
add_decision (state *from, const rtx_test &test, int_set labels, bool optional)
{
  state *to = new state;
  add_decision (from, test, new transition (labels, to, optional));
  return to;
}

/* Insert a decision before decisions R to make them dependent on
   TEST == LABELS.  OPTIONAL says whether the new transition should be
   optional.  */

static decision *
insert_decision_before (state::range r, const rtx_test &test,
			const int_set &labels, bool optional)
{
  decision *newd = new decision (test);
  state *news = new state;
  newd->push_back (new transition (labels, news, optional));
  r.start->s->replace (r, newd);
  news->push_back (r);
  return newd;
}

/* Remove any optional transitions from S that turned out not to be useful.  */

static void
collapse_optional_decisions (state *s)
{
  decision *d = s->first;
  while (d)
    {
      decision *next = d->next;
      for (transition *trans = d->first; trans; trans = trans->next)
	collapse_optional_decisions (trans->to);
      /* A decision with a single optional transition doesn't help
	 partition the potential matches and so is unlikely to be
	 worthwhile.  In particular, if the decision that performs the
	 test is the last in the state, the best it could do is reject
	 an invalid pattern slightly earlier.  If instead the decision
	 is not the last in the state, the condition it tests could hold
	 even for the later decisions in the state.  The best it can do
	 is save work in some cases where only the later decisions can
	 succeed.

	 In both cases the optional transition would add extra work to
	 successful matches when the tested condition holds.  */
      if (transition *trans = d->singleton ())
	if (trans->optional)
	  s->replace (d, trans->to->release ());
      d = next;
    }
}

/* Try to squash several separate tests into simpler ones.  */

static void
simplify_tests (state *s)
{
  for (decision *d = s->first; d; d = d->next)
    {
      uint64_t label;
      /* Convert checks for GET_CODE (x) == CONST_INT and XWINT (x, 0) == N
	 into checks for const_int_rtx[N'], if N is suitably small.  */
      if (d->test.kind == rtx_test::CODE
	  && d->if_statement_p (&label)
	  && label == CONST_INT)
	if (decision *second = d->first->to->singleton ())
	  if (d->test.pos == second->test.pos
	      && second->test.kind == rtx_test::WIDE_INT_FIELD
	      && second->test.u.opno == 0
	      && second->if_statement_p (&label)
	      && IN_RANGE (int64_t (label),
			   -MAX_SAVED_CONST_INT, MAX_SAVED_CONST_INT))
	    {
	      d->test.kind = rtx_test::SAVED_CONST_INT;
	      d->test.u.integer.is_param = false;
	      d->test.u.integer.value = label;
	      d->replace (d->first, second->release ());
	      d->first->labels[0] = true;
	    }
      /* If we have a CODE test followed by a PREDICATE test, rely on
	 the predicate to test the code.

	 This case exists for match_operators.  We initially treat the
	 CODE test for a match_operator as non-optional so that we can
	 safely move down to its operands.  It may turn out that all
	 paths that reach that code test require the same predicate
	 to be true.  cse_tests will then put the predicate test in
	 series with the code test.  */
      if (d->test.kind == rtx_test::CODE)
	if (transition *trans = d->singleton ())
	  {
	    state *s = trans->to;
	    while (decision *d2 = s->singleton ())
	      {
		if (d->test.pos != d2->test.pos)
		  break;
		transition *trans2 = d2->singleton ();
		if (!trans2)
		  break;
		if (d2->test.kind == rtx_test::PREDICATE)
		  {
		    d->test = d2->test;
		    trans->labels = int_set (true);
		    s->replace (d2, trans2->to->release ());
		    break;
		  }
		s = trans2->to;
	      }
	  }
      for (transition *trans = d->first; trans; trans = trans->next)
	simplify_tests (trans->to);
    }
}

/* Return true if all successful returns passing through D require the
   condition tested by COMMON to be true.

   When returning true, add all transitions like COMMON in D to WHERE.
   WHERE may contain a partial result on failure.  */

static bool
common_test_p (decision *d, transition *common, vec <transition *> *where)
{
  if (d->test.kind == rtx_test::ACCEPT)
    /* We found a successful return that didn't require COMMON.  */
    return false;
  if (d->test == common->from->test)
    {
      transition *trans = d->singleton ();
      if (!trans
	  || trans->optional != common->optional
	  || trans->labels != common->labels)
	return false;
      where->safe_push (trans);
      return true;
    }
  for (transition *trans = d->first; trans; trans = trans->next)
    for (decision *subd = trans->to->first; subd; subd = subd->next)
      if (!common_test_p (subd, common, where))
	return false;
  return true;
}

/* Indicates that we have tested GET_CODE (X) for a particular rtx X.  */
const unsigned char TESTED_CODE = 1;

/* Indicates that we have tested XVECLEN (X, 0) for a particular rtx X.  */
const unsigned char TESTED_VECLEN = 2;

/* Represents a set of conditions that are known to hold.  */
struct known_conditions
{
  /* A mask of TESTED_ values for each position, indexed by the position's
     id field.  */
  auto_vec <unsigned char> position_tests;

  /* Index N says whether operands[N] has been set.  */
  auto_vec <bool> set_operands;

  /* A guranteed lower bound on the value of peep2_current_count.  */
  int peep2_count;
};

/* Return true if TEST can safely be performed at D, where
   the conditions in KC hold.  TEST is known to occur along the
   first path from D (i.e. always following the first transition
   of the first decision).  Any intervening tests can be used as
   negative proof that hoisting isn't safe, but only KC can be used
   as positive proof.  */

static bool
safe_to_hoist_p (decision *d, const rtx_test &test, known_conditions *kc)
{
  switch (test.kind)
    {
    case rtx_test::C_TEST:
      /* In general, C tests require everything else to have been
	 verified and all operands to have been set up.  */
      return false;

    case rtx_test::ACCEPT:
      /* Don't accept something before all conditions have been tested.  */
      return false;

    case rtx_test::PREDICATE:
      /* Don't move a predicate over a test for VECLEN_GE, since the
	 predicate used in a match_parallel can legitimately expect the
	 length to be checked first.  */
      for (decision *subd = d;
	   subd->test != test;
	   subd = subd->first->to->first)
	if (subd->test.pos == test.pos
	    && subd->test.kind == rtx_test::VECLEN_GE)
	  return false;
      goto any_rtx;

    case rtx_test::DUPLICATE:
      /* Don't test for a match_dup until the associated operand has
	 been set.  */
      if (!kc->set_operands[test.u.opno])
	return false;
      goto any_rtx;

    case rtx_test::CODE:
    case rtx_test::MODE:
    case rtx_test::SAVED_CONST_INT:
    case rtx_test::SET_OP:
    any_rtx:
      /* Check whether it is safe to access the rtx under test.  */
      switch (test.pos->type)
	{
	case POS_PEEP2_INSN:
	  return test.pos->arg < kc->peep2_count;

	case POS_XEXP:
	  return kc->position_tests[test.pos->base->id] & TESTED_CODE;

	case POS_XVECEXP0:
	  return kc->position_tests[test.pos->base->id] & TESTED_VECLEN;
	}
      gcc_unreachable ();

    case rtx_test::REGNO_FIELD:
    case rtx_test::INT_FIELD:
    case rtx_test::WIDE_INT_FIELD:
    case rtx_test::VECLEN:
    case rtx_test::VECLEN_GE:
      /* These tests access a specific part of an rtx, so are only safe
	 once we know what the rtx is.  */
      return kc->position_tests[test.pos->id] & TESTED_CODE;

    case rtx_test::PEEP2_COUNT:
    case rtx_test::HAVE_NUM_CLOBBERS:
      /* These tests can be performed anywhere.  */
      return true;

    case rtx_test::PATTERN:
      gcc_unreachable ();
    }
  gcc_unreachable ();
}

/* Look for a transition that is taken by all successful returns from a range
   of decisions starting at OUTER and that would be better performed by
   OUTER's state instead.  On success, store all instances of that transition
   in WHERE and return the last decision in the range.  The range could
   just be OUTER, or it could include later decisions as well.

   WITH_POSITION_P is true if only tests with position POS should be tried,
   false if any test should be tried.  WORTHWHILE_SINGLE_P is true if the
   result is useful even when the range contains just a single decision
   with a single transition.  KC are the conditions that are known to
   hold at OUTER.  */

static decision *
find_common_test (decision *outer, bool with_position_p,
		  position *pos, bool worthwhile_single_p,
		  known_conditions *kc, vec <transition *> *where)
{
  /* After this, WORTHWHILE_SINGLE_P indicates whether a range that contains
     just a single decision is useful, regardless of the number of
     transitions it has.  */
  if (!outer->singleton ())
    worthwhile_single_p = true;
  /* Quick exit if we don't have enough decisions to form a worthwhile
     range.  */
  if (!worthwhile_single_p && !outer->next)
    return 0;
  /* Follow the first chain down, as one example of a path that needs
     to contain the common test.  */
  for (decision *d = outer; d; d = d->first->to->first)
    {
      transition *trans = d->singleton ();
      if (trans
	  && (!with_position_p || d->test.pos == pos)
	  && safe_to_hoist_p (outer, d->test, kc))
	{
	  if (common_test_p (outer, trans, where))
	    {
	      if (!outer->next)
		/* We checked above whether the move is worthwhile.  */
		return outer;
	      /* See how many decisions in OUTER's chain could reuse
		 the same test.  */
	      decision *outer_end = outer;
	      do
		{
		  unsigned int length = where->length ();
		  if (!common_test_p (outer_end->next, trans, where))
		    {
		      where->truncate (length);
		      break;
		    }
		  outer_end = outer_end->next;
		}
	      while (outer_end->next);
	      /* It is worth moving TRANS if it can be shared by more than
		 one decision.  */
	      if (outer_end != outer || worthwhile_single_p)
		return outer_end;
	    }
	  where->truncate (0);
	}
    }
  return 0;
}

/* Try to promote common subtests in S to a single, shared decision.
   Also try to bunch tests for the same position together.  POS is the
   position of the rtx tested before reaching S.  KC are the conditions
   that are known to hold on entry to S.  */

static void
cse_tests (position *pos, state *s, known_conditions *kc)
{
  for (decision *d = s->first; d; d = d->next)
    {
      auto_vec <transition *, 16> where;
      if (d->test.pos)
	{
	  /* Try to find conditions that don't depend on a particular rtx,
	     such as pnum_clobbers != NULL or peep2_current_count >= X.
	     It's usually better to check these conditions as soon as
	     possible, so the change is worthwhile even if there is
	     only one copy of the test.  */
	  decision *endd = find_common_test (d, true, 0, true, kc, &where);
	  if (!endd && d->test.pos != pos)
	    /* Try to find other conditions related to position POS
	       before moving to the new position.  Again, this is
	       worthwhile even if there is only one copy of the test,
	       since it means that fewer position variables are live
	       at a given time.  */
	    endd = find_common_test (d, true, pos, true, kc, &where);
	  if (!endd)
	    /* Try to find any condition that is used more than once.  */
	    endd = find_common_test (d, false, 0, false, kc, &where);
	  if (endd)
	    {
	      transition *common = where[0];
	      /* Replace [D, ENDD] with a test like COMMON.  We'll recurse
		 on the common test and see the original D again next time.  */
	      d = insert_decision_before (state::range (d, endd),
					  common->from->test,
					  common->labels,
					  common->optional);
	      /* Remove the old tests.  */
	      while (!where.is_empty ())
		{
		  transition *trans = where.pop ();
		  trans->from->s->replace (trans->from, trans->to->release ());
		}
	    }
	}

      /* Make sure that safe_to_hoist_p isn't being overly conservative.
	 It should realize that D's test is safe in the current
	 environment.  */
      gcc_assert (d->test.kind == rtx_test::C_TEST
		  || d->test.kind == rtx_test::ACCEPT
		  || safe_to_hoist_p (d, d->test, kc));

      /* D won't be changed any further by the current optimization.
	 Recurse with the state temporarily updated to include D.  */
      int prev = 0;
      switch (d->test.kind)
	{
	case rtx_test::CODE:
	  prev = kc->position_tests[d->test.pos->id];
	  kc->position_tests[d->test.pos->id] |= TESTED_CODE;
	  break;

	case rtx_test::VECLEN:
	case rtx_test::VECLEN_GE:
	  prev = kc->position_tests[d->test.pos->id];
	  kc->position_tests[d->test.pos->id] |= TESTED_VECLEN;
	  break;

	case rtx_test::SET_OP:
	  prev = kc->set_operands[d->test.u.opno];
	  gcc_assert (!prev);
	  kc->set_operands[d->test.u.opno] = true;
	  break;

	case rtx_test::PEEP2_COUNT:
	  prev = kc->peep2_count;
	  kc->peep2_count = MAX (prev, d->test.u.min_len);
	  break;

	default:
	  break;
	}
      for (transition *trans = d->first; trans; trans = trans->next)
	cse_tests (d->test.pos ? d->test.pos : pos, trans->to, kc);
      switch (d->test.kind)
	{
	case rtx_test::CODE:
	case rtx_test::VECLEN:
	case rtx_test::VECLEN_GE:
	  kc->position_tests[d->test.pos->id] = prev;
	  break;

	case rtx_test::SET_OP:
	  kc->set_operands[d->test.u.opno] = prev;
	  break;

	case rtx_test::PEEP2_COUNT:
	  kc->peep2_count = prev;
	  break;

	default:
	  break;
	}
    }
}

/* Return the type of value that can be used to parameterize test KIND,
   or parameter::UNSET if none.  */

parameter::type_enum
transition_parameter_type (rtx_test::kind_enum kind)
{
  switch (kind)
    {
    case rtx_test::CODE:
      return parameter::CODE;

    case rtx_test::MODE:
      return parameter::MODE;

    case rtx_test::REGNO_FIELD:
      return parameter::UINT;

    case rtx_test::INT_FIELD:
    case rtx_test::VECLEN:
    case rtx_test::PATTERN:
      return parameter::INT;

    case rtx_test::WIDE_INT_FIELD:
      return parameter::WIDE_INT;

    case rtx_test::PEEP2_COUNT:
    case rtx_test::VECLEN_GE:
    case rtx_test::SAVED_CONST_INT:
    case rtx_test::PREDICATE:
    case rtx_test::DUPLICATE:
    case rtx_test::HAVE_NUM_CLOBBERS:
    case rtx_test::C_TEST:
    case rtx_test::SET_OP:
    case rtx_test::ACCEPT:
      return parameter::UNSET;
    }
  gcc_unreachable ();
}

/* Initialize the pos_operand fields of each state reachable from S.
   If OPERAND_POS[ID] >= 0, the position with id ID is stored in
   operands[OPERAND_POS[ID]] on entry to S.  */

static void
find_operand_positions (state *s, vec <int> &operand_pos)
{
  for (decision *d = s->first; d; d = d->next)
    {
      int this_operand = (d->test.pos ? operand_pos[d->test.pos->id] : -1);
      if (this_operand >= 0)
	d->test.pos_operand = this_operand;
      if (d->test.kind == rtx_test::SET_OP)
	operand_pos[d->test.pos->id] = d->test.u.opno;
      for (transition *trans = d->first; trans; trans = trans->next)
	find_operand_positions (trans->to, operand_pos);
      if (d->test.kind == rtx_test::SET_OP)
	operand_pos[d->test.pos->id] = this_operand;
    }
}

/* Statistics about a matching routine.  */
struct stats
{
  stats ();

  /* The total number of decisions in the routine, excluding trivial
     ones that never fail.  */
  unsigned int num_decisions;

  /* The number of non-trivial decisions on the longest path through
     the routine, and the return value that contributes most to that
     long path.  */
  unsigned int longest_path;
  int longest_path_code;

  /* The maximum number of times that a single call to the routine
     can backtrack, and the value returned at the end of that path.
     "Backtracking" here means failing one decision in state and
     going onto to the next.  */
  unsigned int longest_backtrack;
  int longest_backtrack_code;
};

stats::stats ()
  : num_decisions (0), longest_path (0), longest_path_code (-1),
    longest_backtrack (0), longest_backtrack_code (-1) {}

/* Return statistics about S.  */

static stats
get_stats (state *s)
{
  stats for_s;
  unsigned int longest_path = 0;
  for (decision *d = s->first; d; d = d->next)
    {
      /* Work out the statistics for D.  */
      stats for_d;
      for (transition *trans = d->first; trans; trans = trans->next)
	{
	  stats for_trans = get_stats (trans->to);
	  for_d.num_decisions += for_trans.num_decisions;
	  /* Each transition is mutually-exclusive, so just pick the
	     longest of the individual paths.  */
	  if (for_d.longest_path <= for_trans.longest_path)
	    {
	      for_d.longest_path = for_trans.longest_path;
	      for_d.longest_path_code = for_trans.longest_path_code;
	    }
	  /* Likewise for backtracking.  */
	  if (for_d.longest_backtrack <= for_trans.longest_backtrack)
	    {
	      for_d.longest_backtrack = for_trans.longest_backtrack;
	      for_d.longest_backtrack_code = for_trans.longest_backtrack_code;
	    }
	}

      /* Account for D's test in its statistics.  */
      if (!d->test.single_outcome_p ())
	{
	  for_d.num_decisions += 1;
	  for_d.longest_path += 1;
	}
      if (d->test.kind == rtx_test::ACCEPT)
	{
	  for_d.longest_path_code = d->test.u.acceptance.u.full.code;
	  for_d.longest_backtrack_code = d->test.u.acceptance.u.full.code;
	}

      /* Keep a running count of the number of backtracks.  */
      if (d->prev)
	for_s.longest_backtrack += 1;

      /* Accumulate D's statistics into S's.  */
      for_s.num_decisions += for_d.num_decisions;
      for_s.longest_path += for_d.longest_path;
      for_s.longest_backtrack += for_d.longest_backtrack;

      /* Use the code from the decision with the longest individual path,
	 since that's more likely to be useful if trying to make the
	 path shorter.  In the event of a tie, pick the later decision,
	 since that's closer to the end of the path.  */
      if (longest_path <= for_d.longest_path)
	{
	  longest_path = for_d.longest_path;
	  for_s.longest_path_code = for_d.longest_path_code;
	}

      /* Later decisions in a state are necessarily in a longer backtrack
	 than earlier decisions.  */
      for_s.longest_backtrack_code = for_d.longest_backtrack_code;
    }
  return for_s;
}

/* Optimize ROOT.  Use TYPE to describe ROOT in status messages.  */

static void
optimize_subroutine_group (const char *type, state *root)
{
  /* Remove optional transitions that turned out not to be worthwhile.  */
  if (collapse_optional_decisions_p)
    collapse_optional_decisions (root);

  /* Try to remove duplicated tests and to rearrange tests into a more
     logical order.  */
  if (cse_tests_p)
    {
      known_conditions kc;
      kc.position_tests.safe_grow_cleared (num_positions);
      kc.set_operands.safe_grow_cleared (num_operands);
      kc.peep2_count = 1;
      cse_tests (&root_pos, root, &kc);
    }

  /* Try to simplify two or more tests into one.  */
  if (simplify_tests_p)
    simplify_tests (root);

  /* Try to use operands[] instead of xN variables.  */
  if (use_operand_variables_p)
    {
      auto_vec <int> operand_pos (num_positions);
      for (unsigned int i = 0; i < num_positions; ++i)
	operand_pos.quick_push (-1);
      find_operand_positions (root, operand_pos);
    }

  /* Print a summary of the new state.  */
  stats st = get_stats (root);
  fprintf (stderr, "Statistics for %s:\n", type);
  fprintf (stderr, "  Number of decisions: %6d\n", st.num_decisions);
  fprintf (stderr, "  longest path:        %6d (code: %6d)\n",
	   st.longest_path, st.longest_path_code);
  fprintf (stderr, "  longest backtrack:   %6d (code: %6d)\n",
	   st.longest_backtrack, st.longest_backtrack_code);
}

struct merge_pattern_info;

/* Represents a transition from one pattern to another.  */
struct merge_pattern_transition
{
  merge_pattern_transition (merge_pattern_info *);

  /* The target pattern.  */
  merge_pattern_info *to;

  /* The parameters that the source pattern passes to the target pattern.
     "parameter (TYPE, true, I)" represents parameter I of the source
     pattern.  */
  auto_vec <parameter, MAX_PATTERN_PARAMS> params;
};

merge_pattern_transition::merge_pattern_transition (merge_pattern_info *to_in)
  : to (to_in)
{
}

/* Represents a pattern that can might match several states.  The pattern
   may replace parts of the test with a parameter value.  It may also
   replace transition labels with parameters.  */
struct merge_pattern_info
{
  merge_pattern_info (unsigned int);

  /* If PARAM_TEST_P, the state's singleton test should be generalized
     to use the runtime value of PARAMS[PARAM_TEST].  */
  unsigned int param_test : 8;

  /* If PARAM_TRANSITION_P, the state's single transition label should
     be replaced by the runtime value of PARAMS[PARAM_TRANSITION].  */
  unsigned int param_transition : 8;

  /* True if we have decided to generalize the root decision's test,
     as per PARAM_TEST.  */
  unsigned int param_test_p : 1;

  /* Likewise for the root decision's transition, as per PARAM_TRANSITION.  */
  unsigned int param_transition_p : 1;

  /* True if the contents of the structure are completely filled in.  */
  unsigned int complete_p : 1;

  /* The number of pseudo-statements in the pattern.  Used to decide
     whether it's big enough to break out into a subroutine.  */
  unsigned int num_statements;

  /* The number of states that use this pattern.  */
  unsigned int num_users;

  /* The number of distinct success values that the pattern returns.  */
  unsigned int num_results;

  /* This array has one element for each runtime parameter to the pattern.
     PARAMS[I] gives the default value of parameter I, which is always
     constant.

     These default parameters are used in cases where we match the
     pattern against some state S1, then add more parameters while
     matching against some state S2.  S1 is then left passing fewer
     parameters than S2.  The array gives us enough informatino to
     construct a full parameter list for S1 (see update_parameters).

     If we decide to create a subroutine for this pattern,
     PARAMS[I].type determines the C type of parameter I.  */
  auto_vec <parameter, MAX_PATTERN_PARAMS> params;

  /* All states that match this pattern must have the same number of
     transitions.  TRANSITIONS[I] describes the subpattern for transition
     number I; it is null if transition I represents a successful return
     from the pattern.  */
  auto_vec <merge_pattern_transition *, 1> transitions;

  /* The routine associated with the pattern, or null if we haven't generated
     one yet.  */
  pattern_routine *routine;
};

merge_pattern_info::merge_pattern_info (unsigned int num_transitions)
  : param_test (0),
    param_transition (0),
    param_test_p (false),
    param_transition_p (false),
    complete_p (false),
    num_statements (0),
    num_users (0),
    num_results (0),
    routine (0)
{
  transitions.safe_grow_cleared (num_transitions);
}

/* Describes one way of matching a particular state to a particular
   pattern.  */
struct merge_state_result
{
  merge_state_result (merge_pattern_info *, position *, merge_state_result *);

  /* A pattern that matches the state.  */
  merge_pattern_info *pattern;

  /* If we decide to use this match and create a subroutine for PATTERN,
     the state should pass the rtx at position ROOT to the pattern's
     rtx parameter.  A null root means that the pattern doesn't need
     an rtx parameter; all the rtxes it matches come from elsewhere.  */
  position *root;

  /* The parameters that should be passed to PATTERN for this state.
     If the array is shorter than PATTERN->params, the missing entries
     should be taken from the corresponding element of PATTERN->params.  */
  auto_vec <parameter, MAX_PATTERN_PARAMS> params;

  /* An earlier match for the same state, or null if none.  Patterns
     matched by earlier entries are smaller than PATTERN.  */
  merge_state_result *prev;
};

merge_state_result::merge_state_result (merge_pattern_info *pattern_in,
					position *root_in,
					merge_state_result *prev_in)
  : pattern (pattern_in), root (root_in), prev (prev_in)
{}

/* Information about a state, used while trying to match it against
   a pattern.  */
struct merge_state_info
{
  merge_state_info (state *);

  /* The state itself.  */
  state *s;

  /* Index I gives information about the target of transition I.  */
  merge_state_info *to_states;

  /* The number of transitions in S.  */
  unsigned int num_transitions;

  /* True if the state has been deleted in favor of a call to a
     pattern routine.  */
  bool merged_p;

  /* The previous state that might be a merge candidate for S, or null
     if no previous states could be merged with S.  */
  merge_state_info *prev_same_test;

  /* A list of pattern matches for this state.  */
  merge_state_result *res;
};

merge_state_info::merge_state_info (state *s_in)
  : s (s_in),
    to_states (0),
    num_transitions (0),
    merged_p (false),
    prev_same_test (0),
    res (0) {}

/* True if PAT would be useful as a subroutine.  */

static bool
useful_pattern_p (merge_pattern_info *pat)
{
  return pat->num_statements >= MIN_COMBINE_COST;
}

/* PAT2 is a subpattern of PAT1.  Return true if PAT2 should be inlined
   into PAT1's C routine.  */

static bool
same_pattern_p (merge_pattern_info *pat1, merge_pattern_info *pat2)
{
  return pat1->num_users == pat2->num_users || !useful_pattern_p (pat2);
}

/* PAT was previously matched against SINFO based on tentative matches
   for the target states of SINFO's state.  Return true if the match
   still holds; that is, if the target states of SINFO's state still
   match the corresponding transitions of PAT.  */

static bool
valid_result_p (merge_pattern_info *pat, merge_state_info *sinfo)
{
  for (unsigned int j = 0; j < sinfo->num_transitions; ++j)
    if (merge_pattern_transition *ptrans = pat->transitions[j])
      {
	merge_state_result *to_res = sinfo->to_states[j].res;
	if (!to_res || to_res->pattern != ptrans->to)
	  return false;
      }
  return true;
}

/* Remove any matches that are no longer valid from the head of SINFO's
   list of matches.  */

static void
prune_invalid_results (merge_state_info *sinfo)
{
  while (sinfo->res && !valid_result_p (sinfo->res->pattern, sinfo))
    {
      sinfo->res = sinfo->res->prev;
      gcc_assert (sinfo->res);
    }
}

/* Return true if PAT represents the biggest posssible match for SINFO;
   that is, if the next action of SINFO's state on return from PAT will
   be something that cannot be merged with any other state.  */

static bool
complete_result_p (merge_pattern_info *pat, merge_state_info *sinfo)
{
  for (unsigned int j = 0; j < sinfo->num_transitions; ++j)
    if (sinfo->to_states[j].res && !pat->transitions[j])
      return false;
  return true;
}

/* Update TO for any parameters that have been added to FROM since TO
   was last set.  The extra parameters in FROM will be constants or
   instructions to duplicate earlier parameters.  */

static void
update_parameters (vec <parameter> &to, const vec <parameter> &from)
{
  for (unsigned int i = to.length (); i < from.length (); ++i)
    to.quick_push (from[i]);
}

/* Return true if A and B can be tested by a single test.  If the test
   can be parameterised, store the parameter value for A in *PARAMA and
   the parameter value for B in *PARAMB, otherwise leave PARAMA and
   PARAMB alone.  */

static bool
compatible_tests_p (const rtx_test &a, const rtx_test &b,
		    parameter *parama, parameter *paramb)
{
  if (a.kind != b.kind)
    return false;
  switch (a.kind)
    {
    case rtx_test::PREDICATE:
      if (a.u.predicate.data != b.u.predicate.data)
	return false;
      *parama = parameter (parameter::MODE, false, a.u.predicate.mode);
      *paramb = parameter (parameter::MODE, false, b.u.predicate.mode);
      return true;

    case rtx_test::SAVED_CONST_INT:
      *parama = parameter (parameter::INT, false, a.u.integer.value);
      *paramb = parameter (parameter::INT, false, b.u.integer.value);
      return true;

    default:
      return a == b;
    }
}

/* PARAMS is an array of the parameters that a state is going to pass
   to a pattern routine.  It is still incomplete; index I has a kind of
   parameter::UNSET if we don't yet know what the state will pass
   as parameter I.  Try to make parameter ID equal VALUE, returning
   true on success.  */

static bool
set_parameter (vec <parameter> &params, unsigned int id,
	       const parameter &value)
{
  if (params[id].type == parameter::UNSET)
    {
      if (force_unique_params_p)
	for (unsigned int i = 0; i < params.length (); ++i)
	  if (params[i] == value)
	    return false;
      params[id] = value;
      return true;
    }
  return params[id] == value;
}

/* PARAMS2 is the "params" array for a pattern and PARAMS1 is the
   set of parameters that a particular state is going to pass to
   that pattern.

   Try to extend PARAMS1 and PARAMS2 so that there is a parameter
   that is equal to PARAM1 for the state and has a default value of
   PARAM2.  Parameters beginning at START were added as part of the
   same match and so may be reused.  */

static bool
add_parameter (vec <parameter> &params1, vec <parameter> &params2,
	       const parameter &param1, const parameter &param2,
	       unsigned int start, unsigned int *res)
{
  gcc_assert (params1.length () == params2.length ());
  gcc_assert (!param1.is_param && !param2.is_param);

  for (unsigned int i = start; i < params2.length (); ++i)
    if (params1[i] == param1 && params2[i] == param2)
      {
	*res = i;
	return true;
      }

  if (force_unique_params_p)
    for (unsigned int i = 0; i < params2.length (); ++i)
      if (params1[i] == param1 || params2[i] == param2)
	return false;

  if (params2.length () >= MAX_PATTERN_PARAMS)
    return false;

  *res = params2.length ();
  params1.quick_push (param1);
  params2.quick_push (param2);
  return true;
}

/* If *ROOTA is nonnull, return true if the same sequence of steps are
   required to reach A from *ROOTA as to reach B from ROOTB.  If *ROOTA
   is null, update it if necessary in order to make the condition hold.  */

static bool
merge_relative_positions (position **roota, position *a,
			  position *rootb, position *b)
{
  if (!relative_patterns_p)
    {
      if (a != b)
	return false;
      if (!*roota)
	{
	  *roota = rootb;
	  return true;
	}
      return *roota == rootb;
    }
  /* If B does not belong to the same instruction as ROOTB, we don't
     start with ROOTB but instead start with a call to peep2_next_insn.
     In that case the sequences for B and A are identical iff B and A
     are themselves identical.  */
  if (rootb->insn_id != b->insn_id)
    return a == b;
  while (rootb != b)
    {
      if (!a || b->type != a->type || b->arg != a->arg)
	return false;
      b = b->base;
      a = a->base;
    }
  if (!*roota)
    *roota = a;
  return *roota == a;
}

/* A hasher of states that treats two states as "equal" if they might be
   merged (but trying to be more discriminating than "return true").  */
struct test_pattern_hasher : nofree_ptr_hash <merge_state_info>
{
  static inline hashval_t hash (const value_type &);
  static inline bool equal (const value_type &, const compare_type &);
};

hashval_t
test_pattern_hasher::hash (merge_state_info *const &sinfo)
{
  inchash::hash h;
  decision *d = sinfo->s->singleton ();
  h.add_int (d->test.pos_operand + 1);
  if (!relative_patterns_p)
    h.add_int (d->test.pos ? d->test.pos->id + 1 : 0);
  h.add_int (d->test.kind);
  h.add_int (sinfo->num_transitions);
  return h.end ();
}

bool
test_pattern_hasher::equal (merge_state_info *const &sinfo1,
			    merge_state_info *const &sinfo2)
{
  decision *d1 = sinfo1->s->singleton ();
  decision *d2 = sinfo2->s->singleton ();
  gcc_assert (d1 && d2);

  parameter new_param1, new_param2;
  return (d1->test.pos_operand == d2->test.pos_operand
	  && (relative_patterns_p || d1->test.pos == d2->test.pos)
	  && compatible_tests_p (d1->test, d2->test, &new_param1, &new_param2)
	  && sinfo1->num_transitions == sinfo2->num_transitions);
}

/* Try to make the state described by SINFO1 use the same pattern as the
   state described by SINFO2.  Return true on success.

   SINFO1 and SINFO2 are known to have the same hash value.  */

static bool
merge_patterns (merge_state_info *sinfo1, merge_state_info *sinfo2)
{
  merge_state_result *res2 = sinfo2->res;
  merge_pattern_info *pat = res2->pattern;

  /* Write to temporary arrays while matching, in case we have to abort
     half way through.  */
  auto_vec <parameter, MAX_PATTERN_PARAMS> params1;
  auto_vec <parameter, MAX_PATTERN_PARAMS> params2;
  params1.quick_grow_cleared (pat->params.length ());
  params2.splice (pat->params);
  unsigned int start_param = params2.length ();

  /* An array for recording changes to PAT->transitions[?].params.
     All changes involve replacing a constant parameter with some
     PAT->params[N], where N is the second element of the pending_param.  */
  typedef std::pair <parameter *, unsigned int> pending_param;
  auto_vec <pending_param, 32> pending_params;

  decision *d1 = sinfo1->s->singleton ();
  decision *d2 = sinfo2->s->singleton ();
  gcc_assert (d1 && d2);

  /* If D2 tests a position, SINFO1's root relative to D1 is the same
     as SINFO2's root relative to D2.  */
  position *root1 = 0;
  position *root2 = res2->root;
  if (d2->test.pos_operand < 0
      && d1->test.pos
      && !merge_relative_positions (&root1, d1->test.pos,
				    root2, d2->test.pos))
    return false;

  /* Check whether the patterns have the same shape.  */
  unsigned int num_transitions = sinfo1->num_transitions;
  gcc_assert (num_transitions == sinfo2->num_transitions);
  for (unsigned int i = 0; i < num_transitions; ++i)
    if (merge_pattern_transition *ptrans = pat->transitions[i])
      {
	merge_state_result *to1_res = sinfo1->to_states[i].res;
	merge_state_result *to2_res = sinfo2->to_states[i].res;
	merge_pattern_info *to_pat = ptrans->to;
	gcc_assert (to2_res && to2_res->pattern == to_pat);
	if (!to1_res || to1_res->pattern != to_pat)
	  return false;
	if (to2_res->root
	    && !merge_relative_positions (&root1, to1_res->root,
					  root2, to2_res->root))
	  return false;
	/* Match the parameters that TO1_RES passes to TO_PAT with the
	   parameters that PAT passes to TO_PAT.  */
	update_parameters (to1_res->params, to_pat->params);
	for (unsigned int j = 0; j < to1_res->params.length (); ++j)
	  {
	    const parameter &param1 = to1_res->params[j];
	    const parameter &param2 = ptrans->params[j];
	    gcc_assert (!param1.is_param);
	    if (param2.is_param)
	      {
		if (!set_parameter (params1, param2.value, param1))
		  return false;
	      }
	    else if (param1 != param2)
	      {
		unsigned int id;
		if (!add_parameter (params1, params2,
				    param1, param2, start_param, &id))
		  return false;
		/* Record that PAT should now pass parameter ID to TO_PAT,
		   instead of the current contents of *PARAM2.  We only
		   make the change if the rest of the match succeeds.  */
		pending_params.safe_push
		  (pending_param (&ptrans->params[j], id));
	      }
	  }
      }

  unsigned int param_test = pat->param_test;
  unsigned int param_transition = pat->param_transition;
  bool param_test_p = pat->param_test_p;
  bool param_transition_p = pat->param_transition_p;

  /* If the tests don't match exactly, try to parameterize them.  */
  parameter new_param1, new_param2;
  if (!compatible_tests_p (d1->test, d2->test, &new_param1, &new_param2))
    gcc_unreachable ();
  if (new_param1.type != parameter::UNSET)
    {
      /* If the test has not already been parameterized, all existing
	 matches use constant NEW_PARAM2.  */
      if (param_test_p)
	{
	  if (!set_parameter (params1, param_test, new_param1))
	    return false;
	}
      else if (new_param1 != new_param2)
	{
	  if (!add_parameter (params1, params2, new_param1, new_param2,
			      start_param, &param_test))
	    return false;
	  param_test_p = true;
	}
    }

  /* Match the transitions.  */
  transition *trans1 = d1->first;
  transition *trans2 = d2->first;
  for (unsigned int i = 0; i < num_transitions; ++i)
    {
      if (param_transition_p || trans1->labels != trans2->labels)
	{
	  /* We can only generalize a single transition with a single
	     label.  */
	  if (num_transitions != 1
	      || trans1->labels.length () != 1
	      || trans2->labels.length () != 1)
	    return false;

	  /* Although we can match wide-int fields, in practice it leads
	     to some odd results for const_vectors.  We end up
	     parameterizing the first N const_ints of the vector
	     and then (once we reach the maximum number of parameters)
	     we go on to match the other elements exactly.  */
	  if (d1->test.kind == rtx_test::WIDE_INT_FIELD)
	    return false;

	  /* See whether the label has a generalizable type.  */
	  parameter::type_enum param_type
	    = transition_parameter_type (d1->test.kind);
	  if (param_type == parameter::UNSET)
	    return false;

	  /* Match the labels using parameters.  */
	  new_param1 = parameter (param_type, false, trans1->labels[0]);
	  if (param_transition_p)
	    {
	      if (!set_parameter (params1, param_transition, new_param1))
		return false;
	    }
	  else
	    {
	      new_param2 = parameter (param_type, false, trans2->labels[0]);
	      if (!add_parameter (params1, params2, new_param1, new_param2,
				  start_param, &param_transition))
		return false;
	      param_transition_p = true;
	    }
	}
      trans1 = trans1->next;
      trans2 = trans2->next;
    }

  /* Set any unset parameters to their default values.  This occurs if some
     other state needed something to be parameterized in order to match SINFO2,
     but SINFO1 on its own does not.  */
  for (unsigned int i = 0; i < params1.length (); ++i)
    if (params1[i].type == parameter::UNSET)
      params1[i] = params2[i];

  /* The match was successful.  Commit all pending changes to PAT.  */
  update_parameters (pat->params, params2);
  {
    pending_param *pp;
    unsigned int i;
    FOR_EACH_VEC_ELT (pending_params, i, pp)
      *pp->first = parameter (pp->first->type, true, pp->second);
  }
  pat->param_test = param_test;
  pat->param_transition = param_transition;
  pat->param_test_p = param_test_p;
  pat->param_transition_p = param_transition_p;

  /* Record the match of SINFO1.  */
  merge_state_result *new_res1 = new merge_state_result (pat, root1,
							 sinfo1->res);
  new_res1->params.splice (params1);
  sinfo1->res = new_res1;
  return true;
}

/* The number of states that were removed by calling pattern routines.  */
static unsigned int pattern_use_states;

/* The number of states used while defining pattern routines.  */
static unsigned int pattern_def_states;

/* Information used while constructing a use or definition of a pattern
   routine.  */
struct create_pattern_info
{
  /* The routine itself.  */
  pattern_routine *routine;

  /* The first unclaimed return value for this particular use or definition.
     We walk the substates of uses and definitions in the same order
     so each return value always refers to the same position within
     the pattern.  */
  unsigned int next_result;
};

static void populate_pattern_routine (create_pattern_info *,
				      merge_state_info *, state *,
				      const vec <parameter> &);

/* SINFO matches a pattern for which we've decided to create a C routine.
   Return a decision that performs a call to the pattern routine,
   but leave the caller to add the transitions to it.  Initialize CPI
   for this purpose.  Also create a definition for the pattern routine,
   if it doesn't already have one.

   PARAMS are the parameters that SINFO passes to its pattern.  */

static decision *
init_pattern_use (create_pattern_info *cpi, merge_state_info *sinfo,
		  const vec <parameter> &params)
{
  state *s = sinfo->s;
  merge_state_result *res = sinfo->res;
  merge_pattern_info *pat = res->pattern;
  cpi->routine = pat->routine;
  if (!cpi->routine)
    {
      /* We haven't defined the pattern routine yet, so create
	 a definition now.  */
      pattern_routine *routine = new pattern_routine;
      pat->routine = routine;
      cpi->routine = routine;
      routine->s = new state;
      routine->insn_p = false;
      routine->pnum_clobbers_p = false;

      /* Create an "idempotent" mapping of parameter I to parameter I.
	 Also record the C type of each parameter to the routine.  */
      auto_vec <parameter, MAX_PATTERN_PARAMS> def_params;
      for (unsigned int i = 0; i < pat->params.length (); ++i)
	{
	  def_params.quick_push (parameter (pat->params[i].type, true, i));
	  routine->param_types.quick_push (pat->params[i].type);
	}

      /* Any of the states that match the pattern could be used to
	 create the routine definition.  We might as well use SINFO
	 since it's already to hand.  This means that all positions
	 in the definition will be relative to RES->root.  */
      routine->pos = res->root;
      cpi->next_result = 0;
      populate_pattern_routine (cpi, sinfo, routine->s, def_params);
      gcc_assert (cpi->next_result == pat->num_results);

      /* Add the routine to the global list, after the subroutines
	 that it calls.  */
      routine->pattern_id = patterns.length ();
      patterns.safe_push (routine);
    }

  /* Create a decision to call the routine, passing PARAMS to it.  */
  pattern_use *use = new pattern_use;
  use->routine = pat->routine;
  use->params.splice (params);
  decision *d = new decision (rtx_test::pattern (res->root, use));

  /* If the original decision could use an element of operands[] instead
     of an rtx variable, try to transfer it to the new decision.  */
  if (s->first->test.pos && res->root == s->first->test.pos)
    d->test.pos_operand = s->first->test.pos_operand;

  cpi->next_result = 0;
  return d;
}

/* Make S return the next unclaimed pattern routine result for CPI.  */

static void
add_pattern_acceptance (create_pattern_info *cpi, state *s)
{
  acceptance_type acceptance;
  acceptance.type = SUBPATTERN;
  acceptance.partial_p = false;
  acceptance.u.full.code = cpi->next_result;
  add_decision (s, rtx_test::accept (acceptance), true, false);
  cpi->next_result += 1;
}

/* Initialize new empty state NEWS so that it implements SINFO's pattern
   (here referred to as "P").  P may be the top level of a pattern routine
   or a subpattern that should be inlined into its parent pattern's routine
   (as per same_pattern_p).  The choice of SINFO for a top-level pattern is
   arbitrary; it could be any of the states that use P.  The choice for
   subpatterns follows the choice for the parent pattern.

   PARAMS gives the value of each parameter to P in terms of the parameters
   to the top-level pattern.  If P itself is the top level pattern, PARAMS[I]
   is always "parameter (TYPE, true, I)".  */

static void
populate_pattern_routine (create_pattern_info *cpi, merge_state_info *sinfo,
			  state *news, const vec <parameter> &params)
{
  pattern_def_states += 1;

  decision *d = sinfo->s->singleton ();
  merge_pattern_info *pat = sinfo->res->pattern;
  pattern_routine *routine = cpi->routine;

  /* Create a copy of D's test for the pattern routine and generalize it
     as appropriate.  */
  decision *newd = new decision (d->test);
  gcc_assert (newd->test.pos_operand >= 0
	      || !newd->test.pos
	      || common_position (newd->test.pos,
				  routine->pos) == routine->pos);
  if (pat->param_test_p)
    {
      const parameter &param = params[pat->param_test];
      switch (newd->test.kind)
	{
	case rtx_test::PREDICATE:
	  newd->test.u.predicate.mode_is_param = param.is_param;
	  newd->test.u.predicate.mode = param.value;
	  break;

	case rtx_test::SAVED_CONST_INT:
	  newd->test.u.integer.is_param = param.is_param;
	  newd->test.u.integer.value = param.value;
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}
    }
  if (d->test.kind == rtx_test::C_TEST)
    routine->insn_p = true;
  else if (d->test.kind == rtx_test::HAVE_NUM_CLOBBERS)
    routine->pnum_clobbers_p = true;
  news->push_back (newd);

  /* Fill in the transitions of NEWD.  */
  unsigned int i = 0;
  for (transition *trans = d->first; trans; trans = trans->next)
    {
      /* Create a new state to act as the target of the new transition.  */
      state *to_news = new state;
      if (merge_pattern_transition *ptrans = pat->transitions[i])
	{
	  /* The pattern hasn't finished matching yet.  Get the target
	     pattern and the corresponding target state of SINFO.  */
	  merge_pattern_info *to_pat = ptrans->to;
	  merge_state_info *to = sinfo->to_states + i;
	  gcc_assert (to->res->pattern == to_pat);
	  gcc_assert (ptrans->params.length () == to_pat->params.length ());

	  /* Express the parameters to TO_PAT in terms of the parameters
	     to the top-level pattern.  */
	  auto_vec <parameter, MAX_PATTERN_PARAMS> to_params;
	  for (unsigned int j = 0; j < ptrans->params.length (); ++j)
	    {
	      const parameter &param = ptrans->params[j];
	      to_params.quick_push (param.is_param
				    ? params[param.value]
				    : param);
	    }

	  if (same_pattern_p (pat, to_pat))
	    /* TO_PAT is part of the current routine, so just recurse.  */
	    populate_pattern_routine (cpi, to, to_news, to_params);
	  else
	    {
	      /* TO_PAT should be matched by calling a separate routine.  */
	      create_pattern_info sub_cpi;
	      decision *subd = init_pattern_use (&sub_cpi, to, to_params);
	      routine->insn_p |= sub_cpi.routine->insn_p;
	      routine->pnum_clobbers_p |= sub_cpi.routine->pnum_clobbers_p;

	      /* Add the pattern routine call to the new target state.  */
	      to_news->push_back (subd);

	      /* Add a transition for each successful call result.  */
	      for (unsigned int j = 0; j < to_pat->num_results; ++j)
		{
		  state *res = new state;
		  add_pattern_acceptance (cpi, res);
		  subd->push_back (new transition (j, res, false));
		}
	    }
	}
      else
	/* This transition corresponds to a successful match.  */
	add_pattern_acceptance (cpi, to_news);

      /* Create the transition itself, generalizing as necessary.  */
      transition *new_trans = new transition (trans->labels, to_news,
					      trans->optional);
      if (pat->param_transition_p)
	{
	  const parameter &param = params[pat->param_transition];
	  new_trans->is_param = param.is_param;
	  new_trans->labels[0] = param.value;
	}
      newd->push_back (new_trans);
      i += 1;
    }
}

/* USE is a decision that calls a pattern routine and SINFO is part of the
   original state tree that the call is supposed to replace.  Add the
   transitions for SINFO and its substates to USE.  */

static void
populate_pattern_use (create_pattern_info *cpi, decision *use,
		      merge_state_info *sinfo)
{
  pattern_use_states += 1;
  gcc_assert (!sinfo->merged_p);
  sinfo->merged_p = true;
  merge_state_result *res = sinfo->res;
  merge_pattern_info *pat = res->pattern;
  decision *d = sinfo->s->singleton ();
  unsigned int i = 0;
  for (transition *trans = d->first; trans; trans = trans->next)
    {
      if (pat->transitions[i])
	/* The target state is also part of the pattern.  */
	populate_pattern_use (cpi, use, sinfo->to_states + i);
      else
	{
	  /* The transition corresponds to a successful return from the
	     pattern routine.  */
	  use->push_back (new transition (cpi->next_result, trans->to, false));
	  cpi->next_result += 1;
	}
      i += 1;
    }
}

/* We have decided to replace SINFO's state with a call to a pattern
   routine.  Make the change, creating a definition of the pattern routine
   if it doesn't have one already.  */

static void
use_pattern (merge_state_info *sinfo)
{
  merge_state_result *res = sinfo->res;
  merge_pattern_info *pat = res->pattern;
  state *s = sinfo->s;

  /* The pattern may have acquired new parameters after it was matched
     against SINFO.  Update the parameters that SINFO passes accordingly.  */
  update_parameters (res->params, pat->params);

  create_pattern_info cpi;
  decision *d = init_pattern_use (&cpi, sinfo, res->params);
  populate_pattern_use (&cpi, d, sinfo);
  s->release ();
  s->push_back (d);
}

/* Look through the state trees in STATES for common patterns and
   split them into subroutines.  */

static void
split_out_patterns (vec <merge_state_info> &states)
{
  unsigned int first_transition = states.length ();
  hash_table <test_pattern_hasher> hashtab (128);
  /* Stage 1: Create an order in which parent states come before their child
     states and in which sibling states are at consecutive locations.
     Having consecutive sibling states allows merge_state_info to have
     a single to_states pointer.  */
  for (unsigned int i = 0; i < states.length (); ++i)
    for (decision *d = states[i].s->first; d; d = d->next)
      for (transition *trans = d->first; trans; trans = trans->next)
	{
	  states.safe_push (trans->to);
	  states[i].num_transitions += 1;
	}
  /* Stage 2: Now that the addresses are stable, set up the to_states
     pointers.  Look for states that might be merged and enter them
     into the hash table.  */
  for (unsigned int i = 0; i < states.length (); ++i)
    {
      merge_state_info *sinfo = &states[i];
      if (sinfo->num_transitions)
	{
	  sinfo->to_states = &states[first_transition];
	  first_transition += sinfo->num_transitions;
	}
      /* For simplicity, we only try to merge states that have a single
	 decision.  This is in any case the best we can do for peephole2,
	 since whether a peephole2 ACCEPT succeeds or not depends on the
	 specific peephole2 pattern (which is unique to each ACCEPT
	 and so couldn't be shared between states).  */
      if (decision *d = sinfo->s->singleton ())
	/* ACCEPT states are unique, so don't even try to merge them.  */
	if (d->test.kind != rtx_test::ACCEPT
	    && (pattern_have_num_clobbers_p
		|| d->test.kind != rtx_test::HAVE_NUM_CLOBBERS)
	    && (pattern_c_test_p
		|| d->test.kind != rtx_test::C_TEST))
	  {
	    merge_state_info **slot = hashtab.find_slot (sinfo, INSERT);
	    sinfo->prev_same_test = *slot;
	    *slot = sinfo;
	  }
    }
  /* Stage 3: Walk backwards through the list of states and try to merge
     them.  This is a greedy, bottom-up match; parent nodes can only start
     a new leaf pattern if they fail to match when combined with all child
     nodes that have matching patterns.

     For each state we keep a list of potential matches, with each
     potential match being larger (and deeper) than the next match in
     the list.  The final element in the list is a leaf pattern that
     matches just a single state.

     Each candidate pattern created in this loop is unique -- it won't
     have been seen by an earlier iteration.  We try to match each pattern
     with every state that appears earlier in STATES.

     Because the patterns created in the loop are unique, any state
     that already has a match must have a final potential match that
     is different from any new leaf pattern.  Therefore, when matching
     leaf patterns, we need only consider states whose list of matches
     is empty.

     The non-leaf patterns that we try are as deep as possible
     and are an extension of the state's previous best candidate match (PB).
     We need only consider states whose current potential match is also PB;
     any states that don't match as much as PB cannnot match the new pattern,
     while any states that already match more than PB must be different from
     the new pattern.  */
  for (unsigned int i2 = states.length (); i2-- > 0; )
    {
      merge_state_info *sinfo2 = &states[i2];

      /* Enforce the bottom-upness of the match: remove matches with later
	 states if SINFO2's child states ended up finding a better match.  */
      prune_invalid_results (sinfo2);

      /* Do nothing if the state doesn't match a later one and if there are
	 no earlier states it could match.  */
      if (!sinfo2->res && !sinfo2->prev_same_test)
	continue;

      merge_state_result *res2 = sinfo2->res;
      decision *d2 = sinfo2->s->singleton ();
      position *root2 = (d2->test.pos_operand < 0 ? d2->test.pos : 0);
      unsigned int num_transitions = sinfo2->num_transitions;

      /* If RES2 is null then SINFO2's test in isolation has not been seen
	 before.  First try matching that on its own.  */
      if (!res2)
	{
	  merge_pattern_info *new_pat
	    = new merge_pattern_info (num_transitions);
	  merge_state_result *new_res2
	    = new merge_state_result (new_pat, root2, res2);
	  sinfo2->res = new_res2;

	  new_pat->num_statements = !d2->test.single_outcome_p ();
	  new_pat->num_results = num_transitions;
	  bool matched_p = false;
	  /* Look for states that don't currently match anything but
	     can be made to match SINFO2 on its own.  */
	  for (merge_state_info *sinfo1 = sinfo2->prev_same_test; sinfo1;
	       sinfo1 = sinfo1->prev_same_test)
	    if (!sinfo1->res && merge_patterns (sinfo1, sinfo2))
	      matched_p = true;
	  if (!matched_p)
	    {
	      /* No other states match.  */
	      sinfo2->res = res2;
	      delete new_pat;
	      delete new_res2;
	      continue;
	    }
	  else
	    res2 = new_res2;
	}

      /* Keep the existing pattern if it's as good as anything we'd
	 create for SINFO2.  */
      if (complete_result_p (res2->pattern, sinfo2))
	{
	  res2->pattern->num_users += 1;
	  continue;
	}

      /* Create a new pattern for SINFO2.  */
      merge_pattern_info *new_pat = new merge_pattern_info (num_transitions);
      merge_state_result *new_res2
	= new merge_state_result (new_pat, root2, res2);
      sinfo2->res = new_res2;

      /* Fill in details about the pattern.  */
      new_pat->num_statements = !d2->test.single_outcome_p ();
      new_pat->num_results = 0;
      for (unsigned int j = 0; j < num_transitions; ++j)
	if (merge_state_result *to_res = sinfo2->to_states[j].res)
	  {
	    /* Count the target state as part of this pattern.
	       First update the root position so that it can reach
	       the target state's root.  */
	    if (to_res->root)
	      {
		if (new_res2->root)
		  new_res2->root = common_position (new_res2->root,
						    to_res->root);
		else
		  new_res2->root = to_res->root;
	      }
	    merge_pattern_info *to_pat = to_res->pattern;
	    merge_pattern_transition *ptrans
	      = new merge_pattern_transition (to_pat);

	    /* TO_PAT may have acquired more parameters when matching
	       states earlier in STATES than TO_RES's, but the list is
	       now final.  Make sure that TO_RES is up to date.  */
	    update_parameters (to_res->params, to_pat->params);

	    /* Start out by assuming that every user of NEW_PAT will
	       want to pass the same (constant) parameters as TO_RES.  */
	    update_parameters (ptrans->params, to_res->params);

	    new_pat->transitions[j] = ptrans;
	    new_pat->num_statements += to_pat->num_statements;
	    new_pat->num_results += to_pat->num_results;
	  }
	else
	  /* The target state doesn't match anything and so is not part
	     of the pattern.  */
	  new_pat->num_results += 1;

      /* See if any earlier states that match RES2's pattern also match
	 NEW_PAT.  */
      bool matched_p = false;
      for (merge_state_info *sinfo1 = sinfo2->prev_same_test; sinfo1;
	   sinfo1 = sinfo1->prev_same_test)
	{
	  prune_invalid_results (sinfo1);
	  if (sinfo1->res
	      && sinfo1->res->pattern == res2->pattern
	      && merge_patterns (sinfo1, sinfo2))
	    matched_p = true;
	}
      if (!matched_p)
	{
	  /* Nothing else matches NEW_PAT, so go back to the previous
	     pattern (possibly just a single-state one).  */
	  sinfo2->res = res2;
	  delete new_pat;
	  delete new_res2;
	}
      /* Assume that SINFO2 will use RES.  At this point we don't know
	 whether earlier states that match the same pattern will use
	 that match or a different one.  */
      sinfo2->res->pattern->num_users += 1;
    }
  /* Step 4: Finalize the choice of pattern for each state, ignoring
     patterns that were only used once.  Update each pattern's size
     so that it doesn't include subpatterns that are going to be split
     out into subroutines.  */
  for (unsigned int i = 0; i < states.length (); ++i)
    {
      merge_state_info *sinfo = &states[i];
      merge_state_result *res = sinfo->res;
      /* Wind past patterns that are only used by SINFO.  */
      while (res && res->pattern->num_users == 1)
	{
	  res = res->prev;
	  sinfo->res = res;
	  if (res)
	    res->pattern->num_users += 1;
	}
      if (!res)
	continue;

      /* We have a shared pattern and are now committed to the match.  */
      merge_pattern_info *pat = res->pattern;
      gcc_assert (valid_result_p (pat, sinfo));

      if (!pat->complete_p)
	{
	  /* Look for subpatterns that are going to be split out and remove
	     them from the number of statements.  */
	  for (unsigned int j = 0; j < sinfo->num_transitions; ++j)
	    if (merge_pattern_transition *ptrans = pat->transitions[j])
	      {
		merge_pattern_info *to_pat = ptrans->to;
		if (!same_pattern_p (pat, to_pat))
		  pat->num_statements -= to_pat->num_statements;
	      }
	  pat->complete_p = true;
	}
    }
  /* Step 5: Split out the patterns.  */
  for (unsigned int i = 0; i < states.length (); ++i)
    {
      merge_state_info *sinfo = &states[i];
      merge_state_result *res = sinfo->res;
      if (!sinfo->merged_p && res && useful_pattern_p (res->pattern))
	use_pattern (sinfo);
    }
  fprintf (stderr, "Shared %d out of %d states by creating %d new states,"
	   " saving %d\n",
	   pattern_use_states, states.length (), pattern_def_states,
	   pattern_use_states - pattern_def_states);
}

/* Information about a state tree that we're considering splitting into a
   subroutine.  */
struct state_size
{
  /* The number of pseudo-statements in the state tree.  */
  unsigned int num_statements;

  /* The approximate number of nested "if" and "switch" statements that
     would be required if control could fall through to a later state.  */
  unsigned int depth;
};

/* Pairs a transition with information about its target state.  */
typedef std::pair <transition *, state_size> subroutine_candidate;

/* Sort two subroutine_candidates so that the one with the largest
   number of statements comes last.  */

static int
subroutine_candidate_cmp (const void *a, const void *b)
{
  return int (((const subroutine_candidate *) a)->second.num_statements
	      - ((const subroutine_candidate *) b)->second.num_statements);
}

/* Turn S into a subroutine of type TYPE and add it to PROCS.  Return a new
   state that performs a subroutine call to S.  */

static state *
create_subroutine (routine_type type, state *s, vec <state *> &procs)
{
  procs.safe_push (s);
  acceptance_type acceptance;
  acceptance.type = type;
  acceptance.partial_p = true;
  acceptance.u.subroutine_id = procs.length ();
  state *news = new state;
  add_decision (news, rtx_test::accept (acceptance), true, false);
  return news;
}

/* Walk state tree S, of type TYPE, and look for subtrees that would be
   better split into subroutines.  Accumulate all such subroutines in PROCS.
   Return the size of the new state tree (excluding subroutines).  */

static state_size
find_subroutines (routine_type type, state *s, vec <state *> &procs)
{
  auto_vec <subroutine_candidate, 16> candidates;
  state_size size;
  size.num_statements = 0;
  size.depth = 0;
  for (decision *d = s->first; d; d = d->next)
    {
      if (!d->test.single_outcome_p ())
	size.num_statements += 1;
      for (transition *trans = d->first; trans; trans = trans->next)
	{
	  /* Keep chains of simple decisions together if we know that no
	     change of position is required.  We'll output this chain as a
	     single "if" statement, so it counts as a single nesting level.  */
	  if (d->test.pos && d->if_statement_p ())
	    for (;;)
	      {
		decision *newd = trans->to->singleton ();
		if (!newd
		    || (newd->test.pos
			&& newd->test.pos_operand < 0
			&& newd->test.pos != d->test.pos)
		    || !newd->if_statement_p ())
		  break;
		if (!newd->test.single_outcome_p ())
		  size.num_statements += 1;
		trans = newd->singleton ();
		if (newd->test.kind == rtx_test::SET_OP
		    || newd->test.kind == rtx_test::ACCEPT)
		  break;
	      }
	  /* The target of TRANS is a subroutine candidate.  First recurse
	     on it to see how big it is after subroutines have been
	     split out.  */
	  state_size to_size = find_subroutines (type, trans->to, procs);
	  if (d->next && to_size.depth > MAX_DEPTH)
	    /* Keeping the target state in the same routine would lead
	       to an excessive nesting of "if" and "switch" statements.
	       Split it out into a subroutine so that it can use
	       inverted tests that return early on failure.  */
	    trans->to = create_subroutine (type, trans->to, procs);
	  else
	    {
	      size.num_statements += to_size.num_statements;
	      if (to_size.num_statements < MIN_NUM_STATEMENTS)
		/* The target state is too small to be worth splitting.
		   Keep it in the same routine as S.  */
		size.depth = MAX (size.depth, to_size.depth);
	      else
		/* Assume for now that we'll keep the target state in the
		   same routine as S, but record it as a subroutine candidate
		   if S grows too big.  */
		candidates.safe_push (subroutine_candidate (trans, to_size));
	    }
	}
    }
  if (size.num_statements > MAX_NUM_STATEMENTS)
    {
      /* S is too big.  Sort the subroutine candidates so that bigger ones
	 are nearer the end.  */
      candidates.qsort (subroutine_candidate_cmp);
      while (!candidates.is_empty ()
	     && size.num_statements > MAX_NUM_STATEMENTS)
	{
	  /* Peel off a candidate and force it into a subroutine.  */
	  subroutine_candidate cand = candidates.pop ();
	  size.num_statements -= cand.second.num_statements;
	  cand.first->to = create_subroutine (type, cand.first->to, procs);
	}
    }
  /* Update the depth for subroutine candidates that we decided not to
     split out.  */
  for (unsigned int i = 0; i < candidates.length (); ++i)
    size.depth = MAX (size.depth, candidates[i].second.depth);
  size.depth += 1;
  return size;
}

/* Return true if, for all X, PRED (X, MODE) implies that X has mode MODE.  */

static bool
safe_predicate_mode (const struct pred_data *pred, machine_mode mode)
{
  /* Scalar integer constants have VOIDmode.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && (pred->codes[CONST_INT]
	  || pred->codes[CONST_DOUBLE]
	  || pred->codes[CONST_WIDE_INT]
	  || pred->codes[LABEL_REF]))
    return false;

  return !pred->special && mode != VOIDmode;
}

/* Fill CODES with the set of codes that could be matched by PRED.  */

static void
get_predicate_codes (const struct pred_data *pred, int_set *codes)
{
  for (int i = 0; i < NUM_TRUE_RTX_CODE; ++i)
    if (!pred || pred->codes[i])
      codes->safe_push (i);
}

/* Return true if the first path through D1 tests the same thing as D2.  */

static bool
has_same_test_p (decision *d1, decision *d2)
{
  do
    {
      if (d1->test == d2->test)
        return true;
      d1 = d1->first->to->first;
    }
  while (d1);
  return false;
}

/* Return true if D1 and D2 cannot match the same rtx.  All states reachable
   from D2 have single decisions and all those decisions have single
   transitions.  */

static bool
mutually_exclusive_p (decision *d1, decision *d2)
{
  /* If one path through D1 fails to test the same thing as D2, assume
     that D2's test could be true for D1 and look for a later, more useful,
     test.  This isn't as expensive as it looks in practice.  */
  while (!has_same_test_p (d1, d2))
    {
      d2 = d2->singleton ()->to->singleton ();
      if (!d2)
	return false;
    }
  if (d1->test == d2->test)
    {
      /* Look for any transitions from D1 that have the same labels as
	 the transition from D2.  */
      transition *trans2 = d2->singleton ();
      for (transition *trans1 = d1->first; trans1; trans1 = trans1->next)
	{
	  int_set::iterator i1 = trans1->labels.begin ();
	  int_set::iterator end1 = trans1->labels.end ();
	  int_set::iterator i2 = trans2->labels.begin ();
	  int_set::iterator end2 = trans2->labels.end ();
	  while (i1 != end1 && i2 != end2)
	    if (*i1 < *i2)
	      ++i1;
	    else if (*i2 < *i1)
	      ++i2;
	    else
	      {
		/* TRANS1 has some labels in common with TRANS2.  Assume
		   that D1 and D2 could match the same rtx if the target
		   of TRANS1 could match the same rtx as D2.  */
		for (decision *subd1 = trans1->to->first;
		     subd1; subd1 = subd1->next)
		  if (!mutually_exclusive_p (subd1, d2))
		    return false;
		break;
	      }
	}
      return true;
    }
  for (transition *trans1 = d1->first; trans1; trans1 = trans1->next)
    for (decision *subd1 = trans1->to->first; subd1; subd1 = subd1->next)
      if (!mutually_exclusive_p (subd1, d2))
	return false;
  return true;
}

/* Try to merge S2's decision into D1, given that they have the same test.
   Fail only if EXCLUDE is nonnull and the new transition would have the
   same labels as *EXCLUDE.  When returning true, set *NEXT_S1, *NEXT_S2
   and *NEXT_EXCLUDE as for merge_into_state_1, or set *NEXT_S2 to null
   if the merge is complete.  */

static bool
merge_into_decision (decision *d1, state *s2, const int_set *exclude,
		     state **next_s1, state **next_s2,
		     const int_set **next_exclude)
{
  decision *d2 = s2->singleton ();
  transition *trans2 = d2->singleton ();

  /* Get a list of the transitions that intersect TRANS2.  */
  auto_vec <transition *, 32> intersecting;
  for (transition *trans1 = d1->first; trans1; trans1 = trans1->next)
    {
      int_set::iterator i1 = trans1->labels.begin ();
      int_set::iterator end1 = trans1->labels.end ();
      int_set::iterator i2 = trans2->labels.begin ();
      int_set::iterator end2 = trans2->labels.end ();
      bool trans1_is_subset = true;
      bool trans2_is_subset = true;
      bool intersect_p = false;
      while (i1 != end1 && i2 != end2)
	if (*i1 < *i2)
	  {
	    trans1_is_subset = false;
	    ++i1;
	  }
	else if (*i2 < *i1)
	  {
	    trans2_is_subset = false;
	    ++i2;
	  }
	else
	  {
	    intersect_p = true;
	    ++i1;
	    ++i2;
	  }
      if (i1 != end1)
	trans1_is_subset = false;
      if (i2 != end2)
	trans2_is_subset = false;
      if (trans1_is_subset && trans2_is_subset)
	{
	  /* There's already a transition that matches exactly.
	     Merge the target states.  */
	  trans1->optional &= trans2->optional;
	  *next_s1 = trans1->to;
	  *next_s2 = trans2->to;
	  *next_exclude = 0;
	  return true;
	}
      if (trans2_is_subset)
	{
	  /* TRANS1 has all the labels that TRANS2 needs.  Merge S2 into
	     the target of TRANS1, but (to avoid infinite recursion)
	     make sure that we don't end up creating another transition
	     like TRANS1.  */
	  *next_s1 = trans1->to;
	  *next_s2 = s2;
	  *next_exclude = &trans1->labels;
	  return true;
	}
      if (intersect_p)
	intersecting.safe_push (trans1);
    }

  if (intersecting.is_empty ())
    {
      /* No existing labels intersect the new ones.  We can just add
	 TRANS2 itself.  */
      d1->push_back (d2->release ());
      *next_s1 = 0;
      *next_s2 = 0;
      *next_exclude = 0;
      return true;
    }

  /* Take the union of the labels in INTERSECTING and TRANS2.  Store the
     result in COMBINED and use NEXT as a temporary.  */
  int_set tmp1 = trans2->labels, tmp2;
  int_set *combined = &tmp1, *next = &tmp2;
  for (unsigned int i = 0; i < intersecting.length (); ++i)
    {
      transition *trans1 = intersecting[i];
      next->truncate (0);
      next->safe_grow (trans1->labels.length () + combined->length ());
      int_set::iterator end
	= std::set_union (trans1->labels.begin (), trans1->labels.end (),
			  combined->begin (), combined->end (),
			  next->begin ());
      next->truncate (end - next->begin ());
      std::swap (next, combined);
    }

  /* Stop now if we've been told not to create a transition with these
     labels.  */
  if (exclude && *combined == *exclude)
    return false;

  /* Get the transition that should carry the new labels.  */
  transition *new_trans = intersecting[0];
  if (intersecting.length () == 1)
    {
      /* We're merging with one existing transition whose labels are a
	 subset of those required.  If both transitions are optional,
	 we can just expand the set of labels so that it's suitable
	 for both transitions.  It isn't worth preserving the original
	 transitions since we know that they can't be merged; we would
	 need to backtrack to S2 if TRANS1->to fails.  In contrast,
	 we might be able to merge the targets of the transitions
	 without any backtracking.

	 If instead the existing transition is not optional, ensure that
	 all target decisions are suitably protected.  Some decisions
	 might already have a more specific requirement than NEW_TRANS,
	 in which case there's no point testing NEW_TRANS as well.  E.g. this
	 would have happened if a test for an (eq ...) rtx had been
	 added to a decision that tested whether the code is suitable
	 for comparison_operator.  The original comparison_operator
	 transition would have been non-optional and the (eq ...) test
	 would be performed by a second decision in the target of that
	 transition.

	 The remaining case -- keeping the original optional transition
	 when adding a non-optional TRANS2 -- is a wash.  Preserving
	 the optional transition only helps if we later merge another
	 state S3 that is mutually exclusive with S2 and whose labels
	 belong to *COMBINED - TRANS1->labels.  We can then test the
	 original NEW_TRANS and S3 in the same decision.  We keep the
	 optional transition around for that case, but it occurs very
	 rarely.  */
      gcc_assert (new_trans->labels != *combined);
      if (!new_trans->optional || !trans2->optional)
	{
	  decision *start = 0;
	  for (decision *end = new_trans->to->first; end; end = end->next)
	    {
	      if (!start && end->test != d1->test)
		/* END belongs to a range of decisions that need to be
		   protected by NEW_TRANS.  */
		start = end;
	      if (start && (!end->next || end->next->test == d1->test))
		{
		  /* Protect [START, END] with NEW_TRANS.  The decisions
		     move to NEW_S and NEW_D becomes part of NEW_TRANS->to.  */
		  state *new_s = new state;
		  decision *new_d = new decision (d1->test);
		  new_d->push_back (new transition (new_trans->labels, new_s,
						    new_trans->optional));
		  state::range r (start, end);
		  new_trans->to->replace (r, new_d);
		  new_s->push_back (r);

		  /* Continue with an empty range.  */
		  start = 0;

		  /* Continue from the decision after NEW_D.  */
		  end = new_d;
		}
	    }
	}
      new_trans->optional = true;
      new_trans->labels = *combined;
    }
  else
    {
      /* We're merging more than one existing transition together.
	 Those transitions are successfully dividing the matching space
	 and so we want to preserve them, even if they're optional.

	 Create a new transition with the union set of labels and make
	 it go to a state that has the original transitions.  */
      decision *new_d = new decision (d1->test);
      for (unsigned int i = 0; i < intersecting.length (); ++i)
	new_d->push_back (d1->remove (intersecting[i]));

      state *new_s = new state;
      new_s->push_back (new_d);

      new_trans = new transition (*combined, new_s, true);
      d1->push_back (new_trans);
    }

  /* We now have an optional transition with labels *COMBINED.  Decide
     whether we can use it as TRANS2 or whether we need to merge S2
     into the target of NEW_TRANS.  */
  gcc_assert (new_trans->optional);
  if (new_trans->labels == trans2->labels)
    {
      /* NEW_TRANS matches TRANS2.  Just merge the target states.  */
      new_trans->optional = trans2->optional;
      *next_s1 = new_trans->to;
      *next_s2 = trans2->to;
      *next_exclude = 0;
    }
  else
    {
      /* Try to merge TRANS2 into the target of the overlapping transition,
	 but (to prevent infinite recursion or excessive redundancy) without
	 creating another transition of the same type.  */
      *next_s1 = new_trans->to;
      *next_s2 = s2;
      *next_exclude = &new_trans->labels;
    }
  return true;
}

/* Make progress in merging S2 into S1, given that each state in S2
   has a single decision.  If EXCLUDE is nonnull, avoid creating a new
   transition with the same test as S2's decision and with the labels
   in *EXCLUDE.

   Return true if there is still work to do.  When returning true,
   set *NEXT_S1, *NEXT_S2 and *NEXT_EXCLUDE to the values that
   S1, S2 and EXCLUDE should have next time round.

   If S1 and S2 both match a particular rtx, give priority to S1.  */

static bool
merge_into_state_1 (state *s1, state *s2, const int_set *exclude,
		    state **next_s1, state **next_s2,
		    const int_set **next_exclude)
{
  decision *d2 = s2->singleton ();
  if (decision *d1 = s1->last)
    {
      if (d1->test.terminal_p ())
	/* D1 is an unconditional return, so S2 can never match.  This can
	   sometimes be a bug in the .md description, but might also happen
	   if genconditions forces some conditions to true for certain
	   configurations.  */
	return false;

      /* Go backwards through the decisions in S1, stopping once we find one
	 that could match the same thing as S2.  */
      while (d1->prev && mutually_exclusive_p (d1, d2))
	d1 = d1->prev;

      /* Search forwards from that point, merging D2 into the first
	 decision we can.  */
      for (; d1; d1 = d1->next)
	{
	  /* If S2 performs some optional tests before testing the same thing
	     as D1, those tests do not help to distinguish D1 and S2, so it's
	     better to drop them.  Search through such optional decisions
	     until we find something that tests the same thing as D1.  */
	  state *sub_s2 = s2;
	  for (;;)
	    {
	      decision *sub_d2 = sub_s2->singleton ();
	      if (d1->test == sub_d2->test)
		{
		  /* Only apply EXCLUDE if we're testing the same thing
		     as D2.  */
		  const int_set *sub_exclude = (d2 == sub_d2 ? exclude : 0);

		  /* Try to merge SUB_S2 into D1.  This can only fail if
		     it would involve creating a new transition with
		     labels SUB_EXCLUDE.  */
		  if (merge_into_decision (d1, sub_s2, sub_exclude,
					   next_s1, next_s2, next_exclude))
		    return *next_s2 != 0;

		  /* Can't merge with D1; try a later decision.  */
		  break;
		}
	      transition *sub_trans2 = sub_d2->singleton ();
	      if (!sub_trans2->optional)
		/* Can't merge with D1; try a later decision.  */
		break;
	      sub_s2 = sub_trans2->to;
	    }
	}
    }

  /* We can't merge D2 with any existing decision.  Just add it to the end.  */
  s1->push_back (s2->release ());
  return false;
}

/* Merge S2 into S1.  If they both match a particular rtx, give
   priority to S1.  Each state in S2 has a single decision.  */

static void
merge_into_state (state *s1, state *s2)
{
  const int_set *exclude = 0;
  while (s2 && merge_into_state_1 (s1, s2, exclude, &s1, &s2, &exclude))
    continue;
}

/* Pairs a pattern that needs to be matched with the rtx position at
   which the pattern should occur.  */
struct pattern_pos {
  pattern_pos () {}
  pattern_pos (rtx, position *);

  rtx pattern;
  position *pos;
};

pattern_pos::pattern_pos (rtx pattern_in, position *pos_in)
  : pattern (pattern_in), pos (pos_in)
{}

/* Compare entries according to their depth-first order.  There shouldn't
   be two entries at the same position.  */

bool
operator < (const pattern_pos &e1, const pattern_pos &e2)
{
  int diff = compare_positions (e1.pos, e2.pos);
  gcc_assert (diff != 0 || e1.pattern == e2.pattern);
  return diff < 0;
}

/* Add new decisions to S that check whether the rtx at position POS
   matches PATTERN.  Return the state that is reached in that case.
   TOP_PATTERN is the overall pattern, as passed to match_pattern_1.  */

static state *
match_pattern_2 (state *s, md_rtx_info *info, position *pos, rtx pattern)
{
  auto_vec <pattern_pos, 32> worklist;
  auto_vec <pattern_pos, 32> pred_and_mode_tests;
  auto_vec <pattern_pos, 32> dup_tests;

  worklist.safe_push (pattern_pos (pattern, pos));
  while (!worklist.is_empty ())
    {
      pattern_pos next = worklist.pop ();
      pattern = next.pattern;
      pos = next.pos;
      unsigned int reverse_s = worklist.length ();

      enum rtx_code code = GET_CODE (pattern);
      switch (code)
	{
	case MATCH_OP_DUP:
	case MATCH_DUP:
	case MATCH_PAR_DUP:
	  /* Add a test that the rtx matches the earlier one, but only
	     after the structure and predicates have been checked.  */
	  dup_tests.safe_push (pattern_pos (pattern, pos));

	  /* Use the same code check as the original operand.  */
	  pattern = find_operand (info->def, XINT (pattern, 0), NULL_RTX);
	  /* Fall through.  */

	case MATCH_PARALLEL:
	case MATCH_OPERAND:
	case MATCH_SCRATCH:
	case MATCH_OPERATOR:
	  {
	    const char *pred_name = predicate_name (pattern);
	    const struct pred_data *pred = 0;
	    if (pred_name[0] != 0)
	      {
		pred = lookup_predicate (pred_name);
		/* Only report errors once per rtx.  */
		if (code == GET_CODE (pattern))
		  {
		    if (!pred)
		      error_at (info->loc, "unknown predicate '%s' used in %s",
				pred_name, GET_RTX_NAME (code));
		    else if (code == MATCH_PARALLEL
			     && pred->singleton != PARALLEL)
		      error_at (info->loc, "predicate '%s' used in"
				" match_parallel does not allow only PARALLEL",
				pred->name);
		  }
	      }

	    if (code == MATCH_PARALLEL || code == MATCH_PAR_DUP)
	      {
		/* Check that we have a parallel with enough elements.  */
		s = add_decision (s, rtx_test::code (pos), PARALLEL, false);
		int min_len = XVECLEN (pattern, 2);
		s = add_decision (s, rtx_test::veclen_ge (pos, min_len),
				  true, false);
	      }
	    else
	      {
		/* Check that the rtx has one of codes accepted by the
		   predicate.  This is necessary when matching suboperands
		   of a MATCH_OPERATOR or MATCH_OP_DUP, since we can't
		   call XEXP (X, N) without checking that X has at least
		   N+1 operands.  */
		int_set codes;
		get_predicate_codes (pred, &codes);
		bool need_codes = (pred
				   && (code == MATCH_OPERATOR
				       || code == MATCH_OP_DUP));
		s = add_decision (s, rtx_test::code (pos), codes, !need_codes);
	      }

	    /* Postpone the predicate check until we've checked the rest
	       of the rtx structure.  */
	    if (code == GET_CODE (pattern))
	      pred_and_mode_tests.safe_push (pattern_pos (pattern, pos));

	    /* If we need to match suboperands, add them to the worklist.  */
	    if (code == MATCH_OPERATOR || code == MATCH_PARALLEL)
	      {
		position **subpos_ptr;
		enum position_type pos_type;
		int i;
		if (code == MATCH_OPERATOR || code == MATCH_OP_DUP)
		  {
		    pos_type = POS_XEXP;
		    subpos_ptr = &pos->xexps;
		    i = (code == MATCH_OPERATOR ? 2 : 1);
		  }
		else
		  {
		    pos_type = POS_XVECEXP0;
		    subpos_ptr = &pos->xvecexp0s;
		    i = 2;
		  }
		for (int j = 0; j < XVECLEN (pattern, i); ++j)
		  {
		    position *subpos = next_position (subpos_ptr, pos,
						      pos_type, j);
		    worklist.safe_push (pattern_pos (XVECEXP (pattern, i, j),
					       subpos));
		    subpos_ptr = &subpos->next;
		  }
	      }
	    break;
	  }

	default:
	  {
	    /* Check that the rtx has the right code.  */
	    s = add_decision (s, rtx_test::code (pos), code, false);

	    /* Queue a test for the mode if one is specified.  */
	    if (GET_MODE (pattern) != VOIDmode)
	      pred_and_mode_tests.safe_push (pattern_pos (pattern, pos));

	    /* Push subrtxes onto the worklist.  Match nonrtx operands now.  */
	    const char *fmt = GET_RTX_FORMAT (code);
	    position **subpos_ptr = &pos->xexps;
	    for (size_t i = 0; fmt[i]; ++i)
	      {
		position *subpos = next_position (subpos_ptr, pos,
						  POS_XEXP, i);
		switch (fmt[i])
		  {
		  case 'e': case 'u':
		    worklist.safe_push (pattern_pos (XEXP (pattern, i),
						     subpos));
		    break;

		  case 'E':
		    {
		      /* Make sure the vector has the right number of
			 elements.  */
		      int length = XVECLEN (pattern, i);
		      s = add_decision (s, rtx_test::veclen (pos),
					length, false);

		      position **subpos2_ptr = &pos->xvecexp0s;
		      for (int j = 0; j < length; j++)
			{
			  position *subpos2 = next_position (subpos2_ptr, pos,
							     POS_XVECEXP0, j);
			  rtx x = XVECEXP (pattern, i, j);
			  worklist.safe_push (pattern_pos (x, subpos2));
			  subpos2_ptr = &subpos2->next;
			}
		      break;
		    }

		  case 'i':
		    /* Make sure that XINT (X, I) has the right value.  */
		    s = add_decision (s, rtx_test::int_field (pos, i),
				      XINT (pattern, i), false);
		    break;

		  case 'r':
		    /* Make sure that REGNO (X) has the right value.  */
		    gcc_assert (i == 0);
		    s = add_decision (s, rtx_test::regno_field (pos),
				      REGNO (pattern), false);
		    break;

		  case 'w':
		    /* Make sure that XWINT (X, I) has the right value.  */
		    s = add_decision (s, rtx_test::wide_int_field (pos, i),
				      XWINT (pattern, 0), false);
		    break;

		  case '0':
		    break;

		  default:
		    gcc_unreachable ();
		  }
		subpos_ptr = &subpos->next;
	      }
	  }
	  break;
	}
      /* Operands are pushed onto the worklist so that later indices are
	 nearer the top.  That's what we want for SETs, since a SET_SRC
	 is a better discriminator than a SET_DEST.  In other cases it's
	 usually better to match earlier indices first.  This is especially
	 true of PARALLELs, where the first element tends to be the most
	 individual.  It's also true for commutative operators, where the
	 canonicalization rules say that the more complex operand should
	 come first.  */
      if (code != SET && worklist.length () > reverse_s)
	std::reverse (&worklist[0] + reverse_s,
		      &worklist[0] + worklist.length ());
    }

  /* Sort the predicate and mode tests so that they're in depth-first order.
     The main goal of this is to put SET_SRC match_operands after SET_DEST
     match_operands and after mode checks for the enclosing SET_SRC operators
     (such as the mode of a PLUS in an addition instruction).  The latter
     two types of test can determine the mode exactly, whereas a SET_SRC
     match_operand often has to cope with the possibility of the operand
     being a modeless constant integer.  E.g. something that matches
     register_operand (x, SImode) never matches register_operand (x, DImode),
     but a const_int that matches immediate_operand (x, SImode) also matches
     immediate_operand (x, DImode).  The register_operand cases can therefore
     be distinguished by a switch on the mode, but the immediate_operand
     cases can't.  */
  if (pred_and_mode_tests.length () > 1)
    std::sort (&pred_and_mode_tests[0],
	       &pred_and_mode_tests[0] + pred_and_mode_tests.length ());

  /* Add the mode and predicate tests.  */
  pattern_pos *e;
  unsigned int i;
  FOR_EACH_VEC_ELT (pred_and_mode_tests, i, e)
    {
      switch (GET_CODE (e->pattern))
	{
	case MATCH_PARALLEL:
	case MATCH_OPERAND:
	case MATCH_SCRATCH:
	case MATCH_OPERATOR:
	  {
	    int opno = XINT (e->pattern, 0);
	    num_operands = MAX (num_operands, opno + 1);
	    const char *pred_name = predicate_name (e->pattern);
	    if (pred_name[0])
	      {
		const struct pred_data *pred = lookup_predicate (pred_name);
		/* Check the mode first, to distinguish things like SImode
		   and DImode register_operands, as described above.  */
		machine_mode mode = GET_MODE (e->pattern);
		if (pred && safe_predicate_mode (pred, mode))
		  s = add_decision (s, rtx_test::mode (e->pos), mode, true);

		/* Assign to operands[] first, so that the rtx usually doesn't
		   need to be live across the call to the predicate.

		   This shouldn't cause a problem with dirtying the page,
		   since we fully expect to assign to operands[] at some point,
		   and since the caller usually writes to other parts of
		   recog_data anyway.  */
		s = add_decision (s, rtx_test::set_op (e->pos, opno),
				  true, false);
		s = add_decision (s, rtx_test::predicate (e->pos, pred, mode),
				  true, false);
	      }
	    else
	      /* Historically we've ignored the mode when there's no
		 predicate.  Just set up operands[] unconditionally.  */
	      s = add_decision (s, rtx_test::set_op (e->pos, opno),
				true, false);
	    break;
	  }

	default:
	  s = add_decision (s, rtx_test::mode (e->pos),
			    GET_MODE (e->pattern), false);
	  break;
	}
    }

  /* Finally add rtx_equal_p checks for duplicated operands.  */
  FOR_EACH_VEC_ELT (dup_tests, i, e)
    s = add_decision (s, rtx_test::duplicate (e->pos, XINT (e->pattern, 0)),
		      true, false);
  return s;
}

/* Add new decisions to S that make it return ACCEPTANCE if:

   (1) the rtx doesn't match anything already matched by S
   (2) the rtx matches TOP_PATTERN and
   (3) the C test required by INFO->def is true

   For peephole2, TOP_PATTERN is a SEQUENCE of the instruction patterns
   to match, otherwise it is a single instruction pattern.  */

static void
match_pattern_1 (state *s, md_rtx_info *info, rtx pattern,
		 acceptance_type acceptance)
{
  if (acceptance.type == PEEPHOLE2)
    {
      /* Match each individual instruction.  */
      position **subpos_ptr = &peep2_insn_pos_list;
      int count = 0;
      for (int i = 0; i < XVECLEN (pattern, 0); ++i)
	{
	  rtx x = XVECEXP (pattern, 0, i);
	  position *subpos = next_position (subpos_ptr, &root_pos,
					    POS_PEEP2_INSN, count);
	  if (count > 0)
	    s = add_decision (s, rtx_test::peep2_count (count + 1),
			      true, false);
	  s = match_pattern_2 (s, info, subpos, x);
	  subpos_ptr = &subpos->next;
	  count += 1;
	}
      acceptance.u.full.u.match_len = count - 1;
    }
  else
    {
      /* Make the rtx itself.  */
      s = match_pattern_2 (s, info, &root_pos, pattern);

      /* If the match is only valid when extra clobbers are added,
	 make sure we're able to pass that information to the caller.  */
      if (acceptance.type == RECOG && acceptance.u.full.u.num_clobbers)
	s = add_decision (s, rtx_test::have_num_clobbers (), true, false);
    }

  /* Make sure that the C test is true.  */
  const char *c_test = get_c_test (info->def);
  if (maybe_eval_c_test (c_test) != 1)
    s = add_decision (s, rtx_test::c_test (c_test), true, false);

  /* Accept the pattern.  */
  add_decision (s, rtx_test::accept (acceptance), true, false);
}

/* Like match_pattern_1, but (if merge_states_p) try to merge the
   decisions with what's already in S, to reduce the amount of
   backtracking.  */

static void
match_pattern (state *s, md_rtx_info *info, rtx pattern,
	       acceptance_type acceptance)
{
  if (merge_states_p)
    {
      state root;
      /* Add the decisions to a fresh state and then merge the full tree
	 into the existing one.  */
      match_pattern_1 (&root, info, pattern, acceptance);
      merge_into_state (s, &root);
    }
  else
    match_pattern_1 (s, info, pattern, acceptance);
}

/* Begin the output file.  */

static void
write_header (void)
{
  puts ("\
/* Generated automatically by the program `genrecog' from the target\n\
   machine description file.  */\n\
\n\
#include \"config.h\"\n\
#include \"system.h\"\n\
#include \"coretypes.h\"\n\
#include \"backend.h\"\n\
#include \"predict.h\"\n\
#include \"rtl.h\"\n\
#include \"tm_p.h\"\n\
#include \"emit-rtl.h\"\n\
#include \"insn-config.h\"\n\
#include \"recog.h\"\n\
#include \"output.h\"\n\
#include \"flags.h\"\n\
#include \"df.h\"\n\
#include \"resource.h\"\n\
#include \"diagnostic-core.h\"\n\
#include \"reload.h\"\n\
#include \"regs.h\"\n\
#include \"memmodel.h\"\n\
#include \"tm-constrs.h\"\n\
\n");

  puts ("\n\
/* `recog' contains a decision tree that recognizes whether the rtx\n\
   X0 is a valid instruction.\n\
\n\
   recog returns -1 if the rtx is not valid.  If the rtx is valid, recog\n\
   returns a nonnegative number which is the insn code number for the\n\
   pattern that matched.  This is the same as the order in the machine\n\
   description of the entry that matched.  This number can be used as an\n\
   index into `insn_data' and other tables.\n");
  puts ("\
   The third parameter to recog is an optional pointer to an int.  If\n\
   present, recog will accept a pattern if it matches except for missing\n\
   CLOBBER expressions at the end.  In that case, the value pointed to by\n\
   the optional pointer will be set to the number of CLOBBERs that need\n\
   to be added (it should be initialized to zero by the caller).  If it");
  puts ("\
   is set nonzero, the caller should allocate a PARALLEL of the\n\
   appropriate size, copy the initial entries, and call add_clobbers\n\
   (found in insn-emit.c) to fill in the CLOBBERs.\n\
");

  puts ("\n\
   The function split_insns returns 0 if the rtl could not\n\
   be split or the split rtl as an INSN list if it can be.\n\
\n\
   The function peephole2_insns returns 0 if the rtl could not\n\
   be matched. If there was a match, the new rtl is returned in an INSN list,\n\
   and LAST_INSN will point to the last recognized insn in the old sequence.\n\
*/\n\n");
}

/* Return the C type of a parameter with type TYPE.  */

static const char *
parameter_type_string (parameter::type_enum type)
{
  switch (type)
    {
    case parameter::UNSET:
      break;

    case parameter::CODE:
      return "rtx_code";

    case parameter::MODE:
      return "machine_mode";

    case parameter::INT:
      return "int";

    case parameter::UINT:
      return "unsigned int";

    case parameter::WIDE_INT:
      return "HOST_WIDE_INT";
    }
  gcc_unreachable ();
}

/* Return true if ACCEPTANCE requires only a single C statement even in
   a backtracking context.  */

static bool
single_statement_p (const acceptance_type &acceptance)
{
  if (acceptance.partial_p)
    /* We need to handle failures of the subroutine.  */
    return false;
  switch (acceptance.type)
    {
    case SUBPATTERN:
    case SPLIT:
      return true;

    case RECOG:
      /* False if we need to assign to pnum_clobbers.  */
      return acceptance.u.full.u.num_clobbers == 0;

    case PEEPHOLE2:
      /* We need to assign to pmatch_len_ and handle null returns from the
	 peephole2 routine.  */
      return false;
    }
  gcc_unreachable ();
}

/* Return the C failure value for a routine of type TYPE.  */

static const char *
get_failure_return (routine_type type)
{
  switch (type)
    {
    case SUBPATTERN:
    case RECOG:
      return "-1";

    case SPLIT:
    case PEEPHOLE2:
      return "NULL";
    }
  gcc_unreachable ();
}

/* Indicates whether a block of code always returns or whether it can fall
   through.  */

enum exit_state {
  ES_RETURNED,
  ES_FALLTHROUGH
};

/* Information used while writing out code.  */

struct output_state
{
  /* The type of routine that we're generating.  */
  routine_type type;

  /* Maps position ids to xN variable numbers.  The entry is only valid if
     it is less than the length of VAR_TO_ID, but this holds for every position
     tested by a state when writing out that state.  */
  auto_vec <unsigned int> id_to_var;

  /* Maps xN variable numbers to position ids.  */
  auto_vec <unsigned int> var_to_id;

  /* Index N is true if variable xN has already been set.  */
  auto_vec <bool> seen_vars;
};

/* Return true if D is a call to a pattern routine and if there is some X
   such that the transition for pattern result N goes to a successful return
   with code X+N.  When returning true, set *BASE_OUT to this X and *COUNT_OUT
   to the number of return values.  (We know that every PATTERN decision has
   a transition for every successful return.)  */

static bool
terminal_pattern_p (decision *d, unsigned int *base_out,
		    unsigned int *count_out)
{
  if (d->test.kind != rtx_test::PATTERN)
    return false;
  unsigned int base = 0;
  unsigned int count = 0;
  for (transition *trans = d->first; trans; trans = trans->next)
    {
      if (trans->is_param || trans->labels.length () != 1)
	return false;
      decision *subd = trans->to->singleton ();
      if (!subd || subd->test.kind != rtx_test::ACCEPT)
	return false;
      unsigned int this_base = (subd->test.u.acceptance.u.full.code
				- trans->labels[0]);
      if (trans == d->first)
	base = this_base;
      else if (base != this_base)
	return false;
      count += 1;
    }
  *base_out = base;
  *count_out = count;
  return true;
}

/* Return true if TEST doesn't test an rtx or if the rtx it tests is
   already available in state OS.  */

static bool
test_position_available_p (output_state *os, const rtx_test &test)
{
  return (!test.pos
	  || test.pos_operand >= 0
	  || os->seen_vars[os->id_to_var[test.pos->id]]);
}

/* Like printf, but print INDENT spaces at the beginning.  */

static void ATTRIBUTE_PRINTF_2
printf_indent (unsigned int indent, const char *format, ...)
{
  va_list ap;
  va_start (ap, format);
  printf ("%*s", indent, "");
  vprintf (format, ap);
  va_end (ap);
}

/* Emit code to initialize the variable associated with POS, if it isn't
   already valid in state OS.  Indent each line by INDENT spaces.  Update
   OS with the new state.  */

static void
change_state (output_state *os, position *pos, unsigned int indent)
{
  unsigned int var = os->id_to_var[pos->id];
  gcc_assert (var < os->var_to_id.length () && os->var_to_id[var] == pos->id);
  if (os->seen_vars[var])
    return;
  switch (pos->type)
    {
    case POS_PEEP2_INSN:
      printf_indent (indent, "x%d = PATTERN (peep2_next_insn (%d));\n",
		     var, pos->arg);
      break;

    case POS_XEXP:
      change_state (os, pos->base, indent);
      printf_indent (indent, "x%d = XEXP (x%d, %d);\n",
		     var, os->id_to_var[pos->base->id], pos->arg);
      break;

    case POS_XVECEXP0:
      change_state (os, pos->base, indent);
      printf_indent (indent, "x%d = XVECEXP (x%d, 0, %d);\n",
		     var, os->id_to_var[pos->base->id], pos->arg);
      break;
    }
  os->seen_vars[var] = true;
}

/* Print the enumerator constant for CODE -- the upcase version of
   the name.  */

static void
print_code (enum rtx_code code)
{
  const char *p;
  for (p = GET_RTX_NAME (code); *p; p++)
    putchar (TOUPPER (*p));
}

/* Emit a uint64_t as an integer constant expression.  We need to take
   special care to avoid "decimal constant is so large that it is unsigned"
   warnings in the resulting code.  */

static void
print_host_wide_int (uint64_t val)
{
  uint64_t min = uint64_t (1) << (HOST_BITS_PER_WIDE_INT - 1);
  if (val == min)
    printf ("(" HOST_WIDE_INT_PRINT_DEC_C " - 1)", val + 1);
  else
    printf (HOST_WIDE_INT_PRINT_DEC_C, val);
}

/* Print the C expression for actual parameter PARAM.  */

static void
print_parameter_value (const parameter &param)
{
  if (param.is_param)
    printf ("i%d", (int) param.value + 1);
  else
    switch (param.type)
      {
      case parameter::UNSET:
	gcc_unreachable ();
	break;

      case parameter::CODE:
	print_code ((enum rtx_code) param.value);
	break;

      case parameter::MODE:
	printf ("%smode", GET_MODE_NAME ((machine_mode) param.value));
	break;

      case parameter::INT:
	printf ("%d", (int) param.value);
	break;

      case parameter::UINT:
	printf ("%u", (unsigned int) param.value);
	break;

      case parameter::WIDE_INT:
	print_host_wide_int (param.value);
	break;
      }
}

/* Print the C expression for the rtx tested by TEST.  */

static void
print_test_rtx (output_state *os, const rtx_test &test)
{
  if (test.pos_operand >= 0)
    printf ("operands[%d]", test.pos_operand);
  else
    printf ("x%d", os->id_to_var[test.pos->id]);
}

/* Print the C expression for non-boolean test TEST.  */

static void
print_nonbool_test (output_state *os, const rtx_test &test)
{
  switch (test.kind)
    {
    case rtx_test::CODE:
      printf ("GET_CODE (");
      print_test_rtx (os, test);
      printf (")");
      break;

    case rtx_test::MODE:
      printf ("GET_MODE (");
      print_test_rtx (os, test);
      printf (")");
      break;

    case rtx_test::VECLEN:
      printf ("XVECLEN (");
      print_test_rtx (os, test);
      printf (", 0)");
      break;

    case rtx_test::INT_FIELD:
      printf ("XINT (");
      print_test_rtx (os, test);
      printf (", %d)", test.u.opno);
      break;

    case rtx_test::REGNO_FIELD:
      printf ("REGNO (");
      print_test_rtx (os, test);
      printf (")");
      break;

    case rtx_test::WIDE_INT_FIELD:
      printf ("XWINT (");
      print_test_rtx (os, test);
      printf (", %d)", test.u.opno);
      break;

    case rtx_test::PATTERN:
      {
	pattern_routine *routine = test.u.pattern->routine;
	printf ("pattern%d (", routine->pattern_id);
	const char *sep = "";
	if (test.pos)
	  {
	    print_test_rtx (os, test);
	    sep = ", ";
	  }
	if (routine->insn_p)
	  {
	    printf ("%sinsn", sep);
	    sep = ", ";
	  }
	if (routine->pnum_clobbers_p)
	  {
	    printf ("%spnum_clobbers", sep);
	    sep = ", ";
	  }
	for (unsigned int i = 0; i < test.u.pattern->params.length (); ++i)
	  {
	    fputs (sep, stdout);
	    print_parameter_value (test.u.pattern->params[i]);
	    sep = ", ";
	  }
	printf (")");
	break;
      }

    case rtx_test::PEEP2_COUNT:
    case rtx_test::VECLEN_GE:
    case rtx_test::SAVED_CONST_INT:
    case rtx_test::DUPLICATE:
    case rtx_test::PREDICATE:
    case rtx_test::SET_OP:
    case rtx_test::HAVE_NUM_CLOBBERS:
    case rtx_test::C_TEST:
    case rtx_test::ACCEPT:
      gcc_unreachable ();
    }
}

/* IS_PARAM and LABEL are taken from a transition whose source
   decision performs TEST.  Print the C code for the label.  */

static void
print_label_value (const rtx_test &test, bool is_param, uint64_t value)
{
  print_parameter_value (parameter (transition_parameter_type (test.kind),
				    is_param, value));
}

/* If IS_PARAM, print code to compare TEST with the C variable i<VALUE+1>.
   If !IS_PARAM, print code to compare TEST with the C constant VALUE.
   Test for inequality if INVERT_P, otherwise test for equality.  */

static void
print_test (output_state *os, const rtx_test &test, bool is_param,
	    uint64_t value, bool invert_p)
{
  switch (test.kind)
    {
      /* Handle the non-boolean TESTs.  */
    case rtx_test::CODE:
    case rtx_test::MODE:
    case rtx_test::VECLEN:
    case rtx_test::REGNO_FIELD:
    case rtx_test::INT_FIELD:
    case rtx_test::WIDE_INT_FIELD:
    case rtx_test::PATTERN:
      print_nonbool_test (os, test);
      printf (" %s ", invert_p ? "!=" : "==");
      print_label_value (test, is_param, value);
      break;

    case rtx_test::SAVED_CONST_INT:
      gcc_assert (!is_param && value == 1);
      print_test_rtx (os, test);
      printf (" %s const_int_rtx[MAX_SAVED_CONST_INT + ",
	      invert_p ? "!=" : "==");
      print_parameter_value (parameter (parameter::INT,
					test.u.integer.is_param,
					test.u.integer.value));
      printf ("]");
      break;

    case rtx_test::PEEP2_COUNT:
      gcc_assert (!is_param && value == 1);
      printf ("peep2_current_count %s %d", invert_p ? "<" : ">=",
	      test.u.min_len);
      break;

    case rtx_test::VECLEN_GE:
      gcc_assert (!is_param && value == 1);
      printf ("XVECLEN (");
      print_test_rtx (os, test);
      printf (", 0) %s %d", invert_p ? "<" : ">=", test.u.min_len);
      break;

    case rtx_test::PREDICATE:
      gcc_assert (!is_param && value == 1);
      printf ("%s%s (", invert_p ? "!" : "", test.u.predicate.data->name);
      print_test_rtx (os, test);
      printf (", ");
      print_parameter_value (parameter (parameter::MODE,
					test.u.predicate.mode_is_param,
					test.u.predicate.mode));
      printf (")");
      break;

    case rtx_test::DUPLICATE:
      gcc_assert (!is_param && value == 1);
      printf ("%srtx_equal_p (", invert_p ? "!" : "");
      print_test_rtx (os, test);
      printf (", operands[%d])", test.u.opno);
      break;

    case rtx_test::HAVE_NUM_CLOBBERS:
      gcc_assert (!is_param && value == 1);
      printf ("pnum_clobbers %s NULL", invert_p ? "==" : "!=");
      break;

    case rtx_test::C_TEST:
      gcc_assert (!is_param && value == 1);
      if (invert_p)
	printf ("!");
      print_c_condition (test.u.string);
      break;

    case rtx_test::ACCEPT:
    case rtx_test::SET_OP:
      gcc_unreachable ();
    }
}

static exit_state print_decision (output_state *, decision *,
				  unsigned int, bool);

/* Print code to perform S, indent each line by INDENT spaces.
   IS_FINAL is true if there are no fallback decisions to test on failure;
   if the state fails then the entire routine fails.  */

static exit_state
print_state (output_state *os, state *s, unsigned int indent, bool is_final)
{
  exit_state es = ES_FALLTHROUGH;
  for (decision *d = s->first; d; d = d->next)
    es = print_decision (os, d, indent, is_final && !d->next);
  if (es != ES_RETURNED && is_final)
    {
      printf_indent (indent, "return %s;\n", get_failure_return (os->type));
      es = ES_RETURNED;
    }
  return es;
}

/* Print the code for subroutine call ACCEPTANCE (for which partial_p
   is known to be true).  Return the C condition that indicates a successful
   match.  */

static const char *
print_subroutine_call (const acceptance_type &acceptance)
{
  switch (acceptance.type)
    {
    case SUBPATTERN:
      gcc_unreachable ();

    case RECOG:
      printf ("recog_%d (x1, insn, pnum_clobbers)",
	      acceptance.u.subroutine_id);
      return ">= 0";

    case SPLIT:
      printf ("split_%d (x1, insn)", acceptance.u.subroutine_id);
      return "!= NULL_RTX";

    case PEEPHOLE2:
      printf ("peephole2_%d (x1, insn, pmatch_len_)",
	      acceptance.u.subroutine_id);
      return "!= NULL_RTX";
    }
  gcc_unreachable ();
}

/* Print code for the successful match described by ACCEPTANCE.
   INDENT and IS_FINAL are as for print_state.  */

static exit_state
print_acceptance (const acceptance_type &acceptance, unsigned int indent,
		  bool is_final)
{
  if (acceptance.partial_p)
    {
      /* Defer the rest of the match to a subroutine.  */
      if (is_final)
	{
	  printf_indent (indent, "return ");
	  print_subroutine_call (acceptance);
	  printf (";\n");
	  return ES_RETURNED;
	}
      else
	{
	  printf_indent (indent, "res = ");
	  const char *res_test = print_subroutine_call (acceptance);
	  printf (";\n");
	  printf_indent (indent, "if (res %s)\n", res_test);
	  printf_indent (indent + 2, "return res;\n");
	  return ES_FALLTHROUGH;
	}
    }
  switch (acceptance.type)
    {
    case SUBPATTERN:
      printf_indent (indent, "return %d;\n", acceptance.u.full.code);
      return ES_RETURNED;

    case RECOG:
      if (acceptance.u.full.u.num_clobbers != 0)
	printf_indent (indent, "*pnum_clobbers = %d;\n",
		       acceptance.u.full.u.num_clobbers);
      printf_indent (indent, "return %d; /* %s */\n", acceptance.u.full.code,
		     get_insn_name (acceptance.u.full.code));
      return ES_RETURNED;

    case SPLIT:
      printf_indent (indent, "return gen_split_%d (insn, operands);\n",
		     acceptance.u.full.code);
      return ES_RETURNED;

    case PEEPHOLE2:
      printf_indent (indent, "*pmatch_len_ = %d;\n",
		     acceptance.u.full.u.match_len);
      if (is_final)
	{
	  printf_indent (indent, "return gen_peephole2_%d (insn, operands);\n",
			 acceptance.u.full.code);
	  return ES_RETURNED;
	}
      else
	{
	  printf_indent (indent, "res = gen_peephole2_%d (insn, operands);\n",
			 acceptance.u.full.code);
	  printf_indent (indent, "if (res != NULL_RTX)\n");
	  printf_indent (indent + 2, "return res;\n");
	  return ES_FALLTHROUGH;
	}
    }
  gcc_unreachable ();
}

/* Print code to perform D.  INDENT and IS_FINAL are as for print_state.  */

static exit_state
print_decision (output_state *os, decision *d, unsigned int indent,
		bool is_final)
{
  uint64_t label;
  unsigned int base, count;

  /* Make sure the rtx under test is available either in operands[] or
     in an xN variable.  */
  if (d->test.pos && d->test.pos_operand < 0)
    change_state (os, d->test.pos, indent);

  /* Look for cases where a pattern routine P1 calls another pattern routine
     P2 and where P1 returns X + BASE whenever P2 returns X.  If IS_FINAL
     is true and BASE is zero we can simply use:

        return patternN (...);

     Otherwise we can use:

        res = patternN (...);
	if (res >= 0)
	  return res + BASE;

     However, if BASE is nonzero and patternN only returns 0 or -1,
     the usual "return BASE;" is better than "return res + BASE;".
     If BASE is zero, "return res;" should be better than "return 0;",
     since no assignment to the return register is required.  */
  if (os->type == SUBPATTERN
      && terminal_pattern_p (d, &base, &count)
      && (base == 0 || count > 1))
    {
      if (is_final && base == 0)
	{
	  printf_indent (indent, "return ");
	  print_nonbool_test (os, d->test);
	  printf ("; /* [-1, %d] */\n", count - 1);
	  return ES_RETURNED;
	}
      else
	{
	  printf_indent (indent, "res = ");
	  print_nonbool_test (os, d->test);
	  printf (";\n");
	  printf_indent (indent, "if (res >= 0)\n");
	  printf_indent (indent + 2, "return res");
	  if (base != 0)
	    printf (" + %d", base);
	  printf ("; /* [%d, %d] */\n", base, base + count - 1);
	  return ES_FALLTHROUGH;
	}
    }
  else if (d->test.kind == rtx_test::ACCEPT)
    return print_acceptance (d->test.u.acceptance, indent, is_final);
  else if (d->test.kind == rtx_test::SET_OP)
    {
      printf_indent (indent, "operands[%d] = ", d->test.u.opno);
      print_test_rtx (os, d->test);
      printf (";\n");
      return print_state (os, d->singleton ()->to, indent, is_final);
    }
  /* Handle decisions with a single transition and a single transition
     label.  */
  else if (d->if_statement_p (&label))
    {
      transition *trans = d->singleton ();
      if (mark_optional_transitions_p && trans->optional)
	printf_indent (indent, "/* OPTIONAL IF */\n");

      /* Print the condition associated with TRANS.  Invert it if IS_FINAL,
	 so that we return immediately on failure and fall through on
	 success.  */
      printf_indent (indent, "if (");
      print_test (os, d->test, trans->is_param, label, is_final);

      /* Look for following states that would be handled by this code
	 on recursion.  If they don't need any preparatory statements,
	 include them in the current "if" statement rather than creating
	 a new one.  */
      for (;;)
	{
	  d = trans->to->singleton ();
	  if (!d
	      || d->test.kind == rtx_test::ACCEPT
	      || d->test.kind == rtx_test::SET_OP
	      || !d->if_statement_p (&label)
	      || !test_position_available_p (os, d->test))
	    break;
	  trans = d->first;
	  printf ("\n");
	  if (mark_optional_transitions_p && trans->optional)
	    printf_indent (indent + 4, "/* OPTIONAL IF */\n");
	  printf_indent (indent + 4, "%s ", is_final ? "||" : "&&");
	  print_test (os, d->test, trans->is_param, label, is_final);
	}
      printf (")\n");

      /* Print the conditional code with INDENT + 2 and the fallthrough
	 code with indent INDENT.  */
      state *to = trans->to;
      if (is_final)
	{
	  /* We inverted the condition above, so return failure in the
	     "if" body and fall through to the target of the transition.  */
	  printf_indent (indent + 2, "return %s;\n",
			 get_failure_return (os->type));
	  return print_state (os, to, indent, is_final);
	}
      else if (to->singleton ()
	       && to->first->test.kind == rtx_test::ACCEPT
	       && single_statement_p (to->first->test.u.acceptance))
	{
	  /* The target of the transition is a simple "return" statement.
	     It doesn't need any braces and doesn't fall through.  */
	  if (print_acceptance (to->first->test.u.acceptance,
				indent + 2, true) != ES_RETURNED)
	    gcc_unreachable ();
	  return ES_FALLTHROUGH;
	}
      else
	{
	  /* The general case.  Output code for the target of the transition
	     in braces.  This will not invalidate any of the xN variables
	     that are already valid, but we mustn't rely on any that are
	     set by the "if" body.  */
	  auto_vec <bool, 32> old_seen;
	  old_seen.safe_splice (os->seen_vars);

	  printf_indent (indent + 2, "{\n");
	  print_state (os, trans->to, indent + 4, is_final);
	  printf_indent (indent + 2, "}\n");

	  os->seen_vars.truncate (0);
	  os->seen_vars.splice (old_seen);
	  return ES_FALLTHROUGH;
	}
    }
  else
    {
      /* Output the decision as a switch statement.  */
      printf_indent (indent, "switch (");
      print_nonbool_test (os, d->test);
      printf (")\n");

      /* Each case statement starts with the same set of valid variables.
	 These are also the only variables will be valid on fallthrough.  */
      auto_vec <bool, 32> old_seen;
      old_seen.safe_splice (os->seen_vars);

      printf_indent (indent + 2, "{\n");
      for (transition *trans = d->first; trans; trans = trans->next)
	{
	  gcc_assert (!trans->is_param);
	  if (mark_optional_transitions_p && trans->optional)
	    printf_indent (indent + 2, "/* OPTIONAL CASE */\n");
	  for (int_set::iterator j = trans->labels.begin ();
	       j != trans->labels.end (); ++j)
	    {
	      printf_indent (indent + 2, "case ");
	      print_label_value (d->test, trans->is_param, *j);
	      printf (":\n");
	    }
	  if (print_state (os, trans->to, indent + 4, is_final))
	    {
	      /* The state can fall through.  Add an explicit break.  */
	      gcc_assert (!is_final);
	      printf_indent (indent + 4, "break;\n");
	    }
	  printf ("\n");

	  /* Restore the original set of valid variables.  */
	  os->seen_vars.truncate (0);
	  os->seen_vars.splice (old_seen);
	}
      /* Add a default case.  */
      printf_indent (indent + 2, "default:\n");
      if (is_final)
	printf_indent (indent + 4, "return %s;\n",
		       get_failure_return (os->type));
      else
	printf_indent (indent + 4, "break;\n");
      printf_indent (indent + 2, "}\n");
      return is_final ? ES_RETURNED : ES_FALLTHROUGH;
    }
}

/* Make sure that OS has a position variable for POS.  ROOT_P is true if
   POS is the root position for the routine.  */

static void
assign_position_var (output_state *os, position *pos, bool root_p)
{
  unsigned int idx = os->id_to_var[pos->id];
  if (idx < os->var_to_id.length () && os->var_to_id[idx] == pos->id)
    return;
  if (!root_p && pos->type != POS_PEEP2_INSN)
    assign_position_var (os, pos->base, false);
  os->id_to_var[pos->id] = os->var_to_id.length ();
  os->var_to_id.safe_push (pos->id);
}

/* Make sure that OS has the position variables required by S.  */

static void
assign_position_vars (output_state *os, state *s)
{
  for (decision *d = s->first; d; d = d->next)
    {
      /* Positions associated with operands can be read from the
	 operands[] array.  */
      if (d->test.pos && d->test.pos_operand < 0)
	assign_position_var (os, d->test.pos, false);
      for (transition *trans = d->first; trans; trans = trans->next)
	assign_position_vars (os, trans->to);
    }
}

/* Print the open brace and variable definitions for a routine that
   implements S.  ROOT is the deepest rtx from which S can access all
   relevant parts of the first instruction it matches.  Initialize OS
   so that every relevant position has an rtx variable xN and so that
   only ROOT's variable has a valid value.  */

static void
print_subroutine_start (output_state *os, state *s, position *root)
{
  printf ("{\n  rtx * const operands ATTRIBUTE_UNUSED"
	  " = &recog_data.operand[0];\n");
  os->var_to_id.truncate (0);
  os->seen_vars.truncate (0);
  if (root)
    {
      /* Create a fake entry for position 0 so that an id_to_var of 0
	 is always invalid.  This also makes the xN variables naturally
	 1-based rather than 0-based.  */
      os->var_to_id.safe_push (num_positions);

      /* Associate ROOT with x1.  */
      assign_position_var (os, root, true);

      /* Assign xN variables to all other relevant positions.  */
      assign_position_vars (os, s);

      /* Output the variable declarations (except for ROOT's, which is
	 passed in as a parameter).  */
      unsigned int num_vars = os->var_to_id.length ();
      if (num_vars > 2)
	{
	  for (unsigned int i = 2; i < num_vars; ++i)
	    /* Print 8 rtx variables to a line.  */
	    printf ("%s x%d",
		    i == 2 ? "  rtx" : (i - 2) % 8 == 0 ? ";\n  rtx" : ",", i);
	  printf (";\n");
	}

      /* Say that x1 is valid and the rest aren't.  */
      os->seen_vars.safe_grow_cleared (num_vars);
      os->seen_vars[1] = true;
    }
  if (os->type == SUBPATTERN || os->type == RECOG)
    printf ("  int res ATTRIBUTE_UNUSED;\n");
  else
    printf ("  rtx_insn *res ATTRIBUTE_UNUSED;\n");
}

/* Output the definition of pattern routine ROUTINE.  */

static void
print_pattern (output_state *os, pattern_routine *routine)
{
  printf ("\nstatic int\npattern%d (", routine->pattern_id);
  const char *sep = "";
  /* Add the top-level rtx parameter, if any.  */
  if (routine->pos)
    {
      printf ("%srtx x1", sep);
      sep = ", ";
    }
  /* Add the optional parameters.  */
  if (routine->insn_p)
    {
      /* We can't easily tell whether a C condition actually reads INSN,
	 so add an ATTRIBUTE_UNUSED just in case.  */
      printf ("%srtx_insn *insn ATTRIBUTE_UNUSED", sep);
      sep = ", ";
    }
  if (routine->pnum_clobbers_p)
    {
      printf ("%sint *pnum_clobbers", sep);
      sep = ", ";
    }
  /* Add the "i" parameters.  */
  for (unsigned int i = 0; i < routine->param_types.length (); ++i)
    {
      printf ("%s%s i%d", sep,
	      parameter_type_string (routine->param_types[i]), i + 1);
      sep = ", ";
    }
  printf (")\n");
  os->type = SUBPATTERN;
  print_subroutine_start (os, routine->s, routine->pos);
  print_state (os, routine->s, 2, true);
  printf ("}\n");
}

/* Output a routine of type TYPE that implements S.  PROC_ID is the
   number of the subroutine associated with S, or 0 if S is the main
   routine.  */

static void
print_subroutine (output_state *os, state *s, int proc_id)
{
  /* For now, the top-level "recog" takes a plain "rtx", and performs a
     checked cast to "rtx_insn *" for use throughout the rest of the
     function and the code it calls.  */
  const char *insn_param
    = proc_id > 0 ? "rtx_insn *insn" : "rtx uncast_insn";
  printf ("\n");
  switch (os->type)
    {
    case SUBPATTERN:
      gcc_unreachable ();

    case RECOG:
      if (proc_id)
	printf ("static int\nrecog_%d", proc_id);
      else
	printf ("int\nrecog");
      printf (" (rtx x1 ATTRIBUTE_UNUSED,\n"
	      "\t%s ATTRIBUTE_UNUSED,\n"
	      "\tint *pnum_clobbers ATTRIBUTE_UNUSED)\n", insn_param);
      break;

    case SPLIT:
      if (proc_id)
	printf ("static rtx_insn *\nsplit_%d", proc_id);
      else
	printf ("rtx_insn *\nsplit_insns");
      printf (" (rtx x1 ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)\n");
      break;

    case PEEPHOLE2:
      if (proc_id)
	printf ("static rtx_insn *\npeephole2_%d", proc_id);
      else
	printf ("rtx_insn *\npeephole2_insns");
      printf (" (rtx x1 ATTRIBUTE_UNUSED,\n"
	      "\trtx_insn *insn ATTRIBUTE_UNUSED,\n"
	      "\tint *pmatch_len_ ATTRIBUTE_UNUSED)\n");
      break;
    }
  print_subroutine_start (os, s, &root_pos);
  if (proc_id == 0)
    {
      printf ("  recog_data.insn = NULL;\n");
      if (os->type == RECOG)
	{
	  printf ("  rtx_insn *insn ATTRIBUTE_UNUSED;\n");
	  printf ("  insn = safe_as_a <rtx_insn *> (uncast_insn);\n");
	}
    }
  print_state (os, s, 2, true);
  printf ("}\n");
}

/* Print out a routine of type TYPE that performs ROOT.  */

static void
print_subroutine_group (output_state *os, routine_type type, state *root)
{
  os->type = type;
  if (use_subroutines_p)
    {
      /* Split ROOT up into smaller pieces, both for readability and to
	 help the compiler.  */
      auto_vec <state *> subroutines;
      find_subroutines (type, root, subroutines);

      /* Output the subroutines (but not ROOT itself).  */
      unsigned int i;
      state *s;
      FOR_EACH_VEC_ELT (subroutines, i, s)
	print_subroutine (os, s, i + 1);
    }
  /* Output the main routine.  */
  print_subroutine (os, root, 0);
}

/* Return the rtx pattern for the list of rtxes in a define_peephole2.  */

static rtx
get_peephole2_pattern (md_rtx_info *info)
{
  int i, j;
  rtvec vec = XVEC (info->def, 0);
  rtx pattern = rtx_alloc (SEQUENCE);
  XVEC (pattern, 0) = rtvec_alloc (GET_NUM_ELEM (vec));
  for (i = j = 0; i < GET_NUM_ELEM (vec); i++)
    {
      rtx x = RTVEC_ELT (vec, i);
      /* Ignore scratch register requirements.  */
      if (GET_CODE (x) != MATCH_SCRATCH && GET_CODE (x) != MATCH_DUP)
	{
	  XVECEXP (pattern, 0, j) = x;
	  j++;
	}
    }
  XVECLEN (pattern, 0) = j;
  if (j == 0)
    error_at (info->loc, "empty define_peephole2");
  return pattern;
}

/* Return true if *PATTERN_PTR is a PARALLEL in which at least one trailing
   rtx can be added automatically by add_clobbers.  If so, update
   *ACCEPTANCE_PTR so that its num_clobbers field contains the number
   of such trailing rtxes and update *PATTERN_PTR so that it contains
   the pattern without those rtxes.  */

static bool
remove_clobbers (acceptance_type *acceptance_ptr, rtx *pattern_ptr)
{
  int i;
  rtx new_pattern;

  /* Find the last non-clobber in the parallel.  */
  rtx pattern = *pattern_ptr;
  for (i = XVECLEN (pattern, 0); i > 0; i--)
    {
      rtx x = XVECEXP (pattern, 0, i - 1);
      if (GET_CODE (x) != CLOBBER
	  || (!REG_P (XEXP (x, 0))
	      && GET_CODE (XEXP (x, 0)) != MATCH_SCRATCH))
	break;
    }

  if (i == XVECLEN (pattern, 0))
    return false;

  /* Build a similar insn without the clobbers.  */
  if (i == 1)
    new_pattern = XVECEXP (pattern, 0, 0);
  else
    {
      new_pattern = rtx_alloc (PARALLEL);
      XVEC (new_pattern, 0) = rtvec_alloc (i);
      for (int j = 0; j < i; ++j)
	XVECEXP (new_pattern, 0, j) = XVECEXP (pattern, 0, j);
    }

  /* Recognize it.  */
  acceptance_ptr->u.full.u.num_clobbers = XVECLEN (pattern, 0) - i;
  *pattern_ptr = new_pattern;
  return true;
}

int
main (int argc, const char **argv)
{
  state insn_root, split_root, peephole2_root;

  progname = "genrecog";

  if (!init_rtx_reader_args (argc, argv))
    return (FATAL_EXIT_CODE);

  write_header ();

  /* Read the machine description.  */

  md_rtx_info info;
  while (read_md_rtx (&info))
    {
      rtx def = info.def;

      acceptance_type acceptance;
      acceptance.partial_p = false;
      acceptance.u.full.code = info.index;

      rtx pattern;
      switch (GET_CODE (def))
	{
	case DEFINE_INSN:
	  {
	    /* Match the instruction in the original .md form.  */
	    acceptance.type = RECOG;
	    acceptance.u.full.u.num_clobbers = 0;
	    pattern = add_implicit_parallel (XVEC (def, 1));
	    validate_pattern (pattern, &info, NULL_RTX, 0);
	    match_pattern (&insn_root, &info, pattern, acceptance);

	    /* If the pattern is a PARALLEL with trailing CLOBBERs,
	       allow recog_for_combine to match without the clobbers.  */
	    if (GET_CODE (pattern) == PARALLEL
		&& remove_clobbers (&acceptance, &pattern))
	      match_pattern (&insn_root, &info, pattern, acceptance);
	    break;
	  }

	case DEFINE_SPLIT:
	  acceptance.type = SPLIT;
	  pattern = add_implicit_parallel (XVEC (def, 0));
	  validate_pattern (pattern, &info, NULL_RTX, 0);
	  match_pattern (&split_root, &info, pattern, acceptance);

	  /* Declare the gen_split routine that we'll call if the
	     pattern matches.  The definition comes from insn-emit.c.  */
	  printf ("extern rtx_insn *gen_split_%d (rtx_insn *, rtx *);\n",
		  info.index);
	  break;

	case DEFINE_PEEPHOLE2:
	  acceptance.type = PEEPHOLE2;
	  pattern = get_peephole2_pattern (&info);
	  validate_pattern (pattern, &info, NULL_RTX, 0);
	  match_pattern (&peephole2_root, &info, pattern, acceptance);

	  /* Declare the gen_peephole2 routine that we'll call if the
	     pattern matches.  The definition comes from insn-emit.c.  */
	  printf ("extern rtx_insn *gen_peephole2_%d (rtx_insn *, rtx *);\n",
		  info.index);
	  break;

	default:
	  /* do nothing */;
	}
    }

  if (have_error)
    return FATAL_EXIT_CODE;

  puts ("\n\n");

  /* Optimize each routine in turn.  */
  optimize_subroutine_group ("recog", &insn_root);
  optimize_subroutine_group ("split_insns", &split_root);
  optimize_subroutine_group ("peephole2_insns", &peephole2_root);

  output_state os;
  os.id_to_var.safe_grow_cleared (num_positions);

  if (use_pattern_routines_p)
    {
      /* Look for common patterns and split them out into subroutines.  */
      auto_vec <merge_state_info> states;
      states.safe_push (&insn_root);
      states.safe_push (&split_root);
      states.safe_push (&peephole2_root);
      split_out_patterns (states);

      /* Print out the routines that we just created.  */
      unsigned int i;
      pattern_routine *routine;
      FOR_EACH_VEC_ELT (patterns, i, routine)
	print_pattern (&os, routine);
    }

  /* Print out the matching routines.  */
  print_subroutine_group (&os, RECOG, &insn_root);
  print_subroutine_group (&os, SPLIT, &split_root);
  print_subroutine_group (&os, PEEPHOLE2, &peephole2_root);

  fflush (stdout);
  return (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}
