/* Code to test for "definitive assignment".

   Copyright (C) 1999, 2000  Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com>, January 1999. */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "java-tree.h"
#include "toplev.h" /* Needed for fatal. */

/* The basic idea is that we assign each local variable declaration
   an index, and then we pass around bitstrings, where the i'th bit
   is set if decl whose DECL_BIT_INDEX is i is definitely assigned. */

/* One segment of a bitstring. */
typedef unsigned int word;

/* Pointer to a bitstring. */
typedef word *words;

/* For a local VAR_DECL, holds the index into a words bitstring that
   specifies if this decl is definitively assigned.
   A DECL_BIT_INDEX of -1 means we no longer care. */
#define DECL_BIT_INDEX(DECL) DECL_FIELD_SIZE(DECL)

/* Number of locals variables currently active. */
int num_current_locals = 0;

/* The index of the first local variable in the current block.

   The variables whose DECL_BIT_INDEX are in the range from
   start_current_locals (inclusive) up to num_current_locals (exclusive)
   are declared in the "current" block.  If there is a loop or branch
   form, we set start_current_locals to num_current_locals to indicate
   there is no current block.

   The point is that if a variable in the current block is set,
   there are no other control paths that we have to worry about.
   Hence, we can remove it from the set of variables we are
   checking, making its bit index available for some other variable.
   For simplicity, we only do that if the variable's bit index
   is (num_current_locals-1);  freeing up its bit index is then
   just a simple matter of decrementing num_current_locals.
   The reason this is worth doing is that it is simple, and
   allows us to use short (usually one-word) bit-strings,
   even for methods with thousands of local variables, as
   long as most of them are initialized immediately after or in
   their declaration. */
int start_current_locals = 0;

int num_current_words = 1;

static tree wfl;

#define COPYN(DST, SRC, NWORDS) memcpy (DST, SRC, NWORDS * sizeof(word))
#define COPY(DST, SRC) COPYN (DST, SRC, num_current_words)

#define SET_ALL(DST) memset (DST, ~0, num_current_words * sizeof(word))
#define CLEAR_ALL(DST) memset (DST, 0, num_current_words * sizeof(word))

#define INTERSECTN(DST, SRC1, SRC2, N) \
  do { int n = N; \
  while (--n >= 0) DST[n] = SRC1[n] & SRC2[n]; \
  } while (0)

#define UNION(DST, SRC1, SRC2) \
  UNIONN (DST, SRC1, SRC2, num_current_words)

#define UNIONN(DST, SRC1, SRC2, N) \
  do { int n = N; \
  while (--n >= 0) DST[n] = SRC1[n] | SRC2[n]; \
  } while (0)

#define INTERSECT(DST, SRC1, SRC2) \
  INTERSECTN (DST, SRC1, SRC2, num_current_words)

#define WORD_SIZE  ((unsigned int)(sizeof(word) * 8))

static void check_bool_init PARAMS ((tree, words, words, words));
static void check_init PARAMS ((tree, words));
static void check_cond_init PARAMS ((tree, tree, tree, words, words, words));
static void check_bool2_init PARAMS ((enum tree_code, tree, tree, words, words, words));
struct alternatives;
static void done_alternative PARAMS ((words, struct alternatives *));

#if 0
#define ALLOC_WORDS(NUM) ((word*) xmalloc ((NUM) * sizeof (word)))
#define FREE_WORDS(PTR) (free (PTR))
#else
#define ALLOC_WORDS(NUM) ((word*)alloca ((NUM) * sizeof (word)))
#define FREE_WORDS(PTR) ((void)0)
#endif

#define SET_P(WORDS, BIT) \
  (WORDS[BIT / WORD_SIZE] & (1 << (BIT % WORD_SIZE)))

#define CLEAR_BIT(WORDS, BIT) \
  (WORDS[BIT / WORD_SIZE] &= ~ (1 << (BIT % WORD_SIZE)))

#define SET_BIT(WORDS, BIT) \
  (WORDS[BIT / WORD_SIZE] |= (1 << (BIT % WORD_SIZE)))

#define WORDS_NEEDED(BITS) (((BITS)+(WORD_SIZE-1))/(WORD_SIZE))

/* Check a conditional form (TEST_EXP ? THEN_EXP : ELSE_EXP) for
   definite assignment.
   BEFORE, WHEN_FALSE, and WHEN_TRUE are as in check_bool_init. */

static void
check_cond_init (test_exp, then_exp, else_exp,
		 before, when_false, when_true)
     tree test_exp, then_exp, else_exp;
     words before, when_false, when_true;
{
  words tmp = ALLOC_WORDS (6 * num_current_words);
  words test_false = tmp;
  words test_true = tmp + num_current_words;
  words then_false = tmp + 2 * num_current_words;
  words then_true = tmp + 3 * num_current_words;
  words else_false = tmp + 4 * num_current_words;
  words else_true = tmp + 5 * num_current_words;
  check_bool_init (test_exp, before, test_false, test_true);
  check_bool_init (then_exp, test_true, then_false, then_true);
  check_bool_init (else_exp, test_false, else_false, else_true);
  INTERSECT (when_false, then_false, else_false);
  INTERSECT (when_true, then_true, else_true);
  FREE_WORDS (tmp);
}

/* Check a boolean binary form CODE (EXP0, EXP1),
   where CODE is one of EQ_EXPR, BIT_AND_EXPR, or BIT_IOR_EXPR.
   BEFORE, WHEN_FALSE, and WHEN_TRUE are as in check_bool_init. */

static void
check_bool2_init (code, exp0, exp1, before, when_false, when_true)
     enum tree_code code;  tree exp0, exp1;
     words before, when_false, when_true;
{
  word buf[4];
  words tmp = num_current_words <= 1 ? buf
    : ALLOC_WORDS (4 * num_current_words);
  words when_false_0 = tmp;
  words when_false_1 = tmp+num_current_words;
  words when_true_0 = tmp+2*num_current_words;
  words when_true_1 = tmp+3*num_current_words;
  check_bool_init (exp0, before, when_false_0, when_true_0);
  INTERSECT (before, when_false_0, when_true_0);
  check_bool_init (exp1, before, when_false_1, when_true_1);

  INTERSECT (before, when_false_1, when_true_1);

  if (code == EQ_EXPR)
    {
      /* Now set:
       * when_true = (when_false_1 INTERSECTION when_true_1)
       *   UNION (when_true_0 INTERSECTION when_false_1)
       *   UNION (when_false_0 INTERSECTION when_true_1);
       * using when_false and before as temporary working areas.  */
      INTERSECT (when_true, when_true_0, when_false_1);
      INTERSECT (when_false, when_true_0, when_false_1);
      UNION (when_true, when_true, when_false);
      UNION (when_true, when_true, before);

      /* Now set:
       * when_false = (when_false_1 INTERSECTION when_true_1)
       *   UNION (when_true_0 INTERSECTION when_true_1)
       *   UNION (when_false_0 INTERSECTION when_false_1);
       * using before as a temporary working area.  */
      INTERSECT (when_false, when_true_0, when_true_1);
      UNION (when_false, when_false, before);
      INTERSECT (before, when_false_0, when_false_1);
      UNION (when_false, when_false, before);
    }
  else if (code == BIT_AND_EXPR || code == TRUTH_AND_EXPR)
    {
      UNION (when_true, when_true_0, when_true_1);
      INTERSECT (when_false, when_false_0, when_false_1);
      UNION (when_false, when_false, before);
    }
  else /* if (code == BIT_IOR_EXPR || code == TRUTH_OR_EXPR) */
    {
      UNION (when_false, when_false_0, when_false_1);
      INTERSECT (when_true, when_true_0, when_true_1);
      UNION (when_true, when_true, before);
    }

  if (tmp != buf)
    FREE_WORDS (tmp);
}

/* Check a boolean expression EXP for definite assignment.
   BEFORE is the set of variables definitely assigned before the conditional.
   (This bitstring may be modified arbitrarily in this function.)
   On output, WHEN_FALSE is the set of variables definitely assigned after
   the conditional when the conditional is false.
   On output, WHEN_TRUE is the set of variables definitely assigned after
   the conditional when the conditional is true.
   (WHEN_FALSE and WHEN_TRUE are overwritten with initial values ignored.)
   (None of BEFORE, WHEN_FALSE, or WHEN_TRUE can overlap, as they may
   be used as temporary working areas. */

static void
check_bool_init (exp, before, when_false, when_true)
     tree exp;
     words before, when_false, when_true;
{
  switch (TREE_CODE (exp))
    {
    case COND_EXPR:
      check_cond_init (TREE_OPERAND (exp, 0), TREE_OPERAND (exp, 1),
		       TREE_OPERAND (exp, 2),
		       before, when_false, when_true);
      return;

    case TRUTH_ANDIF_EXPR:
      check_cond_init (TREE_OPERAND (exp, 0),
		       TREE_OPERAND (exp, 1), boolean_false_node,
		       before, when_false, when_true);
      return;
    case TRUTH_ORIF_EXPR:
      check_cond_init (TREE_OPERAND (exp, 0),
		       boolean_true_node, TREE_OPERAND (exp, 1),
		       before, when_false, when_true);
      return;
    case TRUTH_NOT_EXPR:
      check_bool_init (TREE_OPERAND (exp, 0), before, when_true, when_false);
      return;
    case MODIFY_EXPR:
      {
	tree tmp = TREE_OPERAND (exp, 0);
	if (TREE_CODE (tmp) == VAR_DECL && ! FIELD_STATIC (tmp))
	  {
	    int index;
	    check_bool_init (TREE_OPERAND (exp, 1), before,
			     when_false, when_true);
	    index = DECL_BIT_INDEX (tmp);
	    if (index >= 0)
	      {
		SET_BIT (when_false, index);
		SET_BIT (when_true, index);
	      }
	    break;
	  }
      }
      goto do_default;

    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case EQ_EXPR:
      check_bool2_init (TREE_CODE (exp),
			TREE_OPERAND (exp, 0), TREE_OPERAND (exp, 1),
			before, when_false, when_true);
      return;

    case TRUTH_XOR_EXPR:
    case BIT_XOR_EXPR:
    case NE_EXPR:
      /* Just like EQ_EXPR, but switch when_true and when_false. */
      check_bool2_init (EQ_EXPR, TREE_OPERAND (exp, 0), TREE_OPERAND (exp, 1),
			before, when_true, when_false);

      return;

    case INTEGER_CST:
      if (integer_zerop (exp))
	{
	  SET_ALL (when_true);
	  COPY (when_false, before);
	}
      else
	{
	  SET_ALL (when_false);
	  COPY (when_true, before);
	}
      break;
    default:
    do_default:
      check_init (exp, before);
      COPY (when_false, before);
      COPY (when_true, before);
    }
}

/* Used to keep track of control flow branches. */

struct alternatives
{
  struct alternatives *outer;

  /* The value of num_current_locals at the start of this compound. */
  int num_locals;

  /* The value of the "before" set at the start of the control stucture.
   Used for SWITCH_EXPR but not set for LABELED_BLOCK_EXPR. */
  words saved;

  int save_start_current_locals;

  /* If num_current_words==1, combined==&one_word, for efficiency. */
  word one_word;

  /* The intersection of the "after" sets from previous branches. */
  words combined;

  tree block;
};

struct alternatives * alternatives = NULL;

#define BEGIN_ALTERNATIVES(before, current) \
{ \
  current.saved = NULL; \
  current.num_locals = num_current_locals; \
  current.combined = num_current_words <= 1 ? &current.one_word \
    : ALLOC_WORDS (num_current_words); \
  SET_ALL (current.combined); \
  current.outer = alternatives; \
  alternatives = &current; \
  current.save_start_current_locals = start_current_locals; \
  start_current_locals = num_current_locals; \
}

static void
done_alternative (after, current)
     words after;
     struct alternatives *current; 
{
  INTERSECTN (current->combined, current->combined, after,
	      WORDS_NEEDED (current->num_locals));
}

#define END_ALTERNATIVES(after, current) \
{ \
  alternatives = current.outer; \
  COPY (after, current.combined); \
  if (current.combined != &current.one_word) \
    FREE_WORDS (current.combined); \
  start_current_locals = current.save_start_current_locals; \
}

/* Check for (un)initialized local variables in EXP.
*/

static void
check_init (exp, before)
     tree exp;
     words before;
{
  tree tmp;
 again:
  switch (TREE_CODE (exp))
    {
    case VAR_DECL:
      if (! FIELD_STATIC (exp) && DECL_NAME (exp) != NULL_TREE)
	{
	  int index = DECL_BIT_INDEX (exp);
	  if (index >= 0 && ! SET_P (before, index))
	    {
#if 1
	      parse_error_context (wfl,
				   "Variable `%s' may not have been initialized"
				   , IDENTIFIER_POINTER (DECL_NAME (exp)));
#else
	      error_with_decl (exp, "variable may be used uninitialized");
#endif
	      /* Suppress further errors. */
	      DECL_BIT_INDEX (exp) = -1;
	    }
	}
      break;
    case MODIFY_EXPR:
      tmp = TREE_OPERAND (exp, 0);
      if (TREE_CODE (tmp) == VAR_DECL && ! FIELD_STATIC (tmp))
	{
	  int index;
	  check_init (TREE_OPERAND (exp, 1), before);
	  index = DECL_BIT_INDEX (tmp);
	  if (index >= 0)
	    SET_BIT (before, index);
	  /* Minor optimization.  See comment for start_current_locals. */
	  if (index >= start_current_locals
	      && index == num_current_locals - 1)
	    {
	      num_current_locals--;
	      DECL_BIT_INDEX (tmp) = -1;
	    }
	 break;
       }
     else
       goto binop;
    case BLOCK:
      if (BLOCK_EXPR_BODY (exp))
	{
	  tree decl = BLOCK_EXPR_DECLS (exp);
	  int words_needed;
	  word* tmp;
	  int i;
	  int save_start_current_locals = start_current_locals;
	  int save_num_current_words = num_current_words;
	  start_current_locals = num_current_locals;
	  for (;  decl != NULL_TREE;  decl = TREE_CHAIN (decl))
	    {
	      DECL_BIT_INDEX (decl) = num_current_locals++;
	    }
	  words_needed = WORDS_NEEDED (num_current_locals);
	  if (words_needed > num_current_words)
	    {
	      tmp = ALLOC_WORDS (words_needed);
	      COPY (tmp, before);
	      num_current_words = words_needed;
	    }
	  else
	    tmp = before;
	  for (i = start_current_locals;  i < num_current_locals;  i++)
	    CLEAR_BIT (tmp, i);
	  check_init (BLOCK_EXPR_BODY (exp), tmp);
	  num_current_locals = start_current_locals;
	  start_current_locals = save_start_current_locals;
	  if (tmp != before)
	    {
	      num_current_words = save_num_current_words;
	      COPY (before, tmp);
	      FREE_WORDS (tmp);
	    }
	}
      break;
    case LOOP_EXPR:
      {
	struct alternatives alt;
	BEGIN_ALTERNATIVES (before, alt);
	alt.block = exp;
	check_init (TREE_OPERAND (exp, 0), before);
	done_alternative (before, &alt);
	END_ALTERNATIVES (before, alt);
	return;
      }
    case EXIT_EXPR:
      {
	struct alternatives *alt = alternatives;
	words tmp = ALLOC_WORDS (2 * num_current_words);
	words when_true = tmp;
	words when_false = tmp + num_current_words;
#ifdef ENABLE_CHECKING
	if (TREE_CODE (alt->block) != LOOP_EXPR)
	  fatal ("internal error in check-init:  EXIT_EXPR not in LOOP_EXPR");
#endif
	check_bool_init (TREE_OPERAND (exp, 0), before, when_false, when_true);
	done_alternative (when_true, alt);
	COPY (before, when_false);
	FREE_WORDS (tmp);
	return;
      }
    case LABELED_BLOCK_EXPR:
      {
	struct alternatives alt;
	BEGIN_ALTERNATIVES (before, alt);
	alt.block = exp;
	if (LABELED_BLOCK_BODY (exp))
	  check_init (LABELED_BLOCK_BODY (exp), before);
	done_alternative (before, &alt);
	END_ALTERNATIVES (before, alt);
	return;
      }
    case EXIT_BLOCK_EXPR:
      {
	tree block = TREE_OPERAND (exp, 0);
	struct alternatives *alt = alternatives;
	while (alt->block != block)
	  alt = alt->outer;
	done_alternative (before, alt);
	SET_ALL (before);
	return;
      }
    case SWITCH_EXPR:
      {
	struct alternatives alt;
	check_init (TREE_OPERAND (exp, 0), before);
	BEGIN_ALTERNATIVES (before, alt);
	alt.saved = ALLOC_WORDS (num_current_words);
	COPY (alt.saved, before);
	alt.block = exp;
	check_init (TREE_OPERAND (exp, 1), before);
	done_alternative (before, &alt);
	FREE_WORDS (alt.saved);
	END_ALTERNATIVES (before, alt);
	return;
      }
    case CASE_EXPR:
    case DEFAULT_EXPR:
      {
	int i;
	struct alternatives *alt = alternatives;
	while (TREE_CODE (alt->block) != SWITCH_EXPR)
	  alt = alt->outer;
	COPYN (before, alt->saved, WORDS_NEEDED (alt->num_locals));
	for (i = alt->num_locals;  i < num_current_locals;  i++)
	  CLEAR_BIT (before, i);
	break;
      }

    case CLEANUP_POINT_EXPR:
      {
	struct alternatives alt;
	BEGIN_ALTERNATIVES (before, alt);
	CLEAR_ALL (alt.combined);
	check_init (TREE_OPERAND (exp, 0), before); 
	UNION (alt.combined, alt.combined, before);
	END_ALTERNATIVES (before, alt);
      }
      return;
    case WITH_CLEANUP_EXPR:
      {
	struct alternatives *alt = alternatives;	
#ifdef ENABLE_CHECKING
	if (TREE_CODE (alt->block) != CLEANUP_POINT_EXPR)
	  fatal ("internal error in check-init:  WITH_CLEANUP_EXPR not in CLEANUP_POINT_EXPR");
#endif
	check_init (TREE_OPERAND (exp, 0), before);
	UNION (alt->combined, alt->combined, before);
	check_init (TREE_OPERAND (exp, 2), alt->combined);
	return;
      }

    case TRY_EXPR:
      {
	tree try_clause = TREE_OPERAND (exp, 0);
	tree clause = TREE_OPERAND (exp, 1);
	words save = ALLOC_WORDS (num_current_words);
	words tmp = ALLOC_WORDS (num_current_words);
	struct alternatives alt;
	BEGIN_ALTERNATIVES (before, alt);
	COPY (save, before);
	COPY (tmp, save);
	check_init (try_clause, tmp);
	done_alternative (tmp, &alt);
	for ( ; clause != NULL_TREE;  clause = TREE_CHAIN (clause))
	  {
	    tree catch_clause = TREE_OPERAND (clause, 0);
	    COPY (tmp, save);
	    check_init (catch_clause, tmp);
	    done_alternative (tmp, &alt);
	  }
	FREE_WORDS (tmp);
	FREE_WORDS (save);
	END_ALTERNATIVES (before, alt);
      }
    return;

    case TRY_FINALLY_EXPR:
      {
	words tmp = ALLOC_WORDS (num_current_words);
	COPY (tmp, before);
	check_init (TREE_OPERAND (exp, 0), tmp);
	check_init (TREE_OPERAND (exp, 1), before);
	FREE_WORDS (tmp);
      }
      return;

    case RETURN_EXPR:
    case THROW_EXPR:
      if (TREE_OPERAND (exp, 0))
	check_init (TREE_OPERAND (exp, 0), before);
      goto never_continues;

    case ERROR_MARK:
    never_continues:
      SET_ALL (before);
      return;
      
    case COND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      {
	words tmp = ALLOC_WORDS (2 * num_current_words);
	words when_true = tmp;
	words when_false = tmp + num_current_words;
	check_bool_init (exp, before, when_false, when_true);
	INTERSECT (before, when_false, when_true);
	FREE_WORDS (tmp);
      }
      break;
    case UNARY_PLUS_EXPR:
    case NEGATE_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
    case BIT_NOT_EXPR:
    case CONVERT_EXPR:
    case COMPONENT_REF:
    case NOP_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case INDIRECT_REF:
    case ADDR_EXPR:
    case SAVE_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case NON_LVALUE_EXPR:
    case INSTANCEOF_EXPR:
      /* Avoid needless recursion. */
      exp = TREE_OPERAND (exp, 0);
      goto again;

    case COMPOUND_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case RDIV_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case URSHIFT_EXPR:
    case BIT_AND_EXPR:
    case BIT_XOR_EXPR:
    case BIT_IOR_EXPR:
    case EQ_EXPR: 
    case NE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case ARRAY_REF:
    binop:
      check_init (TREE_OPERAND (exp, 0), before);
      /* Avoid needless recursion, especially for COMPOUND_EXPR. */
      exp = TREE_OPERAND (exp, 1);
      goto again;

    case PARM_DECL:
    case RESULT_DECL:
    case FUNCTION_DECL:
    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
      break;

    case NEW_CLASS_EXPR:
    case CALL_EXPR:
      {
	tree func = TREE_OPERAND (exp, 0);
	tree x = TREE_OPERAND (exp, 1);
	if (TREE_CODE (func) == ADDR_EXPR)
	  func = TREE_OPERAND (func, 0);
	check_init (func, before);

	for ( ;  x != NULL_TREE;  x = TREE_CHAIN (x))
	  check_init (TREE_VALUE (x), before);
	if (func == throw_node[0]
	    || func == throw_node[1])
	  goto never_continues;
      }
      break;

    case NEW_ARRAY_INIT:
      {
	tree x = CONSTRUCTOR_ELTS (TREE_OPERAND (exp, 0));
	for ( ;  x != NULL_TREE;  x = TREE_CHAIN (x))
	  check_init (TREE_VALUE (x), before);
      }
      break;

    case EXPR_WITH_FILE_LOCATION:
      {
	char *saved_input_filename = input_filename;
	tree saved_wfl = wfl;
	tree body = EXPR_WFL_NODE (exp);
	int saved_lineno = lineno;
	if (body == empty_stmt_node)
	  break;
	wfl = exp;
	input_filename = EXPR_WFL_FILENAME (exp);
	lineno = EXPR_WFL_LINENO (exp);
	check_init (body, before);
	input_filename = saved_input_filename;
	lineno = saved_lineno;
	wfl = saved_wfl;
      }
      break;
      
    default:
      fatal ("internal error in check-init: tree code not implemented: %s",
	    tree_code_name [(int) TREE_CODE (exp)]);
    }
}

void
check_for_initialization (body)
     tree body;
{
  word before = 0;
  check_init (body, &before);
}
