/* Code to test for "definitive [un]assignment".
   Copyright (C) 1999, 2000, 2001, 2003, 2004, 2005 Free Software Foundation,
   Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com>, January 1999. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h" /* Needed for optimize. */
#include "java-tree.h"
#include "toplev.h" /* Needed for fatal. */

/* The basic idea is that we assign each local variable declaration
   and each blank final field an index, and then we pass around
   bitstrings, where the (2*i)'th bit is set if decl whose DECL_BIT_INDEX
   is i is definitely assigned, and the the (2*i=1)'th bit is set if 
   decl whose DECL_BIT_INDEX is i is definitely unassigned */

/* One segment of a bitstring. */
typedef unsigned int word;

/* Pointer to a bitstring. */
typedef word *words;

/* Number of locals variables currently active. */
static int num_current_locals = 0;

/* The value of num_current_locals when we entered the closest
   enclosing LOOP_EXPR. */
static int loop_current_locals;

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
static int start_current_locals = 0;

static int num_current_words;

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

#define WORD_SIZE  ((unsigned int)(sizeof(word) * BITS_PER_UNIT))

static void check_bool_init (tree, words, words, words);
static void check_init (tree, words);
static void check_cond_init (tree, tree, tree, words, words, words);
static void check_bool2_init (enum tree_code, tree, tree, words, words, words);
struct alternatives;
static void done_alternative (words, struct alternatives *);
static tree get_variable_decl (tree);
static void final_assign_error (tree);
static void check_final_reassigned (tree, words);

#define ALLOC_WORDS(NUM) (xmalloc ((NUM) * sizeof (word)))
#define FREE_WORDS(PTR) (free (PTR))

/* DECLARE_BUFFERS is used to allocate NUMBUFFER bit sets, each of
   which is an array of length num_current_words number of words.
   Declares a new local variable BUFFER to hold the result (or rather
   a pointer to the first of the bit sets).  In almost all cases
   num_current_words will be 1 or at most 2, so we try to stack
   allocate the arrays in that case, using a stack array
   named BUFFER##_short.  Each DECLARE_BUFFERS must be matched by
   a corresponding RELEASE_BUFFERS to avoid memory leaks.  */

#define DECLARE_BUFFERS(BUFFER, NUMBUFFERS) \
  word BUFFER##_short[2 * NUMBUFFERS]; \
  words BUFFER = ALLOC_BUFFER(BUFFER##_short, NUMBUFFERS * num_current_words)

#define RELEASE_BUFFERS(BUFFER) \
  FREE_BUFFER(BUFFER, BUFFER##_short)

#define ALLOC_BUFFER(SHORTBUFFER, NUMWORDS) \
  ((NUMWORDS) * sizeof(word) <= sizeof(SHORTBUFFER) ? SHORTBUFFER \
   : ALLOC_WORDS(NUMWORDS))

#define FREE_BUFFER(BUFFER, SHORTBUFFER) \
  if (BUFFER != SHORTBUFFER) FREE_WORDS(BUFFER)

#define SET_P(WORDS, BIT) \
  (WORDS[(BIT) / WORD_SIZE] & (1 << ((BIT) % WORD_SIZE)))

#define CLEAR_BIT(WORDS, BIT) \
  (WORDS[(BIT) / WORD_SIZE] &= ~ (1 << ((BIT) % WORD_SIZE)))

#define SET_BIT(WORDS, BIT) \
  (WORDS[(BIT) / WORD_SIZE] |= (1 << ((BIT) % WORD_SIZE)))

#define WORDS_NEEDED(BITS) (((BITS)+(WORD_SIZE-1))/(WORD_SIZE))

#define ASSIGNED_P(WORDS, BIT)  SET_P(WORDS, 2 * (BIT))
#define UNASSIGNED_P(WORDS, BIT)  SET_P(WORDS, 2 * (BIT) + 1)

#define SET_ASSIGNED(WORDS, INDEX) SET_BIT (WORDS, 2 * (INDEX))
#define SET_UNASSIGNED(WORDS, INDEX) SET_BIT (WORDS, 2 * (INDEX) + 1)

#define CLEAR_ASSIGNED(WORDS, INDEX) CLEAR_BIT (WORDS, 2 * (INDEX))
#define CLEAR_UNASSIGNED(WORDS, INDEX) CLEAR_BIT (WORDS, 2 * (INDEX) + 1)

/* Get the "interesting" declaration from a MODIFY_EXPR or COMPONENT_REF.
   Return the declaration or NULL_TREE if no interesting declaration.  */

static tree
get_variable_decl (tree exp)
{
  /* A static field can be wrapped in a COMPOUND_EXPR where the first
     argument initializes the class.  */
  if (TREE_CODE (exp) == COMPOUND_EXPR)
    exp = extract_field_decl (exp);

  if (TREE_CODE (exp) == VAR_DECL)
    {
      if (! TREE_STATIC (exp) ||  FIELD_FINAL (exp))
	return exp;
    }
  /* We only care about final parameters. */
  else if (TREE_CODE (exp) == PARM_DECL)
    {
      if (DECL_FINAL (exp))
	return exp;
    }
  /* See if exp is this.field. */
  else if (TREE_CODE (exp) == COMPONENT_REF)
    {
      tree op0 = TREE_OPERAND (exp, 0);
      tree op1 = TREE_OPERAND (exp, 1);
      tree mdecl = current_function_decl;
      if (TREE_CODE (op0) == INDIRECT_REF
	  && TREE_CODE (op1) == FIELD_DECL
	  && ! METHOD_STATIC (mdecl)
	  && FIELD_FINAL (op1))
	{
	  op0 = TREE_OPERAND (op0, 0);
	  if (op0 == BLOCK_EXPR_DECLS (DECL_FUNCTION_BODY (mdecl)))
	    return op1;
	}
    }
  else if (TREE_CODE (exp) == INDIRECT_REF)
    {
      /* For indirect dispatch, look for an expression of the form 
      (indirect_ref (+ (array_ref otable <N>) this)).  
      FIXME: it would probably be better to generate a JAVA_FIELD_REF
      expression that gets converted to OTABLE access at
      gimplification time.  */
      exp = TREE_OPERAND (exp, 0);
      if (TREE_CODE (exp) == PLUS_EXPR)
	{
	  tree op0 = TREE_OPERAND (exp, 0);
	  STRIP_NOPS (op0);
	  if (TREE_CODE (op0) == ARRAY_REF)
	    {
	      tree table = TREE_OPERAND (op0, 0);
	      if (TREE_CODE (table) == VAR_DECL
		  && DECL_LANG_SPECIFIC (table)
		  && DECL_OWNER (table) 
		  && TYPE_OTABLE_DECL (DECL_OWNER (table)) == table)
		{
		  HOST_WIDE_INT index 
		    = TREE_INT_CST_LOW (TREE_OPERAND (op0, 1));
		  tree otable_methods 
		    = TYPE_OTABLE_METHODS (DECL_OWNER (table));
		  tree element;
		  for (element = otable_methods; 
		       element; 
		       element = TREE_CHAIN (element))
		    {
		      if (index == 1)
			{
			  tree purpose = TREE_PURPOSE (element);
			  if (TREE_CODE (purpose) == FIELD_DECL)
			    return purpose;
			  else
			    return NULL_TREE;
			}
		      --index;
		    }
		}
	    }
	}
    }

  return NULL_TREE;
}

static void
final_assign_error (tree name)
{
  parse_error_context (wfl,
                       "Can't reassign a value to the final variable %qs",
                       IDENTIFIER_POINTER (name));
}

static void
check_final_reassigned (tree decl, words before)
{
  int index = DECL_BIT_INDEX (decl);
  /* A final local already assigned or a final parameter
     assigned must be reported as errors */
  if (DECL_FINAL (decl) && index != -2
      && (index < loop_current_locals /* I.e. -1, or outside current loop. */
          || (DECL_LOCAL_FINAL_IUD (decl) ? ASSIGNED_P (before, index)
              : ! UNASSIGNED_P (before, index))))
    {
      final_assign_error (DECL_NAME (decl));
    }
}

/* Check a conditional form (TEST_EXP ? THEN_EXP : ELSE_EXP) for
   definite [un]assignment.
   BEFORE, WHEN_FALSE, and WHEN_TRUE are as in check_bool_init. */

static void
check_cond_init (tree test_exp, tree then_exp, tree else_exp,
		 words before, words when_false, words when_true)
{
  int save_start_current_locals = start_current_locals;
  DECLARE_BUFFERS(test_false, 6);
  words test_true = test_false + num_current_words;
  words then_false = test_true + num_current_words;
  words then_true = then_false + num_current_words;
  words else_false = then_true + num_current_words;
  words else_true = else_false + num_current_words;
  start_current_locals = num_current_locals;

  check_bool_init (test_exp, before, test_false, test_true);
  check_bool_init (then_exp, test_true, then_false, then_true);
  check_bool_init (else_exp, test_false, else_false, else_true);
  INTERSECT (when_false, then_false, else_false);
  INTERSECT (when_true, then_true, else_true);
  RELEASE_BUFFERS(test_false);
  start_current_locals = save_start_current_locals;
}

/* Check a boolean binary form CODE (EXP0, EXP1),
   where CODE is one of EQ_EXPR, BIT_AND_EXPR, or BIT_IOR_EXPR.
   BEFORE, WHEN_FALSE, and WHEN_TRUE are as in check_bool_init. */

static void
check_bool2_init (enum tree_code code, tree exp0, tree exp1,
		  words before, words when_false, words when_true)
{
  word buf[2*4];
  words tmp = num_current_words <= 2 ? buf
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

/* Check a boolean expression EXP for definite [un]assignment.
   BEFORE is the set of variables definitely [un]assigned before the
   conditional.  (This bitstring may be modified arbitrarily in this function.)
   On output, WHEN_FALSE is the set of variables [un]definitely assigned after
   the conditional when the conditional is false.
   On output, WHEN_TRUE is the set of variables definitely [un]assigned after
   the conditional when the conditional is true.
   (WHEN_FALSE and WHEN_TRUE are overwritten with initial values ignored.)
   (None of BEFORE, WHEN_FALSE, or WHEN_TRUE can overlap, as they may
   be used as temporary working areas. */

static void
check_bool_init (tree exp, words before, words when_false, words when_true)
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

  /* The value of the "before" set at the start of the control structure.
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

/* Begin handling a control flow branch.
   BEFORE is the state of [un]assigned variables on entry.
   CURRENT is a struct alt to manage the branch alternatives. */

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

/* We have finished with one branch of branching control flow.
   Store the [un]assigned state, merging (intersecting) it with the state
   of previous alternative branches. */

static void
done_alternative (words after, struct alternatives *current)
{
  INTERSECTN (current->combined, current->combined, after,
	      WORDS_NEEDED (2 * current->num_locals));
}

/* Used when we done with a control flow branch and are all merged again.
 * AFTER is the merged state of [un]assigned variables,
   CURRENT is a struct alt that was passed to BEGIN_ALTERNATIVES. */

#define END_ALTERNATIVES(after, current) \
{ \
  alternatives = current.outer; \
  COPY (after, current.combined); \
  if (current.combined != &current.one_word) \
    FREE_WORDS (current.combined); \
  start_current_locals = current.save_start_current_locals; \
}

/* Check for (un)initialized local variables in EXP.  */

static void
check_init (tree exp, words before)
{
  tree tmp;
 again:
  switch (TREE_CODE (exp))
    {
    case VAR_DECL:
    case PARM_DECL:
      if (! FIELD_STATIC (exp) && DECL_NAME (exp) != NULL_TREE
	  && DECL_NAME (exp) != this_identifier_node)
	{
	  int index = DECL_BIT_INDEX (exp);
	  /* We don't want to report and mark as non initialized class
	     initialization flags. */
	  if (! LOCAL_CLASS_INITIALIZATION_FLAG_P (exp)
	      && index >= 0 && ! ASSIGNED_P (before, index))
	    {
	      parse_error_context 
		(wfl, "Variable %qs may not have been initialized",
		 IDENTIFIER_POINTER (DECL_NAME (exp)));
	      /* Suppress further errors. */
	      DECL_BIT_INDEX (exp) = -2;
	    }
	}
      break;

    case COMPONENT_REF:
      check_init (TREE_OPERAND (exp, 0), before);
      if ((tmp = get_variable_decl (exp)) != NULL_TREE)
	{
	  int index = DECL_BIT_INDEX (tmp);
	  if (index >= 0 && ! ASSIGNED_P (before, index))
	    {
	      parse_error_context 
		(wfl, "variable %qs may not have been initialized",
		 IDENTIFIER_POINTER (DECL_NAME (tmp)));
	      /* Suppress further errors. */
	      DECL_BIT_INDEX (tmp) = -2;
	    }
	}
      break;
      
    case MODIFY_EXPR:
      tmp = TREE_OPERAND (exp, 0);
      /* We're interested in variable declaration and parameter
         declaration when they're declared with the `final' modifier. */
      if ((tmp = get_variable_decl (tmp)) != NULL_TREE)
	{
	  int index;
	  check_init (TREE_OPERAND (exp, 1), before);
	  check_final_reassigned (tmp, before);
	  index = DECL_BIT_INDEX (tmp);
	  if (index >= 0)
	    {
	      SET_ASSIGNED (before, index);
	      CLEAR_UNASSIGNED (before, index);
	    }
	  /* Minor optimization.  See comment for start_current_locals.
	     If we're optimizing for class initialization, we keep
	     this information to check whether the variable is
	     definitely assigned when once we checked the whole
	     function. */
	  if (! STATIC_CLASS_INIT_OPT_P () /* FIXME */
	      && ! DECL_FINAL (tmp)
	      && index >= start_current_locals
	      && index == num_current_locals - 1)
	    {
	      num_current_locals--;
	      DECL_BIT_INDEX (tmp) = -1;
	    }
	 break;
       }
      else if (TREE_CODE (tmp = TREE_OPERAND (exp, 0)) == COMPONENT_REF)
	{
	  tree decl;
	  check_init (tmp, before);
	  check_init (TREE_OPERAND (exp, 1), before);
	  decl = TREE_OPERAND (tmp, 1);
	  if (DECL_FINAL (decl))
	    final_assign_error (DECL_NAME (decl));
	  break;
	}
      else if (TREE_CODE (tmp) == COMPONENT_REF && IS_ARRAY_LENGTH_ACCESS (tmp))
	{
	  /* We can't emit a more specific message here, because when
	     compiling to bytecodes we don't get here. */
	  final_assign_error (length_identifier_node);
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
	  words_needed = WORDS_NEEDED (2 * num_current_locals);
	  if (words_needed > num_current_words)
	    {
	      tmp = ALLOC_WORDS (words_needed);
	      COPY (tmp, before);
	      num_current_words = words_needed;
	    }
	  else
	    tmp = before;
	  for (i = start_current_locals;  i < num_current_locals;  i++)
	    {
	      CLEAR_ASSIGNED (tmp, i);
	      SET_UNASSIGNED (tmp, i);
	    }
	  check_init (BLOCK_EXPR_BODY (exp), tmp);

	  /* Re-set DECL_BIT_INDEX since it is also DECL_POINTER_ALIAS_SET. */
	  for (decl = BLOCK_EXPR_DECLS (exp);
	       decl != NULL_TREE;  decl = TREE_CHAIN (decl))
	    {
	      if (LOCAL_CLASS_INITIALIZATION_FLAG_P (decl))
		{
		  int index = DECL_BIT_INDEX (decl);
		  tree fndecl = DECL_CONTEXT (decl);
		  if (fndecl && METHOD_STATIC (fndecl)
		      && (DECL_INITIAL (decl) == boolean_true_node
			  || (index >= 0 && ASSIGNED_P (tmp, index))))
		    *(htab_find_slot 
		      (DECL_FUNCTION_INITIALIZED_CLASS_TABLE (fndecl),
		       DECL_FUNCTION_INIT_TEST_CLASS (decl), INSERT)) =
		      DECL_FUNCTION_INIT_TEST_CLASS (decl);
		}
	      DECL_BIT_INDEX (decl) = -1;
	    }

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
	/* The JLS 2nd edition discusses a complication determining
	   definite unassignment of loop statements.  They define a
	   "hypothetical" analysis model.  We do something much
	   simpler: We just disallow assignments inside loops to final
	   variables declared outside the loop.  This means we may
	   disallow some contrived assignments that the JLS, but I
	   can't see how anything except a very contrived testcase (a
	   do-while whose condition is false?) would care. */

	struct alternatives alt;
	int save_loop_current_locals = loop_current_locals;
	int save_start_current_locals = start_current_locals;
	loop_current_locals = num_current_locals;
	start_current_locals = num_current_locals;
	BEGIN_ALTERNATIVES (before, alt);
	alt.block = exp;
	check_init (TREE_OPERAND (exp, 0), before);
	END_ALTERNATIVES (before, alt);
	loop_current_locals = save_loop_current_locals;
	start_current_locals = save_start_current_locals;
	return;
      }
    case EXIT_EXPR:
      {
	struct alternatives *alt = alternatives;
	DECLARE_BUFFERS(when_true, 2);
	words when_false = when_true + num_current_words;
#ifdef ENABLE_JC1_CHECKING
	if (TREE_CODE (alt->block) != LOOP_EXPR)
	  abort ();
#endif
	check_bool_init (TREE_OPERAND (exp, 0), before, when_false, when_true);
	done_alternative (when_true, alt);
	COPY (before, when_false);
	RELEASE_BUFFERS(when_true);
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
	word buf[2];
	check_init (TREE_OPERAND (exp, 0), before);
	BEGIN_ALTERNATIVES (before, alt);
	alt.saved = ALLOC_BUFFER(buf, num_current_words);
	COPY (alt.saved, before);
	alt.block = exp;
	check_init (TREE_OPERAND (exp, 1), before);
	done_alternative (before, &alt);
	if (! SWITCH_HAS_DEFAULT (exp))
	  done_alternative (alt.saved, &alt);
	FREE_BUFFER(alt.saved, buf);
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
	COPYN (before, alt->saved, WORDS_NEEDED (2 * alt->num_locals));
	for (i = alt->num_locals;  i < num_current_locals;  i++)
	  CLEAR_ASSIGNED (before, i);
	break;
      }

    case TRY_EXPR:
      {
	tree try_clause = TREE_OPERAND (exp, 0);
	tree clause = TREE_OPERAND (exp, 1);
	word buf[2*2];
	words tmp = (num_current_words <= 2 ? buf
		    : ALLOC_WORDS (2 * num_current_words));
	words save = tmp + num_current_words;
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
	if (tmp != buf)
	  {
	    FREE_WORDS (tmp);
	  }
	END_ALTERNATIVES (before, alt);
      }
    return;

    case TRY_FINALLY_EXPR:
      {
	DECLARE_BUFFERS(tmp, 1);
	COPY (tmp, before);
	check_init (TREE_OPERAND (exp, 0), before);
	check_init (TREE_OPERAND (exp, 1), tmp);
	UNION (before, before, tmp);
	RELEASE_BUFFERS(tmp);
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
	DECLARE_BUFFERS(when_true, 2);
	words when_false = when_true + num_current_words;
	check_bool_init (exp, before, when_false, when_true);
	INTERSECT (before, when_false, when_true);
	RELEASE_BUFFERS(when_true);
      }
      break;

    case NOP_EXPR:
      if (IS_EMPTY_STMT (exp))
	break;
      /* ... else fall through ... */
    case UNARY_PLUS_EXPR:
    case NEGATE_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
    case BIT_NOT_EXPR:
    case CONVERT_EXPR:
    case BIT_FIELD_REF:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case INDIRECT_REF:
    case ADDR_EXPR:
    case NON_LVALUE_EXPR:
    case INSTANCEOF_EXPR:
    case FIX_CEIL_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case ABS_EXPR:
      /* Avoid needless recursion. */
      exp = TREE_OPERAND (exp, 0);
      goto again;

    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      tmp = get_variable_decl (TREE_OPERAND (exp, 0));
      if (tmp != NULL_TREE && DECL_FINAL (tmp))
	final_assign_error (DECL_NAME (tmp));
      else if (TREE_CODE (tmp = TREE_OPERAND (exp, 0)) == COMPONENT_REF)
        {
          /* Take care of array length accesses too.  */
          tree decl = TREE_OPERAND (tmp, 1);
          if (DECL_FINAL (decl))
            final_assign_error (DECL_NAME (decl));
        }

      /* Avoid needless recursion.  */
      exp = TREE_OPERAND (exp, 0);
      goto again;

    case SAVE_EXPR:
      if (IS_INIT_CHECKED (exp))
	return;
      IS_INIT_CHECKED (exp) = 1;
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
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case EXACT_DIV_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    binop:
      check_init (TREE_OPERAND (exp, 0), before);
      /* Avoid needless recursion, especially for COMPOUND_EXPR. */
      exp = TREE_OPERAND (exp, 1);
      goto again;

    case RESULT_DECL:
    case FUNCTION_DECL:
    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
    case DECL_EXPR:
    case JAVA_EXC_OBJ_EXPR:
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
	if (func == throw_node)
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
	location_t saved_location = input_location;
	tree saved_wfl = wfl;
	tree body = EXPR_WFL_NODE (exp);
	if (IS_EMPTY_STMT (body))
	  break;
	wfl = exp;
#ifdef USE_MAPPED_LOCATION
	input_location = EXPR_LOCATION (exp);
#else
	input_filename = EXPR_WFL_FILENAME (exp);
	input_line = EXPR_WFL_LINENO (exp);
#endif
	check_init (body, before);
	input_location = saved_location;
	wfl = saved_wfl;
      }
      break;
      
    default:
      internal_error
	("internal error in check-init: tree code not implemented: %s",
	 tree_code_name [(int) TREE_CODE (exp)]);
    }
}

void
check_for_initialization (tree body, tree mdecl)
{
  tree decl;
  word buf[2];
  words before = buf;
  tree owner = DECL_CONTEXT (mdecl);
  int is_static_method = METHOD_STATIC (mdecl);
  /* We don't need to check final fields of <init> it it calls this(). */
  int is_finit_method = DECL_FINIT_P (mdecl) || DECL_INSTINIT_P (mdecl);
  int is_init_method
    = (is_finit_method || DECL_CLINIT_P (mdecl)
       || (DECL_INIT_P (mdecl) && ! DECL_INIT_CALLS_THIS (mdecl)));

  start_current_locals = num_current_locals = 0;
  num_current_words = 2;

  if (is_init_method)
    {
      int words_needed, i;
      for (decl = TYPE_FIELDS (owner);
	   decl != NULL_TREE;  decl = TREE_CHAIN (decl))
	{
	  if (DECL_FINAL (decl) && FIELD_STATIC (decl) == is_static_method)
	    {
	      if (DECL_FIELD_FINAL_IUD (decl))
		DECL_BIT_INDEX (decl) = -1;
	      else
		DECL_BIT_INDEX (decl) = num_current_locals++;
	    }
	}
      words_needed = WORDS_NEEDED (2 * num_current_locals);
      if (words_needed > 2)
	{
	  num_current_words = words_needed;
	  before = ALLOC_WORDS(words_needed);
	}
      i = 0;
      for (decl = TYPE_FIELDS (owner);
	   decl != NULL_TREE;  decl = TREE_CHAIN (decl))
	{
	  if (FIELD_FINAL (decl) && FIELD_STATIC (decl) == is_static_method)
	    {
	      if (! DECL_FIELD_FINAL_IUD (decl))
		{
		  CLEAR_ASSIGNED (before, i);
		  SET_UNASSIGNED (before, i);
		  i++;
		}
	    }
	}

    }

  check_init (body, before);

  if (is_init_method)
    {
      for (decl = TYPE_FIELDS (owner);
	   decl != NULL_TREE;  decl = TREE_CHAIN (decl))
	{
	  if (FIELD_FINAL (decl) && FIELD_STATIC (decl) == is_static_method)
	    {
	      int index = DECL_BIT_INDEX (decl);
	      if (index >= 0 && ! ASSIGNED_P (before, index))
		{
		  if (! is_finit_method)
		    error ("%Jfinal field %qD may not have been initialized",
                           decl, decl);
		}
	      else if (is_finit_method)
		DECL_FIELD_FINAL_IUD (decl) = 1;

	      /* Re-set to initial state, since we later may use the
		 same bit for DECL_POINTER_ALIAS_SET. */
	      DECL_BIT_INDEX (decl) = -1;
	    }
	}
    }

  start_current_locals = num_current_locals = 0;
}
