/* Alias analysis for GNU C
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by John Carr (jfc@mit.edu).

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

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "function.h"
#include "insn-flags.h"
#include "expr.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "output.h"
#include "toplev.h"
#include "splay-tree.h"
#include "ggc.h"

/* The alias sets assigned to MEMs assist the back-end in determining
   which MEMs can alias which other MEMs.  In general, two MEMs in
   different alias sets to not alias each other.  There is one
   exception, however.  Consider something like:

     struct S {int i; double d; };

   a store to an `S' can alias something of either type `int' or type
   `double'.  (However, a store to an `int' cannot alias a `double'
   and vice versa.)  We indicate this via a tree structure that looks
   like:
           struct S
            /   \
	   /     \
         |/_     _\|
         int    double

   (The arrows are directed and point downwards.)  If, when comparing
   two alias sets, we can hold one set fixed,  trace the other set
   downwards, and at some point find the first set, the two MEMs can
   alias one another.  In this situation we say the alias set for
   `struct S' is the `superset' and that those for `int' and `double'
   are `subsets'.  

   Alias set zero is implicitly a superset of all other alias sets.
   However, this is no actual entry for alias set zero.  It is an
   error to attempt to explicitly construct a subset of zero.  */

typedef struct alias_set_entry
{
  /* The alias set number, as stored in MEM_ALIAS_SET.  */
  int alias_set;

  /* The children of the alias set.  These are not just the immediate
     children, but, in fact, all children.  So, if we have:

       struct T { struct S s; float f; } 

     continuing our example above, the children here will be all of
     `int', `double', `float', and `struct S'.  */
  splay_tree children;
} *alias_set_entry;

static rtx canon_rtx			PARAMS ((rtx));
static int rtx_equal_for_memref_p	PARAMS ((rtx, rtx));
static rtx find_symbolic_term		PARAMS ((rtx));
static int memrefs_conflict_p		PARAMS ((int, rtx, int, rtx,
						 HOST_WIDE_INT));
static void record_set			PARAMS ((rtx, rtx, void *));
static rtx find_base_term		PARAMS ((rtx));
static int base_alias_check		PARAMS ((rtx, rtx, enum machine_mode,
						 enum machine_mode));
static rtx find_base_value		PARAMS ((rtx));
static int mems_in_disjoint_alias_sets_p PARAMS ((rtx, rtx));
static int insert_subset_children       PARAMS ((splay_tree_node, void*));
static alias_set_entry get_alias_set_entry PARAMS ((int));
static rtx fixed_scalar_and_varying_struct_p PARAMS ((rtx, rtx, int (*)(rtx)));
static int aliases_everything_p         PARAMS ((rtx));
static int write_dependence_p           PARAMS ((rtx, rtx, int));
static int nonlocal_reference_p         PARAMS ((rtx));

/* Set up all info needed to perform alias analysis on memory references.  */

/* Returns the size in bytes of the mode of X.  */
#define SIZE_FOR_MODE(X) (GET_MODE_SIZE (GET_MODE (X)))

/* Returns nonzero if MEM1 and MEM2 do not alias because they are in
   different alias sets.  We ignore alias sets in functions making use
   of variable arguments because the va_arg macros on some systems are
   not legal ANSI C.  */
#define DIFFERENT_ALIAS_SETS_P(MEM1, MEM2)			\
  mems_in_disjoint_alias_sets_p (MEM1, MEM2)

/* Cap the number of passes we make over the insns propagating alias
   information through set chains.

   10 is a completely arbitrary choice.  */
#define MAX_ALIAS_LOOP_PASSES 10
   
/* reg_base_value[N] gives an address to which register N is related.
   If all sets after the first add or subtract to the current value
   or otherwise modify it so it does not point to a different top level
   object, reg_base_value[N] is equal to the address part of the source
   of the first set.

   A base address can be an ADDRESS, SYMBOL_REF, or LABEL_REF.  ADDRESS
   expressions represent certain special values: function arguments and
   the stack, frame, and argument pointers.  

   The contents of an ADDRESS is not normally used, the mode of the
   ADDRESS determines whether the ADDRESS is a function argument or some
   other special value.  Pointer equality, not rtx_equal_p, determines whether
   two ADDRESS expressions refer to the same base address.

   The only use of the contents of an ADDRESS is for determining if the
   current function performs nonlocal memory memory references for the
   purposes of marking the function as a constant function.  */

static rtx *reg_base_value;
static rtx *new_reg_base_value;
static unsigned int reg_base_value_size; /* size of reg_base_value array */

#define REG_BASE_VALUE(X) \
  ((unsigned) REGNO (X) < reg_base_value_size ? reg_base_value[REGNO (X)] : 0)

/* Vector of known invariant relationships between registers.  Set in
   loop unrolling.  Indexed by register number, if nonzero the value
   is an expression describing this register in terms of another.

   The length of this array is REG_BASE_VALUE_SIZE.

   Because this array contains only pseudo registers it has no effect
   after reload.  */
static rtx *alias_invariant;

/* Vector indexed by N giving the initial (unchanging) value known
   for pseudo-register N.  */
rtx *reg_known_value;

/* Indicates number of valid entries in reg_known_value.  */
static int reg_known_value_size;

/* Vector recording for each reg_known_value whether it is due to a
   REG_EQUIV note.  Future passes (viz., reload) may replace the
   pseudo with the equivalent expression and so we account for the
   dependences that would be introduced if that happens. */
/* ??? This is a problem only on the Convex.  The REG_EQUIV notes created in
   assign_parms mention the arg pointer, and there are explicit insns in the
   RTL that modify the arg pointer.  Thus we must ensure that such insns don't
   get scheduled across each other because that would invalidate the REG_EQUIV
   notes.  One could argue that the REG_EQUIV notes are wrong, but solving
   the problem in the scheduler will likely give better code, so we do it
   here.  */
char *reg_known_equiv_p;

/* True when scanning insns from the start of the rtl to the
   NOTE_INSN_FUNCTION_BEG note.  */

static int copying_arguments;

/* The splay-tree used to store the various alias set entries.  */

static splay_tree alias_sets;

/* Returns a pointer to the alias set entry for ALIAS_SET, if there is
   such an entry, or NULL otherwise.  */

static alias_set_entry
get_alias_set_entry (alias_set)
     int alias_set;
{
  splay_tree_node sn
    = splay_tree_lookup (alias_sets, (splay_tree_key) alias_set);

  return sn != 0 ? ((alias_set_entry) sn->value) : 0;
}

/* Returns nonzero value if the alias sets for MEM1 and MEM2 are such
   that the two MEMs cannot alias each other.  */

static int 
mems_in_disjoint_alias_sets_p (mem1, mem2)
     rtx mem1;
     rtx mem2;
{
  alias_set_entry ase;

#ifdef ENABLE_CHECKING	
/* Perform a basic sanity check.  Namely, that there are no alias sets
   if we're not using strict aliasing.  This helps to catch bugs
   whereby someone uses PUT_CODE, but doesn't clear MEM_ALIAS_SET, or
   where a MEM is allocated in some way other than by the use of
   gen_rtx_MEM, and the MEM_ALIAS_SET is not cleared.  If we begin to
   use alias sets to indicate that spilled registers cannot alias each
   other, we might need to remove this check.  */
  if (! flag_strict_aliasing
      && (MEM_ALIAS_SET (mem1) != 0 || MEM_ALIAS_SET (mem2) != 0))
    abort ();
#endif

  /* The code used in varargs macros are often not conforming ANSI C,
     which can trick the compiler into making incorrect aliasing
     assumptions in these functions.  So, we don't use alias sets in
     such a function.  FIXME: This should be moved into the front-end;
     it is a language-dependent notion, and there's no reason not to
     still use these checks to handle globals.  */
  if (current_function_stdarg || current_function_varargs)
    return 0;

  /* If have no alias set information for one of the MEMs, we have to assume
     it can alias anything.  */
  if (MEM_ALIAS_SET (mem1) == 0 || MEM_ALIAS_SET (mem2) == 0)
    return 0;

  /* If the two alias sets are the same, they may alias.  */
  if (MEM_ALIAS_SET (mem1) == MEM_ALIAS_SET (mem2))
    return 0;

  /* Iterate through each of the children of the first alias set,
     comparing it with the second alias set.  */
  ase = get_alias_set_entry (MEM_ALIAS_SET (mem1));
  if (ase != 0 && splay_tree_lookup (ase->children,
				     (splay_tree_key) MEM_ALIAS_SET (mem2)))
    return  0;

  /* Now do the same, but with the alias sets reversed.  */
  ase = get_alias_set_entry (MEM_ALIAS_SET (mem2));
  if (ase != 0 && splay_tree_lookup (ase->children,
				     (splay_tree_key) MEM_ALIAS_SET (mem1)))
    return  0;

  /* The two MEMs are in distinct alias sets, and neither one is the
     child of the other.  Therefore, they cannot alias.  */
  return 1;
}

/* Insert the NODE into the splay tree given by DATA.  Used by
   record_alias_subset via splay_tree_foreach.  */

static int
insert_subset_children (node, data)
     splay_tree_node node;
     void *data;
{
  splay_tree_insert ((splay_tree) data, node->key, node->value);

  return 0;
}

/* Indicate that things in SUBSET can alias things in SUPERSET, but
   not vice versa.  For example, in C, a store to an `int' can alias a
   structure containing an `int', but not vice versa.  Here, the
   structure would be the SUPERSET and `int' the SUBSET.  This
   function should be called only once per SUPERSET/SUBSET pair.  At
   present any given alias set may only be a subset of one superset.  

   It is illegal for SUPERSET to be zero; everything is implicitly a
   subset of alias set zero.  */

void
record_alias_subset (superset, subset)
     int superset;
     int subset;
{
  alias_set_entry superset_entry;
  alias_set_entry subset_entry;

  if (superset == 0)
    abort ();

  superset_entry = get_alias_set_entry (superset);
  if (superset_entry == 0) 
    {
      /* Create an entry for the SUPERSET, so that we have a place to
	 attach the SUBSET.  */
      superset_entry
	= (alias_set_entry) xmalloc (sizeof (struct alias_set_entry));
      superset_entry->alias_set = superset;
      superset_entry->children 
	= splay_tree_new (splay_tree_compare_ints, 0, 0);
      splay_tree_insert (alias_sets, (splay_tree_key) superset,
			 (splay_tree_value) superset_entry);

    }

  subset_entry = get_alias_set_entry (subset);

  /* If there is an entry for the subset, enter all of its children
     (if they are not already present) as children of the SUPERSET.  */
  if (subset_entry) 
    splay_tree_foreach (subset_entry->children,
			insert_subset_children,
			superset_entry->children);

  /* Enter the SUBSET itself as a child of the SUPERSET.  */
  splay_tree_insert (superset_entry->children, 
		     (splay_tree_key) subset, 0);
}

/* Inside SRC, the source of a SET, find a base address.  */

static rtx
find_base_value (src)
     register rtx src;
{
  switch (GET_CODE (src))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return src;

    case REG:
      /* At the start of a function, argument registers have known base
	 values which may be lost later.  Returning an ADDRESS
	 expression here allows optimization based on argument values
	 even when the argument registers are used for other purposes.  */
      if (REGNO (src) < FIRST_PSEUDO_REGISTER && copying_arguments)
	return new_reg_base_value[REGNO (src)];

      /* If a pseudo has a known base value, return it.  Do not do this
	 for hard regs since it can result in a circular dependency
	 chain for registers which have values at function entry.

	 The test above is not sufficient because the scheduler may move
	 a copy out of an arg reg past the NOTE_INSN_FUNCTION_BEGIN.  */
      if (REGNO (src) >= FIRST_PSEUDO_REGISTER
	  && (unsigned) REGNO (src) < reg_base_value_size
	  && reg_base_value[REGNO (src)])
	return reg_base_value[REGNO (src)];

      return src;

    case MEM:
      /* Check for an argument passed in memory.  Only record in the
	 copying-arguments block; it is too hard to track changes
	 otherwise.  */
      if (copying_arguments
	  && (XEXP (src, 0) == arg_pointer_rtx
	      || (GET_CODE (XEXP (src, 0)) == PLUS
		  && XEXP (XEXP (src, 0), 0) == arg_pointer_rtx)))
	return gen_rtx_ADDRESS (VOIDmode, src);
      return 0;

    case CONST:
      src = XEXP (src, 0);
      if (GET_CODE (src) != PLUS && GET_CODE (src) != MINUS)
	break;

      /* ... fall through ... */

    case PLUS:
    case MINUS:
      {
	rtx temp, src_0 = XEXP (src, 0), src_1 = XEXP (src, 1);

	/* If either operand is a REG, then see if we already have
	   a known value for it.  */
	if (GET_CODE (src_0) == REG)
	  {
	    temp = find_base_value (src_0);
	    if (temp != 0)
	      src_0 = temp;
	  }

	if (GET_CODE (src_1) == REG)
	  {
	    temp = find_base_value (src_1);
	    if (temp!= 0)
	      src_1 = temp;
	  }

	/* Guess which operand is the base address:
	   If either operand is a symbol, then it is the base.  If
	   either operand is a CONST_INT, then the other is the base.  */
	if (GET_CODE (src_1) == CONST_INT || CONSTANT_P (src_0))
	  return find_base_value (src_0);
	else if (GET_CODE (src_0) == CONST_INT || CONSTANT_P (src_1))
	  return find_base_value (src_1);

	/* This might not be necessary anymore:
	   If either operand is a REG that is a known pointer, then it
	   is the base.  */
	else if (GET_CODE (src_0) == REG && REGNO_POINTER_FLAG (REGNO (src_0)))
	  return find_base_value (src_0);
	else if (GET_CODE (src_1) == REG && REGNO_POINTER_FLAG (REGNO (src_1)))
	  return find_base_value (src_1);

	return 0;
      }

    case LO_SUM:
      /* The standard form is (lo_sum reg sym) so look only at the
	 second operand.  */
      return find_base_value (XEXP (src, 1));

    case AND:
      /* If the second operand is constant set the base
	 address to the first operand. */
      if (GET_CODE (XEXP (src, 1)) == CONST_INT && INTVAL (XEXP (src, 1)) != 0)
	return find_base_value (XEXP (src, 0));
      return 0;

    case ZERO_EXTEND:
    case SIGN_EXTEND:	/* used for NT/Alpha pointers */
    case HIGH:
      return find_base_value (XEXP (src, 0));

    default:
      break;
    }

  return 0;
}

/* Called from init_alias_analysis indirectly through note_stores.  */

/* While scanning insns to find base values, reg_seen[N] is nonzero if
   register N has been set in this function.  */
static char *reg_seen;

/* Addresses which are known not to alias anything else are identified
   by a unique integer.  */
static int unique_id;

static void
record_set (dest, set, data)
     rtx dest, set;
     void *data ATTRIBUTE_UNUSED;
{
  register unsigned regno;
  rtx src;

  if (GET_CODE (dest) != REG)
    return;

  regno = REGNO (dest);

  if (regno >= reg_base_value_size)
    abort ();

  if (set)
    {
      /* A CLOBBER wipes out any old value but does not prevent a previously
	 unset register from acquiring a base address (i.e. reg_seen is not
	 set).  */
      if (GET_CODE (set) == CLOBBER)
	{
	  new_reg_base_value[regno] = 0;
	  return;
	}
      src = SET_SRC (set);
    }
  else
    {
      if (reg_seen[regno])
	{
	  new_reg_base_value[regno] = 0;
	  return;
	}
      reg_seen[regno] = 1;
      new_reg_base_value[regno] = gen_rtx_ADDRESS (Pmode,
						   GEN_INT (unique_id++));
      return;
    }

  /* This is not the first set.  If the new value is not related to the
     old value, forget the base value. Note that the following code is
     not detected:
     extern int x, y;  int *p = &x; p += (&y-&x);
     ANSI C does not allow computing the difference of addresses
     of distinct top level objects.  */
  if (new_reg_base_value[regno])
    switch (GET_CODE (src))
      {
      case LO_SUM:
      case PLUS:
      case MINUS:
	if (XEXP (src, 0) != dest && XEXP (src, 1) != dest)
	  new_reg_base_value[regno] = 0;
	break;
      case AND:
	if (XEXP (src, 0) != dest || GET_CODE (XEXP (src, 1)) != CONST_INT)
	  new_reg_base_value[regno] = 0;
	break;
      default:
	new_reg_base_value[regno] = 0;
	break;
      }
  /* If this is the first set of a register, record the value.  */
  else if ((regno >= FIRST_PSEUDO_REGISTER || ! fixed_regs[regno])
	   && ! reg_seen[regno] && new_reg_base_value[regno] == 0)
    new_reg_base_value[regno] = find_base_value (src);

  reg_seen[regno] = 1;
}

/* Called from loop optimization when a new pseudo-register is created.  */

void
record_base_value (regno, val, invariant)
     int regno;
     rtx val;
     int invariant;
{
  if ((unsigned) regno >= reg_base_value_size)
    return;

  /* If INVARIANT is true then this value also describes an invariant
     relationship which can be used to deduce that two registers with
     unknown values are different.  */
  if (invariant && alias_invariant)
    alias_invariant[regno] = val;

  if (GET_CODE (val) == REG)
    {
      if ((unsigned) REGNO (val) < reg_base_value_size)
	reg_base_value[regno] = reg_base_value[REGNO (val)];

      return;
    }

  reg_base_value[regno] = find_base_value (val);
}

static rtx
canon_rtx (x)
     rtx x;
{
  /* Recursively look for equivalences.  */
  if (GET_CODE (x) == REG && REGNO (x) >= FIRST_PSEUDO_REGISTER
      && REGNO (x) < reg_known_value_size)
    return reg_known_value[REGNO (x)] == x
      ? x : canon_rtx (reg_known_value[REGNO (x)]);
  else if (GET_CODE (x) == PLUS)
    {
      rtx x0 = canon_rtx (XEXP (x, 0));
      rtx x1 = canon_rtx (XEXP (x, 1));

      if (x0 != XEXP (x, 0) || x1 != XEXP (x, 1))
	{
	  /* We can tolerate LO_SUMs being offset here; these
	     rtl are used for nothing other than comparisons.  */
	  if (GET_CODE (x0) == CONST_INT)
	    return plus_constant_for_output (x1, INTVAL (x0));
	  else if (GET_CODE (x1) == CONST_INT)
	    return plus_constant_for_output (x0, INTVAL (x1));
	  return gen_rtx_PLUS (GET_MODE (x), x0, x1);
	}
    }

  /* This gives us much better alias analysis when called from
     the loop optimizer.   Note we want to leave the original
     MEM alone, but need to return the canonicalized MEM with
     all the flags with their original values.  */
  else if (GET_CODE (x) == MEM)
    {
      rtx addr = canon_rtx (XEXP (x, 0));

      if (addr != XEXP (x, 0))
	{
	  rtx new = gen_rtx_MEM (GET_MODE (x), addr);

	  RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (x);
	  MEM_COPY_ATTRIBUTES (new, x);
	  MEM_ALIAS_SET (new) = MEM_ALIAS_SET (x);
	  x = new;
	}
    }
  return x;
}

/* Return 1 if X and Y are identical-looking rtx's.

   We use the data in reg_known_value above to see if two registers with
   different numbers are, in fact, equivalent.  */

static int
rtx_equal_for_memref_p (x, y)
     rtx x, y;
{
  register int i;
  register int j;
  register enum rtx_code code;
  register const char *fmt;

  if (x == 0 && y == 0)
    return 1;
  if (x == 0 || y == 0)
    return 0;

  x = canon_rtx (x);
  y = canon_rtx (y);

  if (x == y)
    return 1;

  code = GET_CODE (x);
  /* Rtx's of different codes cannot be equal.  */
  if (code != GET_CODE (y))
    return 0;

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.
     (REG:SI x) and (REG:HI x) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* REG, LABEL_REF, and SYMBOL_REF can be compared nonrecursively.  */

  if (code == REG)
    return REGNO (x) == REGNO (y);
  if (code == LABEL_REF)
    return XEXP (x, 0) == XEXP (y, 0);
  if (code == SYMBOL_REF)
    return XSTR (x, 0) == XSTR (y, 0);
  if (code == CONST_INT)
    return INTVAL (x) == INTVAL (y);
  /* There's no need to compare the contents of CONST_DOUBLEs because
     they're unique. */
  if (code == CONST_DOUBLE)
    return 0;
  if (code == ADDRESSOF)
    return (REGNO (XEXP (x, 0)) == REGNO (XEXP (y, 0))
	    && XINT (x, 1) == XINT (y, 1));

  /* For commutative operations, the RTX match if the operand match in any
     order.  Also handle the simple binary and unary cases without a loop.  */
  if (code == EQ || code == NE || GET_RTX_CLASS (code) == 'c')
    return ((rtx_equal_for_memref_p (XEXP (x, 0), XEXP (y, 0))
	     && rtx_equal_for_memref_p (XEXP (x, 1), XEXP (y, 1)))
	    || (rtx_equal_for_memref_p (XEXP (x, 0), XEXP (y, 1))
		&& rtx_equal_for_memref_p (XEXP (x, 1), XEXP (y, 0))));
  else if (GET_RTX_CLASS (code) == '<' || GET_RTX_CLASS (code) == '2')
    return (rtx_equal_for_memref_p (XEXP (x, 0), XEXP (y, 0))
	    && rtx_equal_for_memref_p (XEXP (x, 1), XEXP (y, 1)));
  else if (GET_RTX_CLASS (code) == '1')
    return rtx_equal_for_memref_p (XEXP (x, 0), XEXP (y, 0));

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole things.

     Limit cases to types which actually appear in addresses.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'E':
	  /* Two vectors must have the same length.  */
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;

	  /* And the corresponding elements must match.  */
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (rtx_equal_for_memref_p (XVECEXP (x, i, j),
					XVECEXP (y, i, j)) == 0)
	      return 0;
	  break;

	case 'e':
	  if (rtx_equal_for_memref_p (XEXP (x, i), XEXP (y, i)) == 0)
	    return 0;
	  break;

	/* This can happen for an asm which clobbers memory.  */
	case '0':
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's,
	     except for within LABEL_REFs and SYMBOL_REFs.  */
	default:
	  abort ();
	}
    }
  return 1;
}

/* Given an rtx X, find a SYMBOL_REF or LABEL_REF within
   X and return it, or return 0 if none found.  */

static rtx
find_symbolic_term (x)
     rtx x;
{
  register int i;
  register enum rtx_code code;
  register const char *fmt;

  code = GET_CODE (x);
  if (code == SYMBOL_REF || code == LABEL_REF)
    return x;
  if (GET_RTX_CLASS (code) == 'o')
    return 0;

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      rtx t;

      if (fmt[i] == 'e')
	{
	  t = find_symbolic_term (XEXP (x, i));
	  if (t != 0)
	    return t;
	}
      else if (fmt[i] == 'E')
	break;
    }
  return 0;
}

static rtx
find_base_term (x)
     register rtx x;
{
  switch (GET_CODE (x))
    {
    case REG:
      return REG_BASE_VALUE (x);

    case ZERO_EXTEND:
    case SIGN_EXTEND:	/* Used for Alpha/NT pointers */
    case HIGH:
    case PRE_INC:
    case PRE_DEC:
    case POST_INC:
    case POST_DEC:
      return find_base_term (XEXP (x, 0));

    case CONST:
      x = XEXP (x, 0);
      if (GET_CODE (x) != PLUS && GET_CODE (x) != MINUS)
	return 0;
      /* fall through */
    case LO_SUM:
    case PLUS:
    case MINUS:
      {
	rtx tmp1 = XEXP (x, 0);
	rtx tmp2 = XEXP (x, 1);

	/* This is a litle bit tricky since we have to determine which of
	   the two operands represents the real base address.  Otherwise this
	   routine may return the index register instead of the base register.

	   That may cause us to believe no aliasing was possible, when in
	   fact aliasing is possible.

	   We use a few simple tests to guess the base register.  Additional
	   tests can certainly be added.  For example, if one of the operands
	   is a shift or multiply, then it must be the index register and the
	   other operand is the base register.  */
	
	/* If either operand is known to be a pointer, then use it
	   to determine the base term.  */
	if (REG_P (tmp1) && REGNO_POINTER_FLAG (REGNO (tmp1)))
	  return find_base_term (tmp1);

	if (REG_P (tmp2) && REGNO_POINTER_FLAG (REGNO (tmp2)))
	  return find_base_term (tmp2);

	/* Neither operand was known to be a pointer.  Go ahead and find the
	   base term for both operands.  */
	tmp1 = find_base_term (tmp1);
	tmp2 = find_base_term (tmp2);

	/* If either base term is named object or a special address
	   (like an argument or stack reference), then use it for the
	   base term.  */
	if (tmp1 != 0
	    && (GET_CODE (tmp1) == SYMBOL_REF
		|| GET_CODE (tmp1) == LABEL_REF
		|| (GET_CODE (tmp1) == ADDRESS
		    && GET_MODE (tmp1) != VOIDmode)))
	  return tmp1;

	if (tmp2 != 0
	    && (GET_CODE (tmp2) == SYMBOL_REF
		|| GET_CODE (tmp2) == LABEL_REF
		|| (GET_CODE (tmp2) == ADDRESS
		    && GET_MODE (tmp2) != VOIDmode)))
	  return tmp2;

	/* We could not determine which of the two operands was the
	   base register and which was the index.  So we can determine
	   nothing from the base alias check.  */
	return 0;
      }

    case AND:
      if (GET_CODE (XEXP (x, 0)) == REG && GET_CODE (XEXP (x, 1)) == CONST_INT)
	return REG_BASE_VALUE (XEXP (x, 0));
      return 0;

    case SYMBOL_REF:
    case LABEL_REF:
      return x;

    default:
      return 0;
    }
}

/* Return 0 if the addresses X and Y are known to point to different
   objects, 1 if they might be pointers to the same object.  */

static int
base_alias_check (x, y, x_mode, y_mode)
     rtx x, y;
     enum machine_mode x_mode, y_mode;
{
  rtx x_base = find_base_term (x);
  rtx y_base = find_base_term (y);

  /* If the address itself has no known base see if a known equivalent
     value has one.  If either address still has no known base, nothing
     is known about aliasing.  */
  if (x_base == 0)
    {
      rtx x_c;

      if (! flag_expensive_optimizations || (x_c = canon_rtx (x)) == x)
	return 1;

      x_base = find_base_term (x_c);
      if (x_base == 0)
	return 1;
    }

  if (y_base == 0)
    {
      rtx y_c;
      if (! flag_expensive_optimizations || (y_c = canon_rtx (y)) == y)
	return 1;

      y_base = find_base_term (y_c);
      if (y_base == 0)
	return 1;
    }

  /* If the base addresses are equal nothing is known about aliasing.  */
  if (rtx_equal_p (x_base, y_base))
    return 1;

  /* The base addresses of the read and write are different expressions. 
     If they are both symbols and they are not accessed via AND, there is
     no conflict.  We can bring knowledge of object alignment into play
     here.  For example, on alpha, "char a, b;" can alias one another,
     though "char a; long b;" cannot.  */
  if (GET_CODE (x_base) != ADDRESS && GET_CODE (y_base) != ADDRESS)
    {
      if (GET_CODE (x) == AND && GET_CODE (y) == AND)
	return 1;
      if (GET_CODE (x) == AND
	  && (GET_CODE (XEXP (x, 1)) != CONST_INT
	      || GET_MODE_UNIT_SIZE (y_mode) < -INTVAL (XEXP (x, 1))))
	return 1;
      if (GET_CODE (y) == AND
	  && (GET_CODE (XEXP (y, 1)) != CONST_INT
	      || GET_MODE_UNIT_SIZE (x_mode) < -INTVAL (XEXP (y, 1))))
	return 1;
      /* Differing symbols never alias.  */
      return 0;
    }

  /* If one address is a stack reference there can be no alias:
     stack references using different base registers do not alias,
     a stack reference can not alias a parameter, and a stack reference
     can not alias a global.  */
  if ((GET_CODE (x_base) == ADDRESS && GET_MODE (x_base) == Pmode)
      || (GET_CODE (y_base) == ADDRESS && GET_MODE (y_base) == Pmode))
    return 0;

  if (! flag_argument_noalias)
    return 1;

  if (flag_argument_noalias > 1)
    return 0;

  /* Weak noalias assertion (arguments are distinct, but may match globals). */
  return ! (GET_MODE (x_base) == VOIDmode && GET_MODE (y_base) == VOIDmode);
}

/*  Return the address of the (N_REFS + 1)th memory reference to ADDR
    where SIZE is the size in bytes of the memory reference.  If ADDR
    is not modified by the memory reference then ADDR is returned.  */

rtx
addr_side_effect_eval (addr, size, n_refs)
     rtx addr;
     int size;
     int n_refs;
{
  int offset = 0;
  
  switch (GET_CODE (addr))
    {
    case PRE_INC:
      offset = (n_refs + 1) * size;
      break;
    case PRE_DEC:
      offset = -(n_refs + 1) * size;
      break;
    case POST_INC:
      offset = n_refs * size;
      break;
    case POST_DEC:
      offset = -n_refs * size;
      break;

    default:
      return addr;
    }
  
  if (offset)
    addr = gen_rtx_PLUS (GET_MODE (addr), XEXP (addr, 0), GEN_INT (offset));
  else
    addr = XEXP (addr, 0);

  return addr;
}

/* Return nonzero if X and Y (memory addresses) could reference the
   same location in memory.  C is an offset accumulator.  When
   C is nonzero, we are testing aliases between X and Y + C.
   XSIZE is the size in bytes of the X reference,
   similarly YSIZE is the size in bytes for Y.

   If XSIZE or YSIZE is zero, we do not know the amount of memory being
   referenced (the reference was BLKmode), so make the most pessimistic
   assumptions.

   If XSIZE or YSIZE is negative, we may access memory outside the object
   being referenced as a side effect.  This can happen when using AND to
   align memory references, as is done on the Alpha.

   Nice to notice that varying addresses cannot conflict with fp if no
   local variables had their addresses taken, but that's too hard now.  */


static int
memrefs_conflict_p (xsize, x, ysize, y, c)
     register rtx x, y;
     int xsize, ysize;
     HOST_WIDE_INT c;
{
  if (GET_CODE (x) == HIGH)
    x = XEXP (x, 0);
  else if (GET_CODE (x) == LO_SUM)
    x = XEXP (x, 1);
  else
    x = canon_rtx (addr_side_effect_eval (x, xsize, 0));
  if (GET_CODE (y) == HIGH)
    y = XEXP (y, 0);
  else if (GET_CODE (y) == LO_SUM)
    y = XEXP (y, 1);
  else
    y = canon_rtx (addr_side_effect_eval (y, ysize, 0));

  if (rtx_equal_for_memref_p (x, y))
    {
      if (xsize <= 0 || ysize <= 0)
	return 1;
      if (c >= 0 && xsize > c)
	return 1;
      if (c < 0 && ysize+c > 0)
	return 1;
      return 0;
    }

  /* This code used to check for conflicts involving stack references and
     globals but the base address alias code now handles these cases.  */

  if (GET_CODE (x) == PLUS)
    {
      /* The fact that X is canonicalized means that this
	 PLUS rtx is canonicalized.  */
      rtx x0 = XEXP (x, 0);
      rtx x1 = XEXP (x, 1);

      if (GET_CODE (y) == PLUS)
	{
	  /* The fact that Y is canonicalized means that this
	     PLUS rtx is canonicalized.  */
	  rtx y0 = XEXP (y, 0);
	  rtx y1 = XEXP (y, 1);

	  if (rtx_equal_for_memref_p (x1, y1))
	    return memrefs_conflict_p (xsize, x0, ysize, y0, c);
	  if (rtx_equal_for_memref_p (x0, y0))
	    return memrefs_conflict_p (xsize, x1, ysize, y1, c);
	  if (GET_CODE (x1) == CONST_INT)
	    {
	      if (GET_CODE (y1) == CONST_INT)
		return memrefs_conflict_p (xsize, x0, ysize, y0,
					   c - INTVAL (x1) + INTVAL (y1));
	      else
		return memrefs_conflict_p (xsize, x0, ysize, y,
					   c - INTVAL (x1));
	    }
	  else if (GET_CODE (y1) == CONST_INT)
	    return memrefs_conflict_p (xsize, x, ysize, y0, c + INTVAL (y1));

	  return 1;
	}
      else if (GET_CODE (x1) == CONST_INT)
	return memrefs_conflict_p (xsize, x0, ysize, y, c - INTVAL (x1));
    }
  else if (GET_CODE (y) == PLUS)
    {
      /* The fact that Y is canonicalized means that this
	 PLUS rtx is canonicalized.  */
      rtx y0 = XEXP (y, 0);
      rtx y1 = XEXP (y, 1);

      if (GET_CODE (y1) == CONST_INT)
	return memrefs_conflict_p (xsize, x, ysize, y0, c + INTVAL (y1));
      else
	return 1;
    }

  if (GET_CODE (x) == GET_CODE (y))
    switch (GET_CODE (x))
      {
      case MULT:
	{
	  /* Handle cases where we expect the second operands to be the
	     same, and check only whether the first operand would conflict
	     or not.  */
	  rtx x0, y0;
	  rtx x1 = canon_rtx (XEXP (x, 1));
	  rtx y1 = canon_rtx (XEXP (y, 1));
	  if (! rtx_equal_for_memref_p (x1, y1))
	    return 1;
	  x0 = canon_rtx (XEXP (x, 0));
	  y0 = canon_rtx (XEXP (y, 0));
	  if (rtx_equal_for_memref_p (x0, y0))
	    return (xsize == 0 || ysize == 0
		    || (c >= 0 && xsize > c) || (c < 0 && ysize+c > 0));

	  /* Can't properly adjust our sizes.  */
	  if (GET_CODE (x1) != CONST_INT)
	    return 1;
	  xsize /= INTVAL (x1);
	  ysize /= INTVAL (x1);
	  c /= INTVAL (x1);
	  return memrefs_conflict_p (xsize, x0, ysize, y0, c);
	}

      case REG:
	/* Are these registers known not to be equal?  */
	if (alias_invariant)
	  {
	    unsigned int r_x = REGNO (x), r_y = REGNO (y);
	    rtx i_x, i_y;	/* invariant relationships of X and Y */

	    i_x = r_x >= reg_base_value_size ? 0 : alias_invariant[r_x];
	    i_y = r_y >= reg_base_value_size ? 0 : alias_invariant[r_y];

	    if (i_x == 0 && i_y == 0)
	      break;

	    if (! memrefs_conflict_p (xsize, i_x ? i_x : x,
				      ysize, i_y ? i_y : y, c))
	      return 0;
	  }
	break;

      default:
	break;
      }

  /* Treat an access through an AND (e.g. a subword access on an Alpha)
     as an access with indeterminate size.  Assume that references 
     besides AND are aligned, so if the size of the other reference is
     at least as large as the alignment, assume no other overlap.  */
  if (GET_CODE (x) == AND && GET_CODE (XEXP (x, 1)) == CONST_INT)
    {
      if (GET_CODE (y) == AND || ysize < -INTVAL (XEXP (x, 1)))
	xsize = -1;
      return memrefs_conflict_p (xsize, XEXP (x, 0), ysize, y, c);
    }
  if (GET_CODE (y) == AND && GET_CODE (XEXP (y, 1)) == CONST_INT)
    {
      /* ??? If we are indexing far enough into the array/structure, we
	 may yet be able to determine that we can not overlap.  But we 
	 also need to that we are far enough from the end not to overlap
	 a following reference, so we do nothing with that for now.  */
      if (GET_CODE (x) == AND || xsize < -INTVAL (XEXP (y, 1)))
	ysize = -1;
      return memrefs_conflict_p (xsize, x, ysize, XEXP (y, 0), c);
    }

  if (CONSTANT_P (x))
    {
      if (GET_CODE (x) == CONST_INT && GET_CODE (y) == CONST_INT)
	{
	  c += (INTVAL (y) - INTVAL (x));
	  return (xsize <= 0 || ysize <= 0
		  || (c >= 0 && xsize > c) || (c < 0 && ysize+c > 0));
	}

      if (GET_CODE (x) == CONST)
	{
	  if (GET_CODE (y) == CONST)
	    return memrefs_conflict_p (xsize, canon_rtx (XEXP (x, 0)),
				       ysize, canon_rtx (XEXP (y, 0)), c);
	  else
	    return memrefs_conflict_p (xsize, canon_rtx (XEXP (x, 0)),
				       ysize, y, c);
	}
      if (GET_CODE (y) == CONST)
	return memrefs_conflict_p (xsize, x, ysize,
				   canon_rtx (XEXP (y, 0)), c);

      if (CONSTANT_P (y))
	return (xsize < 0 || ysize < 0
		|| (rtx_equal_for_memref_p (x, y)
		    && (xsize == 0 || ysize == 0
		        || (c >= 0 && xsize > c) || (c < 0 && ysize+c > 0))));

      return 1;
    }
  return 1;
}

/* Functions to compute memory dependencies.

   Since we process the insns in execution order, we can build tables
   to keep track of what registers are fixed (and not aliased), what registers
   are varying in known ways, and what registers are varying in unknown
   ways.

   If both memory references are volatile, then there must always be a
   dependence between the two references, since their order can not be
   changed.  A volatile and non-volatile reference can be interchanged
   though. 

   A MEM_IN_STRUCT reference at a non-QImode non-AND varying address can never
   conflict with a non-MEM_IN_STRUCT reference at a fixed address.   We must
   allow QImode aliasing because the ANSI C standard allows character
   pointers to alias anything.  We are assuming that characters are
   always QImode here.  We also must allow AND addresses, because they may
   generate accesses outside the object being referenced.  This is used to
   generate aligned addresses from unaligned addresses, for instance, the
   alpha storeqi_unaligned pattern.  */

/* Read dependence: X is read after read in MEM takes place.  There can
   only be a dependence here if both reads are volatile.  */

int
read_dependence (mem, x)
     rtx mem;
     rtx x;
{
  return MEM_VOLATILE_P (x) && MEM_VOLATILE_P (mem);
}

/* Returns MEM1 if and only if MEM1 is a scalar at a fixed address and
   MEM2 is a reference to a structure at a varying address, or returns
   MEM2 if vice versa.  Otherwise, returns NULL_RTX.  If a non-NULL
   value is returned MEM1 and MEM2 can never alias.  VARIES_P is used
   to decide whether or not an address may vary; it should return
   nozero whenever variation is possible.  */

static rtx
fixed_scalar_and_varying_struct_p (mem1, mem2, varies_p)
     rtx mem1;
     rtx mem2;
     int (*varies_p) PARAMS ((rtx));
{
  rtx mem1_addr = XEXP (mem1, 0);
  rtx mem2_addr = XEXP (mem2, 0);
  
  if (MEM_SCALAR_P (mem1) && MEM_IN_STRUCT_P (mem2) 
      && !varies_p (mem1_addr) && varies_p (mem2_addr))
    /* MEM1 is a scalar at a fixed address; MEM2 is a struct at a
       varying address.  */
    return mem1;

  if (MEM_IN_STRUCT_P (mem1) && MEM_SCALAR_P (mem2) 
      && varies_p (mem1_addr) && !varies_p (mem2_addr))
    /* MEM2 is a scalar at a fixed address; MEM1 is a struct at a
       varying address.  */
    return mem2;

  return NULL_RTX;
}

/* Returns nonzero if something about the mode or address format MEM1
   indicates that it might well alias *anything*.  */

static int
aliases_everything_p (mem)
     rtx mem;
{
  if (GET_MODE (mem) == QImode)
    /* ANSI C says that a `char*' can point to anything.  */
    return 1;

  if (GET_CODE (XEXP (mem, 0)) == AND)
    /* If the address is an AND, its very hard to know at what it is
       actually pointing.  */
    return 1;
    
  return 0;
}

/* True dependence: X is read after store in MEM takes place.  */

int
true_dependence (mem, mem_mode, x, varies)
     rtx mem;
     enum machine_mode mem_mode;
     rtx x;
     int (*varies) PARAMS ((rtx));
{
  register rtx x_addr, mem_addr;

  if (MEM_VOLATILE_P (x) && MEM_VOLATILE_P (mem))
    return 1;

  if (DIFFERENT_ALIAS_SETS_P (x, mem))
    return 0;

  /* If X is an unchanging read, then it can't possibly conflict with any
     non-unchanging store.  It may conflict with an unchanging write though,
     because there may be a single store to this address to initialize it.
     Just fall through to the code below to resolve the case where we have
     both an unchanging read and an unchanging write.  This won't handle all
     cases optimally, but the possible performance loss should be
     negligible.  */
  if (RTX_UNCHANGING_P (x) && ! RTX_UNCHANGING_P (mem))
    return 0;

  if (mem_mode == VOIDmode)
    mem_mode = GET_MODE (mem);

  if (! base_alias_check (XEXP (x, 0), XEXP (mem, 0), GET_MODE (x), mem_mode))
    return 0;

  x_addr = canon_rtx (XEXP (x, 0));
  mem_addr = canon_rtx (XEXP (mem, 0));

  if (! memrefs_conflict_p (GET_MODE_SIZE (mem_mode), mem_addr,
			    SIZE_FOR_MODE (x), x_addr, 0))
    return 0;

  if (aliases_everything_p (x))
    return 1;

  /* We cannot use aliases_everyting_p to test MEM, since we must look
     at MEM_MODE, rather than GET_MODE (MEM).  */
  if (mem_mode == QImode || GET_CODE (mem_addr) == AND)
    return 1;

  /* In true_dependence we also allow BLKmode to alias anything.  Why
     don't we do this in anti_dependence and output_dependence?  */
  if (mem_mode == BLKmode || GET_MODE (x) == BLKmode)
    return 1;

  return !fixed_scalar_and_varying_struct_p (mem, x, varies);
}

/* Returns non-zero if a write to X might alias a previous read from
   (or, if WRITEP is non-zero, a write to) MEM.  */

static int
write_dependence_p (mem, x, writep)
     rtx mem;
     rtx x;
     int writep;
{
  rtx x_addr, mem_addr;
  rtx fixed_scalar;

  if (MEM_VOLATILE_P (x) && MEM_VOLATILE_P (mem))
    return 1;

  /* If MEM is an unchanging read, then it can't possibly conflict with
     the store to X, because there is at most one store to MEM, and it must
     have occurred somewhere before MEM.  */
  if (!writep && RTX_UNCHANGING_P (mem))
    return 0;

  if (! base_alias_check (XEXP (x, 0), XEXP (mem, 0), GET_MODE (x),
			  GET_MODE (mem)))
    return 0;

  x = canon_rtx (x);
  mem = canon_rtx (mem);

  if (DIFFERENT_ALIAS_SETS_P (x, mem))
    return 0;

  x_addr = XEXP (x, 0);
  mem_addr = XEXP (mem, 0);

  if (!memrefs_conflict_p (SIZE_FOR_MODE (mem), mem_addr,
			   SIZE_FOR_MODE (x), x_addr, 0))
    return 0;

  fixed_scalar 
    = fixed_scalar_and_varying_struct_p (mem, x, rtx_addr_varies_p);
  
  return (!(fixed_scalar == mem && !aliases_everything_p (x))
	  && !(fixed_scalar == x && !aliases_everything_p (mem)));
}

/* Anti dependence: X is written after read in MEM takes place.  */

int
anti_dependence (mem, x)
     rtx mem;
     rtx x;
{
  return write_dependence_p (mem, x, /*writep=*/0);
}

/* Output dependence: X is written after store in MEM takes place.  */

int
output_dependence (mem, x)
     register rtx mem;
     register rtx x;
{
  return write_dependence_p (mem, x, /*writep=*/1);
}

/* Returns non-zero if X might refer to something which is not
   local to the function and is not constant.  */

static int
nonlocal_reference_p (x)
     rtx x;
{
  rtx base;
  register RTX_CODE code;
  int regno;

  code = GET_CODE (x);

  if (GET_RTX_CLASS (code) == 'i')
    {
      /* Constant functions are constant.  */
      if (code == CALL_INSN && CONST_CALL_P (x))
	return 0;
      x = PATTERN (x);
      code = GET_CODE (x);
    }

  switch (code)
    {
    case SUBREG:
      if (GET_CODE (SUBREG_REG (x)) == REG)
	{
	  /* Global registers are not local.  */
	  if (REGNO (SUBREG_REG (x)) < FIRST_PSEUDO_REGISTER
	      && global_regs[REGNO (SUBREG_REG (x)) + SUBREG_WORD (x)])
	    return 1;
	  return 0;
	}
      break;

    case REG:
      regno = REGNO (x);
      /* Global registers are not local.  */
      if (regno < FIRST_PSEUDO_REGISTER && global_regs[regno])
	return 1;
      return 0;

    case SCRATCH:
    case PC:
    case CC0:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case LABEL_REF:
      return 0;

    case SYMBOL_REF:
      /* Constants in the function's constants pool are constant.  */
      if (CONSTANT_POOL_ADDRESS_P (x))
	return 0;
      return 1;

    case CALL:
      /* Recursion introduces no additional considerations.  */
      if (GET_CODE (XEXP (x, 0)) == MEM
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
	  && strcmp(XSTR (XEXP (XEXP (x, 0), 0), 0),
		    IDENTIFIER_POINTER (
			  DECL_ASSEMBLER_NAME (current_function_decl))) == 0)
	return 0;
      return 1;

    case MEM:
      /* Be overly conservative and consider any volatile memory
	 reference as not local.  */
      if (MEM_VOLATILE_P (x))
	return 1;
      base = find_base_term (XEXP (x, 0));
      if (base)
	{
	  /* A Pmode ADDRESS could be a reference via the structure value
	     address or static chain.  Such memory references are nonlocal.

	     Thus, we have to examine the contents of the ADDRESS to find
	     out if this is a local reference or not.  */
	  if (GET_CODE (base) == ADDRESS
	      && GET_MODE (base) == Pmode
	      && (XEXP (base, 0) == stack_pointer_rtx
		  || XEXP (base, 0) == arg_pointer_rtx
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
		  || XEXP (base, 0) == hard_frame_pointer_rtx
#endif
		  || XEXP (base, 0) == frame_pointer_rtx))
	    return 0;
	  /* Constants in the function's constant pool are constant.  */
	  if (GET_CODE (base) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (base))
	    return 0;
	}
      return 1;

    case ASM_INPUT:
    case ASM_OPERANDS:
      return 1;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    register const char *fmt = GET_RTX_FORMAT (code);
    register int i;
    
    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    if (nonlocal_reference_p (XEXP (x, i)))
	      return 1;
	  }
	else if (fmt[i] == 'E')
	  {
	    register int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      if (nonlocal_reference_p (XVECEXP (x, i, j)))
		return 1;
	  }
      }
  }

  return 0;
}

/* Mark the function if it is constant.  */

void
mark_constant_function ()
{
  rtx insn;

  if (TREE_PUBLIC (current_function_decl)
      || TREE_READONLY (current_function_decl)
      || TREE_THIS_VOLATILE (current_function_decl)
      || TYPE_MODE (TREE_TYPE (current_function_decl)) == VOIDmode)
    return;

  /* Determine if this is a constant function.  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
	&& nonlocal_reference_p (insn))
      return;

  /* Mark the function.  */

  TREE_READONLY (current_function_decl) = 1;
}


static HARD_REG_SET argument_registers;

void
init_alias_once ()
{
  register int i;

#ifndef OUTGOING_REGNO
#define OUTGOING_REGNO(N) N
#endif
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    /* Check whether this register can hold an incoming pointer
       argument.  FUNCTION_ARG_REGNO_P tests outgoing register
       numbers, so translate if necessary due to register windows. */
    if (FUNCTION_ARG_REGNO_P (OUTGOING_REGNO (i))
	&& HARD_REGNO_MODE_OK (i, Pmode))
      SET_HARD_REG_BIT (argument_registers, i);

  alias_sets = splay_tree_new (splay_tree_compare_ints, 0, 0);
}

void
init_alias_analysis ()
{
  int maxreg = max_reg_num ();
  int changed, pass;
  register int i;
  register unsigned int ui;
  register rtx insn;

  reg_known_value_size = maxreg;

  reg_known_value 
    = (rtx *) xcalloc ((maxreg - FIRST_PSEUDO_REGISTER), sizeof (rtx))
    - FIRST_PSEUDO_REGISTER;
  reg_known_equiv_p 
    = (char*) xcalloc ((maxreg - FIRST_PSEUDO_REGISTER), sizeof (char))
    - FIRST_PSEUDO_REGISTER;

  /* Overallocate reg_base_value to allow some growth during loop
     optimization.  Loop unrolling can create a large number of
     registers.  */
  reg_base_value_size = maxreg * 2;
  reg_base_value = (rtx *) xcalloc (reg_base_value_size, sizeof (rtx));
  if (ggc_p)
    ggc_add_rtx_root (reg_base_value, reg_base_value_size);

  new_reg_base_value = (rtx *) xmalloc (reg_base_value_size * sizeof (rtx));
  reg_seen = (char *) xmalloc (reg_base_value_size);
  if (! reload_completed && flag_unroll_loops)
    {
      /* ??? Why are we realloc'ing if we're just going to zero it?  */
      alias_invariant = (rtx *)xrealloc (alias_invariant,
					 reg_base_value_size * sizeof (rtx));
      bzero ((char *)alias_invariant, reg_base_value_size * sizeof (rtx));
    }
    

  /* The basic idea is that each pass through this loop will use the
     "constant" information from the previous pass to propagate alias
     information through another level of assignments.

     This could get expensive if the assignment chains are long.  Maybe
     we should throttle the number of iterations, possibly based on
     the optimization level or flag_expensive_optimizations.

     We could propagate more information in the first pass by making use
     of REG_N_SETS to determine immediately that the alias information
     for a pseudo is "constant".

     A program with an uninitialized variable can cause an infinite loop
     here.  Instead of doing a full dataflow analysis to detect such problems
     we just cap the number of iterations for the loop.

     The state of the arrays for the set chain in question does not matter
     since the program has undefined behavior.  */

  pass = 0;
  do
    {
      /* Assume nothing will change this iteration of the loop.  */
      changed = 0;

      /* We want to assign the same IDs each iteration of this loop, so
	 start counting from zero each iteration of the loop.  */
      unique_id = 0;

      /* We're at the start of the funtion each iteration through the
	 loop, so we're copying arguments.  */
      copying_arguments = 1;

      /* Wipe the potential alias information clean for this pass.  */
      bzero ((char *) new_reg_base_value, reg_base_value_size * sizeof (rtx));

      /* Wipe the reg_seen array clean.  */
      bzero ((char *) reg_seen, reg_base_value_size);

      /* Mark all hard registers which may contain an address.
	 The stack, frame and argument pointers may contain an address.
	 An argument register which can hold a Pmode value may contain
	 an address even if it is not in BASE_REGS.

	 The address expression is VOIDmode for an argument and
	 Pmode for other registers.  */

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (argument_registers, i))
	  new_reg_base_value[i] = gen_rtx_ADDRESS (VOIDmode,
						   gen_rtx_REG (Pmode, i));

      new_reg_base_value[STACK_POINTER_REGNUM]
	= gen_rtx_ADDRESS (Pmode, stack_pointer_rtx);
      new_reg_base_value[ARG_POINTER_REGNUM]
	= gen_rtx_ADDRESS (Pmode, arg_pointer_rtx);
      new_reg_base_value[FRAME_POINTER_REGNUM]
	= gen_rtx_ADDRESS (Pmode, frame_pointer_rtx);
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
      new_reg_base_value[HARD_FRAME_POINTER_REGNUM]
	= gen_rtx_ADDRESS (Pmode, hard_frame_pointer_rtx);
#endif
      if (struct_value_incoming_rtx
	  && GET_CODE (struct_value_incoming_rtx) == REG)
	new_reg_base_value[REGNO (struct_value_incoming_rtx)]
	  = gen_rtx_ADDRESS (Pmode, struct_value_incoming_rtx);

      if (static_chain_rtx
	  && GET_CODE (static_chain_rtx) == REG)
	new_reg_base_value[REGNO (static_chain_rtx)]
	  = gen_rtx_ADDRESS (Pmode, static_chain_rtx);

      /* Walk the insns adding values to the new_reg_base_value array.  */
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	{
#if defined (HAVE_prologue) || defined (HAVE_epilogue)
	  if (prologue_epilogue_contains (insn))
	    continue;
#endif
	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	    {
	      rtx note, set;
	      /* If this insn has a noalias note, process it,  Otherwise,
	         scan for sets.  A simple set will have no side effects
	         which could change the base value of any other register. */

	      if (GET_CODE (PATTERN (insn)) == SET
		  && (find_reg_note (insn, REG_NOALIAS, NULL_RTX)))
		record_set (SET_DEST (PATTERN (insn)), NULL_RTX, NULL);
	      else
		note_stores (PATTERN (insn), record_set, NULL);

	      set = single_set (insn);

	      if (set != 0
		  && GET_CODE (SET_DEST (set)) == REG
		  && REGNO (SET_DEST (set)) >= FIRST_PSEUDO_REGISTER
		  && (((note = find_reg_note (insn, REG_EQUAL, 0)) != 0
		       && REG_N_SETS (REGNO (SET_DEST (set))) == 1)
		      || (note = find_reg_note (insn, REG_EQUIV, NULL_RTX)) != 0)
		  && GET_CODE (XEXP (note, 0)) != EXPR_LIST
		  && ! reg_overlap_mentioned_p (SET_DEST (set), XEXP (note, 0)))
		{
		  int regno = REGNO (SET_DEST (set));
		  reg_known_value[regno] = XEXP (note, 0);
		  reg_known_equiv_p[regno] = REG_NOTE_KIND (note) == REG_EQUIV;
		}
	    }
	  else if (GET_CODE (insn) == NOTE
		   && NOTE_LINE_NUMBER (insn) == NOTE_INSN_FUNCTION_BEG)
	    copying_arguments = 0;
	}

      /* Now propagate values from new_reg_base_value to reg_base_value.  */
      for (ui = 0; ui < reg_base_value_size; ui++)
	{
	  if (new_reg_base_value[ui]
	      && new_reg_base_value[ui] != reg_base_value[ui]
	      && ! rtx_equal_p (new_reg_base_value[ui], reg_base_value[ui]))
	    {
	      reg_base_value[ui] = new_reg_base_value[ui];
	      changed = 1;
	    }
	}
    }
  while (changed && ++pass < MAX_ALIAS_LOOP_PASSES);

  /* Fill in the remaining entries.  */
  for (i = FIRST_PSEUDO_REGISTER; i < maxreg; i++)
    if (reg_known_value[i] == 0)
      reg_known_value[i] = regno_reg_rtx[i];

  /* Simplify the reg_base_value array so that no register refers to
     another register, except to special registers indirectly through
     ADDRESS expressions.

     In theory this loop can take as long as O(registers^2), but unless
     there are very long dependency chains it will run in close to linear
     time.

     This loop may not be needed any longer now that the main loop does
     a better job at propagating alias information.  */
  pass = 0;
  do
    {
      changed = 0;
      pass++;
      for (ui = 0; ui < reg_base_value_size; ui++)
	{
	  rtx base = reg_base_value[ui];
	  if (base && GET_CODE (base) == REG)
	    {
	      unsigned int base_regno = REGNO (base);
	      if (base_regno == ui)		/* register set from itself */
		reg_base_value[ui] = 0;
	      else
		reg_base_value[ui] = reg_base_value[base_regno];
	      changed = 1;
	    }
	}
    }
  while (changed && pass < MAX_ALIAS_LOOP_PASSES);

  /* Clean up.  */
  free (new_reg_base_value);
  new_reg_base_value = 0;
  free (reg_seen);
  reg_seen = 0;
}

void
end_alias_analysis ()
{
  free (reg_known_value + FIRST_PSEUDO_REGISTER);
  reg_known_value = 0;
  reg_known_value_size = 0;
  free (reg_known_equiv_p + FIRST_PSEUDO_REGISTER);
  reg_known_equiv_p = 0;
  if (reg_base_value)
    {
      if (ggc_p)
	ggc_del_root (reg_base_value);
      free (reg_base_value);
      reg_base_value = 0;
    }
  reg_base_value_size = 0;
  if (alias_invariant)
    {
      free (alias_invariant);
      alias_invariant = 0;
    }
}
