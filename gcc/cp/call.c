/* Functions related to invoking methods and overloaded functions.
   Copyright (C) 1987, 1992, 1993 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) and
   hacked by Brendan Kehoe (brendan@cygnus.com).

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


/* High-level class interface. */

#include "config.h"
#include "tree.h"
#include <stdio.h>
#include "cp-tree.h"
#include "class.h"
#include "flags.h"

#include "obstack.h"
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern void sorry ();

extern int inhibit_warnings;
extern int flag_assume_nonnull_objects;
extern tree ctor_label, dtor_label;

/* From typeck.c:  */
extern tree unary_complex_lvalue ();

/* Compute the ease with which a conversion can be performed
   between an expected and the given type.  */
static int convert_harshness_old ();
static struct harshness_code convert_harshness_ansi ();

/* OLD METHOD */
/* Note the old method also uses USER_HARSHNESS, BASE_DERIVED_HARSHNESS,
   CONST_HARSHNESS.  */
#define EVIL 1
#define TRIVIAL 0
#define EVIL_HARSHNESS(ARG) ((ARG) & 1)
#define ELLIPSIS_HARSHNESS(ARG) ((ARG) & 2)
#define CONTRAVARIANT_HARSHNESS(ARG) ((ARG) & 8)
#define INT_TO_BD_HARSHNESS(ARG) (((ARG) << 5) | 16)
#define INT_FROM_BD_HARSHNESS(ARG) ((ARG) >> 5)
#define INT_TO_EASY_HARSHNESS(ARG) ((ARG) << 5)
#define INT_FROM_EASY_HARSHNESS(ARG) ((ARG) >> 5)
#define ONLY_EASY_HARSHNESS(ARG) (((ARG) & 31) == 0)


/* NEW METHOD */
#define EVIL_RETURN(ARG)	((ARG).code = EVIL_CODE, (ARG))
#define QUAL_RETURN(ARG)	((ARG).code = QUAL_CODE, (ARG))
#define TRIVIAL_RETURN(ARG)	((ARG).code = TRIVIAL_CODE, (ARG))
#define ZERO_RETURN(ARG)	((ARG).code = 0, (ARG))

#define USER_HARSHNESS(ARG) ((ARG) & 4)
#define BASE_DERIVED_HARSHNESS(ARG) ((ARG) & 16)
#define CONST_HARSHNESS(ARG) ((ARG) & 2048)

/* Ordering function for overload resolution.  Compare two candidates
   by gross quality.  */
int
rank_for_overload_ansi (x, y)
     struct candidate *x, *y;
{
  if (y->h.code & (EVIL_CODE|ELLIPSIS_CODE|USER_CODE))
    return y->h.code - x->h.code;
  if (x->h.code & (EVIL_CODE|ELLIPSIS_CODE|USER_CODE))
    return -1;

  /* This is set by compute_conversion_costs, for calling a non-const
     member function from a const member function.  */
  if ((y->v.ansi_harshness[0].code & CONST_CODE) ^ (x->v.ansi_harshness[0].code & CONST_CODE))
    return y->v.ansi_harshness[0].code - x->v.ansi_harshness[0].code;

  if (y->h.code & STD_CODE)
    {
      if (x->h.code & STD_CODE)
	return y->h.distance - x->h.distance;
      return 1;
    }
  if (x->h.code & STD_CODE)
    return -1;

  return y->h.code - x->h.code;
}

int
rank_for_overload_old (x, y)
     struct candidate *x, *y;
{
  if (y->evil - x->evil)
    return y->evil - x->evil;
  if (CONST_HARSHNESS (y->v.old_harshness[0]) ^ CONST_HARSHNESS (x->v.old_harshness[0]))
    return y->v.old_harshness[0] - x->v.old_harshness[0];
  if (y->ellipsis - x->ellipsis)
    return y->ellipsis - x->ellipsis;
  if (y->user - x->user)
    return y->user - x->user;
  if (y->b_or_d - x->b_or_d)
    return y->b_or_d - x->b_or_d;
  return y->easy - x->easy;
}

int
rank_for_overload (x, y)
     struct candidate *x, *y;
{
  if (flag_ansi_overloading)
    return rank_for_overload_ansi (x, y);
  else
    return rank_for_overload_old (x, y);
}

/* Compare two candidates, argument by argument.  */
int
rank_for_ideal (x, y)
     struct candidate *x, *y;
{
  int i;

  if (x->h_len != y->h_len)
    abort ();

  for (i = 0; i < x->h_len; i++)
    {
      if (y->v.ansi_harshness[i].code - x->v.ansi_harshness[i].code)
	return y->v.ansi_harshness[i].code - x->v.ansi_harshness[i].code;
      if ((y->v.ansi_harshness[i].code & STD_CODE)
	  && (y->v.ansi_harshness[i].distance - x->v.ansi_harshness[i].distance))
	return y->v.ansi_harshness[i].distance - x->v.ansi_harshness[i].distance;

      /* They're both the same code.  Now see if we're dealing with an
	 integral promotion that needs a finer grain of accuracy.  */
      if (y->v.ansi_harshness[0].code & PROMO_CODE
	  && (y->v.ansi_harshness[i].int_penalty ^ x->v.ansi_harshness[i].int_penalty))
	return y->v.ansi_harshness[i].int_penalty - x->v.ansi_harshness[i].int_penalty;
    }
  return 0;
}

/* TYPE is the type we wish to convert to.  PARM is the parameter
   we have to work with.  We use a somewhat arbitrary cost function
   to measure this conversion.  */
static struct harshness_code
convert_harshness_ansi (type, parmtype, parm)
     register tree type, parmtype;
     tree parm;
{
  struct harshness_code h;
  register enum tree_code codel;
  register enum tree_code coder;

  h.code = 0;
  h.distance = 0;
  h.int_penalty = 0;

#ifdef GATHER_STATISTICS
  n_convert_harshness++;
#endif

  if (TYPE_PTRMEMFUNC_P (type))
    type = TYPE_PTRMEMFUNC_FN_TYPE (type);
  if (TYPE_PTRMEMFUNC_P (parmtype))
    parmtype = TYPE_PTRMEMFUNC_FN_TYPE (parmtype);

  codel = TREE_CODE (type);
  coder = TREE_CODE (parmtype);

  if (TYPE_MAIN_VARIANT (parmtype) == TYPE_MAIN_VARIANT (type))
    return ZERO_RETURN (h);

  if (coder == ERROR_MARK)
    return EVIL_RETURN (h);

  if (codel == POINTER_TYPE && fntype_p (parmtype))
    {
      tree p1, p2;
      struct harshness_code h1, h2;

      /* Get to the METHOD_TYPE or FUNCTION_TYPE that this might be.  */
      type = TREE_TYPE (type);

      if (coder == POINTER_TYPE)
	{
	  parmtype = TREE_TYPE (parmtype);
	  coder = TREE_CODE (parmtype);
	}

      if (coder != TREE_CODE (type))
	return EVIL_RETURN (h);

      /* We allow the default conversion between function type
	 and pointer-to-function type for free.  */
      if (type == parmtype)
	return ZERO_RETURN (h);

      /* Compare return types.  */
      p1 = TREE_TYPE (type);
      p2 = TREE_TYPE (parmtype);
      h2 = convert_harshness_ansi (p1, p2, NULL_TREE);
      if (h2.code & EVIL_CODE)
	return h2;

      h1.code = TRIVIAL_CODE;
      h1.distance = 0;

      if (h2.distance != 0)
	{
	  tree binfo;

	  /* This only works for pointers.  */
	  if (TREE_CODE (p1) != POINTER_TYPE
	      && TREE_CODE (p1) != REFERENCE_TYPE)
	    return EVIL_RETURN (h);

	  p1 = TREE_TYPE (p1);
	  p2 = TREE_TYPE (p2);
	  /* Don't die if we happen to be dealing with void*.  */
	  if (!IS_AGGR_TYPE (p1) || !IS_AGGR_TYPE (p2))
	    return EVIL_RETURN (h);
	  if (h2.distance < 0)
	    binfo = get_binfo (p2, p1, 0);
	  else
	    binfo = get_binfo (p1, p2, 0);

	  if (! BINFO_OFFSET_ZEROP (binfo))
	    {
	      static int explained = 0;
	      if (h2.distance < 0)
		message_2_types (sorry, "cannot cast `%d' to `%d' at function call site", p2, p1);
	      else
		message_2_types (sorry, "cannot cast `%d' to `%d' at function call site", p1, p2);

	      if (! explained++)
		sorry ("(because pointer values change during conversion)");
	      return EVIL_RETURN (h);
	    }
	}

      h1.code |= h2.code;
      if (h2.distance > h1.distance)
	h1.distance = h2.distance;

      p1 = TYPE_ARG_TYPES (type);
      p2 = TYPE_ARG_TYPES (parmtype);
      while (p1 && TREE_VALUE (p1) != void_type_node
	     && p2 && TREE_VALUE (p2) != void_type_node)
	{
	  h2 = convert_harshness_ansi (TREE_VALUE (p1), TREE_VALUE (p2),
				       NULL_TREE);
	  if (h2.code & EVIL_CODE)
	    return h2;

	  if (h2.distance)
	    {
	      /* This only works for pointers and references. */
	      if (TREE_CODE (TREE_VALUE (p1)) != POINTER_TYPE
		  && TREE_CODE (TREE_VALUE (p1)) != REFERENCE_TYPE)
		return EVIL_RETURN (h);
	      h2.distance = - h2.distance;
	    }

	  h1.code |= h2.code;
	  if (h2.distance > h1.distance)
	    h1.distance = h2.distance;
	  p1 = TREE_CHAIN (p1);
	  p2 = TREE_CHAIN (p2);
	}
      if (p1 == p2)
	return h1;
      if (p2)
	{
	  if (p1)
	    return EVIL_RETURN (h);
	  h1.code |= ELLIPSIS_CODE;
	  return h1;
	}
      if (p1)
	{
	  if (TREE_PURPOSE (p1) == NULL_TREE)
	    h1.code |= EVIL_CODE;
	  return h1;
	}
    }
  else if (codel == POINTER_TYPE && coder == OFFSET_TYPE)
    {
      /* Get to the OFFSET_TYPE that this might be.  */
      type = TREE_TYPE (type);

      if (coder != TREE_CODE (type))
	return EVIL_RETURN (h);

      if (TYPE_OFFSET_BASETYPE (type) == TYPE_OFFSET_BASETYPE (parmtype))
	h.code = 0;
      else if (UNIQUELY_DERIVED_FROM_P (TYPE_OFFSET_BASETYPE (type),
			       TYPE_OFFSET_BASETYPE (parmtype)))
	{
	  h.code = STD_CODE;
	  h.distance = 1;
	}
      else if (UNIQUELY_DERIVED_FROM_P (TYPE_OFFSET_BASETYPE (parmtype),
			       TYPE_OFFSET_BASETYPE (type)))
	{
	  h.code = STD_CODE;
	  h.distance = -1;
	}
      else
	return EVIL_RETURN (h);
      /* Now test the OFFSET_TYPE's target compatibility.  */
      type = TREE_TYPE (type);
      parmtype = TREE_TYPE (parmtype);
    }

  if (coder == UNKNOWN_TYPE)
    {
      if (codel == FUNCTION_TYPE
	  || codel == METHOD_TYPE
	  || (codel == POINTER_TYPE
	      && (TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE
		  || TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE)))
	return TRIVIAL_RETURN (h);
      return EVIL_RETURN (h);
    }

  if (coder == VOID_TYPE)
    return EVIL_RETURN (h);

  if (codel == ENUMERAL_TYPE || codel == INTEGER_TYPE)
    {
      /* Control equivalence of ints an enums.  */

      if (codel == ENUMERAL_TYPE
	  && flag_int_enum_equivalence == 0)
	{
	  /* Enums can be converted to ints, but not vice-versa.  */
	  if (coder != ENUMERAL_TYPE
	      || TYPE_MAIN_VARIANT (type) != TYPE_MAIN_VARIANT (parmtype))
	    return EVIL_RETURN (h);
	}

      /* else enums and ints (almost) freely interconvert.  */

      if (coder == INTEGER_TYPE || coder == ENUMERAL_TYPE)
	{
	  if ((TREE_UNSIGNED (type) ^ TREE_UNSIGNED (parmtype))
	      || codel != coder
	      || TYPE_MODE (type) != TYPE_MODE (parmtype))
	    {
	      /* Make sure a value-preserving condition [from a smaller type to
		 a larger type] is preferred to a possibly value-destroying
		 standard conversion [from a larger type to a smaller type].  */
	      if (TYPE_PRECISION (type) >= TYPE_PRECISION (parmtype))
		{
		  h.code = PROMO_CODE;
		  /* A char, short, wchar_t, etc., should promote to an int if
		     it can handle it, otherwise to an unsigned.  So we'll make
		     an unsigned.  */
		  if (type != integer_type_node)
		    h.int_penalty = 1;
		}
	      else
		h.code = STD_CODE;
	    }

	  /* If the three above conditions didn't trigger, we have found two
	     very similar types.  On systems where they're the same size, we
	     can end up here with TYPE as `long' and PARMTYPE as `int'.  Make
	     sure we realize that, even though they're the same mode, we will
	     have to do some sort of integral promotion on the type, since
	     they're not the same.  */
	  if (! comptypes (type, parmtype, 1) && h.code == 0)
	    {
	      /* This call to common_type will return the best type for the
		 combination.  If it matches TYPE, that means we'll be converting
		 from a so-called smaller type (in PARMTYPE) to the larger in TYPE,
		 thus an integral promotion.  Otherwise, it must be going from a
		 larger type in PARMTYPE to a smaller expected type in TYPE, so we
		 make it a standard conversion instead.  */
	      if (common_type (type, parmtype) == type)
		h.code = PROMO_CODE;
	      else
		h.code = STD_CODE;
	    }
	    
	  return h;
	}
      else if (coder == REAL_TYPE)
	{
	  h.code = STD_CODE;
	  h.distance = 0;
	  return h;
	}
    }

  if (codel == REAL_TYPE)
    {
      if (coder == REAL_TYPE)
	{
	  /* Shun converting among float, double, and long double if a
	     choice exists.  */
	  h.code = PROMO_CODE;
	  return h;
	}
      else if (coder == INTEGER_TYPE || coder == ENUMERAL_TYPE)
	{
	  h.code = STD_CODE;
	  h.distance = 0;
	  return h;
	}
    }

  /* Convert arrays which have not previously been converted.  */
  if (codel == ARRAY_TYPE)
    codel = POINTER_TYPE;
  if (coder == ARRAY_TYPE)
    coder = POINTER_TYPE;

  /* Conversions among pointers */
  if (codel == POINTER_TYPE && coder == POINTER_TYPE)
    {
      register tree ttl = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      register tree ttr = TYPE_MAIN_VARIANT (TREE_TYPE (parmtype));
      int penalty = 4 * (ttl != ttr);

      /* Anything converts to void *.  void * converts to anything.
	 Since these may be `const void *' (etc.) use VOID_TYPE
	 instead of void_type_node.  Otherwise, the targets must be the same,
	 except that we do allow (at some cost) conversion between signed and
	 unsigned pointer types.  */

      if ((TREE_CODE (ttl) == METHOD_TYPE
	   || TREE_CODE (ttl) == FUNCTION_TYPE)
	  && TREE_CODE (ttl) == TREE_CODE (ttr))
	{
	  if (comptypes (ttl, ttr, -1))
	    {
	      h.code = penalty ? STD_CODE : 0;
	      h.distance =  0;
	    }
	  else
	    h.code = EVIL_CODE;
	  return h;
	}

#if 1
      if (TREE_CODE (ttl) != VOID_TYPE && TREE_CODE (ttr) != VOID_TYPE)
	{
	  if (TREE_UNSIGNED (ttl) != TREE_UNSIGNED (ttr))
	    {
	      ttl = unsigned_type (ttl);
	      ttr = unsigned_type (ttr);
	      penalty = 10;
	    }
	  if (! comp_target_types (ttl, ttr, 0))
	    return EVIL_RETURN (h);
	}
#else
      if (!(TREE_CODE (ttl) == VOID_TYPE
	    || TREE_CODE (ttr) == VOID_TYPE
	    || (TREE_UNSIGNED (ttl) ^ TREE_UNSIGNED (ttr)
		&& (ttl = unsigned_type (ttl),
		    ttr = unsigned_type (ttr),
		    penalty = 10, 0))
	    || (comp_target_types (ttl, ttr, 0))))
	return EVIL_RETURN (h);
#endif

      if (penalty == 10 || ttr == ttl)
	{
	  tree tmp1 = TREE_TYPE (type), tmp2 = TREE_TYPE (parmtype);

	  /* If one was unsigned but the other wasn't, then we need to
	     do a standard conversion from T to unsigned T.  */
	  if (penalty == 10)
	    h.code = PROMO_CODE; /* was STD_CODE */
	  else
	    h.code = 0;

	  /* Note conversion from `T*' to `const T*',
	                       or `T*' to `volatile T*'.  */
	  if (ttl == ttr
	      && ((TYPE_READONLY (tmp1) != TREE_READONLY (tmp2))
		  || (TYPE_VOLATILE (tmp1) != TYPE_VOLATILE (tmp2))))
	    h.code |= QUAL_CODE;

	  h.distance = 0;
	  return h;
	}


      if (TREE_CODE (ttl) == RECORD_TYPE && TREE_CODE (ttr) == RECORD_TYPE)
	{
	  int b_or_d = get_base_distance (ttl, ttr, 0, 0);
	  if (b_or_d < 0)
	    {
	      b_or_d = get_base_distance (ttr, ttl, 0, 0);
	      if (b_or_d < 0)
		return EVIL_RETURN (h);
	      h.distance = -b_or_d;
	    }
	  else
	    h.distance = b_or_d;
	  h.code = STD_CODE;
	  return h;
	}

      /* If converting from a `class*' to a `void*', make it
	 less favorable than any inheritance relationship.  */
      if (TREE_CODE (ttl) == VOID_TYPE && IS_AGGR_TYPE (ttr))
	{
	  h.code = STD_CODE;
	  h.distance = CLASSTYPE_MAX_DEPTH (ttr)+1;
	  return h;
	}
      h.code = penalty ? STD_CODE : PROMO_CODE;
      return h;
    }

  if (codel == POINTER_TYPE && coder == INTEGER_TYPE)
    {
      /* This is not a bad match, but don't let it beat
	 integer-enum combinations.  */
      if (parm && integer_zerop (parm))
	{
	  h.code = STD_CODE;
	  h.distance = 0;
	  return h;
	}
    }

  /* C++: one of the types must be a reference type.  */
  {
    tree ttl, ttr;
    register tree intype = TYPE_MAIN_VARIANT (parmtype);
    register enum tree_code form = TREE_CODE (intype);
    int penalty;

    if (codel == REFERENCE_TYPE || coder == REFERENCE_TYPE)
      {
	ttl = TYPE_MAIN_VARIANT (type);

	if (codel == REFERENCE_TYPE)
	  {
	    ttl = TREE_TYPE (ttl);

	    /* When passing a non-const argument into a const reference,
	       dig it a little, so a non-const reference is preferred over
	       this one. (mrs) */
	    if (parm && TREE_READONLY (ttl) && ! TREE_READONLY (parm))
	      penalty = 2;
	    else
	      penalty = 0;

	    ttl = TYPE_MAIN_VARIANT (ttl);

	    if (form == OFFSET_TYPE)
	      {
		intype = TREE_TYPE (intype);
		form = TREE_CODE (intype);
	      }

	    if (form == REFERENCE_TYPE)
	      {
		intype = TYPE_MAIN_VARIANT (TREE_TYPE (intype));

		if (ttl == intype)
		  return ZERO_RETURN (h);
		penalty = 2;
	      }
	    else
	      {
		/* Can reference be built up?  */
		if (ttl == intype && penalty == 0) {
		  /* Because the READONLY and VIRTUAL bits are not always in
		     the type, this extra check is necessary.  The problem
		     should be fixed someplace else, and this extra code
		     removed.

		     Also, if type if a reference, the readonly bits could
		     either be in the outer type (with reference) or on the
		     inner type (the thing being referenced).  (mrs)  */
		  if (parm
		      && ((TREE_READONLY (parm)
			   && ! (TYPE_READONLY (type)
				 || (TREE_CODE (type) == REFERENCE_TYPE
				     && TYPE_READONLY (TREE_TYPE (type)))))
			  || (TREE_SIDE_EFFECTS (parm)
			      && ! (TYPE_VOLATILE (type)
				    || (TREE_CODE (type) == REFERENCE_TYPE
					&& TYPE_VOLATILE (TREE_TYPE (type)))))))
		    penalty = 2;
		  else
		    return ZERO_RETURN (h);
		}
		else
		  penalty = 2;
	      }
	  }
	else if (form == REFERENCE_TYPE)
	  {
	    if (parm)
	      {
		tree tmp = convert_from_reference (parm);
		intype = TYPE_MAIN_VARIANT (TREE_TYPE (tmp));
	      }
	    else
	      {
		intype = parmtype;
		do
		  intype = TREE_TYPE (intype);
		while (TREE_CODE (intype) == REFERENCE_TYPE);
		intype = TYPE_MAIN_VARIANT (intype);
	      }

	    if (ttl == intype)
	      return ZERO_RETURN (h);
	    else
	      penalty = 2;
	  }

	if (TREE_UNSIGNED (ttl) ^ TREE_UNSIGNED (intype))
	  {
	    ttl = unsigned_type (ttl);
	    intype = unsigned_type (intype);
	    penalty += 2;
	  }

	ttr = intype;

	/* If the initializer is not an lvalue, then it does not
	   matter if we make life easier for the programmer
	   by creating a temporary variable with which to
	   hold the result.  */
	if (parm && (coder == INTEGER_TYPE
		     || coder == ENUMERAL_TYPE
		     || coder == REAL_TYPE)
	    && ! lvalue_p (parm))
	  {
	    h = convert_harshness_ansi (ttl, ttr, NULL_TREE);
	    if (penalty > 2 || h.code != 0)
	      h.code |= STD_CODE;
	    else
	      h.code |= TRIVIAL_CODE;
	    h.distance = 0;
	    return h;
	  }

	if (ttl == ttr)
	  {
	    if (penalty > 2)
	      {
		h.code = STD_CODE;
		h.distance = 0;
	      }
	    else
	      {
		h.code = TRIVIAL_CODE;
		/* We set this here so that build_overload_call_real will be
		   able to see the penalty we found, rather than just looking
		   at a TRIVIAL_CODE with no other information.  */
		h.int_penalty = penalty;
	      }
	    return h;
	  }

	/* Pointers to voids always convert for pointers.  But
	   make them less natural than more specific matches.  */
	if (TREE_CODE (ttl) == POINTER_TYPE && TREE_CODE (ttr) == POINTER_TYPE)
	  {
	    if (TREE_TYPE (ttl) == void_type_node
		|| TREE_TYPE (ttr) == void_type_node)
	      {
		h.code = STD_CODE;
		h.distance = 0;
		return h;
	      }
	  }

	if (parm && codel != REFERENCE_TYPE)
	  {
	    h = convert_harshness_ansi (ttl, ttr, NULL_TREE);
	    if (penalty)
	      h.code |= STD_CODE;
	    h.distance = 0;
	    return h;
	  }

	/* Here it does matter.  If this conversion is from derived to base,
	   allow it.  Otherwise, types must be compatible in the strong sense.  */
	if (TREE_CODE (ttl) == RECORD_TYPE && TREE_CODE (ttr) == RECORD_TYPE)
	  {
	    int b_or_d = get_base_distance (ttl, ttr, 0, 0);
	    if (b_or_d < 0)
	      {
		b_or_d = get_base_distance (ttr, ttl, 0, 0);
		if (b_or_d < 0)
		  return EVIL_RETURN (h);
		h.distance = -b_or_d;
	      }
	    /* Say that this conversion is relatively painless.
	       If it turns out that there is a user-defined X(X&)
	       constructor, then that will be invoked, but that's
	       preferable to dealing with other user-defined conversions
	       that may produce surprising results.  */
	    else
	      h.distance = b_or_d;
	    h.code = STD_CODE;
	    return h;
	  }

	if (comp_target_types (ttl, intype, 1))
	  {
	    if (penalty)
	      h.code = STD_CODE;
	    h.distance = 0;
	    return h;
	  }
      }
  }
  if (codel == RECORD_TYPE && coder == RECORD_TYPE)
    {
      int b_or_d = get_base_distance (type, parmtype, 0, 0);
      if (b_or_d < 0)
	{
	  b_or_d = get_base_distance (parmtype, type, 0, 0);
	  if (b_or_d < 0)
	    return EVIL_RETURN (h);
	  h.distance = -b_or_d;
	}
      else
	h.distance = b_or_d;
      h.code = STD_CODE;
      return h;
    }
  return EVIL_RETURN (h);
}

/* TYPE is the type we wish to convert to.  PARM is the parameter
   we have to work with.  We use a somewhat arbitrary cost function
   to measure this conversion.  */
static int
convert_harshness_old (type, parmtype, parm)
     register tree type, parmtype;
     tree parm;
{
  register enum tree_code codel;
  register enum tree_code coder;

#ifdef GATHER_STATISTICS
  n_convert_harshness++;
#endif

  if (TYPE_PTRMEMFUNC_P (type))
    type = TYPE_PTRMEMFUNC_FN_TYPE (type);
  if (TYPE_PTRMEMFUNC_P (parmtype))
    parmtype = TYPE_PTRMEMFUNC_FN_TYPE (parmtype);

  codel = TREE_CODE (type);
  coder = TREE_CODE (parmtype);

  if (TYPE_MAIN_VARIANT (parmtype) == TYPE_MAIN_VARIANT (type))
    return TRIVIAL;

  if (coder == ERROR_MARK)
    return EVIL;

  if (codel == POINTER_TYPE && fntype_p (parmtype))
    {
      tree p1, p2;
      int harshness, new_harshness;

      /* Get to the METHOD_TYPE or FUNCTION_TYPE that this might be.  */
      type = TREE_TYPE (type);

      if (coder == POINTER_TYPE)
	{
	  parmtype = TREE_TYPE (parmtype);
	  coder = TREE_CODE (parmtype);
	}

      if (coder != TREE_CODE (type))
	return EVIL;

      harshness = 0;

      /* We allow the default conversion between function type
	 and pointer-to-function type for free.  */
      if (type == parmtype)
	return TRIVIAL;

      /* Compare return types.  */
      p1 = TREE_TYPE (type);
      p2 = TREE_TYPE (parmtype);
      new_harshness = convert_harshness_old (p1, p2, NULL_TREE);
      if (EVIL_HARSHNESS (new_harshness))
	return EVIL;

      if (BASE_DERIVED_HARSHNESS (new_harshness))
	{
	  tree binfo;

	  /* This only works for pointers.  */
	  if (TREE_CODE (p1) != POINTER_TYPE
	      && TREE_CODE (p1) != REFERENCE_TYPE)
	    return EVIL;

	  p1 = TREE_TYPE (p1);
	  p2 = TREE_TYPE (p2);
	  /* Don't die if we happen to be dealing with void*.  */
	  if (!IS_AGGR_TYPE (p1) || !IS_AGGR_TYPE (p2))
	    return EVIL;
	  if (CONTRAVARIANT_HARSHNESS (new_harshness))
	    binfo = get_binfo (p2, p1, 0);
	  else
	    binfo = get_binfo (p1, p2, 0);

	  if (! BINFO_OFFSET_ZEROP (binfo))
	    {
	      static int explained = 0;
	      if (CONTRAVARIANT_HARSHNESS (new_harshness))
		message_2_types (sorry, "cannot cast `%d' to `%d' at function call site", p2, p1);
	      else
		message_2_types (sorry, "cannot cast `%d' to `%d' at function call site", p1, p2);

	      if (! explained++)
		sorry ("(because pointer values change during conversion)");
	      return EVIL;
	    }
	}

      harshness |= new_harshness;

      p1 = TYPE_ARG_TYPES (type);
      p2 = TYPE_ARG_TYPES (parmtype);
      while (p1 && TREE_VALUE (p1) != void_type_node
	     && p2 && TREE_VALUE (p2) != void_type_node)
	{
	  new_harshness = convert_harshness_old (TREE_VALUE (p1),
						 TREE_VALUE (p2), NULL_TREE);
	  if (EVIL_HARSHNESS (new_harshness))
	    return EVIL;

	  if (BASE_DERIVED_HARSHNESS (new_harshness))
	    {
	      /* This only works for pointers and references. */
	      if (TREE_CODE (TREE_VALUE (p1)) != POINTER_TYPE
		  && TREE_CODE (TREE_VALUE (p1)) != REFERENCE_TYPE)
		return EVIL;
	      new_harshness ^= CONTRAVARIANT_HARSHNESS (new_harshness);
	      harshness |= new_harshness;
	    }
	  /* This trick allows use to accumulate easy type
	     conversions without messing up the bits that encode
	     info about more involved things.  */
	  else if (ONLY_EASY_HARSHNESS (new_harshness))
	    harshness += new_harshness;
	  else
	    harshness |= new_harshness;
	  p1 = TREE_CHAIN (p1);
	  p2 = TREE_CHAIN (p2);
	}
      if (p1 == p2)
	return harshness;
      if (p2)
	return p1 ? EVIL : (harshness | ELLIPSIS_HARSHNESS (-1));
      if (p1)
	return harshness | (TREE_PURPOSE (p1) == NULL_TREE);
    }
  else if (codel == POINTER_TYPE && coder == OFFSET_TYPE)
    {
      /* XXX: Note this is set a few times, but it's never actually
	 used! (bpk) */
      int harshness;

      /* Get to the OFFSET_TYPE that this might be.  */
      type = TREE_TYPE (type);

      if (coder != TREE_CODE (type))
	return EVIL;

      harshness = 0;

      if (TYPE_OFFSET_BASETYPE (type) == TYPE_OFFSET_BASETYPE (parmtype))
	harshness = 0;
      else if (UNIQUELY_DERIVED_FROM_P (TYPE_OFFSET_BASETYPE (type),
			       TYPE_OFFSET_BASETYPE (parmtype)))
	harshness = INT_TO_BD_HARSHNESS (1);
      else if (UNIQUELY_DERIVED_FROM_P (TYPE_OFFSET_BASETYPE (parmtype),
			       TYPE_OFFSET_BASETYPE (type)))
	harshness = CONTRAVARIANT_HARSHNESS (-1);
      else
	return EVIL;
      /* Now test the OFFSET_TYPE's target compatibility.  */
      type = TREE_TYPE (type);
      parmtype = TREE_TYPE (parmtype);
    }

  if (coder == UNKNOWN_TYPE)
    {
      if (codel == FUNCTION_TYPE
	  || codel == METHOD_TYPE
	  || (codel == POINTER_TYPE
	      && (TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE
		  || TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE)))
	return TRIVIAL;
      return EVIL;
    }

  if (coder == VOID_TYPE)
    return EVIL;

  if (codel == ENUMERAL_TYPE || codel == INTEGER_TYPE)
    {
      /* Control equivalence of ints an enums.  */

      if (codel == ENUMERAL_TYPE
	  && flag_int_enum_equivalence == 0)
	{
	  /* Enums can be converted to ints, but not vice-versa.  */
	  if (coder != ENUMERAL_TYPE
	      || TYPE_MAIN_VARIANT (type) != TYPE_MAIN_VARIANT (parmtype))
	    return EVIL;
	}

      /* else enums and ints (almost) freely interconvert.  */

      if (coder == INTEGER_TYPE || coder == ENUMERAL_TYPE)
	{
	  int easy = TREE_UNSIGNED (type) ^ TREE_UNSIGNED (parmtype);
	  if (codel != coder)
	    easy += 1;
	  if (TYPE_MODE (type) != TYPE_MODE (parmtype))
	    easy += 2;
	  return INT_TO_EASY_HARSHNESS (easy);
	}
      else if (coder == REAL_TYPE)
	return INT_TO_EASY_HARSHNESS (4);
    }

  if (codel == REAL_TYPE)
    if (coder == REAL_TYPE)
      /* Shun converting between float and double if a choice exists.  */
      {
	if (TYPE_MODE (type) != TYPE_MODE (parmtype))
	  return INT_TO_EASY_HARSHNESS (2);
	return TRIVIAL;
      }
    else if (coder == INTEGER_TYPE || coder == ENUMERAL_TYPE)
      return INT_TO_EASY_HARSHNESS (4);

  /* convert arrays which have not previously been converted.  */
  if (codel == ARRAY_TYPE)
    codel = POINTER_TYPE;
  if (coder == ARRAY_TYPE)
    coder = POINTER_TYPE;

  /* Conversions among pointers */
  if (codel == POINTER_TYPE && coder == POINTER_TYPE)
    {
      register tree ttl = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      register tree ttr = TYPE_MAIN_VARIANT (TREE_TYPE (parmtype));
      int penalty = 4 * (ttl != ttr);
      /* Anything converts to void *.  void * converts to anything.
	 Since these may be `const void *' (etc.) use VOID_TYPE
	 instead of void_type_node.
	 Otherwise, the targets must be the same,
	 except that we do allow (at some cost) conversion
	 between signed and unsinged pointer types.  */

      if ((TREE_CODE (ttl) == METHOD_TYPE
	   || TREE_CODE (ttl) == FUNCTION_TYPE)
	  && TREE_CODE (ttl) == TREE_CODE (ttr))
	{
	  if (comptypes (ttl, ttr, -1))
	    return INT_TO_EASY_HARSHNESS (penalty);
	  return EVIL;
	}

      if (!(TREE_CODE (ttl) == VOID_TYPE
	    || TREE_CODE (ttr) == VOID_TYPE
	    || (TREE_UNSIGNED (ttl) ^ TREE_UNSIGNED (ttr)
		&& (ttl = unsigned_type (ttl),
		    ttr = unsigned_type (ttr),
		    penalty = 10, 0))
	    || (comp_target_types (ttl, ttr, 0))))
	return EVIL;

      if (penalty == 10)
	return INT_TO_EASY_HARSHNESS (10);
      if (ttr == ttl)
	return INT_TO_BD_HARSHNESS (0);

      if (TREE_CODE (ttl) == RECORD_TYPE && TREE_CODE (ttr) == RECORD_TYPE)
	{
	  int b_or_d = get_base_distance (ttl, ttr, 0, 0);
	  if (b_or_d < 0)
	    {
	      b_or_d = get_base_distance (ttr, ttl, 0, 0);
	      if (b_or_d < 0)
		return EVIL;
	      return CONTRAVARIANT_HARSHNESS (-1);
	    }
	  return INT_TO_BD_HARSHNESS (b_or_d);
	}
      /* If converting from a `class*' to a `void*', make it
	 less favorable than any inheritance relationship.  */
      if (TREE_CODE (ttl) == VOID_TYPE && IS_AGGR_TYPE (ttr))
	return INT_TO_BD_HARSHNESS (CLASSTYPE_MAX_DEPTH (ttr)+1);
      return INT_TO_EASY_HARSHNESS (penalty);
    }

  if (codel == POINTER_TYPE && coder == INTEGER_TYPE)
    {
      /* This is not a bad match, but don't let it beat
	 integer-enum combinations.  */
      if (parm && integer_zerop (parm))
	return INT_TO_EASY_HARSHNESS (4);
    }

  /* C++: Since the `this' parameter of a signature member function
     is represented as a signature pointer to handle default implementations
     correctly, we can have the case that `type' is a signature pointer
     while `parmtype' is a pointer to a signature table.  We don't really
     do any conversions in this case, so just return 0.  */

  if (codel == RECORD_TYPE && coder == POINTER_TYPE
      && IS_SIGNATURE_POINTER (type) && IS_SIGNATURE (TREE_TYPE (parmtype)))
    return 0;

  /* C++: one of the types must be a reference type.  */
  {
    tree ttl, ttr;
    register tree intype = TYPE_MAIN_VARIANT (parmtype);
    register enum tree_code form = TREE_CODE (intype);
    int penalty;

    if (codel == REFERENCE_TYPE || coder == REFERENCE_TYPE)
      {
	ttl = TYPE_MAIN_VARIANT (type);

	if (codel == REFERENCE_TYPE)
	  {
	    ttl = TREE_TYPE (ttl);

	    /* When passing a non-const argument into a const reference,
	       dig it a little, so a non-const reference is preferred over
	       this one. (mrs) */
	    if (parm && TREE_READONLY (ttl) && ! TREE_READONLY (parm))
	      penalty = 2;
	    else
	      penalty = 0;

	    ttl = TYPE_MAIN_VARIANT (ttl);

	    if (form == OFFSET_TYPE)
	      {
		intype = TREE_TYPE (intype);
		form = TREE_CODE (intype);
	      }

	    if (form == REFERENCE_TYPE)
	      {
		intype = TYPE_MAIN_VARIANT (TREE_TYPE (intype));

		if (ttl == intype)
		  return TRIVIAL;
		penalty = 2;
	      }
	    else
	      {
		/* Can reference be built up?  */
		if (ttl == intype && penalty == 0) {
		  /* Because the READONLY bits and VIRTUAL bits are not always
		     in the type, this extra check is necessary.  The problem
		     should be fixed someplace else, and this extra code
		     removed.

		     Also, if type if a reference, the readonly bits could
		     either be in the outer type (with reference) or on the
		     inner type (the thing being referenced).  (mrs)  */
		  if (parm
		      && ((TREE_READONLY (parm)
			   && ! (TYPE_READONLY (type)
				 || (TREE_CODE (type) == REFERENCE_TYPE
				     && TYPE_READONLY (TREE_TYPE (type)))))
			  || (TREE_SIDE_EFFECTS (parm)
			      && ! (TYPE_VOLATILE (type)
				    || (TREE_CODE (type) == REFERENCE_TYPE
					&& TYPE_VOLATILE (TREE_TYPE (type)))))))
		    penalty = 2;
		  else
		    return TRIVIAL;
		}
		else
		  penalty = 2;
	      }
	  }
	else if (form == REFERENCE_TYPE)
	  {
	    if (parm)
	      {
		tree tmp = convert_from_reference (parm);
		intype = TYPE_MAIN_VARIANT (TREE_TYPE (tmp));
	      }
	    else
	      {
		intype = parmtype;
		do
		  {
		    intype = TREE_TYPE (intype);
		  }
		while (TREE_CODE (intype) == REFERENCE_TYPE);
		intype = TYPE_MAIN_VARIANT (intype);
	      }

	    if (ttl == intype)
	      return TRIVIAL;
	    else
	      penalty = 2;
	  }

	if (TREE_UNSIGNED (ttl) ^ TREE_UNSIGNED (intype))
	  {
	    ttl = unsigned_type (ttl);
	    intype = unsigned_type (intype);
	    penalty += 2;
	  }

	ttr = intype;

	/* If the initializer is not an lvalue, then it does not
	   matter if we make life easier for the programmer
	   by creating a temporary variable with which to
	   hold the result.  */
	if (parm && (coder == INTEGER_TYPE
		     || coder == ENUMERAL_TYPE
		     || coder == REAL_TYPE)
	    && ! lvalue_p (parm))
	  return (convert_harshness_old (ttl, ttr, NULL_TREE)
		  | INT_TO_EASY_HARSHNESS (penalty));

	if (ttl == ttr)
	  {
	    if (penalty)
	      return INT_TO_EASY_HARSHNESS (penalty);
	    return INT_TO_BD_HARSHNESS (0);
	  }

	/* Pointers to voids always convert for pointers.  But
	   make them less natural than more specific matches.  */
	if (TREE_CODE (ttl) == POINTER_TYPE && TREE_CODE (ttr) == POINTER_TYPE)
	  if (TREE_TYPE (ttl) == void_type_node
	      || TREE_TYPE (ttr) == void_type_node)
	    return INT_TO_EASY_HARSHNESS (penalty+1);

	if (parm && codel != REFERENCE_TYPE)
	  return (convert_harshness_old (ttl, ttr, NULL_TREE)
		  | INT_TO_EASY_HARSHNESS (penalty));

	/* Here it does matter.  If this conversion is from
	   derived to base, allow it.  Otherwise, types must
	   be compatible in the strong sense.  */
	if (TREE_CODE (ttl) == RECORD_TYPE && TREE_CODE (ttr) == RECORD_TYPE)
	  {
	    int b_or_d = get_base_distance (ttl, ttr, 0, 0);
	    if (b_or_d < 0)
	      {
		b_or_d = get_base_distance (ttr, ttl, 0, 0);
		if (b_or_d < 0)
		  return EVIL;
		return CONTRAVARIANT_HARSHNESS (-1);
	      }
	    /* Say that this conversion is relatively painless.
	       If it turns out that there is a user-defined X(X&)
	       constructor, then that will be invoked, but that's
	       preferable to dealing with other user-defined conversions
	       that may produce surprising results.  */
	    return INT_TO_BD_HARSHNESS (b_or_d);
	  }

	if (comp_target_types (ttl, intype, 1))
	  return INT_TO_EASY_HARSHNESS (penalty);
      }
  }
  if (codel == RECORD_TYPE && coder == RECORD_TYPE)
    {
      int b_or_d = get_base_distance (type, parmtype, 0, 0);
      if (b_or_d < 0)
	{
	  b_or_d = get_base_distance (parmtype, type, 0, 0);
	  if (b_or_d < 0)
	    return EVIL;
	  return CONTRAVARIANT_HARSHNESS (-1);
	}
      return INT_TO_BD_HARSHNESS (b_or_d);
    }
  return EVIL;
}

#ifdef DEBUG_MATCHING
static char *
print_harshness (h)
     struct harshness_code *h;
{
  static char buf[1024];
  char tmp[1024];

  bzero (buf, 1024 * sizeof (char));
  strcat (buf, "codes=[");
  if (h->code & EVIL_CODE)
    strcat (buf, "EVIL");
  if (h->code & CONST_CODE)
    strcat (buf, " CONST");
  if (h->code & ELLIPSIS_CODE)
    strcat (buf, " ELLIPSIS");
  if (h->code & USER_CODE)
    strcat (buf, " USER");
  if (h->code & STD_CODE)
    strcat (buf, " STD");
  if (h->code & PROMO_CODE)
    strcat (buf, " PROMO");
  if (h->code & QUAL_CODE)
    strcat (buf, " QUAL");
  if (h->code & TRIVIAL_CODE)
    strcat (buf, " TRIVIAL");
  if (buf[0] == '\0')
    strcat (buf, "0");

  sprintf (tmp, "] distance=%d int_penalty=%d", h->distance, h->int_penalty);

  strcat (buf, tmp);

  return buf;
}
#endif

/* Algorithm: For each argument, calculate how difficult it is to
   make FUNCTION accept that argument.  If we can easily tell that
   FUNCTION won't be acceptable to one of the arguments, then we
   don't need to compute the ease of converting the other arguments,
   since it will never show up in the intersection of all arguments'
   favorite functions.

   Conversions between builtin and user-defined types are allowed, but
   no function involving such a conversion is preferred to one which
   does not require such a conversion.  Furthermore, such conversions
   must be unique.  */

void
compute_conversion_costs_ansi (function, tta_in, cp, arglen)
     tree function;
     tree tta_in;
     struct candidate *cp;
     int arglen;
{
  tree ttf_in = TYPE_ARG_TYPES (TREE_TYPE (function));
  tree ttf = ttf_in;
  tree tta = tta_in;

  /* Start out with no strikes against.  */
  int evil_strikes = 0;
  int ellipsis_strikes = 0;
  int user_strikes = 0;
  int b_or_d_strikes = 0;
  int easy_strikes = 0;

  int strike_index = 0, win;
  struct harshness_code lose;

#ifdef GATHER_STATISTICS
  n_compute_conversion_costs++;
#endif

  cp->function = function;
  cp->arg = tta ? TREE_VALUE (tta) : NULL_TREE;
  cp->u.bad_arg = 0;		/* optimistic!  */

  cp->h.code = 0;
  cp->h.distance = 0;
  cp->h.int_penalty = 0;
  bzero (cp->v.ansi_harshness,
	 (cp->h_len + 1) * sizeof (struct harshness_code));

  while (ttf && tta)
    {
      struct harshness_code h;

      if (ttf == void_list_node)
	break;

      if (type_unknown_p (TREE_VALUE (tta)))
	{	  
	  /* Must perform some instantiation here.  */
	  tree rhs = TREE_VALUE (tta);
	  tree lhstype = TREE_VALUE (ttf);

	  /* Keep quiet about possible contravariance violations.  */
	  int old_inhibit_warnings = inhibit_warnings;
	  inhibit_warnings = 1;

	  /* @@ This is to undo what `grokdeclarator' does to
	     parameter types.  It really should go through
	     something more general.  */

	  TREE_TYPE (tta) = unknown_type_node;
	  rhs = instantiate_type (lhstype, rhs, 0);
	  inhibit_warnings = old_inhibit_warnings;

	  if (TREE_CODE (rhs) == ERROR_MARK)
	    h.code = EVIL_CODE;
	  else
	    h = convert_harshness_ansi (lhstype, TREE_TYPE (rhs), rhs);
	}
      else
	{
#ifdef DEBUG_MATCHING
	  static tree old_function = NULL_TREE;

	  if (!old_function || function != old_function)
	    {
	      cp_error ("trying %D", function);
	      old_function = function;
	    }

	  cp_error ("      doing (%T) %E against arg %T",
		    TREE_TYPE (TREE_VALUE (tta)), TREE_VALUE (tta),
		    TREE_VALUE (ttf));
#endif

	  h = convert_harshness_ansi (TREE_VALUE (ttf),
				      TREE_TYPE (TREE_VALUE (tta)),
				      TREE_VALUE (tta));

#ifdef DEBUG_MATCHING
	  cp_error ("     evaluated %s", print_harshness (&h));
#endif
	}

      cp->v.ansi_harshness[strike_index] = h;
      if ((h.code & EVIL_CODE)
	  || ((h.code & STD_CODE) && h.distance < 0))
	{
	  cp->u.bad_arg = strike_index;
	  evil_strikes = 1;
	}
     else if (h.code & ELLIPSIS_CODE)
       ellipsis_strikes += 1;
#if 0
      /* This is never set by `convert_harshness_ansi'.  */
      else if (h.code & USER_CODE)
	{
	  user_strikes += 1;
	}
#endif
      else
	{
	  if ((h.code & STD_CODE) && h.distance)
	    {
	      if (h.distance > b_or_d_strikes)
		b_or_d_strikes = h.distance;
	    }
	  else
	    easy_strikes += (h.code & (STD_CODE|PROMO_CODE|TRIVIAL_CODE));
	  cp->h.code |= h.code;
	  /* Make sure we communicate this.  */
	  cp->h.int_penalty += h.int_penalty;
	}

      ttf = TREE_CHAIN (ttf);
      tta = TREE_CHAIN (tta);
      strike_index += 1;
    }

  if (tta)
    {
      /* ran out of formals, and parmlist is fixed size.  */
      if (ttf /* == void_type_node */)
	{
	  cp->h.code = EVIL_CODE;
	  cp->u.bad_arg = -1;
	  return;
	}
      else
	{
	  struct harshness_code h;
	  int l = list_length (tta);
	  ellipsis_strikes += l;
	  h.code = ELLIPSIS_CODE;
	  h.distance = 0;
	  h.int_penalty = 0;
	  for (; l; --l)
	    cp->v.ansi_harshness[strike_index++] = h;
	}
    }
  else if (ttf && ttf != void_list_node)
    {
      /* ran out of actuals, and no defaults.  */
      if (TREE_PURPOSE (ttf) == NULL_TREE)
	{
	  cp->h.code = EVIL_CODE;
	  cp->u.bad_arg = -2;
	  return;
	}
      /* Store index of first default.  */
      cp->v.ansi_harshness[arglen].distance = strike_index+1;
    }
  else
    cp->v.ansi_harshness[arglen].distance = 0;

  /* Argument list lengths work out, so don't need to check them again.  */
  if (evil_strikes)
    {
      /* We do not check for derived->base conversions here, since in
	 no case would they give evil strike counts, unless such conversions
	 are somehow ambiguous.  */

      /* See if any user-defined conversions apply.
         But make sure that we do not loop.  */
      static int dont_convert_types = 0;

      if (dont_convert_types)
	{
	  cp->h.code = EVIL_CODE;
	  return;
	}

      win = 0;			/* Only get one chance to win.  */
      ttf = TYPE_ARG_TYPES (TREE_TYPE (function));
      tta = tta_in;
      strike_index = 0;
      evil_strikes = 0;

      while (ttf && tta)
	{
	  if (ttf == void_list_node)
	    break;

	  lose = cp->v.ansi_harshness[strike_index];
	  if ((lose.code & EVIL_CODE)
	      || ((lose.code & STD_CODE) && lose.distance < 0))
	    {
	      tree actual_type = TREE_TYPE (TREE_VALUE (tta));
	      tree formal_type = TREE_VALUE (ttf);

	      dont_convert_types = 1;

	      if (TREE_CODE (formal_type) == REFERENCE_TYPE)
		formal_type = TREE_TYPE (formal_type);
	      if (TREE_CODE (actual_type) == REFERENCE_TYPE)
		actual_type = TREE_TYPE (actual_type);

	      if (formal_type != error_mark_node
		  && actual_type != error_mark_node)
		{
		  formal_type = TYPE_MAIN_VARIANT (formal_type);
		  actual_type = TYPE_MAIN_VARIANT (actual_type);

		  if (TYPE_HAS_CONSTRUCTOR (formal_type))
		    {
		      /* If it has a constructor for this type,
			 try to use it.  */
		      /* @@ There is no way to save this result yet, so
			 success is a NULL_TREE for now.  */
		      if (convert_to_aggr (formal_type, TREE_VALUE (tta), 0, 1)
			  != error_mark_node)
			win++;
		    }
		  if (TYPE_LANG_SPECIFIC (actual_type)
		      && TYPE_HAS_CONVERSION (actual_type))
		    {
		      if (TREE_CODE (formal_type) == INTEGER_TYPE
			  && TYPE_HAS_INT_CONVERSION (actual_type))
			win++;
		      else if (TREE_CODE (formal_type) == REAL_TYPE
			       && TYPE_HAS_REAL_CONVERSION (actual_type))
			win++;
		      else
			{
			  tree conv;
			  /* Don't issue warnings since we're only groping
			     around for the right answer, we haven't yet
			     committed to going with this solution.  */
			  int old_inhibit_warnings = inhibit_warnings;

			  inhibit_warnings = 1;
			  conv = build_type_conversion (CALL_EXPR, TREE_VALUE (ttf), TREE_VALUE (tta), 0);
			  inhibit_warnings = old_inhibit_warnings;

			  if (conv)
			    {
			      if (conv == error_mark_node)
				win += 2;
			      else
				win++;
			    }
			  else if (TREE_CODE (TREE_VALUE (ttf)) == REFERENCE_TYPE)
			    {
			      conv = build_type_conversion (CALL_EXPR, formal_type, TREE_VALUE (tta), 0);
			      if (conv)
				{
				  if (conv == error_mark_node)
				    win += 2;
				  else
				    win++;
				}
			    }
			}
		    }
		}
	      dont_convert_types = 0;

	      if (win == 1)
		{
		  user_strikes += 1;
		  cp->v.ansi_harshness[strike_index].code = USER_CODE;
		  win = 0;
		}
	      else
		{
		  if (cp->u.bad_arg > strike_index)
		    cp->u.bad_arg = strike_index;

		  evil_strikes = win ? 2 : 1;
		  break;
		}
	    }

	  ttf = TREE_CHAIN (ttf);
	  tta = TREE_CHAIN (tta);
	  strike_index += 1;
	}
    }

  /* Const member functions get a small penalty because defaulting
     to const is less useful than defaulting to non-const. */
  /* This is bogus, it does not correspond to anything in the ARM.
     This code will be fixed when this entire section is rewritten
     to conform to the ARM.  (mrs)  */
  if (TREE_CODE (TREE_TYPE (function)) == METHOD_TYPE)
    {
      tree this_parm = TREE_VALUE (ttf_in);

      if (TREE_CODE (this_parm) == RECORD_TYPE	/* Is `this' a sig ptr?  */
	    ? TYPE_READONLY (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (this_parm))))
	    : TYPE_READONLY (TREE_TYPE (this_parm)))
	{
	  cp->v.ansi_harshness[0].code |= TRIVIAL_CODE;
	  ++easy_strikes;
	}
      else
	{
	  /* Calling a non-const member function from a const member function
	     is probably invalid, but for now we let it only draw a warning.
	     We indicate that such a mismatch has occurred by setting the
	     harshness to a maximum value.  */
	  if (TREE_CODE (TREE_TYPE (TREE_VALUE (tta_in))) == POINTER_TYPE
	      && (TYPE_READONLY (TREE_TYPE (TREE_TYPE (TREE_VALUE (tta_in))))))
	    cp->v.ansi_harshness[0].code |= CONST_CODE;
	}
    }

  if (evil_strikes)
    cp->h.code = EVIL_CODE;
  if (ellipsis_strikes)
    cp->h.code |= ELLIPSIS_CODE;
  if (user_strikes)
    cp->h.code |= USER_CODE;
#ifdef DEBUG_MATCHING
  cp_error ("final eval %s", print_harshness (&cp->h));
#endif
}

void
compute_conversion_costs_old (function, tta_in, cp, arglen)
     tree function;
     tree tta_in;
     struct candidate *cp;
     int arglen;
{
  tree ttf_in = TYPE_ARG_TYPES (TREE_TYPE (function));
  tree ttf = ttf_in;
  tree tta = tta_in;

  /* Start out with no strikes against.  */
  int evil_strikes = 0;
  int ellipsis_strikes = 0;
  int user_strikes = 0;
  int b_or_d_strikes = 0;
  int easy_strikes = 0;

  int strike_index = 0, win, lose;

#ifdef GATHER_STATISTICS
  n_compute_conversion_costs++;
#endif

  cp->function = function;
  cp->arg = tta ? TREE_VALUE (tta) : NULL_TREE;
  cp->u.bad_arg = 0;		/* optimistic!  */

  bzero (cp->v.old_harshness, (cp->h_len + 1) * sizeof (unsigned short));

  while (ttf && tta)
    {
      int harshness;

      if (ttf == void_list_node)
	break;

      if (type_unknown_p (TREE_VALUE (tta)))
	{	  
	  /* Must perform some instantiation here.  */
	  tree rhs = TREE_VALUE (tta);
	  tree lhstype = TREE_VALUE (ttf);

	  /* Keep quiet about possible contravariance violations.  */
	  int old_inhibit_warnings = inhibit_warnings;
	  inhibit_warnings = 1;

	  /* @@ This is to undo what `grokdeclarator' does to
	     parameter types.  It really should go through
	     something more general.  */

	  TREE_TYPE (tta) = unknown_type_node;
	  rhs = instantiate_type (lhstype, rhs, 0);
	  inhibit_warnings = old_inhibit_warnings;

	  if (TREE_CODE (rhs) == ERROR_MARK)
	    harshness = 1;
	  else
	    {
	      harshness = convert_harshness_old (lhstype, TREE_TYPE (rhs),
						 rhs);
	      /* harshness |= 2; */
	    }
	}
      else
	harshness = convert_harshness_old (TREE_VALUE (ttf),
					   TREE_TYPE (TREE_VALUE (tta)),
					   TREE_VALUE (tta));

      cp->v.old_harshness[strike_index] = harshness;
      if (EVIL_HARSHNESS (harshness)
	  || CONTRAVARIANT_HARSHNESS (harshness))
	{
	  cp->u.bad_arg = strike_index;
	  evil_strikes = 1;
	}
     else if (ELLIPSIS_HARSHNESS (harshness))
	{
	  ellipsis_strikes += 1;
	}
#if 0
      /* This is never set by `convert_harshness_old'.  */
      else if (USER_HARSHNESS (harshness))
	{
	  user_strikes += 1;
	}
#endif
      else if (BASE_DERIVED_HARSHNESS (harshness))
	{
	  b_or_d_strikes += INT_FROM_BD_HARSHNESS (harshness);
	}
      else
	easy_strikes += INT_FROM_EASY_HARSHNESS (harshness);
      ttf = TREE_CHAIN (ttf);
      tta = TREE_CHAIN (tta);
      strike_index += 1;
    }

  if (tta)
    {
      /* ran out of formals, and parmlist is fixed size.  */
      if (ttf /* == void_type_node */)
	{
	  cp->evil = 1;
	  cp->u.bad_arg = -1;
	  return;
	}
      else ellipsis_strikes += list_length (tta);
    }
  else if (ttf && ttf != void_list_node)
    {
      /* ran out of actuals, and no defaults.  */
      if (TREE_PURPOSE (ttf) == NULL_TREE)
	{
	  cp->evil = 1;
	  cp->u.bad_arg = -2;
	  return;
	}
      /* Store index of first default.  */
      cp->v.old_harshness[arglen] = strike_index+1;
    }
  else
    cp->v.old_harshness[arglen] = 0;

  /* Argument list lengths work out, so don't need to check them again.  */
  if (evil_strikes)
    {
      /* We do not check for derived->base conversions here, since in
	 no case would they give evil strike counts, unless such conversions
	 are somehow ambiguous.  */

      /* See if any user-defined conversions apply.
         But make sure that we do not loop.  */
      static int dont_convert_types = 0;

      if (dont_convert_types)
	{
	  cp->evil = 1;
	  return;
	}

      win = 0;			/* Only get one chance to win.  */
      ttf = TYPE_ARG_TYPES (TREE_TYPE (function));
      tta = tta_in;
      strike_index = 0;
      evil_strikes = 0;

      while (ttf && tta)
	{
	  if (ttf == void_list_node)
	    break;

	  lose = cp->v.old_harshness[strike_index];
	  if (EVIL_HARSHNESS (lose)
	      || CONTRAVARIANT_HARSHNESS (lose))
	    {
	      tree actual_type = TREE_TYPE (TREE_VALUE (tta));
	      tree formal_type = TREE_VALUE (ttf);

	      dont_convert_types = 1;

	      if (TREE_CODE (formal_type) == REFERENCE_TYPE)
		formal_type = TREE_TYPE (formal_type);
	      if (TREE_CODE (actual_type) == REFERENCE_TYPE)
		actual_type = TREE_TYPE (actual_type);

	      if (formal_type != error_mark_node
		  && actual_type != error_mark_node)
		{
		  formal_type = TYPE_MAIN_VARIANT (formal_type);
		  actual_type = TYPE_MAIN_VARIANT (actual_type);

		  if (TYPE_HAS_CONSTRUCTOR (formal_type))
		    {
		      /* If it has a constructor for this type, try to use it.  */
		      if (convert_to_aggr (formal_type, TREE_VALUE (tta), 0, 1)
			  != error_mark_node)
			{
			  /* @@ There is no way to save this result yet.
			     @@ So success is NULL_TREE for now.  */
			  win++;
			}
		    }
		  if (TYPE_LANG_SPECIFIC (actual_type) && TYPE_HAS_CONVERSION (actual_type))
		    {
		      if (TREE_CODE (formal_type) == INTEGER_TYPE
			  && TYPE_HAS_INT_CONVERSION (actual_type))
			win++;
		      else if (TREE_CODE (formal_type) == REAL_TYPE
			       && TYPE_HAS_REAL_CONVERSION (actual_type))
			win++;
		      else
			{
			  tree conv = build_type_conversion (CALL_EXPR, TREE_VALUE (ttf), TREE_VALUE (tta), 0);
			  if (conv)
			    {
			      if (conv == error_mark_node)
				win += 2;
			      else
				win++;
			    }
			  else if (TREE_CODE (TREE_VALUE (ttf)) == REFERENCE_TYPE)
			    {
			      conv = build_type_conversion (CALL_EXPR, formal_type, TREE_VALUE (tta), 0);
			      if (conv)
				{
				  if (conv == error_mark_node)
				    win += 2;
				  else
				    win++;
				}
			    }
			}
		    }
		}
	      dont_convert_types = 0;

	      if (win == 1)
		{
		  user_strikes += 1;
		  cp->v.old_harshness[strike_index] = USER_HARSHNESS (-1);
		  win = 0;
		}
	      else
		{
		  if (cp->u.bad_arg > strike_index)
		    cp->u.bad_arg = strike_index;

		  evil_strikes = win ? 2 : 1;
		  break;
		}
	    }

	  ttf = TREE_CHAIN (ttf);
	  tta = TREE_CHAIN (tta);
	  strike_index += 1;
	}
    }

  /* Const member functions get a small penalty because defaulting
     to const is less useful than defaulting to non-const. */
  /* This is bogus, it does not correspond to anything in the ARM.
     This code will be fixed when this entire section is rewritten
     to conform to the ARM.  (mrs)  */
  if (TREE_CODE (TREE_TYPE (function)) == METHOD_TYPE)
    {
      tree this_parm = TREE_VALUE (ttf_in);

      if (TREE_CODE (this_parm) == RECORD_TYPE	/* Is `this' a sig ptr?  */
	    ? TYPE_READONLY (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (this_parm))))
	    : TYPE_READONLY (TREE_TYPE (this_parm)))
	{
	  cp->v.old_harshness[0] += INT_TO_EASY_HARSHNESS (1);
	  ++easy_strikes;
	}
      else
	{
	  /* Calling a non-const member function from a const member function
	     is probably invalid, but for now we let it only draw a warning.
	     We indicate that such a mismatch has occurred by setting the
	     harshness to a maximum value.  */
	  if (TREE_CODE (TREE_TYPE (TREE_VALUE (tta_in))) == POINTER_TYPE
	      && (TYPE_READONLY (TREE_TYPE (TREE_TYPE (TREE_VALUE (tta_in))))))
	    cp->v.old_harshness[0] |= CONST_HARSHNESS (-1);
	}
    }

  cp->evil = evil_strikes;
  cp->ellipsis = ellipsis_strikes;
  cp->user = user_strikes;
  cp->b_or_d = b_or_d_strikes;
  cp->easy = easy_strikes;
}

void
compute_conversion_costs (function, tta_in, cp, arglen)
     tree function;
     tree tta_in;
     struct candidate *cp;
     int arglen;
{
  if (flag_ansi_overloading)
    compute_conversion_costs_ansi (function, tta_in, cp, arglen);
  else
    compute_conversion_costs_old (function, tta_in, cp, arglen);
}

/* When one of several possible overloaded functions and/or methods
   can be called, choose the best candidate for overloading.

   BASETYPE is the context from which we start method resolution
   or NULL if we are comparing overloaded functions.
   CANDIDATES is the array of candidates we have to choose from.
   N_CANDIDATES is the length of CANDIDATES.
   PARMS is a TREE_LIST of parameters to the function we'll ultimately
   choose.  It is modified in place when resolving methods.  It is not
   modified in place when resolving overloaded functions.
   LEN is the length of the parameter list.  */

static struct candidate *
ideal_candidate_old (basetype, candidates, n_candidates, parms, len)
     tree basetype;
     struct candidate *candidates;
     int n_candidates;
     tree parms;
     int len;
{
  struct candidate *cp = candidates + n_candidates;
  int index, i;
  tree ttf;

  qsort (candidates,		/* char *base */
	 n_candidates,		/* int nel */
	 sizeof (struct candidate), /* int width */
	 rank_for_overload);	/* int (*compar)() */

  /* If the best candidate requires user-defined conversions,
     and its user-defined conversions are a strict subset
     of all other candidates requiring user-defined conversions,
     then it is, in fact, the best.  */
  for (i = -1; cp + i != candidates; i--)
    if (cp[i].user == 0)
      break;

  if (i < -1)
    {
      tree ttf0;

      /* Check that every other candidate requires those conversions
	 as a strict subset of their conversions.  */
      if (cp[i].user == cp[-1].user)
	goto non_subset;

      /* Look at subset relationship more closely.  */
      while (i != -1)
	{
	  for (ttf = TYPE_ARG_TYPES (TREE_TYPE (cp[i].function)),
	       ttf0 = TYPE_ARG_TYPES (TREE_TYPE (cp[-1].function)),
	       index = 0; index < len; index++)
	    {
	      if (USER_HARSHNESS (cp[i].v.old_harshness[index]))
		{
		  /* If our "best" candidate also needs a conversion,
		     it must be the same one.  */
		  if (USER_HARSHNESS (cp[-1].v.old_harshness[index])
		      && TREE_VALUE (ttf) != TREE_VALUE (ttf0))
		    goto non_subset;
		}
	      ttf = TREE_CHAIN (ttf);
	      ttf0 = TREE_CHAIN (ttf0);
	      /* Handle `...' gracefully.  */
	      if (ttf == NULL_TREE || ttf0 == NULL_TREE)
		break;
	    }
	  i++;
	}
      /* The best was the best.  */
      return cp - 1;
    non_subset:
      /* Use other rules for determining "bestness".  */
      ;
    }

  /* If the best two candidates we find require user-defined
     conversions, we may need to report and error message.  */
  if (cp[-1].user && cp[-2].user
      && (cp[-1].b_or_d || cp[-2].b_or_d == 0))
    {
      /* If the best two methods found involved user-defined
	 type conversions, then we must see whether one
	 of them is exactly what we wanted.  If not, then
	 we have an ambiguity.  */
      int best = 0;
      tree tta = parms;
      tree f1;
#if 0
      /* for LUCID */
      tree p1;
#endif

      /* Stash all of our parameters in safe places
	 so that we can perform type conversions in place.  */
      while (tta)
	{
	  TREE_PURPOSE (tta) = TREE_VALUE (tta);
	  tta = TREE_CHAIN (tta);
	}

      i = 0;
      do
	{
	  int exact_conversions = 0;

	  i -= 1;
	  tta = parms;
	  if (DECL_STATIC_FUNCTION_P (cp[i].function))
	    tta = TREE_CHAIN (tta);
	  /* special note, we don't go through len parameters, because we
	     may only need len-1 parameters because of a call to a static
	     member. */
	  for (ttf = TYPE_ARG_TYPES (TREE_TYPE (cp[i].function)), index = 0;
	       tta;
	       tta = TREE_CHAIN (tta), ttf = TREE_CHAIN (ttf), index++)
	    {
	      /* If this is a varargs function, there's no conversion to do,
		 but don't accept an arg that needs a copy ctor.  */
	      if (ttf == NULL_TREE)
		{
		  /* FIXME: verify that we cannot get here with an
		     arg that needs a ctor.  */
		  break;
		}

	      if (USER_HARSHNESS (cp[i].v.old_harshness[index]))
		{
		  tree this_parm = build_type_conversion (CALL_EXPR, TREE_VALUE (ttf), TREE_PURPOSE (tta), 2);
		  if (basetype != NULL_TREE)
		    TREE_VALUE (tta) = this_parm;
		  if (this_parm)
		    {
		      if (TREE_CODE (this_parm) != CONVERT_EXPR
			  && (TREE_CODE (this_parm) != NOP_EXPR
			      || comp_target_types (TREE_TYPE (this_parm),
						    TREE_TYPE (TREE_OPERAND (this_parm, 0)), 1)))
			exact_conversions += 1;
		    }
		  else if (PROMOTES_TO_AGGR_TYPE (TREE_VALUE (ttf), REFERENCE_TYPE))
		    {
		      /* To get here we had to have succeeded via
			 a constructor.  */
		      TREE_VALUE (tta) = TREE_PURPOSE (tta);
		      exact_conversions += 1;
		    }
		}
	    }
	  if (exact_conversions == cp[i].user)
	    {
	      if (best == 0)
		{
		  best = i;
		  f1 = cp[best].function;
#if 0
		  /* For LUCID */
		  p1 = TYPE_ARG_TYPES (TREE_TYPE (f1));
#endif
		}
	      else
		{
		  /* Don't complain if next best is from base class.  */
		  tree f2 = cp[i].function;

		  if (TREE_CODE (TREE_TYPE (f1)) == METHOD_TYPE
		      && TREE_CODE (TREE_TYPE (f2)) == METHOD_TYPE
		      && BASE_DERIVED_HARSHNESS (cp[i].v.old_harshness[0])
		      && cp[best].v.old_harshness[0] < cp[i].v.old_harshness[0])
		    {
#if 0
		      tree p2 = TYPE_ARG_TYPES (TREE_TYPE (f2));
		      /* For LUCID.  */
		      if (! compparms (TREE_CHAIN (p1), TREE_CHAIN (p2), 1))
			goto ret0;
		      else
#endif
			continue;
		    }
		  else
		    {
		      /* Ensure that there's nothing ambiguous about these
			 two fns.  */
		      int identical = 1;
		      for (index = 0; index < len; index++)
			{
			  /* Type conversions must be piecewise equivalent.  */
			  if (USER_HARSHNESS (cp[best].v.old_harshness[index])
			      != USER_HARSHNESS (cp[i].v.old_harshness[index]))
			    goto ret0;
			  /* If there's anything we like better about the
			     other function, consider it ambiguous.  */
			  if (cp[i].v.old_harshness[index] < cp[best].v.old_harshness[index])
			    goto ret0;
			  /* If any single one it diffent, then the whole is
			     not identical.  */
			  if (cp[i].v.old_harshness[index] != cp[best].v.old_harshness[index])
			    identical = 0;
			}

		      /* If we can't tell the difference between the two, it
			 is ambiguous.  */
		      if (identical)
			goto ret0;

		      /* If we made it to here, it means we're satisfied that
			 BEST is still best.  */
		      continue;
		    }
		}
	    }
	} while (cp + i != candidates);

      if (best)
	{
	  int exact_conversions = cp[best].user;
	  tta = parms;
	  if (DECL_STATIC_FUNCTION_P (cp[best].function))
	    tta = TREE_CHAIN (parms);
	  for (ttf = TYPE_ARG_TYPES (TREE_TYPE (cp[best].function)), index = 0;
	       exact_conversions > 0;
	       tta = TREE_CHAIN (tta), ttf = TREE_CHAIN (ttf), index++)
	    {
	      if (USER_HARSHNESS (cp[best].v.old_harshness[index]))
		{
		  /* We must now fill in the slot we left behind.
		     @@ This could be optimized to use the value previously
		     @@ computed by build_type_conversion in some cases.  */
		  if (basetype != NULL_TREE)
		    TREE_VALUE (tta) = convert (TREE_VALUE (ttf), TREE_PURPOSE (tta));
		  exact_conversions -= 1;
		}
	      else
		TREE_VALUE (tta) = TREE_PURPOSE (tta);
	    }
	  return cp + best;
	}
      goto ret0;
    }
  /* If the best two candidates we find both use default parameters,
     we may need to report and error.  Don't need to worry if next-best
     candidate is forced to use user-defined conversion when best is not.  */
  if (cp[-2].user == 0
      && cp[-1].v.old_harshness[len] != 0 && cp[-2].v.old_harshness[len] != 0)
    {
      tree tt1 = TYPE_ARG_TYPES (TREE_TYPE (cp[-1].function));
      tree tt2 = TYPE_ARG_TYPES (TREE_TYPE (cp[-2].function));
      unsigned i = cp[-1].v.old_harshness[len];

      if (cp[-2].v.old_harshness[len] < i)
	i = cp[-2].v.old_harshness[len];
      while (--i > 0)
	{
	  if (TYPE_MAIN_VARIANT (TREE_VALUE (tt1))
	      != TYPE_MAIN_VARIANT (TREE_VALUE (tt2)))
	    /* These lists are not identical, so we can choose our best candidate.  */
	    return cp - 1;
	  tt1 = TREE_CHAIN (tt1);
	  tt2 = TREE_CHAIN (tt2);
	}
      /* To get here, both lists had the same parameters up to the defaults
	 which were used.  This is an ambiguous request.  */
      goto ret0;
    }

  /* Otherwise, return our best candidate.  Note that if we get candidates
     from independent base classes, we have an ambiguity, even if one
     argument list look a little better than another one.  */
  if (cp[-1].b_or_d && basetype && TYPE_USES_MULTIPLE_INHERITANCE (basetype))
    {
      int i = n_candidates - 1, best = i;
      tree base1 = NULL_TREE;

      if (TREE_CODE (TREE_TYPE (candidates[i].function)) == FUNCTION_TYPE)
	return cp - 1;

      for (; i >= 0 && candidates[i].user == 0 && candidates[i].evil == 0; i--)
	{
	  if (TREE_CODE (TREE_TYPE (candidates[i].function)) == METHOD_TYPE)
	    {
	      tree newbase = DECL_CLASS_CONTEXT (candidates[i].function);

	      if (base1 != NULL_TREE)
		{
		  /* newbase could be a base or a parent of base1 */
		  if (newbase != base1 && ! UNIQUELY_DERIVED_FROM_P (newbase, base1)
		      && ! UNIQUELY_DERIVED_FROM_P (base1, newbase))
		    {
		      cp_error ("ambiguous request for function from distinct base classes of type `%T'", basetype);
		      cp_error_at ("  first candidate is `%#D'",
				     candidates[best].function);
		      cp_error_at ("  second candidate is `%#D'",
				     candidates[i].function);
		      cp[-1].evil = 1;
		      return cp - 1;
		    }
		}
	      else
		{
		  best = i;
		  base1 = newbase;
		}
	    }
	  else
	    return cp - 1;
	}
    }

  /* Don't accept a candidate as being ideal if it's indistinguishable
     from another candidate.  */
  if (rank_for_overload (cp-1, cp-2) == 0)
    {
      /* If the types are distinguishably different (like
	 `long' vs. `unsigned long'), that's ok.  But if they are arbitrarily
	 different, such as `int (*)(void)' vs. `void (*)(int)',
	 that's not ok.  */
      tree p1 = TYPE_ARG_TYPES (TREE_TYPE (cp[-1].function));
      tree p2 = TYPE_ARG_TYPES (TREE_TYPE (cp[-2].function));
      while (p1 && p2)
	{
	  if (TREE_CODE (TREE_VALUE (p1)) == POINTER_TYPE
	      && TREE_CODE (TREE_TYPE (TREE_VALUE (p1))) == FUNCTION_TYPE
	      && TREE_VALUE (p1) != TREE_VALUE (p2))
	    return NULL;
	  p1 = TREE_CHAIN (p1);
	  p2 = TREE_CHAIN (p2);
	}
      if (p1 || p2)
	return NULL;
    }

  return cp - 1;

 ret0:
  /* In the case where there is no ideal candidate, restore
     TREE_VALUE slots of PARMS from TREE_PURPOSE slots.  */
  while (parms)
    {
      TREE_VALUE (parms) = TREE_PURPOSE (parms);
      parms = TREE_CHAIN (parms);
    }
  return NULL;
}

/* Subroutine of ideal_candidate.  See if X or Y is a better match
   than the other.  */
static int
strictly_better (x, y)
     unsigned short x, y;
{
  unsigned short xor;

  if (x == y)
    return 0;

  xor = x ^ y;
  if (xor >= x || xor >= y)
    return 1;
  return 0;
}

static struct candidate *
ideal_candidate_ansi (basetype, candidates, n_candidates, parms, len)
     tree basetype;
     struct candidate *candidates;
     int n_candidates;
     tree parms;
     int len;
{
  struct candidate *cp = candidates+n_candidates;
  int i, j = -1, best_code;

  /* For each argument, sort the functions from best to worst for the arg.
     For each function that's not best for this arg, set its overall
     harshness to EVIL so that other args won't like it.  The candidate
     list for the last argument is the intersection of all the best-liked
     functions.  */

#if 0
  for (i = 0; i < len; i++)
    {
      qsort (candidates, n_candidates, sizeof (struct candidate),
	     rank_for_overload);
      best_code = cp[-1].h.code;

      /* To find out functions that are worse than that represented
	 by BEST_CODE, we can't just do a comparison like h.code>best_code.
	 The total harshness for the "best" fn may be 8|8 for two args, and
	 the harshness for the next-best may be 8|2.  If we just compared,
	 that would be checking 8>10, which would lead to the next-best
	 being disqualified.  What we actually want to do is get rid
	 of functions that are definitely worse than that represented
	 by best_code, i.e. those which have bits set higher than the
	 highest in best_code.  Sooooo, what we do is clear out everything
	 represented by best_code, and see if we still come up with something
	 higher.  If so (e.g., 8|8 vs 8|16), it'll disqualify it properly.  */
      for (j = n_candidates-2; j >= 0; j--)
	if ((candidates[j].h.code & ~best_code) > best_code)
	  candidates[j].h.code = EVIL_CODE;
    }

  if (cp[-1].h.code & EVIL_CODE)
    return NULL;
#else
  qsort (candidates, n_candidates, sizeof (struct candidate),
	 rank_for_overload);
  best_code = cp[-1].h.code;
#endif

  /* If they're at least as good as each other, do an arg-by-arg check.  */
  if (! strictly_better (cp[-1].h.code, cp[-2].h.code))
    {
      int better = 0;
      int worse = 0;

      for (j = 0; j < n_candidates; j++)
	if (! strictly_better (candidates[j].h.code, best_code))
	  break;

      qsort (candidates+j, n_candidates-j, sizeof (struct candidate),
	     rank_for_ideal);
      for (i = 0; i < len; i++)
	{
	  if (cp[-1].v.ansi_harshness[i].code < cp[-2].v.ansi_harshness[i].code)
	    better = 1;
	  else if (cp[-1].v.ansi_harshness[i].code > cp[-2].v.ansi_harshness[i].code)
	    worse = 1;
	  else if (cp[-1].v.ansi_harshness[i].code & STD_CODE)
	    {
	      /* If it involves a standard conversion, let the
		 inheritance lattice be the final arbiter.  */
	      if (cp[-1].v.ansi_harshness[i].distance > cp[-2].v.ansi_harshness[i].distance)
		worse = 1;
	      else if (cp[-1].v.ansi_harshness[i].distance < cp[-2].v.ansi_harshness[i].distance)
		better = 1;
	    }
	  else if (cp[-1].v.ansi_harshness[i].code & PROMO_CODE)
	    {
	      /* For integral promotions, take into account a finer
		 granularity for determining which types should be favored
		 over others in such promotions.  */
	      if (cp[-1].v.ansi_harshness[i].int_penalty > cp[-2].v.ansi_harshness[i].int_penalty)
		worse = 1;
	      else if (cp[-1].v.ansi_harshness[i].int_penalty < cp[-2].v.ansi_harshness[i].int_penalty)
		better = 1;
	    }
	}

      if (! better || worse)
	return NULL;
    }
  return cp-1;
}

static struct candidate *
ideal_candidate (basetype, candidates, n_candidates, parms, len)
     tree basetype;
     struct candidate *candidates;
     int n_candidates;
     tree parms;
     int len;
{
  if (flag_ansi_overloading)
    return ideal_candidate_ansi (basetype, candidates, n_candidates, parms,
				 len);
  else
    return ideal_candidate_old (basetype, candidates, n_candidates, parms,
				len);
}

/* Assume that if the class referred to is not in the
   current class hierarchy, that it may be remote.
   PARENT is assumed to be of aggregate type here.  */
static int
may_be_remote (parent)
     tree parent;
{
  if (TYPE_OVERLOADS_METHOD_CALL_EXPR (parent) == 0)
    return 0;

  if (current_class_type == NULL_TREE)
    return 0;

  if (parent == current_class_type)
    return 0;

  if (UNIQUELY_DERIVED_FROM_P (parent, current_class_type))
    return 0;
  return 1;
}

tree
build_vfield_ref (datum, type)
     tree datum, type;
{
  tree rval;
  int old_assume_nonnull_objects = flag_assume_nonnull_objects;

  if (datum == error_mark_node)
    return error_mark_node;

  /* Vtable references are always made from non-null objects.  */
  flag_assume_nonnull_objects = 1;
  if (TREE_CODE (TREE_TYPE (datum)) == REFERENCE_TYPE)
    datum = convert_from_reference (datum);

  if (! TYPE_USES_COMPLEX_INHERITANCE (type))
    rval = build (COMPONENT_REF, TREE_TYPE (CLASSTYPE_VFIELD (type)),
		  datum, CLASSTYPE_VFIELD (type));
  else
    rval = build_component_ref (datum, DECL_NAME (CLASSTYPE_VFIELD (type)), 0, 0);
  flag_assume_nonnull_objects = old_assume_nonnull_objects;

  return rval;
}

/* Build a call to a member of an object.  I.e., one that overloads
   operator ()(), or is a pointer-to-function or pointer-to-method.  */
static tree
build_field_call (basetype_path, instance_ptr, name, parms)
     tree basetype_path, instance_ptr, name, parms;
{
  tree field, instance;

  if (instance_ptr == current_class_decl)
    {
      /* Check to see if we really have a reference to an instance variable
	 with `operator()()' overloaded.  */
      field = IDENTIFIER_CLASS_VALUE (name);

      if (field == NULL_TREE)
	{
	  cp_error ("`this' has no member named `%D'", name);
	  return error_mark_node;
	}

      if (TREE_CODE (field) == FIELD_DECL)
	{
	  /* If it's a field, try overloading operator (),
	     or calling if the field is a pointer-to-function.  */
	  instance = build_component_ref_1 (C_C_D, field, 0);
	  if (instance == error_mark_node)
	    return error_mark_node;

	  if (TYPE_LANG_SPECIFIC (TREE_TYPE (instance))
	      && TYPE_OVERLOADS_CALL_EXPR (TREE_TYPE (instance)))
	    return build_opfncall (CALL_EXPR, LOOKUP_NORMAL, instance, parms, NULL_TREE);

	  if (TREE_CODE (TREE_TYPE (instance)) == POINTER_TYPE)
	    {
	      if (TREE_CODE (TREE_TYPE (TREE_TYPE (instance))) == FUNCTION_TYPE)
		return build_function_call (instance, parms);
	      else if (TREE_CODE (TREE_TYPE (TREE_TYPE (instance))) == METHOD_TYPE)
		return build_function_call (instance, tree_cons (NULL_TREE, current_class_decl, parms));
	    }
	}
      return NULL_TREE;
    }

  /* Check to see if this is not really a reference to an instance variable
     with `operator()()' overloaded.  */
  field = lookup_field (basetype_path, name, 1, 0);

  /* This can happen if the reference was ambiguous or for access
     violations.  */
  if (field == error_mark_node)
    return error_mark_node;

  if (field)
    {
      tree basetype;
      tree ftype = TREE_TYPE (field);

      if (TREE_CODE (ftype) == REFERENCE_TYPE)
	ftype = TREE_TYPE (ftype);

      if (TYPE_LANG_SPECIFIC (ftype) && TYPE_OVERLOADS_CALL_EXPR (ftype))
	{
	  /* Make the next search for this field very short.  */
	  basetype = DECL_FIELD_CONTEXT (field);
	  instance_ptr = convert_pointer_to (basetype, instance_ptr);

	  instance = build_indirect_ref (instance_ptr, NULL_PTR);
	  return build_opfncall (CALL_EXPR, LOOKUP_NORMAL,
				 build_component_ref_1 (instance, field, 0),
				 parms, NULL_TREE);
	}
      if (TREE_CODE (ftype) == POINTER_TYPE)
	{
	  if (TREE_CODE (TREE_TYPE (ftype)) == FUNCTION_TYPE
	      || TREE_CODE (TREE_TYPE (ftype)) == METHOD_TYPE)
	    {
	      /* This is a member which is a pointer to function.  */
	      tree ref
		= build_component_ref_1 (build_indirect_ref (instance_ptr,
							     NULL_PTR),
					 field, LOOKUP_COMPLAIN);
	      if (ref == error_mark_node)
		return error_mark_node;
	      return build_function_call (ref, parms);
	    }
	}
      else if (TREE_CODE (ftype) == METHOD_TYPE)
	{
	  error ("invalid call via pointer-to-member function");
	  return error_mark_node;
	}
      else
	return NULL_TREE;
    }
  return NULL_TREE;
}

tree
find_scoped_type (type, inner_name, inner_types)
     tree type, inner_name, inner_types;
{
  tree tags = CLASSTYPE_TAGS (type);

  while (tags)
    {
      /* The TREE_PURPOSE of an enum tag (which becomes a member of the
	 enclosing class) is set to the name for the enum type.  So, if
	 inner_name is `bar', and we strike `baz' for `enum bar { baz }',
	 then this test will be true.  */
      if (TREE_PURPOSE (tags) == inner_name)
	{
	  if (inner_types == NULL_TREE)
	    return DECL_NESTED_TYPENAME (TYPE_NAME (TREE_VALUE (tags)));
	  return resolve_scope_to_name (TREE_VALUE (tags), inner_types);
	}
      tags = TREE_CHAIN (tags);
    }

#if 0
  /* XXX This needs to be fixed better.  */
  if (TREE_CODE (type) == UNINSTANTIATED_P_TYPE)
    {
      sorry ("nested class lookup in template type");
      return NULL_TREE;
    }
#endif

  /* Look for a TYPE_DECL.  */
  for (tags = TYPE_FIELDS (type); tags; tags = TREE_CHAIN (tags))
    if (TREE_CODE (tags) == TYPE_DECL && DECL_NAME (tags) == inner_name)
      {
	/* Code by raeburn.  */
	if (inner_types == NULL_TREE)
	  return DECL_NESTED_TYPENAME (tags);
	return resolve_scope_to_name (TREE_TYPE (tags), inner_types);
      }

  return NULL_TREE;
}

/* Resolve an expression NAME1::NAME2::...::NAMEn to
   the name that names the above nested type.  INNER_TYPES
   is a chain of nested type names (held together by SCOPE_REFs);
   OUTER_TYPE is the type we know to enclose INNER_TYPES.
   Returns NULL_TREE if there is an error.  */
tree
resolve_scope_to_name (outer_type, inner_stuff)
     tree outer_type, inner_stuff;
{
  register tree tmp;
  tree inner_name, inner_type;

  if (outer_type == NULL_TREE && current_class_type != NULL_TREE)
    {
      /* We first try to look for a nesting in our current class context,
         then try any enclosing classes.  */
      tree type = current_class_type;
      
      while (type && (TREE_CODE (type) == RECORD_TYPE
		      || TREE_CODE (type) == UNION_TYPE))
        {
          tree rval = resolve_scope_to_name (type, inner_stuff);

	  if (rval != NULL_TREE)
	    return rval;
	  type = DECL_CONTEXT (TYPE_NAME (type));
	}
    }

  if (TREE_CODE (inner_stuff) == SCOPE_REF)
    {
      inner_name = TREE_OPERAND (inner_stuff, 0);
      inner_type = TREE_OPERAND (inner_stuff, 1);
    }
  else
    {
      inner_name = inner_stuff;
      inner_type = NULL_TREE;
    }

  if (outer_type == NULL_TREE)
    {
      /* If we have something that's already a type by itself,
	 use that.  */
      if (IDENTIFIER_HAS_TYPE_VALUE (inner_name))
	{
	  if (inner_type)
	    return resolve_scope_to_name (IDENTIFIER_TYPE_VALUE (inner_name),
					  inner_type);
	  return inner_name;
	}
      return NULL_TREE;
    }

  if (! IS_AGGR_TYPE (outer_type))
    return NULL_TREE;

  /* Look for member classes or enums.  */
  tmp = find_scoped_type (outer_type, inner_name, inner_type);

  /* If it's not a type in this class, then go down into the
     base classes and search there.  */
  if (! tmp && TYPE_BINFO (outer_type))
    {
      tree binfos = TYPE_BINFO_BASETYPES (outer_type);
      int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

      for (i = 0; i < n_baselinks; i++)
	{
	  tree base_binfo = TREE_VEC_ELT (binfos, i);
	  tmp = resolve_scope_to_name (BINFO_TYPE (base_binfo), inner_stuff);
	  if (tmp)
	    return tmp;
	}
      tmp = NULL_TREE;
    }

  return tmp;
}

/* Build a method call of the form `EXP->SCOPES::NAME (PARMS)'.
   This is how virtual function calls are avoided.  */
tree
build_scoped_method_call (exp, scopes, name, parms)
     tree exp, scopes, name, parms;
{
  /* Because this syntactic form does not allow
     a pointer to a base class to be `stolen',
     we need not protect the derived->base conversion
     that happens here.
     
     @@ But we do have to check access privileges later.  */
  tree basename = resolve_scope_to_name (NULL_TREE, scopes);
  tree basetype, binfo, decl;
  tree type = TREE_TYPE (exp);

  if (type == error_mark_node
      || basename == NULL_TREE)
    return error_mark_node;

  basetype = IDENTIFIER_TYPE_VALUE (basename);

  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  /* Destructors can be "called" for simple types; see 5.2.4 and 12.4 Note
     that explicit ~int is caught in the parser; this deals with typedefs
     and template parms.  */
  if (TREE_CODE (name) == BIT_NOT_EXPR && ! is_aggr_typedef (basename, 0))
    {
      if (type != basetype)
	cp_error ("type of `%E' does not match destructor type `%T' (type was `%T')",
		  exp, basetype, type);
      name = IDENTIFIER_TYPE_VALUE (TREE_OPERAND (name, 0));
      if (basetype != name)
	cp_error ("qualified type `%T' does not match destructor type `%T'",
		  basetype, name);
      return void_zero_node;
    }

  if (! is_aggr_typedef (basename, 1))
    return error_mark_node;

  if (! IS_AGGR_TYPE (type))
    {
      cp_error ("base object `%E' of scoped method call is of non-aggregate type `%T'",
		exp, type);
      return error_mark_node;
    }

  if ((binfo = binfo_or_else (basetype, type)))
    {
      if (binfo == error_mark_node)
	return error_mark_node;
      if (TREE_CODE (exp) == INDIRECT_REF)
	decl = build_indirect_ref (convert_pointer_to (binfo,
						       build_unary_op (ADDR_EXPR, exp, 0)), NULL_PTR);
      else
	decl = build_scoped_ref (exp, scopes);

      /* Call to a destructor.  */
      if (TREE_CODE (name) == BIT_NOT_EXPR)
	{
	  /* Explicit call to destructor.  */
	  name = TREE_OPERAND (name, 0);
	  if (TREE_TYPE (decl) !=
	      (IDENTIFIER_CLASS_VALUE (name)
	       ? IDENTIFIER_CLASS_TYPE_VALUE (name)
	       : IDENTIFIER_TYPE_VALUE (name)))
	    {
	      cp_error
		("qualified type `%T' does not match destructor type `%T'",
		 TREE_TYPE (decl), name);
	      return error_mark_node;
	    }
	  if (! TYPE_HAS_DESTRUCTOR (TREE_TYPE (decl)))
	    return void_zero_node;
	  
	  return build_delete (TREE_TYPE (decl), decl, integer_two_node,
			       LOOKUP_NORMAL|LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR,
			       0);
	}

      /* Call to a method.  */
      return build_method_call (decl, name, parms, NULL_TREE,
				LOOKUP_NORMAL|LOOKUP_NONVIRTUAL);
    }
  return error_mark_node;
}

static void
print_candidates (candidates)
     tree candidates;
{
  cp_error_at ("candidates are: %D", TREE_VALUE (candidates));
  candidates = TREE_CHAIN (candidates);

  while (candidates)
    {
      cp_error_at ("                %D", TREE_VALUE (candidates));
      candidates = TREE_CHAIN (candidates);
    }
}

static void
print_n_candidates (candidates, n)
     struct candidate *candidates;
     int n;
{
  int i;

  cp_error_at ("candidates are: %D", candidates[0].function);
  for (i = 1; i < n; i++)
    cp_error_at ("                %D", candidates[i].function);
}

/* Build something of the form ptr->method (args)
   or object.method (args).  This can also build
   calls to constructors, and find friends.

   Member functions always take their class variable
   as a pointer.

   INSTANCE is a class instance.

   NAME is the name of the method desired, usually an IDENTIFIER_NODE.

   PARMS help to figure out what that NAME really refers to.

   BASETYPE_PATH, if non-NULL, contains a chain from the type of INSTANCE
   down to the real instance type to use for access checking.  We need this
   information to get protected accesses correct.  This parameter is used
   by build_member_call.

   FLAGS is the logical disjunction of zero or more LOOKUP_
   flags.  See cp-tree.h for more info.

   If this is all OK, calls build_function_call with the resolved
   member function.

   This function must also handle being called to perform
   initialization, promotion/coercion of arguments, and
   instantiation of default parameters.

   Note that NAME may refer to an instance variable name.  If
   `operator()()' is defined for the type of that field, then we return
   that result.  */
tree
build_method_call (instance, name, parms, basetype_path, flags)
     tree instance, name, parms, basetype_path;
     int flags;
{
  register tree function, fntype, value_type;
  register tree basetype, save_basetype;
  register tree baselink, result, method_name, parmtypes, parm;
  tree last;
  int pass;
  enum access_type access = access_public;

  /* Range of cases for vtable optimization.  */
  enum vtable_needs { not_needed, maybe_needed, unneeded, needed };
  enum vtable_needs need_vtbl = not_needed;

  char *name_kind;
  int ever_seen = 0;
  tree instance_ptr = NULL_TREE;
  int all_virtual = flag_all_virtual;
  int static_call_context = 0;
  tree found_fns = NULL_TREE;

  /* Keep track of `const' and `volatile' objects.  */
  int constp, volatilep;

#ifdef GATHER_STATISTICS
  n_build_method_call++;
#endif

  if (instance == error_mark_node
      || name == error_mark_node
      || parms == error_mark_node
      || (instance != NULL_TREE && TREE_TYPE (instance) == error_mark_node))
    return error_mark_node;

  /* This is the logic that magically deletes the second argument to
     operator delete, if it is not needed. */
  if (name == ansi_opname[(int) DELETE_EXPR] && list_length (parms)==2)
    {
      tree save_last = TREE_CHAIN (parms);
      tree result;
      /* get rid of unneeded argument */
      TREE_CHAIN (parms) = NULL_TREE;
      result = build_method_call (instance, name, parms, basetype_path,
				  (LOOKUP_SPECULATIVELY|flags)
				  &~LOOKUP_COMPLAIN);
      /* If it works, return it. */
      if (result && result != error_mark_node)
	return build_method_call (instance, name, parms, basetype_path, flags);
      /* If it doesn't work, two argument delete must work */
      TREE_CHAIN (parms) = save_last;
    }

  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      flags |= LOOKUP_DESTRUCTOR;
      name = TREE_OPERAND (name, 0);
      if (parms)
	error ("destructors take no parameters");
      basetype = get_type_value (name);
      if (basetype == NULL_TREE)
	{
	  cp_error ("call to destructor for non-type `%D'", name);
	  return void_zero_node;
	}
      if (basetype != TREE_TYPE(instance))
	basetype = TREE_TYPE(instance);
      if (! TYPE_HAS_DESTRUCTOR (basetype))
	return void_zero_node;
      instance = default_conversion (instance);
      instance_ptr = build_unary_op (ADDR_EXPR, instance, 0);
      return build_delete (build_pointer_type (basetype),
			   instance_ptr, integer_two_node,
			   LOOKUP_NORMAL|LOOKUP_DESTRUCTOR, 0);
    }

  {
    char *xref_name;
    
    /* Initialize name for error reporting.  */
    if (IDENTIFIER_OPNAME_P (name) && ! IDENTIFIER_TYPENAME_P (name))
      {
	char *p = operator_name_string (name);
	xref_name = (char *)alloca (strlen (p) + 10);
	sprintf (xref_name, "operator %s", p);
      }
    else if (TREE_CODE (name) == SCOPE_REF)
      xref_name = IDENTIFIER_POINTER (TREE_OPERAND (name, 1));
    else
      xref_name = IDENTIFIER_POINTER (name);

    GNU_xref_call (current_function_decl, xref_name);
  }

  if (instance == NULL_TREE)
    {
      basetype = NULL_TREE;
      /* Check cases where this is really a call to raise
	 an exception.  */
      if (current_class_type && TREE_CODE (name) == IDENTIFIER_NODE)
	{
	  basetype = purpose_member (name, CLASSTYPE_TAGS (current_class_type));
	  if (basetype)
	    basetype = TREE_VALUE (basetype);
	}
      else if (TREE_CODE (name) == SCOPE_REF
	       && TREE_CODE (TREE_OPERAND (name, 0)) == IDENTIFIER_NODE)
	{
	  if (! is_aggr_typedef (TREE_OPERAND (name, 0), 1))
	    return error_mark_node;
	  basetype = purpose_member (TREE_OPERAND (name, 1),
				     CLASSTYPE_TAGS (IDENTIFIER_TYPE_VALUE (TREE_OPERAND (name, 0))));
	  if (basetype)
	    basetype = TREE_VALUE (basetype);
	}

      if (basetype != NULL_TREE)
	;
      /* call to a constructor... */
      else if (IDENTIFIER_HAS_TYPE_VALUE (name))
	{
	  basetype = IDENTIFIER_TYPE_VALUE (name);
	  name = constructor_name_full (basetype);
	}
      else
	{
	  tree typedef_name = lookup_name (name, 1);
	  if (typedef_name && TREE_CODE (typedef_name) == TYPE_DECL)
	    {
	      /* Canonicalize the typedef name.  */
	      basetype = TREE_TYPE (typedef_name);
	      name = TYPE_IDENTIFIER (basetype);
	    }
	  else
	    {
	      cp_error ("no constructor named `%T' in scope",
			name);
	      return error_mark_node;
	    }
	}

      if (! IS_AGGR_TYPE (basetype))
	{
	non_aggr_error:
	  if ((flags & LOOKUP_COMPLAIN) && TREE_CODE (basetype) != ERROR_MARK)
	    cp_error ("request for member `%D' in `%E', which is of non-aggregate type `%T'",
		      name, instance, basetype);

	  return error_mark_node;
	}
    }
  else if (instance == C_C_D || instance == current_class_decl)
    {
      /* When doing initialization, we side-effect the TREE_TYPE of
	 C_C_D, hence we cannot set up BASETYPE from CURRENT_CLASS_TYPE.  */
      basetype = TREE_TYPE (C_C_D);

      /* Anything manifestly `this' in constructors and destructors
	 has a known type, so virtual function tables are not needed.  */
      if (TYPE_VIRTUAL_P (basetype)
	  && !(flags & LOOKUP_NONVIRTUAL))
	need_vtbl = (dtor_label || ctor_label)
	  ? unneeded : maybe_needed;

      instance = C_C_D;
      instance_ptr = current_class_decl;
      result = build_field_call (TYPE_BINFO (current_class_type),
				 instance_ptr, name, parms);

      if (result)
	return result;
    }
  else if (TREE_CODE (instance) == RESULT_DECL)
    {
      basetype = TREE_TYPE (instance);
      /* Should we ever have to make a virtual function reference
	 from a RESULT_DECL, know that it must be of fixed type
	 within the scope of this function.  */
      if (!(flags & LOOKUP_NONVIRTUAL) && TYPE_VIRTUAL_P (basetype))
	need_vtbl = maybe_needed;
      instance_ptr = build1 (ADDR_EXPR, TYPE_POINTER_TO (basetype), instance);
    }
  else if (instance == current_exception_object)
    {
      instance_ptr = build1 (ADDR_EXPR, TYPE_POINTER_TO (current_exception_type),
			    TREE_OPERAND (current_exception_object, 0));
      mark_addressable (TREE_OPERAND (current_exception_object, 0));
      result = build_field_call (TYPE_BINFO (current_exception_type),
				 instance_ptr, name, parms);
      if (result)
	return result;
      cp_error ("exception member `%D' cannot be invoked", name);
      return error_mark_node;
    }
  else
    {
      /* The MAIN_VARIANT of the type that `instance_ptr' winds up being.  */
      tree inst_ptr_basetype;

      static_call_context =
	(TREE_CODE (instance) == INDIRECT_REF
	 && TREE_CODE (TREE_OPERAND (instance, 0)) == NOP_EXPR
	 && TREE_OPERAND (TREE_OPERAND (instance, 0), 0) == error_mark_node);

      /* the base type of an instance variable is pointer to class */
      basetype = TREE_TYPE (instance);

      if (TREE_CODE (basetype) == REFERENCE_TYPE)
	{
	  basetype = TREE_TYPE (basetype);
	  if (! IS_AGGR_TYPE (basetype))
	    goto non_aggr_error;
	  /* Call to convert not needed because we are remaining
	     within the same type.  */
	  instance_ptr = build1 (NOP_EXPR, build_pointer_type (basetype),
				 instance);
	  inst_ptr_basetype = TYPE_MAIN_VARIANT (basetype);
	}
      else
	{
	  if (! IS_AGGR_TYPE (basetype))
	    goto non_aggr_error;

	  if (IS_SIGNATURE_POINTER (basetype)
	      || IS_SIGNATURE_REFERENCE (basetype))
	    basetype = SIGNATURE_TYPE (basetype);

	  if ((IS_SIGNATURE (basetype)
	       && (instance_ptr = build_optr_ref (instance)))
	      || (lvalue_p (instance)
	       && (instance_ptr = build_unary_op (ADDR_EXPR, instance, 0)))
	      || (instance_ptr = unary_complex_lvalue (ADDR_EXPR, instance)))
	    {
	      if (instance_ptr == error_mark_node)
		return error_mark_node;
	    }
	  else if (TREE_CODE (instance) == NOP_EXPR
		   || TREE_CODE (instance) == CONSTRUCTOR)
	    {
	      /* A cast is not an lvalue.  Initialize a fresh temp
		 with the value we are casting from, and proceed with
		 that temporary.  We can't cast to a reference type,
		 so that simplifies the initialization to something
		 we can manage.  */
	      tree temp = get_temp_name (TREE_TYPE (instance), 0);
	      if (IS_AGGR_TYPE (TREE_TYPE (instance)))
		expand_aggr_init (temp, instance, 0);
	      else
		{
		  store_init_value (temp, instance);
		  expand_decl_init (temp);
		}
	      instance = temp;
	      instance_ptr = build_unary_op (ADDR_EXPR, instance, 0);
	    }
	  else
	    {
	      if (TREE_CODE (instance) != CALL_EXPR)
		my_friendly_abort (125);
	      if (TYPE_NEEDS_CONSTRUCTING (basetype))
		instance = build_cplus_new (basetype, instance, 0);
	      else
		{
		  instance = get_temp_name (basetype, 0);
		  TREE_ADDRESSABLE (instance) = 1;
		}
	      instance_ptr = build_unary_op (ADDR_EXPR, instance, 0);
	    }
	  /* @@ Should we call comp_target_types here?  */
	  inst_ptr_basetype = TREE_TYPE (TREE_TYPE (instance_ptr));
	  if (TYPE_MAIN_VARIANT (basetype) == TYPE_MAIN_VARIANT (inst_ptr_basetype))
	    basetype = inst_ptr_basetype;
	  else
	    {
	      instance_ptr = convert (TYPE_POINTER_TO (basetype), instance_ptr);
	      if (instance_ptr == error_mark_node)
		return error_mark_node;
	    }
	}

      /* After converting `instance_ptr' above, `inst_ptr_basetype' was
	 not updated, so we use `basetype' instead.  */
      if (basetype_path == NULL_TREE
	  && IS_SIGNATURE (basetype))
	basetype_path = TYPE_BINFO (basetype);
      else if (basetype_path == NULL_TREE ||
	BINFO_TYPE (basetype_path) != TYPE_MAIN_VARIANT (inst_ptr_basetype))
	basetype_path = TYPE_BINFO (inst_ptr_basetype);

      result = build_field_call (basetype_path, instance_ptr, name, parms);
      if (result)
	return result;

      if (!(flags & LOOKUP_NONVIRTUAL) && TYPE_VIRTUAL_P (basetype))
	{
	  if (TREE_SIDE_EFFECTS (instance_ptr))
	    {
	      /* This action is needed because the instance is needed
		 for providing the base of the virtual function table.
		 Without using a SAVE_EXPR, the function we are building
		 may be called twice, or side effects on the instance
		 variable (such as a post-increment), may happen twice.  */
	      instance_ptr = save_expr (instance_ptr);
	      instance = build_indirect_ref (instance_ptr, NULL_PTR);
	    }
	  else if (TREE_CODE (TREE_TYPE (instance)) == POINTER_TYPE)
	    {
	      /* This happens when called for operator new ().  */
	      instance = build_indirect_ref (instance, NULL_PTR);
	    }

	  need_vtbl = maybe_needed;
	}
    }

  if (TYPE_SIZE (basetype) == 0)
    {
      /* This is worth complaining about, I think.  */
      cp_error ("cannot lookup method in incomplete type `%T'", basetype);
      return error_mark_node;
    }

  save_basetype = TYPE_MAIN_VARIANT (basetype);

#if 0
  if (all_virtual == 1
      && (! strncmp (IDENTIFIER_POINTER (name), OPERATOR_METHOD_FORMAT,
		     OPERATOR_METHOD_LENGTH)
	  || instance_ptr == NULL_TREE
	  || (TYPE_OVERLOADS_METHOD_CALL_EXPR (basetype) == 0)))
    all_virtual = 0;
#endif

  last = NULL_TREE;
  for (parmtypes = NULL_TREE, parm = parms; parm; parm = TREE_CHAIN (parm))
    {
      tree t = TREE_TYPE (TREE_VALUE (parm));
      if (TREE_CODE (t) == OFFSET_TYPE)
	{
	  /* Convert OFFSET_TYPE entities to their normal selves.  */
	  TREE_VALUE (parm) = resolve_offset_ref (TREE_VALUE (parm));
	  t = TREE_TYPE (TREE_VALUE (parm));
	}
      if (TREE_CODE (TREE_VALUE (parm)) == OFFSET_REF
	  && TREE_CODE (t) == METHOD_TYPE)
	{
	  TREE_VALUE (parm) = build_unary_op (ADDR_EXPR, TREE_VALUE (parm), 0);
	}
      if (TREE_CODE (t) == ARRAY_TYPE)
	{
	  /* Perform the conversion from ARRAY_TYPE to POINTER_TYPE in place.
	     This eliminates needless calls to `compute_conversion_costs'.  */
	  TREE_VALUE (parm) = default_conversion (TREE_VALUE (parm));
	  t = TREE_TYPE (TREE_VALUE (parm));
	}
      if (t == error_mark_node)
	return error_mark_node;
      last = build_tree_list (NULL_TREE, t);
      parmtypes = chainon (parmtypes, last);
    }

  if (instance)
    {
      /* TREE_READONLY (instance) fails for references.  */
      constp = TYPE_READONLY (TREE_TYPE (TREE_TYPE (instance_ptr)));
      volatilep = TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (instance_ptr)));
      parms = tree_cons (NULL_TREE, instance_ptr, parms);
    }
  else
    {
      /* Raw constructors are always in charge.  */
      if (TYPE_USES_VIRTUAL_BASECLASSES (basetype)
	  && ! (flags & LOOKUP_HAS_IN_CHARGE))
	{
	  flags |= LOOKUP_HAS_IN_CHARGE;
	  parms = tree_cons (NULL_TREE, integer_one_node, parms);
	  parmtypes = tree_cons (NULL_TREE, integer_type_node, parmtypes);
	}

      if (flag_this_is_variable > 0)
	{
	  constp = 0;
	  volatilep = 0;
	  parms = tree_cons (NULL_TREE, build1 (NOP_EXPR, TYPE_POINTER_TO (basetype), integer_zero_node), parms);
	}
      else
	{
	  constp = 0;
	  volatilep = 0;
	  instance_ptr = build_new (NULL_TREE, basetype, void_type_node, 0);
	  if (instance_ptr == error_mark_node)
	    return error_mark_node;
	  instance_ptr = save_expr (instance_ptr);
	  TREE_CALLS_NEW (instance_ptr) = 1;
	  instance = build_indirect_ref (instance_ptr, NULL_PTR);

	  /* If it's a default argument initialized from a ctor, what we get
	     from instance_ptr will match the arglist for the FUNCTION_DECL
	     of the constructor.  */
	  if (parms && TREE_CODE (TREE_VALUE (parms)) == CALL_EXPR
	      && TREE_OPERAND (TREE_VALUE (parms), 1)
	      && TREE_CALLS_NEW (TREE_VALUE (TREE_OPERAND (TREE_VALUE (parms), 1))))
	    parms = build_tree_list (NULL_TREE, instance_ptr);
	  else
	    parms = tree_cons (NULL_TREE, instance_ptr, parms);
	}
    }

  parmtypes = tree_cons (NULL_TREE, TREE_TYPE (instance_ptr), parmtypes);

  if (last == NULL_TREE)
    last = parmtypes;

  /* Look up function name in the structure type definition.  */

  if ((IDENTIFIER_HAS_TYPE_VALUE (name)
       && IS_AGGR_TYPE (IDENTIFIER_TYPE_VALUE (name))
       && TREE_CODE(IDENTIFIER_TYPE_VALUE (name)) != UNINSTANTIATED_P_TYPE)
      || name == constructor_name (basetype))
    {
      tree tmp = NULL_TREE;
      if (IDENTIFIER_TYPE_VALUE (name) == basetype
	  || name == constructor_name (basetype))
	tmp = TYPE_BINFO (basetype);
      else
	tmp = get_binfo (IDENTIFIER_TYPE_VALUE (name), basetype, 0);
      
      if (tmp != NULL_TREE)
	{
	  name_kind = "constructor";
	  
	  if (TYPE_USES_VIRTUAL_BASECLASSES (basetype)
	      && ! (flags & LOOKUP_HAS_IN_CHARGE))
	    {
	      /* Constructors called for initialization
		 only are never in charge.  */
	      tree tmplist;
	      
	      flags |= LOOKUP_HAS_IN_CHARGE;
	      tmplist = tree_cons (NULL_TREE, integer_zero_node,
				   TREE_CHAIN (parms));
	      TREE_CHAIN (parms) = tmplist;
	      tmplist = tree_cons (NULL_TREE, integer_type_node, TREE_CHAIN (parmtypes));
	      TREE_CHAIN (parmtypes) = tmplist;
	    }
	  basetype = BINFO_TYPE (tmp);
	}
      else
	name_kind = "method";
    }
  else
    name_kind = "method";
  
  if (basetype_path == NULL_TREE
      || BINFO_TYPE (basetype_path) != TYPE_MAIN_VARIANT (basetype))
    basetype_path = TYPE_BINFO (basetype);
  result = lookup_fnfields (basetype_path, name,
			    (flags & LOOKUP_COMPLAIN));
  if (result == error_mark_node)
    return error_mark_node;


  /* Now, go look for this method name.  We do not find destructors here.

     Putting `void_list_node' on the end of the parmtypes
     fakes out `build_decl_overload' into doing the right thing.  */
  TREE_CHAIN (last) = void_list_node;
  method_name = build_decl_overload (name, parmtypes,
				     1 + (name == constructor_name (save_basetype)
					  || name == constructor_name_full (save_basetype)));
  TREE_CHAIN (last) = NULL_TREE;

  for (pass = 0; pass < 2; pass++)
    {
      struct candidate *candidates;
      struct candidate *cp;
      int len;
      unsigned best = 1;

      /* This increments every time we go up the type hierarchy.
	 The idea is to prefer a function of the derived class if possible. */
      int b_or_d = 0;

      baselink = result;

      if (pass > 0)
	{
	  candidates
	    = (struct candidate *) alloca ((ever_seen+1)
					   * sizeof (struct candidate));
	  bzero (candidates, (ever_seen + 1) * sizeof (struct candidate));
	  cp = candidates;
	  len = list_length (parms);
	  ever_seen = 0;

	  /* First see if a global function has a shot at it.  */
	  if (flags & LOOKUP_GLOBAL)
	    {
	      tree friend_parms;
	      tree parm = instance_ptr;

	      if (TREE_CODE (TREE_TYPE (parm)) == REFERENCE_TYPE)
		{
		  /* TREE_VALUE (parms) may have been modified by now;
                     restore it to its original value. */
		  TREE_VALUE (parms) = parm;
		  friend_parms = parms;
		}
	      else if (TREE_CODE (TREE_TYPE (parm)) == POINTER_TYPE)
		{
		  tree new_type;
		  parm = build_indirect_ref (parm, "friendifying parms (compiler error)");
		  new_type = build_reference_type (TREE_TYPE (parm));
		  /* It is possible that this should go down a layer. */
		  new_type = build_type_variant (new_type, constp, volatilep);
		  parm = convert (new_type, parm);
		  friend_parms = tree_cons (NULL_TREE, parm, TREE_CHAIN (parms));
		}
	      else
		my_friendly_abort (167);

	      cp->h_len = len;
	      if (flag_ansi_overloading)
		cp->v.ansi_harshness = (struct harshness_code *)
		  alloca ((len + 1) * sizeof (struct harshness_code));
	      else
		cp->v.old_harshness = (unsigned short *)
		  alloca ((len + 1) * sizeof (unsigned short));

	      result = build_overload_call (name, friend_parms, 0, cp);
	      /* If it turns out to be the one we were actually looking for
		 (it was probably a friend function), the return the
		 good result.  */
	      if (TREE_CODE (result) == CALL_EXPR)
		return result;

	      if (flag_ansi_overloading)
		while ((cp->h.code & EVIL_CODE) == 0)
		  {
		    /* non-standard uses: set the field to 0 to indicate
		       we are using a non-member function.  */
		    cp->u.field = 0;
		    if (cp->v.ansi_harshness[len].distance == 0
			&& cp->h.code < best)
		      best = cp->h.code;
		    cp += 1;
		  }
	      else
		while (cp->evil == 0)
		  {
		    /* non-standard uses: set the field to 0 to indicate
		       we are using a non-member function.  */
		    cp->u.field = 0;
		    if (cp->v.old_harshness[len] == 0
			&& cp->v.old_harshness[len] == 0
			&& cp->ellipsis == 0 && cp->user == 0 && cp->b_or_d == 0
			&& cp->easy < best)
		      best = cp->easy;
		    cp += 1;
		  }
	    }
	}

      while (baselink)
	{
	  /* We have a hit (of sorts). If the parameter list is
	     "error_mark_node", or some variant thereof, it won't
	     match any methods.  Since we have verified that the is
	     some method vaguely matching this one (in name at least),
	     silently return.
	     
	     Don't stop for friends, however.  */
	  basetype_path = TREE_PURPOSE (baselink);

	  function = TREE_VALUE (baselink);
	  if (TREE_CODE (basetype_path) == TREE_LIST)
	    basetype_path = TREE_VALUE (basetype_path);
	  basetype = BINFO_TYPE (basetype_path);

	  /* Cast the instance variable if necessary.  */
	  if (basetype != TYPE_MAIN_VARIANT
	      (TREE_TYPE (TREE_TYPE (TREE_VALUE (parms)))))
	    {
	      if (basetype == save_basetype)
		TREE_VALUE (parms) = instance_ptr;
	      else
		{
		  tree type = build_pointer_type
		    (build_type_variant (basetype, constp, volatilep));
		  TREE_VALUE (parms) = convert_force (type, instance_ptr);
		}
	    }

	  /* FIXME: this is the wrong place to get an error.  Hopefully
	     the access-control rewrite will make this change more cleanly.  */
	  if (TREE_VALUE (parms) == error_mark_node)
	    return error_mark_node;

	  if (DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (function)))
	    function = DECL_CHAIN (function);

	  for (; function; function = DECL_CHAIN (function))
	    {
#ifdef GATHER_STATISTICS
	      n_inner_fields_searched++;
#endif
	      ever_seen++;
	      if (pass > 0)
		found_fns = tree_cons (NULL_TREE, function, found_fns);

	      /* Not looking for friends here.  */
	      if (TREE_CODE (TREE_TYPE (function)) == FUNCTION_TYPE
		  && ! DECL_STATIC_FUNCTION_P (function))
		continue;

	      if (pass == 0
		  && DECL_ASSEMBLER_NAME (function) == method_name)
		goto found;

	      if (pass > 0)
		{
		  tree these_parms = parms;

#ifdef GATHER_STATISTICS
		  n_inner_fields_searched++;
#endif
		  cp->h_len = len;
		  if (flag_ansi_overloading)
		    cp->v.ansi_harshness = (struct harshness_code *)
		      alloca ((len + 1) * sizeof (struct harshness_code));
		  else
		    cp->v.old_harshness = (unsigned short *)
		      alloca ((len + 1) * sizeof (unsigned short));

		  if (DECL_STATIC_FUNCTION_P (function))
		    these_parms = TREE_CHAIN (these_parms);
		  compute_conversion_costs (function, these_parms, cp, len);

		  if (!flag_ansi_overloading)
		      cp->b_or_d += b_or_d;

		  if ((flag_ansi_overloading && (cp->h.code & EVIL_CODE) == 0)
		      || (!flag_ansi_overloading && cp->evil == 0))
		    {
		      cp->u.field = function;
		      cp->function = function;
		      cp->basetypes = basetype_path;

		      /* No "two-level" conversions.  */
		      if (flags & LOOKUP_NO_CONVERSION
			  && ((flag_ansi_overloading
			       && (cp->h.code & USER_CODE))
			      || (!flag_ansi_overloading
				  && cp->user != 0)))
			continue;

		      /* If we used default parameters, we must
			 check to see whether anyone else might
			 use them also, and report a possible
			 ambiguity.  */
		      if (! TYPE_USES_MULTIPLE_INHERITANCE (save_basetype)
			  && ((flag_ansi_overloading
			       && cp->v.ansi_harshness[len].distance == 0
			       && cp->h.code < best)
			      || (!flag_ansi_overloading
				  && cp->v.old_harshness[len] == 0
				  && CONST_HARSHNESS (cp->v.old_harshness[0]) == 0
				  && cp->ellipsis == 0 && cp->user == 0 && cp->b_or_d == 0
				  && cp->easy < best)))
			{
			  if (! DECL_STATIC_FUNCTION_P (function))
			    TREE_VALUE (parms) = cp->arg;
			  if (best == 1)
			    goto found_and_maybe_warn;
			}
		      cp++;
		    }
		}
	    }
	  /* Now we have run through one link's member functions.
	     arrange to head-insert this link's links.  */
	  baselink = next_baselink (baselink);
	  b_or_d += 1;
	  /* Don't grab functions from base classes.  lookup_fnfield will
	     do the work to get us down into the right place.  */
	  baselink = NULL_TREE;
	}
      if (pass == 0)
	{
	  tree igv = IDENTIFIER_GLOBAL_VALUE (name);

	  /* No exact match could be found.  Now try to find match
	     using default conversions.  */
	  if ((flags & LOOKUP_GLOBAL) && igv)
	    {
	      if (TREE_CODE (igv) == FUNCTION_DECL)
		ever_seen += 1;
	      else if (TREE_CODE (igv) == TREE_LIST)
		ever_seen += count_functions (igv);
	    }

	  if (ever_seen == 0)
	    {
	      if ((flags & (LOOKUP_SPECULATIVELY|LOOKUP_COMPLAIN))
		  == LOOKUP_SPECULATIVELY)
		return NULL_TREE;
	      
	      TREE_CHAIN (last) = void_list_node;
	      if (flags & LOOKUP_GLOBAL)
		cp_error ("no global or member function `%D(%A)' defined",
			  name, parmtypes);
	      else
		cp_error ("no member function `%T::%D(%A)' defined",
			  save_basetype, name, TREE_CHAIN (parmtypes));
	      return error_mark_node;
	    }
	  continue;
	}

      if (cp - candidates != 0)
	{
	  /* Rank from worst to best.  Then cp will point to best one.
	     Private fields have their bits flipped.  For unsigned
	     numbers, this should make them look very large.
	     If the best alternate has a (signed) negative value,
	     then all we ever saw were private members.  */
	  if (cp - candidates > 1)
	    {
	      int n_candidates = cp - candidates;
	      TREE_VALUE (parms) = instance_ptr;
	      cp = ideal_candidate (save_basetype, candidates,
				    n_candidates, parms, len);
	      if (cp == (struct candidate *)0)
		{
		  if (flags & LOOKUP_COMPLAIN)
		    {
		      cp_error ("call of overloaded %s `%D' is ambiguous",
				name_kind, name);
		      print_n_candidates (candidates, n_candidates);
		    }
		  return error_mark_node;
		}
	      if ((flag_ansi_overloading && (cp->h.code & EVIL_CODE))
		  || (!flag_ansi_overloading && cp->evil))
		return error_mark_node;
	    }
	  else if ((flag_ansi_overloading && (cp[-1].h.code & EVIL_CODE))
		   || (!flag_ansi_overloading && cp[-1].evil == 2))
	    {
	      if (flags & LOOKUP_COMPLAIN)
		cp_error ("ambiguous type conversion requested for %s `%D'",
			  name_kind, name);
	      return error_mark_node;
	    }
	  else
	    cp--;

	  /* The global function was the best, so use it.  */
	  if (cp->u.field == 0)
	    {
	      /* We must convert the instance pointer into a reference type.
		 Global overloaded functions can only either take
		 aggregate objects (which come for free from references)
		 or reference data types anyway.  */
	      TREE_VALUE (parms) = copy_node (instance_ptr);
	      TREE_TYPE (TREE_VALUE (parms)) = build_reference_type (TREE_TYPE (TREE_TYPE (instance_ptr)));
	      return build_function_call (cp->function, parms);
	    }

	  function = cp->function;
	  basetype_path = cp->basetypes;
	  if (! DECL_STATIC_FUNCTION_P (function))
	    TREE_VALUE (parms) = cp->arg;
	  goto found_and_maybe_warn;
	}

      if ((flags & ~LOOKUP_GLOBAL) & (LOOKUP_COMPLAIN|LOOKUP_SPECULATIVELY))
	{
	  if ((flags & (LOOKUP_SPECULATIVELY|LOOKUP_COMPLAIN))
	      == LOOKUP_SPECULATIVELY)
	    return NULL_TREE;

	  if (DECL_STATIC_FUNCTION_P (cp->function))
	    parms = TREE_CHAIN (parms);
	  if (ever_seen)
	    {
	      if (flags & LOOKUP_SPECULATIVELY)
		return NULL_TREE;
	      if (static_call_context
		  && TREE_CODE (TREE_TYPE (cp->function)) == METHOD_TYPE)
		cp_error ("object missing in call to `%D'", cp->function);
	      else if (ever_seen > 1)
		{
		  TREE_CHAIN (last) = void_list_node;
		  cp_error ("no matching function for call to `%T::%D (%A)'",
			    TREE_TYPE (TREE_TYPE (instance_ptr)),
			    name, TREE_CHAIN (parmtypes));
		  TREE_CHAIN (last) = NULL_TREE;
		  print_candidates (found_fns);
		}
	      else
		report_type_mismatch (cp, parms, name_kind);
	      return error_mark_node;
	    }

	  if ((flags & (LOOKUP_SPECULATIVELY|LOOKUP_COMPLAIN))
	      == LOOKUP_COMPLAIN)
	    {
	      cp_error ("%T has no method named %D", save_basetype, name);
	      return error_mark_node;
	    }
	  return NULL_TREE;
	}
      continue;

    found_and_maybe_warn:
      if (((flag_ansi_overloading
	    && (cp->v.ansi_harshness[0].code & CONST_CODE))
	   || (!flag_ansi_overloading
	       && CONST_HARSHNESS (cp->v.old_harshness[0])))
	  /* 12.1p2: Constructors can be called for const objects.  */
	  && ! DECL_CONSTRUCTOR_P (cp->function))
	{
	  if (flags & LOOKUP_COMPLAIN)
	    {
	      cp_error_at ("non-const member function `%D'", cp->function);
	      error ("called for const object at this point in file");
	    }
	  /* Not good enough for a match.  */
	  else
	    return error_mark_node;
	}
      goto found;
    }
  /* Silently return error_mark_node.  */
  return error_mark_node;

 found:
  if (flags & LOOKUP_PROTECT)
    access = compute_access (basetype_path, function);

  if (access == access_private)
    {
      if (flags & LOOKUP_COMPLAIN)
	{
	  cp_error_at ("%s `%+#D' is %s", name_kind, function, 
		       TREE_PRIVATE (function) ? "private"
		       : "from private base class");
	  error ("within this context");
	}
      return error_mark_node;
    }
  else if (access == access_protected)
    {
      if (flags & LOOKUP_COMPLAIN)
	{
	  cp_error_at ("%s `%+#D' %s", name_kind, function,
		       TREE_PROTECTED (function) ? "is protected"
		       : "has protected accessibility");
	  error ("within this context");
	}
      return error_mark_node;
    }

  /* From here on down, BASETYPE is the type that INSTANCE_PTR's
     type (if it exists) is a pointer to.  */

  if (IS_SIGNATURE (basetype) && static_call_context)
    {
      cp_error ("cannot call signature member function `%T::%D' without signature pointer/reference",
		basetype, name);
      return error_mark_node;
	}
  else if (IS_SIGNATURE (basetype))
    return build_signature_method_call (basetype, instance, function, parms);

  function = DECL_MAIN_VARIANT (function);
  /* Declare external function if necessary. */
  assemble_external (function);

  fntype = TREE_TYPE (function);
  if (TREE_CODE (fntype) == POINTER_TYPE)
    fntype = TREE_TYPE (fntype);
  basetype = DECL_CLASS_CONTEXT (function);

  /* If we are referencing a virtual function from an object
     of effectively static type, then there is no need
     to go through the virtual function table.  */
  if (need_vtbl == maybe_needed)
    {
      int fixed_type = resolves_to_fixed_type_p (instance, 0);

      if (all_virtual == 1
	  && DECL_VINDEX (function)
	  && may_be_remote (basetype))
	need_vtbl = needed;
      else if (DECL_VINDEX (function))
	need_vtbl = fixed_type ? unneeded : needed;
      else
	need_vtbl = not_needed;
    }

  if (TREE_CODE (fntype) == METHOD_TYPE && static_call_context
      && !DECL_CONSTRUCTOR_P (function))
    {
      /* Let's be nice to the user for now, and give reasonable
	 default behavior.  */
      instance_ptr = current_class_decl;
      if (instance_ptr)
	{
	  if (basetype != current_class_type)
	    {
	      tree binfo = get_binfo (basetype, current_class_type, 1);
	      if (binfo == NULL_TREE)
		{
		  error_not_base_type (function, current_class_type);
		  return error_mark_node;
		}
	      else if (basetype == error_mark_node)
		return error_mark_node;
	    }
	}
      /* Only allow a static member function to call another static member
	 function.  */
      else if (DECL_LANG_SPECIFIC (function)
	       && !DECL_STATIC_FUNCTION_P (function))
	{
	  cp_error ("cannot call member function `%D' without object",
		    function);
	  return error_mark_node;
	}
    }

  value_type = TREE_TYPE (fntype) ? TREE_TYPE (fntype) : void_type_node;

  if (TYPE_SIZE (value_type) == 0)
    {
      if (flags & LOOKUP_COMPLAIN)
	incomplete_type_error (0, value_type);
      return error_mark_node;
    }

  /* We do not pass FUNCTION into `convert_arguments', because by
     now everything should be ok.  If not, then we have a serious error.  */
  if (DECL_STATIC_FUNCTION_P (function))
    parms = convert_arguments (NULL_TREE, TYPE_ARG_TYPES (fntype),
			       TREE_CHAIN (parms), NULL_TREE, LOOKUP_NORMAL);
  else if (need_vtbl == unneeded)
    {
      int sub_flags = DECL_CONSTRUCTOR_P (function) ? flags : LOOKUP_NORMAL;
      basetype = TREE_TYPE (instance);
      if (TYPE_METHOD_BASETYPE (TREE_TYPE (function)) != TYPE_MAIN_VARIANT (basetype)
	  && TYPE_USES_COMPLEX_INHERITANCE (basetype))
	{
	  basetype = DECL_CLASS_CONTEXT (function);
	  instance_ptr = convert_pointer_to (basetype, instance_ptr);
	  instance = build_indirect_ref (instance_ptr, NULL_PTR);
	}
      parms = tree_cons (NULL_TREE, instance_ptr,
			 convert_arguments (NULL_TREE, TREE_CHAIN (TYPE_ARG_TYPES (fntype)), TREE_CHAIN (parms), NULL_TREE, sub_flags));
    }
  else
    {
      if ((flags & LOOKUP_NONVIRTUAL) == 0)
	basetype = DECL_CONTEXT (function);

      /* First parm could be integer_zerop with casts like
	 ((Object*)0)->Object::IsA()  */
      if (!integer_zerop (TREE_VALUE (parms)))
	{
	  /* Since we can't have inheritance with a union, doing get_binfo
	     on it won't work.  We do all the convert_pointer_to_real
	     stuff to handle MI correctly...for unions, that's not
	     an issue, so we must short-circuit that extra work here.  */
	  tree tmp = TREE_TYPE (TREE_TYPE (TREE_VALUE (parms)));
	  if (tmp != NULL_TREE && TREE_CODE (tmp) == UNION_TYPE)
	    instance_ptr = TREE_VALUE (parms);
	  else
	    {
	      tree binfo = get_binfo (basetype,
				      TREE_TYPE (TREE_TYPE (TREE_VALUE (parms))),
				      0);
	      instance_ptr = convert_pointer_to_real (binfo, TREE_VALUE (parms));
	    }
	  instance_ptr
	    = convert_pointer_to (build_type_variant (basetype,
						      constp, volatilep),
				  instance_ptr);

	  if (TREE_CODE (instance_ptr) == COND_EXPR)
	    {
	      instance_ptr = save_expr (instance_ptr);
	      instance = build_indirect_ref (instance_ptr, NULL_PTR);
	    }
	  else if (TREE_CODE (instance_ptr) == NOP_EXPR
		   && TREE_CODE (TREE_OPERAND (instance_ptr, 0)) == ADDR_EXPR
		   && TREE_OPERAND (TREE_OPERAND (instance_ptr, 0), 0) == instance)
	    ;
	  /* The call to `convert_pointer_to' may return error_mark_node.  */
	  else if (TREE_CODE (instance_ptr) == ERROR_MARK)
	    return instance_ptr;
	  else if (instance == NULL_TREE
		   || TREE_CODE (instance) != INDIRECT_REF
		   || TREE_OPERAND (instance, 0) != instance_ptr)
	    instance = build_indirect_ref (instance_ptr, NULL_PTR);
	}
      parms = tree_cons (NULL_TREE, instance_ptr,
			 convert_arguments (NULL_TREE, TREE_CHAIN (TYPE_ARG_TYPES (fntype)), TREE_CHAIN (parms), NULL_TREE, LOOKUP_NORMAL));
    }

#if 0
  /* Constructors do not overload method calls.  */
  else if (TYPE_OVERLOADS_METHOD_CALL_EXPR (basetype)
	   && name != TYPE_IDENTIFIER (basetype)
	   && (TREE_CODE (function) != FUNCTION_DECL
	       || strncmp (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (function)),
			   OPERATOR_METHOD_FORMAT,
			   OPERATOR_METHOD_LENGTH))
  	   && (may_be_remote (basetype) || instance != C_C_D))
    {
      tree fn_as_int;

      parms = TREE_CHAIN (parms);

      if (!all_virtual && TREE_CODE (function) == FUNCTION_DECL)
	fn_as_int = build_unary_op (ADDR_EXPR, function, 0);
      else
	fn_as_int = convert (TREE_TYPE (default_conversion (function)), DECL_VINDEX (function));
      if (all_virtual == 1)
	fn_as_int = convert (integer_type_node, fn_as_int);

      result = build_opfncall (METHOD_CALL_EXPR, LOOKUP_NORMAL, instance, fn_as_int, parms);

      if (result == NULL_TREE)
	{
	  compiler_error ("could not overload `operator->()(...)'");
	  return error_mark_node;
	}
      else if (result == error_mark_node)
	return error_mark_node;

#if 0
      /* Do this if we want the result of operator->() to inherit
	 the type of the function it is subbing for.  */
      TREE_TYPE (result) = value_type;
#endif

      return result;
    }
#endif

  if (need_vtbl == needed)
    {
      function = build_vfn_ref (&TREE_VALUE (parms), instance, DECL_VINDEX (function));
      TREE_TYPE (function) = build_pointer_type (fntype);
    }

  if (TREE_CODE (function) == FUNCTION_DECL)
    GNU_xref_call (current_function_decl,
		   IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (function)));

  {
    int is_constructor;
    
    if (TREE_CODE (function) == FUNCTION_DECL)
      {
	is_constructor = DECL_CONSTRUCTOR_P (function);
	if (DECL_INLINE (function))
	  function = build1 (ADDR_EXPR, build_pointer_type (fntype), function);
	else
	  {
	    assemble_external (function);
	    TREE_USED (function) = 1;
	    function = default_conversion (function);
	  }
      }
    else
      {
	is_constructor = 0;
	function = default_conversion (function);
      }

    result = build_nt (CALL_EXPR, function, parms, NULL_TREE);

    TREE_TYPE (result) = value_type;
    TREE_SIDE_EFFECTS (result) = 1;
    TREE_RAISES (result)
      = TYPE_RAISES_EXCEPTIONS (fntype) || (parms && TREE_RAISES (parms));
    TREE_HAS_CONSTRUCTOR (result) = is_constructor;
    return result;
  }
}

/* Similar to `build_method_call', but for overloaded non-member functions.
   The name of this function comes through NAME.  The name depends
   on PARMS.

   Note that this function must handle simple `C' promotions,
   as well as variable numbers of arguments (...), and
   default arguments to boot.

   If the overloading is successful, we return a tree node which
   contains the call to the function.

   If overloading produces candidates which are probable, but not definite,
   we hold these candidates.  If FINAL_CP is non-zero, then we are free
   to assume that final_cp points to enough storage for all candidates that
   this function might generate.  The `harshness' array is preallocated for
   the first candidate, but not for subsequent ones.

   Note that the DECL_RTL of FUNCTION must be made to agree with this
   function's new name.  */

tree
build_overload_call_real (fnname, parms, flags, final_cp, buildxxx)
     tree fnname, parms;
     int flags;
     struct candidate *final_cp;
     int buildxxx;
{
  /* must check for overloading here */
  tree overload_name, functions, function, parm;
  tree parmtypes = NULL_TREE, last = NULL_TREE;
  register tree outer;
  int length;
  int parmlength = list_length (parms);

  struct candidate *candidates, *cp;

  if (final_cp)
    {
      if (flag_ansi_overloading)
	{
	  final_cp[0].h.code = 0;
	  final_cp[0].h.distance = 0;
	  final_cp[0].function = 0;
	  /* end marker.  */
	  final_cp[1].h.code = EVIL_CODE;
	}
      else
	{
	  final_cp[0].evil = 0;
	  final_cp[0].user = 0;
	  final_cp[0].b_or_d = 0;
	  final_cp[0].easy = 0;
	  final_cp[0].function = 0;
	  /* end marker.  */
	  final_cp[1].evil = 1;
	}
    }

  for (parm = parms; parm; parm = TREE_CHAIN (parm))
    {
      register tree t = TREE_TYPE (TREE_VALUE (parm));

      if (t == error_mark_node)
	{
	  if (final_cp)
	    {
	      if (flag_ansi_overloading)
		final_cp->h.code = EVIL_CODE;
	      else
		final_cp->evil = 1;
	    }
	  return error_mark_node;
	}
      if (TREE_CODE (t) == ARRAY_TYPE || TREE_CODE (t) == OFFSET_TYPE)
	{
	  /* Perform the conversion from ARRAY_TYPE to POINTER_TYPE in place.
	     Also convert OFFSET_TYPE entities to their normal selves.
	     This eliminates needless calls to `compute_conversion_costs'.  */
	  TREE_VALUE (parm) = default_conversion (TREE_VALUE (parm));
	  t = TREE_TYPE (TREE_VALUE (parm));
	}
      last = build_tree_list (NULL_TREE, t);
      parmtypes = chainon (parmtypes, last);
    }
  if (last)
    TREE_CHAIN (last) = void_list_node;
  else
    parmtypes = void_list_node;

  if (! flag_ansi_overloading)
    {
      /* This is a speed improvement that ends up not working properly in
	 the situation of fns with and without default parameters.  I turned
	 this off in the new method so it'll go through the argument matching
	 code to properly diagnose a match/failure. (bpk)  */
      overload_name = build_decl_overload (fnname, parmtypes, 0);

      /* Now check to see whether or not we can win.
	 Note that if we are called from `build_method_call',
	 then we cannot have a mis-match, because we would have
	 already found such a winning case.  */

      if (IDENTIFIER_GLOBAL_VALUE (overload_name))
	if (TREE_CODE (IDENTIFIER_GLOBAL_VALUE (overload_name)) != TREE_LIST)
	  return build_function_call (DECL_MAIN_VARIANT (IDENTIFIER_GLOBAL_VALUE (overload_name)), parms);
    }

  functions = IDENTIFIER_GLOBAL_VALUE (fnname);

  if (functions == NULL_TREE)
    {
      if (flags & LOOKUP_SPECULATIVELY)
	return NULL_TREE;
      if (flags & LOOKUP_COMPLAIN)
	error ("only member functions apply");
      if (final_cp)
	{
	  if (flag_ansi_overloading)
	    final_cp->h.code = EVIL_CODE;
	  else
	    final_cp->evil = 1;
	}
      return error_mark_node;
    }

  if (! TREE_OVERLOADED (fnname))
    {
      functions = DECL_MAIN_VARIANT (functions);
      if (final_cp)
	{
	  /* We are just curious whether this is a viable alternative or
             not.  */
	  compute_conversion_costs (functions, parms, final_cp, parmlength);
	  return functions;
	}
      else
	return build_function_call_real (functions, parms, 1, flags);
    }

  if (TREE_CODE (functions) == TREE_LIST
      && TREE_VALUE (functions) == NULL_TREE)
    {
      if (flags & LOOKUP_SPECULATIVELY)
	return NULL_TREE;
      
      if (flags & LOOKUP_COMPLAIN)
	cp_error ("function `%D' declared overloaded, but no instances of that function declared",
		  TREE_PURPOSE (functions));
      if (final_cp)
	{
	  if (flag_ansi_overloading)
	    final_cp->h.code = EVIL_CODE;
	  else
	    final_cp->evil = 1;
	}
      return error_mark_node;
    }

  length = count_functions (functions);
  
  if (final_cp)
    candidates = final_cp;
  else
    {
      candidates
	= (struct candidate *)alloca ((length+1) * sizeof (struct candidate));
      bzero (candidates, (length + 1) * sizeof (struct candidate));
    }

  cp = candidates;

  my_friendly_assert (is_overloaded_fn (functions), 169);

  functions = get_first_fn (functions);

  /* OUTER is the list of FUNCTION_DECLS, in a TREE_LIST.  */
  for (outer = functions; outer; outer = DECL_CHAIN (outer))
    {
      int template_cost = 0;
      function = outer;
      if (TREE_CODE (function) != FUNCTION_DECL
	  && ! (TREE_CODE (function) == TEMPLATE_DECL
		&& ! DECL_TEMPLATE_IS_CLASS (function)
		&& TREE_CODE (DECL_TEMPLATE_RESULT (function)) == FUNCTION_DECL))
	{
	  enum tree_code code = TREE_CODE (function);
	  if (code == TEMPLATE_DECL)
	    code = TREE_CODE (DECL_TEMPLATE_RESULT (function));
	  if (code == CONST_DECL)
	    cp_error_at
	      ("enumeral value `%D' conflicts with function of same name",
	       function);
	  else if (code == VAR_DECL)
	    {
	      if (TREE_STATIC (function))
		cp_error_at
		  ("variable `%D' conflicts with function of same name",
		   function);
	      else
		cp_error_at
		  ("constant field `%D' conflicts with function of same name",
		   function);
	    }
	  else if (code == TYPE_DECL)
	    continue;
	  else
	    my_friendly_abort (2);
	  error ("at this point in file");
	  continue;
	}
      if (TREE_CODE (function) == TEMPLATE_DECL)
	{
	  int ntparms = TREE_VEC_LENGTH (DECL_TEMPLATE_PARMS (function));
	  tree *targs = (tree *) alloca (sizeof (tree) * ntparms);
	  int i;

	  i = type_unification (DECL_TEMPLATE_PARMS (function), targs,
				TYPE_ARG_TYPES (TREE_TYPE (function)),
				parms, &template_cost, 0);
	  if (i == 0)
	    {
	      struct candidate *cp2;

	      function = instantiate_template (function, targs);
	      /* Now check that the template instantiated for this is not
		 the same as a function that's in the list due to some
		 previous instantiation.  */
	      cp2 = candidates;
	      while (cp2 != cp)
		if (cp2->function == function)
		  break;
		else
		  cp2 += 1;
	      if (cp2->function == function)
		continue;
	    }
	}

      if (TREE_CODE (function) == TEMPLATE_DECL)
	{
	  /* Unconverted template -- failed match.  */
	  cp->function = function;
	  cp->u.bad_arg = -4;
	  if (flag_ansi_overloading)
	    cp->h.code = EVIL_CODE;
	  else
	    cp->evil = 1;
	}
      else
	{
	  function = DECL_MAIN_VARIANT (function);

	  /* Can't use alloca here, since result might be
	     passed to calling function.  */
	  cp->h_len = parmlength;
	  if (flag_ansi_overloading)
	    cp->v.ansi_harshness = (struct harshness_code *)
	      oballoc ((parmlength + 1) * sizeof (struct harshness_code));
	  else
	    cp->v.old_harshness = (unsigned short *)
	      oballoc ((parmlength + 1) * sizeof (unsigned short));

	  compute_conversion_costs (function, parms, cp, parmlength);

	  if (flag_ansi_overloading)
	    /* Make sure this is clear as well.  */
	    cp->h.int_penalty += template_cost;
	  else
	    /* Should really add another field...  */
	    cp->easy = cp->easy * 128 + template_cost;

	  /* It seemed easier to have both if stmts in here, rather
	     than excluding the hell out of it with flag_ansi_overloading
	     everywhere. (bpk) */
	  if (flag_ansi_overloading)
	    {
	      if ((cp[0].h.code & EVIL_CODE) == 0)
		{
		  cp[1].h.code = EVIL_CODE;

		  /* int_penalty is set by convert_harshness_ansi for cases
		     where we need to know about any penalties that would
		     otherwise make a TRIVIAL_CODE pass.  */
		  if (final_cp
		      && template_cost == 0
		      && cp[0].h.code <= TRIVIAL_CODE
		      && cp[0].h.int_penalty == 0)
		    {
		      final_cp[0].h = cp[0].h;
		      return function;
		    }
		  cp++;
		}
	    }
	  else
	    {
	      if (cp[0].evil == 0)
		{
		  cp[1].evil = 1;
		  if (final_cp
		      && cp[0].user == 0 && cp[0].b_or_d == 0
		      && template_cost == 0
		      && cp[0].easy <= 1)
		    {
		      final_cp[0].easy = cp[0].easy;
		      return function;
		    }
		  cp++;
		}
	    }
	}
    }

  if (cp - candidates)
    {
      tree rval = error_mark_node;

      /* Leave marker.  */
      if (flag_ansi_overloading)
	cp[0].h.code = EVIL_CODE;
      else
	cp[0].evil = 1;
      if (cp - candidates > 1)
	{
	  struct candidate *best_cp
	    = ideal_candidate (NULL_TREE, candidates,
			       cp - candidates, parms, parmlength);
	  if (best_cp == (struct candidate *)0)
	    {
	      if (flags & LOOKUP_COMPLAIN)
		{
		  cp_error ("call of overloaded `%D' is ambiguous", fnname);
		  print_n_candidates (candidates, cp - candidates);
		}
	      return error_mark_node;
	    }
	  else
	    rval = best_cp->function;
	}
      else
	{
	  cp -= 1;
	  if ((flag_ansi_overloading && (cp->h.code & EVIL_CODE))
	      || (!flag_ansi_overloading && cp->evil > 1))
	    {
	      if (flags & LOOKUP_COMPLAIN)
		error ("type conversion ambiguous");
	    }
	  else
	    rval = cp->function;
	}

      if (final_cp)
	return rval;

      return buildxxx ? build_function_call_real (rval, parms, 0, flags)
        : build_function_call_real (rval, parms, 1, flags);
    }

  if (flags & LOOKUP_SPECULATIVELY)
    return NULL_TREE;
  
  if (flags & LOOKUP_COMPLAIN)
    report_type_mismatch (cp, parms, "function",
			  decl_as_string (cp->function, 1));

  return error_mark_node;
}

tree
build_overload_call (fnname, parms, flags, final_cp)
     tree fnname, parms;
     int flags;
     struct candidate *final_cp;
{
  return build_overload_call_real (fnname, parms, flags, final_cp, 0);
}

tree
build_overload_call_maybe (fnname, parms, flags, final_cp)
     tree fnname, parms;
     int flags;
     struct candidate *final_cp;
{
  return build_overload_call_real (fnname, parms, flags, final_cp, 1);
}
