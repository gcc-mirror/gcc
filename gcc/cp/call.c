/* Functions related to invoking methods and overloaded functions.
   Copyright (C) 1987, 92, 93, 94, 95, 96, 1997 Free Software Foundation, Inc.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* High-level class interface.  */

#include "config.h"
#include "tree.h"
#include <stdio.h>
#include "cp-tree.h"
#include "class.h"
#include "output.h"
#include "flags.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "obstack.h"
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern int inhibit_warnings;
extern tree ctor_label, dtor_label;

/* Compute the ease with which a conversion can be performed
   between an expected and the given type.  */

static struct harshness_code convert_harshness PROTO((register tree, register tree, tree));
static tree build_new_method_call		PROTO((tree, tree, tree, tree, int));

static int rank_for_ideal PROTO((struct candidate *,
				 struct candidate *));
static int user_harshness PROTO((tree, tree));
static int strictly_better PROTO((unsigned int, unsigned int));
static struct candidate * ideal_candidate PROTO((struct candidate *,
						 int, int));
static int may_be_remote PROTO((tree));
static tree build_field_call PROTO((tree, tree, tree, tree));
static tree find_scoped_type PROTO((tree, tree, tree));
static void print_candidates PROTO((tree));
static struct z_candidate * tourney PROTO((struct z_candidate *));
static int joust PROTO((struct z_candidate *, struct z_candidate *));
static int compare_qual PROTO((tree, tree));
static int compare_ics PROTO((tree, tree));
static tree build_over_call PROTO((tree, tree, tree, int));
static tree convert_default_arg PROTO((tree, tree));
static void enforce_access PROTO((tree, tree));
static tree convert_like PROTO((tree, tree));
static void op_error PROTO((enum tree_code, enum tree_code, tree, tree,
			    tree, char *));
static tree build_object_call PROTO((tree, tree));
static tree resolve_args PROTO((tree));
static struct z_candidate * build_user_type_conversion_1
	PROTO ((tree, tree, int));
static void print_z_candidates PROTO((struct z_candidate *));
static tree build_this PROTO((tree));
static struct z_candidate * splice_viable PROTO((struct z_candidate *));
static int any_viable PROTO((struct z_candidate *));
static struct z_candidate * add_template_candidate
	PROTO((struct z_candidate *, tree, tree, tree, tree, int));
static struct z_candidate * add_template_conv_candidate 
        PROTO((struct z_candidate *, tree, tree, tree, tree));
static struct z_candidate * add_builtin_candidates
	PROTO((struct z_candidate *, enum tree_code, enum tree_code,
	       tree, tree *, int));
static struct z_candidate * add_builtin_candidate
	PROTO((struct z_candidate *, enum tree_code, enum tree_code,
	       tree, tree, tree, tree *, tree *, int));
static int is_complete PROTO((tree));
static struct z_candidate * build_builtin_candidate 
	PROTO((struct z_candidate *, tree, tree, tree, tree *, tree *,
	       int));
static struct z_candidate * add_conv_candidate 
	PROTO((struct z_candidate *, tree, tree, tree));
static struct z_candidate * add_function_candidate 
	PROTO((struct z_candidate *, tree, tree, int));
static tree implicit_conversion PROTO((tree, tree, tree, int));
static tree standard_conversion PROTO((tree, tree, tree));
static tree reference_binding PROTO((tree, tree, tree, int));
static tree strip_top_quals PROTO((tree));
static tree non_reference PROTO((tree));
static tree build_conv PROTO((enum tree_code, tree, tree));
static void print_n_candidates PROTO((struct candidate *, int));
static tree default_parm_conversions PROTO((tree, tree *));
static int is_subseq PROTO((tree, tree));

#define EVIL_RETURN(ARG)	((ARG).code = EVIL_CODE, (ARG))
#define STD_RETURN(ARG)		((ARG).code = STD_CODE, (ARG))
#define QUAL_RETURN(ARG)	((ARG).code = QUAL_CODE, (ARG))
#define TRIVIAL_RETURN(ARG)	((ARG).code = TRIVIAL_CODE, (ARG))
#define ZERO_RETURN(ARG)	((ARG).code = 0, (ARG))

/* Ordering function for overload resolution.  Compare two candidates
   by gross quality.  */

int
rank_for_overload (x, y)
     struct candidate *x, *y;
{
  if (y->h.code & (EVIL_CODE|ELLIPSIS_CODE|USER_CODE))
    return y->h.code - x->h.code;
  if (x->h.code & (EVIL_CODE|ELLIPSIS_CODE|USER_CODE))
    return -1;

  /* This is set by compute_conversion_costs, for calling a non-const
     member function from a const member function.  */
  if ((y->harshness[0].code & CONST_CODE) ^ (x->harshness[0].code & CONST_CODE))
    return y->harshness[0].code - x->harshness[0].code;

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

/* Compare two candidates, argument by argument.  */

static int
rank_for_ideal (x, y)
     struct candidate *x, *y;
{
  int i;

  if (x->h_len != y->h_len)
    abort ();

  for (i = 0; i < x->h_len; i++)
    {
      if (y->harshness[i].code - x->harshness[i].code)
	return y->harshness[i].code - x->harshness[i].code;
      if ((y->harshness[i].code & STD_CODE)
	  && (y->harshness[i].distance - x->harshness[i].distance))
	return y->harshness[i].distance - x->harshness[i].distance;

      /* They're both the same code.  Now see if we're dealing with an
	 integral promotion that needs a finer grain of accuracy.  */
      if (y->harshness[0].code & PROMO_CODE
	  && (y->harshness[i].int_penalty ^ x->harshness[i].int_penalty))
	return y->harshness[i].int_penalty - x->harshness[i].int_penalty;
    }
  return 0;
}

/* TYPE is the type we wish to convert to.  PARM is the parameter
   we have to work with.  We use a somewhat arbitrary cost function
   to measure this conversion.  */

static struct harshness_code
convert_harshness (type, parmtype, parm)
     register tree type, parmtype;
     tree parm;
{
  struct harshness_code h;
  register enum tree_code codel;
  register enum tree_code coder;
  int lvalue;

  h.code = 0;
  h.distance = 0;
  h.int_penalty = 0;

#ifdef GATHER_STATISTICS
  n_convert_harshness++;
#endif

  if (TREE_CODE (parmtype) == REFERENCE_TYPE)
    {
      if (parm)
	parm = convert_from_reference (parm);
      parmtype = TREE_TYPE (parmtype);
      lvalue = 1;
    }
  else if (parm)
    lvalue = lvalue_p (parm);
  else
    lvalue = 0;

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

  if (codel == REFERENCE_TYPE)
    {
      tree ttl, ttr;
      int constp = parm ? TREE_READONLY (parm) : TYPE_READONLY (parmtype);
      int volatilep = (parm ? TREE_THIS_VOLATILE (parm)
		       : TYPE_VOLATILE (parmtype));
      register tree intype = TYPE_MAIN_VARIANT (parmtype);
      register enum tree_code form = TREE_CODE (intype);
      int penalty = 0;

      ttl = TREE_TYPE (type);

      /* Only allow const reference binding if we were given a parm to deal
         with, since it isn't really a conversion.  This is a hack to
         prevent build_type_conversion from finding this conversion, but
         still allow overloading to find it.  */
      if (! lvalue && ! (parm && TYPE_READONLY (ttl)))
	return EVIL_RETURN (h);

      if ((TYPE_READONLY (ttl) < constp)
	  || (TYPE_VOLATILE (ttl) < volatilep))
	return EVIL_RETURN (h);

      /* When passing a non-const argument into a const reference, dig it a
	 little, so a non-const reference is preferred over this one.  */
      penalty = ((TYPE_READONLY (ttl) > constp)
		 + (TYPE_VOLATILE (ttl) > volatilep));

      ttl = TYPE_MAIN_VARIANT (ttl);

      if (form == OFFSET_TYPE)
	{
	  intype = TREE_TYPE (intype);
	  form = TREE_CODE (intype);
	}

      ttr = intype;

      if (TREE_CODE (ttl) == ARRAY_TYPE && TREE_CODE (ttr) == ARRAY_TYPE)
	{
	  if (comptypes (ttl, ttr, 1))
	    return ZERO_RETURN (h);
	  return EVIL_RETURN (h);
	}

      h = convert_harshness (ttl, ttr, NULL_TREE);
      if (penalty && h.code == 0)
	{
	  h.code = QUAL_CODE;
	  h.int_penalty = penalty;
	}
      return h;
    }

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

      if (type != parmtype && coder == METHOD_TYPE)
	{
	  tree ttl = TYPE_METHOD_BASETYPE (type);
	  tree ttr = TYPE_METHOD_BASETYPE (parmtype);

	  int b_or_d = get_base_distance (ttr, ttl, 0, (tree*)0);
	  if (b_or_d < 0)
	    {
	      b_or_d = get_base_distance (ttl, ttr, 0, (tree*)0);
	      if (b_or_d < 0)
		return EVIL_RETURN (h);
	      h.distance = -b_or_d;
	    }
	  else
	    h.distance = b_or_d;
	  h.code = STD_CODE;

	  type = build_function_type
	    (TREE_TYPE (type), TREE_CHAIN (TYPE_ARG_TYPES (type)));
	  parmtype = build_function_type
	    (TREE_TYPE (parmtype), TREE_CHAIN (TYPE_ARG_TYPES (parmtype)));
	}

      /* We allow the default conversion between function type
	 and pointer-to-function type for free.  */
      if (comptypes (type, parmtype, 1))
	return h;

      if (pedantic)
	return EVIL_RETURN (h);

      /* Compare return types.  */
      p1 = TREE_TYPE (type);
      p2 = TREE_TYPE (parmtype);
      h2 = convert_harshness (p1, p2, NULL_TREE);
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
#if 0
	      static int explained = 0;
	      if (h2.distance < 0)
		message_2_types (sorry, "cannot cast `%s' to `%s' at function call site", p2, p1);
	      else
		message_2_types (sorry, "cannot cast `%s' to `%s' at function call site", p1, p2);

	      if (! explained++)
		sorry ("(because pointer values change during conversion)");
#endif
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
	  h2 = convert_harshness (TREE_VALUE (p1), TREE_VALUE (p2),
				       NULL_TREE);
	  if (h2.code & EVIL_CODE)
	    return h2;

	  if (h2.distance)
	    {
	      /* This only works for pointers and references.  */
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
      tree ttl, ttr;

      /* Get to the OFFSET_TYPE that this might be.  */
      type = TREE_TYPE (type);

      if (coder != TREE_CODE (type))
	return EVIL_RETURN (h);

      ttl = TYPE_OFFSET_BASETYPE (type);
      ttr = TYPE_OFFSET_BASETYPE (parmtype);

      if (ttl == ttr)
	h.code = 0;
      else
	{
	  int b_or_d = get_base_distance (ttr, ttl, 0, (tree*)0);
	  if (b_or_d < 0)
	    {
	      b_or_d = get_base_distance (ttl, ttr, 0, (tree*)0);
	      if (b_or_d < 0)
		return EVIL_RETURN (h);
	      h.distance = -b_or_d;
	    }
	  else
	    h.distance = b_or_d;
	  h.code = STD_CODE;
	}

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

  if (codel == BOOLEAN_TYPE)
    {
      if (INTEGRAL_CODE_P (coder) || coder == REAL_TYPE)
	return STD_RETURN (h);
      else if (coder == POINTER_TYPE || coder == OFFSET_TYPE)
	{
	  /* Make this worse than any conversion to another pointer.
	     FIXME this is how I think the language should work, but it may not
	     end up being how the language is standardized (jason 1/30/95).  */
	  h.distance = 32767;
	  return STD_RETURN (h);
	}
      return EVIL_RETURN (h);
    }

  if (INTEGRAL_CODE_P (codel))
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

      if (INTEGRAL_CODE_P (coder))
	{
	  if (TYPE_MAIN_VARIANT (type)
	      == TYPE_MAIN_VARIANT (type_promotes_to (parmtype)))
	    {
	      h.code = PROMO_CODE;
	    }
	  else
	    h.code = STD_CODE;
	    
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
	  if (TYPE_MAIN_VARIANT (type)
	      == TYPE_MAIN_VARIANT (type_promotes_to (parmtype)))
	    h.code = PROMO_CODE;
	  else
	    h.code = STD_CODE;
	    
	  return h;
	}
      else if (INTEGRAL_CODE_P (coder))
	{
	  h.code = STD_CODE;
	  h.distance = 0;
	  return h;
	}
    }

  /* Convert arrays which have not previously been converted.  */
  if (coder == ARRAY_TYPE)
    {
      coder = POINTER_TYPE;
      if (parm)
	{
	  parm = decay_conversion (parm);
	  parmtype = TREE_TYPE (parm);
	}
      else
	parmtype = build_pointer_type (TREE_TYPE (parmtype));
    }

  /* Conversions among pointers */
  if (codel == POINTER_TYPE && coder == POINTER_TYPE)
    {
      register tree ttl = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      register tree ttr = TYPE_MAIN_VARIANT (TREE_TYPE (parmtype));
      int penalty = 4 * (ttl != ttr);

      /* Anything converts to void *.  Since this may be `const void *'
	 (etc.) use VOID_TYPE instead of void_type_node.  Otherwise, the
	 targets must be the same, except that we do allow (at some cost)
	 conversion between signed and unsigned pointer types.  */

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
      if (TREE_CODE (ttl) != VOID_TYPE
	  && (TREE_CODE (ttr) != VOID_TYPE || !parm || !null_ptr_cst_p (parm)))
	{
	  if (comp_target_types (type, parmtype, 1) <= 0)
	    return EVIL_RETURN (h);
	}
#else
      if (!(TREE_CODE (ttl) == VOID_TYPE
	    || TREE_CODE (ttr) == VOID_TYPE
	    || (TREE_UNSIGNED (ttl) ^ TREE_UNSIGNED (ttr)
		&& (ttl = unsigned_type (ttl),
		    ttr = unsigned_type (ttr),
		    penalty = 10, 0))
	    || (comp_target_types (ttl, ttr, 0) > 0)))
	return EVIL_RETURN (h);
#endif

      if (ttr == ttl)
	{
	  tree tmp1 = TREE_TYPE (type), tmp2 = TREE_TYPE (parmtype);

	  h.code = 0;
	  /* Note conversion from `T*' to `const T*',
	                       or `T*' to `volatile T*'.  */
	  if ((TYPE_READONLY (tmp1) < TREE_READONLY (tmp2))
	      || (TYPE_VOLATILE (tmp1) < TYPE_VOLATILE (tmp2)))
	    h.code = EVIL_CODE;
	  else if ((TYPE_READONLY (tmp1) != TREE_READONLY (tmp2))
		   || (TYPE_VOLATILE (tmp1) != TYPE_VOLATILE (tmp2)))
	    h.code |= QUAL_CODE;

	  h.distance = 0;
	  return h;
	}


      if (TREE_CODE (ttl) == RECORD_TYPE && TREE_CODE (ttr) == RECORD_TYPE)
	{
	  int b_or_d = get_base_distance (ttl, ttr, 0, (tree*)0);
	  if (b_or_d < 0)
	    {
	      b_or_d = get_base_distance (ttr, ttl, 0, (tree*)0);
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
      /* Catch things like `const char *' -> `const void *'
	 vs `const char *' -> `void *'.  */
      if (ttl != ttr)
	{
	  tree tmp1 = TREE_TYPE (type), tmp2 = TREE_TYPE (parmtype);
	  if ((TYPE_READONLY (tmp1) < TREE_READONLY (tmp2))
	      || (TYPE_VOLATILE (tmp1) < TYPE_VOLATILE (tmp2)))
	    h.code = EVIL_CODE;
	  else if ((TYPE_READONLY (tmp1) > TREE_READONLY (tmp2))
		   || (TYPE_VOLATILE (tmp1) > TYPE_VOLATILE (tmp2)))
	    h.code |= QUAL_CODE;
	}
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

  /* C++: Since the `this' parameter of a signature member function
     is represented as a signature pointer to handle default implementations
     correctly, we can have the case that `type' is a signature pointer
     while `parmtype' is a pointer to a signature table.  We don't really
     do any conversions in this case, so just return 0.  */

  if (codel == RECORD_TYPE && coder == POINTER_TYPE
      && IS_SIGNATURE_POINTER (type) && IS_SIGNATURE (TREE_TYPE (parmtype)))
    return ZERO_RETURN (h);

  if (codel == RECORD_TYPE && coder == RECORD_TYPE)
    {
      int b_or_d = get_base_distance (type, parmtype, 0, (tree*)0);
      if (b_or_d < 0)
	{
	  b_or_d = get_base_distance (parmtype, type, 0, (tree*)0);
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

/* A clone of build_type_conversion for checking user-defined conversions in
   overload resolution.  */

static int
user_harshness (type, parmtype)
     register tree type, parmtype;
{
  tree conv;
  tree winner = NULL_TREE;
  int code;

  {
    tree typename = build_typename_overload (type);
    if (lookup_fnfields (TYPE_BINFO (parmtype), typename, 0))
      return 0;
  }
			
  for (conv = lookup_conversions (parmtype); conv; conv = TREE_CHAIN (conv))
    {
      struct harshness_code tmp;
      tree cand = TREE_VALUE (conv);

      if (winner && winner == cand)
	continue;

      tmp = convert_harshness (type, TREE_TYPE (TREE_TYPE (cand)), NULL_TREE);
      if ((tmp.code < USER_CODE) && (tmp.distance >= 0))
	{
	  if (winner)
	    return EVIL_CODE;
	  else
	    {
	      winner = cand;
	      code = tmp.code;
	    }
	}
    }

  if (winner)
    return code;

  return -1;
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
compute_conversion_costs (function, tta_in, cp, arglen)
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
  extern int cp_silent;

#ifdef GATHER_STATISTICS
  n_compute_conversion_costs++;
#endif

#ifndef DEBUG_MATCHING
  /* We don't emit any warnings or errors while trying out each candidate.  */
  cp_silent = 1;
#endif

  cp->function = function;
  cp->arg = tta ? TREE_VALUE (tta) : NULL_TREE;
  cp->u.bad_arg = 0;		/* optimistic!  */

  cp->h.code = 0;
  cp->h.distance = 0;
  cp->h.int_penalty = 0;
  bzero ((char *) cp->harshness,
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
	    h = convert_harshness (lhstype, TREE_TYPE (rhs), rhs);
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

	  h = convert_harshness (TREE_VALUE (ttf),
				 TREE_TYPE (TREE_VALUE (tta)),
				 TREE_VALUE (tta));

#ifdef DEBUG_MATCHING
	  cp_error ("     evaluated %s", print_harshness (&h));
#endif
	}

      cp->harshness[strike_index] = h;
      if ((h.code & EVIL_CODE)
	  || ((h.code & STD_CODE) && h.distance < 0))
	{
	  cp->u.bad_arg = strike_index;
	  evil_strikes = 1;
	}
     else if (h.code & ELLIPSIS_CODE)
       ellipsis_strikes += 1;
#if 0
      /* This is never set by `convert_harshness'.  */
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
	  cp_silent = 0;
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
	    cp->harshness[strike_index++] = h;
	}
    }
  else if (ttf && ttf != void_list_node)
    {
      /* ran out of actuals, and no defaults.  */
      if (TREE_PURPOSE (ttf) == NULL_TREE)
	{
	  cp->h.code = EVIL_CODE;
	  cp->u.bad_arg = -2;
	  cp_silent = 0;
	  return;
	}
      /* Store index of first default.  */
      cp->harshness[arglen].distance = strike_index+1;
    }
  else
    cp->harshness[arglen].distance = 0;

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
	  cp_silent = 0;
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

	  lose = cp->harshness[strike_index];
	  if ((lose.code & EVIL_CODE)
	      || ((lose.code & STD_CODE) && lose.distance < 0))
	    {
	      tree actual_type = TREE_TYPE (TREE_VALUE (tta));
	      tree formal_type = TREE_VALUE (ttf);
	      int extra_conversions = 0;

	      dont_convert_types = 1;

	      if (TREE_CODE (formal_type) == REFERENCE_TYPE)
		formal_type = TREE_TYPE (formal_type);
	      if (TREE_CODE (actual_type) == REFERENCE_TYPE)
		actual_type = TREE_TYPE (actual_type);

	      if (formal_type != error_mark_node
		  && actual_type != error_mark_node)
		{
		  formal_type = complete_type (TYPE_MAIN_VARIANT (formal_type));
		  actual_type = complete_type (TYPE_MAIN_VARIANT (actual_type));

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
		      int extra = user_harshness (formal_type, actual_type);

		      if (extra == EVIL_CODE)
			win += 2;
		      else if (extra >= 0)
			{
			  win++;
			  extra_conversions = extra;
			}
		    }
		}
	      dont_convert_types = 0;

	      if (win == 1)
		{
		  user_strikes += 1;
		  cp->harshness[strike_index].code
		    = USER_CODE | (extra_conversions ? STD_CODE : 0);
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
     to const is less useful than defaulting to non-const.  */
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
	  cp->harshness[0].code |= TRIVIAL_CODE;
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
	    cp->harshness[0].code |= CONST_CODE;
	}
    }

  if (evil_strikes)
    cp->h.code = EVIL_CODE;
  if (ellipsis_strikes)
    cp->h.code |= ELLIPSIS_CODE;
  if (user_strikes)
    cp->h.code |= USER_CODE;
  cp_silent = 0;
#ifdef DEBUG_MATCHING
  cp_error ("final eval %s", print_harshness (&cp->h));
#endif
}

/* Subroutine of ideal_candidate.  See if X or Y is a better match
   than the other.  */

static int
strictly_better (x, y)
     unsigned int x, y;
{
  unsigned short xor;

  if (x == y)
    return 0;

  xor = x ^ y;
  if (xor >= x || xor >= y)
    return 1;
  return 0;
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
ideal_candidate (candidates, n_candidates, len)
     struct candidate *candidates;
     int n_candidates;
     int len;
{
  struct candidate *cp = candidates+n_candidates;
  int i, j = -1, best_code;

  /* For each argument, sort the functions from best to worst for the arg.
     For each function that's not best for this arg, set its overall
     harshness to EVIL so that other args won't like it.  The candidate
     list for the last argument is the intersection of all the best-liked
     functions.  */

  qsort (candidates, n_candidates, sizeof (struct candidate),
	 (int (*) PROTO((const void *, const void *))) rank_for_overload);
  best_code = cp[-1].h.code;

  /* If they're at least as good as each other, do an arg-by-arg check.  */
  if (! strictly_better (cp[-1].h.code, cp[-2].h.code))
    {
      int better = 0;
      int worse = 0;

      for (j = 0; j < n_candidates; j++)
	if (! strictly_better (candidates[j].h.code, best_code))
	  break;

      qsort (candidates+j, n_candidates-j, sizeof (struct candidate),
	     (int (*) PROTO((const void *, const void *))) rank_for_ideal);
      for (i = 0; i < len; i++)
	{
	  if (cp[-1].harshness[i].code < cp[-2].harshness[i].code)
	    better = 1;
	  else if (cp[-1].harshness[i].code > cp[-2].harshness[i].code)
	    worse = 1;
	  else if (cp[-1].harshness[i].code & STD_CODE)
	    {
	      /* If it involves a standard conversion, let the
		 inheritance lattice be the final arbiter.  */
	      if (cp[-1].harshness[i].distance > cp[-2].harshness[i].distance)
		worse = 1;
	      else if (cp[-1].harshness[i].distance < cp[-2].harshness[i].distance)
		better = 1;
	    }
	  else if (cp[-1].harshness[i].code & PROMO_CODE)
	    {
	      /* For integral promotions, take into account a finer
		 granularity for determining which types should be favored
		 over others in such promotions.  */
	      if (cp[-1].harshness[i].int_penalty > cp[-2].harshness[i].int_penalty)
		worse = 1;
	      else if (cp[-1].harshness[i].int_penalty < cp[-2].harshness[i].int_penalty)
		better = 1;
	    }
	}

      if (! better || worse)
	return NULL;
    }
  return cp-1;
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
    rval = build_component_ref (datum, DECL_NAME (CLASSTYPE_VFIELD (type)), NULL_TREE, 0);
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

  if (name == ctor_identifier || name == dtor_identifier)
    return NULL_TREE;

  if (instance_ptr == current_class_ptr)
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
	  instance = build_component_ref_1 (current_class_ref, field, 0);
	  if (instance == error_mark_node)
	    return error_mark_node;

	  if (TYPE_LANG_SPECIFIC (TREE_TYPE (instance))
	      && (TYPE_OVERLOADS_CALL_EXPR (TREE_TYPE (instance))
		  || flag_ansi_overloading))
	    return build_opfncall (CALL_EXPR, LOOKUP_NORMAL, instance, parms, NULL_TREE);

	  if (TREE_CODE (TREE_TYPE (instance)) == POINTER_TYPE)
	    {
	      if (TREE_CODE (TREE_TYPE (TREE_TYPE (instance))) == FUNCTION_TYPE)
		return build_function_call (instance, parms);
	      else if (TREE_CODE (TREE_TYPE (TREE_TYPE (instance))) == METHOD_TYPE)
		return build_function_call (instance, expr_tree_cons (NULL_TREE, current_class_ptr, parms));
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

      if (TYPE_LANG_SPECIFIC (ftype)
	  && (TYPE_OVERLOADS_CALL_EXPR (ftype) || flag_ansi_overloading))
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

static tree
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
	    return TYPE_MAIN_DECL (TREE_VALUE (tags));
	  return resolve_scope_to_name (TREE_VALUE (tags), inner_types);
	}
      tags = TREE_CHAIN (tags);
    }

  /* Look for a TYPE_DECL.  */
  for (tags = TYPE_FIELDS (type); tags; tags = TREE_CHAIN (tags))
    if (TREE_CODE (tags) == TYPE_DECL && DECL_NAME (tags) == inner_name)
      {
	/* Code by raeburn.  */
	if (inner_types == NULL_TREE)
	  return tags;
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
	  type = DECL_CONTEXT (TYPE_MAIN_DECL (type));
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
      tree x;
      /* If we have something that's already a type by itself,
	 use that.  */
      if (IDENTIFIER_HAS_TYPE_VALUE (inner_name))
	{
	  if (inner_type)
	    return resolve_scope_to_name (IDENTIFIER_TYPE_VALUE (inner_name),
					  inner_type);
	  return inner_name;
	}
      
      x = lookup_name (inner_name, 0);

      if (x && TREE_CODE (x) == NAMESPACE_DECL)
	{
	  x = lookup_namespace_name (x, inner_type);
	  return x;
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
build_scoped_method_call (exp, basetype, name, parms)
     tree exp, basetype, name, parms;
{
  /* Because this syntactic form does not allow
     a pointer to a base class to be `stolen',
     we need not protect the derived->base conversion
     that happens here.
     
     @@ But we do have to check access privileges later.  */
  tree binfo, decl;
  tree type = TREE_TYPE (exp);

  if (type == error_mark_node
      || basetype == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    {
      if (TREE_CODE (name) == BIT_NOT_EXPR)
	{
	  tree type = get_aggr_from_typedef (TREE_OPERAND (name, 0), 1);
	  name = build_min_nt (BIT_NOT_EXPR, type);
	}
      name = build_min_nt (SCOPE_REF, basetype, name);
      return build_min_nt (METHOD_CALL_EXPR, name, exp, parms, NULL_TREE);
    }

  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  if (TREE_CODE (basetype) == TREE_VEC)
    {
      binfo = basetype;
      basetype = BINFO_TYPE (binfo);
    }
  else
    binfo = NULL_TREE;

  /* Destructors can be "called" for simple types; see 5.2.4 and 12.4 Note
     that explicit ~int is caught in the parser; this deals with typedefs
     and template parms.  */
  if (TREE_CODE (name) == BIT_NOT_EXPR && ! IS_AGGR_TYPE (basetype))
    {
      if (type != basetype)
	cp_error ("type of `%E' does not match destructor type `%T' (type was `%T')",
		  exp, basetype, type);
      name = TREE_OPERAND (name, 0);
      if (basetype != name && basetype != get_type_value (name))
	cp_error ("qualified type `%T' does not match destructor name `~%T'",
		  basetype, name);
      return cp_convert (void_type_node, exp);
    }

  if (! is_aggr_type (basetype, 1))
    return error_mark_node;

  if (! IS_AGGR_TYPE (type))
    {
      cp_error ("base object `%E' of scoped method call is of non-aggregate type `%T'",
		exp, type);
      return error_mark_node;
    }

  if (! binfo)
    {
      binfo = get_binfo (basetype, type, 1);
      if (binfo == error_mark_node)
	return error_mark_node;
      if (! binfo)
	error_not_base_type (basetype, type);
    }

  if (binfo)
    {
      if (TREE_CODE (exp) == INDIRECT_REF)
	decl = build_indirect_ref
	  (convert_pointer_to_real
	   (binfo, build_unary_op (ADDR_EXPR, exp, 0)), NULL_PTR);
      else
	decl = build_scoped_ref (exp, basetype);

      /* Call to a destructor.  */
      if (TREE_CODE (name) == BIT_NOT_EXPR)
	{
	  /* Explicit call to destructor.  */
	  name = TREE_OPERAND (name, 0);
	  if (! (name == TYPE_MAIN_VARIANT (TREE_TYPE (decl))
		 || name == constructor_name (TREE_TYPE (decl))
		 || TREE_TYPE (decl) == get_type_value (name)))
	    {
	      cp_error
		("qualified type `%T' does not match destructor name `~%T'",
		 TREE_TYPE (decl), name);
	      return error_mark_node;
	    }
	  if (! TYPE_HAS_DESTRUCTOR (TREE_TYPE (decl)))
	    return cp_convert (void_type_node, exp);
	  
	  return build_delete (TREE_TYPE (decl), decl, integer_two_node,
			       LOOKUP_NORMAL|LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR,
			       0);
	}

      /* Call to a method.  */
      return build_method_call (decl, name, parms, binfo,
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

/* We want the address of a function or method.  We avoid creating a
   pointer-to-member function.  */

tree
build_addr_func (function)
     tree function;
{
  tree type = TREE_TYPE (function);

  /* We have to do these by hand to avoid real pointer to member
     functions.  */
  if (TREE_CODE (type) == METHOD_TYPE)
    {
      tree addr;

      type = build_pointer_type (type);

      if (mark_addressable (function) == 0)
	return error_mark_node;

      addr = build1 (ADDR_EXPR, type, function);

      /* Address of a static or external variable or function counts
	 as a constant */
      if (staticp (function))
	TREE_CONSTANT (addr) = 1;

      function = addr;
    }
  else
    function = default_conversion (function);

  return function;
}

/* Build a CALL_EXPR, we can handle FUNCTION_TYPEs, METHOD_TYPEs, or
   POINTER_TYPE to those.  Note, pointer to member function types
   (TYPE_PTRMEMFUNC_P) must be handled by our callers.  */

tree
build_call (function, result_type, parms)
     tree function, result_type, parms;
{
  int is_constructor = 0;

  function = build_addr_func (function);

  if (TYPE_PTRMEMFUNC_P (TREE_TYPE (function)))
    {
      sorry ("unable to call pointer to member function here");
      return error_mark_node;
    }

  if (TREE_CODE (function) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (function, 0)) == FUNCTION_DECL
      && DECL_CONSTRUCTOR_P (TREE_OPERAND (function, 0)))
    is_constructor = 1;

  function = build_nt (CALL_EXPR, function, parms, NULL_TREE);
  TREE_HAS_CONSTRUCTOR (function) = is_constructor;
  TREE_TYPE (function) = result_type;
  TREE_SIDE_EFFECTS (function) = 1;
  
  return function;
}

static tree
default_parm_conversions (parms, last)
     tree parms, *last;
{
  tree parm, parmtypes = NULL_TREE;

  *last = NULL_TREE;

  for (parm = parms; parm; parm = TREE_CHAIN (parm))
    {
      tree t = TREE_TYPE (TREE_VALUE (parm));

      if (TREE_CODE (t) == OFFSET_TYPE
	  || TREE_CODE (t) == METHOD_TYPE
	  || TREE_CODE (t) == FUNCTION_TYPE)
	{
	  TREE_VALUE (parm) = default_conversion (TREE_VALUE (parm));
	  t = TREE_TYPE (TREE_VALUE (parm));
	}

      if (t == error_mark_node)
	  return error_mark_node;

      *last = build_tree_list (NULL_TREE, t);
      parmtypes = chainon (parmtypes, *last);
    }

  return parmtypes;
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
  register tree baselink, result, parmtypes;
  tree last;
  int pass;
  tree access = access_public_node;
  tree orig_basetype = basetype_path ? BINFO_TYPE (basetype_path) : NULL_TREE;

  /* Range of cases for vtable optimization.  */
  enum vtable_needs { not_needed, maybe_needed, unneeded, needed };
  enum vtable_needs need_vtbl = not_needed;

  char *name_kind;
  tree save_name = name;
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

  if (processing_template_decl)
    {
      if (TREE_CODE (name) == BIT_NOT_EXPR)
	{
	  tree type = get_aggr_from_typedef (TREE_OPERAND (name, 0), 1);
	  name = build_min_nt (BIT_NOT_EXPR, type);
	}

      return build_min_nt (METHOD_CALL_EXPR, name, instance, parms, NULL_TREE);
    }

  /* This is the logic that magically deletes the second argument to
     operator delete, if it is not needed.  */
  if (name == ansi_opname[(int) DELETE_EXPR] && list_length (parms)==2)
    {
      tree save_last = TREE_CHAIN (parms);
      tree result;
      /* get rid of unneeded argument */
      TREE_CHAIN (parms) = NULL_TREE;
      result = build_method_call (instance, name, parms, basetype_path,
				  (LOOKUP_SPECULATIVELY|flags)
				  &~LOOKUP_COMPLAIN);
      /* If it finds a match, return it.  */
      if (result)
	return build_method_call (instance, name, parms, basetype_path, flags);
      /* If it doesn't work, two argument delete must work */
      TREE_CHAIN (parms) = save_last;
    }
  /* We already know whether it's needed or not for vec delete.  */
  else if (name == ansi_opname[(int) VEC_DELETE_EXPR]
	   && TYPE_LANG_SPECIFIC (TREE_TYPE (instance))
	   && ! TYPE_VEC_DELETE_TAKES_SIZE (TREE_TYPE (instance)))
    TREE_CHAIN (parms) = NULL_TREE;

  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      flags |= LOOKUP_DESTRUCTOR;
      name = TREE_OPERAND (name, 0);
      if (parms)
	error ("destructors take no parameters");
      basetype = TREE_TYPE (instance);
      if (TREE_CODE (basetype) == REFERENCE_TYPE)
	basetype = TREE_TYPE (basetype);
      if (! (name == basetype
	     || (IS_AGGR_TYPE (basetype)
		 && name == constructor_name (basetype))
	     || basetype == get_type_value (name)))
	{
	  cp_error ("destructor name `~%D' does not match type `%T' of expression",
		    name, basetype);
	  return cp_convert (void_type_node, instance);
	}

      if (! TYPE_HAS_DESTRUCTOR (complete_type (basetype)))
	return cp_convert (void_type_node, instance);
      instance = default_conversion (instance);
      instance_ptr = build_unary_op (ADDR_EXPR, instance, 0);
      return build_delete (build_pointer_type (basetype),
			   instance_ptr, integer_two_node,
			   LOOKUP_NORMAL|LOOKUP_DESTRUCTOR, 0);
    }

  if (flag_ansi_overloading)
    return build_new_method_call (instance, name, parms, basetype_path, flags);

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
      /* call to a constructor...  */
      else if (basetype_path)
	{
	  basetype = BINFO_TYPE (basetype_path);
	  if (name == TYPE_IDENTIFIER (basetype))
	    name = ctor_identifier;
	}
      else if (IDENTIFIER_HAS_TYPE_VALUE (name))
	{
	  basetype = IDENTIFIER_TYPE_VALUE (name);
	  name = ctor_identifier;
	}
      else
	{
	  tree typedef_name = lookup_name (name, 1);
	  if (typedef_name && TREE_CODE (typedef_name) == TYPE_DECL)
	    {
	      /* Canonicalize the typedef name.  */
	      basetype = TREE_TYPE (typedef_name);
	      name = ctor_identifier;
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
	  if ((flags & LOOKUP_COMPLAIN) && basetype != error_mark_node)
	    cp_error ("request for member `%D' in `%E', which is of non-aggregate type `%T'",
		      name, instance, basetype);

	  return error_mark_node;
	}
    }
  else if (instance == current_class_ref || instance == current_class_ptr)
    {
      /* When doing initialization, we side-effect the TREE_TYPE of
	 current_class_ref, hence we cannot set up BASETYPE from CURRENT_CLASS_TYPE.  */
      basetype = TREE_TYPE (current_class_ref);

      /* Anything manifestly `this' in constructors and destructors
	 has a known type, so virtual function tables are not needed.  */
      if (TYPE_VIRTUAL_P (basetype)
	  && !(flags & LOOKUP_NONVIRTUAL))
	need_vtbl = (dtor_label || ctor_label)
	  ? unneeded : maybe_needed;

      /* If `this' is a signature pointer and `name' is not a constructor,
	 we are calling a signature member function.  In that case, set the
	 `basetype' to the signature type and dereference the `optr' field.  */
      if (IS_SIGNATURE_POINTER (basetype)
	  && TYPE_IDENTIFIER (basetype) != name)
	{
	  basetype = SIGNATURE_TYPE (basetype);
	  instance_ptr = instance;
	  basetype_path = TYPE_BINFO (basetype);
	}
      else
	{
	  instance = current_class_ref;
	  instance_ptr = current_class_ptr;
	  basetype_path = TYPE_BINFO (current_class_type);
	}
      result = build_field_call (basetype_path, instance_ptr, name, parms);

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
      instance_ptr = build1 (ADDR_EXPR, build_pointer_type (basetype), instance);
    }
  else
    {
      /* The MAIN_VARIANT of the type that `instance_ptr' winds up being.  */
      tree inst_ptr_basetype;

      static_call_context
	= (TREE_CODE (instance) == INDIRECT_REF
	   && TREE_CODE (TREE_OPERAND (instance, 0)) == NOP_EXPR
	   && TREE_OPERAND (TREE_OPERAND (instance, 0), 0) == error_mark_node);

      if (TREE_CODE (instance) == OFFSET_REF)
	instance = resolve_offset_ref (instance);

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
	  if (! IS_AGGR_TYPE (basetype)
	      && ! (TYPE_LANG_SPECIFIC (basetype)
		    && (IS_SIGNATURE_POINTER (basetype)
			|| IS_SIGNATURE_REFERENCE (basetype))))
	    goto non_aggr_error;

	  /* If `instance' is a signature pointer/reference and `name' is
	     not a constructor, we are calling a signature member function.
	     In that case set the `basetype' to the signature type.  */
	  if ((IS_SIGNATURE_POINTER (basetype)
	       || IS_SIGNATURE_REFERENCE (basetype))
	      && TYPE_IDENTIFIER (basetype) != name)
	    basetype = SIGNATURE_TYPE (basetype);

	  basetype = complete_type (basetype);

	  if ((IS_SIGNATURE (basetype)
	       && (instance_ptr = instance))
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
		expand_aggr_init (temp, instance, 0, flags);
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
		instance = build_cplus_new (basetype, instance);
	      else
		{
		  instance = get_temp_name (basetype, 0);
		  TREE_ADDRESSABLE (instance) = 1;
		}
	      instance_ptr = build_unary_op (ADDR_EXPR, instance, 0);
	    }
	  /* @@ Should we call comp_target_types here?  */
	  if (IS_SIGNATURE (basetype))
	    inst_ptr_basetype = basetype;
	  else
	    inst_ptr_basetype = TREE_TYPE (TREE_TYPE (instance_ptr));
	  if (TYPE_MAIN_VARIANT (basetype) == TYPE_MAIN_VARIANT (inst_ptr_basetype))
	    basetype = inst_ptr_basetype;
	  else
	    {
	      instance_ptr = cp_convert (build_pointer_type (basetype), instance_ptr);
	      if (instance_ptr == error_mark_node)
		return error_mark_node;
	    }
	}

      /* After converting `instance_ptr' above, `inst_ptr_basetype' was
	 not updated, so we use `basetype' instead.  */
      if (basetype_path == NULL_TREE
	  && IS_SIGNATURE (basetype))
	basetype_path = TYPE_BINFO (basetype);
      else if (basetype_path == NULL_TREE
	       || (BINFO_TYPE (basetype_path)
		   != TYPE_MAIN_VARIANT (inst_ptr_basetype)))
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

  if (save_name == ctor_identifier)
    save_name = TYPE_IDENTIFIER (basetype);

  if (TYPE_SIZE (complete_type (basetype)) == 0)
    {
      /* This is worth complaining about, I think.  */
      cp_error ("cannot lookup method in incomplete type `%T'", basetype);
      return error_mark_node;
    }

  save_basetype = TYPE_MAIN_VARIANT (basetype);

  parmtypes = default_parm_conversions (parms, &last);
  if (parmtypes == error_mark_node)
    {
      return error_mark_node;
    }

  if (instance && IS_SIGNATURE (basetype))
    {
      /* @@ Should this be the constp/volatilep flags for the optr field
	 of the signature pointer?  */
      constp = TYPE_READONLY (basetype);
      volatilep = TYPE_VOLATILE (basetype);
      parms = expr_tree_cons (NULL_TREE, instance_ptr, parms);
    }
  else if (instance)
    {
      /* TREE_READONLY (instance) fails for references.  */
      constp = TYPE_READONLY (TREE_TYPE (TREE_TYPE (instance_ptr)));
      volatilep = TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (instance_ptr)));
      parms = expr_tree_cons (NULL_TREE, instance_ptr, parms);
    }
  else
    {
      /* Raw constructors are always in charge.  */
      if (TYPE_USES_VIRTUAL_BASECLASSES (basetype)
	  && ! (flags & LOOKUP_HAS_IN_CHARGE))
	{
	  flags |= LOOKUP_HAS_IN_CHARGE;
	  parms = expr_tree_cons (NULL_TREE, integer_one_node, parms);
	  parmtypes = scratch_tree_cons (NULL_TREE, integer_type_node, parmtypes);
	}

      constp = 0;
      volatilep = 0;
      instance_ptr = build_int_2 (0, 0);
      TREE_TYPE (instance_ptr) = build_pointer_type (basetype);
      parms = expr_tree_cons (NULL_TREE, instance_ptr, parms);
    }

  parmtypes = scratch_tree_cons (NULL_TREE, TREE_TYPE (instance_ptr), parmtypes);

  if (last == NULL_TREE)
    last = parmtypes;

  /* Look up function name in the structure type definition.  */

  /* FIXME Axe most of this now?  */
  if ((IDENTIFIER_HAS_TYPE_VALUE (name)
       && ! IDENTIFIER_OPNAME_P (name)
       && IS_AGGR_TYPE (IDENTIFIER_TYPE_VALUE (name)))
      || name == constructor_name (basetype)
      || name == ctor_identifier)
    {
      tree tmp = NULL_TREE;
      if (IDENTIFIER_TYPE_VALUE (name) == basetype
	  || name == constructor_name (basetype)
	  || name == ctor_identifier)
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
	      tmplist = expr_tree_cons (NULL_TREE, integer_zero_node,
				   TREE_CHAIN (parms));
	      TREE_CHAIN (parms) = tmplist;
	      tmplist = scratch_tree_cons (NULL_TREE, integer_type_node, TREE_CHAIN (parmtypes));
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

  for (pass = 0; pass < 2; pass++)
    {
      struct candidate *candidates;
      struct candidate *cp;
      int len;
      unsigned best = 1;

      baselink = result;

      if (pass > 0)
	{
	  candidates
	    = (struct candidate *) alloca ((ever_seen+1)
					   * sizeof (struct candidate));
	  bzero ((char *) candidates, (ever_seen + 1) * sizeof (struct candidate));
	  cp = candidates;
	  len = list_length (parms);
	  ever_seen = 0;

	  /* First see if a global function has a shot at it.  */
	  if (flags & LOOKUP_GLOBAL)
	    {
	      tree friend_parms;
	      tree parm = instance_ptr;

	      if (TREE_CODE (TREE_TYPE (parm)) == REFERENCE_TYPE)
		parm = convert_from_reference (parm);
	      else if (TREE_CODE (TREE_TYPE (parm)) == POINTER_TYPE)
		parm = build_indirect_ref (parm, "friendifying parms (compiler error)");
	      else
		my_friendly_abort (167);

	      friend_parms = expr_tree_cons (NULL_TREE, parm, TREE_CHAIN (parms));

	      cp->h_len = len;
	      cp->harshness = (struct harshness_code *)
		alloca ((len + 1) * sizeof (struct harshness_code));

	      result = build_overload_call_real (name, friend_parms, 0, cp, 1);

	      /* If it turns out to be the one we were actually looking for
		 (it was probably a friend function), the return the
		 good result.  */
	      if (TREE_CODE (result) == CALL_EXPR)
		return result;

	      while ((cp->h.code & EVIL_CODE) == 0)
		{
		  /* non-standard uses: set the field to 0 to indicate
		     we are using a non-member function.  */
		  cp->u.field = 0;
		  if (cp->harshness[len].distance == 0
		      && cp->h.code < best)
		    best = cp->h.code;
		  cp += 1;
		}
	    }
	}

      if (baselink)
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

	  for (; function; function = DECL_CHAIN (function))
	    {
#ifdef GATHER_STATISTICS
	      n_inner_fields_searched++;
#endif
	      ever_seen++;
	      if (pass > 0)
		found_fns = scratch_tree_cons (NULL_TREE, function, found_fns);

	      /* Not looking for friends here.  */
	      if (TREE_CODE (TREE_TYPE (function)) == FUNCTION_TYPE
		  && ! DECL_STATIC_FUNCTION_P (function))
		continue;

	      if (pass > 0)
		{
		  tree these_parms = parms;

#ifdef GATHER_STATISTICS
		  n_inner_fields_searched++;
#endif
		  cp->h_len = len;
		  cp->harshness = (struct harshness_code *)
		    alloca ((len + 1) * sizeof (struct harshness_code));

		  if (DECL_STATIC_FUNCTION_P (function))
		    these_parms = TREE_CHAIN (these_parms);
		  compute_conversion_costs (function, these_parms, cp, len);

		  if ((cp->h.code & EVIL_CODE) == 0)
		    {
		      cp->u.field = function;
		      cp->function = function;
		      cp->basetypes = basetype_path;

		      /* Don't allow non-converting constructors to convert.  */
		      if (flags & LOOKUP_ONLYCONVERTING
			  && DECL_LANG_SPECIFIC (function)
			  && DECL_NONCONVERTING_P (function))
			continue;

		      /* No "two-level" conversions.  */
		      if (flags & LOOKUP_NO_CONVERSION
			  && (cp->h.code & USER_CODE))
			continue;

		      cp++;
		    }
		}
	    }
	}

      if (pass == 0)
	{
	  tree igv = lookup_name_nonclass (name);

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
			  save_name, parmtypes);
	      else
		cp_error ("no member function `%T::%D(%A)' defined",
			  save_basetype, save_name, TREE_CHAIN (parmtypes));
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
	      extern int warn_synth;
	      TREE_VALUE (parms) = instance_ptr;
	      cp = ideal_candidate (candidates, n_candidates, len);
	      if (cp == (struct candidate *)0)
		{
		  if (flags & LOOKUP_COMPLAIN)
		    {
		      TREE_CHAIN (last) = void_list_node;
		      cp_error ("call of overloaded %s `%D(%A)' is ambiguous",
				name_kind, save_name, TREE_CHAIN (parmtypes));
		      print_n_candidates (candidates, n_candidates);
		    }
		  return error_mark_node;
		}
	      if (cp->h.code & EVIL_CODE)
		return error_mark_node;
	      if (warn_synth
		  && DECL_NAME (cp->function) == ansi_opname[MODIFY_EXPR]
		  && DECL_ARTIFICIAL (cp->function)
		  && n_candidates == 2)
		{
		  cp_warning ("using synthesized `%#D' for copy assignment",
			      cp->function);
		  cp_warning_at ("  where cfront would use `%#D'",
				 candidates->function);
		}
	    }
	  else if (cp[-1].h.code & EVIL_CODE)
	    {
	      if (flags & LOOKUP_COMPLAIN)
		cp_error ("ambiguous type conversion requested for %s `%D'",
			  name_kind, save_name);
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

      if (flags & (LOOKUP_COMPLAIN|LOOKUP_SPECULATIVELY))
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
		  cp_error ("no matching function for call to `%T::%D (%A)%V'",
			    TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (instance_ptr))),
			    save_name, TREE_CHAIN (parmtypes),
			    TREE_TYPE (TREE_TYPE (instance_ptr)));
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
	      cp_error ("%T has no method named %D", save_basetype, save_name);
	      return error_mark_node;
	    }
	  return NULL_TREE;
	}
      continue;

    found_and_maybe_warn:
      if ((cp->harshness[0].code & CONST_CODE)
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

  if (access == access_private_node)
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
  else if (access == access_protected_node)
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

  if (DECL_ABSTRACT_VIRTUAL_P (function)
      && instance == current_class_ref
      && DECL_CONSTRUCTOR_P (current_function_decl)
      && ! (flags & LOOKUP_NONVIRTUAL)
      && value_member (function, get_abstract_virtuals (basetype)))
    cp_error ("abstract virtual `%#D' called from constructor", function);

  if (IS_SIGNATURE (basetype))
    {
      if (static_call_context)
	{
	  cp_error ("cannot call signature member function `%T::%D' without signature pointer/reference",
		    basetype, save_name);
	  return error_mark_node;
	}
      return build_signature_method_call (function, parms);
    }

  function = DECL_MAIN_VARIANT (function);
  mark_used (function);

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
      /* Let's be nasty to the user now, and give reasonable
	 error messages.  */
      instance_ptr = current_class_ptr;
      if (instance_ptr)
	{
	  if (basetype != current_class_type)
	    {
	      if (basetype == error_mark_node)
		return error_mark_node;
	      else 
                {
		  if (orig_basetype != NULL_TREE)
		    error_not_base_type (orig_basetype, current_class_type);
		  else
		    error_not_base_type (function, current_class_type);
                  return error_mark_node;
                }
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

  if (TYPE_SIZE (complete_type (value_type)) == 0)
    {
      if (flags & LOOKUP_COMPLAIN)
	incomplete_type_error (0, value_type);
      return error_mark_node;
    }

  if (DECL_STATIC_FUNCTION_P (function))
    parms = convert_arguments (NULL_TREE, TYPE_ARG_TYPES (fntype),
			       TREE_CHAIN (parms), function, LOOKUP_NORMAL);
  else if (need_vtbl == unneeded)
    {
      int sub_flags = DECL_CONSTRUCTOR_P (function) ? flags : LOOKUP_NORMAL;
      basetype = TREE_TYPE (instance);
      if (TYPE_METHOD_BASETYPE (TREE_TYPE (function))
	  != TYPE_MAIN_VARIANT (basetype))
	{
	  basetype = DECL_CLASS_CONTEXT (function);
	  instance_ptr = convert_pointer_to (basetype, instance_ptr);
	  instance = build_indirect_ref (instance_ptr, NULL_PTR);
	}
      parms = expr_tree_cons (NULL_TREE, instance_ptr,
			 convert_arguments (NULL_TREE, TREE_CHAIN (TYPE_ARG_TYPES (fntype)), TREE_CHAIN (parms), function, sub_flags));
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
	  else if (instance_ptr == error_mark_node)
	    return instance_ptr;
	  else if (instance == NULL_TREE
		   || TREE_CODE (instance) != INDIRECT_REF
		   || TREE_OPERAND (instance, 0) != instance_ptr)
	    instance = build_indirect_ref (instance_ptr, NULL_PTR);
	}
      parms = expr_tree_cons (NULL_TREE, instance_ptr,
			 convert_arguments (NULL_TREE, TREE_CHAIN (TYPE_ARG_TYPES (fntype)), TREE_CHAIN (parms), function, LOOKUP_NORMAL));
    }

  if (parms == error_mark_node
      || (parms && TREE_CHAIN (parms) == error_mark_node))
    return error_mark_node;

  if (need_vtbl == needed)
    {
      function = build_vfn_ref (&TREE_VALUE (parms), instance,
				DECL_VINDEX (function));
      TREE_TYPE (function) = build_pointer_type (fntype);
    }

  if (TREE_CODE (function) == FUNCTION_DECL)
    GNU_xref_call (current_function_decl,
		   IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (function)));

  result = build_call (function, value_type, parms);
  if (IS_AGGR_TYPE (value_type))
    result = build_cplus_new (value_type, result);
  result = convert_from_reference (result);
  return result;
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
build_overload_call_real (fnname, parms, flags, final_cp, require_complete)
     tree fnname, parms;
     int flags;
     struct candidate *final_cp;
     int require_complete;
{
  /* must check for overloading here */
  tree functions, function;
  tree parmtypes, last;
  register tree outer;
  int length;
  int parmlength = list_length (parms);

  struct candidate *candidates, *cp;

  if (final_cp)
    {
      final_cp[0].h.code = 0;
      final_cp[0].h.distance = 0;
      final_cp[0].function = 0;
      /* end marker.  */
      final_cp[1].h.code = EVIL_CODE;
    }

  parmtypes = default_parm_conversions (parms, &last);
  if (parmtypes == error_mark_node)
    {
      if (final_cp)
	final_cp->h.code = EVIL_CODE;
      return error_mark_node;
    }

  if (last)
    TREE_CHAIN (last) = void_list_node;
  else
    parmtypes = void_list_node;

  if (is_overloaded_fn (fnname))
    {
      functions = fnname;
      if (TREE_CODE (fnname) == TREE_LIST)
	fnname = TREE_PURPOSE (functions);
      else if (TREE_CODE (fnname) == FUNCTION_DECL)
	fnname = DECL_NAME (functions);
    }
  else 
    functions = lookup_name_nonclass (fnname);

  if (functions == NULL_TREE)
    {
      if (flags & LOOKUP_SPECULATIVELY)
	return NULL_TREE;
      if (flags & LOOKUP_COMPLAIN)
	error ("only member functions apply");
      if (final_cp)
	final_cp->h.code = EVIL_CODE;
      return error_mark_node;
    }

  if (TREE_CODE (functions) == FUNCTION_DECL && ! IDENTIFIER_OPNAME_P (fnname))
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
	final_cp->h.code = EVIL_CODE;
      return error_mark_node;
    }

  length = count_functions (functions);
  
  if (final_cp)
    candidates = final_cp;
  else
    {
      candidates
	= (struct candidate *)alloca ((length+1) * sizeof (struct candidate));
      bzero ((char *) candidates, (length + 1) * sizeof (struct candidate));
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
	  int ntparms = DECL_NTPARMS (function);
	  tree targs = make_scratch_vec (ntparms);
	  int i;

	  i = type_unification (DECL_INNERMOST_TEMPLATE_PARMS (function),
				&TREE_VEC_ELT (targs, 0),
				TYPE_ARG_TYPES (TREE_TYPE (function)),
				parms, NULL_TREE, &template_cost, 0, 0);
	  if (i == 0)
	    {
	      function = instantiate_template (function, targs);
	      if (function == error_mark_node)
		return function;
	    }
	}

      if (TREE_CODE (function) == TEMPLATE_DECL)
	{
	  /* Unconverted template -- failed match.  */
	  cp->function = function;
	  cp->u.bad_arg = -4;
	  cp->h.code = EVIL_CODE;
	}
      else
	{
	  struct candidate *cp2;

	  /* Check that this decl is not the same as a function that's in
	     the list due to some template instantiation.  */
	  cp2 = candidates;
	  while (cp2 != cp)
	    if (cp2->function == function)
	      break;
	    else
	      cp2 += 1;
	  if (cp2->function == function)
	    continue;

	  function = DECL_MAIN_VARIANT (function);

	  /* Can't use alloca here, since result might be
	     passed to calling function.  */
	  cp->h_len = parmlength;
	  cp->harshness = (struct harshness_code *)
	    scratchalloc ((parmlength + 1) * sizeof (struct harshness_code));

	  compute_conversion_costs (function, parms, cp, parmlength);

	  /* Make sure this is clear as well.  */
	  cp->h.int_penalty += template_cost;

	  if ((cp[0].h.code & EVIL_CODE) == 0)
	    {
	      cp[1].h.code = EVIL_CODE;
	      cp++;
	    }
	}
    }

  if (cp - candidates)
    {
      tree rval = error_mark_node;

      /* Leave marker.  */
      cp[0].h.code = EVIL_CODE;
      if (cp - candidates > 1)
	{
	  struct candidate *best_cp
	    = ideal_candidate (candidates, cp - candidates, parmlength);
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
	  if (cp->h.code & EVIL_CODE)
	    {
	      if (flags & LOOKUP_COMPLAIN)
		error ("type conversion ambiguous");
	    }
	  else
	    rval = cp->function;
	}

      if (final_cp)
	return rval;

      return build_function_call_real (rval, parms, require_complete, flags);
    }

  if (flags & LOOKUP_SPECULATIVELY)
    return NULL_TREE;
  
  if (flags & LOOKUP_COMPLAIN)
    report_type_mismatch (cp, parms, "function");

  return error_mark_node;
}

/* This requires a complete type on the result of the call.  */

tree
build_overload_call (fnname, parms, flags)
     tree fnname, parms;
     int flags;
{
  return build_overload_call_real (fnname, parms, flags, (struct candidate *)0, 1);
}

/* New overloading code.  */

struct z_candidate {
  tree fn;
  tree convs;
  tree second_conv;
  int viable;
  tree basetype_path;
  tree template;
  struct z_candidate *next;
};

#define IDENTITY_RANK 0
#define EXACT_RANK 1
#define PROMO_RANK 2
#define STD_RANK 3
#define PBOOL_RANK 4
#define USER_RANK 5
#define ELLIPSIS_RANK 6
#define BAD_RANK 7

#define ICS_RANK(NODE)				\
  (ICS_BAD_FLAG (NODE) ? BAD_RANK   \
   : ICS_ELLIPSIS_FLAG (NODE) ? ELLIPSIS_RANK	\
   : ICS_USER_FLAG (NODE) ? USER_RANK		\
   : ICS_STD_RANK (NODE))

#define ICS_STD_RANK(NODE) TREE_COMPLEXITY (NODE)

#define ICS_USER_FLAG(NODE) TREE_LANG_FLAG_0 (NODE)
#define ICS_ELLIPSIS_FLAG(NODE) TREE_LANG_FLAG_1 (NODE)
#define ICS_THIS_FLAG(NODE) TREE_LANG_FLAG_2 (NODE)
#define ICS_BAD_FLAG(NODE) TREE_LANG_FLAG_3 (NODE)

#define USER_CONV_FN(NODE) TREE_OPERAND (NODE, 1)

int
null_ptr_cst_p (t)
     tree t;
{
  if (t == null_node
      || integer_zerop (t) && TREE_CODE (TREE_TYPE (t)) == INTEGER_TYPE)
    return 1;
  return 0;
}

static tree
build_conv (code, type, from)
     enum tree_code code;
     tree type, from;
{
  tree t = build1 (code, type, from);
  int rank = ICS_STD_RANK (from);
  switch (code)
    {
    case PTR_CONV:
    case PMEM_CONV:
    case BASE_CONV:
    case STD_CONV:
      if (rank < STD_RANK)
	rank = STD_RANK;
      break;

    case QUAL_CONV:
      if (rank < EXACT_RANK)
	rank = EXACT_RANK;

    default:
      break;
    }
  ICS_STD_RANK (t) = rank;
  ICS_USER_FLAG (t) = ICS_USER_FLAG (from);
  ICS_BAD_FLAG (t) = ICS_BAD_FLAG (from);
  return t;
}

static tree
non_reference (t)
     tree t;
{
  if (TREE_CODE (t) == REFERENCE_TYPE)
    t = TREE_TYPE (t);
  return t;
}

static tree
strip_top_quals (t)
     tree t;
{
  if (TREE_CODE (t) == ARRAY_TYPE)
    return t;
  return TYPE_MAIN_VARIANT (t);
}

/* Returns the standard conversion path (see [conv]) from type FROM to type
   TO, if any.  For proper handling of null pointer constants, you must
   also pass the expression EXPR to convert from.  */

static tree
standard_conversion (to, from, expr)
     tree to, from, expr;
{
  enum tree_code fcode, tcode;
  tree conv;
  int fromref = 0;

  if (TREE_CODE (to) == REFERENCE_TYPE)
    to = TREE_TYPE (to);
  if (TREE_CODE (from) == REFERENCE_TYPE)
    {
      fromref = 1;
      from = TREE_TYPE (from);
    }
  to = strip_top_quals (to);
  from = strip_top_quals (from);

  fcode = TREE_CODE (from);
  tcode = TREE_CODE (to);

  conv = build1 (IDENTITY_CONV, from, expr);

  if (fcode == FUNCTION_TYPE)
    {
      from = build_pointer_type (from);
      fcode = TREE_CODE (from);
      conv = build_conv (LVALUE_CONV, from, conv);
    }
  else if (fcode == ARRAY_TYPE)
    {
      from = build_pointer_type (TREE_TYPE (from));
      fcode = TREE_CODE (from);
      conv = build_conv (LVALUE_CONV, from, conv);
    }
  else if (fromref || (expr && real_lvalue_p (expr)))
    conv = build_conv (RVALUE_CONV, from, conv);

  if (from == to)
    return conv;

  if ((tcode == POINTER_TYPE || TYPE_PTRMEMFUNC_P (to))
      && expr && null_ptr_cst_p (expr))
    {
      conv = build_conv (STD_CONV, to, conv);
    }
  else if (tcode == POINTER_TYPE && fcode == POINTER_TYPE)
    {
      enum tree_code ufcode = TREE_CODE (TREE_TYPE (from));
      enum tree_code utcode = TREE_CODE (TREE_TYPE (to));
      tree nconv = NULL_TREE;

      if (comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (from)),
		     TYPE_MAIN_VARIANT (TREE_TYPE (to)), 1))
	nconv = conv;
      else if (utcode == VOID_TYPE && ufcode != OFFSET_TYPE
	       && ufcode != FUNCTION_TYPE)
	{
	  from = build_pointer_type
	    (cp_build_type_variant (void_type_node,
				    TYPE_READONLY (TREE_TYPE (from)),
				    TYPE_VOLATILE (TREE_TYPE (from))));
	  nconv = build_conv (PTR_CONV, from, conv);
	}
      else if (ufcode == OFFSET_TYPE && utcode == OFFSET_TYPE)
	{
	  tree fbase = TYPE_OFFSET_BASETYPE (TREE_TYPE (from));
	  tree tbase = TYPE_OFFSET_BASETYPE (TREE_TYPE (to));

	  if (DERIVED_FROM_P (fbase, tbase)
	      && (comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (from))),
			     TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (to))),
			     1)))
	    {
	      from = build_offset_type (tbase, TREE_TYPE (TREE_TYPE (from)));
	      from = build_pointer_type (from);
	      nconv = build_conv (PMEM_CONV, from, conv);
	    }
	}
      else if (IS_AGGR_TYPE (TREE_TYPE (from))
	       && IS_AGGR_TYPE (TREE_TYPE (to)))
	{
	  if (DERIVED_FROM_P (TREE_TYPE (to), TREE_TYPE (from)))
	    {
	      from = cp_build_type_variant (TREE_TYPE (to),
					    TYPE_READONLY (TREE_TYPE (from)),
					    TYPE_VOLATILE (TREE_TYPE (from)));
	      from = build_pointer_type (from);
	      nconv = build_conv (PTR_CONV, from, conv);
	    }
	}

      if (nconv && comptypes (from, to, 1))
	conv = nconv;
      else if (nconv && comp_ptr_ttypes (TREE_TYPE (to), TREE_TYPE (from)))
	conv = build_conv (QUAL_CONV, to, nconv);
      else if (ptr_reasonably_similar (TREE_TYPE (to), TREE_TYPE (from)))
	{
	  conv = build_conv (PTR_CONV, to, conv);
	  ICS_BAD_FLAG (conv) = 1;
	}
      else
	return 0;

      from = to;
    }
  else if (TYPE_PTRMEMFUNC_P (to) && TYPE_PTRMEMFUNC_P (from))
    {
      tree fromfn = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (from));
      tree tofn = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (to));
      tree fbase = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (fromfn)));
      tree tbase = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (tofn)));

      if (! DERIVED_FROM_P (fbase, tbase)
	  || ! comptypes (TREE_TYPE (fromfn), TREE_TYPE (tofn), 1)
	  || ! compparms (TREE_CHAIN (TYPE_ARG_TYPES (fromfn)),
			  TREE_CHAIN (TYPE_ARG_TYPES (tofn)), 1)
	  || TYPE_READONLY (fbase) != TYPE_READONLY (tbase)
	  || TYPE_VOLATILE (fbase) != TYPE_VOLATILE (tbase))
	return 0;

      from = cp_build_type_variant (tbase, TYPE_READONLY (fbase),
				    TYPE_VOLATILE (fbase));
      from = build_cplus_method_type (from, TREE_TYPE (fromfn),
				      TREE_CHAIN (TYPE_ARG_TYPES (fromfn)));
      from = build_ptrmemfunc_type (build_pointer_type (from));
      conv = build_conv (PMEM_CONV, from, conv);
    }
  else if (tcode == BOOLEAN_TYPE)
    {
      if (! (INTEGRAL_CODE_P (fcode) || fcode == REAL_TYPE
	     || fcode == POINTER_TYPE || TYPE_PTRMEMFUNC_P (from)))
	return 0;

      conv = build_conv (STD_CONV, to, conv);
      if (fcode == POINTER_TYPE || TYPE_PTRMEMFUNC_P (from)
	  && ICS_STD_RANK (conv) < PBOOL_RANK)
	ICS_STD_RANK (conv) = PBOOL_RANK;
    }
  /* We don't check for ENUMERAL_TYPE here because there are no standard
     conversions to enum type.  */
  else if (tcode == INTEGER_TYPE || tcode == BOOLEAN_TYPE
	   || tcode == REAL_TYPE)
    {
      if (! (INTEGRAL_CODE_P (fcode) || fcode == REAL_TYPE))
	return 0;
      conv = build_conv (STD_CONV, to, conv);

      /* Give this a better rank if it's a promotion.  */
      if (to == type_promotes_to (from)
	  && ICS_STD_RANK (TREE_OPERAND (conv, 0)) <= PROMO_RANK)
	ICS_STD_RANK (conv) = PROMO_RANK;
    }
  else if (IS_AGGR_TYPE (to) && IS_AGGR_TYPE (from)
	   && DERIVED_FROM_P (to, from))
    conv = build_conv (BASE_CONV, to, conv);
  else
    return 0;

  return conv;
}

/* Returns the conversion path from type FROM to reference type TO for
   purposes of reference binding.  For lvalue binding, either pass a
   reference type to FROM or an lvalue expression to EXPR.

   Currently does not distinguish in the generated trees between binding to
   an lvalue and a temporary.  Should it?  */

static tree
reference_binding (rto, rfrom, expr, flags)
     tree rto, rfrom, expr;
     int flags;
{
  tree conv;
  int lvalue = 1;
  tree to = TREE_TYPE (rto);
  tree from = rfrom;
  int related;

  if (TREE_CODE (from) == REFERENCE_TYPE)
    from = TREE_TYPE (from);
  else if (! expr || ! real_lvalue_p (expr))
    lvalue = 0;

  related = (TYPE_MAIN_VARIANT (to) == TYPE_MAIN_VARIANT (from)
	     || (IS_AGGR_TYPE (to) && IS_AGGR_TYPE (from)
		 && DERIVED_FROM_P (to, from)));

  if (lvalue && related
      && TYPE_READONLY (to) >= TYPE_READONLY (from)
      && TYPE_VOLATILE (to) >= TYPE_VOLATILE (from))
    {
      conv = build1 (IDENTITY_CONV, from, expr);

      if (TYPE_MAIN_VARIANT (to) == TYPE_MAIN_VARIANT (from))
	conv = build_conv (REF_BIND, rto, conv);
      else
	{
	  conv = build_conv (REF_BIND, rto, conv);
	  ICS_STD_RANK (conv) = STD_RANK;
	}
    }
  else
    conv = NULL_TREE;

  if (! conv)
    {
      conv = standard_conversion (to, rfrom, expr);
      if (conv)
	{
	  conv = build_conv (REF_BIND, rto, conv);

	  /* Bind directly to a base subobject of a class rvalue.  Do it
             after building the conversion for proper handling of ICS_RANK.  */
	  if (TREE_CODE (TREE_OPERAND (conv, 0)) == BASE_CONV)
	    TREE_OPERAND (conv, 0) = TREE_OPERAND (TREE_OPERAND (conv, 0), 0);
	}
      if (conv
	  && ((! (TYPE_READONLY (to) && ! TYPE_VOLATILE (to)
		  && (flags & LOOKUP_NO_TEMP_BIND) == 0))
	      /* If T1 is reference-related to T2, cv1 must be the same
		 cv-qualification as, or greater cv-qualification than,
		 cv2; otherwise, the program is ill-formed.  */
	      || (related
		  && (TYPE_READONLY (to) < TYPE_READONLY (from)
		      || TYPE_VOLATILE (to) < TYPE_VOLATILE (from)))))
	ICS_BAD_FLAG (conv) = 1;
    }

  return conv;
}

/* Returns the implicit conversion sequence (see [over.ics]) from type FROM
   to type TO.  The optional expression EXPR may affect the conversion.
   FLAGS are the usual overloading flags.  Only LOOKUP_NO_CONVERSION is
   significant.  */

static tree
implicit_conversion (to, from, expr, flags)
     tree to, from, expr;
     int flags;
{
  tree conv;
  struct z_candidate *cand;

  if (expr && type_unknown_p (expr))
    {
      expr = instantiate_type (to, expr, 0);
      if (expr == error_mark_node)
	return 0;
      from = TREE_TYPE (expr);
    }

  if (TREE_CODE (to) == REFERENCE_TYPE)
    conv = reference_binding (to, from, expr, flags);
  else
    conv = standard_conversion (to, from, expr);

  if (conv)
    ;
  else if ((IS_AGGR_TYPE (non_reference (from))
	    || IS_AGGR_TYPE (non_reference (to)))
	   && (flags & LOOKUP_NO_CONVERSION) == 0)
    {
      cand = build_user_type_conversion_1
	(to, expr, LOOKUP_ONLYCONVERTING);
      if (cand)
	conv = cand->second_conv;
      if ((! conv || ICS_BAD_FLAG (conv))
	  && TREE_CODE (to) == REFERENCE_TYPE
	  && (flags & LOOKUP_NO_TEMP_BIND) == 0)
	{
	  cand = build_user_type_conversion_1
	    (TYPE_MAIN_VARIANT (TREE_TYPE (to)), expr, LOOKUP_ONLYCONVERTING);
	  if (cand)
	    {
	      if (! TYPE_READONLY (TREE_TYPE (to))
		  || TYPE_VOLATILE (TREE_TYPE (to)))
		ICS_BAD_FLAG (cand->second_conv) = 1;
	      if (!conv || (ICS_BAD_FLAG (conv)
			    > ICS_BAD_FLAG (cand->second_conv)))
		conv = build_conv (REF_BIND, to, cand->second_conv);
	    }
	}
    }

  return conv;
}

/* Create an overload candidate for the function or method FN called with
   the argument list ARGLIST and add it to CANDIDATES.  FLAGS is passed on
   to implicit_conversion.  */

static struct z_candidate *
add_function_candidate (candidates, fn, arglist, flags)
     struct z_candidate *candidates;
     tree fn, arglist;
     int flags;
{
  tree parmlist = TYPE_ARG_TYPES (TREE_TYPE (fn));
  int i, len;
  tree convs;
  tree parmnode = parmlist;
  tree argnode = arglist;
  int viable = 1;
  struct z_candidate *cand;

  /* The `this' and `in_chrg' arguments to constructors are not considered
     in overload resolution.  */
  if (DECL_CONSTRUCTOR_P (fn))
    {
      parmnode = TREE_CHAIN (parmnode);
      argnode = TREE_CHAIN (argnode);
      if (TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (fn)))
	{
	  parmnode = TREE_CHAIN (parmnode);
	  argnode = TREE_CHAIN (argnode);
	}
    }

  len = list_length (argnode);
  convs = make_scratch_vec (len);

  for (i = 0; i < len; ++i)
    {
      tree arg = TREE_VALUE (argnode);
      tree argtype = TREE_TYPE (arg);
      tree t;

      argtype = cp_build_type_variant
	(argtype, TREE_READONLY (arg), TREE_THIS_VOLATILE (arg));

      if (parmnode == void_list_node)
	break;
      else if (parmnode)
	t = implicit_conversion (TREE_VALUE (parmnode), argtype, arg, flags);
      else
	{
	  t = build1 (IDENTITY_CONV, argtype, arg);
	  ICS_ELLIPSIS_FLAG (t) = 1;
	}

      if (i == 0 && t && TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE
	  && ! DECL_CONSTRUCTOR_P (fn))
	ICS_THIS_FLAG (t) = 1;

      TREE_VEC_ELT (convs, i) = t;
      if (! t)
	break;

      if (ICS_BAD_FLAG (t))
	viable = -1;

      if (parmnode)
	parmnode = TREE_CHAIN (parmnode);
      argnode = TREE_CHAIN (argnode);
    }

  if (i < len)
    viable = 0;

  /* Make sure there are default args for the rest of the parms.  */
  for (; parmnode && parmnode != void_list_node;
       parmnode = TREE_CHAIN (parmnode))
    if (! TREE_PURPOSE (parmnode))
      {
	viable = 0;
	break;
      }

  cand = (struct z_candidate *) scratchalloc (sizeof (struct z_candidate));

  cand->fn = fn;
  cand->convs = convs;
  cand->second_conv = NULL_TREE;
  cand->viable = viable;
  cand->basetype_path = NULL_TREE;
  cand->template = NULL_TREE;
  cand->next = candidates;

  return cand;
}

/* Create an overload candidate for the conversion function FN which will
   be invoked for expression OBJ, producing a pointer-to-function which
   will in turn be called with the argument list ARGLIST, and add it to
   CANDIDATES.  FLAGS is passed on to implicit_conversion.  */

static struct z_candidate *
add_conv_candidate (candidates, fn, obj, arglist)
     struct z_candidate *candidates;
     tree fn, obj, arglist;
{
  tree totype = TREE_TYPE (TREE_TYPE (fn));
  tree parmlist = TYPE_ARG_TYPES (TREE_TYPE (totype));
  int i, len = list_length (arglist) + 1;
  tree convs = make_scratch_vec (len);
  tree parmnode = parmlist;
  tree argnode = arglist;
  int viable = 1;
  struct z_candidate *cand;
  int flags = LOOKUP_NORMAL;

  for (i = 0; i < len; ++i)
    {
      tree arg = i == 0 ? obj : TREE_VALUE (argnode);
      tree argtype = lvalue_type (arg);
      tree t;

      if (i == 0)
	t = implicit_conversion (totype, argtype, arg, flags);
      else if (parmnode == void_list_node)
	break;
      else if (parmnode)
	t = implicit_conversion (TREE_VALUE (parmnode), argtype, arg, flags);
      else
	{
	  t = build1 (IDENTITY_CONV, argtype, arg);
	  ICS_ELLIPSIS_FLAG (t) = 1;
	}

      TREE_VEC_ELT (convs, i) = t;
      if (! t)
	break;

      if (ICS_BAD_FLAG (t))
	viable = -1;

      if (i == 0)
	continue;

      if (parmnode)
	parmnode = TREE_CHAIN (parmnode);
      argnode = TREE_CHAIN (argnode);
    }

  if (i < len)
    viable = 0;

  for (; parmnode && parmnode != void_list_node;
       parmnode = TREE_CHAIN (parmnode))
    if (! TREE_PURPOSE (parmnode))
      {
	viable = 0;
	break;
      }

  cand = (struct z_candidate *) scratchalloc (sizeof (struct z_candidate));

  cand->fn = fn;
  cand->convs = convs;
  cand->second_conv = NULL_TREE;
  cand->viable = viable;
  cand->basetype_path = NULL_TREE;
  cand->template = NULL_TREE;
  cand->next = candidates;

  return cand;
}

static struct z_candidate *
build_builtin_candidate (candidates, fnname, type1, type2,
			 args, argtypes, flags)
     struct z_candidate *candidates;
     tree fnname, type1, type2, *args, *argtypes;
     int flags;

{
  tree t, convs;
  int viable = 1, i;
  struct z_candidate *cand;
  tree types[2];

  types[0] = type1;
  types[1] = type2;

  convs = make_scratch_vec (args[2] ? 3 : (args[1] ? 2 : 1));

  for (i = 0; i < 2; ++i)
    {
      if (! args[i])
	break;

      t = implicit_conversion (types[i], argtypes[i], args[i], flags);
      if (! t)
	{
	  viable = 0;
	  /* We need something for printing the candidate.  */
	  t = build1 (IDENTITY_CONV, types[i], NULL_TREE);
	}
      else if (ICS_BAD_FLAG (t))
	viable = 0;
      TREE_VEC_ELT (convs, i) = t;
    }

  /* For COND_EXPR we rearranged the arguments; undo that now.  */
  if (args[2])
    {
      TREE_VEC_ELT (convs, 2) = TREE_VEC_ELT (convs, 1);
      TREE_VEC_ELT (convs, 1) = TREE_VEC_ELT (convs, 0);
      t = implicit_conversion (boolean_type_node, argtypes[2], args[2], flags);
      if (t)
	TREE_VEC_ELT (convs, 0) = t;
      else
	viable = 0;
    }      

  cand = (struct z_candidate *) scratchalloc (sizeof (struct z_candidate));

  cand->fn = fnname;
  cand->convs = convs;
  cand->second_conv = NULL_TREE;
  cand->viable = viable;
  cand->basetype_path = NULL_TREE;
  cand->template = NULL_TREE;
  cand->next = candidates;

  return cand;
}

static int
is_complete (t)
     tree t;
{
  return TYPE_SIZE (complete_type (t)) != NULL_TREE;
}

/* Create any builtin operator overload candidates for the operator in
   question given the converted operand types TYPE1 and TYPE2.  The other
   args are passed through from add_builtin_candidates to
   build_builtin_candidate.  */

static struct z_candidate *
add_builtin_candidate (candidates, code, code2, fnname, type1, type2,
		       args, argtypes, flags)
     struct z_candidate *candidates;
     enum tree_code code, code2;
     tree fnname, type1, type2, *args, *argtypes;
     int flags;
{
  switch (code)
    {
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      args[1] = integer_zero_node;
      type2 = integer_type_node;
    }

  switch (code)
    {

/* 4 For every pair T, VQ), where T is an arithmetic or  enumeration  type,
     and  VQ  is  either  volatile or empty, there exist candidate operator
     functions of the form
	     VQ T&   operator++(VQ T&);
	     T       operator++(VQ T&, int);
   5 For every pair T, VQ), where T is an enumeration type or an arithmetic
     type  other than bool, and VQ is either volatile or empty, there exist
     candidate operator functions of the form
	     VQ T&   operator--(VQ T&);
	     T       operator--(VQ T&, int);
   6 For every pair T, VQ), where T is  a  cv-qualified  or  cv-unqualified
     complete  object type, and VQ is either volatile or empty, there exist
     candidate operator functions of the form
	     T*VQ&   operator++(T*VQ&);
	     T*VQ&   operator--(T*VQ&);
	     T*      operator++(T*VQ&, int);
	     T*      operator--(T*VQ&, int);  */

    case POSTDECREMENT_EXPR:
    case PREDECREMENT_EXPR:
      if (TREE_CODE (type1) == BOOLEAN_TYPE)
	return candidates;
    case POSTINCREMENT_EXPR:
    case PREINCREMENT_EXPR:
      if ((ARITHMETIC_TYPE_P (type1) && TREE_CODE (type1) != ENUMERAL_TYPE)
	  || TYPE_PTROB_P (type1))
	{
	  type1 = build_reference_type (type1);
	  break;
	}
      return candidates;

/* 7 For every cv-qualified or cv-unqualified complete object type T, there
     exist candidate operator functions of the form

	     T&      operator*(T*);

   8 For every function type T, there exist candidate operator functions of
     the form
	     T&      operator*(T*);  */

    case INDIRECT_REF:
      if (TREE_CODE (type1) == POINTER_TYPE
	  && (TYPE_PTROB_P (type1)
	      || TREE_CODE (TREE_TYPE (type1)) == FUNCTION_TYPE))
	break;
      return candidates;

/* 9 For every type T, there exist candidate operator functions of the form
	     T*      operator+(T*);

   10For  every  promoted arithmetic type T, there exist candidate operator
     functions of the form
	     T       operator+(T);
	     T       operator-(T);  */

    case CONVERT_EXPR: /* unary + */
      if (TREE_CODE (type1) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (type1)) != OFFSET_TYPE)
	break;
    case NEGATE_EXPR:
      if (ARITHMETIC_TYPE_P (type1))
	break;
      return candidates;

/* 11For every promoted integral type T,  there  exist  candidate  operator
     functions of the form
	     T       operator~(T);  */

    case BIT_NOT_EXPR:
      if (INTEGRAL_TYPE_P (type1))
	break;
      return candidates;

/* 12For every quintuple C1, C2, T, CV1, CV2), where C2 is a class type, C1
     is the same type as C2 or is a derived class of C2, T  is  a  complete
     object type or a function type, and CV1 and CV2 are cv-qualifier-seqs,
     there exist candidate operator functions of the form
	     CV12 T& operator->*(CV1 C1*, CV2 T C2::*);
     where CV12 is the union of CV1 and CV2.  */

    case MEMBER_REF:
      if (TREE_CODE (type1) == POINTER_TYPE
	  && (TYPE_PTRMEMFUNC_P (type2) || TYPE_PTRMEM_P (type2)))
	{
	  tree c1 = TREE_TYPE (type1);
	  tree c2 = (TYPE_PTRMEMFUNC_P (type2)
		     ? TYPE_METHOD_BASETYPE (TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (type2)))
		     : TYPE_OFFSET_BASETYPE (TREE_TYPE (type2)));

	  if (IS_AGGR_TYPE (c1) && DERIVED_FROM_P (c2, c1)
	      && (TYPE_PTRMEMFUNC_P (type2)
		  || is_complete (TREE_TYPE (TREE_TYPE (type2)))))
	    break;
	}
      return candidates;

/* 13For every pair of promoted arithmetic types L and R, there exist  can-
     didate operator functions of the form
	     LR      operator*(L, R);
	     LR      operator/(L, R);
	     LR      operator+(L, R);
	     LR      operator-(L, R);
	     bool    operator<(L, R);
	     bool    operator>(L, R);
	     bool    operator<=(L, R);
	     bool    operator>=(L, R);
	     bool    operator==(L, R);
	     bool    operator!=(L, R);
     where  LR  is  the  result of the usual arithmetic conversions between
     types L and R.

   14For every pair of types T and I, where T  is  a  cv-qualified  or  cv-
     unqualified  complete  object  type and I is a promoted integral type,
     there exist candidate operator functions of the form
	     T*      operator+(T*, I);
	     T&      operator[](T*, I);
	     T*      operator-(T*, I);
	     T*      operator+(I, T*);
	     T&      operator[](I, T*);

   15For every T, where T is a pointer to complete object type, there exist
     candidate operator functions of the form112)
	     ptrdiff_t operator-(T, T);

   16For  every pointer type T, there exist candidate operator functions of
     the form
	     bool    operator<(T, T);
	     bool    operator>(T, T);
	     bool    operator<=(T, T);
	     bool    operator>=(T, T);
	     bool    operator==(T, T);
	     bool    operator!=(T, T);

   17For every pointer to member type T,  there  exist  candidate  operator
     functions of the form
	     bool    operator==(T, T);
	     bool    operator!=(T, T);  */

    case MINUS_EXPR:
      if (TYPE_PTROB_P (type1) && TYPE_PTROB_P (type2))
	break;
      if (TYPE_PTROB_P (type1) && INTEGRAL_TYPE_P (type2))
	{
	  type2 = ptrdiff_type_node;
	  break;
	}
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	break;
      return candidates;

    case EQ_EXPR:
    case NE_EXPR:
      if (TYPE_PTRMEMFUNC_P (type1) && TYPE_PTRMEMFUNC_P (type2)
	  || TYPE_PTRMEM_P (type1) && TYPE_PTRMEM_P (type2))
	break;
      if ((TYPE_PTRMEMFUNC_P (type1) || TYPE_PTRMEM_P (type1))
	  && null_ptr_cst_p (args[1]))
	{
	  type2 = type1;
	  break;
	}
      if ((TYPE_PTRMEMFUNC_P (type2) || TYPE_PTRMEM_P (type2))
	  && null_ptr_cst_p (args[0]))
	{
	  type1 = type2;
	  break;
	}
    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2)
	  || TYPE_PTR_P (type1) && TYPE_PTR_P (type2))
	break;
      if (TYPE_PTR_P (type1) && null_ptr_cst_p (args[1]))
	{
	  type2 = type1;
	  break;
	}
      if (null_ptr_cst_p (args[0]) && TYPE_PTR_P (type2))
	{
	  type1 = type2;
	  break;
	}
      return candidates;

    case PLUS_EXPR:
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	break;
    case ARRAY_REF:
      if (INTEGRAL_TYPE_P (type1) && TYPE_PTROB_P (type2))
	{
	  type1 = ptrdiff_type_node;
	  break;
	}
      if (TYPE_PTROB_P (type1) && INTEGRAL_TYPE_P (type2))
	{
	  type2 = ptrdiff_type_node;
	  break;
	}
      return candidates;

/* 18For  every pair of promoted integral types L and R, there exist candi-
     date operator functions of the form
	     LR      operator%(L, R);
	     LR      operator&(L, R);
	     LR      operator^(L, R);
	     LR      operator|(L, R);
	     L       operator<<(L, R);
	     L       operator>>(L, R);
     where LR is the result of the  usual  arithmetic  conversions  between
     types L and R.  */

    case TRUNC_MOD_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      if (INTEGRAL_TYPE_P (type1) && INTEGRAL_TYPE_P (type2))
	break;
      return candidates;

/* 19For  every  triple  L, VQ, R), where L is an arithmetic or enumeration
     type, VQ is either volatile or empty, and R is a  promoted  arithmetic
     type, there exist candidate operator functions of the form
	     VQ L&   operator=(VQ L&, R);
	     VQ L&   operator*=(VQ L&, R);
	     VQ L&   operator/=(VQ L&, R);
	     VQ L&   operator+=(VQ L&, R);
	     VQ L&   operator-=(VQ L&, R);

   20For  every  pair T, VQ), where T is any type and VQ is either volatile
     or empty, there exist candidate operator functions of the form
	     T*VQ&   operator=(T*VQ&, T*);

   21For every pair T, VQ), where T is a pointer to member type and  VQ  is
     either  volatile or empty, there exist candidate operator functions of
     the form
	     VQ T&   operator=(VQ T&, T);

   22For every triple  T,  VQ,  I),  where  T  is  a  cv-qualified  or  cv-
     unqualified  complete object type, VQ is either volatile or empty, and
     I is a promoted integral type, there exist  candidate  operator  func-
     tions of the form
	     T*VQ&   operator+=(T*VQ&, I);
	     T*VQ&   operator-=(T*VQ&, I);

   23For  every  triple  L,  VQ,  R), where L is an integral or enumeration
     type, VQ is either volatile or empty, and R  is  a  promoted  integral
     type, there exist candidate operator functions of the form

	     VQ L&   operator%=(VQ L&, R);
	     VQ L&   operator<<=(VQ L&, R);
	     VQ L&   operator>>=(VQ L&, R);
	     VQ L&   operator&=(VQ L&, R);
	     VQ L&   operator^=(VQ L&, R);
	     VQ L&   operator|=(VQ L&, R);  */

    case MODIFY_EXPR:
      switch (code2)
	{
	case PLUS_EXPR:
	case MINUS_EXPR:
	  if (TYPE_PTROB_P (type1) && INTEGRAL_TYPE_P (type2))
	    {
	      type2 = ptrdiff_type_node;
	      break;
	    }
	case MULT_EXPR:
	case TRUNC_DIV_EXPR:
	  if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	    break;
	  return candidates;

	case TRUNC_MOD_EXPR:
	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	case LSHIFT_EXPR:
	case RSHIFT_EXPR:
	  if (INTEGRAL_TYPE_P (type1) && INTEGRAL_TYPE_P (type2))
	    break;
	  return candidates;

	case NOP_EXPR:
	  if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	    break;
	  if ((TYPE_PTRMEMFUNC_P (type1) && TYPE_PTRMEMFUNC_P (type2))
	      || (TYPE_PTR_P (type1) && TYPE_PTR_P (type2))
	      || (TYPE_PTRMEM_P (type1) && TYPE_PTRMEM_P (type2))
	      || ((TYPE_PTRMEMFUNC_P (type1)
		   || TREE_CODE (type1) == POINTER_TYPE)
		  && null_ptr_cst_p (args[1])))
	    {
	      type2 = type1;
	      break;
	    }
	  return candidates;

	default:
	  my_friendly_abort (367);
	}
      type1 = build_reference_type (type1);
      break;

    case COND_EXPR:
      /* Kludge around broken overloading rules whereby
	 bool ? const char& : enum is ambiguous
	 (between int and const char&).  */
      flags |= LOOKUP_NO_TEMP_BIND;

      /* Extension: Support ?: of enumeral type.  Hopefully this will not
         be an extension for long.  */
      if (TREE_CODE (type1) == ENUMERAL_TYPE && type1 == type2)
	break;
      else if (TREE_CODE (type1) == ENUMERAL_TYPE
	       || TREE_CODE (type2) == ENUMERAL_TYPE)
	return candidates;
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	break;
      if (TREE_CODE (type1) == TREE_CODE (type2)
	  && (TREE_CODE (type1) == REFERENCE_TYPE
	      || TREE_CODE (type1) == POINTER_TYPE
	      || TYPE_PTRMEMFUNC_P (type1)
	      || IS_AGGR_TYPE (type1)))
	break;
      if (TREE_CODE (type1) == REFERENCE_TYPE
	  || TREE_CODE (type2) == REFERENCE_TYPE)
	return candidates;
      if (((TYPE_PTRMEMFUNC_P (type1) || TREE_CODE (type1) == POINTER_TYPE)
	   && null_ptr_cst_p (args[1]))
	  || IS_AGGR_TYPE (type1))
	{
	  type2 = type1;
	  break;
	}
      if (((TYPE_PTRMEMFUNC_P (type2) || TREE_CODE (type2) == POINTER_TYPE)
	   && null_ptr_cst_p (args[0]))
	  || IS_AGGR_TYPE (type2))
	{
	  type1 = type2;
	  break;
	}
      return candidates;

    default:
      my_friendly_abort (367);
    }

  /* If we're dealing with two pointer types, we need candidates
     for both of them.  */
  if (type2 && type1 != type2
      && TREE_CODE (type1) == TREE_CODE (type2)
      && (TREE_CODE (type1) == REFERENCE_TYPE
	  || (TREE_CODE (type1) == POINTER_TYPE
	      && TYPE_PTRMEM_P (type1) == TYPE_PTRMEM_P (type2))
	  || TYPE_PTRMEMFUNC_P (type1)
	  || IS_AGGR_TYPE (type1)))
    {
      candidates = build_builtin_candidate
	(candidates, fnname, type1, type1, args, argtypes, flags);
      return build_builtin_candidate
	(candidates, fnname, type2, type2, args, argtypes, flags);
    }

  return build_builtin_candidate
    (candidates, fnname, type1, type2, args, argtypes, flags);
}

tree
type_decays_to (type)
     tree type;
{
  if (TREE_CODE (type) == ARRAY_TYPE)
    return build_pointer_type (TREE_TYPE (type));
  if (TREE_CODE (type) == FUNCTION_TYPE)
    return build_pointer_type (type);
  return type;
}

/* There are three conditions of builtin candidates:

   1) bool-taking candidates.  These are the same regardless of the input.
   2) pointer-pair taking candidates.  These are generated for each type
      one of the input types converts to.
   3) arithmetic candidates.  According to the WP, we should generate
      all of these, but I'm trying not to... */

static struct z_candidate *
add_builtin_candidates (candidates, code, code2, fnname, args, flags)
     struct z_candidate *candidates;
     enum tree_code code, code2;
     tree fnname, *args;
     int flags;
{
  int ref1, i;
  tree type, argtypes[3], types[2];

  for (i = 0; i < 3; ++i)
    {
      if (args[i])
	argtypes[i]  = lvalue_type (args[i]);
      else
	argtypes[i] = NULL_TREE;
    }

  switch (code)
    {
/* 4 For every pair T, VQ), where T is an arithmetic or  enumeration  type,
     and  VQ  is  either  volatile or empty, there exist candidate operator
     functions of the form
		 VQ T&   operator++(VQ T&);  */

    case POSTINCREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case MODIFY_EXPR:
      ref1 = 1;
      break;

/* 24There also exist candidate operator functions of the form
	     bool    operator!(bool);
	     bool    operator&&(bool, bool);
	     bool    operator||(bool, bool);  */

    case TRUTH_NOT_EXPR:
      return build_builtin_candidate
	(candidates, fnname, boolean_type_node,
	 NULL_TREE, args, argtypes, flags);

    case TRUTH_ORIF_EXPR:
    case TRUTH_ANDIF_EXPR:
      return build_builtin_candidate
	(candidates, fnname, boolean_type_node,
	 boolean_type_node, args, argtypes, flags);

    case ADDR_EXPR:
    case COMPOUND_EXPR:
    case COMPONENT_REF:
      return candidates;

    default:
      ref1 = 0;
    }

  types[0] = types[1] = NULL_TREE;

  for (i = 0; i < 2; ++i)
    {
      if (! args[i])
	;
      else if (IS_AGGR_TYPE (argtypes[i]))
	{
	  tree convs = lookup_conversions (argtypes[i]);

	  if (code == COND_EXPR)
	    {
	      if (real_lvalue_p (args[i]))
		types[i] = scratch_tree_cons
		  (NULL_TREE, build_reference_type (argtypes[i]), types[i]);

	      types[i] = scratch_tree_cons
		(NULL_TREE, TYPE_MAIN_VARIANT (argtypes[i]), types[i]);
	    }
		
	  else if (! convs || (i == 0 && code == MODIFY_EXPR
			       && code2 == NOP_EXPR))
	    return candidates;

	  for (; convs; convs = TREE_CHAIN (convs))
	    {
	      type = TREE_TYPE (TREE_TYPE (TREE_VALUE (convs)));

	      if (i == 0 && ref1
		  && (TREE_CODE (type) != REFERENCE_TYPE
		      || TYPE_READONLY (TREE_TYPE (type))))
		continue;

	      if (code == COND_EXPR && TREE_CODE (type) == REFERENCE_TYPE)
		types[i] = scratch_tree_cons (NULL_TREE, type, types[i]);

	      type = non_reference (type);
	      if (i != 0 || ! ref1)
		{
		  type = TYPE_MAIN_VARIANT (type_decays_to (type));
		  if (code == COND_EXPR && TREE_CODE (type) == ENUMERAL_TYPE)
		    types[i] = scratch_tree_cons (NULL_TREE, type, types[i]);
		  if (INTEGRAL_TYPE_P (type))
		    type = type_promotes_to (type);
		}

	      if (! value_member (type, types[i]))
		types[i] = scratch_tree_cons (NULL_TREE, type, types[i]);
	    }
	}
      else
	{
	  if (code == COND_EXPR && real_lvalue_p (args[i]))
	    types[i] = scratch_tree_cons
	      (NULL_TREE, build_reference_type (argtypes[i]), types[i]);
	  type = non_reference (argtypes[i]);
	  if (i != 0 || ! ref1)
	    {
	      type = TYPE_MAIN_VARIANT (type_decays_to (type));
	      if (code == COND_EXPR && TREE_CODE (type) == ENUMERAL_TYPE)
		types[i] = scratch_tree_cons (NULL_TREE, type, types[i]);
	      if (INTEGRAL_TYPE_P (type))
		type = type_promotes_to (type);
	    }
	  types[i] = scratch_tree_cons (NULL_TREE, type, types[i]);
	}
    }

  for (; types[0]; types[0] = TREE_CHAIN (types[0]))
    {
      if (types[1])
	for (type = types[1]; type; type = TREE_CHAIN (type))
	  candidates = add_builtin_candidate
	    (candidates, code, code2, fnname, TREE_VALUE (types[0]),
	     TREE_VALUE (type), args, argtypes, flags);
      else
	candidates = add_builtin_candidate
	  (candidates, code, code2, fnname, TREE_VALUE (types[0]),
	   NULL_TREE, args, argtypes, flags);
    }

  return candidates;
}

/* If TMPL can be successfully instantiated as indicated by
   EXPLICIT_TARGS and ARGLIST, adds the instantiation to CANDIDATES.

   TMPL is the template.  EXPLICIT_TARGS are any explicit template arguments.
   ARGLIST is the arguments provided at the call-site.  The RETURN_TYPE
   is the desired type for conversion operators.  FLAGS are as for
   add_function_candidate.  */

static struct z_candidate *
add_template_candidate (candidates, tmpl, explicit_targs, 
			arglist, return_type, flags)
     struct z_candidate *candidates;
     tree tmpl, explicit_targs, arglist, return_type;
     int flags;
{
  int ntparms = DECL_NTPARMS (tmpl);
  tree targs = make_scratch_vec (ntparms);
  struct z_candidate *cand;
  int i;
  tree fn;

  i = fn_type_unification (tmpl, explicit_targs, targs, arglist,
			   return_type, 0); 

  if (i != 0)
    return candidates;

  fn = instantiate_template (tmpl, targs);
  if (fn == error_mark_node)
    return candidates;

  cand = add_function_candidate (candidates, fn, arglist, flags);
  cand->template = DECL_TEMPLATE_INFO (fn);
  return cand;
}


static struct z_candidate *
add_template_conv_candidate (candidates, tmpl, obj, arglist, return_type)
     struct z_candidate *candidates;
     tree tmpl, obj, arglist, return_type;
{
  int ntparms = DECL_NTPARMS (tmpl);
  tree targs = make_scratch_vec (ntparms);
  struct z_candidate *cand;
  int i;
  tree fn;

  i = fn_type_unification (tmpl, NULL_TREE, targs, arglist, return_type, 0);

  if (i != 0)
    return candidates;

  fn = instantiate_template (tmpl, targs);
  if (fn == error_mark_node)
    return candidates;

  cand = add_conv_candidate (candidates, fn, obj, arglist);
  cand->template = DECL_TEMPLATE_INFO (fn);
  return cand;
}


static int
any_viable (cands)
     struct z_candidate *cands;
{
  for (; cands; cands = cands->next)
    if (pedantic ? cands->viable == 1 : cands->viable)
      return 1;
  return 0;
}

static struct z_candidate *
splice_viable (cands)
     struct z_candidate *cands;
{
  struct z_candidate **p = &cands;

  for (; *p; )
    {
      if (pedantic ? (*p)->viable == 1 : (*p)->viable)
	p = &((*p)->next);
      else
	*p = (*p)->next;
    }

  return cands;
}

static tree
build_this (obj)
     tree obj;
{
  /* Fix this to work on non-lvalues.  */
  if (IS_SIGNATURE_POINTER (TREE_TYPE (obj))
      || IS_SIGNATURE_REFERENCE (TREE_TYPE (obj)))
    return obj;
  else
    return build_unary_op (ADDR_EXPR, obj, 0);
}

static void
print_z_candidates (candidates)
     struct z_candidate *candidates;
{
  char *str = "candidates are:";
  for (; candidates; candidates = candidates->next)
    {
      if (TREE_CODE (candidates->fn) == IDENTIFIER_NODE)
	{
	  if (candidates->fn == ansi_opname [COND_EXPR])
	    cp_error ("%s %D(%T, %T, %T) <builtin>", str, candidates->fn,
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 0)),
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 1)),
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 2)));
	  else if (TREE_VEC_LENGTH (candidates->convs) == 2)
	    cp_error ("%s %D(%T, %T) <builtin>", str, candidates->fn,
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 0)),
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 1)));
	  else
	    cp_error ("%s %D(%T) <builtin>", str, candidates->fn,
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 0)));
	}
      else
	cp_error_at ("%s %+D%s", str, candidates->fn,
		     candidates->viable == -1 ? " <near match>" : "");
      str = "               "; 
    }
}

/* Returns the best overload candidate to perform the requested
   conversion.  This function is used for three the overloading situations
   described in [over.match.copy], [over.match.conv], and [over.match.ref].
   If TOTYPE is a REFERENCE_TYPE, we're trying to find an lvalue binding as
   per [dcl.init.ref], so we ignore temporary bindings.  */

static struct z_candidate *
build_user_type_conversion_1 (totype, expr, flags)
     tree totype, expr;
     int flags;
{
  struct z_candidate *candidates, *cand;
  tree fromtype = TREE_TYPE (expr);
  tree ctors = NULL_TREE, convs = NULL_TREE, *p;
  tree args;
  tree templates = NULL_TREE;

  if (IS_AGGR_TYPE (totype))
    ctors = lookup_fnfields (TYPE_BINFO (totype), ctor_identifier, 0);
  if (IS_AGGR_TYPE (fromtype)
      && (! IS_AGGR_TYPE (totype) || ! DERIVED_FROM_P (totype, fromtype)))
    convs = lookup_conversions (fromtype);

  candidates = 0;
  flags |= LOOKUP_NO_CONVERSION;

  if (ctors)
    {
      tree t = build_int_2 (0, 0);
      TREE_TYPE (t) = build_pointer_type (totype);
      args = build_scratch_list (NULL_TREE, expr);
      if (TYPE_USES_VIRTUAL_BASECLASSES (totype))
	args = scratch_tree_cons (NULL_TREE, integer_one_node, args);
      args = scratch_tree_cons (NULL_TREE, t, args);

      ctors = TREE_VALUE (ctors);
    }
  for (; ctors; ctors = DECL_CHAIN (ctors))
    {
      if (DECL_NONCONVERTING_P (ctors))
	continue;

      if (TREE_CODE (ctors) == TEMPLATE_DECL) 
	{
	  templates = scratch_tree_cons (NULL_TREE, ctors, templates);
	  candidates = 
	    add_template_candidate (candidates, ctors,
				    NULL_TREE, args, NULL_TREE, flags);
	} 
      else 
	candidates = add_function_candidate (candidates, ctors,
					     args, flags); 

      if (candidates) 
	{
	  candidates->second_conv = build1 (IDENTITY_CONV, totype, NULL_TREE);
	  candidates->basetype_path = TYPE_BINFO (totype);
	} 
    }

  if (convs)
    args = build_scratch_list (NULL_TREE, build_this (expr));

  for (; convs; convs = TREE_CHAIN (convs))
    {
      tree fn = TREE_VALUE (convs);
      int convflags = LOOKUP_NO_CONVERSION;
      tree ics;

      /* If we are called to convert to a reference type, we are trying to
	 find an lvalue binding, so don't even consider temporaries.  If
	 we don't find an lvalue binding, the caller will try again to
	 look for a temporary binding.  */
      if (TREE_CODE (totype) == REFERENCE_TYPE)
	convflags |= LOOKUP_NO_TEMP_BIND;

      ics = implicit_conversion
	(totype, TREE_TYPE (TREE_TYPE (fn)), 0, convflags);

      if (TREE_CODE (totype) == REFERENCE_TYPE && ics && ICS_BAD_FLAG (ics))
	/* ignore the near match.  */;
      else if (ics)
	for (; fn; fn = DECL_CHAIN (fn))
	  {
	    if (TREE_CODE (fn) == TEMPLATE_DECL)
	      {
		templates = scratch_tree_cons (NULL_TREE, fn, templates);
		candidates = 
		  add_template_candidate (candidates, fn, NULL_TREE,
					  args, totype, flags);
	      } 
	    else 
	      candidates = add_function_candidate (candidates, fn,
						   args, flags); 

	    if (candidates) 
	      {
		candidates->second_conv = ics;
		candidates->basetype_path = TREE_PURPOSE (convs);
		if (candidates->viable == 1 && ICS_BAD_FLAG (ics))
		  candidates->viable = -1;
	      }
	  }
    }

  if (! any_viable (candidates))
    {
#if 0
      if (flags & LOOKUP_COMPLAIN)
	{
	  if (candidates && ! candidates->next)
	    /* say why this one won't work or try to be loose */;
	  else
	    cp_error ("no viable candidates");
	}
#endif

      return 0;
    }

  candidates = splice_viable (candidates);
  cand = tourney (candidates);

  if (cand == 0)
    {
      if (flags & LOOKUP_COMPLAIN)
	{
	  cp_error ("conversion from `%T' to `%T' is ambiguous",
		    fromtype, totype);
	  print_z_candidates (candidates);
	}

      cand = candidates;	/* any one will do */
      cand->second_conv = build1 (AMBIG_CONV, totype, expr);
      ICS_USER_FLAG (cand->second_conv) = 1;
      ICS_BAD_FLAG (cand->second_conv) = 1;

      return cand;
    }

  for (p = &(cand->second_conv); TREE_CODE (*p) != IDENTITY_CONV; )
    p = &(TREE_OPERAND (*p, 0));

  /* Pedantically, normal function declarations are never considered
     to refer to template instantiations, so we only do this with
     -fguiding-decls.  */ 
  if (flag_guiding_decls && templates && ! cand->template 
      && !DECL_INITIAL (cand->fn) 
      && TREE_CODE (TREE_TYPE (cand->fn)) != METHOD_TYPE)
    add_maybe_template (cand->fn, templates);

  *p = build
    (USER_CONV,
     (DECL_CONSTRUCTOR_P (cand->fn)
      ? totype : non_reference (TREE_TYPE (TREE_TYPE (cand->fn)))),
     expr, cand->fn, cand->convs, cand->basetype_path);
  ICS_USER_FLAG (cand->second_conv) = 1;
  if (cand->viable == -1)
    ICS_BAD_FLAG (cand->second_conv) = 1;

  return cand;
}

tree
build_user_type_conversion (totype, expr, flags)
     tree totype, expr;
     int flags;
{
  struct z_candidate *cand
    = build_user_type_conversion_1 (totype, expr, flags);

  if (cand)
    {
      if (TREE_CODE (cand->second_conv) == AMBIG_CONV)
	return error_mark_node;
      return convert_from_reference (convert_like (cand->second_conv, expr));
    }
  return NULL_TREE;
}

/* Do any initial processing on the arguments to a function call.  */

static tree
resolve_args (args)
     tree args;
{
  tree t;
  for (t = args; t; t = TREE_CHAIN (t))
    {
      if (TREE_VALUE (t) == error_mark_node)
	return error_mark_node;
      else if (TREE_CODE (TREE_TYPE (TREE_VALUE (t))) == VOID_TYPE)
	{
	  error ("invalid use of void expression");
	  return error_mark_node;
	}
      else if (TREE_CODE (TREE_VALUE (t)) == OFFSET_REF)
	TREE_VALUE (t) = resolve_offset_ref (TREE_VALUE (t));
    }
  return args;
}
      
tree
build_new_function_call (fn, args, obj)
     tree fn, args, obj;
{
  struct z_candidate *candidates = 0, *cand;
  tree explicit_targs = NULL_TREE;
  int template_only = 0;

  if (TREE_CODE (fn) == TEMPLATE_ID_EXPR)
    {
      explicit_targs = TREE_OPERAND (fn, 1);
      fn = TREE_OPERAND (fn, 0);
      template_only = 1;
    }

  if (obj == NULL_TREE && really_overloaded_fn (fn))
    {
      tree t;
      tree templates = NULL_TREE;

      args = resolve_args (args);

      if (args == error_mark_node)
	return error_mark_node;

      for (t = TREE_VALUE (fn); t; t = DECL_CHAIN (t))
	{
	  if (TREE_CODE (t) == TEMPLATE_DECL)
	    {
	      templates = scratch_tree_cons (NULL_TREE, t, templates);
	      candidates = add_template_candidate
		(candidates, t, explicit_targs, args, NULL_TREE,
		 LOOKUP_NORMAL);  
	    }
	  else if (! template_only)
	    candidates = add_function_candidate
	      (candidates, t, args, LOOKUP_NORMAL);
	}

      if (! any_viable (candidates))
	{
	  if (candidates && ! candidates->next)
	    return build_function_call (candidates->fn, args);
	  cp_error ("no matching function for call to `%D (%A)'",
		    TREE_PURPOSE (fn), args);
	  if (candidates)
	    print_z_candidates (candidates);
	  return error_mark_node;
	}
      candidates = splice_viable (candidates);
      cand = tourney (candidates);

      if (cand == 0)
	{
	  cp_error ("call of overloaded `%D (%A)' is ambiguous",
		    TREE_PURPOSE (fn), args);
	  print_z_candidates (candidates);
	  return error_mark_node;
	}

      /* Pedantically, normal function declarations are never considered
	 to refer to template instantiations, so we only do this with
	 -fguiding-decls.  */
      if (flag_guiding_decls && templates && ! cand->template 
	  && ! DECL_INITIAL (cand->fn))
	add_maybe_template (cand->fn, templates);

      return build_over_call (cand->fn, cand->convs, args, LOOKUP_NORMAL);
    }

  return build_function_call (fn, args);
}

static tree
build_object_call (obj, args)
     tree obj, args;
{
  struct z_candidate *candidates = 0, *cand;
  tree fns, convs, mem_args;
  tree type = TREE_TYPE (obj);
  tree templates = NULL_TREE;

  fns = lookup_fnfields (TYPE_BINFO (type), ansi_opname [CALL_EXPR], 0);

  args = resolve_args (args);

  if (args == error_mark_node)
    return error_mark_node;

  if (fns)
    {
      tree fn = TREE_VALUE (fns);
      mem_args = scratch_tree_cons (NULL_TREE, build_this (obj), args);

      for (; fn; fn = DECL_CHAIN (fn))
	{
	  if (TREE_CODE (fn) == TEMPLATE_DECL)
	    {
	      templates = scratch_tree_cons (NULL_TREE, fn, templates);
	      candidates 
		= add_template_candidate (candidates, fn, NULL_TREE,
					  mem_args, NULL_TREE, 
					  LOOKUP_NORMAL);
	    }
	  else
	    candidates = add_function_candidate
	      (candidates, fn, mem_args, LOOKUP_NORMAL);

	  if (candidates)
	    candidates->basetype_path = TREE_PURPOSE (fns);
	}
    }

  convs = lookup_conversions (type);

  for (; convs; convs = TREE_CHAIN (convs))
    {
      tree fn = TREE_VALUE (convs);
      tree totype = TREE_TYPE (TREE_TYPE (fn));

      if (TREE_CODE (totype) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (totype)) == FUNCTION_TYPE)
	for (; fn; fn = DECL_CHAIN (fn))
	  {
	    if (TREE_CODE (fn) == TEMPLATE_DECL) 
	      {
		templates = scratch_tree_cons (NULL_TREE, fn, templates);
		candidates = add_template_conv_candidate (candidates,
							  fn,
							  obj,
							  args,
							  totype);
	      }
	    else
	      candidates = add_conv_candidate (candidates, fn, obj, args);

	    if (candidates)
	      candidates->basetype_path = TREE_PURPOSE (convs);
	  }
    }

  if (! any_viable (candidates))
    {
      cp_error ("no match for call to `(%T) (%A)'", TREE_TYPE (obj), args);
      print_z_candidates (candidates);
      return error_mark_node;
    }

  candidates = splice_viable (candidates);
  cand = tourney (candidates);

  if (cand == 0)
    {
      cp_error ("call of `(%T) (%A)' is ambiguous", TREE_TYPE (obj), args);
      print_z_candidates (candidates);
      return error_mark_node;
    }

  if (DECL_NAME (cand->fn) == ansi_opname [CALL_EXPR])
    return build_over_call (cand->fn, cand->convs, mem_args, LOOKUP_NORMAL);

  obj = convert_like (TREE_VEC_ELT (cand->convs, 0), obj);

  /* FIXME */
  return build_function_call (obj, args);
}

static void
op_error (code, code2, arg1, arg2, arg3, problem)
     enum tree_code code, code2;
     tree arg1, arg2, arg3;
     char *problem;
{
  char * opname
    = (code == MODIFY_EXPR ? assignop_tab [code2] : opname_tab [code]);

  switch (code)
    {
    case COND_EXPR:
      cp_error ("%s for `%T ? %T : %T'", problem,
		error_type (arg1), error_type (arg2), error_type (arg3));
      break;
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      cp_error ("%s for `%T%s'", problem, error_type (arg1), opname);
      break;
    case ARRAY_REF:
      cp_error ("%s for `%T[%T]'", problem,
		error_type (arg1), error_type (arg2));
      break;
    default:
      if (arg2)
	cp_error ("%s for `%T %s %T'", problem,
		  error_type (arg1), opname, error_type (arg2));
      else
	cp_error ("%s for `%s%T'", problem, opname, error_type (arg1));
    }
}

tree
build_new_op (code, flags, arg1, arg2, arg3)
     enum tree_code code;
     int flags;
     tree arg1, arg2, arg3;
{
  struct z_candidate *candidates = 0, *cand;
  tree fns, mem_arglist, arglist, fnname;
  enum tree_code code2 = NOP_EXPR;
  tree templates = NULL_TREE;
  tree conv;

  if (arg1 == error_mark_node
      || arg2 == error_mark_node
      || arg3 == error_mark_node)
    return error_mark_node;

  /* This can happen if a template takes all non-type parameters, e.g.
     undeclared_template<1, 5, 72>a;  */
  if (code == LT_EXPR && TREE_CODE (arg1) == TEMPLATE_DECL)
    {
      cp_error ("`%D' must be declared before use", arg1);
      return error_mark_node;
    }

  if (code == MODIFY_EXPR)
    {
      code2 = TREE_CODE (arg3);
      arg3 = NULL_TREE;
      fnname = ansi_assopname[code2];
    }
  else
    fnname = ansi_opname[code];

  switch (code)
    {
    case NEW_EXPR:
    case VEC_NEW_EXPR:
      {
	tree rval;

	arglist = scratch_tree_cons (NULL_TREE, arg2, arg3);
	if (flags & LOOKUP_GLOBAL)
	  return build_new_function_call
	    (lookup_name_nonclass (fnname), arglist, NULL_TREE);

	/* FIXME */
	rval = build_method_call
	  (build_indirect_ref (build1 (NOP_EXPR, arg1, error_mark_node),
			       "new"),
	   fnname, arglist, NULL_TREE, flags);
	if (rval == error_mark_node)
	  /* User might declare fancy operator new, but invoke it
	     like standard one.  */
	  return rval;

	TREE_TYPE (rval) = arg1;
	TREE_CALLS_NEW (rval) = 1;
	return rval;
      }

    case VEC_DELETE_EXPR:
    case DELETE_EXPR:
      {
	tree rval;

	if (flags & LOOKUP_GLOBAL)
	  return build_new_function_call
	    (lookup_name_nonclass (fnname),
	     build_scratch_list (NULL_TREE, arg1), NULL_TREE);

	arglist = scratch_tree_cons (NULL_TREE, arg1, build_scratch_list (NULL_TREE, arg2));

	arg1 = TREE_TYPE (arg1);

	/* This handles the case where we're trying to delete
	   X (*a)[10];
	   a=new X[5][10];
	   delete[] a; */
	   
	if (TREE_CODE (TREE_TYPE (arg1)) == ARRAY_TYPE)
	  {
	    /* Strip off the pointer and the array.  */
	    arg1 = TREE_TYPE (TREE_TYPE (arg1));

	    while (TREE_CODE (arg1) == ARRAY_TYPE)
		arg1 = (TREE_TYPE (arg1));

	    arg1 = build_pointer_type (arg1);
	  }

	/* FIXME */
	rval = build_method_call
	  (build_indirect_ref (build1 (NOP_EXPR, arg1,
				       error_mark_node),
			       NULL_PTR),
	   fnname, arglist, NULL_TREE, flags);
#if 0
	/* This can happen when operator delete is protected.  */
	my_friendly_assert (rval != error_mark_node, 250);
	TREE_TYPE (rval) = void_type_node;
#endif
	return rval;
      }

    case CALL_EXPR:
      return build_object_call (arg1, arg2);
    }

  /* The comma operator can have void args.  */
  if (TREE_CODE (arg1) == OFFSET_REF)
    arg1 = resolve_offset_ref (arg1);
  if (arg2 && TREE_CODE (arg2) == OFFSET_REF)
    arg2 = resolve_offset_ref (arg2);
  if (arg3 && TREE_CODE (arg3) == OFFSET_REF)
    arg3 = resolve_offset_ref (arg3);

  if (code == COND_EXPR)
    {
      if (arg2 == NULL_TREE
	  || TREE_CODE (TREE_TYPE (arg2)) == VOID_TYPE
	  || TREE_CODE (TREE_TYPE (arg3)) == VOID_TYPE
	  || (! IS_OVERLOAD_TYPE (TREE_TYPE (arg2))
	      && ! IS_OVERLOAD_TYPE (TREE_TYPE (arg3))))
	goto builtin;
    }
  else if (! IS_OVERLOAD_TYPE (TREE_TYPE (arg1))
	   && (! arg2 || ! IS_OVERLOAD_TYPE (TREE_TYPE (arg2))))
    goto builtin;

  if (code == POSTINCREMENT_EXPR || code == POSTDECREMENT_EXPR)
    arg2 = integer_zero_node;

  fns = lookup_name_nonclass (fnname);
  /* + Koenig lookup */

  if (arg2 && arg3)
    arglist = scratch_tree_cons (NULL_TREE, arg1, scratch_tree_cons
		      (NULL_TREE, arg2, build_scratch_list (NULL_TREE, arg3)));
  else if (arg2)
    arglist = scratch_tree_cons (NULL_TREE, arg1, build_scratch_list (NULL_TREE, arg2));
  else
    arglist = build_scratch_list (NULL_TREE, arg1);

  if (fns && TREE_CODE (fns) == TREE_LIST)
    fns = TREE_VALUE (fns);
  for (; fns; fns = DECL_CHAIN (fns))
    {
      if (TREE_CODE (fns) == TEMPLATE_DECL)
	{
	  templates = scratch_tree_cons (NULL_TREE, fns, templates);
	  candidates 
	    = add_template_candidate (candidates, fns, NULL_TREE,
				      arglist, TREE_TYPE (fnname),
				      flags); 
	}
      else
	candidates = add_function_candidate (candidates, fns, arglist, flags);
    }

  if (IS_AGGR_TYPE (TREE_TYPE (arg1)))
    fns = lookup_fnfields (TYPE_BINFO (TREE_TYPE (arg1)), fnname, 0);
  else
    fns = NULL_TREE;

  if (fns)
    {
      tree fn = TREE_VALUE (fns);
      mem_arglist = scratch_tree_cons (NULL_TREE, build_this (arg1), TREE_CHAIN (arglist));
      for (; fn; fn = DECL_CHAIN (fn))
	{
	  tree this_arglist;

	  if (TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE)
	    this_arglist = mem_arglist;
	  else
	    this_arglist = arglist;

	  if (TREE_CODE (fn) == TEMPLATE_DECL)
	    {
	      /* A member template. */
	      templates = scratch_tree_cons (NULL_TREE, fn, templates);
	      candidates 
		= add_template_candidate (candidates, fn, NULL_TREE,
					  this_arglist,  TREE_TYPE
					  (fnname), LOOKUP_NORMAL); 
	    }
	  else
	    candidates = add_function_candidate
	      (candidates, fn, this_arglist, flags);

	  if (candidates) 
	    candidates->basetype_path = TREE_PURPOSE (fns);
	}
    }

  {
    tree args[3];

    /* Rearrange the arguments for ?: so that add_builtin_candidate only has
       to know about two args; a builtin candidate will always have a first
       parameter of type bool.  We'll handle that in
       build_builtin_candidate.  */
    if (code == COND_EXPR)
      {
	args[0] = arg2;
	args[1] = arg3;
	args[2] = arg1;
      }
    else
      {
	args[0] = arg1;
	args[1] = arg2;
	args[2] = NULL_TREE;
      }

    candidates = add_builtin_candidates
      (candidates, code, code2, fnname, args, flags);
  }

  if (! any_viable (candidates))
    {
      switch (code)
	{
	case POSTINCREMENT_EXPR:
	case POSTDECREMENT_EXPR:
	  /* Look for an `operator++ (int)'.  If they didn't have
	     one, then we fall back to the old way of doing things.  */
	  if (flags & LOOKUP_COMPLAIN)
	    cp_pedwarn ("no `%D (int)' declared for postfix `%s', trying prefix operator instead",
			fnname, opname_tab [code]);
	  if (code == POSTINCREMENT_EXPR)
	    code = PREINCREMENT_EXPR;
	  else
	    code = PREDECREMENT_EXPR;	
	  return build_new_op (code, flags, arg1, NULL_TREE, NULL_TREE);
	  
	  /* The caller will deal with these.  */
	case ADDR_EXPR:
	case COMPOUND_EXPR:
	case COMPONENT_REF:
	  return NULL_TREE;
	}
      if (flags & LOOKUP_COMPLAIN)
	{
	  op_error (code, code2, arg1, arg2, arg3, "no match");
	  print_z_candidates (candidates);
	}
      return error_mark_node;
    }
  candidates = splice_viable (candidates);
  cand = tourney (candidates);

  if (cand == 0)
    {
      if (flags & LOOKUP_COMPLAIN)
	{
	  op_error (code, code2, arg1, arg2, arg3, "ambiguous overload");
	  print_z_candidates (candidates);
	}
      return error_mark_node;
    }

  if (TREE_CODE (cand->fn) == FUNCTION_DECL)
    {
      extern int warn_synth;
      if (warn_synth
	  && fnname == ansi_opname[MODIFY_EXPR]
	  && DECL_ARTIFICIAL (cand->fn)
	  && candidates->next
	  && ! candidates->next->next)
	{
	  cp_warning ("using synthesized `%#D' for copy assignment",
		      cand->fn);
	  cp_warning_at ("  where cfront would use `%#D'",
			 cand == candidates
			 ? candidates->next->fn
			 : candidates->fn);
	}

      if (DECL_FUNCTION_MEMBER_P (cand->fn))
	enforce_access (cand->basetype_path, cand->fn);

      /* Pedantically, normal function declarations are never considered
	 to refer to template instantiations, so we only do this with
	 -fguiding-decls.  */ 
      if (flag_guiding_decls && templates && ! cand->template 
	  && ! DECL_INITIAL (cand->fn)
	  && TREE_CODE (TREE_TYPE (cand->fn)) != METHOD_TYPE)
	add_maybe_template (cand->fn, templates);

      return build_over_call
	(cand->fn, cand->convs,
	 TREE_CODE (TREE_TYPE (cand->fn)) == METHOD_TYPE
	 ? mem_arglist : arglist,
	 LOOKUP_NORMAL);
    }

  /* Check for comparison of different enum types.  */
  switch (code)
    {
    case GT_EXPR:
    case LT_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      if (flag_int_enum_equivalence == 0 
	  && TREE_CODE (TREE_TYPE (arg1)) == ENUMERAL_TYPE 
	  && TREE_CODE (TREE_TYPE (arg2)) == ENUMERAL_TYPE 
	  && (TYPE_MAIN_VARIANT (TREE_TYPE (arg1))
	      != TYPE_MAIN_VARIANT (TREE_TYPE (arg2))))
	{
	  cp_warning ("comparison between `%#T' and `%#T'", 
		      TREE_TYPE (arg1), TREE_TYPE (arg2));
	}
    }

  /* We need to strip any leading REF_BIND so that bitfields don't cause
     errors.  This should not remove any important conversions, because
     builtins don't apply to class objects directly.  */
  conv = TREE_VEC_ELT (cand->convs, 0);
  if (TREE_CODE (conv) == REF_BIND)
    conv = TREE_OPERAND (conv, 0);
  arg1 = convert_like (conv, arg1);
  if (arg2)
    arg2 = convert_like (TREE_VEC_ELT (cand->convs, 1), arg2);
  if (arg3)
    arg3 = convert_like (TREE_VEC_ELT (cand->convs, 2), arg3);

builtin:
  switch (code)
    {
    case MODIFY_EXPR:
      return build_modify_expr (arg1, code2, arg2);

    case INDIRECT_REF:
      return build_indirect_ref (arg1, "unary *");

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case GT_EXPR:
    case LT_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case TRUNC_MOD_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      return build_binary_op_nodefault (code, arg1, arg2, code);

    case CONVERT_EXPR:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      return build_unary_op (code, arg1, candidates != 0);

    case ARRAY_REF:
      return build_array_ref (arg1, arg2);

    case COND_EXPR:
      return build_conditional_expr (arg1, arg2, arg3);

    case MEMBER_REF:
      return build_m_component_ref
	(build_indirect_ref (arg1, NULL_PTR), arg2);

      /* The caller will deal with these.  */
    case ADDR_EXPR:
    case COMPONENT_REF:
    case COMPOUND_EXPR:
      return NULL_TREE;

    default:
      my_friendly_abort (367);
    }
}

static void
enforce_access (basetype_path, function)
     tree basetype_path, function;
{
  tree access = compute_access (basetype_path, function);

  if (access == access_private_node)
    {
      cp_error_at ("`%+#D' is %s", function, 
		   TREE_PRIVATE (function) ? "private"
		   : "from private base class");
      error ("within this context");
    }
  else if (access == access_protected_node)
    {
      cp_error_at ("`%+#D' %s", function,
		   TREE_PROTECTED (function) ? "is protected"
		   : "has protected accessibility");
      error ("within this context");
    }
}

/* Perform the conversions in CONVS on the expression EXPR.  */

static tree
convert_like (convs, expr)
     tree convs, expr;
{
  if (ICS_BAD_FLAG (convs)
      && TREE_CODE (convs) != USER_CONV
      && TREE_CODE (convs) != AMBIG_CONV)
    {
      tree t = convs; 
      for (; t; t = TREE_OPERAND (t, 0))
	{
	  if (TREE_CODE (t) == USER_CONV)
	    {
	      expr = convert_like (t, expr);
	      break;
	    }
	  else if (TREE_CODE (t) == AMBIG_CONV)
	    return convert_like (t, expr);
	  else if (TREE_CODE (t) == IDENTITY_CONV)
	    break;
	}
      return convert_for_initialization
	(NULL_TREE, TREE_TYPE (convs), expr, LOOKUP_NORMAL,
	 "conversion", NULL_TREE, 0);
    }

  switch (TREE_CODE (convs))
    {
    case USER_CONV:
      {
	tree fn = TREE_OPERAND (convs, 1);
	tree args;
	enforce_access (TREE_OPERAND (convs, 3), fn);

	if (DECL_CONSTRUCTOR_P (fn))
	  {
	    tree t = build_int_2 (0, 0);
	    TREE_TYPE (t) = build_pointer_type (DECL_CONTEXT (fn));

	    args = build_scratch_list (NULL_TREE, expr);
	    if (TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (fn)))
	      args = scratch_tree_cons (NULL_TREE, integer_one_node, args);
	    args = scratch_tree_cons (NULL_TREE, t, args);
	  }
	else
	  args = build_this (expr);
	expr = build_over_call
	  (TREE_OPERAND (convs, 1), TREE_OPERAND (convs, 2),
	   args, LOOKUP_NORMAL);

	/* If this is a constructor or a function returning an aggr type,
	   we need to build up a TARGET_EXPR.  */
	if (DECL_CONSTRUCTOR_P (fn))
	  expr = build_cplus_new (TREE_TYPE (convs), expr);

	return expr;
      }
    case IDENTITY_CONV:
      if (type_unknown_p (expr))
	expr = instantiate_type (TREE_TYPE (convs), expr, 1);
      if (TREE_READONLY_DECL_P (expr))
	expr = decl_constant_value (expr);
      return expr;
    case AMBIG_CONV:
      /* Call build_user_type_conversion again for the error.  */
      return build_user_type_conversion
	(TREE_TYPE (convs), TREE_OPERAND (convs, 0), LOOKUP_NORMAL);
    };

  expr = convert_like (TREE_OPERAND (convs, 0), expr);
  if (expr == error_mark_node)
    return error_mark_node;

  switch (TREE_CODE (convs))
    {
    case RVALUE_CONV:
      if (! IS_AGGR_TYPE (TREE_TYPE (convs)))
	return expr;
      /* else fall through */
    case BASE_CONV:
      return build_user_type_conversion
	(TREE_TYPE (convs), expr, LOOKUP_NORMAL);
    case REF_BIND:
      return convert_to_reference
	(TREE_TYPE (convs), expr,
	 CONV_IMPLICIT, LOOKUP_NORMAL|LOOKUP_NO_CONVERSION,
	 error_mark_node);
    case LVALUE_CONV:
      return decay_conversion (expr);
    }
  return ocp_convert (TREE_TYPE (convs), expr, CONV_IMPLICIT,
		      LOOKUP_NORMAL|LOOKUP_NO_CONVERSION);
}

static tree
convert_default_arg (type, arg)
     tree type, arg;
{
  arg = break_out_target_exprs (arg);

  if (TREE_CODE (arg) == CONSTRUCTOR)
    {
      arg = digest_init (type, arg, 0);
      arg = convert_for_initialization (0, type, arg, LOOKUP_NORMAL,
					"default argument", 0, 0);
    }
  else
    {
      /* This could get clobbered by the following call.  */
      if (TREE_HAS_CONSTRUCTOR (arg))
	arg = copy_node (arg);

      arg = convert_for_initialization (0, type, arg, LOOKUP_NORMAL,
					"default argument", 0, 0);
#ifdef PROMOTE_PROTOTYPES
      if ((TREE_CODE (type) == INTEGER_TYPE
	   || TREE_CODE (type) == ENUMERAL_TYPE)
	  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
	arg = default_conversion (arg);
#endif
    }

  return arg;
}

static tree
build_over_call (fn, convs, args, flags)
     tree fn, convs, args;
     int flags;
{
  tree converted_args = NULL_TREE;
  tree parm = TYPE_ARG_TYPES (TREE_TYPE (fn));
  tree conv, arg, val;
  int i = 0;
  int is_method = 0;

  if (args && TREE_CODE (args) != TREE_LIST)
    args = build_scratch_list (NULL_TREE, args);
  arg = args;

  /* The implicit parameters to a constructor are not considered by overload
     resolution, and must be of the proper type.  */
  if (DECL_CONSTRUCTOR_P (fn))
    {
      converted_args = expr_tree_cons (NULL_TREE, TREE_VALUE (arg), converted_args);
      arg = TREE_CHAIN (arg);
      parm = TREE_CHAIN (parm);
      if (TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (fn)))
	{
	  converted_args = expr_tree_cons
	    (NULL_TREE, TREE_VALUE (arg), converted_args);
	  arg = TREE_CHAIN (arg);
	  parm = TREE_CHAIN (parm);
	}
    }      
  /* Bypass access control for 'this' parameter.  */
  else if (TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE)
    {
      tree parmtype = TREE_VALUE (parm);
      tree argtype = TREE_TYPE (TREE_VALUE (arg));
      if (ICS_BAD_FLAG (TREE_VEC_ELT (convs, i)))
	{
	  int dv = (TYPE_VOLATILE (TREE_TYPE (parmtype))
		    < TYPE_VOLATILE (TREE_TYPE (argtype)));
	  int dc = (TYPE_READONLY (TREE_TYPE (parmtype))
		    < TYPE_READONLY (TREE_TYPE (argtype)));
	  char *p = (dv && dc ? "const and volatile"
		              : dc ? "const" : dv ? "volatile" : "");

	  cp_pedwarn ("passing `%T' as `this' argument of `%#D' discards %s",
		      TREE_TYPE (argtype), fn, p);
	}
      converted_args = expr_tree_cons
	(NULL_TREE, convert_force (TREE_VALUE (parm), TREE_VALUE (arg), CONV_C_CAST),
	 converted_args);
      parm = TREE_CHAIN (parm);
      arg = TREE_CHAIN (arg);
      ++i;
      is_method = 1;
    }

  for (; arg && parm;
       parm = TREE_CHAIN (parm), arg = TREE_CHAIN (arg), ++i)
    {
      tree type = TREE_VALUE (parm);

      conv = TREE_VEC_ELT (convs, i);
      if (ICS_BAD_FLAG (conv))
	{
	  tree t = conv;
	  val = TREE_VALUE (arg);

	  for (; t; t = TREE_OPERAND (t, 0))
	    {
	      if (TREE_CODE (t) == USER_CONV
		  || TREE_CODE (t) == AMBIG_CONV)
		{
		  val = convert_like (t, val);
		  break;
		}
	      else if (TREE_CODE (t) == IDENTITY_CONV)
		break;
	    }
	  val = convert_for_initialization
	    (NULL_TREE, type, val, LOOKUP_NORMAL,
	     "argument passing", fn, i - is_method);
	}
      else
	val = convert_like (conv, TREE_VALUE (arg));

#ifdef PROMOTE_PROTOTYPES
      if ((TREE_CODE (type) == INTEGER_TYPE
	   || TREE_CODE (type) == ENUMERAL_TYPE)
	  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
	val = default_conversion (val);
#endif
      converted_args = expr_tree_cons (NULL_TREE, val, converted_args);
    }

  /* Default arguments */
  for (; parm && parm != void_list_node; parm = TREE_CHAIN (parm))
    {
      tree arg = TREE_PURPOSE (parm);

      if (DECL_TEMPLATE_INFO (fn))
	/* This came from a template.  Instantiate the default arg here,
	   not in tsubst.  */
	arg = tsubst_expr (arg, DECL_TI_ARGS (fn),
			   TREE_VEC_LENGTH (DECL_TI_ARGS (fn)), NULL_TREE);
      converted_args = expr_tree_cons
	(NULL_TREE, convert_default_arg (TREE_VALUE (parm), arg),
	 converted_args);
    }

  /* Ellipsis */
  for (; arg; arg = TREE_CHAIN (arg))
    {
      val = TREE_VALUE (arg);

      if (TREE_CODE (TREE_TYPE (val)) == REAL_TYPE
	  && (TYPE_PRECISION (TREE_TYPE (val))
	      < TYPE_PRECISION (double_type_node)))
	/* Convert `float' to `double'.  */
	val = cp_convert (double_type_node, val);
      else if (TYPE_LANG_SPECIFIC (TREE_TYPE (val))
	       && ! TYPE_HAS_TRIVIAL_INIT_REF (TREE_TYPE (val)))
	cp_warning ("cannot pass objects of type `%T' through `...'",
		    TREE_TYPE (val));
      else
	/* Convert `short' and `char' to full-size `int'.  */
	val = default_conversion (val);

      converted_args = expr_tree_cons (NULL_TREE, val, converted_args);
    }

  converted_args = nreverse (converted_args);

  /* Avoid actually calling copy constructors and copy assignment operators,
     if possible.  */
  if (DECL_CONSTRUCTOR_P (fn)
      && TREE_VEC_LENGTH (convs) == 1
      && copy_args_p (fn))
    {
      tree targ;
      arg = TREE_VALUE (TREE_CHAIN (converted_args));

      /* Pull out the real argument, disregarding const-correctness.  */
      targ = arg;
      while (TREE_CODE (targ) == NOP_EXPR
	     || TREE_CODE (targ) == NON_LVALUE_EXPR
	     || TREE_CODE (targ) == CONVERT_EXPR)
	targ = TREE_OPERAND (targ, 0);
      if (TREE_CODE (targ) == ADDR_EXPR)
	{
	  targ = TREE_OPERAND (targ, 0);
	  if (! comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (arg))),
			   TYPE_MAIN_VARIANT (TREE_TYPE (targ)), 1))
	    targ = NULL_TREE;
	}
      else
	targ = NULL_TREE;

      if (targ)
	arg = targ;
      else
	arg = build_indirect_ref (arg, 0);

      /* [class.copy]: the copy constructor is implicitly defined even if
	 the implementation elided its use.  */
      if (TYPE_HAS_COMPLEX_INIT_REF (DECL_CONTEXT (fn)))
	mark_used (fn);

      /* If we're creating a temp and we already have one, don't create a
         new one.  If we're not creating a temp but we get one, use
         INIT_EXPR to collapse the temp into our target.  Otherwise, if the
         ctor is trivial, do a bitwise copy with a simple TARGET_EXPR for a
         temp or an INIT_EXPR otherwise.  */
      if (integer_zerop (TREE_VALUE (args)))
	{
	  if (! real_lvalue_p (arg))
	    return arg;
	  else if (TYPE_HAS_TRIVIAL_INIT_REF (DECL_CONTEXT (fn)))
	    {
	      val = build (VAR_DECL, DECL_CONTEXT (fn));
	      layout_decl (val, 0);
	      val = build (TARGET_EXPR, DECL_CONTEXT (fn), val, arg, 0, 0);
	      TREE_SIDE_EFFECTS (val) = 1;
	      return val;
	    }
	}
      else if (! real_lvalue_p (arg)
	       || TYPE_HAS_TRIVIAL_INIT_REF (DECL_CONTEXT (fn)))
	{
	  tree to = stabilize_reference
	    (build_indirect_ref (TREE_VALUE (args), 0));
	  val = build (INIT_EXPR, DECL_CONTEXT (fn), to, arg);
	  TREE_SIDE_EFFECTS (val) = 1;
	  return build_unary_op (ADDR_EXPR, val, 0);
	}
    }
  else if (DECL_NAME (fn) == ansi_opname[MODIFY_EXPR]
	   && copy_args_p (fn)
	   && TYPE_HAS_TRIVIAL_ASSIGN_REF (DECL_CONTEXT (fn)))
    {
      tree to = stabilize_reference
	(build_indirect_ref (TREE_VALUE (converted_args), 0));
      arg = build_indirect_ref (TREE_VALUE (TREE_CHAIN (converted_args)), 0);
      val = build (MODIFY_EXPR, TREE_TYPE (to), to, arg);
      TREE_SIDE_EFFECTS (val) = 1;
      return val;
    }

  mark_used (fn);

  if (DECL_CONTEXT (fn) && IS_SIGNATURE (DECL_CONTEXT (fn)))
    return build_signature_method_call (fn, converted_args);
  else if (DECL_VINDEX (fn) && (flags & LOOKUP_NONVIRTUAL) == 0)
    {
      tree t, *p = &TREE_VALUE (converted_args);
      tree binfo = get_binfo
	(DECL_CONTEXT (fn), TREE_TYPE (TREE_TYPE (*p)), 0);
      *p = convert_pointer_to_real (binfo, *p);
      if (TREE_SIDE_EFFECTS (*p))
	*p = save_expr (*p);
      t = build_pointer_type (TREE_TYPE (fn));
      fn = build_vfn_ref (p, build_indirect_ref (*p, 0), DECL_VINDEX (fn));
      TREE_TYPE (fn) = t;
    }
  else if (DECL_INLINE (fn))
    fn = inline_conversion (fn);
  else
    fn = build_addr_func (fn);

  fn = build_call (fn, TREE_TYPE (TREE_TYPE (TREE_TYPE (fn))), converted_args);
  if (TREE_TYPE (fn) == void_type_node)
    return fn;
  fn = require_complete_type (fn);
  if (IS_AGGR_TYPE (TREE_TYPE (fn)))
    fn = build_cplus_new (TREE_TYPE (fn), fn);
  return convert_from_reference (fn);
}

static tree
build_new_method_call (instance, name, args, basetype_path, flags)
     tree instance, name, args, basetype_path;
     int flags;
{
  struct z_candidate *candidates = 0, *cand;
  tree explicit_targs = NULL_TREE;
  tree basetype, mem_args, fns, instance_ptr;
  tree pretty_name;
  tree user_args = args;
  tree templates = NULL_TREE;
  int template_only = 0;

  if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
    {
      explicit_targs = TREE_OPERAND (name, 1);
      name = TREE_OPERAND (name, 0);
      template_only = 1;
    }

  /* If there is an extra argument for controlling virtual bases,
     remove it for error reporting.  */
  if (flags & LOOKUP_HAS_IN_CHARGE)
    user_args = TREE_CHAIN (args);

  args = resolve_args (args);

  if (args == error_mark_node)
    return error_mark_node;

  if (instance == NULL_TREE)
    basetype = BINFO_TYPE (basetype_path);
  else
    {
      if (TREE_CODE (instance) == OFFSET_REF)
	instance = resolve_offset_ref (instance);
      if (TREE_CODE (TREE_TYPE (instance)) == REFERENCE_TYPE)
	instance = convert_from_reference (instance);
      basetype = TREE_TYPE (instance);

      /* XXX this should be handled before we get here.  */
      if (! IS_AGGR_TYPE (basetype)
	  && ! (TYPE_LANG_SPECIFIC (basetype)
		&& (IS_SIGNATURE_POINTER (basetype)
		    || IS_SIGNATURE_REFERENCE (basetype))))
	{
	  if ((flags & LOOKUP_COMPLAIN) && basetype != error_mark_node)
	    cp_error ("request for member `%D' in `%E', which is of non-aggregate type `%T'",
		      name, instance, basetype);

	  return error_mark_node;
	}

      /* If `instance' is a signature pointer/reference and `name' is
	 not a constructor, we are calling a signature member function.
	 In that case set the `basetype' to the signature type.  */
      if ((IS_SIGNATURE_POINTER (basetype)
	   || IS_SIGNATURE_REFERENCE (basetype))
	  && TYPE_IDENTIFIER (basetype) != name)
	basetype = SIGNATURE_TYPE (basetype);
    }

  if (basetype_path == NULL_TREE)
    basetype_path = TYPE_BINFO (basetype);

  if (instance)
    {
      instance_ptr = build_this (instance);

      /* XXX this should be handled before we get here.  */
      fns = build_field_call (basetype_path, instance_ptr, name, args);
      if (fns)
	return fns;
    }
  else
    {
      instance_ptr = build_int_2 (0, 0);
      TREE_TYPE (instance_ptr) = build_pointer_type (basetype);
    }

  pretty_name
    = (name == ctor_identifier ? constructor_name (basetype) : name);

  fns = lookup_fnfields (basetype_path, name, 1);

  if (fns == error_mark_node)
    return error_mark_node;
  if (fns)
    {
      tree t = TREE_VALUE (fns);
      if (name == ctor_identifier && TYPE_USES_VIRTUAL_BASECLASSES (basetype)
	  && ! (flags & LOOKUP_HAS_IN_CHARGE))
	{
	  flags |= LOOKUP_HAS_IN_CHARGE;
	  args = scratch_tree_cons (NULL_TREE, integer_one_node, args);
	}
      mem_args = scratch_tree_cons (NULL_TREE, instance_ptr, args);
      for (; t; t = DECL_CHAIN (t))
	{
	  tree this_arglist;

	  /* We can end up here for copy-init of same or base class.  */
	  if (name == ctor_identifier
	      && (flags & LOOKUP_ONLYCONVERTING)
	      && DECL_NONCONVERTING_P (t))
	    continue;
	  if (TREE_CODE (TREE_TYPE (t)) == METHOD_TYPE)
	    this_arglist = mem_args;
	  else
	    this_arglist = args;

	  if (TREE_CODE (t) == TEMPLATE_DECL)
	    {
	      /* A member template. */
	      templates = scratch_tree_cons (NULL_TREE, t, templates);
	      candidates = 
		add_template_candidate (candidates, t, explicit_targs,
					this_arglist,
					TREE_TYPE (name), 
					LOOKUP_NORMAL); 
	    }
	  else if (! template_only)
	    candidates = add_function_candidate (candidates, t,
						 this_arglist, flags);

	  if (candidates)
	    candidates->basetype_path = TREE_PURPOSE (fns);
	}
    }

  if (! any_viable (candidates))
    {
      /* XXX will LOOKUP_SPECULATIVELY be needed when this is done?  */
      if (flags & LOOKUP_SPECULATIVELY)
	return NULL_TREE;
      cp_error ("no matching function for call to `%T::%D (%A)%V'", basetype,
		pretty_name, user_args, TREE_TYPE (TREE_TYPE (instance_ptr)));
      print_z_candidates (candidates);
      return error_mark_node;
    }
  candidates = splice_viable (candidates);
  cand = tourney (candidates);

  if (cand == 0)
    {
      cp_error ("call of overloaded `%D(%A)' is ambiguous", pretty_name,
		user_args);
      print_z_candidates (candidates);
      return error_mark_node;
    }

  enforce_access (cand->basetype_path, cand->fn);
  if (DECL_ABSTRACT_VIRTUAL_P (cand->fn)
      && instance == current_class_ref
      && DECL_CONSTRUCTOR_P (current_function_decl)
      && ! (flags & LOOKUP_NONVIRTUAL)
      && value_member (cand->fn, get_abstract_virtuals (basetype)))
    cp_error ("abstract virtual `%#D' called from constructor", cand->fn);
  if (TREE_CODE (TREE_TYPE (cand->fn)) == METHOD_TYPE
      && TREE_CODE (instance_ptr) == NOP_EXPR
      && TREE_OPERAND (instance_ptr, 0) == error_mark_node)
    cp_error ("cannot call member function `%D' without object", cand->fn);

  if (DECL_VINDEX (cand->fn) && ! (flags & LOOKUP_NONVIRTUAL)
      && ((instance == current_class_ref && (dtor_label || ctor_label))
	  || resolves_to_fixed_type_p (instance, 0)))
    flags |= LOOKUP_NONVIRTUAL;

  /* Pedantically, normal function declarations are never considered
     to refer to template instantiations, so we only do this with
     -fguiding-decls.  */ 
  if (flag_guiding_decls && templates && ! cand->template 
      && ! DECL_INITIAL (cand->fn))
    add_maybe_template (cand->fn, templates);

  return build_over_call
    (cand->fn, cand->convs,
     TREE_CODE (TREE_TYPE (cand->fn)) == METHOD_TYPE ? mem_args : args,
     flags);
}

/* Compare two implicit conversion sequences that differ only in their
   qualification conversion.  Subroutine of compare_ics.  */

static int
compare_qual (ics1, ics2)
     tree ics1, ics2;
{
  tree to1 = TREE_TYPE (ics1);
  tree to2 = TREE_TYPE (ics2);

  if (TYPE_PTRMEMFUNC_P (to1))
    to1 = TYPE_PTRMEMFUNC_FN_TYPE (to1);
  if (TYPE_PTRMEMFUNC_P (to2))
    to2 = TYPE_PTRMEMFUNC_FN_TYPE (to2);

  to1 = TREE_TYPE (to1);
  to2 = TREE_TYPE (to2);

  if (TREE_CODE (to1) == OFFSET_TYPE)
    {
      to1 = TREE_TYPE (to1);
      to2 = TREE_TYPE (to2);
    }

  if (TYPE_READONLY (to1) >= TYPE_READONLY (to2)
      && TYPE_VOLATILE (to1) > TYPE_VOLATILE (to2))
    return -1;
  else if (TYPE_READONLY (to1) > TYPE_READONLY (to2)
	   && TYPE_VOLATILE (to1) == TYPE_VOLATILE (to2))
    return -1;
  else if (TYPE_READONLY (to1) <= TYPE_READONLY (to2)
	   && TYPE_VOLATILE (to1) < TYPE_VOLATILE (to2))
    return 1;
  else if (TYPE_READONLY (to1) < TYPE_READONLY (to2)
	   && TYPE_VOLATILE (to1) == TYPE_VOLATILE (to2))
    return 1;
  return 0;
}

/* Determine whether standard conversion sequence ICS1 is a proper
   subsequence of ICS2.  We assume that a conversion of the same code
   between the same types indicates a subsequence.  */

static int
is_subseq (ics1, ics2)
     tree ics1, ics2;
{
  /* Do not consider lvalue transformations here.  */
  if (TREE_CODE (ics2) == RVALUE_CONV
      || TREE_CODE (ics2) == LVALUE_CONV)
    return 0;

  for (;; ics2 = TREE_OPERAND (ics2, 0))
    {
      if (TREE_CODE (ics2) == TREE_CODE (ics1)
	  && comptypes (TREE_TYPE (ics2), TREE_TYPE (ics1), 1)
	  && comptypes (TREE_TYPE (TREE_OPERAND (ics2, 0)),
			TREE_TYPE (TREE_OPERAND (ics1, 0)), 1))
	return 1;

      if (TREE_CODE (ics2) == USER_CONV
	  || TREE_CODE (ics2) == AMBIG_CONV
	  || TREE_CODE (ics2) == IDENTITY_CONV)
	return 0;
    }
}

/* Compare two implicit conversion sequences according to the rules set out in
   [over.ics.rank].  Return values:

      1: ics1 is better than ics2
     -1: ics2 is better than ics1
      0: ics1 and ics2 are indistinguishable */

static int
compare_ics (ics1, ics2)
     tree ics1, ics2;
{
  tree main1, main2;

  if (TREE_CODE (ics1) == QUAL_CONV)
    main1 = TREE_OPERAND (ics1, 0);
  else
    main1 = ics1;

  if (TREE_CODE (ics2) == QUAL_CONV)
    main2 = TREE_OPERAND (ics2, 0);
  else
    main2 = ics2;

  /* Conversions for `this' are PTR_CONVs, but we compare them as though
     they were REF_BINDs.  */
  if (ICS_THIS_FLAG (ics1))
    {
      tree t = main1;
      if (TREE_CODE (t) == PTR_CONV)
	t = TREE_OPERAND (t, 0);
      t = build1 (IDENTITY_CONV, TREE_TYPE (TREE_TYPE (t)), NULL_TREE);
      t = build_conv (REF_BIND, TREE_TYPE (ics1), t);
      ICS_STD_RANK (t) = ICS_STD_RANK (main1);
      main1 = ics1 = t;
    }
  if (ICS_THIS_FLAG (ics2))
    {
      tree t = main2;
      if (TREE_CODE (t) == PTR_CONV)
	t = TREE_OPERAND (t, 0);
      t = build1 (IDENTITY_CONV, TREE_TYPE (TREE_TYPE (t)), NULL_TREE);
      t = build_conv (REF_BIND, TREE_TYPE (ics2), t);
      ICS_STD_RANK (t) = ICS_STD_RANK (main2);
      main2 = ics2 = t;
    }

  if (ICS_RANK (ics1) > ICS_RANK (ics2))
    return -1;
  else if (ICS_RANK (ics1) < ICS_RANK (ics2))
    return 1;

  if (ICS_RANK (ics1) == BAD_RANK)
    {
      if (ICS_USER_FLAG (ics1) > ICS_USER_FLAG (ics2)
	  || ICS_STD_RANK (ics1) > ICS_STD_RANK (ics2))
	return -1;
      else if (ICS_USER_FLAG (ics1) < ICS_USER_FLAG (ics2)
	       || ICS_STD_RANK (ics1) < ICS_STD_RANK (ics2))
	return 1;

      /* else fall through */
    }

  /* User-defined  conversion sequence U1 is a better conversion sequence
     than another user-defined conversion sequence U2 if they contain the
     same user-defined conversion operator or constructor and if the sec-
     ond standard conversion sequence of U1 is  better  than  the  second
     standard conversion sequence of U2.  */

  if (ICS_USER_FLAG (ics1))
    {
      tree t1, t2;

      for (t1 = ics1; TREE_CODE (t1) != USER_CONV; t1 = TREE_OPERAND (t1, 0))
	if (TREE_CODE (t1) == AMBIG_CONV)
	  return 0;
      for (t2 = ics2; TREE_CODE (t2) != USER_CONV; t2 = TREE_OPERAND (t2, 0))
	if (TREE_CODE (t2) == AMBIG_CONV)
	  return 0;

      if (USER_CONV_FN (t1) != USER_CONV_FN (t2))
	return 0;
      else if (ICS_STD_RANK (ics1) > ICS_STD_RANK (ics2))
	return -1;
      else if (ICS_STD_RANK (ics1) < ICS_STD_RANK (ics2))
	return 1;

      /* else fall through */
    }

#if 0 /* Handled by ranking */
  /* A conversion that is not a conversion of a pointer,  or  pointer  to
     member,  to  bool  is  better than another conversion that is such a
     conversion.  */
#endif

  if (TREE_CODE (main1) != TREE_CODE (main2))
    {
      /* ...if S1  is  a  proper  subsequence  of  S2  */
      if (is_subseq (main1, main2))
	return 1;
      if (is_subseq (main2, main1))
	return -1;
      return 0;
    }

  if (TREE_CODE (main1) == PTR_CONV || TREE_CODE (main1) == PMEM_CONV
      || TREE_CODE (main1) == REF_BIND || TREE_CODE (main1) == BASE_CONV)
    {
      tree to1 = TREE_TYPE (main1);
      tree from1 = TREE_TYPE (TREE_OPERAND (main1, 0));
      tree to2 = TREE_TYPE (main2);
      tree from2 = TREE_TYPE (TREE_OPERAND (main2, 0));
      int distf, distt;

      /* Standard conversion sequence S1 is a better conversion sequence than
	 standard conversion sequence S2 if...

	 S1 and S2 differ only in their qualification conversion  and  they
	 yield types identical except for cv-qualifiers and S2 adds all the
	 qualifiers that S1 adds (and in the same places) and S2  adds  yet
	 more  cv-qualifiers  than  S1,  or the similar case with reference
	 binding15).  */
      if (TREE_CODE (main1) == REF_BIND)
	{
	  if (TYPE_MAIN_VARIANT (TREE_TYPE (to1))
	      == TYPE_MAIN_VARIANT (TREE_TYPE (to2)))
	    return compare_qual (ics1, ics2);
	}
      else if (TREE_CODE (main1) != BASE_CONV && from1 == from2 && to1 == to2)
	return compare_qual (ics1, ics2);
	
      if (TYPE_PTRMEMFUNC_P (to1))
	{
	  to1 = TYPE_METHOD_BASETYPE (TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (to1)));
	  from1 = TYPE_METHOD_BASETYPE (TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (from1)));
	}
      else if (TREE_CODE (main1) != BASE_CONV)
	{
	  to1 = TREE_TYPE (to1);
	  if (TREE_CODE (main1) != REF_BIND)
	    from1 = TREE_TYPE (from1);

	  if (TREE_CODE (to1) == OFFSET_TYPE)
	    {
	      to1 = TYPE_OFFSET_BASETYPE (to1);
	      from1 = TYPE_OFFSET_BASETYPE (from1);
	    }
	}

      if (TYPE_PTRMEMFUNC_P (to2))
	{
	  to2 = TYPE_METHOD_BASETYPE (TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (to2)));
	  from2 = TYPE_METHOD_BASETYPE (TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (from2)));
	}
      else if (TREE_CODE (main1) != BASE_CONV)
	{
	  to2 = TREE_TYPE (to2);
	  if (TREE_CODE (main1) != REF_BIND)
	    from2 = TREE_TYPE (from2);

	  if (TREE_CODE (to2) == OFFSET_TYPE)
	    {
	      to2 = TYPE_OFFSET_BASETYPE (to2);
	      from2 = TYPE_OFFSET_BASETYPE (from2);
	    }
	}

      if (! (IS_AGGR_TYPE (from1) && IS_AGGR_TYPE (from2)))
	return 0;

      /* The sense of pmem conversions is reversed from that of the other
	 conversions.  */
      if (TREE_CODE (main1) == PMEM_CONV)
	{
	  tree t = from1; from1 = from2; from2 = t;
	  t = to1; to1 = to2; to2 = t;
	}

      distf = get_base_distance (from1, from2, 0, 0);
      if (distf == -1)
	{
	  distf = -get_base_distance (from2, from1, 0, 0);
	  if (distf == 1)
	    return 0;
	}

      /* If class B is derived directly or indirectly from class A,
	 conver- sion of B* to A* is better than conversion of B* to
	 void*, and conversion of A* to void* is better than
	 conversion of B* to void*.  */

      if (TREE_CODE (to1) == VOID_TYPE && TREE_CODE (to2) == VOID_TYPE)
	{
	  if (distf > 0)
	    return 1;
	  else if (distf < 0)
	    return -1;
	}
      else if (TREE_CODE (to2) == VOID_TYPE && IS_AGGR_TYPE (to1)
	       && get_base_distance (to1, from1, 0, 0) != -1)
	return 1;
      else if (TREE_CODE (to1) == VOID_TYPE && IS_AGGR_TYPE (to2)
	       && get_base_distance (to2, from2, 0, 0) != -1)
	return -1;

      if (! (IS_AGGR_TYPE (to1) && IS_AGGR_TYPE (to2)))
	return 0;

      /* If  class B is derived directly or indirectly from class A and class
	 C is derived directly or indirectly from B */

      distt = get_base_distance (to1, to2, 0, 0);
      if (distt == -1)
	{
	  distt = -get_base_distance (to2, to1, 0, 0);
	  if (distt == 1)
	    return 0;
	}

      /* --conversion of C* to B* is better than conversion of C* to A*, */
      if (distf == 0)
	{
	  if (distt > 0)
	    return -1;
	  else if (distt < 0)
	    return 1;
	}
      /* --conversion of B* to A* is better than conversion of C* to A*, */
      else if (distt == 0)
	{
	  if (distf > 0)
	    return 1;
	  else if (distf < 0)
	    return -1;
	}
    }
  else if (TREE_CODE (TREE_TYPE (main1)) == POINTER_TYPE
	   || TYPE_PTRMEMFUNC_P (TREE_TYPE (main1)))
    {
      if (TREE_TYPE (main1) == TREE_TYPE (main2))
	return compare_qual (ics1, ics2);

#if 0 /* This is now handled by making identity better than anything else.  */
      /* existing practice, not WP-endorsed: const char * -> const char *
	 is better than char * -> const char *.  (jason 6/29/96) */
      if (TREE_TYPE (ics1) == TREE_TYPE (ics2))
	return -compare_qual (main1, main2);
#endif
    }

  return 0;
}

/* The source type for this standard conversion sequence.  */

static tree
source_type (t)
     tree t;
{
  for (;; t = TREE_OPERAND (t, 0))
    {
      if (TREE_CODE (t) == USER_CONV
	  || TREE_CODE (t) == AMBIG_CONV
	  || TREE_CODE (t) == IDENTITY_CONV)
	return TREE_TYPE (t);
    }
  my_friendly_abort (1823);
}

/* Compare two candidates for overloading as described in
   [over.match.best].  Return values:

      1: cand1 is better than cand2
     -1: cand2 is better than cand1
      0: cand1 and cand2 are indistinguishable */

static int
joust (cand1, cand2)
     struct z_candidate *cand1, *cand2;
{
  int winner = 0;
  int i, off1 = 0, off2 = 0, len;

  /* Candidates that involve bad conversions are always worse than those
     that don't.  */
  if (cand1->viable > cand2->viable)
    return 1;
  if (cand1->viable < cand2->viable)
    return -1;

  /* a viable function F1
     is defined to be a better function than another viable function F2  if
     for  all arguments i, ICSi(F1) is not a worse conversion sequence than
     ICSi(F2), and then */

  /* for some argument j, ICSj(F1) is a better conversion  sequence  than
     ICSj(F2) */

  /* For comparing static and non-static member functions, we ignore the
     implicit object parameter of the non-static function.  The WP says to
     pretend that the static function has an object parm, but that won't
     work with operator overloading.  */
  len = TREE_VEC_LENGTH (cand1->convs);
  if (len != TREE_VEC_LENGTH (cand2->convs))
    {
      if (DECL_STATIC_FUNCTION_P (cand1->fn)
	  && ! DECL_STATIC_FUNCTION_P (cand2->fn))
	off2 = 1;
      else if (! DECL_STATIC_FUNCTION_P (cand1->fn)
	       && DECL_STATIC_FUNCTION_P (cand2->fn))
	{
	  off1 = 1;
	  --len;
	}
      else
	my_friendly_abort (42);
    }

  for (i = 0; i < len; ++i)
    {
      tree t1 = TREE_VEC_ELT (cand1->convs, i+off1);
      tree t2 = TREE_VEC_ELT (cand2->convs, i+off2);
      int comp = compare_ics (t1, t2);

      if (comp != 0)
	{
#if 0 /* move this warning to tourney.  */
	  if (warn_sign_promo
	      && ICS_RANK (t1) + ICS_RANK (t2) == STD_RANK + PROMO_RANK
	      && TREE_CODE (t1) == STD_CONV
	      && TREE_CODE (t2) == STD_CONV
	      && TREE_CODE (TREE_TYPE (t1)) == INTEGER_TYPE
	      && TREE_CODE (TREE_TYPE (t2)) == INTEGER_TYPE
	      && (TYPE_PRECISION (TREE_TYPE (t1))
		  == TYPE_PRECISION (TREE_TYPE (t2)))
	      && (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (t1, 0)))
		  || (TREE_CODE (TREE_TYPE (TREE_OPERAND (t1, 0)))
		      == ENUMERAL_TYPE)))
	    {
	      tree type = TREE_TYPE (TREE_OPERAND (t1, 0));
	      tree type1, type2;
	      if (comp > 0)
		type1 = TREE_TYPE (t1), type2 = TREE_TYPE (t2);
	      else
		type1 = TREE_TYPE (t2), type2 = TREE_TYPE (t1);

	      cp_warning ("passing `%T' chooses `%T' over `%T'",
			  type, type1, type2);
	      cp_warning ("  in call to `%D'", DECL_NAME (cand1->fn));
	    }
#endif

	  if (winner && comp != winner)
	    {
	      winner = 0;
	      goto tweak;
	    }
	  winner = comp;
	}
    }

#if 0 /* move this warning to tourney.  */
  /* warn about confusing overload resolution */
  if (winner && cand1->second_conv
      && ! DECL_CONSTRUCTOR_P (cand1->fn)
      && ! DECL_CONSTRUCTOR_P (cand2->fn))
    {
      int comp = compare_ics (cand1->second_conv, cand2->second_conv);
      if (comp && comp != winner)
	{
	  struct z_candidate *w, *l;
	  if (winner == 1)
	    w = cand1, l = cand2;
	  else
	    w = cand2, l = cand1;
	  cp_warning ("choosing `%D' over `%D'", w->fn, l->fn);
	  cp_warning ("  for conversion from `%T' to `%T'",
		      TREE_TYPE (source_type (TREE_VEC_ELT (w->convs, 0))),
		      TREE_TYPE (w->second_conv));
	  cp_warning ("  because conversion sequence for `this' argument is better");
	}
    }
#endif

  if (winner)
    return winner;

  /* or, if not that,
     F1 is a non-template function and F2 is a template function */

  if (! cand1->template && cand2->template)
    return 1;
  else if (cand1->template && ! cand2->template)
    return -1;
  else if (cand1->template && cand2->template)
    winner = more_specialized
      (TI_TEMPLATE (cand1->template), TI_TEMPLATE (cand2->template));

  /* or, if not that,
     the  context  is  an  initialization by user-defined conversion (see
     _dcl.init_  and  _over.match.user_)  and  the  standard   conversion
     sequence  from  the return type of F1 to the destination type (i.e.,
     the type of the entity being initialized)  is  a  better  conversion
     sequence  than the standard conversion sequence from the return type
     of F2 to the destination type.  */

  if (! winner && cand1->second_conv)
    winner = compare_ics (cand1->second_conv, cand2->second_conv);

  /* If the built-in candidates are the same, arbitrarily pick one.  */
  if (! winner && cand1->fn == cand2->fn
      && TREE_CODE (cand1->fn) == IDENTIFIER_NODE)
    {
      for (i = 0; i < len; ++i)
	if (! comptypes (TREE_TYPE (TREE_VEC_ELT (cand1->convs, i)),
			 TREE_TYPE (TREE_VEC_ELT (cand2->convs, i)), 1))
	  break;
      if (i == TREE_VEC_LENGTH (cand1->convs))
	return 1;

      /* Kludge around broken overloading rules whereby
	 Integer a, b; test ? a : b; is ambiguous, since there's a builtin
	 that takes references and another that takes values.  */
      if (cand1->fn == ansi_opname[COND_EXPR])
	{
	  tree c1 = TREE_VEC_ELT (cand1->convs, 1);
	  tree c2 = TREE_VEC_ELT (cand2->convs, 1);
	  tree t1 = strip_top_quals (non_reference (TREE_TYPE (c1)));
	  tree t2 = strip_top_quals (non_reference (TREE_TYPE (c2)));

	  if (comptypes (t1, t2, 1))
	    {
	      if (TREE_CODE (c1) == REF_BIND && TREE_CODE (c2) != REF_BIND)
		return 1;
	      if (TREE_CODE (c1) != REF_BIND && TREE_CODE (c2) == REF_BIND)
		return -1;
	    }
	}
    }

tweak:

  /* Extension: If the worst conversion for one candidate is worse than the
     worst conversion for the other, take the first.  */
  if (! winner && ! pedantic)
    {
      int rank1 = IDENTITY_RANK, rank2 = IDENTITY_RANK;

      for (i = 0; i < len; ++i)
	{
	  if (ICS_RANK (TREE_VEC_ELT (cand1->convs, i+off1)) > rank1)
	    rank1 = ICS_RANK (TREE_VEC_ELT (cand1->convs, i+off1));
	  if (ICS_RANK (TREE_VEC_ELT (cand2->convs, i+off2)) > rank2)
	    rank2 = ICS_RANK (TREE_VEC_ELT (cand2->convs, i+off2));
	}

      if (rank1 < rank2)
	return 1;
      if (rank1 > rank2)
	return -1;
    }

  return winner;
}

/* Given a list of candidates for overloading, find the best one, if any.
   This algorithm has a worst case of O(2n) (winner is last), and a best
   case of O(n/2) (totally ambiguous); much better than a sorting
   algorithm.  */

static struct z_candidate *
tourney (candidates)
     struct z_candidate *candidates;
{
  struct z_candidate *champ = candidates, *challenger;
  int fate;

  /* Walk through the list once, comparing each current champ to the next
     candidate, knocking out a candidate or two with each comparison.  */

  for (challenger = champ->next; challenger; )
    {
      fate = joust (champ, challenger);
      if (fate == 1)
	challenger = challenger->next;
      else
	{
	  if (fate == 0)
	    {
	      champ = challenger->next;
	      if (champ == 0)
		return 0;
	    }
	  else
	    champ = challenger;

	  challenger = champ->next;
	}
    }

  /* Make sure the champ is better than all the candidates it hasn't yet
     been compared to.  This may do one more comparison than necessary.  Oh
     well.  */

  for (challenger = candidates; challenger != champ;
       challenger = challenger->next)
    {
      fate = joust (champ, challenger);
      if (fate != 1)
	return 0;
    }

  return champ;
}

int
can_convert (to, from)
     tree to, from;
{
  if (flag_ansi_overloading)
    {
      tree t = implicit_conversion (to, from, NULL_TREE, LOOKUP_NORMAL);
      return (t && ! ICS_BAD_FLAG (t));
    }
  else
    {
      struct harshness_code h;
      h = convert_harshness (to, from, NULL_TREE);
      return (h.code < USER_CODE) && (h.distance >= 0);
    }
}

int
can_convert_arg (to, from, arg)
     tree to, from, arg;
{
  if (flag_ansi_overloading)
    {
      tree t = implicit_conversion (to, from, arg, LOOKUP_NORMAL);
      return (t && ! ICS_BAD_FLAG (t));
    }
  else
    {
      struct harshness_code h;
      h = convert_harshness (to, from, arg);
      return (h.code < USER_CODE) && (h.distance >= 0);
    }
}
