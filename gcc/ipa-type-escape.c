/* Type based alias analysis.
   Copyright (C) 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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

/* This pass determines which types in the program contain only
   instances that are completely encapsulated by the compilation unit.
   Those types that are encapsulated must also pass the further
   requirement that there be no bad operations on any instances of
   those types.

   A great deal of freedom in compilation is allowed for the instances
   of those types that pass these conditions.
*/

/* The code in this module is called by the ipa pass manager. It
   should be one of the later passes since its information is used by
   the rest of the compilation. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "ggc.h"
#include "ipa-utils.h"
#include "ipa-type-escape.h"
#include "c-common.h"
#include "tree-gimple.h"
#include "cgraph.h"
#include "output.h"
#include "flags.h"
#include "timevar.h"
#include "diagnostic.h"
#include "langhooks.h"

/* Some of the aliasing is called very early, before this phase is
   called.  To assure that this is not a problem, we keep track of if
   this phase has been run.  */
static bool initialized = false;

/* Scratch bitmap for avoiding work. */
static bitmap been_there_done_that;
static bitmap bitmap_tmp;

/* There are two levels of escape that types can undergo.

   EXPOSED_PARAMETER - some instance of the variable is
   passed by value into an externally visible function or some
   instance of the variable is passed out of an externally visible
   function as a return value.  In this case any of the fields of the
   variable that are pointer types end up having their types marked as
   FULL_ESCAPE.

   FULL_ESCAPE - when bad things happen to good types. One of the
   following things happens to the type: (a) either an instance of the
   variable has its address passed to an externally visible function,
   (b) the address is taken and some bad cast happens to the address
   or (c) explicit arithmetic is done to the address.
*/

enum escape_t
{
  EXPOSED_PARAMETER,
  FULL_ESCAPE
};

/* The following two bit vectors global_types_* correspond to
   previous cases above.  During the analysis phase, a bit is set in
   one of these vectors if an operation of the offending class is
   discovered to happen on the associated type.  */
 
static bitmap global_types_exposed_parameter;
static bitmap global_types_full_escape;

/* All of the types seen in this compilation unit. */
static bitmap global_types_seen;
/* Reverse map to take a canon uid and map it to a canon type.  Uid's
   are never manipulated unless they are associated with a canon
   type.  */
static splay_tree uid_to_canon_type;

/* Internal structure of type mapping code.  This maps a canon type
   name to its canon type.  */
static splay_tree all_canon_types;

/* Map from type clones to the single canon type.  */
static splay_tree type_to_canon_type;

/* A splay tree of bitmaps.  An element X in the splay tree has a bit
   set in its bitmap at TYPE_UID (TYPE_MAIN_VARIANT (Y)) if there was
   an operation in the program of the form "&X.Y".  */
static splay_tree uid_to_addressof_down_map;

/* A splay tree of bitmaps.  An element Y in the splay tree has a bit
   set in its bitmap at TYPE_UID (TYPE_MAIN_VARIANT (X)) if there was
   an operation in the program of the form "&X.Y".  */
static splay_tree uid_to_addressof_up_map;

/* Tree to hold the subtype maps used to mark subtypes of escaped
   types.  */
static splay_tree uid_to_subtype_map;

/* Records tree nodes seen in cgraph_create_edges.  Simply using
   walk_tree_without_duplicates doesn't guarantee each node is visited
   once because it gets a new htab upon each recursive call from
   scan_for_refs.  */
static struct pointer_set_t *visited_nodes;

/* Visited stmts by walk_use_def_chains function because it's called
   recursively.  */
static struct pointer_set_t *visited_stmts;

static bitmap_obstack ipa_obstack;

/* Static functions from this file that are used 
   before being defined.  */
static unsigned int look_for_casts (tree lhs ATTRIBUTE_UNUSED, tree);
static bool is_cast_from_non_pointer (tree, tree, void *);

/* Get the name of TYPE or return the string "<UNNAMED>".  */
static const char*
get_name_of_type (tree type)
{
  tree name = TYPE_NAME (type);
  
  if (!name)
    /* Unnamed type, do what you like here.  */
    return "<UNNAMED>";
  
  /* It will be a TYPE_DECL in the case of a typedef, otherwise, an
     identifier_node */
  if (TREE_CODE (name) == TYPE_DECL)
    {
      /*  Each DECL has a DECL_NAME field which contains an
	  IDENTIFIER_NODE.  (Some decls, most often labels, may have
	  zero as the DECL_NAME).  */
      if (DECL_NAME (name))
	return IDENTIFIER_POINTER (DECL_NAME (name));
      else
	/* Unnamed type, do what you like here.  */
	return "<UNNAMED>";
    }
  else if (TREE_CODE (name) == IDENTIFIER_NODE)
    return IDENTIFIER_POINTER (name);
  else 
    return "<UNNAMED>";
}

struct type_brand_s 
{
  const char* name;
  int seq;
};

/* Splay tree comparison function on type_brand_s structures.  */

static int 
compare_type_brand (splay_tree_key sk1, splay_tree_key sk2)
{
  struct type_brand_s * k1 = (struct type_brand_s *) sk1;
  struct type_brand_s * k2 = (struct type_brand_s *) sk2;

  int value = strcmp(k1->name, k2->name);
  if (value == 0)
    return k2->seq - k1->seq;
  else 
    return value;
}

/* All of the "unique_type" code is a hack to get around the sleazy
   implementation used to compile more than file.  Currently gcc does
   not get rid of multiple instances of the same type that have been
   collected from different compilation units.  */
/* This is a trivial algorithm for removing duplicate types.  This
   would not work for any language that used structural equivalence as
   the basis of its type system.  */
/* Return TYPE if no type compatible with TYPE has been seen so far,
   otherwise return a type compatible with TYPE that has already been
   processed.  */

static tree
discover_unique_type (tree type)
{
  struct type_brand_s * brand = XNEW (struct type_brand_s);
  int i = 0;
  splay_tree_node result;

  brand->name = get_name_of_type (type);

  while (1)
    {
      brand->seq = i++;
      result = splay_tree_lookup (all_canon_types, (splay_tree_key) brand);

      if (result)
	{
	  /* Create an alias since this is just the same as
	     other_type.  */
	  tree other_type = (tree) result->value;
	  if (types_compatible_p (type, other_type))
	    {
	      free (brand);
	      /* Insert this new type as an alias for other_type.  */
	      splay_tree_insert (type_to_canon_type,
				 (splay_tree_key) type,
				 (splay_tree_value) other_type);
	      return other_type;
	    }
	  /* Not compatible, look for next instance with same name.  */
	}
      else
	{
	  /* No more instances, create new one since this is the first
	     time we saw this type.  */
	  brand->seq = i++;
	  /* Insert the new brand.  */
	  splay_tree_insert (all_canon_types,
			     (splay_tree_key) brand,
			     (splay_tree_value) type);

	  /* Insert this new type as an alias for itself.  */
	  splay_tree_insert (type_to_canon_type,
			     (splay_tree_key) type,
			     (splay_tree_value) type);

	  /* Insert the uid for reverse lookup; */
	  splay_tree_insert (uid_to_canon_type,
			     (splay_tree_key) TYPE_UID (type),
			     (splay_tree_value) type);

	  bitmap_set_bit (global_types_seen, TYPE_UID (type));
	  return type;
	}
    }
}

/* Return true if TYPE is one of the type classes that we are willing
   to analyze.  This skips the goofy types like arrays of pointers to
   methods. */
static bool
type_to_consider (tree type)
{
  /* Strip the *'s off.  */
  type = TYPE_MAIN_VARIANT (type);
  while (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE)
    type = TYPE_MAIN_VARIANT (TREE_TYPE (type));

  switch (TREE_CODE (type))
    {
    case BOOLEAN_TYPE:
    case COMPLEX_TYPE:
    case ENUMERAL_TYPE:
    case INTEGER_TYPE:
    case QUAL_UNION_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case RECORD_TYPE:
    case UNION_TYPE:
    case VECTOR_TYPE:
    case VOID_TYPE:
      return true;
  
    default:
      return false;
    }
}

/* Get the canon type of TYPE.  If SEE_THRU_PTRS is true, remove all
   the POINTER_TOs and if SEE_THRU_ARRAYS is true, remove all of the
   ARRAY_OFs and POINTER_TOs.  */

static tree 
get_canon_type (tree type, bool see_thru_ptrs, bool see_thru_arrays)
{
  splay_tree_node result;
  /* Strip the *'s off.  */
  if (!type || !type_to_consider (type))
    return NULL;

  type = TYPE_MAIN_VARIANT (type);
  if (see_thru_arrays) 
    while (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE)
      type = TYPE_MAIN_VARIANT (TREE_TYPE (type));

  else if (see_thru_ptrs) 
    while (POINTER_TYPE_P (type))
	type = TYPE_MAIN_VARIANT (TREE_TYPE (type));

  result = splay_tree_lookup(type_to_canon_type, (splay_tree_key) type);
  
  if (result == NULL)
    return discover_unique_type (type);
  else return (tree) result->value;
}

/* Same as GET_CANON_TYPE, except return the TYPE_ID rather than the
   TYPE.  */

static int
get_canon_type_uid (tree type, bool see_thru_ptrs, bool see_thru_arrays)
{
  type = get_canon_type (type, see_thru_ptrs, see_thru_arrays);
  if (type)
    return TYPE_UID(type);
  else return 0;
}

/* Return 0 if TYPE is a record or union type.  Return a positive
   number if TYPE is a pointer to a record or union.  The number is
   the number of pointer types stripped to get to the record or union
   type.  Return -1 if TYPE is none of the above.  */
 
int
ipa_type_escape_star_count_of_interesting_type (tree type) 
{
  int count = 0;
  /* Strip the *'s off.  */
  if (!type)
    return -1;
  type = TYPE_MAIN_VARIANT (type);
  while (POINTER_TYPE_P (type))
    {
      type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      count++;
    }

  /* We are interested in records, and unions only.  */
  if (TREE_CODE (type) == RECORD_TYPE 
      || TREE_CODE (type) == QUAL_UNION_TYPE 
      || TREE_CODE (type) == UNION_TYPE)
    return count;
  else 
    return -1;
} 


/* Return 0 if TYPE is a record or union type.  Return a positive
   number if TYPE is a pointer to a record or union.  The number is
   the number of pointer types stripped to get to the record or union
   type.  Return -1 if TYPE is none of the above.  */
 
int
ipa_type_escape_star_count_of_interesting_or_array_type (tree type) 
{
  int count = 0;
  /* Strip the *'s off.  */
  if (!type)
    return -1;
  type = TYPE_MAIN_VARIANT (type);
  while (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE)
    {
      type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      count++;
    }

  /* We are interested in records, and unions only.  */
  if (TREE_CODE (type) == RECORD_TYPE 
      || TREE_CODE (type) == QUAL_UNION_TYPE 
      || TREE_CODE (type) == UNION_TYPE)
    return count;
  else 
    return -1;
} 
 
 
/* Return true if the record, or union TYPE passed in escapes this
   compilation unit. Note that all of the pointer-to's are removed
   before testing since these may not be correct.  */

bool
ipa_type_escape_type_contained_p (tree type)
{
  if (!initialized)
    return false;
  return !bitmap_bit_p (global_types_full_escape, 
			get_canon_type_uid (type, true, false));
}

/* Return true if a modification to a field of type FIELD_TYPE cannot
   clobber a record of RECORD_TYPE.  */

bool 
ipa_type_escape_field_does_not_clobber_p (tree record_type, tree field_type)
{ 
  splay_tree_node result;
  int uid;
  
  if (!initialized)
    return false;

  /* Strip off all of the pointer tos on the record type.  Strip the
     same number of pointer tos from the field type.  If the field
     type has fewer, it could not have been aliased. */
  record_type = TYPE_MAIN_VARIANT (record_type);
  field_type = TYPE_MAIN_VARIANT (field_type);
  while (POINTER_TYPE_P (record_type))
    {
      record_type = TYPE_MAIN_VARIANT (TREE_TYPE (record_type));
      if (POINTER_TYPE_P (field_type)) 
	field_type = TYPE_MAIN_VARIANT (TREE_TYPE (field_type));
      else 
	/* However, if field_type is a union, this quick test is not
	   correct since one of the variants of the union may be a
	   pointer to type and we cannot see across that here.  So we
	   just strip the remaining pointer tos off the record type
	   and fall thru to the more precise code.  */
	if (TREE_CODE (field_type) == QUAL_UNION_TYPE 
	    || TREE_CODE (field_type) == UNION_TYPE)
	  {
	    while (POINTER_TYPE_P (record_type))
	      record_type = TYPE_MAIN_VARIANT (TREE_TYPE (record_type));
	    break;
	  } 
	else 
	  return true;
    }
  
  record_type = get_canon_type (record_type, true, true);
  /* The record type must be contained.  The field type may
     escape.  */
  if (!ipa_type_escape_type_contained_p (record_type))
    return false;

  uid = TYPE_UID (record_type);
  result = splay_tree_lookup (uid_to_addressof_down_map, (splay_tree_key) uid);
  
  if (result) 
    {
      bitmap field_type_map = (bitmap) result->value;
      uid = get_canon_type_uid (field_type, true, true);
      /* If the bit is there, the address was taken. If not, it
	 wasn't.  */
      return !bitmap_bit_p (field_type_map, uid);
    }
  else
    /* No bitmap means no addresses were taken.  */
    return true;
}


/* Add TYPE to the suspect type set. Return true if the bit needed to
   be marked.  */

static tree
mark_type (tree type, enum escape_t escape_status)
{
  bitmap map = NULL;
  int uid;

  type = get_canon_type (type, true, true);
  if (!type) 
    return NULL;

  switch (escape_status) 
    {
    case EXPOSED_PARAMETER:
      map = global_types_exposed_parameter;
      break;
    case FULL_ESCAPE:
      map = global_types_full_escape;
      break;
    }

  uid = TYPE_UID (type);
  if (bitmap_bit_p (map, uid))
    return type;
  else
    {
      bitmap_set_bit (map, uid);
      if (escape_status == FULL_ESCAPE)
	{
	  /* Efficiency hack. When things are bad, do not mess around
	     with this type anymore.  */
	  bitmap_set_bit (global_types_exposed_parameter, uid);
	}      
    }
  return type;
}

/* Add interesting TYPE to the suspect type set. If the set is
   EXPOSED_PARAMETER and the TYPE is a pointer type, the set is
   changed to FULL_ESCAPE.  */

static void 
mark_interesting_type (tree type, enum escape_t escape_status)
{
  if (!type) return;
  if (ipa_type_escape_star_count_of_interesting_type (type) >= 0)
    {
      if ((escape_status == EXPOSED_PARAMETER)
	  && POINTER_TYPE_P (type))
	/* EXPOSED_PARAMETERs are only structs or unions are passed by
	   value.  Anything passed by reference to an external
	   function fully exposes the type.  */
	mark_type (type, FULL_ESCAPE);
      else
	mark_type (type, escape_status);
    }
}

/* Return true if PARENT is supertype of CHILD.  Both types must be
   known to be structures or unions. */
 
static bool
parent_type_p (tree parent, tree child)
{
  int i;
  tree binfo, base_binfo;
  if (TYPE_BINFO (parent)) 
    for (binfo = TYPE_BINFO (parent), i = 0;
	 BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
      {
	tree binfotype = BINFO_TYPE (base_binfo);
	if (binfotype == child) 
	  return true;
	else if (parent_type_p (binfotype, child))
	  return true;
      }
  if (TREE_CODE (parent) == UNION_TYPE
      || TREE_CODE (parent) == QUAL_UNION_TYPE) 
    {
      tree field;
      /* Search all of the variants in the union to see if one of them
	 is the child.  */
      for (field = TYPE_FIELDS (parent);
	   field;
	   field = TREE_CHAIN (field))
	{
	  tree field_type;
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;
	  
	  field_type = TREE_TYPE (field);
	  if (field_type == child) 
	    return true;
	}

      /* If we did not find it, recursively ask the variants if one of
	 their children is the child type.  */
      for (field = TYPE_FIELDS (parent);
	   field;
	   field = TREE_CHAIN (field))
	{
	  tree field_type;
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;
	  
	  field_type = TREE_TYPE (field);
	  if (TREE_CODE (field_type) == RECORD_TYPE 
	      || TREE_CODE (field_type) == QUAL_UNION_TYPE 
	      || TREE_CODE (field_type) == UNION_TYPE)
	    if (parent_type_p (field_type, child)) 
	      return true;
	}
    }
  
  if (TREE_CODE (parent) == RECORD_TYPE)
    {
      tree field;
      for (field = TYPE_FIELDS (parent);
	   field;
	   field = TREE_CHAIN (field))
	{
	  tree field_type;
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;
	  
	  field_type = TREE_TYPE (field);
	  if (field_type == child) 
	    return true;
	  /* You can only cast to the first field so if it does not
	     match, quit.  */
	  if (TREE_CODE (field_type) == RECORD_TYPE 
	      || TREE_CODE (field_type) == QUAL_UNION_TYPE 
	      || TREE_CODE (field_type) == UNION_TYPE)
	    {
	      if (parent_type_p (field_type, child)) 
		return true;
	      else 
		break;
	    }
	}
    }
  return false;
}

/* Return the number of pointer tos for TYPE and return TYPE with all
   of these stripped off.  */

static int 
count_stars (tree* type_ptr)
{
  tree type = *type_ptr;
  int i = 0;
  type = TYPE_MAIN_VARIANT (type);
  while (POINTER_TYPE_P (type))
    {
      type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      i++;
    }

  *type_ptr = type;
  return i;
}

enum cast_type {
  CT_UP = 0x1,
  CT_DOWN = 0x2,
  CT_SIDEWAYS = 0x4,
  CT_USELESS = 0x8,
  CT_FROM_P_BAD = 0x10,
  CT_FROM_NON_P = 0x20,
  CT_TO_NON_INTER = 0x40,
  CT_FROM_MALLOC = 0x80,
  CT_NO_CAST = 0x100
};

/* Check the cast FROM_TYPE to TO_TYPE.  This function requires that
   the two types have already passed the
   ipa_type_escape_star_count_of_interesting_type test.  */

static enum cast_type
check_cast_type (tree to_type, tree from_type)
{
  int to_stars = count_stars (&to_type);
  int from_stars = count_stars (&from_type);
  if (to_stars != from_stars) 
    return CT_SIDEWAYS;

  if (to_type == from_type)
    return CT_USELESS;

  if (parent_type_p (to_type, from_type)) return CT_UP;
  if (parent_type_p (from_type, to_type)) return CT_DOWN;
  return CT_SIDEWAYS;
}     

/* This function returns nonzero if VAR is result of call 
   to malloc function.  */

static bool
is_malloc_result (tree var)
{
  tree def_stmt;
  tree rhs;
  int flags;

  if (!var)
    return false;
  
  if (SSA_NAME_IS_DEFAULT_DEF (var))
    return false;

  def_stmt = SSA_NAME_DEF_STMT (var);
  
  if (TREE_CODE (def_stmt) != GIMPLE_MODIFY_STMT)
    return false;

  if (var != GIMPLE_STMT_OPERAND (def_stmt, 0))
    return false;

  rhs = get_call_expr_in (def_stmt);

  if (!rhs)
    return false;

  flags = call_expr_flags (rhs);
    
  return ((flags & ECF_MALLOC) != 0);

}

/* Check a cast FROM this variable, TO_TYPE.  Mark the escaping types
   if appropriate. Returns cast_type as detected.  */
 
static enum cast_type
check_cast (tree to_type, tree from) 
{
  tree from_type = get_canon_type (TREE_TYPE (from), false, false);
  bool to_interesting_type, from_interesting_type;
  enum cast_type cast = CT_NO_CAST;

  to_type = get_canon_type (to_type, false, false);
  if (!from_type || !to_type || from_type == to_type)
    return cast;

  to_interesting_type = 
    ipa_type_escape_star_count_of_interesting_type (to_type) >= 0;
  from_interesting_type = 
    ipa_type_escape_star_count_of_interesting_type (from_type) >= 0;

  if (to_interesting_type) 
    if (from_interesting_type)
      {
	/* Both types are interesting. This can be one of four types
	   of cast: useless, up, down, or sideways.  We do not care
	   about up or useless.  Sideways casts are always bad and
	   both sides get marked as escaping.  Downcasts are not
	   interesting here because if type is marked as escaping, all
	   of its subtypes escape.  */
	cast = check_cast_type (to_type, from_type);
	switch (cast) 
	  {
	  case CT_UP:
	  case CT_USELESS:
	  case CT_DOWN:
	    break;

	  case CT_SIDEWAYS:
	    mark_type (to_type, FULL_ESCAPE);
	    mark_type (from_type, FULL_ESCAPE);
	    break;

	  default:
	    break;
	  }
      }
    else
      {
	/* This code excludes two cases from marking as escaped:
	   
	1. if this is a cast of index of array of structures/unions
	that happens before accessing array element, we should not 
	mark it as escaped.
	2. if this is a cast from the local that is a result from a
	call to malloc, do not mark the cast as bad.  

	*/
	
	if (POINTER_TYPE_P (to_type) && !POINTER_TYPE_P (from_type))
	  cast = CT_FROM_NON_P;
	else if (TREE_CODE (from) == SSA_NAME 
		 && is_malloc_result (from))
	  cast = CT_FROM_MALLOC;
	else
	  {
	    cast = CT_FROM_P_BAD;
	    mark_type (to_type, FULL_ESCAPE);
	  }
      }
  else if (from_interesting_type)
    {
      mark_type (from_type, FULL_ESCAPE);
      cast = CT_TO_NON_INTER;
    }

  return cast;
}

typedef struct cast 
{
  int type;
  tree stmt;
}cast_t;

/* This function is a callback for walk_tree called from 
   is_cast_from_non_pointer. The data->type is set to be:

   0      - if there is no cast
   number - the number of casts from non-pointer type
   -1     - if there is a cast that makes the type to escape

   If data->type = number, then data->stmt will contain the 
   last casting stmt met in traversing.  */

static tree
is_cast_from_non_pointer_1 (tree *tp, int *walk_subtrees, void *data)
{
  tree def_stmt = *tp;


  if (pointer_set_insert (visited_stmts, def_stmt))
    {
      *walk_subtrees = 0;
      return NULL;
    }
  
  switch (TREE_CODE (def_stmt))
    {
    case GIMPLE_MODIFY_STMT:
      {
	use_operand_p use_p; 
	ssa_op_iter iter;
	tree lhs = GIMPLE_STMT_OPERAND (def_stmt, 0);
	tree rhs = GIMPLE_STMT_OPERAND (def_stmt, 1);

        unsigned int cast = look_for_casts (lhs, rhs);
	/* Check that only one cast happened, and it's of 
	   non-pointer type.  */
	if ((cast & CT_FROM_NON_P) == (CT_FROM_NON_P) 
	    && (cast & ~(CT_FROM_NON_P)) == 0)
	  {
	    ((cast_t *)data)->stmt = def_stmt;
	    ((cast_t *)data)->type++;

	    FOR_EACH_SSA_USE_OPERAND (use_p, def_stmt, iter, SSA_OP_ALL_USES)
	      {
		walk_use_def_chains (USE_FROM_PTR (use_p), is_cast_from_non_pointer, 
				     data, false);
		if (((cast_t*)data)->type == -1)
		  return def_stmt;
	      }
	  }

	/* Check that there is no cast, or cast is not harmful. */
	else if ((cast & CT_NO_CAST) == (CT_NO_CAST)
		 || (cast & CT_DOWN) == (CT_DOWN)
		 || (cast & CT_UP) == (CT_UP)
		 || (cast & CT_USELESS) == (CT_USELESS)
		 || (cast & CT_FROM_MALLOC) == (CT_FROM_MALLOC))
	  {
	    FOR_EACH_SSA_USE_OPERAND (use_p, def_stmt, iter, SSA_OP_ALL_USES)
	      {
		walk_use_def_chains (USE_FROM_PTR (use_p), is_cast_from_non_pointer, 
				     data, false);
		if (((cast_t*)data)->type == -1)
		  return def_stmt;
	      }	    
	  }

	/* The cast is harmful.  */
	else
	  {
	    ((cast_t *)data)->type = -1;
	    return def_stmt;
	  }

	*walk_subtrees = 0;
      }     
      break;

    default:
      {
	*walk_subtrees = 0;
	break;
      }
    }

  return NULL;
}

/* This function is a callback for walk_use_def_chains function called 
   from is_array_access_through_pointer_and_index.  */

static bool
is_cast_from_non_pointer (tree var, tree def_stmt, void *data)
{

  if (!def_stmt || !var)
    return false;
  
  if (TREE_CODE (def_stmt) == PHI_NODE)
    return false;

  if (SSA_NAME_IS_DEFAULT_DEF (var))
      return false;

  walk_tree (&def_stmt, is_cast_from_non_pointer_1, data, NULL);
  if (((cast_t*)data)->type == -1)
    return true;
  
  return false;
}

/* When array element a_p[i] is accessed through the pointer a_p 
   and index i, it's translated into the following sequence
   in gimple:

  i.1_5 = (unsigned int) i_1;
  D.1605_6 = i.1_5 * 16;
  D.1606_7 = (struct str_t *) D.1605_6;
  a_p.2_8 = a_p;
  D.1608_9 = D.1606_7 + a_p.2_8;

  OP0 and OP1 are of the same pointer types and stand for 
  D.1606_7 and a_p.2_8 or vise versa.

  This function checks that:

  1. one of OP0 and OP1 (D.1606_7) has passed only one cast from 
  non-pointer type (D.1606_7 = (struct str_t *) D.1605_6;).

  2. one of OP0 and OP1 which has passed the cast from 
  non-pointer type (D.1606_7), is actually generated by multiplication of 
  index by size of type to which both OP0 and OP1 point to
  (in this case D.1605_6 = i.1_5 * 16; ).

  3. an address of def of the var to which was made cast (D.1605_6) 
  was not taken.(How can it happen?)

  The following items are checked implicitly by the end of algorithm:

  4. one of OP0 and OP1 (a_p.2_8) have never been cast 
  (because if it was cast to pointer type, its type, that is also 
  the type of OP0 and OP1, will be marked as escaped during 
  analysis of casting stmt (when check_cast() is called 
  from scan_for_refs for this stmt)).   

  5. defs of OP0 and OP1 are not passed into externally visible function
  (because if they are passed then their type, that is also the type of OP0
  and OP1, will be marked and escaped during check_call function called from 
  scan_for_refs with call stmt).

  In total, 1-5 guaranty that it's an access to array by pointer and index. 

*/

bool
is_array_access_through_pointer_and_index (enum tree_code code, tree op0, 
					   tree op1, tree *base, tree *offset,
					   tree *offset_cast_stmt)
{
  tree before_cast, before_cast_def_stmt;
  cast_t op0_cast, op1_cast;

  *base = NULL;
  *offset = NULL;
  *offset_cast_stmt = NULL;

  /* Check 1.  */
  if (code == POINTER_PLUS_EXPR)
    {
      tree op0type = TYPE_MAIN_VARIANT (TREE_TYPE (op0));
      tree op1type = TYPE_MAIN_VARIANT (TREE_TYPE (op1));

      /* One of op0 and op1 is of pointer type and the other is numerical.  */
      if (POINTER_TYPE_P (op0type) && NUMERICAL_TYPE_CHECK (op1type))
	{
	  *base = op0;
	  *offset = op1;
	}
      else if (POINTER_TYPE_P (op1type) && NUMERICAL_TYPE_CHECK (op0type))
	{
	  *base = op1;
	  *offset = op0;
	}
      else
	return false;
    }
  else
    {
      /* Init data for walk_use_def_chains function.  */
      op0_cast.type = op1_cast.type = 0;
      op0_cast.stmt = op1_cast.stmt = NULL;

      visited_stmts = pointer_set_create ();
      walk_use_def_chains (op0, is_cast_from_non_pointer,(void *)(&op0_cast),
			   false);
      pointer_set_destroy (visited_stmts);

      visited_stmts = pointer_set_create ();  
      walk_use_def_chains (op1, is_cast_from_non_pointer,(void *)(&op1_cast),
			   false);
      pointer_set_destroy (visited_stmts);

      if (op0_cast.type == 1 && op1_cast.type == 0)
	{
	  *base = op1;
	  *offset = op0;
	  *offset_cast_stmt = op0_cast.stmt;
	}
      else if (op0_cast.type == 0 && op1_cast.type == 1)
	{
	  *base = op0;
	  *offset = op1;      
	  *offset_cast_stmt = op1_cast.stmt;
	}
      else
	return false;
    }
  
  /* Check 2.  
     offset_cast_stmt is of the form: 
     D.1606_7 = (struct str_t *) D.1605_6;  */

  if (*offset_cast_stmt)
    {
      before_cast = SINGLE_SSA_TREE_OPERAND (*offset_cast_stmt, SSA_OP_USE);
      if (!before_cast)
	return false;
  
      if (SSA_NAME_IS_DEFAULT_DEF (before_cast))
	return false;
  
      before_cast_def_stmt = SSA_NAME_DEF_STMT (before_cast);
      if (!before_cast_def_stmt)
	return false;
    }
  else
    before_cast_def_stmt = SSA_NAME_DEF_STMT (*offset);

  /* before_cast_def_stmt should be of the form:
     D.1605_6 = i.1_5 * 16; */
  
  if (TREE_CODE (before_cast_def_stmt) == GIMPLE_MODIFY_STMT)
    {
      tree lhs = GIMPLE_STMT_OPERAND (before_cast_def_stmt,0);
      tree rhs = GIMPLE_STMT_OPERAND (before_cast_def_stmt,1);

      /* We expect temporary here.  */
      if (!is_gimple_reg (lhs))	
	return false;

      if (TREE_CODE (rhs) == MULT_EXPR)
	{
	  tree arg0 = TREE_OPERAND (rhs, 0);
	  tree arg1 = TREE_OPERAND (rhs, 1);
	  tree unit_size = 
	    TYPE_SIZE_UNIT (TREE_TYPE (TYPE_MAIN_VARIANT (TREE_TYPE (op0))));

	  if (!(CONSTANT_CLASS_P (arg0) 
	      && simple_cst_equal (arg0,unit_size))
	      && !(CONSTANT_CLASS_P (arg1) 
	      && simple_cst_equal (arg1,unit_size)))
	    return false;	      		   
	}
      else
	return false;
    }
  else
    return false;

  /* Check 3.
     check that address of D.1605_6 was not taken.
     FIXME: if D.1605_6 is gimple reg than it cannot be addressable.  */

  return true;
}

/* Register the parameter and return types of function FN.  The type
   ESCAPES if the function is visible outside of the compilation
   unit.  */
static void 
check_function_parameter_and_return_types (tree fn, bool escapes) 
{
  tree arg;
  
  if (TYPE_ARG_TYPES (TREE_TYPE (fn)))
    {
      for (arg = TYPE_ARG_TYPES (TREE_TYPE (fn));
	   arg && TREE_VALUE (arg) != void_type_node;
	   arg = TREE_CHAIN (arg))
	{
	  tree type = get_canon_type (TREE_VALUE (arg), false, false);
	  if (escapes)
	    mark_interesting_type (type, EXPOSED_PARAMETER);
	}
    }
  else
    {
      /* FIXME - According to Geoff Keating, we should never have to
	 do this; the front ends should always process the arg list
	 from the TYPE_ARG_LIST. However, Geoff is wrong, this code
	 does seem to be live.  */

      for (arg = DECL_ARGUMENTS (fn); arg; arg = TREE_CHAIN (arg))
	{
	  tree type = get_canon_type (TREE_TYPE (arg), false, false);
	  if (escapes)
	    mark_interesting_type (type, EXPOSED_PARAMETER);
	}
    }
  if (escapes)
    {
      tree type = get_canon_type (TREE_TYPE (TREE_TYPE (fn)), false, false);
      mark_interesting_type (type, EXPOSED_PARAMETER); 
    }
}

/* Return true if the variable T is the right kind of static variable to
   perform compilation unit scope escape analysis.  */

static inline void
has_proper_scope_for_analysis (tree t)
{
  /* If the variable has the "used" attribute, treat it as if it had a
     been touched by the devil.  */
  tree type = get_canon_type (TREE_TYPE (t), false, false);
  if (!type) return;

  if (lookup_attribute ("used", DECL_ATTRIBUTES (t)))
    {
      mark_interesting_type (type, FULL_ESCAPE);
      return;
    }

  /* Do not want to do anything with volatile except mark any
     function that uses one to be not const or pure.  */
  if (TREE_THIS_VOLATILE (t)) 
    return;

  /* Do not care about a local automatic that is not static.  */
  if (!TREE_STATIC (t) && !DECL_EXTERNAL (t))
    return;

  if (DECL_EXTERNAL (t) || TREE_PUBLIC (t))
    {
      /* If the front end set the variable to be READONLY and
	 constant, we can allow this variable in pure or const
	 functions but the scope is too large for our analysis to set
	 these bits ourselves.  */
      
      if (TREE_READONLY (t)
	  && DECL_INITIAL (t)
	  && is_gimple_min_invariant (DECL_INITIAL (t)))
	; /* Read of a constant, do not change the function state.  */
      else 
	{
	  /* The type escapes for all public and externs. */
	  mark_interesting_type (type, FULL_ESCAPE);
	}
    }
}

/* If T is a VAR_DECL for a static that we are interested in, add the
   uid to the bitmap.  */

static void
check_operand (tree t)
{
  if (!t) return;

  /* This is an assignment from a function, register the types as
     escaping.  */
  if (TREE_CODE (t) == FUNCTION_DECL)
    check_function_parameter_and_return_types (t, true);

  else if (TREE_CODE (t) == VAR_DECL)
    has_proper_scope_for_analysis (t); 
}

/* Examine tree T for references.   */

static void
check_tree (tree t)
{
  if ((TREE_CODE (t) == EXC_PTR_EXPR) || (TREE_CODE (t) == FILTER_EXPR))
    return;

  /* We want to catch here also REALPART_EXPR and IMAGEPART_EXPR,
     but they already included in handled_component_p.  */
  while (handled_component_p (t))
    {
      if (TREE_CODE (t) == ARRAY_REF)
	check_operand (TREE_OPERAND (t, 1));
      t = TREE_OPERAND (t, 0);
    }

  if (INDIRECT_REF_P (t))
/*  || TREE_CODE (t) == MEM_REF) */
    check_tree (TREE_OPERAND (t, 0));

  if (SSA_VAR_P (t) || (TREE_CODE (t) == FUNCTION_DECL))
    check_operand (t);
}

/* Create an address_of edge FROM_TYPE.TO_TYPE.  */
static void
mark_interesting_addressof (tree to_type, tree from_type)
{
  int from_uid;
  int to_uid;
  bitmap type_map;
  splay_tree_node result; 

  from_type = get_canon_type (from_type, false, false);
  to_type = get_canon_type (to_type, false, false);
  
  if (!from_type || !to_type)
    return;

  from_uid = TYPE_UID (from_type);
  to_uid = TYPE_UID (to_type);

  gcc_assert (ipa_type_escape_star_count_of_interesting_type (from_type) == 0);
  
  /* Process the Y into X map pointer.  */
  result = splay_tree_lookup (uid_to_addressof_down_map, 
			      (splay_tree_key) from_uid);
  
  if (result) 
    type_map = (bitmap) result->value;  
  else 
    {
      type_map = BITMAP_ALLOC (&ipa_obstack);
      splay_tree_insert (uid_to_addressof_down_map,
			 from_uid, 
			 (splay_tree_value)type_map);
    }
  bitmap_set_bit (type_map, TYPE_UID (to_type));
  
  /* Process the X into Y reverse map pointer.  */
  result = 
    splay_tree_lookup (uid_to_addressof_up_map, (splay_tree_key) to_uid);
  
  if (result) 
    type_map = (bitmap) result->value;  
  else 
    {
      type_map = BITMAP_ALLOC (&ipa_obstack);
      splay_tree_insert (uid_to_addressof_up_map,
			 to_uid, 
			 (splay_tree_value)type_map);
    }
  bitmap_set_bit (type_map, TYPE_UID (from_type)); 
}

/* Scan tree T to see if there are any addresses taken in within T.  */

static void 
look_for_address_of (tree t)
{
  if (TREE_CODE (t) == ADDR_EXPR)
    {
      tree x = get_base_var (t);
      tree cref = TREE_OPERAND (t, 0);

      /* If we have an expression of the form "&a.b.c.d", mark a.b,
	 b.c and c.d. as having its address taken.  */ 
      tree fielddecl = NULL_TREE;
      while (cref!= x)
	{
	  if (TREE_CODE (cref) == COMPONENT_REF)
	    {
	      fielddecl =  TREE_OPERAND (cref, 1);
	      mark_interesting_addressof (TREE_TYPE (fielddecl), 
					  DECL_FIELD_CONTEXT (fielddecl));
	    }
	  else if (TREE_CODE (cref) == ARRAY_REF)
	    get_canon_type (TREE_TYPE (cref), false, false);

	  cref = TREE_OPERAND (cref, 0);
	}

      if (TREE_CODE (x) == VAR_DECL) 
	has_proper_scope_for_analysis (x);
    }
}


/* Scan tree T to see if there are any casts within it.
   LHS Is the LHS of the expression involving the cast.  */

static unsigned int 
look_for_casts (tree lhs ATTRIBUTE_UNUSED, tree t)
{
  unsigned int cast = 0;


  if (is_gimple_cast (t) || TREE_CODE (t) == VIEW_CONVERT_EXPR)
    {
      tree castfromvar = TREE_OPERAND (t, 0);
      cast = cast | check_cast (TREE_TYPE (t), castfromvar);
    }
  else 
    while (handled_component_p (t))
      {
	t = TREE_OPERAND (t, 0);
	if (TREE_CODE (t) == VIEW_CONVERT_EXPR)
	  {
	    /* This may be some part of a component ref.
	       IE it may be a.b.VIEW_CONVERT_EXPR<weird_type>(c).d, AFAIK.
	       castfromref will give you a.b.c, not a. */
	    tree castfromref = TREE_OPERAND (t, 0);
	    cast = cast | check_cast (TREE_TYPE (t), castfromref);
	  }
	else if (TREE_CODE (t) == COMPONENT_REF)
	  get_canon_type (TREE_TYPE (TREE_OPERAND (t, 1)), false, false);
      }

  if (!cast)
    cast = CT_NO_CAST;
  return cast;
} 

/* Check to see if T is a read or address of operation on a static var
   we are interested in analyzing.  */

static void
check_rhs_var (tree t)
{
  look_for_address_of (t);
  check_tree(t);
}

/* Check to see if T is an assignment to a static var we are
   interested in analyzing.  */

static void
check_lhs_var (tree t)
{
  check_tree(t);
}

/* This is a scaled down version of get_asm_expr_operands from
   tree_ssa_operands.c.  The version there runs much later and assumes
   that aliasing information is already available. Here we are just
   trying to find if the set of inputs and outputs contain references
   or address of operations to local.  FN is the function being
   analyzed and STMT is the actual asm statement.  */

static void
get_asm_expr_operands (tree stmt)
{
  int noutputs = list_length (ASM_OUTPUTS (stmt));
  const char **oconstraints
    = (const char **) alloca ((noutputs) * sizeof (const char *));
  int i;
  tree link;
  const char *constraint;
  bool allows_mem, allows_reg, is_inout;
  
  for (i=0, link = ASM_OUTPUTS (stmt); link; ++i, link = TREE_CHAIN (link))
    {
      oconstraints[i] = constraint
	= TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
      parse_output_constraint (&constraint, i, 0, 0,
			       &allows_mem, &allows_reg, &is_inout);
      
      check_lhs_var (TREE_VALUE (link));
    }

  for (link = ASM_INPUTS (stmt); link; link = TREE_CHAIN (link))
    {
      constraint
	= TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
      parse_input_constraint (&constraint, 0, 0, noutputs, 0,
			      oconstraints, &allows_mem, &allows_reg);
      
      check_rhs_var (TREE_VALUE (link));
    }
  
  /* There is no code here to check for asm memory clobbers.  The
     casual maintainer might think that such code would be necessary,
     but that appears to be wrong.  In other parts of the compiler,
     the asm memory clobbers are assumed to only clobber variables
     that are addressable.  All types with addressable instances are
     assumed to already escape.  So, we are protected here.  */
}

/* Check the parameters of a function call to CALL_EXPR to mark the
   types that pass across the function boundary.  Also check to see if
   this is either an indirect call, a call outside the compilation
   unit.  */

static void
check_call (tree call_expr) 
{
  tree operand;
  tree callee_t = get_callee_fndecl (call_expr);
  struct cgraph_node* callee;
  enum availability avail = AVAIL_NOT_AVAILABLE;
  call_expr_arg_iterator iter;

  FOR_EACH_CALL_EXPR_ARG (operand, iter, call_expr)
    check_rhs_var (operand);
  
  if (callee_t)
    {
      tree arg_type;
      tree last_arg_type = NULL;
      callee = cgraph_node(callee_t);
      avail = cgraph_function_body_availability (callee);
      
      /* Check that there are no implicit casts in the passing of
	 parameters.  */
      if (TYPE_ARG_TYPES (TREE_TYPE (callee_t)))
	{
	  for (arg_type = TYPE_ARG_TYPES (TREE_TYPE (callee_t)),
		 operand = first_call_expr_arg (call_expr, &iter);
	       arg_type && TREE_VALUE (arg_type) != void_type_node;
	       arg_type = TREE_CHAIN (arg_type),
		 operand = next_call_expr_arg (&iter))
	    {
	      if (operand)
		{
		  last_arg_type = TREE_VALUE(arg_type);
		  check_cast (last_arg_type, operand);
		}
	      else 
		/* The code reaches here for some unfortunate
		   builtin functions that do not have a list of
		   argument types.  */
		break; 
	    }
	} 
      else  
	{ 
	  /* FIXME - According to Geoff Keating, we should never
	     have to do this; the front ends should always process
	     the arg list from the TYPE_ARG_LIST. */
	  for (arg_type = DECL_ARGUMENTS (callee_t),
		 operand = first_call_expr_arg (call_expr, &iter);
	       arg_type;
	       arg_type = TREE_CHAIN (arg_type),
		 operand = next_call_expr_arg (&iter))
	    {
	      if (operand)
		{
		  last_arg_type = TREE_TYPE(arg_type);
		  check_cast (last_arg_type, operand);
		} 
	      else 
		/* The code reaches here for some unfortunate
		   builtin functions that do not have a list of
		   argument types.  */
		break; 
	    }
	}
      
      /* In the case where we have a var_args function, we need to
	 check the remaining parameters against the last argument.  */
      arg_type = last_arg_type;
      for (;
	   operand != NULL_TREE;
	   operand = next_call_expr_arg (&iter))
	{
	  if (arg_type)
	    check_cast (arg_type, operand);
	  else 
	    {
	      /* The code reaches here for some unfortunate
		 builtin functions that do not have a list of
		 argument types.  Most of these functions have
		 been marked as having their parameters not
		 escape, but for the rest, the type is doomed.  */
	      tree type = get_canon_type (TREE_TYPE (operand), false, false);
	      mark_interesting_type (type, FULL_ESCAPE);
	    }
	}
    }

  /* The callee is either unknown (indirect call) or there is just no
     scannable code for it (external call) .  We look to see if there
     are any bits available for the callee (such as by declaration or
     because it is builtin) and process solely on the basis of those
     bits. */

  if (avail == AVAIL_NOT_AVAILABLE || avail == AVAIL_OVERWRITABLE)
    {
      /* If this is a direct call to an external function, mark all of
	 the parameter and return types.  */
      FOR_EACH_CALL_EXPR_ARG (operand, iter, call_expr)
	{
	  tree type = get_canon_type (TREE_TYPE (operand), false, false);
	  mark_interesting_type (type, EXPOSED_PARAMETER);
    }
	  
      if (callee_t) 
	{
	  tree type = 
	    get_canon_type (TREE_TYPE (TREE_TYPE (callee_t)), false, false);
	  mark_interesting_type (type, EXPOSED_PARAMETER);
	}
    }
}

/* CODE is the operation on OP0 and OP1.  OP0 is the operand that we
   *know* is a pointer type.  OP1 may be a pointer type.  */
static bool 
okay_pointer_operation (enum tree_code code, tree op0, tree op1)
{
  tree op0type = TYPE_MAIN_VARIANT (TREE_TYPE (op0));

  switch (code)
    {
    case MULT_EXPR:
      /* Multiplication does not change alignment.  */
      return true;
      break;
    case MINUS_EXPR:
    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      {
	tree base, offset, offset_cast_stmt;

	if (POINTER_TYPE_P (op0type)
	    && TREE_CODE (op0) == SSA_NAME 
	    && TREE_CODE (op1) == SSA_NAME 
	    && is_array_access_through_pointer_and_index (code, op0, op1, 
							  &base, 
							  &offset, 
							  &offset_cast_stmt))
	  return true;
	else
	  {
	    tree size_of_op0_points_to = TYPE_SIZE_UNIT (TREE_TYPE (op0type));
	    
	    if (CONSTANT_CLASS_P (op1)
		&& size_of_op0_points_to
		&& multiple_of_p (TREE_TYPE (size_of_op0_points_to), 
				  op1, size_of_op0_points_to))
	      return true;

	    if (CONSTANT_CLASS_P (op0) 
		&& size_of_op0_points_to
		&& multiple_of_p (TREE_TYPE (size_of_op0_points_to), 
				  op0, size_of_op0_points_to))
	      return true;	    
	  }
      }
      break;
    default:
      return false;
    }
  return false;
}

/* TP is the part of the tree currently under the microscope.
   WALK_SUBTREES is part of the walk_tree api but is unused here.
   DATA is cgraph_node of the function being walked.  */

/* FIXME: When this is converted to run over SSA form, this code
   should be converted to use the operand scanner.  */

static tree
scan_for_refs (tree *tp, int *walk_subtrees, void *data)
{
  struct cgraph_node *fn = (struct cgraph_node *) data;
  tree t = *tp;

  switch (TREE_CODE (t))  
    {
    case VAR_DECL:
      if (DECL_INITIAL (t))
	walk_tree (&DECL_INITIAL (t), scan_for_refs, fn, visited_nodes);
      *walk_subtrees = 0;
      break;

    case GIMPLE_MODIFY_STMT:
      {
	/* First look on the lhs and see what variable is stored to */
	tree lhs = GIMPLE_STMT_OPERAND (t, 0);
	tree rhs = GIMPLE_STMT_OPERAND (t, 1);

	check_lhs_var (lhs);
 	check_cast (TREE_TYPE (lhs), rhs);

	/* For the purposes of figuring out what the cast affects */

	/* Next check the operands on the rhs to see if they are ok. */
	switch (TREE_CODE_CLASS (TREE_CODE (rhs))) 
	  {
	  case tcc_binary:	    
 	    {
 	      tree op0 = TREE_OPERAND (rhs, 0);
	      tree type0 = get_canon_type (TREE_TYPE (op0), false, false);
 	      tree op1 = TREE_OPERAND (rhs, 1);
	      tree type1 = get_canon_type (TREE_TYPE (op1), false, false);
 
 	      /* If this is pointer arithmetic of any bad sort, then
 		 we need to mark the types as bad.  For binary
 		 operations, no binary operator we currently support
 		 is always "safe" in regard to what it would do to
 		 pointers for purposes of determining which types
 		 escape, except operations of the size of the type.
 		 It is possible that min and max under the right set
 		 of circumstances and if the moon is in the correct
 		 place could be safe, but it is hard to see how this
 		 is worth the effort.  */
 
 	      if (type0 && POINTER_TYPE_P (type0)
		  && !okay_pointer_operation (TREE_CODE (rhs), op0, op1))
 		mark_interesting_type (type0, FULL_ESCAPE);
 	      if (type1 && POINTER_TYPE_P (type1)
		  && !okay_pointer_operation (TREE_CODE (rhs), op1, op0))
 		mark_interesting_type (type1, FULL_ESCAPE);
 	      
	      look_for_casts (lhs, op0);
	      look_for_casts (lhs, op1);
 	      check_rhs_var (op0);
 	      check_rhs_var (op1);
	    }
	    break;
	  case tcc_unary:
 	    {
 	      tree op0 = TREE_OPERAND (rhs, 0);
	      tree type0 = get_canon_type (TREE_TYPE (op0), false, false);
	      /* For unary operations, if the operation is NEGATE or
		 ABS on a pointer, this is also considered pointer
		 arithmetic and thus, bad for business.  */
 	      if (type0 && (TREE_CODE (op0) == NEGATE_EXPR
 		   || TREE_CODE (op0) == ABS_EXPR)
 		  && POINTER_TYPE_P (type0))
 		{
 		  mark_interesting_type (type0, FULL_ESCAPE);
 		}
 	      check_rhs_var (op0);
	      look_for_casts (lhs, op0);
	      look_for_casts (lhs, rhs);
 	    }

	    break;
	  case tcc_reference:
	    look_for_casts (lhs, rhs);
	    check_rhs_var (rhs);
	    break;
	  case tcc_declaration:
	    check_rhs_var (rhs);
	    break;
	  case tcc_expression:
	    switch (TREE_CODE (rhs)) 
	      {
	      case ADDR_EXPR:
		look_for_casts (lhs, TREE_OPERAND (rhs, 0));
		check_rhs_var (rhs);
		break;
	      default:
		break;
	      }
	    break;
	  case tcc_vl_exp:
	    switch (TREE_CODE (rhs))
	      {
	      case CALL_EXPR:
		/* If this is a call to malloc, squirrel away the
		   result so we do mark the resulting cast as being
		   bad.  */
		check_call (rhs);
		break;
	      default:
		break;
	      }
	    break;
	  default:
	    break;
	  }
	*walk_subtrees = 0;
      }
      break;

    case ADDR_EXPR:
      /* This case is here to find addresses on rhs of constructors in
	 decl_initial of static variables. */
      check_rhs_var (t);
      *walk_subtrees = 0;
      break;

    case CALL_EXPR: 
      check_call (t);
      *walk_subtrees = 0;
      break;
      
    case ASM_EXPR:
      get_asm_expr_operands (t);
      *walk_subtrees = 0;
      break;
      
    default:
      break;
    }
  return NULL;
}


/* The init routine for analyzing global static variable usage.  See
   comments at top for description.  */
static void 
ipa_init (void) 
{
  bitmap_obstack_initialize (&ipa_obstack);
  global_types_exposed_parameter = BITMAP_ALLOC (&ipa_obstack);
  global_types_full_escape = BITMAP_ALLOC (&ipa_obstack);
  global_types_seen = BITMAP_ALLOC (&ipa_obstack);

  uid_to_canon_type = splay_tree_new (splay_tree_compare_ints, 0, 0);
  all_canon_types = splay_tree_new (compare_type_brand, 0, 0);
  type_to_canon_type = splay_tree_new (splay_tree_compare_pointers, 0, 0);
  uid_to_subtype_map = splay_tree_new (splay_tree_compare_ints, 0, 0);
  uid_to_addressof_down_map = splay_tree_new (splay_tree_compare_ints, 0, 0);
  uid_to_addressof_up_map = splay_tree_new (splay_tree_compare_ints, 0, 0);

  /* There are some shared nodes, in particular the initializers on
     static declarations.  We do not need to scan them more than once
     since all we would be interested in are the addressof
     operations.  */
  visited_nodes = pointer_set_create ();
  initialized = true;
}

/* Check out the rhs of a static or global initialization VNODE to see
   if any of them contain addressof operations.  Note that some of
   these variables may not even be referenced in the code in this
   compilation unit but their right hand sides may contain references
   to variables defined within this unit.  */

static void 
analyze_variable (struct varpool_node *vnode)
{
  tree global = vnode->decl;
  tree type = get_canon_type (TREE_TYPE (global), false, false);

  /* If this variable has exposure beyond the compilation unit, add
     its type to the global types.  */

  if (vnode->externally_visible)
    mark_interesting_type (type, FULL_ESCAPE);

  gcc_assert (TREE_CODE (global) == VAR_DECL);

  if (DECL_INITIAL (global))
    walk_tree (&DECL_INITIAL (global), scan_for_refs, NULL, visited_nodes);
}

/* This is the main routine for finding the reference patterns for
   global variables within a function FN.  */

static void
analyze_function (struct cgraph_node *fn)
{
  tree decl = fn->decl;
  check_function_parameter_and_return_types (decl, 
					     fn->local.externally_visible);
  if (dump_file)
    fprintf (dump_file, "\n local analysis of %s", cgraph_node_name (fn));
  
  {
    struct function *this_cfun = DECL_STRUCT_FUNCTION (decl);
    basic_block this_block;

    FOR_EACH_BB_FN (this_block, this_cfun)
      {
	block_stmt_iterator bsi;
	for (bsi = bsi_start (this_block); !bsi_end_p (bsi); bsi_next (&bsi))
	  walk_tree (bsi_stmt_ptr (bsi), scan_for_refs, 
		     fn, visited_nodes);
      }
  }

  /* There may be const decls with interesting right hand sides.  */
  if (DECL_STRUCT_FUNCTION (decl))
    {
      tree step;
      for (step = DECL_STRUCT_FUNCTION (decl)->unexpanded_var_list;
	   step;
	   step = TREE_CHAIN (step))
	{
	  tree var = TREE_VALUE (step);
	  if (TREE_CODE (var) == VAR_DECL 
	      && DECL_INITIAL (var)
	      && !TREE_STATIC (var))
	    walk_tree (&DECL_INITIAL (var), scan_for_refs, 
		       fn, visited_nodes);
	  get_canon_type (TREE_TYPE (var), false, false);
	}
    }
}



/* Convert a type_UID into a type.  */
static tree
type_for_uid (int uid)
{
  splay_tree_node result = 
    splay_tree_lookup (uid_to_canon_type, (splay_tree_key) uid);
  
  if (result)
    return (tree) result->value;  
  else return NULL;
}

/* Return the a bitmap with the subtypes of the type for UID.  If it
   does not exist, return either NULL or a new bitmap depending on the
   value of CREATE.  */ 

static bitmap
subtype_map_for_uid (int uid, bool create)
{
  splay_tree_node result = splay_tree_lookup (uid_to_subtype_map, 
			      (splay_tree_key) uid);
  
  if (result) 
    return (bitmap) result->value;  
  else if (create)
    {
      bitmap subtype_map = BITMAP_ALLOC (&ipa_obstack);
      splay_tree_insert (uid_to_subtype_map,
			 uid, 
			 (splay_tree_value)subtype_map);
      return subtype_map;
    }
  else return NULL;
}

/* Mark all of the supertypes and field types of TYPE as being seen.
   Also accumulate the subtypes for each type so that
   close_types_full_escape can mark a subtype as escaping if the
   supertype escapes.  */

static void
close_type_seen (tree type)
{
  tree field;
  int i, uid;
  tree binfo, base_binfo;

  /* See thru all pointer tos and array ofs. */
  type = get_canon_type (type, true, true);
  if (!type)
    return;

  uid = TYPE_UID (type);

  if (bitmap_bit_p (been_there_done_that, uid))
    return;
  bitmap_set_bit (been_there_done_that, uid);

  /* If we are doing a language with a type hierarchy, mark all of
     the superclasses.  */
  if (TYPE_BINFO (type)) 
    for (binfo = TYPE_BINFO (type), i = 0;
	 BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
      {
	tree binfo_type = BINFO_TYPE (base_binfo);
	bitmap subtype_map = subtype_map_for_uid 
	  (TYPE_UID (TYPE_MAIN_VARIANT (binfo_type)), true);
	bitmap_set_bit (subtype_map, uid);
	close_type_seen (get_canon_type (binfo_type, true, true));
      }
      
  /* If the field is a struct or union type, mark all of the
     subfields.  */
  for (field = TYPE_FIELDS (type);
       field;
       field = TREE_CHAIN (field))
    {
      tree field_type;
      if (TREE_CODE (field) != FIELD_DECL)
	continue;

      field_type = TREE_TYPE (field);
      if (ipa_type_escape_star_count_of_interesting_or_array_type (field_type) >= 0)
	close_type_seen (get_canon_type (field_type, true, true));
    }
}

/* Take a TYPE that has been passed by value to an external function
   and mark all of the fields that have pointer types as escaping. For
   any of the non pointer types that are structures or unions,
   recurse.  TYPE is never a pointer type.  */ 

static void
close_type_exposed_parameter (tree type)
{
  tree field;
  int uid;

  type = get_canon_type (type, false, false);
  if (!type)
    return;
  uid = TYPE_UID (type);
  gcc_assert (!POINTER_TYPE_P (type));

  if (bitmap_bit_p (been_there_done_that, uid))
    return;
  bitmap_set_bit (been_there_done_that, uid);

  /* If the field is a struct or union type, mark all of the
     subfields.  */
  for (field = TYPE_FIELDS (type);
       field;
       field = TREE_CHAIN (field))
    {
      tree field_type;

      if (TREE_CODE (field) != FIELD_DECL)
	continue;

      field_type = get_canon_type (TREE_TYPE (field), false, false);
      mark_interesting_type (field_type, EXPOSED_PARAMETER);

      /* Only recurse for non pointer types of structures and unions.  */
      if (ipa_type_escape_star_count_of_interesting_type (field_type) == 0) 
	close_type_exposed_parameter (field_type);
    }
}

/* The next function handles the case where a type fully escapes.
   This means that not only does the type itself escape, 

   a) the type of every field recursively escapes
   b) the type of every subtype escapes as well as the super as well
   as all of the pointer to types for each field.

   Note that pointer to types are not marked as escaping.  If the
   pointed to type escapes, the pointer to type also escapes.

   Take a TYPE that has had the address taken for an instance of it
   and mark all of the types for its fields as having their addresses
   taken. */ 

static void
close_type_full_escape (tree type)
{
  tree field;
  unsigned int i;
  int uid;
  tree binfo, base_binfo;
  bitmap_iterator bi;
  bitmap subtype_map;
  splay_tree_node address_result; 

  /* Strip off any pointer or array types.  */
  type = get_canon_type (type, true, true);
  if (!type)
    return;
  uid = TYPE_UID (type);

  if (bitmap_bit_p (been_there_done_that, uid))
    return;
  bitmap_set_bit (been_there_done_that, uid);

  subtype_map = subtype_map_for_uid (uid, false);

  /* If we are doing a language with a type hierarchy, mark all of
     the superclasses.  */
  if (TYPE_BINFO (type)) 
    for (binfo = TYPE_BINFO (type), i = 0;
	 BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
      {
	tree binfotype = BINFO_TYPE (base_binfo);
	binfotype = mark_type (binfotype, FULL_ESCAPE);
	close_type_full_escape (binfotype);
      }
      
  /* Mark as escaped any types that have been down casted to
     this type. */
  if (subtype_map)
    EXECUTE_IF_SET_IN_BITMAP (subtype_map, 0, i, bi)
      {
	tree subtype = type_for_uid (i); 
	subtype = mark_type (subtype, FULL_ESCAPE);
	close_type_full_escape (subtype);
      }

  /* If the field is a struct or union type, mark all of the
     subfields.  */
  for (field = TYPE_FIELDS (type);
       field;
       field = TREE_CHAIN (field))
    {
      tree field_type;
      if (TREE_CODE (field) != FIELD_DECL)
	continue;

      field_type = TREE_TYPE (field);
      if (ipa_type_escape_star_count_of_interesting_or_array_type (field_type) >= 0)
	{
	  field_type = mark_type (field_type, FULL_ESCAPE);
	  close_type_full_escape (field_type);
	}
    }

  /* For all of the types A that contain this type B and were part of
     an expression like "&...A.B...", mark the A's as escaping.  */
  address_result = splay_tree_lookup (uid_to_addressof_up_map, 
				      (splay_tree_key) uid);
  if (address_result)
    {
      bitmap containing_classes = (bitmap) address_result->value;
      EXECUTE_IF_SET_IN_BITMAP (containing_classes, 0, i, bi)
	{
	  close_type_full_escape (type_for_uid (i));
	}
    }
}

/* Transitively close the addressof bitmap for the type with UID.
   This means that if we had a.b and b.c, a would have both b and c in
   its maps.  */ 

static bitmap
close_addressof_down (int uid) 
{
  bitmap_iterator bi;
  splay_tree_node result = 
    splay_tree_lookup (uid_to_addressof_down_map, (splay_tree_key) uid);
  bitmap map = NULL;
  bitmap new_map;
  unsigned int i;
  
  if (result) 
    map = (bitmap) result->value;
  else 
    return NULL;

  if (bitmap_bit_p (been_there_done_that, uid))
    return map;
  bitmap_set_bit (been_there_done_that, uid);

  /* If the type escapes, get rid of the addressof map, it will not be
     needed.  */
  if (bitmap_bit_p (global_types_full_escape, uid))
    {
      BITMAP_FREE (map);
      splay_tree_remove (uid_to_addressof_down_map, (splay_tree_key) uid);
      return NULL;
    }

  /* The new_map will have all of the bits for the enclosed fields and
     will have the unique id version of the old map.  */
  new_map = BITMAP_ALLOC (&ipa_obstack);

  EXECUTE_IF_SET_IN_BITMAP (map, 0, i, bi)
    {
      bitmap submap = close_addressof_down (i);
      bitmap_set_bit (new_map, i);
      if (submap) 
	bitmap_ior_into (new_map, submap);
    }      
  result->value = (splay_tree_value) new_map;

  BITMAP_FREE (map);
  return new_map;
}


/* The main entry point for type escape analysis.  */

static unsigned int
type_escape_execute (void)
{
  struct cgraph_node *node;
  struct varpool_node *vnode;
  unsigned int i;
  bitmap_iterator bi;
  splay_tree_node result;

  ipa_init ();

  /* Process all of the variables first.  */
  FOR_EACH_STATIC_VARIABLE (vnode)
    analyze_variable (vnode);

  /* Process all of the functions. next

     We do not want to process any of the clones so we check that this
     is a master clone.  However, we do need to process any
     AVAIL_OVERWRITABLE functions (these are never clones) because
     they may cause a type variable to escape.  
  */
  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed 
	&& (cgraph_is_master_clone (node)
	    || (cgraph_function_body_availability (node) == AVAIL_OVERWRITABLE)))
      analyze_function (node);


  pointer_set_destroy (visited_nodes);
  visited_nodes = NULL;

  /* Do all of the closures to discover which types escape the
     compilation unit.  */

  been_there_done_that = BITMAP_ALLOC (&ipa_obstack);
  bitmap_tmp = BITMAP_ALLOC (&ipa_obstack);

  /* Examine the types that we have directly seen in scanning the code
     and add to that any contained types or superclasses.  */

  bitmap_copy (bitmap_tmp, global_types_seen);
  EXECUTE_IF_SET_IN_BITMAP (bitmap_tmp, 0, i, bi)
    {
      tree type = type_for_uid (i);
      /* Only look at records and unions and pointer tos.  */
      if (ipa_type_escape_star_count_of_interesting_or_array_type (type) >= 0)
	close_type_seen (type);
    }
  bitmap_clear (been_there_done_that);

  /* Examine all of the types passed by value and mark any enclosed
     pointer types as escaping.  */
  bitmap_copy (bitmap_tmp, global_types_exposed_parameter);
  EXECUTE_IF_SET_IN_BITMAP (bitmap_tmp, 0, i, bi)
    {
      close_type_exposed_parameter (type_for_uid (i));
    }
  bitmap_clear (been_there_done_that);

  /* Close the types for escape.  If something escapes, then any
     enclosed types escape as well as any subtypes.  */
  bitmap_copy (bitmap_tmp, global_types_full_escape);
  EXECUTE_IF_SET_IN_BITMAP (bitmap_tmp, 0, i, bi)
    {
      close_type_full_escape (type_for_uid (i));
    }
  bitmap_clear (been_there_done_that);

  /* Before this pass, the uid_to_addressof_down_map for type X
     contained an entry for Y if there had been an operation of the
     form &X.Y.  This step adds all of the fields contained within Y
     (recursively) to X's map.  */
  
  result = splay_tree_min (uid_to_addressof_down_map);
  while (result)
    {
      int uid = result->key;
      /* Close the addressof map, i.e. copy all of the transitive
	 substructures up to this level.  */
      close_addressof_down (uid);
      result = splay_tree_successor (uid_to_addressof_down_map, uid);
    }

  /* Do not need the array types and pointer types in the persistent
     data structures.  */
  result = splay_tree_min (all_canon_types);
  while (result)
    {
      tree type = (tree) result->value;
      tree key = (tree) result->key;
      if (POINTER_TYPE_P (type) 
	  || TREE_CODE (type) == ARRAY_TYPE)
	{
	  splay_tree_remove (all_canon_types, (splay_tree_key) result->key);
	  splay_tree_remove (type_to_canon_type, (splay_tree_key) type);
	  splay_tree_remove (uid_to_canon_type, (splay_tree_key) TYPE_UID (type));
	  bitmap_clear_bit (global_types_seen, TYPE_UID (type));
	}
      result = splay_tree_successor (all_canon_types, (splay_tree_key) key);
    }

  if (dump_file)
    { 
      EXECUTE_IF_SET_IN_BITMAP (global_types_seen, 0, i, bi)
	{
	  /* The pointer types are in the global_types_full_escape
	     bitmap but not in the backwards map.  They also contain
	     no useful information since they are not marked.  */
	  tree type = type_for_uid (i);
	  fprintf(dump_file, "type %d ", i);
	  print_generic_expr (dump_file, type, 0);
	  if (bitmap_bit_p (global_types_full_escape, i))
	    fprintf(dump_file, " escaped\n");
	  else 
	    fprintf(dump_file, " contained\n");
	}
    }

  /* Get rid of uid_to_addressof_up_map and its bitmaps.  */
  result = splay_tree_min (uid_to_addressof_up_map);
  while (result)
    {
      int uid = (int)result->key;
      bitmap bm = (bitmap)result->value;

      BITMAP_FREE (bm);
      splay_tree_remove (uid_to_addressof_up_map, (splay_tree_key) uid);
      result = splay_tree_successor (uid_to_addressof_up_map, uid);
    }

  /* Get rid of the subtype map.  */
  result = splay_tree_min (uid_to_subtype_map);
  while (result)
    {
      bitmap b = (bitmap)result->value;
      BITMAP_FREE(b);
      splay_tree_remove (uid_to_subtype_map, result->key);
      result = splay_tree_min (uid_to_subtype_map);
    }
  splay_tree_delete (uid_to_subtype_map);
  uid_to_subtype_map = NULL;

  BITMAP_FREE (global_types_exposed_parameter);
  BITMAP_FREE (been_there_done_that);
  BITMAP_FREE (bitmap_tmp);
  return 0;
}

static bool
gate_type_escape_vars (void)
{
  return (flag_unit_at_a_time != 0 && flag_ipa_type_escape
	  /* Don't bother doing anything if the program has errors.  */
	  && !(errorcount || sorrycount));
}

struct tree_opt_pass pass_ipa_type_escape =
{
  "type-escape-var",			/* name */
  gate_type_escape_vars,		/* gate */
  type_escape_execute,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_TYPE_ESCAPE,	        	/* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,                                    /* todo_flags_finish */
  0					/* letter */
};

