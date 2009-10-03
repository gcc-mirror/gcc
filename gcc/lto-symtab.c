/* LTO symbol table.
   Copyright 2009 Free Software Foundation, Inc.
   Contributed by CodeSourcery, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "toplev.h"
#include "tree.h"
#include "gimple.h"
#include "ggc.h"	/* lambda.h needs this */
#include "lambda.h"	/* gcd */
#include "hashtab.h"
#include "plugin-api.h"
#include "lto-streamer.h"

/* Vector to keep track of external variables we've seen so far.  */
VEC(tree,gc) *lto_global_var_decls;

/* Base type for resolution map. It maps NODE to resolution.  */

struct GTY(()) lto_symtab_base_def
{
  /* Key is either an IDENTIFIER or a DECL.  */
  tree node;
};
typedef struct lto_symtab_base_def *lto_symtab_base_t;

struct GTY(()) lto_symtab_identifier_def
{
  struct lto_symtab_base_def base;
  tree decl;
};
typedef struct lto_symtab_identifier_def *lto_symtab_identifier_t;

struct GTY(()) lto_symtab_decl_def
{
  struct lto_symtab_base_def base;
  enum ld_plugin_symbol_resolution resolution;
  struct lto_file_decl_data * GTY((skip (""))) file_data;
};
typedef struct lto_symtab_decl_def *lto_symtab_decl_t;

/* A poor man's symbol table. This hashes identifier to prevailing DECL
   if there is one. */

static GTY ((if_marked ("lto_symtab_identifier_marked_p"),
	     param_is (struct lto_symtab_identifier_def)))
  htab_t lto_symtab_identifiers;

static GTY ((if_marked ("lto_symtab_decl_marked_p"),
	     param_is (struct lto_symtab_decl_def)))
  htab_t lto_symtab_decls;

/* Return the hash value of an lto_symtab_base_t object pointed to by P.  */

static hashval_t
lto_symtab_base_hash (const void *p)
{
  const struct lto_symtab_base_def *base =
    (const struct lto_symtab_base_def*) p;
  return htab_hash_pointer (base->node);
}

/* Return non-zero if P1 and P2 points to lto_symtab_base_def structs
   corresponding to the same tree node.  */

static int
lto_symtab_base_eq (const void *p1, const void *p2)
{
  const struct lto_symtab_base_def *base1 =
     (const struct lto_symtab_base_def *) p1;
  const struct lto_symtab_base_def *base2 =
     (const struct lto_symtab_base_def *) p2;
  return (base1->node == base2->node);
}

/* Returns non-zero if P points to an lto_symtab_base_def struct that needs
   to be marked for GC.  */ 

static int
lto_symtab_base_marked_p (const void *p)
{
  const struct lto_symtab_base_def *base =
     (const struct lto_symtab_base_def *) p;

  /* Keep this only if the key node is marked.  */
  return ggc_marked_p (base->node);
}

/* Returns non-zero if P points to an lto_symtab_identifier_def struct that
   needs to be marked for GC.  */ 

static int
lto_symtab_identifier_marked_p (const void *p)
{
  return lto_symtab_base_marked_p (p);
}

/* Returns non-zero if P points to an lto_symtab_decl_def struct that needs
   to be marked for GC.  */ 

static int
lto_symtab_decl_marked_p (const void *p)
{
  return lto_symtab_base_marked_p (p);
}

#define lto_symtab_identifier_eq	lto_symtab_base_eq
#define lto_symtab_identifier_hash	lto_symtab_base_hash
#define lto_symtab_decl_eq		lto_symtab_base_eq
#define lto_symtab_decl_hash		lto_symtab_base_hash

/* Lazily initialize resolution hash tables.  */

static void
lto_symtab_maybe_init_hash_tables (void)
{
  if (!lto_symtab_identifiers)
    {
      lto_symtab_identifiers =
	htab_create_ggc (1021, lto_symtab_identifier_hash,
			 lto_symtab_identifier_eq, NULL);
      lto_symtab_decls =
	htab_create_ggc (1021, lto_symtab_decl_hash,
			 lto_symtab_decl_eq, NULL);
    }
}

/* Returns true iff the union of ATTRIBUTES_1 and ATTRIBUTES_2 can be
   applied to DECL.  */
static bool
lto_compatible_attributes_p (tree decl ATTRIBUTE_UNUSED, 
			     tree attributes_1, 
			     tree attributes_2)
{
#if 0
  /* ??? For now, assume two attribute sets are compatible only if they
     are both empty.  */
  return !attributes_1 && !attributes_2;
#else
  /* FIXME.  For the moment, live dangerously, and assume the user knows
     what he's doing. I don't think the linker would distinguish these cases.  */
  return true || (!attributes_1 && !attributes_2);
#endif
}

/* Helper for lto_symtab_compatible. Return TRUE if DECL is an external
   variable declaration of an aggregate type. */

static bool
external_aggregate_decl_p (tree decl)
{
  return (TREE_CODE (decl) == VAR_DECL
	  && DECL_EXTERNAL (decl)
	  && AGGREGATE_TYPE_P (TREE_TYPE (decl)));
}

static bool maybe_merge_incomplete_and_complete_type (tree, tree);

/* Try to merge an incomplete type INCOMPLETE with a complete type
   COMPLETE of same kinds.
   Return true if they were merged, false otherwise.  */

static bool
merge_incomplete_and_complete_type (tree incomplete, tree complete)
{
  /* For merging array types do some extra sanity checking.  */
  if (TREE_CODE (incomplete) == ARRAY_TYPE
      && !maybe_merge_incomplete_and_complete_type (TREE_TYPE (incomplete),
						    TREE_TYPE (complete))
      && !gimple_types_compatible_p (TREE_TYPE (incomplete),
				     TREE_TYPE (complete)))
    return false;

  /* ??? Ideally we would do this by means of a common canonical type, but
     that's difficult as we do not have links from the canonical type
     back to all its children.  */
  gimple_force_type_merge (incomplete, complete);

  return true;
}

/* Try to merge a maybe complete / incomplete type pair TYPE1 and TYPE2.
   Return true if they were merged, false otherwise.  */

static bool
maybe_merge_incomplete_and_complete_type (tree type1, tree type2)
{
  bool res = false;

  if (TREE_CODE (type1) != TREE_CODE (type2))
    return false;

  if (!COMPLETE_TYPE_P (type1) && COMPLETE_TYPE_P (type2))
    res = merge_incomplete_and_complete_type (type1, type2);
  else if (COMPLETE_TYPE_P (type1) && !COMPLETE_TYPE_P (type2))
    res = merge_incomplete_and_complete_type (type2, type1);

  /* Recurse on pointer targets.  */
  if (!res
      && POINTER_TYPE_P (type1)
      && POINTER_TYPE_P (type2))
    res = maybe_merge_incomplete_and_complete_type (TREE_TYPE (type1),
						    TREE_TYPE (type2));

  return res;
}

/* Check if OLD_DECL and NEW_DECL are compatible. */

static bool
lto_symtab_compatible (tree old_decl, tree new_decl)
{
  tree merged_type = NULL_TREE;

  if (TREE_CODE (old_decl) != TREE_CODE (new_decl))
    {
      switch (TREE_CODE (new_decl))
	{
	case VAR_DECL:
	  gcc_assert (TREE_CODE (old_decl) == FUNCTION_DECL);
	  error_at (DECL_SOURCE_LOCATION (new_decl),
		    "function %qD redeclared as variable", new_decl);
	  inform (DECL_SOURCE_LOCATION (old_decl),
		  "previously declared here");
	  return false;

	case FUNCTION_DECL:
	  gcc_assert (TREE_CODE (old_decl) == VAR_DECL);
	  error_at (DECL_SOURCE_LOCATION (new_decl),
		    "variable %qD redeclared as function", new_decl);
	  inform (DECL_SOURCE_LOCATION (old_decl),
		  "previously declared here");
	  return false;

	default:
	  gcc_unreachable ();
	}
    }

  /* Handle external declarations with incomplete type or pointed-to
     incomplete types by forcefully merging the types.
     ???  In principle all types involved in the two decls should
     be merged forcefully, for example without considering type or
     field names.  */
  if (TREE_CODE (old_decl) == VAR_DECL)
    {
      tree old_type = TREE_TYPE (old_decl);
      tree new_type = TREE_TYPE (new_decl);

      if (DECL_EXTERNAL (old_decl) || DECL_EXTERNAL (new_decl))
	maybe_merge_incomplete_and_complete_type (old_type, new_type);
      else if (POINTER_TYPE_P (old_type)
	       && POINTER_TYPE_P (new_type))
	maybe_merge_incomplete_and_complete_type (TREE_TYPE (old_type),
						  TREE_TYPE (new_type));

      /* For array types we have to accept external declarations with
	 different sizes than the actual definition (164.gzip).
	 ???  We could emit a warning here.  */
      if (TREE_CODE (old_type) == TREE_CODE (new_type)
	  && TREE_CODE (old_type) == ARRAY_TYPE
	  && COMPLETE_TYPE_P (old_type)
	  && COMPLETE_TYPE_P (new_type)
	  && tree_int_cst_compare (TYPE_SIZE (old_type),
				   TYPE_SIZE (new_type)) != 0
	  && gimple_types_compatible_p (TREE_TYPE (old_type),
					TREE_TYPE (new_type)))
	{
	  /* If only one is external use the type of the non-external decl.
	     Else use the larger one and also adjust the decl size.
	     ???  Directional merging would allow us to simply pick the
	     larger one instead of rewriting it.  */
	  if (DECL_EXTERNAL (old_decl) ^ DECL_EXTERNAL (new_decl))
	    {
	      if (DECL_EXTERNAL (old_decl))
		TREE_TYPE (old_decl) = new_type;
	      else if (DECL_EXTERNAL (new_decl))
		TREE_TYPE (new_decl) = old_type;
	    }
	  else
	    {
	      if (tree_int_cst_compare (TYPE_SIZE (old_type),
					TYPE_SIZE (new_type)) < 0)
		{
		  TREE_TYPE (old_decl) = new_type;
		  DECL_SIZE (old_decl) = DECL_SIZE (new_decl);
		  DECL_SIZE_UNIT (old_decl) = DECL_SIZE_UNIT (new_decl);
		}
	      else
		{
		  TREE_TYPE (new_decl) = old_type;
		  DECL_SIZE (new_decl) = DECL_SIZE (old_decl);
		  DECL_SIZE_UNIT (new_decl) = DECL_SIZE_UNIT (old_decl);
		}
	    }
	}
    }

  if (!gimple_types_compatible_p (TREE_TYPE (old_decl), TREE_TYPE (new_decl)))
    {
      if (TREE_CODE (new_decl) == FUNCTION_DECL)
	{
	  if (!merged_type
	      /* We want either of the types to have argument types,
		 but not both.  */
	      && ((TYPE_ARG_TYPES (TREE_TYPE (old_decl)) != NULL)
		  ^ (TYPE_ARG_TYPES (TREE_TYPE (new_decl)) != NULL)))
	    {
	      /* The situation here is that (in C) somebody was smart
		 enough to use proper declarations in a header file, but
		 the actual definition of the function uses
		 non-ANSI-style argument lists.  Or we have a situation
		 where declarations weren't used anywhere and we're
		 merging the actual definition with a use.  One of the
		 decls will then have a complete function type, whereas
		 the other will only have a result type.  Assume that
		 the more complete type is the right one and don't
		 complain.  */
	      if (TYPE_ARG_TYPES (TREE_TYPE (old_decl)))
		{
		  merged_type = TREE_TYPE (old_decl);
		}
	      else
		{
		  merged_type = TREE_TYPE (new_decl);
		}
	    }

	  /* If we don't have a merged type yet...sigh.  The linker
	     wouldn't complain if the types were mismatched, so we
	     probably shouldn't either.  Just use the type from
	     whichever decl appears to be associated with the
	     definition.  If for some odd reason neither decl is, the
	     older one wins.  */
	  if (!merged_type)
	    {
	      if (!DECL_EXTERNAL (new_decl))
		{
		  merged_type = TREE_TYPE (new_decl);
		}
	      else
		{
		  merged_type = TREE_TYPE (old_decl);
		}
	    }
	}

      if (!merged_type)
	{
	  if (warning_at (DECL_SOURCE_LOCATION (new_decl), 0,
			  "type of %qD does not match original declaration",
			  new_decl))
	    inform (DECL_SOURCE_LOCATION (old_decl),
		    "previously declared here");
	  return false;
	}
    }

  if (DECL_UNSIGNED (old_decl) != DECL_UNSIGNED (new_decl))
    {
      error_at (DECL_SOURCE_LOCATION (new_decl),
		"signedness of %qD does not match original declaration",
		new_decl);
      inform (DECL_SOURCE_LOCATION (old_decl), "previously declared here");
      return false;
    }

  if (!tree_int_cst_equal (DECL_SIZE (old_decl),
			   DECL_SIZE (new_decl))
      || !tree_int_cst_equal (DECL_SIZE_UNIT (old_decl),
			      DECL_SIZE_UNIT (new_decl)))
    {
      /* Permit cases where we are declaring aggregates and at least one
	 of the decls is external and one of the decls has a size whereas
	 the other one does not.  This is perfectly legal in C:

         struct s;
	 extern struct s x;

	 void*
	 f (void)
	 {
	   return &x;
	 }

	 There is no way a compiler can tell the size of x.  So we cannot
	 assume that external aggreates have complete types.  */

      if (!((TREE_CODE (TREE_TYPE (old_decl))
	     == TREE_CODE (TREE_TYPE (new_decl)))
	    && ((external_aggregate_decl_p (old_decl)
		 && DECL_SIZE (old_decl) == NULL_TREE)
		|| (external_aggregate_decl_p (new_decl)
		    && DECL_SIZE (new_decl) == NULL_TREE))))
	{
	  error_at (DECL_SOURCE_LOCATION (new_decl),
		    "size of %qD does not match original declaration",
		    new_decl);
	  inform (DECL_SOURCE_LOCATION (old_decl),
		  "previously declared here");
	  return false;
	}
    }

  /* Report an error if user-specified alignments do not match.  */
  if ((DECL_USER_ALIGN (old_decl) && DECL_USER_ALIGN (new_decl))
      && DECL_ALIGN (old_decl) != DECL_ALIGN (new_decl))
    {
      error_at (DECL_SOURCE_LOCATION (new_decl),
		"alignment of %qD does not match original declaration",
		new_decl);
      inform (DECL_SOURCE_LOCATION (old_decl), "previously declared here");
      return false;
    }

  /* Do not compare the modes of the decls.  The type compatibility
     checks or the completing of types has properly dealt with all issues.  */

  if (!lto_compatible_attributes_p (old_decl,
				    DECL_ATTRIBUTES (old_decl),
				    DECL_ATTRIBUTES (new_decl)))
    {
      error_at (DECL_SOURCE_LOCATION (new_decl),
		"attributes applied to %qD are incompatible with original "
		"declaration", new_decl);
      inform (DECL_SOURCE_LOCATION (old_decl), "previously declared here");
      return false;
    }

  /* We do not require matches for:

     - DECL_NAME

       Only the name used in object files matters.

     - DECL_CONTEXT  

       An entity might be declared in a C++ namespace in one file and
       with a C identifier in another file.  

     - TREE_PRIVATE, TREE_PROTECTED

       Access control is the problem of the front end that created the
       object file.  
       
     Therefore, at this point we have decided to merge the declarations.  */
  return true;
}


/* Marks decl DECL as having resolution RESOLUTION. */

static void
lto_symtab_set_resolution_and_file_data (tree decl,
					 ld_plugin_symbol_resolution_t
					 resolution,
					 struct lto_file_decl_data *file_data)
{
  lto_symtab_decl_t new_entry;
  void **slot;

  gcc_assert (decl);

  gcc_assert (TREE_PUBLIC (decl));
  gcc_assert (TREE_CODE (decl) != FUNCTION_DECL || !DECL_ABSTRACT (decl));

  new_entry = GGC_CNEW (struct lto_symtab_decl_def);
  new_entry->base.node = decl;
  new_entry->resolution = resolution;
  new_entry->file_data = file_data;
  
  lto_symtab_maybe_init_hash_tables ();
  slot = htab_find_slot (lto_symtab_decls, new_entry, INSERT);
  gcc_assert (!*slot);
  *slot = new_entry;
}

/* Get the lto_symtab_identifier_def struct associated with ID
   if there is one.  If there is none and INSERT_P is true, create
   a new one.  */

static lto_symtab_identifier_t
lto_symtab_get_identifier (tree id, bool insert_p)
{
  struct lto_symtab_identifier_def temp;
  lto_symtab_identifier_t symtab_id;
  void **slot;

  lto_symtab_maybe_init_hash_tables ();
  temp.base.node = id;
  slot = htab_find_slot (lto_symtab_identifiers, &temp,
			 insert_p ? INSERT : NO_INSERT);
  if (insert_p)
    {
      if (*slot)
	return (lto_symtab_identifier_t) *slot;
      else
	{
	  symtab_id = GGC_CNEW (struct lto_symtab_identifier_def);
	  symtab_id->base.node = id;
	  *slot = symtab_id;
	  return symtab_id;
	}
    }
  else
    return slot ? (lto_symtab_identifier_t) *slot : NULL;
}

/* Return the DECL associated with an IDENTIFIER ID or return NULL_TREE
   if there is none.  */

static tree
lto_symtab_get_identifier_decl (tree id)
{
  lto_symtab_identifier_t symtab_id = lto_symtab_get_identifier (id, false);
  return symtab_id ? symtab_id->decl : NULL_TREE;
}

/* SET the associated DECL of an IDENTIFIER ID to be DECL.  */

static void
lto_symtab_set_identifier_decl (tree id, tree decl)
{
  lto_symtab_identifier_t symtab_id = lto_symtab_get_identifier (id, true);
  symtab_id->decl = decl;
}

/* Common helper function for merging variable and function declarations.
   NEW_DECL is the newly found decl. RESOLUTION is the decl's resolution
   provided by the linker. */

static void
lto_symtab_merge_decl (tree new_decl,
		       enum ld_plugin_symbol_resolution resolution,
		       struct lto_file_decl_data *file_data)
{
  tree old_decl;
  tree name;
  ld_plugin_symbol_resolution_t old_resolution;

  gcc_assert (TREE_CODE (new_decl) == VAR_DECL
	      || TREE_CODE (new_decl) == FUNCTION_DECL);

  gcc_assert (TREE_PUBLIC (new_decl));

  gcc_assert (DECL_LANG_SPECIFIC (new_decl) == NULL);

  /* Check that declarations reaching this function do not have
     properties inconsistent with having external linkage.  If any of
     these asertions fail, then the object file reader has failed to
     detect these cases and issue appropriate error messages.  */
  if (TREE_CODE (new_decl) == VAR_DECL)
    gcc_assert (!(DECL_EXTERNAL (new_decl) && DECL_INITIAL (new_decl)));

  /* Remember the resolution of this symbol. */
  lto_symtab_set_resolution_and_file_data (new_decl, resolution, file_data);

  /* Ensure DECL_ASSEMBLER_NAME will not set assembler name.  */
  gcc_assert (DECL_ASSEMBLER_NAME_SET_P (new_decl));

  /* Retrieve the previous declaration.  */
  name = DECL_ASSEMBLER_NAME (new_decl);
  old_decl = lto_symtab_get_identifier_decl (name);

  /* If there was no previous declaration, then there is nothing to
     merge.  */
  if (!old_decl)
    {
      lto_symtab_set_identifier_decl (name, new_decl);
      VEC_safe_push (tree, gc, lto_global_var_decls, new_decl);
      return;
    }

  /* Give ODR violation errors.  */
  old_resolution = lto_symtab_get_resolution (old_decl);
  if (resolution == LDPR_PREVAILING_DEF
      || resolution == LDPR_PREVAILING_DEF_IRONLY)
    {
      if ((old_resolution == LDPR_PREVAILING_DEF
	   || old_resolution == LDPR_PREVAILING_DEF_IRONLY)
	  && (old_resolution != resolution || flag_no_common))
	{
	  error_at (DECL_SOURCE_LOCATION (new_decl),
		    "%qD has already been defined", new_decl);
	  inform (DECL_SOURCE_LOCATION (old_decl),
		  "previously defined here");
	  return;
	}
    }

  /* The linker may ask us to combine two incompatible symbols.
     Find a decl we can merge with or chain it in the list of decls
     for that symbol.  */
  while (old_decl
	 && !lto_symtab_compatible (old_decl, new_decl))
    old_decl = (tree) DECL_LANG_SPECIFIC (old_decl);
  if (!old_decl)
    {
      old_decl = lto_symtab_get_identifier_decl (name);
      while (DECL_LANG_SPECIFIC (old_decl) != NULL)
	old_decl = (tree) DECL_LANG_SPECIFIC (old_decl);
      DECL_LANG_SPECIFIC (old_decl) = (struct lang_decl *) new_decl;
      return;
    }

  /* Merge decl state in both directions, we may still end up using
     the new decl.  */
  TREE_ADDRESSABLE (old_decl) |= TREE_ADDRESSABLE (new_decl);
  TREE_ADDRESSABLE (new_decl) |= TREE_ADDRESSABLE (old_decl);

  gcc_assert (resolution != LDPR_UNKNOWN
	      && resolution != LDPR_UNDEF
	      && old_resolution != LDPR_UNKNOWN
	      && old_resolution != LDPR_UNDEF);

  if (resolution == LDPR_PREVAILING_DEF
      || resolution == LDPR_PREVAILING_DEF_IRONLY)
    {
      tree decl;
      gcc_assert (old_resolution == LDPR_PREEMPTED_IR
		  || old_resolution ==  LDPR_RESOLVED_IR
		  || (old_resolution == resolution && !flag_no_common));
      DECL_LANG_SPECIFIC (new_decl) = DECL_LANG_SPECIFIC (old_decl);
      DECL_LANG_SPECIFIC (old_decl) = NULL;
      decl = lto_symtab_get_identifier_decl (name);
      if (decl == old_decl)
	{
	  lto_symtab_set_identifier_decl (name, new_decl);
	  return;
	}
      while ((tree) DECL_LANG_SPECIFIC (decl) != old_decl)
	decl = (tree) DECL_LANG_SPECIFIC (decl);
      DECL_LANG_SPECIFIC (decl) = (struct lang_decl *) new_decl;
      return;
    }

  if (resolution == LDPR_PREEMPTED_REG
      || resolution == LDPR_RESOLVED_EXEC
      || resolution == LDPR_RESOLVED_DYN)
    gcc_assert (old_resolution == LDPR_PREEMPTED_REG
		|| old_resolution == LDPR_RESOLVED_EXEC
		|| old_resolution == LDPR_RESOLVED_DYN);

  if (resolution == LDPR_PREEMPTED_IR
      || resolution == LDPR_RESOLVED_IR)
    gcc_assert (old_resolution == LDPR_PREVAILING_DEF
		|| old_resolution == LDPR_PREVAILING_DEF_IRONLY
		|| old_resolution == LDPR_PREEMPTED_IR
		|| old_resolution == LDPR_RESOLVED_IR);

  return;
}


/* Merge the VAR_DECL NEW_VAR with resolution RESOLUTION with any previous
   declaration with the same name. */

void
lto_symtab_merge_var (tree new_var, enum ld_plugin_symbol_resolution resolution)
{
  lto_symtab_merge_decl (new_var, resolution, NULL);
}

/* Merge the FUNCTION_DECL NEW_FN with resolution RESOLUTION with any previous
   declaration with the same name. */

void
lto_symtab_merge_fn (tree new_fn, enum ld_plugin_symbol_resolution resolution,
		     struct lto_file_decl_data *file_data)
{
  lto_symtab_merge_decl (new_fn, resolution, file_data);
}

/* Given the decl DECL, return the prevailing decl with the same name. */

tree
lto_symtab_prevailing_decl (tree decl)
{
  tree ret;
  gcc_assert (decl);

  /* Builtins and local symbols are their own prevailing decl.  */
  if (!TREE_PUBLIC (decl) || is_builtin_fn (decl))
    return decl;

  /* DECL_ABSTRACTs are their own prevailng decl.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_ABSTRACT (decl))
    return decl;

  /* Ensure DECL_ASSEMBLER_NAME will not set assembler name.  */
  gcc_assert (DECL_ASSEMBLER_NAME_SET_P (decl));

  /* Walk through the list of candidates and return the one we merged to.  */
  ret = lto_symtab_get_identifier_decl (DECL_ASSEMBLER_NAME (decl));
  if (!ret
      || DECL_LANG_SPECIFIC (ret) == NULL)
    return ret;

  /* If there are multiple decls to choose from find the one we merged
     with and return that.  */
  while (ret)
    {
      if (gimple_types_compatible_p (TREE_TYPE (decl), TREE_TYPE (ret)))
	return ret;

      ret = (tree) DECL_LANG_SPECIFIC (ret);
    }

  gcc_unreachable ();
}

/* Return the hash table entry of DECL. */

static struct lto_symtab_decl_def *
lto_symtab_get_symtab_def (tree decl)
{
  struct lto_symtab_decl_def temp, *symtab_decl;
  void **slot;

  gcc_assert (decl);

  lto_symtab_maybe_init_hash_tables ();
  temp.base.node = decl;
  slot = htab_find_slot (lto_symtab_decls, &temp, NO_INSERT);
  gcc_assert (slot && *slot);
  symtab_decl = (struct lto_symtab_decl_def*) *slot;
  return symtab_decl;
}

/* Return the resolution of DECL. */

enum ld_plugin_symbol_resolution
lto_symtab_get_resolution (tree decl)
{
  gcc_assert (decl);

  if (!TREE_PUBLIC (decl) || is_builtin_fn (decl))
    return LDPR_PREVAILING_DEF_IRONLY;

  /* FIXME lto: There should be no DECL_ABSTRACT in the middle end. */
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_ABSTRACT (decl))
    return LDPR_PREVAILING_DEF_IRONLY;

  return lto_symtab_get_symtab_def (decl)->resolution;
}

/* Return the file of DECL. */

struct lto_file_decl_data *
lto_symtab_get_file_data (tree decl)
{
  return lto_symtab_get_symtab_def (decl)->file_data;
}

/* Remove any storage used to store resolution of DECL.  */

void
lto_symtab_clear_resolution (tree decl)
{
  struct lto_symtab_decl_def temp;
  gcc_assert (decl);

  if (!TREE_PUBLIC (decl))
    return;

  /* LTO FIXME: There should be no DECL_ABSTRACT in the middle end. */
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_ABSTRACT (decl))
    return;

  lto_symtab_maybe_init_hash_tables ();
  temp.base.node = decl;
  htab_remove_elt (lto_symtab_decls, &temp);
}

#include "gt-lto-symtab.h"
