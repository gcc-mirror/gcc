/* Breadth-first and depth-first routines for
   searching multiple-inheritance lattice for GNU C++.
   Copyright (C) 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

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
#include "system.h"
#include "tree.h"
#include "cp-tree.h"
#include "obstack.h"
#include "flags.h"
#include "rtl.h"
#include "output.h"
#include "ggc.h"
#include "toplev.h"
#include "stack.h"

/* Obstack used for remembering decision points of breadth-first.  */

static struct obstack search_obstack;

/* Methods for pushing and popping objects to and from obstacks.  */

struct stack_level *
push_stack_level (obstack, tp, size)
     struct obstack *obstack;
     char *tp;  /* Sony NewsOS 5.0 compiler doesn't like void * here.  */
     int size;
{
  struct stack_level *stack;
  obstack_grow (obstack, tp, size);
  stack = (struct stack_level *) ((char*)obstack_next_free (obstack) - size);
  obstack_finish (obstack);
  stack->obstack = obstack;
  stack->first = (tree *) obstack_base (obstack);
  stack->limit = obstack_room (obstack) / sizeof (tree *);
  return stack;
}

struct stack_level *
pop_stack_level (stack)
     struct stack_level *stack;
{
  struct stack_level *tem = stack;
  struct obstack *obstack = tem->obstack;
  stack = tem->prev;
  obstack_free (obstack, tem);
  return stack;
}

#define search_level stack_level
static struct search_level *search_stack;

struct vbase_info 
{
  /* The class dominating the hierarchy.  */
  tree type;
  /* A pointer to a complete object of the indicated TYPE.  */
  tree decl_ptr;
  tree inits;
};

static int is_subobject_of_p PARAMS ((tree, tree, tree));
static int is_subobject_of_p_1 PARAMS ((tree, tree, tree));
static tree dfs_check_overlap PARAMS ((tree, void *));
static tree dfs_no_overlap_yet PARAMS ((tree, void *));
static base_kind lookup_base_r
	PARAMS ((tree, tree, base_access, int, tree *));
static int dynamic_cast_base_recurse PARAMS ((tree, tree, int, tree *));
static tree marked_pushdecls_p PARAMS ((tree, void *));
static tree unmarked_pushdecls_p PARAMS ((tree, void *));
static tree dfs_debug_unmarkedp PARAMS ((tree, void *));
static tree dfs_debug_mark PARAMS ((tree, void *));
static tree dfs_get_vbase_types PARAMS ((tree, void *));
static tree dfs_push_type_decls PARAMS ((tree, void *));
static tree dfs_push_decls PARAMS ((tree, void *));
static tree dfs_unuse_fields PARAMS ((tree, void *));
static tree add_conversions PARAMS ((tree, void *));
static int covariant_return_p PARAMS ((tree, tree));
static int look_for_overrides_r PARAMS ((tree, tree));
static struct search_level *push_search_level
	PARAMS ((struct stack_level *, struct obstack *));
static struct search_level *pop_search_level
	PARAMS ((struct stack_level *));
static tree bfs_walk
	PARAMS ((tree, tree (*) (tree, void *), tree (*) (tree, void *),
	       void *));
static tree lookup_field_queue_p PARAMS ((tree, void *));
static int shared_member_p PARAMS ((tree));
static tree lookup_field_r PARAMS ((tree, void *));
static tree canonical_binfo PARAMS ((tree));
static tree shared_marked_p PARAMS ((tree, void *));
static tree shared_unmarked_p PARAMS ((tree, void *));
static int  dependent_base_p PARAMS ((tree));
static tree dfs_accessible_queue_p PARAMS ((tree, void *));
static tree dfs_accessible_p PARAMS ((tree, void *));
static tree dfs_access_in_type PARAMS ((tree, void *));
static access_kind access_in_type PARAMS ((tree, tree));
static tree dfs_canonical_queue PARAMS ((tree, void *));
static tree dfs_assert_unmarked_p PARAMS ((tree, void *));
static void assert_canonical_unmarked PARAMS ((tree));
static int protected_accessible_p PARAMS ((tree, tree, tree));
static int friend_accessible_p PARAMS ((tree, tree, tree));
static void setup_class_bindings PARAMS ((tree, int));
static int template_self_reference_p PARAMS ((tree, tree));
static tree dfs_find_vbase_instance PARAMS ((tree, void *));
static tree dfs_get_pure_virtuals PARAMS ((tree, void *));
static tree dfs_build_inheritance_graph_order PARAMS ((tree, void *));

/* Allocate a level of searching.  */

static struct search_level *
push_search_level (stack, obstack)
     struct stack_level *stack;
     struct obstack *obstack;
{
  struct search_level tem;

  tem.prev = stack;
  return push_stack_level (obstack, (char *)&tem, sizeof (tem));
}

/* Discard a level of search allocation.  */

static struct search_level *
pop_search_level (obstack)
     struct stack_level *obstack;
{
  register struct search_level *stack = pop_stack_level (obstack);

  return stack;
}

/* Variables for gathering statistics.  */
#ifdef GATHER_STATISTICS
static int n_fields_searched;
static int n_calls_lookup_field, n_calls_lookup_field_1;
static int n_calls_lookup_fnfields, n_calls_lookup_fnfields_1;
static int n_calls_get_base_type;
static int n_outer_fields_searched;
static int n_contexts_saved;
#endif /* GATHER_STATISTICS */


/* Worker for lookup_base.  BINFO is the binfo we are searching at,
   BASE is the RECORD_TYPE we are searching for.  ACCESS is the
   required access checks.  IS_VIRTUAL indicates if BINFO is morally
   virtual.

   If BINFO is of the required type, then *BINFO_PTR is examined to
   compare with any other instance of BASE we might have already
   discovered. *BINFO_PTR is initialized and a base_kind return value
   indicates what kind of base was located.

   Otherwise BINFO's bases are searched.  */

static base_kind
lookup_base_r (binfo, base, access, is_virtual, binfo_ptr)
     tree binfo, base;
     base_access access;
     int is_virtual;		/* inside a virtual part */
     tree *binfo_ptr;
{
  int i;
  tree bases;
  base_kind found = bk_not_base;
  
  if (same_type_p (BINFO_TYPE (binfo), base))
    {
      /* We have found a base. Check against what we have found
         already.  */
      found = bk_same_type;
      if (is_virtual)
	found = bk_via_virtual;
      
      if (!*binfo_ptr)
	*binfo_ptr = binfo;
      else if (!is_virtual || !tree_int_cst_equal (BINFO_OFFSET (binfo),
						   BINFO_OFFSET (*binfo_ptr)))
	{
	  if (access != ba_any)
	    *binfo_ptr = NULL;
	  else if (!is_virtual)
	    /* Prefer a non-virtual base.  */
	    *binfo_ptr = binfo;
	  found = bk_ambig;
	}
      
      return found;
    }
  
  bases = BINFO_BASETYPES (binfo);
  if (!bases)
    return bk_not_base;
  
  for (i = TREE_VEC_LENGTH (bases); i--;)
    {
      tree base_binfo = TREE_VEC_ELT (bases, i);
      base_kind bk;

      bk = lookup_base_r (base_binfo, base,
		    	  access,
			  is_virtual || TREE_VIA_VIRTUAL (base_binfo),
			  binfo_ptr);

      switch (bk)
	{
	case bk_ambig:
	  if (access != ba_any)
	    return bk;
	  found = bk;
	  break;
	  
	case bk_same_type:
	  bk = bk_proper_base;
	  /* FALLTHROUGH */
	case bk_proper_base:
	  my_friendly_assert (found == bk_not_base, 20010723);
	  found = bk;
	  break;
	  
	case bk_via_virtual:
	  if (found != bk_ambig)
	    found = bk;
	  break;
	  
	case bk_not_base:
	  break;

	default:
	  abort ();
	}
    }
  return found;
}

/* Lookup BASE in the hierarchy dominated by T.  Do access checking as
   ACCESS specifies.  Return the binfo we discover (which might not be
   canonical).  If KIND_PTR is non-NULL, fill with information about
   what kind of base we discovered.

   If the base is inaccessible, or ambiguous, and the ba_quiet bit is
   not set in ACCESS, then an error is issued and error_mark_node is
   returned.  If the ba_quiet bit is set, then no error is issued and
   NULL_TREE is returned.  */

tree
lookup_base (t, base, access, kind_ptr)
     tree t, base;
     base_access access;
     base_kind *kind_ptr;
{
  tree binfo = NULL;		/* The binfo we've found so far.  */
  tree t_binfo = NULL;
  base_kind bk;
  
  if (t == error_mark_node || base == error_mark_node)
    {
      if (kind_ptr)
	*kind_ptr = bk_not_base;
      return error_mark_node;
    }
  my_friendly_assert (TYPE_P (base), 20011127);
  
  if (!TYPE_P (t))
    {
      t_binfo = t;
      t = BINFO_TYPE (t);
    }
  else 
    t_binfo = TYPE_BINFO (t);

  /* Ensure that the types are instantiated.  */
  t = complete_type (TYPE_MAIN_VARIANT (t));
  base = complete_type (TYPE_MAIN_VARIANT (base));
  
  bk = lookup_base_r (t_binfo, base, access, 0, &binfo);

  /* Check that the base is unambiguous and accessible.  */
  if (access != ba_any)
    switch (bk)
      {
      case bk_not_base:
	break;

      case bk_ambig:
	binfo = NULL_TREE;
	if (!(access & ba_quiet))
	  {
	    error ("`%T' is an ambiguous base of `%T'", base, t);
	    binfo = error_mark_node;
	  }
	break;

      default:
	if (access != ba_ignore
	    /* If BASE is incomplete, then BASE and TYPE are probably
	       the same, in which case BASE is accessible.  If they
	       are not the same, then TYPE is invalid.  In that case,
	       there's no need to issue another error here, and
	       there's no implicit typedef to use in the code that
	       follows, so we skip the check.  */
	    && COMPLETE_TYPE_P (base))
	  {
	    tree decl;

	    /* [class.access.base]

	       A base class is said to be accessible if an invented public
	       member of the base class is accessible.  */
	    /* Rather than inventing a public member, we use the implicit
	       public typedef created in the scope of every class.  */
	    decl = TYPE_FIELDS (base);
	    while (TREE_CODE (decl) != TYPE_DECL
		   || !DECL_ARTIFICIAL (decl)
		   || DECL_NAME (decl) != constructor_name (base))
	      decl = TREE_CHAIN (decl);
	    while (ANON_AGGR_TYPE_P (t))
	      t = TYPE_CONTEXT (t);
	    if (!accessible_p (t, decl))
	      {
		if (!(access & ba_quiet))
		  {
		    error ("`%T' is an inaccessible base of `%T'", base, t);
		    binfo = error_mark_node;
		  }
		else
		  binfo = NULL_TREE;
		bk = bk_inaccessible;
	      }
	  }
	break;
      }

  if (kind_ptr)
    *kind_ptr = bk;
  
  return binfo;
}

/* Worker function for get_dynamic_cast_base_type.  */

static int
dynamic_cast_base_recurse (subtype, binfo, via_virtual, offset_ptr)
     tree subtype;
     tree binfo;
     int via_virtual;
     tree *offset_ptr;
{
  tree binfos;
  int i, n_baselinks;
  int worst = -2;
  
  if (BINFO_TYPE (binfo) == subtype)
    {
      if (via_virtual)
        return -1;
      else
        {
          *offset_ptr = BINFO_OFFSET (binfo);
          return 0;
        }
    }
  
  binfos = BINFO_BASETYPES (binfo);
  n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;
  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      int rval;
      
      if (!TREE_VIA_PUBLIC (base_binfo))
        continue;
      rval = dynamic_cast_base_recurse
             (subtype, base_binfo,
              via_virtual || TREE_VIA_VIRTUAL (base_binfo), offset_ptr);
      if (worst == -2)
        worst = rval;
      else if (rval >= 0)
        worst = worst >= 0 ? -3 : worst;
      else if (rval == -1)
        worst = -1;
      else if (rval == -3 && worst != -1)
        worst = -3;
    }
  return worst;
}

/* The dynamic cast runtime needs a hint about how the static SUBTYPE type
   started from is related to the required TARGET type, in order to optimize
   the inheritance graph search. This information is independent of the
   current context, and ignores private paths, hence get_base_distance is
   inappropriate. Return a TREE specifying the base offset, BOFF.
   BOFF >= 0, there is only one public non-virtual SUBTYPE base at offset BOFF,
      and there are no public virtual SUBTYPE bases.
   BOFF == -1, SUBTYPE occurs as multiple public virtual or non-virtual bases.
   BOFF == -2, SUBTYPE is not a public base.
   BOFF == -3, SUBTYPE occurs as multiple public non-virtual bases.  */

tree
get_dynamic_cast_base_type (subtype, target)
     tree subtype;
     tree target;
{
  tree offset = NULL_TREE;
  int boff = dynamic_cast_base_recurse (subtype, TYPE_BINFO (target),
                                        0, &offset);
  
  if (!boff)
    return offset;
  offset = build_int_2 (boff, -1);
  TREE_TYPE (offset) = ssizetype;
  return offset;
}

/* Search for a member with name NAME in a multiple inheritance lattice
   specified by TYPE.  If it does not exist, return NULL_TREE.
   If the member is ambiguously referenced, return `error_mark_node'.
   Otherwise, return the FIELD_DECL.  */

/* Do a 1-level search for NAME as a member of TYPE.  The caller must
   figure out whether it can access this field.  (Since it is only one
   level, this is reasonable.)  */

tree
lookup_field_1 (tree type, tree name, bool want_type)
{
  register tree field;

  if (TREE_CODE (type) == TEMPLATE_TYPE_PARM
      || TREE_CODE (type) == BOUND_TEMPLATE_TEMPLATE_PARM
      || TREE_CODE (type) == TYPENAME_TYPE)
    /* The TYPE_FIELDS of a TEMPLATE_TYPE_PARM and 
       BOUND_TEMPLATE_TEMPLATE_PARM are not fields at all;
       instead TYPE_FIELDS is the TEMPLATE_PARM_INDEX.  (Miraculously,
       the code often worked even when we treated the index as a list
       of fields!)
       The TYPE_FIELDS of TYPENAME_TYPE is its TYPENAME_TYPE_FULLNAME.  */
    return NULL_TREE;

  if (TYPE_NAME (type)
      && DECL_LANG_SPECIFIC (TYPE_NAME (type))
      && DECL_SORTED_FIELDS (TYPE_NAME (type)))
    {
      tree *fields = &TREE_VEC_ELT (DECL_SORTED_FIELDS (TYPE_NAME (type)), 0);
      int lo = 0, hi = TREE_VEC_LENGTH (DECL_SORTED_FIELDS (TYPE_NAME (type)));
      int i;

      while (lo < hi)
	{
	  i = (lo + hi) / 2;

#ifdef GATHER_STATISTICS
	  n_fields_searched++;
#endif /* GATHER_STATISTICS */

	  if (DECL_NAME (fields[i]) > name)
	    hi = i;
	  else if (DECL_NAME (fields[i]) < name)
	    lo = i + 1;
	  else
	    {
	      field = NULL_TREE;

	      /* We might have a nested class and a field with the
		 same name; we sorted them appropriately via
		 field_decl_cmp, so just look for the first or last
		 field with this name.  */
	      if (want_type)
		{
		  do
		    field = fields[i--];
		  while (i >= lo && DECL_NAME (fields[i]) == name);
		  if (TREE_CODE (field) != TYPE_DECL
		      && !DECL_CLASS_TEMPLATE_P (field))
		    field = NULL_TREE;
		}
	      else
		{
		  do
		    field = fields[i++];
		  while (i < hi && DECL_NAME (fields[i]) == name);
		}
	      return field;
	    }
	}
      return NULL_TREE;
    }

  field = TYPE_FIELDS (type);

#ifdef GATHER_STATISTICS
  n_calls_lookup_field_1++;
#endif /* GATHER_STATISTICS */
  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
#ifdef GATHER_STATISTICS
      n_fields_searched++;
#endif /* GATHER_STATISTICS */
      my_friendly_assert (DECL_P (field), 0);
      if (DECL_NAME (field) == NULL_TREE
	  && ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	{
	  tree temp = lookup_field_1 (TREE_TYPE (field), name, want_type);
	  if (temp)
	    return temp;
	}
      if (TREE_CODE (field) == USING_DECL)
	/* For now, we're just treating member using declarations as
	   old ARM-style access declarations.  Thus, there's no reason
	   to return a USING_DECL, and the rest of the compiler can't
	   handle it.  Once the class is defined, these are purged
	   from TYPE_FIELDS anyhow; see handle_using_decl.  */
	continue;

      if (DECL_NAME (field) == name
	  && (!want_type 
	      || TREE_CODE (field) == TYPE_DECL
	      || DECL_CLASS_TEMPLATE_P (field)))
	return field;
    }
  /* Not found.  */
  if (name == vptr_identifier)
    {
      /* Give the user what s/he thinks s/he wants.  */
      if (TYPE_POLYMORPHIC_P (type))
	return TYPE_VFIELD (type);
    }
  return NULL_TREE;
}

/* There are a number of cases we need to be aware of here:
			 current_class_type	current_function_decl
     global			NULL			NULL
     fn-local			NULL			SET
     class-local		SET			NULL
     class->fn			SET			SET
     fn->class			SET			SET

   Those last two make life interesting.  If we're in a function which is
   itself inside a class, we need decls to go into the fn's decls (our
   second case below).  But if we're in a class and the class itself is
   inside a function, we need decls to go into the decls for the class.  To
   achieve this last goal, we must see if, when both current_class_ptr and
   current_function_decl are set, the class was declared inside that
   function.  If so, we know to put the decls into the class's scope.  */

tree
current_scope ()
{
  if (current_function_decl == NULL_TREE)
    return current_class_type;
  if (current_class_type == NULL_TREE)
    return current_function_decl;
  if ((DECL_FUNCTION_MEMBER_P (current_function_decl)
       && same_type_p (DECL_CONTEXT (current_function_decl),
		       current_class_type))
      || (DECL_FRIEND_CONTEXT (current_function_decl)
	  && same_type_p (DECL_FRIEND_CONTEXT (current_function_decl),
			  current_class_type)))
    return current_function_decl;

  return current_class_type;
}

/* Returns nonzero if we are currently in a function scope.  Note
   that this function returns zero if we are within a local class, but
   not within a member function body of the local class.  */

int
at_function_scope_p ()
{
  tree cs = current_scope ();
  return cs && TREE_CODE (cs) == FUNCTION_DECL;
}

/* Returns true if the innermost active scope is a class scope.  */

bool
at_class_scope_p ()
{
  tree cs = current_scope ();
  return cs && TYPE_P (cs);
}

/* Return the scope of DECL, as appropriate when doing name-lookup.  */

tree
context_for_name_lookup (decl)
     tree decl;
{
  /* [class.union]
     
     For the purposes of name lookup, after the anonymous union
     definition, the members of the anonymous union are considered to
     have been defined in the scope in which the anonymous union is
     declared.  */ 
  tree context = DECL_CONTEXT (decl);

  while (context && TYPE_P (context) && ANON_AGGR_TYPE_P (context))
    context = TYPE_CONTEXT (context);
  if (!context)
    context = global_namespace;

  return context;
}

/* Return a canonical BINFO if BINFO is a virtual base, or just BINFO
   otherwise.  */

static tree
canonical_binfo (binfo)
     tree binfo;
{
  return (TREE_VIA_VIRTUAL (binfo)
	  ? TYPE_BINFO (BINFO_TYPE (binfo)) : binfo);
}

/* A queue function that simply ensures that we walk into the
   canonical versions of virtual bases.  */

static tree
dfs_canonical_queue (binfo, data)
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{
  return canonical_binfo (binfo);
}

/* Called via dfs_walk from assert_canonical_unmarked.  */

static tree
dfs_assert_unmarked_p (binfo, data)
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{
  my_friendly_assert (!BINFO_MARKED (binfo), 0);
  return NULL_TREE;
}

/* Asserts that all the nodes below BINFO (using the canonical
   versions of virtual bases) are unmarked.  */

static void
assert_canonical_unmarked (binfo)
     tree binfo;
{
  dfs_walk (binfo, dfs_assert_unmarked_p, dfs_canonical_queue, 0);
}

/* If BINFO is marked, return a canonical version of BINFO.
   Otherwise, return NULL_TREE.  */

static tree
shared_marked_p (binfo, data)
     tree binfo;
     void *data;
{
  binfo = canonical_binfo (binfo);
  return markedp (binfo, data);
}

/* If BINFO is not marked, return a canonical version of BINFO.
   Otherwise, return NULL_TREE.  */

static tree
shared_unmarked_p (binfo, data)
     tree binfo;
     void *data;
{
  binfo = canonical_binfo (binfo);
  return unmarkedp (binfo, data);
}

/* The accessibility routines use BINFO_ACCESS for scratch space
   during the computation of the accssibility of some declaration.  */

#define BINFO_ACCESS(NODE) \
  ((access_kind) ((TREE_LANG_FLAG_1 (NODE) << 1) | TREE_LANG_FLAG_6 (NODE)))

/* Set the access associated with NODE to ACCESS.  */

#define SET_BINFO_ACCESS(NODE, ACCESS)			\
  ((TREE_LANG_FLAG_1 (NODE) = ((ACCESS) & 2) != 0),	\
   (TREE_LANG_FLAG_6 (NODE) = ((ACCESS) & 1) != 0))

/* Called from access_in_type via dfs_walk.  Calculate the access to
   DATA (which is really a DECL) in BINFO.  */

static tree
dfs_access_in_type (binfo, data)
     tree binfo;
     void *data;
{
  tree decl = (tree) data;
  tree type = BINFO_TYPE (binfo);
  access_kind access = ak_none;

  if (context_for_name_lookup (decl) == type)
    {
      /* If we have desceneded to the scope of DECL, just note the
	 appropriate access.  */
      if (TREE_PRIVATE (decl))
	access = ak_private;
      else if (TREE_PROTECTED (decl))
	access = ak_protected;
      else
	access = ak_public;
    }
  else 
    {
      /* First, check for an access-declaration that gives us more
	 access to the DECL.  The CONST_DECL for an enumeration
	 constant will not have DECL_LANG_SPECIFIC, and thus no
	 DECL_ACCESS.  */
      if (DECL_LANG_SPECIFIC (decl) && !DECL_DISCRIMINATOR_P (decl))
	{
	  tree decl_access = purpose_member (type, DECL_ACCESS (decl));
	  if (decl_access)
	    access = ((access_kind) 
		      TREE_INT_CST_LOW (TREE_VALUE (decl_access)));
	}

      if (!access)
	{
	  int i;
	  int n_baselinks;
	  tree binfos;
	  
	  /* Otherwise, scan our baseclasses, and pick the most favorable
	     access.  */
	  binfos = BINFO_BASETYPES (binfo);
	  n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;
	  for (i = 0; i < n_baselinks; ++i)
	    {
	      tree base_binfo = TREE_VEC_ELT (binfos, i);
	      access_kind base_access 
		= BINFO_ACCESS (canonical_binfo (base_binfo));

	      if (base_access == ak_none || base_access == ak_private)
		/* If it was not accessible in the base, or only
		   accessible as a private member, we can't access it
		   all.  */
		base_access = ak_none;
	      else if (TREE_VIA_PROTECTED (base_binfo))
		/* Public and protected members in the base are
		   protected here.  */
		base_access = ak_protected;
	      else if (!TREE_VIA_PUBLIC (base_binfo))
		/* Public and protected members in the base are
		   private here.  */
		base_access = ak_private;

	      /* See if the new access, via this base, gives more
		 access than our previous best access.  */
	      if (base_access != ak_none
		  && (base_access == ak_public
		      || (base_access == ak_protected
			  && access != ak_public)
		      || (base_access == ak_private 
			  && access == ak_none)))
		{
		  access = base_access;

		  /* If the new access is public, we can't do better.  */
		  if (access == ak_public)
		    break;
		}
	    }
	}
    }

  /* Note the access to DECL in TYPE.  */
  SET_BINFO_ACCESS (binfo, access);

  /* Mark TYPE as visited so that if we reach it again we do not
     duplicate our efforts here.  */
  SET_BINFO_MARKED (binfo);

  return NULL_TREE;
}

/* Return the access to DECL in TYPE.  */

static access_kind
access_in_type (type, decl)
     tree type;
     tree decl;
{
  tree binfo = TYPE_BINFO (type);

  /* We must take into account

       [class.paths]

       If a name can be reached by several paths through a multiple
       inheritance graph, the access is that of the path that gives
       most access.  

    The algorithm we use is to make a post-order depth-first traversal
    of the base-class hierarchy.  As we come up the tree, we annotate
    each node with the most lenient access.  */
  dfs_walk_real (binfo, 0, dfs_access_in_type, shared_unmarked_p, decl);
  dfs_walk (binfo, dfs_unmark, shared_marked_p,  0);
  assert_canonical_unmarked (binfo);

  return BINFO_ACCESS (binfo);
}

/* Called from dfs_accessible_p via dfs_walk.  */

static tree
dfs_accessible_queue_p (binfo, data)
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{
  if (BINFO_MARKED (binfo))
    return NULL_TREE;

  /* If this class is inherited via private or protected inheritance,
     then we can't see it, unless we are a friend of the subclass.  */
  if (!TREE_VIA_PUBLIC (binfo)
      && !is_friend (BINFO_TYPE (BINFO_INHERITANCE_CHAIN (binfo)),
		     current_scope ()))
    return NULL_TREE;

  return canonical_binfo (binfo);
}

/* Called from dfs_accessible_p via dfs_walk.  */

static tree
dfs_accessible_p (binfo, data)
     tree binfo;
     void *data;
{
  int protected_ok = data != 0;
  access_kind access;

  SET_BINFO_MARKED (binfo);
  access = BINFO_ACCESS (binfo);
  if (access == ak_public || (access == ak_protected && protected_ok))
    return binfo;
  else if (access != ak_none
	   && is_friend (BINFO_TYPE (binfo), current_scope ()))
    return binfo;

  return NULL_TREE;
}

/* Returns nonzero if it is OK to access DECL through an object
   indiated by BINFO in the context of DERIVED.  */

static int
protected_accessible_p (decl, derived, binfo)
     tree decl;
     tree derived;
     tree binfo;
{
  access_kind access;

  /* We're checking this clause from [class.access.base]

       m as a member of N is protected, and the reference occurs in a
       member or friend of class N, or in a member or friend of a
       class P derived from N, where m as a member of P is private or
       protected.  

    Here DERIVED is a possible P and DECL is m.  accessible_p will
    iterate over various values of N, but the access to m in DERIVED
    does not change.

    Note that I believe that the passage above is wrong, and should read
    "...is private or protected or public"; otherwise you get bizarre results
    whereby a public using-decl can prevent you from accessing a protected
    member of a base.  (jason 2000/02/28)  */

  /* If DERIVED isn't derived from m's class, then it can't be a P.  */
  if (!DERIVED_FROM_P (context_for_name_lookup (decl), derived))
    return 0;

  access = access_in_type (derived, decl);

  /* If m is inaccessible in DERIVED, then it's not a P.  */
  if (access == ak_none)
    return 0;
  
  /* [class.protected]

     When a friend or a member function of a derived class references
     a protected nonstatic member of a base class, an access check
     applies in addition to those described earlier in clause
     _class.access_) Except when forming a pointer to member
     (_expr.unary.op_), the access must be through a pointer to,
     reference to, or object of the derived class itself (or any class
     derived from that class) (_expr.ref_).  If the access is to form
     a pointer to member, the nested-name-specifier shall name the
     derived class (or any class derived from that class).  */
  if (DECL_NONSTATIC_MEMBER_P (decl))
    {
      /* We can tell through what the reference is occurring by
	 chasing BINFO up to the root.  */
      tree t = binfo;
      while (BINFO_INHERITANCE_CHAIN (t))
	t = BINFO_INHERITANCE_CHAIN (t);
      
      if (!DERIVED_FROM_P (derived, BINFO_TYPE (t)))
	return 0;
    }

  return 1;
}

/* Returns nonzero if SCOPE is a friend of a type which would be able
   to access DECL through the object indicated by BINFO.  */

static int
friend_accessible_p (scope, decl, binfo)
     tree scope;
     tree decl;
     tree binfo;
{
  tree befriending_classes;
  tree t;

  if (!scope)
    return 0;

  if (TREE_CODE (scope) == FUNCTION_DECL
      || DECL_FUNCTION_TEMPLATE_P (scope))
    befriending_classes = DECL_BEFRIENDING_CLASSES (scope);
  else if (TYPE_P (scope))
    befriending_classes = CLASSTYPE_BEFRIENDING_CLASSES (scope);
  else
    return 0;

  for (t = befriending_classes; t; t = TREE_CHAIN (t))
    if (protected_accessible_p (decl, TREE_VALUE (t), binfo))
      return 1;

  /* Nested classes are implicitly friends of their enclosing types, as
     per core issue 45 (this is a change from the standard).  */
  if (TYPE_P (scope))
    for (t = TYPE_CONTEXT (scope); t && TYPE_P (t); t = TYPE_CONTEXT (t))
      if (protected_accessible_p (decl, t, binfo))
	return 1;

  if (TREE_CODE (scope) == FUNCTION_DECL
      || DECL_FUNCTION_TEMPLATE_P (scope))
    {
      /* Perhaps this SCOPE is a member of a class which is a 
	 friend.  */ 
      if (DECL_CLASS_SCOPE_P (decl)
	  && friend_accessible_p (DECL_CONTEXT (scope), decl, binfo))
	return 1;

      /* Or an instantiation of something which is a friend.  */
      if (DECL_TEMPLATE_INFO (scope))
	return friend_accessible_p (DECL_TI_TEMPLATE (scope), decl, binfo);
    }
  else if (CLASSTYPE_TEMPLATE_INFO (scope))
    return friend_accessible_p (CLASSTYPE_TI_TEMPLATE (scope), decl, binfo);

  return 0;
}

/* Perform access control on TYPE_DECL or TEMPLATE_DECL VAL, which was
   looked up in TYPE.  This is fairly complex, so here's the design:

   The lang_extdef nonterminal sets type_lookups to NULL_TREE before we
     start to process a top-level declaration.
   As we process the decl-specifier-seq for the declaration, any types we
     see that might need access control are passed to type_access_control,
     which defers checking by adding them to type_lookups.
   When we are done with the decl-specifier-seq, we record the lookups we've
     seen in the lookups field of the typed_declspecs nonterminal.
   When we process the first declarator, either in parse_decl or
     begin_function_definition, we call save_type_access_control,
     which stores the lookups from the decl-specifier-seq in
     current_type_lookups.
   As we finish with each declarator, we process everything in type_lookups
     via decl_type_access_control, which resets type_lookups to the value of
     current_type_lookups for subsequent declarators.
   When we enter a function, we set type_lookups to error_mark_node, so all
     lookups are processed immediately.  */

void
type_access_control (type, val)
     tree type, val;
{
  if (val == NULL_TREE
      || (TREE_CODE (val) != TEMPLATE_DECL && TREE_CODE (val) != TYPE_DECL)
      || ! DECL_CLASS_SCOPE_P (val))
    return;

  if (type_lookups == error_mark_node)
    enforce_access (type, val);
  else if (! accessible_p (type, val))
    type_lookups = tree_cons (type, val, type_lookups);
}

/* DECL is a declaration from a base class of TYPE, which was the
   class used to name DECL.  Return nonzero if, in the current
   context, DECL is accessible.  If TYPE is actually a BINFO node,
   then we can tell in what context the access is occurring by looking
   at the most derived class along the path indicated by BINFO.  */

int 
accessible_p (type, decl)
     tree type;
     tree decl;
     
{
  tree binfo;
  tree t;

  /* Nonzero if it's OK to access DECL if it has protected
     accessibility in TYPE.  */
  int protected_ok = 0;

  /* If we're not checking access, everything is accessible.  */
  if (!flag_access_control)
    return 1;

  /* If this declaration is in a block or namespace scope, there's no
     access control.  */
  if (!TYPE_P (context_for_name_lookup (decl)))
    return 1;

  if (!TYPE_P (type))
    {
      binfo = type;
      type = BINFO_TYPE (type);
    }
  else
    binfo = TYPE_BINFO (type);

  /* [class.access.base]

     A member m is accessible when named in class N if

     --m as a member of N is public, or

     --m as a member of N is private, and the reference occurs in a
       member or friend of class N, or

     --m as a member of N is protected, and the reference occurs in a
       member or friend of class N, or in a member or friend of a
       class P derived from N, where m as a member of P is private or
       protected, or

     --there exists a base class B of N that is accessible at the point
       of reference, and m is accessible when named in class B.  

    We walk the base class hierarchy, checking these conditions.  */

  /* Figure out where the reference is occurring.  Check to see if
     DECL is private or protected in this scope, since that will
     determine whether protected access is allowed.  */
  if (current_class_type)
    protected_ok = protected_accessible_p (decl, current_class_type, binfo);

  /* Now, loop through the classes of which we are a friend.  */
  if (!protected_ok)
    protected_ok = friend_accessible_p (current_scope (), decl, binfo);

  /* Standardize the binfo that access_in_type will use.  We don't
     need to know what path was chosen from this point onwards.  */
  binfo = TYPE_BINFO (type);

  /* Compute the accessibility of DECL in the class hierarchy
     dominated by type.  */
  access_in_type (type, decl);
  /* Walk the hierarchy again, looking for a base class that allows
     access.  */
  t = dfs_walk (binfo, dfs_accessible_p, 
		dfs_accessible_queue_p,
		protected_ok ? &protected_ok : 0);
  /* Clear any mark bits.  Note that we have to walk the whole tree
     here, since we have aborted the previous walk from some point
     deep in the tree.  */
  dfs_walk (binfo, dfs_unmark, dfs_canonical_queue,  0);
  assert_canonical_unmarked (binfo);

  return t != NULL_TREE;
}

/* Recursive helper funciton for is_subobject_of_p; see that routine
   for documentation of the parameters.  */

static int
is_subobject_of_p_1 (parent, binfo, most_derived)
     tree parent, binfo, most_derived;
{
  tree binfos;
  int i, n_baselinks;

  if (parent == binfo)
    return 1;

  binfos = BINFO_BASETYPES (binfo);
  n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  /* Iterate through the base types.  */
  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      tree base_type;

      base_type = TREE_TYPE (base_binfo);
      if (!CLASS_TYPE_P (base_type))
	/* If we see a TEMPLATE_TYPE_PARM, or some such, as a base
	   class there's no way to descend into it.  */
	continue;

      /* Avoid walking into the same virtual base more than once.  */
      if (TREE_VIA_VIRTUAL (base_binfo))
	{
	  if (CLASSTYPE_MARKED4 (base_type))
	    continue;
	  SET_CLASSTYPE_MARKED4 (base_type);
	  base_binfo = binfo_for_vbase (base_type, most_derived);
	}

      if (is_subobject_of_p_1 (parent, base_binfo, most_derived))
	return 1;
    }
  return 0;
}

/* Routine to see if the sub-object denoted by the binfo PARENT can be
   found as a base class and sub-object of the object denoted by
   BINFO.  MOST_DERIVED is the most derived type of the hierarchy being
   searched.  */

static int
is_subobject_of_p (tree parent, tree binfo, tree most_derived)
{
  int result;
  tree vbase;

  result = is_subobject_of_p_1 (parent, binfo, most_derived);
  /* Clear the mark bits on virtual bases.  */
  for (vbase = CLASSTYPE_VBASECLASSES (most_derived);
       vbase;
       vbase = TREE_CHAIN (vbase))
    CLEAR_CLASSTYPE_MARKED4 (TREE_TYPE (TREE_VALUE (vbase)));

  return result;
}

struct lookup_field_info {
  /* The type in which we're looking.  */
  tree type;
  /* The name of the field for which we're looking.  */
  tree name;
  /* If non-NULL, the current result of the lookup.  */
  tree rval;
  /* The path to RVAL.  */
  tree rval_binfo;
  /* If non-NULL, the lookup was ambiguous, and this is a list of the
     candidates.  */
  tree ambiguous;
  /* If nonzero, we are looking for types, not data members.  */
  int want_type;
  /* If nonzero, RVAL was found by looking through a dependent base.  */
  int from_dep_base_p;
  /* If something went wrong, a message indicating what.  */
  const char *errstr;
};

/* Returns nonzero if BINFO is not hidden by the value found by the
   lookup so far.  If BINFO is hidden, then there's no need to look in
   it.  DATA is really a struct lookup_field_info.  Called from
   lookup_field via breadth_first_search.  */

static tree
lookup_field_queue_p (binfo, data)
     tree binfo;
     void *data;
{
  struct lookup_field_info *lfi = (struct lookup_field_info *) data;

  /* Don't look for constructors or destructors in base classes.  */
  if (IDENTIFIER_CTOR_OR_DTOR_P (lfi->name))
    return NULL_TREE;

  /* If this base class is hidden by the best-known value so far, we
     don't need to look.  */
  binfo = CANONICAL_BINFO (binfo, lfi->type);
  if (!lfi->from_dep_base_p && lfi->rval_binfo
      && is_subobject_of_p (binfo, lfi->rval_binfo, lfi->type))
    return NULL_TREE;

  return binfo;
}

/* Within the scope of a template class, you can refer to the to the
   current specialization with the name of the template itself.  For
   example:
   
     template <typename T> struct S { S* sp; }

   Returns nonzero if DECL is such a declaration in a class TYPE.  */

static int
template_self_reference_p (type, decl)
     tree type;
     tree decl;
{
  return  (CLASSTYPE_USE_TEMPLATE (type)
	   && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (type))
	   && TREE_CODE (decl) == TYPE_DECL
	   && DECL_ARTIFICIAL (decl)
	   && DECL_NAME (decl) == constructor_name (type));
}


/* Nonzero for a class member means that it is shared between all objects
   of that class.

   [class.member.lookup]:If the resulting set of declarations are not all
   from sub-objects of the same type, or the set has a  nonstatic  member
   and  includes members from distinct sub-objects, there is an ambiguity
   and the program is ill-formed.

   This function checks that T contains no nonstatic members.  */

static int
shared_member_p (t)
     tree t;
{
  if (TREE_CODE (t) == VAR_DECL || TREE_CODE (t) == TYPE_DECL \
      || TREE_CODE (t) == CONST_DECL)
    return 1;
  if (is_overloaded_fn (t))
    {
      for (; t; t = OVL_NEXT (t))
	{
	  tree fn = OVL_CURRENT (t);
	  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn))
	    return 0;
	}
      return 1;
    }
  return 0;
}

/* DATA is really a struct lookup_field_info.  Look for a field with
   the name indicated there in BINFO.  If this function returns a
   non-NULL value it is the result of the lookup.  Called from
   lookup_field via breadth_first_search.  */

static tree
lookup_field_r (binfo, data)
     tree binfo;
     void *data;
{
  struct lookup_field_info *lfi = (struct lookup_field_info *) data;
  tree type = BINFO_TYPE (binfo);
  tree nval = NULL_TREE;
  int from_dep_base_p;

  /* First, look for a function.  There can't be a function and a data
     member with the same name, and if there's a function and a type
     with the same name, the type is hidden by the function.  */
  if (!lfi->want_type)
    {
      int idx = lookup_fnfields_1 (type, lfi->name);
      if (idx >= 0)
	nval = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), idx);
    }

  if (!nval)
    /* Look for a data member or type.  */
    nval = lookup_field_1 (type, lfi->name, lfi->want_type);

  /* If there is no declaration with the indicated name in this type,
     then there's nothing to do.  */
  if (!nval)
    return NULL_TREE;

  /* If we're looking up a type (as with an elaborated type specifier)
     we ignore all non-types we find.  */
  if (lfi->want_type && TREE_CODE (nval) != TYPE_DECL
      && !DECL_CLASS_TEMPLATE_P (nval))
    {
      if (lfi->name == TYPE_IDENTIFIER (type))
	{
	  /* If the aggregate has no user defined constructors, we allow
	     it to have fields with the same name as the enclosing type.
	     If we are looking for that name, find the corresponding
	     TYPE_DECL.  */
	  for (nval = TREE_CHAIN (nval); nval; nval = TREE_CHAIN (nval))
	    if (DECL_NAME (nval) == lfi->name
		&& TREE_CODE (nval) == TYPE_DECL)
	      break;
	}
      else
	nval = NULL_TREE;
      if (!nval && CLASSTYPE_NESTED_UDTS (type) != NULL)
	{
          binding_entry e = binding_table_find (CLASSTYPE_NESTED_UDTS (type),
                                                lfi->name);
	  if (e != NULL)
	    nval = TYPE_MAIN_DECL (e->type);
	  else 
	    return NULL_TREE;
	}
    }

  /* You must name a template base class with a template-id.  */
  if (!same_type_p (type, lfi->type) 
      && template_self_reference_p (type, nval))
    return NULL_TREE;

  from_dep_base_p = dependent_base_p (binfo);
  if (lfi->from_dep_base_p && !from_dep_base_p)
    {
      /* If the new declaration is not found via a dependent base, and
	 the old one was, then we must prefer the new one.  We weren't
	 really supposed to be able to find the old one, so we don't
	 want to be affected by a specialization.  Consider:

	   struct B { typedef int I; };
	   template <typename T> struct D1 : virtual public B {}; 
	   template <typename T> struct D :
	   public D1, virtual pubic B { I i; };

	 The `I' in `D<T>' is unambigousuly `B::I', regardless of how
	 D1 is specialized.  */
      lfi->from_dep_base_p = 0;
      lfi->rval = NULL_TREE;
      lfi->rval_binfo = NULL_TREE;
      lfi->ambiguous = NULL_TREE;
      lfi->errstr = 0;
    }
  else if (lfi->rval_binfo && !lfi->from_dep_base_p && from_dep_base_p)
    /* Similarly, if the old declaration was not found via a dependent
       base, and the new one is, ignore the new one.  */
    return NULL_TREE;

  /* If the lookup already found a match, and the new value doesn't
     hide the old one, we might have an ambiguity.  */
  if (lfi->rval_binfo && !is_subobject_of_p (lfi->rval_binfo, binfo, lfi->type))
    {
      if (nval == lfi->rval && shared_member_p (nval))
	/* The two things are really the same.  */
	;
      else if (is_subobject_of_p (binfo, lfi->rval_binfo, lfi->type))
	/* The previous value hides the new one.  */
	;
      else
	{
	  /* We have a real ambiguity.  We keep a chain of all the
	     candidates.  */
	  if (!lfi->ambiguous && lfi->rval)
	    {
	      /* This is the first time we noticed an ambiguity.  Add
		 what we previously thought was a reasonable candidate
		 to the list.  */
	      lfi->ambiguous = tree_cons (NULL_TREE, lfi->rval, NULL_TREE);
	      TREE_TYPE (lfi->ambiguous) = error_mark_node;
	    }

	  /* Add the new value.  */
	  lfi->ambiguous = tree_cons (NULL_TREE, nval, lfi->ambiguous);
	  TREE_TYPE (lfi->ambiguous) = error_mark_node;
	  lfi->errstr = "request for member `%D' is ambiguous";
	}
    }
  else
    {
      if (from_dep_base_p && TREE_CODE (nval) != TYPE_DECL
	  /* We need to return a member template class so we can
	     define partial specializations.  Is there a better
	     way?  */
	  && !DECL_CLASS_TEMPLATE_P (nval))
	/* The thing we're looking for isn't a type, so the implicit
	   typename extension doesn't apply, so we just pretend we
	   didn't find anything.  */
	return NULL_TREE;

      lfi->rval = nval;
      lfi->from_dep_base_p = from_dep_base_p;
      lfi->rval_binfo = binfo;
    }

  return NULL_TREE;
}

/* Return a "baselink" which BASELINK_BINFO, BASELINK_ACCESS_BINFO,
   BASELINK_FUNCTIONS, and BASELINK_OPTYPE set to BINFO, ACCESS_BINFO,
   FUNCTIONS, and OPTYPE respectively.  */

tree
build_baselink (tree binfo, tree access_binfo, tree functions, tree optype)
{
  tree baselink;

  my_friendly_assert (TREE_CODE (functions) == FUNCTION_DECL
		      || TREE_CODE (functions) == TEMPLATE_DECL
		      || TREE_CODE (functions) == TEMPLATE_ID_EXPR
		      || TREE_CODE (functions) == OVERLOAD,
		      20020730);
  my_friendly_assert (!optype || TYPE_P (optype), 20020730);
  my_friendly_assert (TREE_TYPE (functions), 20020805);

  baselink = build (BASELINK, TREE_TYPE (functions), NULL_TREE,
		    NULL_TREE, NULL_TREE);
  BASELINK_BINFO (baselink) = binfo;
  BASELINK_ACCESS_BINFO (baselink) = access_binfo;
  BASELINK_FUNCTIONS (baselink) = functions;
  BASELINK_OPTYPE (baselink) = optype;

  return baselink;
}

/* Look for a member named NAME in an inheritance lattice dominated by
   XBASETYPE.  If PROTECT is 0 or two, we do not check access.  If it
   is 1, we enforce accessibility.  If PROTECT is zero, then, for an
   ambiguous lookup, we return NULL.  If PROTECT is 1, we issue error
   messages about inaccessible or ambiguous lookup.  If PROTECT is 2,
   we return a TREE_LIST whose TREE_TYPE is error_mark_node and whose
   TREE_VALUEs are the list of ambiguous candidates.

   WANT_TYPE is 1 when we should only return TYPE_DECLs.

   If nothing can be found return NULL_TREE and do not issue an error.  */

tree
lookup_member (xbasetype, name, protect, want_type)
     register tree xbasetype, name;
     int protect, want_type;
{
  tree rval, rval_binfo = NULL_TREE;
  tree type = NULL_TREE, basetype_path = NULL_TREE;
  struct lookup_field_info lfi;

  /* rval_binfo is the binfo associated with the found member, note,
     this can be set with useful information, even when rval is not
     set, because it must deal with ALL members, not just non-function
     members.  It is used for ambiguity checking and the hidden
     checks.  Whereas rval is only set if a proper (not hidden)
     non-function member is found.  */

  const char *errstr = 0;

  if (xbasetype == current_class_type && TYPE_BEING_DEFINED (xbasetype)
      && IDENTIFIER_CLASS_VALUE (name))
    {
      tree field = IDENTIFIER_CLASS_VALUE (name);
      if (! is_overloaded_fn (field)
	  && ! (want_type && TREE_CODE (field) != TYPE_DECL))
	/* We're in the scope of this class, and the value has already
	   been looked up.  Just return the cached value.  */
	return field;
    }

  if (TREE_CODE (xbasetype) == TREE_VEC)
    {
      type = BINFO_TYPE (xbasetype);
      basetype_path = xbasetype;
    }
  else if (IS_AGGR_TYPE_CODE (TREE_CODE (xbasetype)))
    {
      type = xbasetype;
      basetype_path = TYPE_BINFO (type);
      my_friendly_assert (BINFO_INHERITANCE_CHAIN (basetype_path) == NULL_TREE,
			  980827);
    }
  else
    abort ();

  complete_type (type);

#ifdef GATHER_STATISTICS
  n_calls_lookup_field++;
#endif /* GATHER_STATISTICS */

  memset ((PTR) &lfi, 0, sizeof (lfi));
  lfi.type = type;
  lfi.name = name;
  lfi.want_type = want_type;
  bfs_walk (basetype_path, &lookup_field_r, &lookup_field_queue_p, &lfi);
  rval = lfi.rval;
  rval_binfo = lfi.rval_binfo;
  if (rval_binfo)
    type = BINFO_TYPE (rval_binfo);
  errstr = lfi.errstr;

  /* If we are not interested in ambiguities, don't report them;
     just return NULL_TREE.  */
  if (!protect && lfi.ambiguous)
    return NULL_TREE;
  
  if (protect == 2) 
    {
      if (lfi.ambiguous)
	return lfi.ambiguous;
      else
	protect = 0;
    }

  /* [class.access]

     In the case of overloaded function names, access control is
     applied to the function selected by overloaded resolution.  */
  if (rval && protect && !is_overloaded_fn (rval)
      && !enforce_access (xbasetype, rval))
    return error_mark_node;

  if (errstr && protect)
    {
      error (errstr, name, type);
      if (lfi.ambiguous)
        print_candidates (lfi.ambiguous);
      rval = error_mark_node;
    }

  /* If the thing we found was found via the implicit typename
     extension, build the typename type.  */
  if (rval && lfi.from_dep_base_p && !DECL_CLASS_TEMPLATE_P (rval))
    rval = TYPE_STUB_DECL (build_typename_type (BINFO_TYPE (basetype_path),
						name, name,
						TREE_TYPE (rval)));

  if (rval && is_overloaded_fn (rval)) 
    rval = build_baselink (rval_binfo, basetype_path, rval,
			   (IDENTIFIER_TYPENAME_P (name)
			   ? TREE_TYPE (name): NULL_TREE));
  return rval;
}

/* Like lookup_member, except that if we find a function member we
   return NULL_TREE.  */

tree
lookup_field (xbasetype, name, protect, want_type)
     register tree xbasetype, name;
     int protect, want_type;
{
  tree rval = lookup_member (xbasetype, name, protect, want_type);
  
  /* Ignore functions.  */
  if (rval && BASELINK_P (rval))
    return NULL_TREE;

  return rval;
}

/* Like lookup_member, except that if we find a non-function member we
   return NULL_TREE.  */

tree
lookup_fnfields (xbasetype, name, protect)
     register tree xbasetype, name;
     int protect;
{
  tree rval = lookup_member (xbasetype, name, protect, /*want_type=*/0);

  /* Ignore non-functions.  */
  if (rval && !BASELINK_P (rval))
    return NULL_TREE;

  return rval;
}

/* Try to find NAME inside a nested class.  */

tree
lookup_nested_field (name, complain)
     tree name;
     int complain;
{
  register tree t;

  tree id = NULL_TREE;
  if (TYPE_MAIN_DECL (current_class_type))
    {
      /* Climb our way up the nested ladder, seeing if we're trying to
	 modify a field in an enclosing class.  If so, we should only
	 be able to modify if it's static.  */
      for (t = TYPE_MAIN_DECL (current_class_type);
	   t && DECL_CONTEXT (t);
	   t = TYPE_MAIN_DECL (DECL_CONTEXT (t)))
	{
	  if (TREE_CODE (DECL_CONTEXT (t)) != RECORD_TYPE)
	    break;

	  /* N.B.: lookup_field will do the access checking for us */
	  id = lookup_field (DECL_CONTEXT (t), name, complain, 0);
	  if (id == error_mark_node)
	    {
	      id = NULL_TREE;
	      continue;
	    }

	  if (id != NULL_TREE)
	    {
	      if (TREE_CODE (id) == FIELD_DECL
		  && ! TREE_STATIC (id)
		  && TREE_TYPE (id) != error_mark_node)
		{
		  if (complain)
		    {
		      /* At parse time, we don't want to give this error, since
			 we won't have enough state to make this kind of
			 decision properly.  But there are times (e.g., with
			 enums in nested classes) when we do need to call
			 this fn at parse time.  So, in those cases, we pass
			 complain as a 0 and just return a NULL_TREE.  */
		      error ("assignment to non-static member `%D' of enclosing class `%T'",
				id, DECL_CONTEXT (t));
		      /* Mark this for do_identifier().  It would otherwise
			 claim that the variable was undeclared.  */
		      TREE_TYPE (id) = error_mark_node;
		    }
		  else
		    {
		      id = NULL_TREE;
		      continue;
		    }
		}
	      break;
	    }
	}
    }

  return id;
}

/* Return the index in the CLASSTYPE_METHOD_VEC for CLASS_TYPE
   corresponding to "operator TYPE ()", or -1 if there is no such
   operator.  Only CLASS_TYPE itself is searched; this routine does
   not scan the base classes of CLASS_TYPE.  */

static int
lookup_conversion_operator (tree class_type, tree type)
{
  int pass;
  int i;

  tree methods = CLASSTYPE_METHOD_VEC (class_type);

  for (pass = 0; pass < 2; ++pass)
    for (i = CLASSTYPE_FIRST_CONVERSION_SLOT; 
	 i < TREE_VEC_LENGTH (methods);
	 ++i)
      {
	tree fn = TREE_VEC_ELT (methods, i);
	/* The size of the vector may have some unused slots at the
	   end.  */
	if (!fn)
	  break;

	/* All the conversion operators come near the beginning of the
	   class.  Therefore, if FN is not a conversion operator, there
	   is no matching conversion operator in CLASS_TYPE.  */
	fn = OVL_CURRENT (fn);
	if (!DECL_CONV_FN_P (fn))
	  break;

	if (pass == 0)
	  {
	    /* On the first pass we only consider exact matches.  If
	       the types match, this slot is the one where the right
	       conversion operators can be found.  */
	    if (TREE_CODE (fn) != TEMPLATE_DECL
		&& same_type_p (DECL_CONV_FN_TYPE (fn), type))
	      return i;
	  }
	else
	  {
	    /* On the second pass we look for template conversion
	       operators.  It may be possible to instantiate the
	       template to get the type desired.  All of the template
	       conversion operators share a slot.  By looking for
	       templates second we ensure that specializations are
	       preferred over templates.  */
	    if (TREE_CODE (fn) == TEMPLATE_DECL)
	      return i;
	  }
      }

  return -1;
}

/* TYPE is a class type. Return the index of the fields within
   the method vector with name NAME, or -1 is no such field exists.  */

int
lookup_fnfields_1 (tree type, tree name)
{
  tree method_vec;
  tree *methods;
  tree tmp;
  int i;
  int len;

  if (!CLASS_TYPE_P (type))
    return -1;

  method_vec = CLASSTYPE_METHOD_VEC (type);

  if (!method_vec)
    return -1;

  methods = &TREE_VEC_ELT (method_vec, 0);
  len = TREE_VEC_LENGTH (method_vec);

#ifdef GATHER_STATISTICS
  n_calls_lookup_fnfields_1++;
#endif /* GATHER_STATISTICS */

  /* Constructors are first...  */
  if (name == ctor_identifier)
    return (methods[CLASSTYPE_CONSTRUCTOR_SLOT] 
	    ? CLASSTYPE_CONSTRUCTOR_SLOT : -1);
  /* and destructors are second.  */
  if (name == dtor_identifier)
    return (methods[CLASSTYPE_DESTRUCTOR_SLOT]
	    ? CLASSTYPE_DESTRUCTOR_SLOT : -1);
  if (IDENTIFIER_TYPENAME_P (name))
    return lookup_conversion_operator (type, TREE_TYPE (name));

  /* Skip the conversion operators.  */
  i = CLASSTYPE_FIRST_CONVERSION_SLOT;
  while (i < len && methods[i] && DECL_CONV_FN_P (OVL_CURRENT (methods[i])))
    i++;

  /* If the type is complete, use binary search.  */
  if (COMPLETE_TYPE_P (type))
    {
      int lo = i;
      int hi = len;

      while (lo < hi)
	{
	  i = (lo + hi) / 2;

#ifdef GATHER_STATISTICS
	  n_outer_fields_searched++;
#endif /* GATHER_STATISTICS */

	  tmp = methods[i];
	  /* This slot may be empty; we allocate more slots than we
	     need.  In that case, the entry we're looking for is
	     closer to the beginning of the list. */
	  if (tmp)
	    tmp = DECL_NAME (OVL_CURRENT (tmp));
	  if (!tmp || tmp > name)
	    hi = i;
	  else if (tmp < name)
	    lo = i + 1;
	  else
	    return i;
	}
    }
  else
    for (; i < len && methods[i]; ++i)
      {
#ifdef GATHER_STATISTICS
	n_outer_fields_searched++;
#endif /* GATHER_STATISTICS */

	tmp = OVL_CURRENT (methods[i]);
	if (DECL_NAME (tmp) == name)
	  return i;
      }

  return -1;
}

/* DECL is the result of a qualified name lookup.  QUALIFYING_CLASS
   was the class used to qualify the name.  CONTEXT_CLASS is the class
   corresponding to the object in which DECL will be used.  Return a
   possibly modified version of DECL that takes into account the
   CONTEXT_CLASS.

   In particular, consider an expression like `B::m' in the context of
   a derived class `D'.  If `B::m' has been resolved to a BASELINK,
   then the most derived class indicated by the BASELINK_BINFO will be
   `B', not `D'.  This function makes that adjustment.  */

tree
adjust_result_of_qualified_name_lookup (tree decl, 
					tree qualifying_class,
					tree context_class)
{
  my_friendly_assert (CLASS_TYPE_P (qualifying_class), 20020808);
  my_friendly_assert (CLASS_TYPE_P (context_class), 20020808);

  if (BASELINK_P (decl) 
      && DERIVED_FROM_P (qualifying_class, context_class))
    {
      tree base;

      /* Look for the QUALIFYING_CLASS as a base of the CONTEXT_CLASS.
	 Because we do not yet know which function will be chosen by
	 overload resolution, we cannot yet check either accessibility
	 or ambiguity -- in either case, the choice of a static member
	 function might make the usage valid.  */
      base = lookup_base (context_class, qualifying_class,
			  ba_ignore | ba_quiet, NULL);
      if (base)
	{
	  BASELINK_ACCESS_BINFO (decl) = base;
	  BASELINK_BINFO (decl) 
	    = lookup_base (base, BINFO_TYPE (BASELINK_BINFO (decl)),
			   ba_ignore | ba_quiet,
			   NULL);
	}
    }

  return decl;
}


/* Walk the class hierarchy dominated by TYPE.  FN is called for each
   type in the hierarchy, in a breadth-first preorder traversal.
   If it ever returns a non-NULL value, that value is immediately
   returned and the walk is terminated.  At each node, FN is passed a
   BINFO indicating the path from the curently visited base-class to
   TYPE.  Before each base-class is walked QFN is called.  If the
   value returned is nonzero, the base-class is walked; otherwise it
   is not.  If QFN is NULL, it is treated as a function which always
   returns 1.  Both FN and QFN are passed the DATA whenever they are
   called.  */

static tree
bfs_walk (binfo, fn, qfn, data)
     tree binfo;
     tree (*fn) PARAMS ((tree, void *));
     tree (*qfn) PARAMS ((tree, void *));
     void *data;
{
  size_t head;
  size_t tail;
  tree rval = NULL_TREE;
  /* An array of the base classes of BINFO.  These will be built up in
     breadth-first order, except where QFN prunes the search.  */
  varray_type bfs_bases;

  /* Start with enough room for ten base classes.  That will be enough
     for most hierarchies.  */
  VARRAY_TREE_INIT (bfs_bases, 10, "search_stack");

  /* Put the first type into the stack.  */
  VARRAY_TREE (bfs_bases, 0) = binfo;
  tail = 1;

  for (head = 0; head < tail; ++head)
    {
      int i;
      int n_baselinks;
      tree binfos;

      /* Pull the next type out of the queue.  */
      binfo = VARRAY_TREE (bfs_bases, head);

      /* If this is the one we're looking for, we're done.  */
      rval = (*fn) (binfo, data);
      if (rval)
	break;

      /* Queue up the base types.  */
      binfos = BINFO_BASETYPES (binfo);
      n_baselinks = binfos ? TREE_VEC_LENGTH (binfos): 0;
      for (i = 0; i < n_baselinks; i++)
	{
	  tree base_binfo = TREE_VEC_ELT (binfos, i);

	  if (qfn)
	    base_binfo = (*qfn) (base_binfo, data);

	  if (base_binfo)
	    {
	      if (tail == VARRAY_SIZE (bfs_bases))
		VARRAY_GROW (bfs_bases, 2 * VARRAY_SIZE (bfs_bases));
	      VARRAY_TREE (bfs_bases, tail) = base_binfo;
	      ++tail;
	    }
	}
    }

  return rval;
}

/* Exactly like bfs_walk, except that a depth-first traversal is
   performed, and PREFN is called in preorder, while POSTFN is called
   in postorder.  */

tree
dfs_walk_real (binfo, prefn, postfn, qfn, data)
     tree binfo;
     tree (*prefn) PARAMS ((tree, void *));
     tree (*postfn) PARAMS ((tree, void *));
     tree (*qfn) PARAMS ((tree, void *));
     void *data;
{
  int i;
  int n_baselinks;
  tree binfos;
  tree rval = NULL_TREE;

  /* Call the pre-order walking function.  */
  if (prefn)
    {
      rval = (*prefn) (binfo, data);
      if (rval)
	return rval;
    }

  /* Process the basetypes.  */
  binfos = BINFO_BASETYPES (binfo);
  n_baselinks = BINFO_N_BASETYPES (binfo);
  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      
      if (qfn)
	base_binfo = (*qfn) (base_binfo, data);

      if (base_binfo)
	{
	  rval = dfs_walk_real (base_binfo, prefn, postfn, qfn, data);
	  if (rval)
	    return rval;
	}
    }

  /* Call the post-order walking function.  */
  if (postfn)
    rval = (*postfn) (binfo, data);
  
  return rval;
}

/* Exactly like bfs_walk, except that a depth-first post-order traversal is
   performed.  */

tree
dfs_walk (binfo, fn, qfn, data)
     tree binfo;
     tree (*fn) PARAMS ((tree, void *));
     tree (*qfn) PARAMS ((tree, void *));
     void *data;
{
  return dfs_walk_real (binfo, 0, fn, qfn, data);
}

/* Returns > 0 if a function with type DRETTYPE overriding a function
   with type BRETTYPE is covariant, as defined in [class.virtual].

   Returns 1 if trivial covariance, 2 if non-trivial (requiring runtime
   adjustment), or -1 if pedantically invalid covariance.  */

static int
covariant_return_p (brettype, drettype)
     tree brettype, drettype;
{
  tree binfo;
  base_kind kind;

  if (TREE_CODE (brettype) == FUNCTION_DECL)
    {
      brettype = TREE_TYPE (TREE_TYPE (brettype));
      drettype = TREE_TYPE (TREE_TYPE (drettype));
    }
  else if (TREE_CODE (brettype) == METHOD_TYPE)
    {
      brettype = TREE_TYPE (brettype);
      drettype = TREE_TYPE (drettype);
    }

  if (same_type_p (brettype, drettype))
    return 0;

  if (! (TREE_CODE (brettype) == TREE_CODE (drettype)
	 && (TREE_CODE (brettype) == POINTER_TYPE
	     || TREE_CODE (brettype) == REFERENCE_TYPE)
	 && TYPE_QUALS (brettype) == TYPE_QUALS (drettype)))
    return 0;

  if (! can_convert (brettype, drettype))
    return 0;

  brettype = TREE_TYPE (brettype);
  drettype = TREE_TYPE (drettype);

  /* If not pedantic, allow any standard pointer conversion.  */
  if (! IS_AGGR_TYPE (drettype) || ! IS_AGGR_TYPE (brettype))
    return -1;

  binfo = lookup_base (drettype, brettype, ba_check | ba_quiet, &kind);
  
  if (!binfo)
    return 0;
  if (BINFO_OFFSET_ZEROP (binfo) && kind != bk_via_virtual)
    return 1;
  return 2;
}

/* Check that virtual overrider OVERRIDER is acceptable for base function
   BASEFN. Issue diagnostic, and return zero, if unacceptable.  */

int
check_final_overrider (overrider, basefn)
     tree overrider, basefn;
{
  tree over_type = TREE_TYPE (overrider);
  tree base_type = TREE_TYPE (basefn);
  tree over_return = TREE_TYPE (over_type);
  tree base_return = TREE_TYPE (base_type);
  tree over_throw = TYPE_RAISES_EXCEPTIONS (over_type);
  tree base_throw = TYPE_RAISES_EXCEPTIONS (base_type);
  int i;
  
  if (same_type_p (base_return, over_return))
    /* OK */;
  else if ((i = covariant_return_p (base_return, over_return)))
    {
      if (i == 2)
	sorry ("adjusting pointers for covariant returns");

      if (pedantic && i == -1)
	{
	  cp_pedwarn_at ("invalid covariant return type for `%#D'", overrider);
	  cp_pedwarn_at ("  overriding `%#D' (must be pointer or reference to class)", basefn);
	}
    }
  else if (IS_AGGR_TYPE_2 (base_return, over_return)
	   && same_or_base_type_p (base_return, over_return))
    {
      cp_error_at ("invalid covariant return type for `%#D'", overrider);
      cp_error_at ("  overriding `%#D' (must use pointer or reference)", basefn);
      return 0;
    }
  else if (IDENTIFIER_ERROR_LOCUS (DECL_ASSEMBLER_NAME (overrider)) == NULL_TREE)
    {
      cp_error_at ("conflicting return type specified for `%#D'", overrider);
      cp_error_at ("  overriding `%#D'", basefn);
      SET_IDENTIFIER_ERROR_LOCUS (DECL_ASSEMBLER_NAME (overrider),
                                  DECL_CONTEXT (overrider));
      return 0;
    }
  
  /* Check throw specifier is at least as strict.  */
  if (!comp_except_specs (base_throw, over_throw, 0))
    {
      cp_error_at ("looser throw specifier for `%#F'", overrider);
      cp_error_at ("  overriding `%#F'", basefn);
      return 0;
    }
  return 1;
}

/* Given a class TYPE, and a function decl FNDECL, look for
   virtual functions in TYPE's hierarchy which FNDECL overrides.
   We do not look in TYPE itself, only its bases.
   
   Returns nonzero, if we find any. Set FNDECL's DECL_VIRTUAL_P, if we
   find that it overrides anything.
   
   We check that every function which is overridden, is correctly
   overridden.  */

int
look_for_overrides (type, fndecl)
     tree type, fndecl;
{
  tree binfo = TYPE_BINFO (type);
  tree basebinfos = BINFO_BASETYPES (binfo);
  int nbasebinfos = basebinfos ? TREE_VEC_LENGTH (basebinfos) : 0;
  int ix;
  int found = 0;

  for (ix = 0; ix != nbasebinfos; ix++)
    {
      tree basetype = BINFO_TYPE (TREE_VEC_ELT (basebinfos, ix));
      
      if (TYPE_POLYMORPHIC_P (basetype))
        found += look_for_overrides_r (basetype, fndecl);
    }
  return found;
}

/* Look in TYPE for virtual functions with the same signature as
   FNDECL.  */

tree
look_for_overrides_here (type, fndecl)
     tree type, fndecl;
{
  int ix;

  if (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (fndecl))
    ix = CLASSTYPE_DESTRUCTOR_SLOT;
  else
    ix = lookup_fnfields_1 (type, DECL_NAME (fndecl));
  if (ix >= 0)
    {
      tree fns = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), ix);
  
      for (; fns; fns = OVL_NEXT (fns))
        {
          tree fn = OVL_CURRENT (fns);

          if (!DECL_VIRTUAL_P (fn))
            /* Not a virtual.  */;
          else if (DECL_CONTEXT (fn) != type)
            /* Introduced with a using declaration.  */;
	  else if (DECL_STATIC_FUNCTION_P (fndecl))
	    {
	      tree btypes = TYPE_ARG_TYPES (TREE_TYPE (fn));
	      tree dtypes = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  	      if (compparms (TREE_CHAIN (btypes), dtypes))
		return fn;
            }
          else if (same_signature_p (fndecl, fn))
	    return fn;
	}
    }
  return NULL_TREE;
}

/* Look in TYPE for virtual functions overridden by FNDECL. Check both
   TYPE itself and its bases.  */

static int
look_for_overrides_r (type, fndecl)
     tree type, fndecl;
{
  tree fn = look_for_overrides_here (type, fndecl);
  if (fn)
    {
      if (DECL_STATIC_FUNCTION_P (fndecl))
	{
	  /* A static member function cannot match an inherited
	     virtual member function.  */
	  cp_error_at ("`%#D' cannot be declared", fndecl);
	  cp_error_at ("  since `%#D' declared in base class", fn);
	}
      else
	{
	  /* It's definitely virtual, even if not explicitly set.  */
	  DECL_VIRTUAL_P (fndecl) = 1;
	  check_final_overrider (fndecl, fn);
	}
      return 1;
    }

  /* We failed to find one declared in this class. Look in its bases.  */
  return look_for_overrides (type, fndecl);
}

/* A queue function to use with dfs_walk that only walks into
   canonical bases.  DATA should be the type of the complete object,
   or a TREE_LIST whose TREE_PURPOSE is the type of the complete
   object.  By using this function as a queue function, you will walk
   over exactly those BINFOs that actually exist in the complete
   object, including those for virtual base classes.  If you
   SET_BINFO_MARKED for each binfo you process, you are further
   guaranteed that you will walk into each virtual base class exactly
   once.  */

tree
dfs_unmarked_real_bases_queue_p (binfo, data)
     tree binfo;
     void *data;
{
  if (TREE_VIA_VIRTUAL (binfo))
    {
      tree type = (tree) data;

      if (TREE_CODE (type) == TREE_LIST)
	type = TREE_PURPOSE (type);
      binfo = binfo_for_vbase (BINFO_TYPE (binfo), type);
    }
  return unmarkedp (binfo, NULL);
}

/* Like dfs_unmarked_real_bases_queue_p but walks only into things
   that are marked, rather than unmarked.  */

tree
dfs_marked_real_bases_queue_p (binfo, data)
     tree binfo;
     void *data;
{
  if (TREE_VIA_VIRTUAL (binfo))
    {
      tree type = (tree) data;

      if (TREE_CODE (type) == TREE_LIST)
	type = TREE_PURPOSE (type);
      binfo = binfo_for_vbase (BINFO_TYPE (binfo), type);
    }
  return markedp (binfo, NULL);
}

/* A queue function that skips all virtual bases (and their 
   bases).  */

tree
dfs_skip_vbases (binfo, data)
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{
  if (TREE_VIA_VIRTUAL (binfo))
    return NULL_TREE;

  return binfo;
}

/* Called via dfs_walk from dfs_get_pure_virtuals.  */

static tree
dfs_get_pure_virtuals (binfo, data)
     tree binfo;
     void *data;
{
  tree type = (tree) data;

  /* We're not interested in primary base classes; the derived class
     of which they are a primary base will contain the information we
     need.  */
  if (!BINFO_PRIMARY_P (binfo))
    {
      tree virtuals;
      
      for (virtuals = BINFO_VIRTUALS (binfo);
	   virtuals;
	   virtuals = TREE_CHAIN (virtuals))
	if (DECL_PURE_VIRTUAL_P (BV_FN (virtuals)))
	  CLASSTYPE_PURE_VIRTUALS (type) 
	    = tree_cons (NULL_TREE, BV_FN (virtuals),
			 CLASSTYPE_PURE_VIRTUALS (type));
    }
  
  SET_BINFO_MARKED (binfo);

  return NULL_TREE;
}

/* Set CLASSTYPE_PURE_VIRTUALS for TYPE.  */

void
get_pure_virtuals (type)
     tree type;
{
  tree vbases;

  /* Clear the CLASSTYPE_PURE_VIRTUALS list; whatever is already there
     is going to be overridden.  */
  CLASSTYPE_PURE_VIRTUALS (type) = NULL_TREE;
  /* Now, run through all the bases which are not primary bases, and
     collect the pure virtual functions.  We look at the vtable in
     each class to determine what pure virtual functions are present.
     (A primary base is not interesting because the derived class of
     which it is a primary base will contain vtable entries for the
     pure virtuals in the base class.  */
  dfs_walk (TYPE_BINFO (type), dfs_get_pure_virtuals, 
	    dfs_unmarked_real_bases_queue_p, type);
  dfs_walk (TYPE_BINFO (type), dfs_unmark, 
	    dfs_marked_real_bases_queue_p, type);

  /* Put the pure virtuals in dfs order.  */
  CLASSTYPE_PURE_VIRTUALS (type) = nreverse (CLASSTYPE_PURE_VIRTUALS (type));

  for (vbases = CLASSTYPE_VBASECLASSES (type); 
       vbases; 
       vbases = TREE_CHAIN (vbases))
    {
      tree virtuals;

      for (virtuals = BINFO_VIRTUALS (TREE_VALUE (vbases));
	   virtuals;
	   virtuals = TREE_CHAIN (virtuals))
	{
	  tree base_fndecl = BV_FN (virtuals);
	  if (DECL_NEEDS_FINAL_OVERRIDER_P (base_fndecl))
	    error ("`%#D' needs a final overrider", base_fndecl);
	}
    }
}

/* DEPTH-FIRST SEARCH ROUTINES.  */

tree 
markedp (binfo, data) 
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{ 
  return BINFO_MARKED (binfo) ? binfo : NULL_TREE; 
}

tree
unmarkedp (binfo, data) 
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{
  return !BINFO_MARKED (binfo) ? binfo : NULL_TREE;
}

tree
marked_vtable_pathp (binfo, data) 
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{ 
  return BINFO_VTABLE_PATH_MARKED (binfo) ? binfo : NULL_TREE; 
}

tree
unmarked_vtable_pathp (binfo, data) 
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{ 
  return !BINFO_VTABLE_PATH_MARKED (binfo) ? binfo : NULL_TREE; 
}

static tree
marked_pushdecls_p (binfo, data) 
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{
  return (CLASS_TYPE_P (BINFO_TYPE (binfo))
	  && BINFO_PUSHDECLS_MARKED (binfo)) ? binfo : NULL_TREE; 
}

static tree
unmarked_pushdecls_p (binfo, data) 
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{ 
  return (CLASS_TYPE_P (BINFO_TYPE (binfo))
	  && !BINFO_PUSHDECLS_MARKED (binfo)) ? binfo : NULL_TREE;
}

/* The worker functions for `dfs_walk'.  These do not need to
   test anything (vis a vis marking) if they are paired with
   a predicate function (above).  */

tree
dfs_unmark (binfo, data) 
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{ 
  CLEAR_BINFO_MARKED (binfo); 
  return NULL_TREE;
}

/* get virtual base class types.
   This adds type to the vbase_types list in reverse dfs order.
   Ordering is very important, so don't change it.  */

static tree
dfs_get_vbase_types (binfo, data)
     tree binfo;
     void *data;
{
  tree type = (tree) data;

  if (TREE_VIA_VIRTUAL (binfo))
    CLASSTYPE_VBASECLASSES (type)
      = tree_cons (BINFO_TYPE (binfo), 
		   binfo, 
		   CLASSTYPE_VBASECLASSES (type));
  SET_BINFO_MARKED (binfo);
  return NULL_TREE;
}

/* Called via dfs_walk from mark_primary_bases.  Builds the
   inheritance graph order list of BINFOs.  */

static tree
dfs_build_inheritance_graph_order (binfo, data)
     tree binfo;
     void *data;
{
  tree *last_binfo = (tree *) data;

  if (*last_binfo)
    TREE_CHAIN (*last_binfo) = binfo;
  *last_binfo = binfo;
  SET_BINFO_MARKED (binfo);
  return NULL_TREE;
}

/* Set CLASSTYPE_VBASECLASSES for TYPE.  */

void
get_vbase_types (type)
     tree type;
{
  tree last_binfo;

  CLASSTYPE_VBASECLASSES (type) = NULL_TREE;
  dfs_walk (TYPE_BINFO (type), dfs_get_vbase_types, unmarkedp, type);
  /* Rely upon the reverse dfs ordering from dfs_get_vbase_types, and now
     reverse it so that we get normal dfs ordering.  */
  CLASSTYPE_VBASECLASSES (type) = nreverse (CLASSTYPE_VBASECLASSES (type));
  dfs_walk (TYPE_BINFO (type), dfs_unmark, markedp, 0);
  /* Thread the BINFOs in inheritance-graph order.  */
  last_binfo = NULL;
  dfs_walk_real (TYPE_BINFO (type),
		 dfs_build_inheritance_graph_order,
		 NULL,
		 unmarkedp,
		 &last_binfo);
  dfs_walk (TYPE_BINFO (type), dfs_unmark, markedp, NULL);
}

/* Called from find_vbase_instance via dfs_walk.  */

static tree
dfs_find_vbase_instance (binfo, data)
     tree binfo;
     void *data;
{
  tree base = TREE_VALUE ((tree) data);

  if (BINFO_PRIMARY_P (binfo)
      && same_type_p (BINFO_TYPE (binfo), base))
    return binfo;

  return NULL_TREE;
}

/* Find the real occurrence of the virtual BASE (a class type) in the
   hierarchy dominated by TYPE.  */

tree
find_vbase_instance (base, type)
     tree base;
     tree type;
{
  tree instance;

  instance = binfo_for_vbase (base, type);
  if (!BINFO_PRIMARY_P (instance))
    return instance;

  return dfs_walk (TYPE_BINFO (type), 
		   dfs_find_vbase_instance, 
		   NULL,
		   build_tree_list (type, base));
}


/* Debug info for C++ classes can get very large; try to avoid
   emitting it everywhere.

   Note that this optimization wins even when the target supports
   BINCL (if only slightly), and reduces the amount of work for the
   linker.  */

void
maybe_suppress_debug_info (t)
     tree t;
{
  /* We can't do the usual TYPE_DECL_SUPPRESS_DEBUG thing with DWARF, which
     does not support name references between translation units.  It supports
     symbolic references between translation units, but only within a single
     executable or shared library.

     For DWARF 2, we handle TYPE_DECL_SUPPRESS_DEBUG by pretending
     that the type was never defined, so we only get the members we
     actually define.  */
  if (write_symbols == DWARF_DEBUG || write_symbols == NO_DEBUG)
    return;

  /* We might have set this earlier in cp_finish_decl.  */
  TYPE_DECL_SUPPRESS_DEBUG (TYPE_MAIN_DECL (t)) = 0;

  /* If we already know how we're handling this class, handle debug info
     the same way.  */
  if (CLASSTYPE_INTERFACE_KNOWN (t))
    {
      if (CLASSTYPE_INTERFACE_ONLY (t))
	TYPE_DECL_SUPPRESS_DEBUG (TYPE_MAIN_DECL (t)) = 1;
      /* else don't set it.  */
    }
  /* If the class has a vtable, write out the debug info along with
     the vtable.  */
  else if (TYPE_CONTAINS_VPTR_P (t))
    TYPE_DECL_SUPPRESS_DEBUG (TYPE_MAIN_DECL (t)) = 1;

  /* Otherwise, just emit the debug info normally.  */
}

/* Note that we want debugging information for a base class of a class
   whose vtable is being emitted.  Normally, this would happen because
   calling the constructor for a derived class implies calling the
   constructors for all bases, which involve initializing the
   appropriate vptr with the vtable for the base class; but in the
   presence of optimization, this initialization may be optimized
   away, so we tell finish_vtable_vardecl that we want the debugging
   information anyway.  */

static tree
dfs_debug_mark (binfo, data)
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{
  tree t = BINFO_TYPE (binfo);

  CLASSTYPE_DEBUG_REQUESTED (t) = 1;

  return NULL_TREE;
}

/* Returns BINFO if we haven't already noted that we want debugging
   info for this base class.  */

static tree 
dfs_debug_unmarkedp (binfo, data) 
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{ 
  return (!CLASSTYPE_DEBUG_REQUESTED (BINFO_TYPE (binfo)) 
	  ? binfo : NULL_TREE);
}

/* Write out the debugging information for TYPE, whose vtable is being
   emitted.  Also walk through our bases and note that we want to
   write out information for them.  This avoids the problem of not
   writing any debug info for intermediate basetypes whose
   constructors, and thus the references to their vtables, and thus
   the vtables themselves, were optimized away.  */

void
note_debug_info_needed (type)
     tree type;
{
  if (TYPE_DECL_SUPPRESS_DEBUG (TYPE_NAME (type)))
    {
      TYPE_DECL_SUPPRESS_DEBUG (TYPE_NAME (type)) = 0;
      rest_of_type_compilation (type, toplevel_bindings_p ());
    }

  dfs_walk (TYPE_BINFO (type), dfs_debug_mark, dfs_debug_unmarkedp, 0);
}

/* Subroutines of push_class_decls ().  */

/* Returns 1 iff BINFO is a base we shouldn't really be able to see into,
   because it (or one of the intermediate bases) depends on template parms.  */

static int
dependent_base_p (binfo)
     tree binfo;
{
  for (; binfo; binfo = BINFO_INHERITANCE_CHAIN (binfo))
    {
      if (currently_open_class (TREE_TYPE (binfo)))
	break;
      if (uses_template_parms (TREE_TYPE (binfo)))
	return 1;
    }
  return 0;
}

static void
setup_class_bindings (name, type_binding_p)
     tree name;
     int type_binding_p;
{
  tree type_binding = NULL_TREE;
  tree value_binding;

  /* If we've already done the lookup for this declaration, we're
     done.  */
  if (IDENTIFIER_CLASS_VALUE (name))
    return;

  /* First, deal with the type binding.  */
  if (type_binding_p)
    {
      type_binding = lookup_member (current_class_type, name,
				    /*protect=*/2,
				    /*want_type=*/1);
      if (TREE_CODE (type_binding) == TREE_LIST 
	  && TREE_TYPE (type_binding) == error_mark_node)
	/* NAME is ambiguous.  */
	push_class_level_binding (name, type_binding);
      else
	pushdecl_class_level (type_binding);
    }

  /* Now, do the value binding.  */
  value_binding = lookup_member (current_class_type, name,
				 /*protect=*/2,
				 /*want_type=*/0);

  if (type_binding_p
      && (TREE_CODE (value_binding) == TYPE_DECL
	  || DECL_CLASS_TEMPLATE_P (value_binding)
	  || (TREE_CODE (value_binding) == TREE_LIST
	      && TREE_TYPE (value_binding) == error_mark_node
	      && (TREE_CODE (TREE_VALUE (value_binding))
		  == TYPE_DECL))))
    /* We found a type-binding, even when looking for a non-type
       binding.  This means that we already processed this binding
       above.  */;
  else if (value_binding)
    {
      if (TREE_CODE (value_binding) == TREE_LIST 
	  && TREE_TYPE (value_binding) == error_mark_node)
	/* NAME is ambiguous.  */
	push_class_level_binding (name, value_binding);
      else
	{
	  if (BASELINK_P (value_binding))
	    /* NAME is some overloaded functions.  */
	    value_binding = BASELINK_FUNCTIONS (value_binding);
	  pushdecl_class_level (value_binding);
	}
    }
}

/* Push class-level declarations for any names appearing in BINFO that
   are TYPE_DECLS.  */

static tree
dfs_push_type_decls (binfo, data)
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{
  tree type;
  tree fields;

  type = BINFO_TYPE (binfo);
  for (fields = TYPE_FIELDS (type); fields; fields = TREE_CHAIN (fields))
    if (DECL_NAME (fields) && TREE_CODE (fields) == TYPE_DECL
	&& !(!same_type_p (type, current_class_type)
	     && template_self_reference_p (type, fields)))
      setup_class_bindings (DECL_NAME (fields), /*type_binding_p=*/1);

  /* We can't just use BINFO_MARKED because envelope_add_decl uses
     DERIVED_FROM_P, which calls get_base_distance.  */
  SET_BINFO_PUSHDECLS_MARKED (binfo);

  return NULL_TREE;
}

/* Push class-level declarations for any names appearing in BINFO that
   are not TYPE_DECLS.  */

static tree
dfs_push_decls (binfo, data)
     tree binfo;
     void *data;
{
  tree type;
  tree method_vec;
  int dep_base_p;

  type = BINFO_TYPE (binfo);
  dep_base_p = (processing_template_decl && type != current_class_type
		&& dependent_base_p (binfo));
  if (!dep_base_p)
    {
      tree fields;
      for (fields = TYPE_FIELDS (type); fields; fields = TREE_CHAIN (fields))
	if (DECL_NAME (fields) 
	    && TREE_CODE (fields) != TYPE_DECL
	    && TREE_CODE (fields) != USING_DECL
	    && !DECL_ARTIFICIAL (fields))
	  setup_class_bindings (DECL_NAME (fields), /*type_binding_p=*/0);
	else if (TREE_CODE (fields) == FIELD_DECL
		 && ANON_AGGR_TYPE_P (TREE_TYPE (fields)))
	  dfs_push_decls (TYPE_BINFO (TREE_TYPE (fields)), data);
	  
      method_vec = (CLASS_TYPE_P (type) 
		    ? CLASSTYPE_METHOD_VEC (type) : NULL_TREE);

      if (method_vec && TREE_VEC_LENGTH (method_vec) >= 3)
	{
	  tree *methods;
	  tree *end;

	  /* Farm out constructors and destructors.  */
	  end = TREE_VEC_END (method_vec);

	  for (methods = &TREE_VEC_ELT (method_vec, 2);
	       methods < end && *methods;
	       methods++)
	    setup_class_bindings (DECL_NAME (OVL_CURRENT (*methods)), 
				  /*type_binding_p=*/0);
	}
    }

  CLEAR_BINFO_PUSHDECLS_MARKED (binfo);

  return NULL_TREE;
}

/* When entering the scope of a class, we cache all of the
   fields that that class provides within its inheritance
   lattice.  Where ambiguities result, we mark them
   with `error_mark_node' so that if they are encountered
   without explicit qualification, we can emit an error
   message.  */

void
push_class_decls (type)
     tree type;
{
  search_stack = push_search_level (search_stack, &search_obstack);

  /* Enter type declarations and mark.  */
  dfs_walk (TYPE_BINFO (type), dfs_push_type_decls, unmarked_pushdecls_p, 0);

  /* Enter non-type declarations and unmark.  */
  dfs_walk (TYPE_BINFO (type), dfs_push_decls, marked_pushdecls_p, 0);
}

/* Here's a subroutine we need because C lacks lambdas.  */

static tree
dfs_unuse_fields (binfo, data)
     tree binfo;
     void *data ATTRIBUTE_UNUSED;
{
  tree type = TREE_TYPE (binfo);
  tree fields;

  for (fields = TYPE_FIELDS (type); fields; fields = TREE_CHAIN (fields))
    {
      if (TREE_CODE (fields) != FIELD_DECL || DECL_ARTIFICIAL (fields))
	continue;

      TREE_USED (fields) = 0;
      if (DECL_NAME (fields) == NULL_TREE
	  && ANON_AGGR_TYPE_P (TREE_TYPE (fields)))
	unuse_fields (TREE_TYPE (fields));
    }

  return NULL_TREE;
}

void
unuse_fields (type)
     tree type;
{
  dfs_walk (TYPE_BINFO (type), dfs_unuse_fields, unmarkedp, 0);
}

void
pop_class_decls ()
{
  /* We haven't pushed a search level when dealing with cached classes,
     so we'd better not try to pop it.  */
  if (search_stack)
    search_stack = pop_search_level (search_stack);
}

void
print_search_statistics ()
{
#ifdef GATHER_STATISTICS
  fprintf (stderr, "%d fields searched in %d[%d] calls to lookup_field[_1]\n",
	   n_fields_searched, n_calls_lookup_field, n_calls_lookup_field_1);
  fprintf (stderr, "%d fnfields searched in %d calls to lookup_fnfields\n",
	   n_outer_fields_searched, n_calls_lookup_fnfields);
  fprintf (stderr, "%d calls to get_base_type\n", n_calls_get_base_type);
#else /* GATHER_STATISTICS */
  fprintf (stderr, "no search statistics\n");
#endif /* GATHER_STATISTICS */
}

void
init_search_processing ()
{
  gcc_obstack_init (&search_obstack);
}

void
reinit_search_statistics ()
{
#ifdef GATHER_STATISTICS
  n_fields_searched = 0;
  n_calls_lookup_field = 0, n_calls_lookup_field_1 = 0;
  n_calls_lookup_fnfields = 0, n_calls_lookup_fnfields_1 = 0;
  n_calls_get_base_type = 0;
  n_outer_fields_searched = 0;
  n_contexts_saved = 0;
#endif /* GATHER_STATISTICS */
}

static tree
add_conversions (binfo, data)
     tree binfo;
     void *data;
{
  int i;
  tree method_vec = CLASSTYPE_METHOD_VEC (BINFO_TYPE (binfo));
  tree *conversions = (tree *) data;

  /* Some builtin types have no method vector, not even an empty one.  */
  if (!method_vec)
    return NULL_TREE;

  for (i = 2; i < TREE_VEC_LENGTH (method_vec); ++i)
    {
      tree tmp = TREE_VEC_ELT (method_vec, i);
      tree name;

      if (!tmp || ! DECL_CONV_FN_P (OVL_CURRENT (tmp)))
	break;

      name = DECL_NAME (OVL_CURRENT (tmp));

      /* Make sure we don't already have this conversion.  */
      if (! IDENTIFIER_MARKED (name))
	{
	  *conversions = tree_cons (binfo, tmp, *conversions);
	  IDENTIFIER_MARKED (name) = 1;
	}
    }
  return NULL_TREE;
}

/* Return a TREE_LIST containing all the non-hidden user-defined
   conversion functions for TYPE (and its base-classes).  The
   TREE_VALUE of each node is a FUNCTION_DECL or an OVERLOAD
   containing the conversion functions.  The TREE_PURPOSE is the BINFO
   from which the conversion functions in this node were selected.  */

tree
lookup_conversions (type)
     tree type;
{
  tree t;
  tree conversions = NULL_TREE;

  complete_type (type);
  bfs_walk (TYPE_BINFO (type), add_conversions, 0, &conversions);

  for (t = conversions; t; t = TREE_CHAIN (t))
    IDENTIFIER_MARKED (DECL_NAME (OVL_CURRENT (TREE_VALUE (t)))) = 0;

  return conversions;
}

struct overlap_info 
{
  tree compare_type;
  int found_overlap;
};

/* Check whether the empty class indicated by EMPTY_BINFO is also present
   at offset 0 in COMPARE_TYPE, and set found_overlap if so.  */

static tree
dfs_check_overlap (empty_binfo, data)
     tree empty_binfo;
     void *data;
{
  struct overlap_info *oi = (struct overlap_info *) data;
  tree binfo;
  for (binfo = TYPE_BINFO (oi->compare_type); 
       ; 
       binfo = BINFO_BASETYPE (binfo, 0))
    {
      if (BINFO_TYPE (binfo) == BINFO_TYPE (empty_binfo))
	{
	  oi->found_overlap = 1;
	  break;
	}
      else if (BINFO_BASETYPES (binfo) == NULL_TREE)
	break;
    }

  return NULL_TREE;
}

/* Trivial function to stop base traversal when we find something.  */

static tree
dfs_no_overlap_yet (binfo, data)
     tree binfo;
     void *data;
{
  struct overlap_info *oi = (struct overlap_info *) data;
  return !oi->found_overlap ? binfo : NULL_TREE;
}

/* Returns nonzero if EMPTY_TYPE or any of its bases can also be found at
   offset 0 in NEXT_TYPE.  Used in laying out empty base class subobjects.  */

int
types_overlap_p (empty_type, next_type)
     tree empty_type, next_type;
{
  struct overlap_info oi;

  if (! IS_AGGR_TYPE (next_type))
    return 0;
  oi.compare_type = next_type;
  oi.found_overlap = 0;
  dfs_walk (TYPE_BINFO (empty_type), dfs_check_overlap,
	    dfs_no_overlap_yet, &oi);
  return oi.found_overlap;
}

/* Given a vtable VAR, determine which of the inherited classes the vtable
   inherits (in a loose sense) functions from.

   FIXME: This does not work with the new ABI.  */

tree
binfo_for_vtable (var)
     tree var;
{
  tree main_binfo = TYPE_BINFO (DECL_CONTEXT (var));
  tree binfos = TYPE_BINFO_BASETYPES (BINFO_TYPE (main_binfo));
  int n_baseclasses = CLASSTYPE_N_BASECLASSES (BINFO_TYPE (main_binfo));
  int i;

  for (i = 0; i < n_baseclasses; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      if (base_binfo != NULL_TREE && BINFO_VTABLE (base_binfo) == var)
	return base_binfo;
    }

  /* If no secondary base classes matched, return the primary base, if
     there is one.  */
  if (CLASSTYPE_HAS_PRIMARY_BASE_P (BINFO_TYPE (main_binfo)))
    return get_primary_binfo (main_binfo);

  return main_binfo;
}

/* Returns the binfo of the first direct or indirect virtual base derived
   from BINFO, or NULL if binfo is not via virtual.  */

tree
binfo_from_vbase (binfo)
     tree binfo;
{
  for (; binfo; binfo = BINFO_INHERITANCE_CHAIN (binfo))
    {
      if (TREE_VIA_VIRTUAL (binfo))
	return binfo;
    }
  return NULL_TREE;
}

/* Returns the binfo of the first direct or indirect virtual base derived
   from BINFO up to the TREE_TYPE, LIMIT, or NULL if binfo is not
   via virtual.  */

tree
binfo_via_virtual (binfo, limit)
     tree binfo;
     tree limit;
{
  for (; binfo && (!limit || !same_type_p (BINFO_TYPE (binfo), limit));
       binfo = BINFO_INHERITANCE_CHAIN (binfo))
    {
      if (TREE_VIA_VIRTUAL (binfo))
	return binfo;
    }
  return NULL_TREE;
}

/* Returns the BINFO (if any) for the virtual baseclass T of the class
   C from the CLASSTYPE_VBASECLASSES list.  */

tree
binfo_for_vbase (basetype, classtype)
     tree basetype;
     tree classtype;
{
  tree binfo;

  binfo = purpose_member (basetype, CLASSTYPE_VBASECLASSES (classtype));
  return binfo ? TREE_VALUE (binfo) : NULL_TREE;
}
