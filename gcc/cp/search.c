/* Breadth-first and depth-first routines for
   searching multiple-inheritance lattice for GNU C++.
   Copyright (C) 1987-2019 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* High-level class interface.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "intl.h"
#include "toplev.h"
#include "spellcheck-tree.h"
#include "stringpool.h"
#include "attribs.h"

static int is_subobject_of_p (tree, tree);
static tree dfs_lookup_base (tree, void *);
static tree dfs_dcast_hint_pre (tree, void *);
static tree dfs_dcast_hint_post (tree, void *);
static tree dfs_debug_mark (tree, void *);
static int check_hidden_convs (tree, int, int, tree, tree, tree);
static tree split_conversions (tree, tree, tree, tree);
static int lookup_conversions_r (tree, int, int, tree, tree, tree *);
static int look_for_overrides_r (tree, tree);
static tree lookup_field_r (tree, void *);
static tree dfs_accessible_post (tree, void *);
static tree dfs_walk_once_accessible (tree, bool,
				      tree (*pre_fn) (tree, void *),
				      tree (*post_fn) (tree, void *),
				      void *data);
static tree dfs_access_in_type (tree, void *);
static access_kind access_in_type (tree, tree);
static tree dfs_get_pure_virtuals (tree, void *);


/* Data for lookup_base and its workers.  */

struct lookup_base_data_s
{
  tree t;		/* type being searched.  */
  tree base;		/* The base type we're looking for.  */
  tree binfo;		/* Found binfo.  */
  bool via_virtual;	/* Found via a virtual path.  */
  bool ambiguous;	/* Found multiply ambiguous */
  bool repeated_base;	/* Whether there are repeated bases in the
			    hierarchy.  */
  bool want_any;	/* Whether we want any matching binfo.  */
};

/* Worker function for lookup_base.  See if we've found the desired
   base and update DATA_ (a pointer to LOOKUP_BASE_DATA_S).  */

static tree
dfs_lookup_base (tree binfo, void *data_)
{
  struct lookup_base_data_s *data = (struct lookup_base_data_s *) data_;

  if (SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), data->base))
    {
      if (!data->binfo)
	{
	  data->binfo = binfo;
	  data->via_virtual
	    = binfo_via_virtual (data->binfo, data->t) != NULL_TREE;

	  if (!data->repeated_base)
	    /* If there are no repeated bases, we can stop now.  */
	    return binfo;

	  if (data->want_any && !data->via_virtual)
	    /* If this is a non-virtual base, then we can't do
	       better.  */
	    return binfo;

	  return dfs_skip_bases;
	}
      else
	{
	  gcc_assert (binfo != data->binfo);

	  /* We've found more than one matching binfo.  */
	  if (!data->want_any)
	    {
	      /* This is immediately ambiguous.  */
	      data->binfo = NULL_TREE;
	      data->ambiguous = true;
	      return error_mark_node;
	    }

	  /* Prefer one via a non-virtual path.  */
	  if (!binfo_via_virtual (binfo, data->t))
	    {
	      data->binfo = binfo;
	      data->via_virtual = false;
	      return binfo;
	    }

	  /* There must be repeated bases, otherwise we'd have stopped
	     on the first base we found.  */
	  return dfs_skip_bases;
	}
    }

  return NULL_TREE;
}

/* Returns true if type BASE is accessible in T.  (BASE is known to be
   a (possibly non-proper) base class of T.)  If CONSIDER_LOCAL_P is
   true, consider any special access of the current scope, or access
   bestowed by friendship.  */

bool
accessible_base_p (tree t, tree base, bool consider_local_p)
{
  tree decl;

  /* [class.access.base]

     A base class is said to be accessible if an invented public
     member of the base class is accessible.

     If BASE is a non-proper base, this condition is trivially
     true.  */
  if (same_type_p (t, base))
    return true;
  /* Rather than inventing a public member, we use the implicit
     public typedef created in the scope of every class.  */
  decl = TYPE_FIELDS (base);
  while (!DECL_SELF_REFERENCE_P (decl))
    decl = DECL_CHAIN (decl);
  while (ANON_AGGR_TYPE_P (t))
    t = TYPE_CONTEXT (t);
  return accessible_p (t, decl, consider_local_p);
}

/* Lookup BASE in the hierarchy dominated by T.  Do access checking as
   ACCESS specifies.  Return the binfo we discover.  If KIND_PTR is
   non-NULL, fill with information about what kind of base we
   discovered.

   If the base is inaccessible, or ambiguous, then error_mark_node is
   returned.  If the tf_error bit of COMPLAIN is not set, no error
   is issued.  */

tree
lookup_base (tree t, tree base, base_access access,
	     base_kind *kind_ptr, tsubst_flags_t complain)
{
  tree binfo;
  tree t_binfo;
  base_kind bk;

  /* "Nothing" is definitely not derived from Base.  */
  if (t == NULL_TREE)
    {
      if (kind_ptr)
	*kind_ptr = bk_not_base;
      return NULL_TREE;
    }

  if (t == error_mark_node || base == error_mark_node)
    {
      if (kind_ptr)
	*kind_ptr = bk_not_base;
      return error_mark_node;
    }
  gcc_assert (TYPE_P (base));

  if (!TYPE_P (t))
    {
      t_binfo = t;
      t = BINFO_TYPE (t);
    }
  else
    {
      t = complete_type (TYPE_MAIN_VARIANT (t));
      if (dependent_type_p (t))
	if (tree open = currently_open_class (t))
	  t = open;
      t_binfo = TYPE_BINFO (t);
    }

  base = TYPE_MAIN_VARIANT (base);

  /* If BASE is incomplete, it can't be a base of T--and instantiating it
     might cause an error.  */
  if (t_binfo && CLASS_TYPE_P (base) && COMPLETE_OR_OPEN_TYPE_P (base))
    {
      struct lookup_base_data_s data;

      data.t = t;
      data.base = base;
      data.binfo = NULL_TREE;
      data.ambiguous = data.via_virtual = false;
      data.repeated_base = CLASSTYPE_REPEATED_BASE_P (t);
      data.want_any = access == ba_any;

      dfs_walk_once (t_binfo, dfs_lookup_base, NULL, &data);
      binfo = data.binfo;

      if (!binfo)
	bk = data.ambiguous ? bk_ambig : bk_not_base;
      else if (binfo == t_binfo)
	bk = bk_same_type;
      else if (data.via_virtual)
	bk = bk_via_virtual;
      else
	bk = bk_proper_base;
    }
  else
    {
      binfo = NULL_TREE;
      bk = bk_not_base;
    }

  /* Check that the base is unambiguous and accessible.  */
  if (access != ba_any)
    switch (bk)
      {
      case bk_not_base:
	break;

      case bk_ambig:
	if (complain & tf_error)
	  error ("%qT is an ambiguous base of %qT", base, t);
	binfo = error_mark_node;
	break;

      default:
	if ((access & ba_check_bit)
	    /* If BASE is incomplete, then BASE and TYPE are probably
	       the same, in which case BASE is accessible.  If they
	       are not the same, then TYPE is invalid.  In that case,
	       there's no need to issue another error here, and
	       there's no implicit typedef to use in the code that
	       follows, so we skip the check.  */
	    && COMPLETE_TYPE_P (base)
	    && !accessible_base_p (t, base, !(access & ba_ignore_scope)))
	  {
	    if (complain & tf_error)
	      error ("%qT is an inaccessible base of %qT", base, t);
	    binfo = error_mark_node;
	    bk = bk_inaccessible;
	  }
	break;
      }

  if (kind_ptr)
    *kind_ptr = bk;

  return binfo;
}

/* Data for dcast_base_hint walker.  */

struct dcast_data_s
{
  tree subtype;   /* The base type we're looking for.  */
  int virt_depth; /* Number of virtual bases encountered from most
		     derived.  */
  tree offset;    /* Best hint offset discovered so far.  */
  bool repeated_base;  /* Whether there are repeated bases in the
			  hierarchy.  */
};

/* Worker for dcast_base_hint.  Search for the base type being cast
   from.  */

static tree
dfs_dcast_hint_pre (tree binfo, void *data_)
{
  struct dcast_data_s *data = (struct dcast_data_s *) data_;

  if (BINFO_VIRTUAL_P (binfo))
    data->virt_depth++;

  if (SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), data->subtype))
    {
      if (data->virt_depth)
	{
	  data->offset = ssize_int (-1);
	  return data->offset;
	}
      if (data->offset)
	data->offset = ssize_int (-3);
      else
	data->offset = BINFO_OFFSET (binfo);

      return data->repeated_base ? dfs_skip_bases : data->offset;
    }

  return NULL_TREE;
}

/* Worker for dcast_base_hint.  Track the virtual depth.  */

static tree
dfs_dcast_hint_post (tree binfo, void *data_)
{
  struct dcast_data_s *data = (struct dcast_data_s *) data_;

  if (BINFO_VIRTUAL_P (binfo))
    data->virt_depth--;

  return NULL_TREE;
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
dcast_base_hint (tree subtype, tree target)
{
  struct dcast_data_s data;

  data.subtype = subtype;
  data.virt_depth = 0;
  data.offset = NULL_TREE;
  data.repeated_base = CLASSTYPE_REPEATED_BASE_P (target);

  dfs_walk_once_accessible (TYPE_BINFO (target), /*friends=*/false,
			    dfs_dcast_hint_pre, dfs_dcast_hint_post, &data);
  return data.offset ? data.offset : ssize_int (-2);
}

/* Search for a member with name NAME in a multiple inheritance
   lattice specified by TYPE.  If it does not exist, return NULL_TREE.
   If the member is ambiguously referenced, return `error_mark_node'.
   Otherwise, return a DECL with the indicated name.  If WANT_TYPE is
   true, type declarations are preferred.  */

/* Return the FUNCTION_DECL, RECORD_TYPE, UNION_TYPE, or
   NAMESPACE_DECL corresponding to the innermost non-block scope.  */

tree
current_scope (void)
{
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
  if (current_function_decl && current_class_type
      && ((DECL_FUNCTION_MEMBER_P (current_function_decl)
	   && same_type_p (DECL_CONTEXT (current_function_decl),
			   current_class_type))
	  || (DECL_FRIEND_CONTEXT (current_function_decl)
	      && same_type_p (DECL_FRIEND_CONTEXT (current_function_decl),
			      current_class_type))))
    return current_function_decl;

  if (current_class_type)
    return current_class_type;

  if (current_function_decl)
    return current_function_decl;

  return current_namespace;
}

/* Returns nonzero if we are currently in a function scope.  Note
   that this function returns zero if we are within a local class, but
   not within a member function body of the local class.  */

int
at_function_scope_p (void)
{
  tree cs = current_scope ();
  /* Also check cfun to make sure that we're really compiling
     this function (as opposed to having set current_function_decl
     for access checking or some such).  */
  return (cs && TREE_CODE (cs) == FUNCTION_DECL
	  && cfun && cfun->decl == current_function_decl);
}

/* Returns true if the innermost active scope is a class scope.  */

bool
at_class_scope_p (void)
{
  tree cs = current_scope ();
  return cs && TYPE_P (cs);
}

/* Returns true if the innermost active scope is a namespace scope.  */

bool
at_namespace_scope_p (void)
{
  tree cs = current_scope ();
  return cs && TREE_CODE (cs) == NAMESPACE_DECL;
}

/* Return the scope of DECL, as appropriate when doing name-lookup.  */

tree
context_for_name_lookup (tree decl)
{
  /* [class.union]

     For the purposes of name lookup, after the anonymous union
     definition, the members of the anonymous union are considered to
     have been defined in the scope in which the anonymous union is
     declared.  */
  tree context = DECL_CONTEXT (decl);

  while (context && TYPE_P (context)
	 && (ANON_AGGR_TYPE_P (context) || UNSCOPED_ENUM_P (context)))
    context = TYPE_CONTEXT (context);
  if (!context)
    context = global_namespace;

  return context;
}

/* Returns true iff DECL is declared in TYPE.  */

static bool
member_declared_in_type (tree decl, tree type)
{
  /* A normal declaration obviously counts.  */
  if (context_for_name_lookup (decl) == type)
    return true;
  /* So does a using or access declaration.  */
  if (DECL_LANG_SPECIFIC (decl) && !DECL_DISCRIMINATOR_P (decl)
      && purpose_member (type, DECL_ACCESS (decl)))
    return true;
  return false;
}

/* The accessibility routines use BINFO_ACCESS for scratch space
   during the computation of the accessibility of some declaration.  */

/* Avoid walking up past a declaration of the member.  */

static tree
dfs_access_in_type_pre (tree binfo, void *data)
{
  tree decl = (tree) data;
  tree type = BINFO_TYPE (binfo);
  if (member_declared_in_type (decl, type))
    return dfs_skip_bases;
  return NULL_TREE;
}

#define BINFO_ACCESS(NODE) \
  ((access_kind) ((TREE_PUBLIC (NODE) << 1) | TREE_PRIVATE (NODE)))

/* Set the access associated with NODE to ACCESS.  */

#define SET_BINFO_ACCESS(NODE, ACCESS)			\
  ((TREE_PUBLIC (NODE) = ((ACCESS) & 2) != 0),	\
   (TREE_PRIVATE (NODE) = ((ACCESS) & 1) != 0))

/* Called from access_in_type via dfs_walk.  Calculate the access to
   DATA (which is really a DECL) in BINFO.  */

static tree
dfs_access_in_type (tree binfo, void *data)
{
  tree decl = (tree) data;
  tree type = BINFO_TYPE (binfo);
  access_kind access = ak_none;

  if (context_for_name_lookup (decl) == type)
    {
      /* If we have descended to the scope of DECL, just note the
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
	 access to the DECL.  */
      if (DECL_LANG_SPECIFIC (decl) && !DECL_DISCRIMINATOR_P (decl))
	{
	  tree decl_access = purpose_member (type, DECL_ACCESS (decl));

	  if (decl_access)
	    {
	      decl_access = TREE_VALUE (decl_access);

	      if (decl_access == access_public_node)
		access = ak_public;
	      else if (decl_access == access_protected_node)
		access = ak_protected;
	      else if (decl_access == access_private_node)
		access = ak_private;
	      else
		gcc_unreachable ();
	    }
	}

      if (!access)
	{
	  int i;
	  tree base_binfo;
	  vec<tree, va_gc> *accesses;

	  /* Otherwise, scan our baseclasses, and pick the most favorable
	     access.  */
	  accesses = BINFO_BASE_ACCESSES (binfo);
	  for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	    {
	      tree base_access = (*accesses)[i];
	      access_kind base_access_now = BINFO_ACCESS (base_binfo);

	      if (base_access_now == ak_none || base_access_now == ak_private)
		/* If it was not accessible in the base, or only
		   accessible as a private member, we can't access it
		   all.  */
		base_access_now = ak_none;
	      else if (base_access == access_protected_node)
		/* Public and protected members in the base become
		   protected here.  */
		base_access_now = ak_protected;
	      else if (base_access == access_private_node)
		/* Public and protected members in the base become
		   private here.  */
		base_access_now = ak_private;

	      /* See if the new access, via this base, gives more
		 access than our previous best access.  */
	      if (base_access_now != ak_none
		  && (access == ak_none || base_access_now < access))
		{
		  access = base_access_now;

		  /* If the new access is public, we can't do better.  */
		  if (access == ak_public)
		    break;
		}
	    }
	}
    }

  /* Note the access to DECL in TYPE.  */
  SET_BINFO_ACCESS (binfo, access);

  return NULL_TREE;
}

/* Return the access to DECL in TYPE.  */

static access_kind
access_in_type (tree type, tree decl)
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
  dfs_walk_once (binfo, dfs_access_in_type_pre, dfs_access_in_type, decl);

  return BINFO_ACCESS (binfo);
}

/* Returns nonzero if it is OK to access DECL named in TYPE through an object
   of OTYPE in the context of DERIVED.  */

static int
protected_accessible_p (tree decl, tree derived, tree type, tree otype)
{
  /* We're checking this clause from [class.access.base]

       m as a member of N is protected, and the reference occurs in a
       member or friend of class N, or in a member or friend of a
       class P derived from N, where m as a member of P is public, private
       or protected.

    Here DERIVED is a possible P, DECL is m and TYPE is N.  */

  /* If DERIVED isn't derived from N, then it can't be a P.  */
  if (!DERIVED_FROM_P (type, derived))
    return 0;

  /* DECL_NONSTATIC_MEMBER_P won't work for USING_DECLs.  */
  decl = strip_using_decl (decl);
  /* We don't expect or support dependent decls.  */
  gcc_assert (TREE_CODE (decl) != USING_DECL);

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
  if (DECL_NONSTATIC_MEMBER_P (decl)
      && !DERIVED_FROM_P (derived, otype))
    return 0;

  return 1;
}

/* Returns nonzero if SCOPE is a type or a friend of a type which would be able
   to access DECL through TYPE.  OTYPE is the type of the object.  */

static int
friend_accessible_p (tree scope, tree decl, tree type, tree otype)
{
  /* We're checking this clause from [class.access.base]

       m as a member of N is protected, and the reference occurs in a
       member or friend of class N, or in a member or friend of a
       class P derived from N, where m as a member of P is public, private
       or protected.

    Here DECL is m and TYPE is N.  SCOPE is the current context,
    and we check all its possible Ps.  */
  tree befriending_classes;
  tree t;

  if (!scope)
    return 0;

  if (is_global_friend (scope))
    return 1;

  /* Is SCOPE itself a suitable P?  */
  if (TYPE_P (scope) && protected_accessible_p (decl, scope, type, otype))
    return 1;

  if (DECL_DECLARES_FUNCTION_P (scope))
    befriending_classes = DECL_BEFRIENDING_CLASSES (scope);
  else if (TYPE_P (scope))
    befriending_classes = CLASSTYPE_BEFRIENDING_CLASSES (scope);
  else
    return 0;

  for (t = befriending_classes; t; t = TREE_CHAIN (t))
    if (protected_accessible_p (decl, TREE_VALUE (t), type, otype))
      return 1;

  /* Nested classes have the same access as their enclosing types, as
     per DR 45 (this is a change from C++98).  */
  if (TYPE_P (scope))
    if (friend_accessible_p (TYPE_CONTEXT (scope), decl, type, otype))
      return 1;

  if (DECL_DECLARES_FUNCTION_P (scope))
    {
      /* Perhaps this SCOPE is a member of a class which is a
	 friend.  */
      if (DECL_CLASS_SCOPE_P (scope)
	  && friend_accessible_p (DECL_CONTEXT (scope), decl, type, otype))
	return 1;
    }

  /* Maybe scope's template is a friend.  */
  if (tree tinfo = get_template_info (scope))
    {
      tree tmpl = TI_TEMPLATE (tinfo);
      if (DECL_CLASS_TEMPLATE_P (tmpl))
	tmpl = TREE_TYPE (tmpl);
      else
	tmpl = DECL_TEMPLATE_RESULT (tmpl);
      if (tmpl != scope)
	{
	  /* Increment processing_template_decl to make sure that
	     dependent_type_p works correctly.  */
	  ++processing_template_decl;
	  int ret = friend_accessible_p (tmpl, decl, type, otype);
	  --processing_template_decl;
	  if (ret)
	    return 1;
	}
    }

  /* If is_friend is true, we should have found a befriending class.  */
  gcc_checking_assert (!is_friend (type, scope));

  return 0;
}

struct dfs_accessible_data
{
  tree decl;
  tree object_type;
};

/* Avoid walking up past a declaration of the member.  */

static tree
dfs_accessible_pre (tree binfo, void *data)
{
  dfs_accessible_data *d = (dfs_accessible_data *)data;
  tree type = BINFO_TYPE (binfo);
  if (member_declared_in_type (d->decl, type))
    return dfs_skip_bases;
  return NULL_TREE;
}

/* Called via dfs_walk_once_accessible from accessible_p */

static tree
dfs_accessible_post (tree binfo, void *data)
{
  /* access_in_type already set BINFO_ACCESS for us.  */
  access_kind access = BINFO_ACCESS (binfo);
  tree N = BINFO_TYPE (binfo);
  dfs_accessible_data *d = (dfs_accessible_data *)data;
  tree decl = d->decl;
  tree scope = current_nonlambda_scope ();

  /* A member m is accessible at the point R when named in class N if */
  switch (access)
    {
    case ak_none:
      return NULL_TREE;

    case ak_public:
      /* m as a member of N is public, or */
      return binfo;

    case ak_private:
      {
	/* m as a member of N is private, and R occurs in a member or friend of
	   class N, or */
	if (scope && TREE_CODE (scope) != NAMESPACE_DECL
	    && is_friend (N, scope))
	  return binfo;
	return NULL_TREE;
      }

    case ak_protected:
      {
	/* m as a member of N is protected, and R occurs in a member or friend
	   of class N, or in a member or friend of a class P derived from N,
	   where m as a member of P is public, private, or protected  */
	if (friend_accessible_p (scope, decl, N, d->object_type))
	  return binfo;
	return NULL_TREE;
      }

    default:
      gcc_unreachable ();
    }
}

/* Like accessible_p below, but within a template returns true iff DECL is
   accessible in TYPE to all possible instantiations of the template.  */

int
accessible_in_template_p (tree type, tree decl)
{
  int save_ptd = processing_template_decl;
  processing_template_decl = 0;
  int val = accessible_p (type, decl, false);
  processing_template_decl = save_ptd;
  return val;
}

/* DECL is a declaration from a base class of TYPE, which was the
   class used to name DECL.  Return nonzero if, in the current
   context, DECL is accessible.  If TYPE is actually a BINFO node,
   then we can tell in what context the access is occurring by looking
   at the most derived class along the path indicated by BINFO.  If
   CONSIDER_LOCAL is true, do consider special access the current
   scope or friendship thereof we might have.  */

int
accessible_p (tree type, tree decl, bool consider_local_p)
{
  tree binfo;
  access_kind access;

  /* If this declaration is in a block or namespace scope, there's no
     access control.  */
  if (!TYPE_P (context_for_name_lookup (decl)))
    return 1;

  /* There is no need to perform access checks inside a thunk.  */
  if (current_function_decl && DECL_THUNK_P (current_function_decl))
    return 1;

  /* In a template declaration, we cannot be sure whether the
     particular specialization that is instantiated will be a friend
     or not.  Therefore, all access checks are deferred until
     instantiation.  However, PROCESSING_TEMPLATE_DECL is set in the
     parameter list for a template (because we may see dependent types
     in default arguments for template parameters), and access
     checking should be performed in the outermost parameter list.  */
  if (processing_template_decl
      /* FIXME CWG has been talking about doing access checking in the context
	 of the constraint-expression, rather than the constrained declaration,
	 in which case we would want to remove this test.  */
      && !processing_constraint_expression_p ()
      && (!processing_template_parmlist || processing_template_decl > 1))
    return 1;

  tree otype = NULL_TREE;
  if (!TYPE_P (type))
    {
      /* When accessing a non-static member, the most derived type in the
	 binfo chain is the type of the object; remember that type for
	 protected_accessible_p.  */
      for (tree b = type; b; b = BINFO_INHERITANCE_CHAIN (b))
	otype = BINFO_TYPE (b);
      type = BINFO_TYPE (type);
    }
  else
    otype = type;

  /* [class.access.base]

     A member m is accessible when named in class N if

     --m as a member of N is public, or

     --m as a member of N is private, and the reference occurs in a
       member or friend of class N, or

     --m as a member of N is protected, and the reference occurs in a
       member or friend of class N, or in a member or friend of a
       class P derived from N, where m as a member of P is public, private or
       protected, or

     --there exists a base class B of N that is accessible at the point
       of reference, and m is accessible when named in class B.

    We walk the base class hierarchy, checking these conditions.  */

  /* We walk using TYPE_BINFO (type) because access_in_type will set
     BINFO_ACCESS on it and its bases.  */
  binfo = TYPE_BINFO (type);

  /* Compute the accessibility of DECL in the class hierarchy
     dominated by type.  */
  access = access_in_type (type, decl);
  if (access == ak_public)
    return 1;

  /* If we aren't considering the point of reference, only the first bullet
     applies.  */
  if (!consider_local_p)
    return 0;

  dfs_accessible_data d = { decl, otype };

  /* Walk the hierarchy again, looking for a base class that allows
     access.  */
  return dfs_walk_once_accessible (binfo, /*friends=*/true,
				   dfs_accessible_pre,
				   dfs_accessible_post, &d)
    != NULL_TREE;
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
  /* If something went wrong, a message indicating what.  */
  const char *errstr;
};

/* Nonzero for a class member means that it is shared between all objects
   of that class.

   [class.member.lookup]:If the resulting set of declarations are not all
   from sub-objects of the same type, or the set has a  nonstatic  member
   and  includes members from distinct sub-objects, there is an ambiguity
   and the program is ill-formed.

   This function checks that T contains no nonstatic members.  */

int
shared_member_p (tree t)
{
  if (VAR_P (t) || TREE_CODE (t) == TYPE_DECL \
      || TREE_CODE (t) == CONST_DECL)
    return 1;
  if (is_overloaded_fn (t))
    {
      for (ovl_iterator iter (get_fns (t)); iter; ++iter)
	{
	  tree decl = strip_using_decl (*iter);
	  /* We don't expect or support dependent decls.  */
	  gcc_assert (TREE_CODE (decl) != USING_DECL);
	  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (decl))
	    return 0;
	}
      return 1;
    }
  return 0;
}

/* Routine to see if the sub-object denoted by the binfo PARENT can be
   found as a base class and sub-object of the object denoted by
   BINFO.  */

static int
is_subobject_of_p (tree parent, tree binfo)
{
  tree probe;

  for (probe = parent; probe; probe = BINFO_INHERITANCE_CHAIN (probe))
    {
      if (probe == binfo)
	return 1;
      if (BINFO_VIRTUAL_P (probe))
	return (binfo_for_vbase (BINFO_TYPE (probe), BINFO_TYPE (binfo))
		!= NULL_TREE);
    }
  return 0;
}

/* DATA is really a struct lookup_field_info.  Look for a field with
   the name indicated there in BINFO.  If this function returns a
   non-NULL value it is the result of the lookup.  Called from
   lookup_field via breadth_first_search.  */

static tree
lookup_field_r (tree binfo, void *data)
{
  struct lookup_field_info *lfi = (struct lookup_field_info *) data;
  tree type = BINFO_TYPE (binfo);
  tree nval = NULL_TREE;

  /* If this is a dependent base, don't look in it.  */
  if (BINFO_DEPENDENT_BASE_P (binfo))
    return NULL_TREE;

  /* If this base class is hidden by the best-known value so far, we
     don't need to look.  */
  if (lfi->rval_binfo && BINFO_INHERITANCE_CHAIN (binfo) == lfi->rval_binfo
      && !BINFO_VIRTUAL_P (binfo))
    return dfs_skip_bases;

  nval = get_class_binding (type, lfi->name, lfi->want_type);

  /* If we're looking up a type (as with an elaborated type specifier)
     we ignore all non-types we find.  */
  if (lfi->want_type && nval && !DECL_DECLARES_TYPE_P (nval))
    {
      nval = NULL_TREE;
      if (CLASSTYPE_NESTED_UTDS (type))
	if (binding_entry e = binding_table_find (CLASSTYPE_NESTED_UTDS (type),
						  lfi->name))
	  nval = TYPE_MAIN_DECL (e->type);
    }

  /* If there is no declaration with the indicated name in this type,
     then there's nothing to do.  */
  if (!nval)
    goto done;

  /* If the lookup already found a match, and the new value doesn't
     hide the old one, we might have an ambiguity.  */
  if (lfi->rval_binfo
      && !is_subobject_of_p (lfi->rval_binfo, binfo))

    {
      if (nval == lfi->rval && shared_member_p (nval))
	/* The two things are really the same.  */
	;
      else if (is_subobject_of_p (binfo, lfi->rval_binfo))
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
	  lfi->errstr = G_("request for member %qD is ambiguous");
	}
    }
  else
    {
      lfi->rval = nval;
      lfi->rval_binfo = binfo;
    }

 done:
  /* Don't look for constructors or destructors in base classes.  */
  if (IDENTIFIER_CDTOR_P (lfi->name))
    return dfs_skip_bases;
  return NULL_TREE;
}

/* Return a "baselink" with BASELINK_BINFO, BASELINK_ACCESS_BINFO,
   BASELINK_FUNCTIONS, and BASELINK_OPTYPE set to BINFO, ACCESS_BINFO,
   FUNCTIONS, and OPTYPE respectively.  */

tree
build_baselink (tree binfo, tree access_binfo, tree functions, tree optype)
{
  tree baselink;

  gcc_assert (OVL_P (functions) || TREE_CODE (functions) == TEMPLATE_ID_EXPR);
  gcc_assert (!optype || TYPE_P (optype));
  gcc_assert (TREE_TYPE (functions));

  baselink = make_node (BASELINK);
  TREE_TYPE (baselink) = TREE_TYPE (functions);
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

   If nothing can be found return NULL_TREE and do not issue an error.

   If non-NULL, failure information is written back to AFI.  */

tree
lookup_member (tree xbasetype, tree name, int protect, bool want_type,
	       tsubst_flags_t complain, access_failure_info *afi)
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

  if (name == error_mark_node
      || xbasetype == NULL_TREE
      || xbasetype == error_mark_node)
    return NULL_TREE;

  gcc_assert (identifier_p (name));

  if (TREE_CODE (xbasetype) == TREE_BINFO)
    {
      type = BINFO_TYPE (xbasetype);
      basetype_path = xbasetype;
    }
  else
    {
      if (!RECORD_OR_UNION_CODE_P (TREE_CODE (xbasetype)))
	return NULL_TREE;
      type = xbasetype;
      xbasetype = NULL_TREE;
    }

  type = complete_type (type);

  /* Make sure we're looking for a member of the current instantiation in the
     right partial specialization.  */
  if (dependent_type_p (type))
    if (tree t = currently_open_class (type))
      type = t;

  if (!basetype_path)
    basetype_path = TYPE_BINFO (type);

  if (!basetype_path)
    return NULL_TREE;

  memset (&lfi, 0, sizeof (lfi));
  lfi.type = type;
  lfi.name = name;
  lfi.want_type = want_type;
  dfs_walk_all (basetype_path, &lookup_field_r, NULL, &lfi);
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
     applied to the function selected by overloaded resolution.  

     We cannot check here, even if RVAL is only a single non-static
     member function, since we do not know what the "this" pointer
     will be.  For:

        class A { protected: void f(); };
        class B : public A { 
          void g(A *p) {
            f(); // OK
            p->f(); // Not OK.
          }
        };

    only the first call to "f" is valid.  However, if the function is
    static, we can check.  */
  if (rval && protect 
      && !really_overloaded_fn (rval))
    {
      tree decl = is_overloaded_fn (rval) ? get_first_fn (rval) : rval;
      decl = strip_using_decl (decl);
      /* A dependent USING_DECL will be checked after tsubsting.  */
      if (TREE_CODE (decl) != USING_DECL
	  && !DECL_NONSTATIC_MEMBER_FUNCTION_P (decl)
	  && !perform_or_defer_access_check (basetype_path, decl, decl,
					     complain, afi))
	rval = error_mark_node;
    }

  if (errstr && protect)
    {
      if (complain & tf_error)
	{
	  error (errstr, name, type);
	  if (lfi.ambiguous)
	    print_candidates (lfi.ambiguous);
	}
      rval = error_mark_node;
    }

  if (rval && is_overloaded_fn (rval))
    rval = build_baselink (rval_binfo, basetype_path, rval,
			   (IDENTIFIER_CONV_OP_P (name)
			   ? TREE_TYPE (name): NULL_TREE));
  return rval;
}

/* Helper class for lookup_member_fuzzy.  */

class lookup_field_fuzzy_info
{
 public:
  lookup_field_fuzzy_info (bool want_type_p) :
    m_want_type_p (want_type_p), m_candidates () {}

  void fuzzy_lookup_field (tree type);

  /* If true, we are looking for types, not data members.  */
  bool m_want_type_p;
  /* The result: a vec of identifiers.  */
  auto_vec<tree> m_candidates;
};

/* Locate all fields within TYPE, append them to m_candidates.  */

void
lookup_field_fuzzy_info::fuzzy_lookup_field (tree type)
{
  if (!CLASS_TYPE_P (type))
    return;

  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      if (m_want_type_p && !DECL_DECLARES_TYPE_P (field))
	continue;

      if (!DECL_NAME (field))
	continue;

      if (is_lambda_ignored_entity (field))
	continue;

      m_candidates.safe_push (DECL_NAME (field));
    }
}


/* Helper function for lookup_member_fuzzy, called via dfs_walk_all
   DATA is really a lookup_field_fuzzy_info.  Look for a field with
   the name indicated there in BINFO.  Gathers pertinent identifiers into
   m_candidates.  */

static tree
lookup_field_fuzzy_r (tree binfo, void *data)
{
  lookup_field_fuzzy_info *lffi = (lookup_field_fuzzy_info *) data;
  tree type = BINFO_TYPE (binfo);

  lffi->fuzzy_lookup_field (type);

  return NULL_TREE;
}

/* Like lookup_member, but try to find the closest match for NAME,
   rather than an exact match, and return an identifier (or NULL_TREE).
   Do not complain.  */

tree
lookup_member_fuzzy (tree xbasetype, tree name, bool want_type_p)
{
  tree type = NULL_TREE, basetype_path = NULL_TREE;
  class lookup_field_fuzzy_info lffi (want_type_p);

  /* rval_binfo is the binfo associated with the found member, note,
     this can be set with useful information, even when rval is not
     set, because it must deal with ALL members, not just non-function
     members.  It is used for ambiguity checking and the hidden
     checks.  Whereas rval is only set if a proper (not hidden)
     non-function member is found.  */

  if (name == error_mark_node
      || xbasetype == NULL_TREE
      || xbasetype == error_mark_node)
    return NULL_TREE;

  gcc_assert (identifier_p (name));

  if (TREE_CODE (xbasetype) == TREE_BINFO)
    {
      type = BINFO_TYPE (xbasetype);
      basetype_path = xbasetype;
    }
  else
    {
      if (!RECORD_OR_UNION_CODE_P (TREE_CODE (xbasetype)))
	return NULL_TREE;
      type = xbasetype;
      xbasetype = NULL_TREE;
    }

  type = complete_type (type);

  /* Make sure we're looking for a member of the current instantiation in the
     right partial specialization.  */
  if (flag_concepts && dependent_type_p (type))
    type = currently_open_class (type);

  if (!basetype_path)
    basetype_path = TYPE_BINFO (type);

  if (!basetype_path)
    return NULL_TREE;

  /* Populate lffi.m_candidates.  */
  dfs_walk_all (basetype_path, &lookup_field_fuzzy_r, NULL, &lffi);

  return find_closest_identifier (name, &lffi.m_candidates);
}

/* Like lookup_member, except that if we find a function member we
   return NULL_TREE.  */

tree
lookup_field (tree xbasetype, tree name, int protect, bool want_type)
{
  tree rval = lookup_member (xbasetype, name, protect, want_type,
			     tf_warning_or_error);

  /* Ignore functions, but propagate the ambiguity list.  */
  if (!error_operand_p (rval)
      && (rval && BASELINK_P (rval)))
    return NULL_TREE;

  return rval;
}

/* Like lookup_member, except that if we find a non-function member we
   return NULL_TREE.  */

tree
lookup_fnfields (tree xbasetype, tree name, int protect)
{
  tree rval = lookup_member (xbasetype, name, protect, /*want_type=*/false,
			     tf_warning_or_error);

  /* Ignore non-functions, but propagate the ambiguity list.  */
  if (!error_operand_p (rval)
      && (rval && !BASELINK_P (rval)))
    return NULL_TREE;

  return rval;
}

/* DECL is the result of a qualified name lookup.  QUALIFYING_SCOPE is
   the class or namespace used to qualify the name.  CONTEXT_CLASS is
   the class corresponding to the object in which DECL will be used.
   Return a possibly modified version of DECL that takes into account
   the CONTEXT_CLASS.

   In particular, consider an expression like `B::m' in the context of
   a derived class `D'.  If `B::m' has been resolved to a BASELINK,
   then the most derived class indicated by the BASELINK_BINFO will be
   `B', not `D'.  This function makes that adjustment.  */

tree
adjust_result_of_qualified_name_lookup (tree decl,
					tree qualifying_scope,
					tree context_class)
{
  if (context_class && context_class != error_mark_node
      && CLASS_TYPE_P (context_class)
      && CLASS_TYPE_P (qualifying_scope)
      && DERIVED_FROM_P (qualifying_scope, context_class)
      && BASELINK_P (decl))
    {
      tree base;

      /* Look for the QUALIFYING_SCOPE as a base of the CONTEXT_CLASS.
	 Because we do not yet know which function will be chosen by
	 overload resolution, we cannot yet check either accessibility
	 or ambiguity -- in either case, the choice of a static member
	 function might make the usage valid.  */
      base = lookup_base (context_class, qualifying_scope,
			  ba_unique, NULL, tf_none);
      if (base && base != error_mark_node)
	{
	  BASELINK_ACCESS_BINFO (decl) = base;
	  tree decl_binfo
	    = lookup_base (base, BINFO_TYPE (BASELINK_BINFO (decl)),
			   ba_unique, NULL, tf_none);
	  if (decl_binfo && decl_binfo != error_mark_node)
	    BASELINK_BINFO (decl) = decl_binfo;
	}
    }

  if (BASELINK_P (decl))
    BASELINK_QUALIFIED_P (decl) = true;

  return decl;
}


/* Walk the class hierarchy within BINFO, in a depth-first traversal.
   PRE_FN is called in preorder, while POST_FN is called in postorder.
   If PRE_FN returns DFS_SKIP_BASES, child binfos will not be
   walked.  If PRE_FN or POST_FN returns a different non-NULL value,
   that value is immediately returned and the walk is terminated.  One
   of PRE_FN and POST_FN can be NULL.  At each node, PRE_FN and
   POST_FN are passed the binfo to examine and the caller's DATA
   value.  All paths are walked, thus virtual and morally virtual
   binfos can be multiply walked.  */

tree
dfs_walk_all (tree binfo, tree (*pre_fn) (tree, void *),
	      tree (*post_fn) (tree, void *), void *data)
{
  tree rval;
  unsigned ix;
  tree base_binfo;

  /* Call the pre-order walking function.  */
  if (pre_fn)
    {
      rval = pre_fn (binfo, data);
      if (rval)
	{
	  if (rval == dfs_skip_bases)
	    goto skip_bases;
	  return rval;
	}
    }

  /* Find the next child binfo to walk.  */
  for (ix = 0; BINFO_BASE_ITERATE (binfo, ix, base_binfo); ix++)
    {
      rval = dfs_walk_all (base_binfo, pre_fn, post_fn, data);
      if (rval)
	return rval;
    }

 skip_bases:
  /* Call the post-order walking function.  */
  if (post_fn)
    {
      rval = post_fn (binfo, data);
      gcc_assert (rval != dfs_skip_bases);
      return rval;
    }

  return NULL_TREE;
}

/* Worker for dfs_walk_once.  This behaves as dfs_walk_all, except
   that binfos are walked at most once.  */

static tree
dfs_walk_once_r (tree binfo, tree (*pre_fn) (tree, void *),
		 tree (*post_fn) (tree, void *), hash_set<tree> *pset,
		 void *data)
{
  tree rval;
  unsigned ix;
  tree base_binfo;

  /* Call the pre-order walking function.  */
  if (pre_fn)
    {
      rval = pre_fn (binfo, data);
      if (rval)
	{
	  if (rval == dfs_skip_bases)
	    goto skip_bases;

	  return rval;
	}
    }

  /* Find the next child binfo to walk.  */
  for (ix = 0; BINFO_BASE_ITERATE (binfo, ix, base_binfo); ix++)
    {
      if (BINFO_VIRTUAL_P (base_binfo))
	if (pset->add (base_binfo))
	  continue;

      rval = dfs_walk_once_r (base_binfo, pre_fn, post_fn, pset, data);
      if (rval)
	return rval;
    }

 skip_bases:
  /* Call the post-order walking function.  */
  if (post_fn)
    {
      rval = post_fn (binfo, data);
      gcc_assert (rval != dfs_skip_bases);
      return rval;
    }

  return NULL_TREE;
}

/* Like dfs_walk_all, except that binfos are not multiply walked.  For
   non-diamond shaped hierarchies this is the same as dfs_walk_all.
   For diamond shaped hierarchies we must mark the virtual bases, to
   avoid multiple walks.  */

tree
dfs_walk_once (tree binfo, tree (*pre_fn) (tree, void *),
	       tree (*post_fn) (tree, void *), void *data)
{
  static int active = 0;  /* We must not be called recursively. */
  tree rval;

  gcc_assert (pre_fn || post_fn);
  gcc_assert (!active);
  active++;

  if (!CLASSTYPE_DIAMOND_SHAPED_P (BINFO_TYPE (binfo)))
    /* We are not diamond shaped, and therefore cannot encounter the
       same binfo twice.  */
    rval = dfs_walk_all (binfo, pre_fn, post_fn, data);
  else
    {
      hash_set<tree> pset;
      rval = dfs_walk_once_r (binfo, pre_fn, post_fn, &pset, data);
    }

  active--;

  return rval;
}

/* Worker function for dfs_walk_once_accessible.  Behaves like
   dfs_walk_once_r, except (a) FRIENDS_P is true if special
   access given by the current context should be considered, (b) ONCE
   indicates whether bases should be marked during traversal.  */

static tree
dfs_walk_once_accessible_r (tree binfo, bool friends_p, hash_set<tree> *pset,
			    tree (*pre_fn) (tree, void *),
			    tree (*post_fn) (tree, void *), void *data)
{
  tree rval = NULL_TREE;
  unsigned ix;
  tree base_binfo;

  /* Call the pre-order walking function.  */
  if (pre_fn)
    {
      rval = pre_fn (binfo, data);
      if (rval)
	{
	  if (rval == dfs_skip_bases)
	    goto skip_bases;

	  return rval;
	}
    }

  /* Find the next child binfo to walk.  */
  for (ix = 0; BINFO_BASE_ITERATE (binfo, ix, base_binfo); ix++)
    {
      bool mark = pset && BINFO_VIRTUAL_P (base_binfo);

      if (mark && pset->contains (base_binfo))
	continue;

      /* If the base is inherited via private or protected
	 inheritance, then we can't see it, unless we are a friend of
	 the current binfo.  */
      if (BINFO_BASE_ACCESS (binfo, ix) != access_public_node)
	{
	  tree scope;
	  if (!friends_p)
	    continue;
	  scope = current_scope ();
	  if (!scope
	      || TREE_CODE (scope) == NAMESPACE_DECL
	      || !is_friend (BINFO_TYPE (binfo), scope))
	    continue;
	}

      if (mark)
	pset->add (base_binfo);

      rval = dfs_walk_once_accessible_r (base_binfo, friends_p, pset,
					 pre_fn, post_fn, data);
      if (rval)
	return rval;
    }

 skip_bases:
  /* Call the post-order walking function.  */
  if (post_fn)
    {
      rval = post_fn (binfo, data);
      gcc_assert (rval != dfs_skip_bases);
      return rval;
    }

  return NULL_TREE;
}

/* Like dfs_walk_once except that only accessible bases are walked.
   FRIENDS_P indicates whether friendship of the local context
   should be considered when determining accessibility.  */

static tree
dfs_walk_once_accessible (tree binfo, bool friends_p,
			    tree (*pre_fn) (tree, void *),
			    tree (*post_fn) (tree, void *), void *data)
{
  hash_set<tree> *pset = NULL;
  if (CLASSTYPE_DIAMOND_SHAPED_P (BINFO_TYPE (binfo)))
    pset = new hash_set<tree>;
  tree rval = dfs_walk_once_accessible_r (binfo, friends_p, pset,
					  pre_fn, post_fn, data);

  if (pset)
    delete pset;
  return rval;
}

/* Return true iff the code of T is CODE, and it has compatible
   type with TYPE.  */

static bool
matches_code_and_type_p (tree t, enum tree_code code, tree type)
{
  if (TREE_CODE (t) != code)
    return false;
  if (!cxx_types_compatible_p (TREE_TYPE (t), type))
    return false;
  return true;
}

/* Subroutine of direct_accessor_p and reference_accessor_p.
   Determine if COMPONENT_REF is a simple field lookup of this->FIELD_DECL.
   We expect a tree of the form:
	     <component_ref:
	       <indirect_ref:S>
		 <nop_expr:P*
		   <parm_decl (this)>
		 <field_decl (FIELD_DECL)>>>.  */

static bool
field_access_p (tree component_ref, tree field_decl, tree field_type)
{
  if (!matches_code_and_type_p (component_ref, COMPONENT_REF, field_type))
    return false;

  tree indirect_ref = TREE_OPERAND (component_ref, 0);
  if (!INDIRECT_REF_P (indirect_ref))
    return false;

  tree ptr = STRIP_NOPS (TREE_OPERAND (indirect_ref, 0));
  if (!is_this_parameter (ptr))
    return false;

  /* Must access the correct field.  */
  if (TREE_OPERAND (component_ref, 1) != field_decl)
    return false;
  return true;
}

/* Subroutine of field_accessor_p.

   Assuming that INIT_EXPR has already had its code and type checked,
   determine if it is a simple accessor for FIELD_DECL
   (of type FIELD_TYPE).

   Specifically, a simple accessor within struct S of the form:
       T get_field () { return m_field; }
   should have a constexpr_fn_retval (saved_tree) of the form:
	 <init_expr:T
	   <result_decl:T
	   <nop_expr:T
	     <component_ref:
	       <indirect_ref:S>
		 <nop_expr:P*
		   <parm_decl (this)>
		 <field_decl (FIELD_DECL)>>>>>.  */

static bool
direct_accessor_p (tree init_expr, tree field_decl, tree field_type)
{
  tree result_decl = TREE_OPERAND (init_expr, 0);
  if (!matches_code_and_type_p (result_decl, RESULT_DECL, field_type))
    return false;

  tree component_ref = STRIP_NOPS (TREE_OPERAND (init_expr, 1));
  if (!field_access_p (component_ref, field_decl, field_type))
    return false;

  return true;
}

/* Subroutine of field_accessor_p.

   Assuming that INIT_EXPR has already had its code and type checked,
   determine if it is a "reference" accessor for FIELD_DECL
   (of type FIELD_REFERENCE_TYPE).

   Specifically, a simple accessor within struct S of the form:
       T& get_field () { return m_field; }
   should have a constexpr_fn_retval (saved_tree) of the form:
	 <init_expr:T&
	   <result_decl:T&
	   <nop_expr: T&
	     <addr_expr: T*
	       <component_ref:T
		 <indirect_ref:S
		   <nop_expr
		     <parm_decl (this)>>
		   <field (FIELD_DECL)>>>>>>.  */
static bool
reference_accessor_p (tree init_expr, tree field_decl, tree field_type,
		      tree field_reference_type)
{
  tree result_decl = TREE_OPERAND (init_expr, 0);
  if (!matches_code_and_type_p (result_decl, RESULT_DECL, field_reference_type))
    return false;

  tree field_pointer_type = build_pointer_type (field_type);
  tree addr_expr = STRIP_NOPS (TREE_OPERAND (init_expr, 1));
  if (!matches_code_and_type_p (addr_expr, ADDR_EXPR, field_pointer_type))
    return false;

  tree component_ref = STRIP_NOPS (TREE_OPERAND (addr_expr, 0));

  if (!field_access_p (component_ref, field_decl, field_type))
    return false;

  return true;
}

/* Return true if FN is an accessor method for FIELD_DECL.
   i.e. a method of the form { return FIELD; }, with no
   conversions.

   If CONST_P, then additionally require that FN be a const
   method.  */

static bool
field_accessor_p (tree fn, tree field_decl, bool const_p)
{
  if (TREE_CODE (fn) != FUNCTION_DECL)
    return false;

  /* We don't yet support looking up static data, just fields.  */
  if (TREE_CODE (field_decl) != FIELD_DECL)
    return false;

  tree fntype = TREE_TYPE (fn);
  if (TREE_CODE (fntype) != METHOD_TYPE)
    return false;

  /* If the field is accessed via a const "this" argument, verify
     that the "this" parameter is const.  */
  if (const_p)
    {
      tree this_class = class_of_this_parm (fntype);
      if (!TYPE_READONLY (this_class))
	return false;
    }

  tree saved_tree = DECL_SAVED_TREE (fn);

  if (saved_tree == NULL_TREE)
    return false;

  /* Attempt to extract a single return value from the function,
     if it has one.  */
  tree retval = constexpr_fn_retval (saved_tree);
  if (retval == NULL_TREE || retval == error_mark_node)
    return false;
  /* Require an INIT_EXPR.  */
  if (TREE_CODE (retval) != INIT_EXPR)
    return false;
  tree init_expr = retval;

  /* Determine if this is a simple accessor within struct S of the form:
       T get_field () { return m_field; }.  */
  tree field_type = TREE_TYPE (field_decl);
  if (cxx_types_compatible_p (TREE_TYPE (init_expr), field_type))
    return direct_accessor_p (init_expr, field_decl, field_type);

  /* Failing that, determine if it is an accessor of the form:
       T& get_field () { return m_field; }.  */
  tree field_reference_type = cp_build_reference_type (field_type, false);
  if (cxx_types_compatible_p (TREE_TYPE (init_expr), field_reference_type))
    return reference_accessor_p (init_expr, field_decl, field_type,
				 field_reference_type);

  return false;
}

/* Callback data for dfs_locate_field_accessor_pre.  */

class locate_field_data
{
public:
  locate_field_data (tree field_decl_, bool const_p_)
  : field_decl (field_decl_), const_p (const_p_) {}

  tree field_decl;
  bool const_p;
};

/* Return a FUNCTION_DECL that is an "accessor" method for DATA, a FIELD_DECL,
   callable via binfo, if one exists, otherwise return NULL_TREE.

   Callback for dfs_walk_once_accessible for use within
   locate_field_accessor.  */

static tree
dfs_locate_field_accessor_pre (tree binfo, void *data)
{
  locate_field_data *lfd = (locate_field_data *)data;
  tree type = BINFO_TYPE (binfo);

  vec<tree, va_gc> *member_vec;
  tree fn;
  size_t i;

  if (!CLASS_TYPE_P (type))
    return NULL_TREE;

  member_vec = CLASSTYPE_MEMBER_VEC (type);
  if (!member_vec)
    return NULL_TREE;

  for (i = 0; vec_safe_iterate (member_vec, i, &fn); ++i)
    if (fn)
      if (field_accessor_p (fn, lfd->field_decl, lfd->const_p))
	return fn;

  return NULL_TREE;
}

/* Return a FUNCTION_DECL that is an "accessor" method for FIELD_DECL,
   callable via BASETYPE_PATH, if one exists, otherwise return NULL_TREE.  */

tree
locate_field_accessor (tree basetype_path, tree field_decl, bool const_p)
{
  if (TREE_CODE (basetype_path) != TREE_BINFO)
    return NULL_TREE;

  /* Walk the hierarchy, looking for a method of some base class that allows
     access to the field.  */
  locate_field_data lfd (field_decl, const_p);
  return dfs_walk_once_accessible (basetype_path, /*friends=*/true,
				   dfs_locate_field_accessor_pre,
				   NULL, &lfd);
}

/* Check throw specifier of OVERRIDER is at least as strict as
   the one of BASEFN.  */

bool
maybe_check_overriding_exception_spec (tree overrider, tree basefn)
{
  maybe_instantiate_noexcept (basefn);
  maybe_instantiate_noexcept (overrider);
  tree base_throw = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (basefn));
  tree over_throw = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (overrider));

  if (DECL_INVALID_OVERRIDER_P (overrider))
    return true;

  /* Can't check this yet.  Pretend this is fine and let
     noexcept_override_late_checks check this later.  */
  if (UNPARSED_NOEXCEPT_SPEC_P (base_throw)
      || UNPARSED_NOEXCEPT_SPEC_P (over_throw))
    return true;

  if (!comp_except_specs (base_throw, over_throw, ce_derived))
    {
      auto_diagnostic_group d;
      error ("looser exception specification on overriding virtual function "
	     "%q+#F", overrider);
      inform (DECL_SOURCE_LOCATION (basefn),
	      "overridden function is %q#F", basefn);
      DECL_INVALID_OVERRIDER_P (overrider) = 1;
      return false;
    }
  return true;
}

/* Check that virtual overrider OVERRIDER is acceptable for base function
   BASEFN. Issue diagnostic, and return zero, if unacceptable.  */

static int
check_final_overrider (tree overrider, tree basefn)
{
  tree over_type = TREE_TYPE (overrider);
  tree base_type = TREE_TYPE (basefn);
  tree over_return = fndecl_declared_return_type (overrider);
  tree base_return = fndecl_declared_return_type (basefn);

  int fail = 0;

  if (DECL_INVALID_OVERRIDER_P (overrider))
    return 0;

  if (same_type_p (base_return, over_return))
    /* OK */;
  else if ((CLASS_TYPE_P (over_return) && CLASS_TYPE_P (base_return))
	   || (TREE_CODE (base_return) == TREE_CODE (over_return)
	       && INDIRECT_TYPE_P (base_return)))
    {
      /* Potentially covariant.  */
      unsigned base_quals, over_quals;

      fail = !INDIRECT_TYPE_P (base_return);
      if (!fail)
	{
	  fail = cp_type_quals (base_return) != cp_type_quals (over_return);

	  base_return = TREE_TYPE (base_return);
	  over_return = TREE_TYPE (over_return);
	}
      base_quals = cp_type_quals (base_return);
      over_quals = cp_type_quals (over_return);

      if ((base_quals & over_quals) != over_quals)
	fail = 1;

      if (CLASS_TYPE_P (base_return) && CLASS_TYPE_P (over_return))
	{
	  /* Strictly speaking, the standard requires the return type to be
	     complete even if it only differs in cv-quals, but that seems
	     like a bug in the wording.  */
	  if (!same_type_ignoring_top_level_qualifiers_p (base_return,
							  over_return))
	    {
	      tree binfo = lookup_base (over_return, base_return,
					ba_check, NULL, tf_none);

	      if (!binfo || binfo == error_mark_node)
		fail = 1;
	    }
	}
      else if (can_convert_standard (TREE_TYPE (base_type),
				     TREE_TYPE (over_type),
				     tf_warning_or_error))
	/* GNU extension, allow trivial pointer conversions such as
	   converting to void *, or qualification conversion.  */
	{
	  auto_diagnostic_group d;
	  if (pedwarn (DECL_SOURCE_LOCATION (overrider), 0,
		       "invalid covariant return type for %q#D", overrider))
	    inform (DECL_SOURCE_LOCATION (basefn),
		    "overridden function is %q#D", basefn);
	}
      else
	fail = 2;
    }
  else
    fail = 2;
  if (!fail)
    /* OK */;
  else
    {
      if (fail == 1)
	{
	  auto_diagnostic_group d;
	  error ("invalid covariant return type for %q+#D", overrider);
	  inform (DECL_SOURCE_LOCATION (basefn),
		  "overridden function is %q#D", basefn);
	}
      else
	{
	  auto_diagnostic_group d;
	  error ("conflicting return type specified for %q+#D", overrider);
	  inform (DECL_SOURCE_LOCATION (basefn),
		  "overridden function is %q#D", basefn);
	}
      DECL_INVALID_OVERRIDER_P (overrider) = 1;
      return 0;
    }

  if (!maybe_check_overriding_exception_spec (overrider, basefn))
    return 0;

  /* Check for conflicting type attributes.  But leave transaction_safe for
     set_one_vmethod_tm_attributes.  */
  if (!comp_type_attributes (over_type, base_type)
      && !tx_safe_fn_type_p (base_type)
      && !tx_safe_fn_type_p (over_type))
    {
      auto_diagnostic_group d;
      error ("conflicting type attributes specified for %q+#D", overrider);
      inform (DECL_SOURCE_LOCATION (basefn),
	      "overridden function is %q#D", basefn);
      DECL_INVALID_OVERRIDER_P (overrider) = 1;
      return 0;
    }

  /* A function declared transaction_safe_dynamic that overrides a function
     declared transaction_safe (but not transaction_safe_dynamic) is
     ill-formed.  */
  if (tx_safe_fn_type_p (base_type)
      && lookup_attribute ("transaction_safe_dynamic",
			   DECL_ATTRIBUTES (overrider))
      && !lookup_attribute ("transaction_safe_dynamic",
			    DECL_ATTRIBUTES (basefn)))
    {
      auto_diagnostic_group d;
      error_at (DECL_SOURCE_LOCATION (overrider),
		"%qD declared %<transaction_safe_dynamic%>", overrider);
      inform (DECL_SOURCE_LOCATION (basefn),
	      "overriding %qD declared %<transaction_safe%>", basefn);
    }

  if (DECL_DELETED_FN (basefn) != DECL_DELETED_FN (overrider))
    {
      if (DECL_DELETED_FN (overrider))
	{
	  auto_diagnostic_group d;
	  error ("deleted function %q+D overriding non-deleted function",
		 overrider);
	  inform (DECL_SOURCE_LOCATION (basefn),
		  "overridden function is %qD", basefn);
	  maybe_explain_implicit_delete (overrider);
	}
      else
	{
	  auto_diagnostic_group d;
	  error ("non-deleted function %q+D overriding deleted function",
		 overrider);
	  inform (DECL_SOURCE_LOCATION (basefn),
		  "overridden function is %qD", basefn);
	}
      return 0;
    }
  if (DECL_FINAL_P (basefn))
    {
      auto_diagnostic_group d;
      error ("virtual function %q+D overriding final function", overrider);
      inform (DECL_SOURCE_LOCATION (basefn),
	      "overridden function is %qD", basefn);
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
look_for_overrides (tree type, tree fndecl)
{
  tree binfo = TYPE_BINFO (type);
  tree base_binfo;
  int ix;
  int found = 0;

  /* A constructor for a class T does not override a function T
     in a base class.  */
  if (DECL_CONSTRUCTOR_P (fndecl))
    return 0;

  for (ix = 0; BINFO_BASE_ITERATE (binfo, ix, base_binfo); ix++)
    {
      tree basetype = BINFO_TYPE (base_binfo);

      if (TYPE_POLYMORPHIC_P (basetype))
	found += look_for_overrides_r (basetype, fndecl);
    }
  return found;
}

/* Look in TYPE for virtual functions with the same signature as
   FNDECL.  */

tree
look_for_overrides_here (tree type, tree fndecl)
{
  tree ovl = get_class_binding (type, DECL_NAME (fndecl));

  for (ovl_iterator iter (ovl); iter; ++iter)
    {
      tree fn = *iter;

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

  return NULL_TREE;
}

/* Look in TYPE for virtual functions overridden by FNDECL. Check both
   TYPE itself and its bases.  */

static int
look_for_overrides_r (tree type, tree fndecl)
{
  tree fn = look_for_overrides_here (type, fndecl);
  if (fn)
    {
      if (DECL_STATIC_FUNCTION_P (fndecl))
	{
	  /* A static member function cannot match an inherited
	     virtual member function.  */
	  auto_diagnostic_group d;
	  error ("%q+#D cannot be declared", fndecl);
	  error ("  since %q+#D declared in base class", fn);
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

/* Called via dfs_walk from dfs_get_pure_virtuals.  */

static tree
dfs_get_pure_virtuals (tree binfo, void *data)
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
	  vec_safe_push (CLASSTYPE_PURE_VIRTUALS (type), BV_FN (virtuals));
    }

  return NULL_TREE;
}

/* Set CLASSTYPE_PURE_VIRTUALS for TYPE.  */

void
get_pure_virtuals (tree type)
{
  /* Clear the CLASSTYPE_PURE_VIRTUALS list; whatever is already there
     is going to be overridden.  */
  CLASSTYPE_PURE_VIRTUALS (type) = NULL;
  /* Now, run through all the bases which are not primary bases, and
     collect the pure virtual functions.  We look at the vtable in
     each class to determine what pure virtual functions are present.
     (A primary base is not interesting because the derived class of
     which it is a primary base will contain vtable entries for the
     pure virtuals in the base class.  */
  dfs_walk_once (TYPE_BINFO (type), NULL, dfs_get_pure_virtuals, type);
}

/* Debug info for C++ classes can get very large; try to avoid
   emitting it everywhere.

   Note that this optimization wins even when the target supports
   BINCL (if only slightly), and reduces the amount of work for the
   linker.  */

void
maybe_suppress_debug_info (tree t)
{
  if (write_symbols == NO_DEBUG)
    return;

  /* We might have set this earlier in cp_finish_decl.  */
  TYPE_DECL_SUPPRESS_DEBUG (TYPE_MAIN_DECL (t)) = 0;

  /* Always emit the information for each class every time. */
  if (flag_emit_class_debug_always)
    return;

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
dfs_debug_mark (tree binfo, void * /*data*/)
{
  tree t = BINFO_TYPE (binfo);

  if (CLASSTYPE_DEBUG_REQUESTED (t))
    return dfs_skip_bases;

  CLASSTYPE_DEBUG_REQUESTED (t) = 1;

  return NULL_TREE;
}

/* Write out the debugging information for TYPE, whose vtable is being
   emitted.  Also walk through our bases and note that we want to
   write out information for them.  This avoids the problem of not
   writing any debug info for intermediate basetypes whose
   constructors, and thus the references to their vtables, and thus
   the vtables themselves, were optimized away.  */

void
note_debug_info_needed (tree type)
{
  if (TYPE_DECL_SUPPRESS_DEBUG (TYPE_NAME (type)))
    {
      TYPE_DECL_SUPPRESS_DEBUG (TYPE_NAME (type)) = 0;
      rest_of_type_compilation (type, namespace_bindings_p ());
    }

  dfs_walk_all (TYPE_BINFO (type), dfs_debug_mark, NULL, 0);
}

/* Helper for lookup_conversions_r.  TO_TYPE is the type converted to
   by a conversion op in base BINFO.  VIRTUAL_DEPTH is nonzero if
   BINFO is morally virtual, and VIRTUALNESS is nonzero if virtual
   bases have been encountered already in the tree walk.  PARENT_CONVS
   is the list of lists of conversion functions that could hide CONV
   and OTHER_CONVS is the list of lists of conversion functions that
   could hide or be hidden by CONV, should virtualness be involved in
   the hierarchy.  Merely checking the conversion op's name is not
   enough because two conversion operators to the same type can have
   different names.  Return nonzero if we are visible.  */

static int
check_hidden_convs (tree binfo, int virtual_depth, int virtualness,
		    tree to_type, tree parent_convs, tree other_convs)
{
  tree level, probe;

  /* See if we are hidden by a parent conversion.  */
  for (level = parent_convs; level; level = TREE_CHAIN (level))
    for (probe = TREE_VALUE (level); probe; probe = TREE_CHAIN (probe))
      if (same_type_p (to_type, TREE_TYPE (probe)))
	return 0;

  if (virtual_depth || virtualness)
    {
     /* In a virtual hierarchy, we could be hidden, or could hide a
	conversion function on the other_convs list.  */
      for (level = other_convs; level; level = TREE_CHAIN (level))
	{
	  int we_hide_them;
	  int they_hide_us;
	  tree *prev, other;

	  if (!(virtual_depth || TREE_STATIC (level)))
	    /* Neither is morally virtual, so cannot hide each other.  */
	    continue;

	  if (!TREE_VALUE (level))
	    /* They evaporated away already.  */
	    continue;

	  they_hide_us = (virtual_depth
			  && original_binfo (binfo, TREE_PURPOSE (level)));
	  we_hide_them = (!they_hide_us && TREE_STATIC (level)
			  && original_binfo (TREE_PURPOSE (level), binfo));

	  if (!(we_hide_them || they_hide_us))
	    /* Neither is within the other, so no hiding can occur.  */
	    continue;

	  for (prev = &TREE_VALUE (level), other = *prev; other;)
	    {
	      if (same_type_p (to_type, TREE_TYPE (other)))
		{
		  if (they_hide_us)
		    /* We are hidden.  */
		    return 0;

		  if (we_hide_them)
		    {
		      /* We hide the other one.  */
		      other = TREE_CHAIN (other);
		      *prev = other;
		      continue;
		    }
		}
	      prev = &TREE_CHAIN (other);
	      other = *prev;
	    }
	}
    }
  return 1;
}

/* Helper for lookup_conversions_r.  PARENT_CONVS is a list of lists
   of conversion functions, the first slot will be for the current
   binfo, if MY_CONVS is non-NULL.  CHILD_CONVS is the list of lists
   of conversion functions from children of the current binfo,
   concatenated with conversions from elsewhere in the hierarchy --
   that list begins with OTHER_CONVS.  Return a single list of lists
   containing only conversions from the current binfo and its
   children.  */

static tree
split_conversions (tree my_convs, tree parent_convs,
		   tree child_convs, tree other_convs)
{
  tree t;
  tree prev;

  /* Remove the original other_convs portion from child_convs.  */
  for (prev = NULL, t = child_convs;
       t != other_convs; prev = t, t = TREE_CHAIN (t))
    continue;

  if (prev)
    TREE_CHAIN (prev) = NULL_TREE;
  else
    child_convs = NULL_TREE;

  /* Attach the child convs to any we had at this level.  */
  if (my_convs)
    {
      my_convs = parent_convs;
      TREE_CHAIN (my_convs) = child_convs;
    }
  else
    my_convs = child_convs;

  return my_convs;
}

/* Worker for lookup_conversions.  Lookup conversion functions in
   BINFO and its children.  VIRTUAL_DEPTH is nonzero, if BINFO is in a
   morally virtual base, and VIRTUALNESS is nonzero, if we've
   encountered virtual bases already in the tree walk.  PARENT_CONVS
   is a list of conversions within parent binfos.  OTHER_CONVS are
   conversions found elsewhere in the tree.  Return the conversions
   found within this portion of the graph in CONVS.  Return nonzero if
   we encountered virtualness.  We keep template and non-template
   conversions separate, to avoid unnecessary type comparisons.

   The located conversion functions are held in lists of lists.  The
   TREE_VALUE of the outer list is the list of conversion functions
   found in a particular binfo.  The TREE_PURPOSE of both the outer
   and inner lists is the binfo at which those conversions were
   found.  TREE_STATIC is set for those lists within of morally
   virtual binfos.  The TREE_VALUE of the inner list is the conversion
   function or overload itself.  The TREE_TYPE of each inner list node
   is the converted-to type.  */

static int
lookup_conversions_r (tree binfo, int virtual_depth, int virtualness,
		      tree parent_convs, tree other_convs, tree *convs)
{
  int my_virtualness = 0;
  tree my_convs = NULL_TREE;
  tree child_convs = NULL_TREE;

  /* If we have no conversion operators, then don't look.  */
  if (!TYPE_HAS_CONVERSION (BINFO_TYPE (binfo)))
    {
      *convs = NULL_TREE;

      return 0;
    }

  if (BINFO_VIRTUAL_P (binfo))
    virtual_depth++;

  /* First, locate the unhidden ones at this level.  */
  if (tree conv = get_class_binding (BINFO_TYPE (binfo), conv_op_identifier))
  for (ovl_iterator iter (conv); iter; ++iter)
    {
      tree fn = *iter;
      tree type = DECL_CONV_FN_TYPE (fn);

      if (TREE_CODE (fn) != TEMPLATE_DECL && type_uses_auto (type))
	{
	  mark_used (fn);
	  type = DECL_CONV_FN_TYPE (fn);
	}

      if (check_hidden_convs (binfo, virtual_depth, virtualness,
			      type, parent_convs, other_convs))
	{
	  my_convs = tree_cons (binfo, fn, my_convs);
	  TREE_TYPE (my_convs) = type;
	  if (virtual_depth)
	    {
	      TREE_STATIC (my_convs) = 1;
	      my_virtualness = 1;
	    }
	}
    }

  if (my_convs)
    {
      parent_convs = tree_cons (binfo, my_convs, parent_convs);
      if (virtual_depth)
	TREE_STATIC (parent_convs) = 1;
    }

  child_convs = other_convs;

  /* Now iterate over each base, looking for more conversions.  */
  unsigned i;
  tree base_binfo;
  for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    {
      tree base_convs;
      unsigned base_virtualness;

      base_virtualness = lookup_conversions_r (base_binfo,
					       virtual_depth, virtualness,
					       parent_convs, child_convs,
					       &base_convs);
      if (base_virtualness)
	my_virtualness = virtualness = 1;
      child_convs = chainon (base_convs, child_convs);
    }

  *convs = split_conversions (my_convs, parent_convs,
			      child_convs, other_convs);

  return my_virtualness;
}

/* Return a TREE_LIST containing all the non-hidden user-defined
   conversion functions for TYPE (and its base-classes).  The
   TREE_VALUE of each node is the FUNCTION_DECL of the conversion
   function.  The TREE_PURPOSE is the BINFO from which the conversion
   functions in this node were selected.  This function is effectively
   performing a set of member lookups as lookup_fnfield does, but
   using the type being converted to as the unique key, rather than the
   field name.  */

tree
lookup_conversions (tree type)
{
  tree convs;

  complete_type (type);
  if (!CLASS_TYPE_P (type) || !TYPE_BINFO (type))
    return NULL_TREE;

  lookup_conversions_r (TYPE_BINFO (type), 0, 0, NULL_TREE, NULL_TREE, &convs);

  tree list = NULL_TREE;
  
  /* Flatten the list-of-lists */
  for (; convs; convs = TREE_CHAIN (convs))
    {
      tree probe, next;

      for (probe = TREE_VALUE (convs); probe; probe = next)
	{
	  next = TREE_CHAIN (probe);

	  TREE_CHAIN (probe) = list;
	  list = probe;
	}
    }

  return list;
}

/* Returns the binfo of the first direct or indirect virtual base derived
   from BINFO, or NULL if binfo is not via virtual.  */

tree
binfo_from_vbase (tree binfo)
{
  for (; binfo; binfo = BINFO_INHERITANCE_CHAIN (binfo))
    {
      if (BINFO_VIRTUAL_P (binfo))
	return binfo;
    }
  return NULL_TREE;
}

/* Returns the binfo of the first direct or indirect virtual base derived
   from BINFO up to the TREE_TYPE, LIMIT, or NULL if binfo is not
   via virtual.  */

tree
binfo_via_virtual (tree binfo, tree limit)
{
  if (limit && !CLASSTYPE_VBASECLASSES (limit))
    /* LIMIT has no virtual bases, so BINFO cannot be via one.  */
    return NULL_TREE;

  for (; binfo && !SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), limit);
       binfo = BINFO_INHERITANCE_CHAIN (binfo))
    {
      if (BINFO_VIRTUAL_P (binfo))
	return binfo;
    }
  return NULL_TREE;
}

/* BINFO is for a base class in some hierarchy.  Return true iff it is a
   direct base.  */

bool
binfo_direct_p (tree binfo)
{
  tree d_binfo = BINFO_INHERITANCE_CHAIN (binfo);
  if (BINFO_INHERITANCE_CHAIN (d_binfo))
    /* A second inheritance chain means indirect.  */
    return false;
  if (!BINFO_VIRTUAL_P (binfo))
    /* Non-virtual, so only one inheritance chain means direct.  */
    return true;
  /* A virtual base looks like a direct base, so we need to look through the
     direct bases to see if it's there.  */
  tree b_binfo;
  for (int i = 0; BINFO_BASE_ITERATE (d_binfo, i, b_binfo); ++i)
    if (b_binfo == binfo)
      return true;
  return false;
}

/* BINFO is a base binfo in the complete type BINFO_TYPE (HERE).
   Find the equivalent binfo within whatever graph HERE is located.
   This is the inverse of original_binfo.  */

tree
copied_binfo (tree binfo, tree here)
{
  tree result = NULL_TREE;

  if (BINFO_VIRTUAL_P (binfo))
    {
      tree t;

      for (t = here; BINFO_INHERITANCE_CHAIN (t);
	   t = BINFO_INHERITANCE_CHAIN (t))
	continue;

      result = binfo_for_vbase (BINFO_TYPE (binfo), BINFO_TYPE (t));
    }
  else if (BINFO_INHERITANCE_CHAIN (binfo))
    {
      tree cbinfo;
      tree base_binfo;
      int ix;

      cbinfo = copied_binfo (BINFO_INHERITANCE_CHAIN (binfo), here);
      for (ix = 0; BINFO_BASE_ITERATE (cbinfo, ix, base_binfo); ix++)
	if (SAME_BINFO_TYPE_P (BINFO_TYPE (base_binfo), BINFO_TYPE (binfo)))
	  {
	    result = base_binfo;
	    break;
	  }
    }
  else
    {
      gcc_assert (SAME_BINFO_TYPE_P (BINFO_TYPE (here), BINFO_TYPE (binfo)));
      result = here;
    }

  gcc_assert (result);
  return result;
}

tree
binfo_for_vbase (tree base, tree t)
{
  unsigned ix;
  tree binfo;
  vec<tree, va_gc> *vbases;

  for (vbases = CLASSTYPE_VBASECLASSES (t), ix = 0;
       vec_safe_iterate (vbases, ix, &binfo); ix++)
    if (SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), base))
      return binfo;
  return NULL;
}

/* BINFO is some base binfo of HERE, within some other
   hierarchy. Return the equivalent binfo, but in the hierarchy
   dominated by HERE.  This is the inverse of copied_binfo.  If BINFO
   is not a base binfo of HERE, returns NULL_TREE.  */

tree
original_binfo (tree binfo, tree here)
{
  tree result = NULL;

  if (SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), BINFO_TYPE (here)))
    result = here;
  else if (BINFO_VIRTUAL_P (binfo))
    result = (CLASSTYPE_VBASECLASSES (BINFO_TYPE (here))
	      ? binfo_for_vbase (BINFO_TYPE (binfo), BINFO_TYPE (here))
	      : NULL_TREE);
  else if (BINFO_INHERITANCE_CHAIN (binfo))
    {
      tree base_binfos;

      base_binfos = original_binfo (BINFO_INHERITANCE_CHAIN (binfo), here);
      if (base_binfos)
	{
	  int ix;
	  tree base_binfo;

	  for (ix = 0; (base_binfo = BINFO_BASE_BINFO (base_binfos, ix)); ix++)
	    if (SAME_BINFO_TYPE_P (BINFO_TYPE (base_binfo),
				   BINFO_TYPE (binfo)))
	      {
		result = base_binfo;
		break;
	      }
	}
    }

  return result;
}

/* True iff TYPE has any dependent bases (and therefore we can't say
   definitively that another class is not a base of an instantiation of
   TYPE).  */

bool
any_dependent_bases_p (tree type)
{
  if (!type || !CLASS_TYPE_P (type) || !uses_template_parms (type))
    return false;

  /* If we haven't set TYPE_BINFO yet, we don't know anything about the bases.
     Return false because in this situation we aren't actually looking up names
     in the scope of the class, so it doesn't matter whether it has dependent
     bases.  */
  if (!TYPE_BINFO (type))
    return false;

  unsigned i;
  tree base_binfo;
  FOR_EACH_VEC_SAFE_ELT (BINFO_BASE_BINFOS (TYPE_BINFO (type)), i, base_binfo)
    if (BINFO_DEPENDENT_BASE_P (base_binfo))
      return true;

  return false;
}
