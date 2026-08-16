/* Handling for the known behavior of various functions specific to C++.
   Copyright (C) 2020-2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "analyzer/common.h"
#include "cgraph.h"
#include "ipa-utils.h"

#include "diagnostic.h"

#include "analyzer/analyzer-logging.h"
#include "analyzer/region-model.h"
#include "analyzer/call-details.h"

#if ENABLE_ANALYZER

/* Return true if CALL is a non-allocating operator new or operator new []
   that contains no user-defined args, i.e. having any signature of:

    - void* operator new (std::size_t count, void* ptr);
    - void* operator new[] (std::size_t count, void* ptr);

   See https://en.cppreference.com/w/cpp/memory/new/operator_new.  */

bool is_placement_new_p (const gcall &call)
{
  tree fndecl = gimple_call_fndecl (&call);

  if (!fndecl || TREE_CODE (TREE_TYPE (fndecl)) == METHOD_TYPE)
    /* Give up on overloaded operator new.  */
    return false;

  if (!is_named_call_p (fndecl, "operator new", call, 2)
      && !is_named_call_p (fndecl, "operator new []", call, 2))
    return false;

  /* We must distinguish between an allocating non-throwing new
    and a non-allocating new.

    The former might have one of the following signatures :
    void* operator new (std::size_t count, const std::nothrow_t& tag);
    void* operator new[] (std::size_t count, const std::nothrow_t& tag);
    Whereas a placement new would take a pointer.  */
  tree arg1_type = TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
  return TREE_CODE (TREE_VALUE (arg1_type)) == POINTER_TYPE;
}

namespace ana {

/* Implementations of specific functions.  */

/* Handler for __dynamic_cast.  */

/* A candidate TYPE subobject found on one inheritance path.  */

struct dyncast_subobject
{
  dyncast_subobject () : binfo (NULL_TREE), accessible (false) {}
  dyncast_subobject (tree binfo, bool accessible)
    : binfo (binfo), accessible (accessible)
  {}

  tree binfo;	   /* the BINFO that represents our subobject.
		      NULL_TREE if failed.  */
  bool accessible; /* Every edge from the root was public.  */
};

/* Recover the class type from a type_info argument of __dynamic_cast, expected
   to be &_ZTIxxx.  The C++ FE sets TREE_TYPE on the tinfo decl's DECL_NAME
   identifier.  __dynamic_cast is callable directly, so a runtime tinfo pointer
   (an SSA name) can exist.  Return NULL_TREE on any shape mismatch.  */

static tree
get_type_from_tinfo_arg (tree arg)
{
  if (!arg || TREE_CODE (arg) != ADDR_EXPR)
    return NULL_TREE;
  tree tinfo_decl = TREE_OPERAND (arg, 0);
  if (!DECL_P (tinfo_decl) || !DECL_NAME (tinfo_decl))
    return NULL_TREE;
  tree type = TREE_TYPE (DECL_NAME (tinfo_decl));
  if (!type || !RECORD_OR_UNION_TYPE_P (type))
    return NULL_TREE;
  return TYPE_MAIN_VARIANT (type);
}

/* Find the sub-BINFO of BINFO that has type TARGET_TYPE and sits at the same
   address as BINFO itself (i.e. at relative offset 0) by descending through
   every base at that same (absolute) offset.  */

static tree
lookup_binfo_at_same_offset (tree binfo, tree target_type)
{
  if (types_same_for_odr (BINFO_TYPE (binfo), target_type))
    return binfo;

  tree offset = BINFO_OFFSET (binfo);
  tree base_binfo;
  for (unsigned i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    if (tree_int_cst_equal (BINFO_OFFSET (base_binfo), offset))
      if (tree found = lookup_binfo_at_same_offset (base_binfo, target_type))
	return found;
  return NULL_TREE;
}

/* Look recursively for a BINFO that matches our DST_TYPE.  This method might
   find multiple matches. If none is found, matches won't be changed.

   Even if one path is private, it is still ambiguous according to the
   definition, so that case still counts as having multiple matches.  That
   means we ignore access specifiers when searching bases.

   Morally virtual matches of the same type under different virtual ancestors
   are distinct subobjects, i.e:

			 class B0 {};
	class V1 : B0 {};            class V2 : B0 {};
    class B2 : virtual V1 {};    class B4 : virtual V2 {};
		     class MD : B2, B3 {};

   Here, each B0 is a different subobject, so we must account for this case
   when checking virtual inheritance.  We compare the BINFO_OFFSET of all the
   BINFOs that match (the offset is relative to our most-derived object) to
   decide if they belong to the same suboject.  Note that if it is at the same
   BINFO_OFFSET and has the same TREE_TYPE, it must necessarily be the same
   subobject.  */

static void
lookup_subobject_matches (const_tree target_type, const dyncast_subobject match,
			  auto_vec<dyncast_subobject> &matches)
{
  if (types_same_for_odr (BINFO_TYPE (match.binfo), target_type))
    {
      /* Check if we had already found this particular subobject.  */
      tree match_offset = BINFO_OFFSET (match.binfo);
      for (auto &subobject : matches)
	if (tree_int_cst_equal (BINFO_OFFSET (subobject.binfo), match_offset))
	  {
	    subobject.accessible |= match.accessible;
	    return; /* We are adding the same subobject, so skip it.  */
	  }
      matches.safe_push (match);
      return;
    }
  tree parent_binfo = match.binfo;
  tree base_binfo;
  for (unsigned i = 0; BINFO_BASE_ITERATE (parent_binfo, i, base_binfo); i++)
    {
      dyncast_subobject child = match;
      child.binfo = base_binfo;
      /* If BINFO_BASE_ACCESSES is not present, public access is implied.  */
      child.accessible
	&= !BINFO_BASE_ACCESSES (parent_binfo)
	   || BINFO_BASE_ACCESS (parent_binfo, i) == access_public_node;
      /* Check if the next binfo might be our DST_TYPE binfo recursively.  */
      lookup_subobject_matches (target_type, child, matches);
    }
}

/* We implement the runtime check rules as per [expr.dynamic.cast]9.
   As a general overview, those rules state:
     [expr.dynamic.cast]/9.1: Does SRC_OBJ point to a public base subobject of
       a DST_TYPE object?  And is there only one DST_TYPE object derived from
       SRC_OBJ?
       We expect the hierarchy to be something like:
       SRC_TYPE -> ... -> DST_TYPE -> ... -> MD_TYPE.

     [expr.dynamic.cast]/9.2: Otherwise, does SRC_OBJ point to a public base
       subobject of a MDTYPE object?  And is DST_TYPE an unambiguous and public
       base of MDTYPE?
       We expect the hierarchy to be something like:
       MD_TYPE -> ... -> DST_TYPE

     [expr.dynamic.cast]/9.3: Otherwise, the runtime check fails.  */

static dyncast_subobject
evaluate_dyncast (tree dst_type, tree md_binfo, tree src_binfo)
{
  dyncast_subobject no_base_match;

  /* Start by assuming the path will be public.  */
  auto_vec<dyncast_subobject> dst_matches;
  dyncast_subobject md_subobject = {md_binfo, /* accessible = */ true};
  lookup_subobject_matches (dst_type, md_subobject, dst_matches);

  /* Per [expr.dynamic.cast]/9.1:
     Only one object of DST_TYPE can be derived from this SRC_OBJ and
     The path from DST -> SRC must be public.  */
  tree src_offset = BINFO_OFFSET (src_binfo);
  tree src_type = BINFO_TYPE (src_binfo);

  auto_vec<dyncast_subobject> clause1_matches;
  for (const auto &dst_subobj : dst_matches)
    {
      auto_vec<dyncast_subobject> src_matches;
      dyncast_subobject from_dst = {dst_subobj.binfo, /* accessible = */ true};
      lookup_subobject_matches (src_type, from_dst, src_matches);
      /* Only keep matches that derive from this src subobject.  */
      for (const auto &src_subobj : src_matches)
	if (tree_int_cst_equal (BINFO_OFFSET (src_subobj.binfo), src_offset))
	  /* Keep dst BINFO but save whether SRC is a public base of DST.  */
	  clause1_matches.safe_push ({dst_subobj.binfo, src_subobj.accessible});
    }
  if (clause1_matches.length () == 1 && clause1_matches[0].accessible)
    return clause1_matches[0]; /* No ambiguity, only one public match.  */

  /* No match or the match we found was private.  Try clause 2.  */

  /* Otherwise, per [expr.dynamic.cast]/9.2:
      Require a public path from MD_OBJ -> SRC_OBJ and
      Require that DST_TYPE is an unambiguous and public base of MD_TYPE.  */
  if (dst_matches.length () == 1 && dst_matches[0].accessible)
    {
      auto_vec<dyncast_subobject> src_matches;
      lookup_subobject_matches (src_type, md_subobject, src_matches);
      /* Find any public path from MD_OBJ -> SRC_OBJ.  */
      for (const auto &src_subobj : src_matches)
	if (src_subobj.accessible)
	  return dst_matches[0]; /* Found a public match.  */
    }
  return no_base_match; /* No match or the match we found was private.  */
}

class kf_dynamic_cast : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    /* A call will look something like:
       Derived *d;
       d = __dynamic_cast ((Base*) b, &_ZTI1Base, &_ZTI1Derived, 8);  */
    return (cd.num_args () == 4 && POINTER_TYPE_P (cd.get_arg_type (0))
	    && POINTER_TYPE_P (cd.get_arg_type (1))
	    && POINTER_TYPE_P (cd.get_arg_type (2))
	    && INTEGRAL_TYPE_P (cd.get_arg_type (3)));
  }
  void impl_call_post (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    region_model_manager *mgr = cd.get_manager ();

    cd.set_any_lhs_with_defaults ();

    tree dst_ptr_type = cd.get_lhs_type ();
    if (!dst_ptr_type)
      return;

    /* Recover the class types from the tinfo args.  */
    tree src_type = get_type_from_tinfo_arg (cd.get_arg_tree (1));
    tree dst_type = get_type_from_tinfo_arg (cd.get_arg_tree (2));
    if (!src_type || !dst_type)
      return;

    /* Read the vptr binding of the object; VPTR_OFF selects the
       sub-vtable within the vtable decl, so it identifies which subobject's
       vptr we read.  */
    tree src_obj = cd.get_arg_tree (0);
    unsigned HOST_WIDE_INT vptr_off;
    tree vtable
      = model->get_vtable_from_obj (src_obj, src_type, mgr, nullptr, &vptr_off);
    /* The class the vtable belongs to is the dynamic (most-derived) type of
       the object.  VTABLE is whatever decl the vptr slot happened to point at,
       so check it really is a vtable.  */
    if (!vtable || !VAR_P (vtable) || !DECL_VIRTUAL_P (vtable))
      return;
    tree mdtype = DECL_CONTEXT (vtable);
    if (!mdtype || !RECORD_OR_UNION_TYPE_P (mdtype))
      return;
    tree md_binfo = TYPE_BINFO (mdtype);
    if (!md_binfo)
      return;

    /* Given the value stored to SRC_OBJ's vtpr field (&_ZTV* + offset), find
       which subobject of this hierarchy would have this value written into its
       vptr.  */
    tree vtable_binfo
      = subbinfo_with_vtable_at_offset (md_binfo, vptr_off, vtable);
    if (!vtable_binfo)
      return;
    /* With a shared primary-base vtable the owning binfo may be an enclosing
       type.  Consider:
	 class A {};
	 class B {};
	 class C : B {};
	 class D : A, C {};
       Here the vptr value stored in the B-subobject's slot is owned by the C
       binfo (C's sub-vtable group), and a lookup with B's vptr value returns
       the C binfo, not B's (BINFO_VTABLE is only set on the owner, cf.
       ipa-devirt.cc:61).  In this case, the src subobject sits on its primary
       chain (relative offset 0), which lookup_binfo_at_same_offset finds.  */
    tree src_binfo = lookup_binfo_at_same_offset (vtable_binfo, src_type);
    if (!src_binfo)
      return;

    dyncast_subobject dst_match
      = evaluate_dyncast (dst_type, md_binfo, src_binfo);

    if (!dst_match.binfo)
      { /* [expr.dynamic.cast]/9.3: Otherwise, the runtime check failed.  */
	cd.maybe_set_lhs (mgr->get_or_create_null_ptr (dst_ptr_type));
	return;
      }

    /* Build a pointer to the dst subobject.  Work in byte offsets relative to
       SRC_REG's base region; we never need a region for the mdtype object
       itself, only its start offset, recovered from where the src subobject
       sits within MDTYPE.  */
    const region *src_reg = cd.deref_ptr_arg (0);
    region_offset off = src_reg->get_offset (mgr);
    if (!off.concrete_p ())
      return; /* Bail, leave lhs conjured.  */
    byte_offset_t src_obj_start;
    if (!off.get_concrete_byte_offset (&src_obj_start))
      return;

    HOST_WIDE_INT src_off_in_md = tree_to_shwi (BINFO_OFFSET (src_binfo));
    HOST_WIDE_INT dst_off_in_md = tree_to_shwi (BINFO_OFFSET (dst_match.binfo));
    HOST_WIDE_INT md_start_in_base = src_obj_start.to_shwi () - src_off_in_md;
    if (md_start_in_base < 0)
      return; /* Layout disagreement between the store and the binfo data;
		 bail rather than build a negative-offset region.  */
    HOST_WIDE_INT dst_off_in_base = md_start_in_base + dst_off_in_md;

    /* BASE_REG is the outermost region, not necessarily the mdtype
       object (it might sit at a nonzero offset inside BASE_REG, e.g. as an
       array element or a member subobject).  The store binds values by byte
       ranges within a base region, so a concrete offset_region aliases the
       FE's field-path accesses to the same bytes.  */
    const region *base_reg = off.get_base_region ();
    const svalue *dst_off_sval
      = mgr->get_or_create_int_cst (size_type_node, dst_off_in_base);
    const region *dst_reg
      = mgr->get_offset_region (base_reg, dst_type, dst_off_sval);
    cd.maybe_set_lhs (mgr->get_ptr_svalue (dst_ptr_type, dst_reg));
  }
};

/* Handler for "operator new" and "operator new []".  */

class kf_operator_new : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1
      && cd.arg_is_size_p (0))
      || (cd.num_args () == 2
      && cd.arg_is_size_p (0)
      && POINTER_TYPE_P (cd.get_arg_type (1)));
  }

  void
  check_any_preconditions (const call_details &cd) const final override
  {
    region_model_context *ctxt = cd.get_ctxt ();
    if (!ctxt)
      return;
    region_model *model = cd.get_model ();
    const gcall &call = cd.get_call_stmt ();

    /* If the call was actually a placement new, check that accessing
       the buffer lhs is placed into does not result in out-of-bounds.  */
    if (is_placement_new_p (call))
      {
	if (const region *sized_reg = get_sized_region_for_placement_new (cd))
	  model->check_region_for_write (sized_reg,
					 nullptr,
					 ctxt);
      }
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    region_model_manager *mgr = cd.get_manager ();
    const svalue *size_sval = cd.get_arg_svalue (0);
    region_model_context *ctxt = cd.get_ctxt ();
    const gcall &call = cd.get_call_stmt ();

    if (is_placement_new_p (call))
      {
	const region *ptr_reg = cd.deref_ptr_arg (1);
	if (ptr_reg && cd.get_lhs_type ())
	  if (const region *sized_reg = get_sized_region_for_placement_new (cd))
	    {
	      const svalue *ptr_sval
		= mgr->get_ptr_svalue (cd.get_lhs_type (), sized_reg);
	      cd.maybe_set_lhs (ptr_sval);
	    }
      }
    /* If the call is an allocating new, then create a heap allocated
       region.  */
    else
      {
	const region *new_reg
	  = model->get_or_create_region_for_heap_alloc (size_sval, ctxt);
	if (cd.get_lhs_type ())
	  {
	    const svalue *ptr_sval
	      = mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
	    cd.maybe_set_lhs (ptr_sval);
	  }
      }
  }

  void impl_call_post (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    region_model_manager *mgr = cd.get_manager ();
    tree callee_fndecl = cd.get_fndecl_for_call ();
    region_model_context *ctxt = cd.get_ctxt ();

    /* If the call is guaranteed to return nonnull
       then add a nonnull constraint to the allocated region.  */
    if (!TREE_NOTHROW (callee_fndecl)
	&& flag_exceptions
	&& cd.get_lhs_type ())
      {
	const svalue *null_sval
	  = mgr->get_or_create_null_ptr (cd.get_lhs_type ());
	const svalue *result
	  = model->get_store_value (cd.get_lhs_region (), ctxt);
	model->add_constraint (result, NE_EXPR, null_sval, ctxt);
      }
  }

private:
  const region *
  get_sized_region_for_placement_new (const call_details &cd) const
  {
    const region *ptr_reg = cd.deref_ptr_arg (1);
    if (ptr_reg && cd.get_lhs_type ())
      {
	region_model_manager *mgr = cd.get_manager ();
	const svalue *num_bytes_sval = cd.get_arg_svalue (0);
	return mgr->get_sized_region (ptr_reg,
				      cd.get_lhs_type (),
				      num_bytes_sval);
      }
    return nullptr;
  }
};

/* Handler for "operator delete" and for "operator delete []",
   both the sized and unsized variants
   (2 arguments and 1 argument respectively).  */

class kf_operator_delete : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 1 or cd.num_args () == 2;
  }

  void impl_call_post (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    const svalue *ptr_sval = cd.get_arg_svalue (0);
    if (const region *freed_reg = ptr_sval->maybe_get_region ())
      {
	/* If the ptr points to an underlying heap region, delete it,
	   poisoning pointers.  */
	model->unbind_region_and_descendents (freed_reg,
					      poison_kind::deleted);
      }
  }

};

class kf_cxa_allocate_exception : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 1 && cd.arg_is_size_p (0);
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    region_model_manager *mgr = cd.get_manager ();
    const svalue *size_sval = cd.get_arg_svalue (0);
    region_model_context *ctxt = cd.get_ctxt ();

    /* Create a heap allocated region.  */
    const region *new_reg
      = model->get_or_create_region_for_heap_alloc (size_sval, ctxt);
    if (cd.get_lhs_type ())
      {
	const svalue *ptr_sval
	  = mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
	cd.maybe_set_lhs (ptr_sval);
      }
  }
};

class kf_cxa_begin_catch : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1
	    && POINTER_TYPE_P (cd.get_arg_type (0)));
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();

    auto node = model->pop_thrown_exception ();
    model->push_caught_exception (node);
    cd.maybe_set_lhs (node.m_exception_sval);
  }
};

class kf_cxa_end_catch : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 0;
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    model->pop_caught_exception ();
  }
};

/* A subclass of pending_diagnostic for complaining about an exception
   of an unexpected type being thrown (due to a call to
   __cxa_call_unexpected).
   See https://en.cppreference.com/w/cpp/language/except_spec  */

class throw_of_unexpected_type
: public pending_diagnostic_subclass<throw_of_unexpected_type>
{
public:
  throw_of_unexpected_type (tree exception_type,
			    tree thrown_from_fndecl)
  : m_exception_type (exception_type),
    m_thrown_from_fndecl (thrown_from_fndecl)
  {
    gcc_assert (m_exception_type);
    gcc_assert (m_thrown_from_fndecl);
  }

  const char *get_kind () const final override
  {
    return "throw_of_unexpected_type";
  }

  bool operator== (const throw_of_unexpected_type &other) const
  {
    return (m_exception_type == other.m_exception_type
	    && m_thrown_from_fndecl == other.m_thrown_from_fndecl);
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_throw_of_unexpected_type;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    auto_diagnostic_group d;

    bool warned
      = ctxt.warn ("throwing exception of unexpected type %qT from %qE",
		   m_exception_type, m_thrown_from_fndecl);
    if (warned)
      {
	inform (DECL_SOURCE_LOCATION (m_thrown_from_fndecl),
		"%qE declared here", m_thrown_from_fndecl);
	// TODO: show specified types?
      }
    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    pp_printf  (&pp,
		"exception of unexpected type %qT thrown from %qE",
		m_exception_type, m_thrown_from_fndecl);
    return true;
  }

private:
  tree m_exception_type;
  tree m_thrown_from_fndecl;
};

/* See https://en.cppreference.com/w/cpp/language/except_spec  */

class kf_cxa_call_unexpected : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1
	    && POINTER_TYPE_P (cd.get_arg_type (0)));
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    if (region_model_context *ctxt = cd.get_ctxt ())
      {
	region_model *model = cd.get_model ();
	tree thrown_from_fndecl = model->get_current_function ()->decl;
	/* We must have a thrown exception.  */
	auto eh_node = model->get_current_thrown_exception ();
	gcc_assert (eh_node);
	tree exception_type = eh_node->maybe_get_type ();
	ctxt->warn
	  (std::make_unique<throw_of_unexpected_type> (exception_type,
						       thrown_from_fndecl));
	ctxt->terminate_path ();
      }
  }
};

/* Populate KFM with instances of known functions relating to C++.  */

void
register_known_functions_lang_cp (known_function_manager &kfm)
{
  kfm.add ("operator new", std::make_unique<kf_operator_new> ());
  kfm.add ("operator new []", std::make_unique<kf_operator_new> ());
  kfm.add ("operator delete", std::make_unique<kf_operator_delete> ());
  kfm.add ("operator delete []", std::make_unique<kf_operator_delete> ());

  /* Functions mentioned in "Itanium C++ ABI: Exception Handling"'s
     "Level II: C++ ABI"
     https://itanium-cxx-abi.github.io/cxx-abi/abi-eh.html#cxx-abi  */
  kfm.add ("__cxa_allocate_exception",
	   std::make_unique<kf_cxa_allocate_exception> ());
  // We treat __cxa_throw and __cxa_rethrow as special cases
  kfm.add ("__cxa_begin_catch", std::make_unique<kf_cxa_begin_catch> ());
  kfm.add ("__cxa_end_catch", std::make_unique<kf_cxa_end_catch> ());
  kfm.add ("__cxa_call_unexpected",
	   std::make_unique<kf_cxa_call_unexpected> ());

  /* Itanium C++ ABI's "The dynamic_cast Algorithm"
     https://itanium-cxx-abi.github.io/cxx-abi/abi.html#dynamic_cast-algorithm
   */
  kfm.add ("__dynamic_cast", std::make_unique<kf_dynamic_cast> ());
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
