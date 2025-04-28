/* Handling for the known behavior of various functions specific to C++.
   Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

  void impl_call_pre (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    region_model_manager *mgr = cd.get_manager ();
    const svalue *size_sval = cd.get_arg_svalue (0);
    region_model_context *ctxt = cd.get_ctxt ();
    const gcall &call = cd.get_call_stmt ();

    /* If the call was actually a placement new, check that accessing
       the buffer lhs is placed into does not result in out-of-bounds.  */
    if (is_placement_new_p (call))
      {
	const region *ptr_reg = cd.deref_ptr_arg (1);
	if (ptr_reg && cd.get_lhs_type ())
	  {
	    const svalue *num_bytes_sval = cd.get_arg_svalue (0);
	    const region *sized_new_reg
		= mgr->get_sized_region (ptr_reg,
					 cd.get_lhs_type (),
					 num_bytes_sval);
	    model->check_region_for_write (sized_new_reg,
					   nullptr,
					   ctxt);
	    const svalue *ptr_sval
	      = mgr->get_ptr_svalue (cd.get_lhs_type (), sized_new_reg);
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
    if (!TREE_NOTHROW (callee_fndecl) && flag_exceptions)
      {
	const svalue *null_sval
	  = mgr->get_or_create_null_ptr (cd.get_lhs_type ());
	const svalue *result
	  = model->get_store_value (cd.get_lhs_region (), ctxt);
	model->add_constraint (result, NE_EXPR, null_sval, ctxt);
      }
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
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
