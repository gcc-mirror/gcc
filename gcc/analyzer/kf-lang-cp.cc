/* Handling for the known behavior of various functions specific to C++.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#include "config.h"
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "diagnostic.h"
#include "analyzer/region-model.h"
#include "analyzer/call-details.h"
#include "make-unique.h"

#if ENABLE_ANALYZER

/* Return true if CALL is a non-allocating operator new or operator new []
   that contains no user-defined args, i.e. having any signature of:

    - void* operator new (std::size_t count, void* ptr);
    - void* operator new[] (std::size_t count, void* ptr);

   See https://en.cppreference.com/w/cpp/memory/new/operator_new.  */

bool is_placement_new_p (const gcall *call)
{
  gcc_assert (call);
  tree fndecl = gimple_call_fndecl (call);

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
    const gcall *call = cd.get_call_stmt ();

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
					      POISON_KIND_DELETED);
      }
  }

};

/* Populate KFM with instances of known functions relating to C++.  */

void
register_known_functions_lang_cp (known_function_manager &kfm)
{
  kfm.add ("operator new", make_unique<kf_operator_new> ());
  kfm.add ("operator new []", make_unique<kf_operator_new> ());
  kfm.add ("operator delete", make_unique<kf_operator_delete> ());
  kfm.add ("operator delete []", make_unique<kf_operator_delete> ());
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
