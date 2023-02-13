/* Handling for the known behavior of various functions specific to C++.
   Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

namespace ana {

/* Implementations of specific functions.  */

/* Handler for "operator new" and "operator new []".  */

class kf_operator_new : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 1;
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    region_model_manager *mgr = cd.get_manager ();
    const svalue *size_sval = cd.get_arg_svalue (0);
    const region *new_reg
      = model->get_or_create_region_for_heap_alloc (size_sval, cd.get_ctxt ());
    if (cd.get_lhs_type ())
      {
	const svalue *ptr_sval
	  = mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
	cd.maybe_set_lhs (ptr_sval);
      }
  }
};

/* Handler for "operator delete", both the sized and unsized variants
   (2 arguments and 1 argument respectively), and for "operator delete []"  */

class kf_operator_delete : public known_function
{
public:
  kf_operator_delete (unsigned num_args) : m_num_args (num_args) {}

  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == m_num_args;
  }

  void impl_call_post (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    const svalue *ptr_sval = cd.get_arg_svalue (0);
    if (const region *freed_reg = ptr_sval->maybe_get_region ())
      {
	/* If the ptr points to an underlying heap region, delete it,
	   poisoning pointers.  */
	model->unbind_region_and_descendents (freed_reg, POISON_KIND_FREED);
      }
  }

private:
  unsigned m_num_args;
};

/* Populate KFM with instances of known functions relating to C++.  */

void
register_known_functions_lang_cp (known_function_manager &kfm)
{
  kfm.add ("operator new", make_unique<kf_operator_new> ());
  kfm.add ("operator new []", make_unique<kf_operator_new> ());
  kfm.add ("operator delete", make_unique<kf_operator_delete> (1));
  kfm.add ("operator delete", make_unique<kf_operator_delete> (2));
  kfm.add ("operator delete []", make_unique<kf_operator_delete> (1));
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
