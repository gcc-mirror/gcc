/* Handling for the known behavior of various specific functions.
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

#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "diagnostic-core.h"
#include "diagnostic-metadata.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "diagnostic.h"
#include "analyzer/region-model.h"
#include "analyzer/call-details.h"
#include "analyzer/call-info.h"
#include "make-unique.h"

#if ENABLE_ANALYZER

namespace ana {

/* Abstract subclass for describing undefined behavior of an API.  */

class undefined_function_behavior
  : public pending_diagnostic_subclass<undefined_function_behavior>
{
public:
  undefined_function_behavior (const call_details &cd)
  : m_call_stmt (cd.get_call_stmt ()),
    m_callee_fndecl (cd.get_fndecl_for_call ())
  {
    gcc_assert (m_call_stmt);
    gcc_assert (m_callee_fndecl);
  }

  const char *get_kind () const final override
  {
    return "undefined_behavior";
  }

  bool operator== (const undefined_function_behavior &other) const
  {
    return (m_call_stmt == other.m_call_stmt
	    && m_callee_fndecl == other.m_callee_fndecl);
  }

  bool terminate_path_p () const final override { return true; }

  tree get_callee_fndecl () const { return m_callee_fndecl; }

private:
  const gimple *m_call_stmt;
  tree m_callee_fndecl;
};

/* class pure_known_function_with_default_return : public known_function.  */

void
pure_known_function_with_default_return::
impl_call_pre (const call_details &cd) const
{
  cd.set_any_lhs_with_defaults ();
}

/* Implementations of specific functions.  */

/* Handler for "alloca".  */

class kf_alloca : public builtin_known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 1;
  }
  enum built_in_function builtin_code () const final override
  {
    return BUILT_IN_ALLOCA;
  }
  void impl_call_pre (const call_details &cd) const final override;
};

void
kf_alloca::impl_call_pre (const call_details &cd) const
{
  const svalue *size_sval = cd.get_arg_svalue (0);

  region_model *model = cd.get_model ();
  region_model_manager *mgr = cd.get_manager ();

  const region *new_reg
    = model->create_region_for_alloca (size_sval, cd.get_ctxt ());
  const svalue *ptr_sval
    = mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
  cd.maybe_set_lhs (ptr_sval);
}

/* Handler for __atomic_exchange.
   Although the user-facing documentation specifies it as having this
   signature:
     void __atomic_exchange (type *ptr, type *val, type *ret, int memorder)

   by the time the C/C++ frontends have acted on it, any calls that
   can't be mapped to a _N variation end up with this signature:

     void
     __atomic_exchange (size_t sz, void *ptr, void *val, void *ret,
			int memorder)

   as seen in the gimple seen by the analyzer, and as specified
   in sync-builtins.def.  */

class kf_atomic_exchange : public internal_known_function
{
public:
  /* This is effectively:
       tmpA = *PTR;
       tmpB = *VAL;
       *PTR = tmpB;
       *RET = tmpA;
  */
  void impl_call_pre (const call_details &cd) const final override
  {
    const svalue *num_bytes_sval = cd.get_arg_svalue (0);
    const svalue *ptr_sval = cd.get_arg_svalue (1);
    tree ptr_tree = cd.get_arg_tree (1);
    const svalue *val_sval = cd.get_arg_svalue (2);
    tree val_tree = cd.get_arg_tree (2);
    const svalue *ret_sval = cd.get_arg_svalue (3);
    tree ret_tree = cd.get_arg_tree (3);
    /* Ignore the memorder param.  */

    region_model *model = cd.get_model ();
    region_model_context *ctxt = cd.get_ctxt ();

    const region *ptr_reg = model->deref_rvalue (ptr_sval, ptr_tree, ctxt);
    const region *val_reg = model->deref_rvalue (val_sval, val_tree, ctxt);
    const region *ret_reg = model->deref_rvalue (ret_sval, ret_tree, ctxt);

    const svalue *tmp_a_sval
      = model->read_bytes (ptr_reg, ptr_tree, num_bytes_sval, ctxt);
    const svalue *tmp_b_sval
      = model->read_bytes (val_reg, val_tree, num_bytes_sval, ctxt);
    model->write_bytes (ptr_reg, num_bytes_sval, tmp_b_sval, ctxt);
    model->write_bytes (ret_reg, num_bytes_sval, tmp_a_sval, ctxt);
  }
};

/* Handler for:
   __atomic_exchange_n (type *ptr, type val, int memorder).  */

class kf_atomic_exchange_n : public internal_known_function
{
public:
  /* This is effectively:
       RET = *PTR;
       *PTR = VAL;
       return RET;
  */
  void impl_call_pre (const call_details &cd) const final override
  {
    const svalue *ptr_sval = cd.get_arg_svalue (0);
    tree ptr_tree = cd.get_arg_tree (0);
    const svalue *set_sval = cd.get_arg_svalue (1);
    /* Ignore the memorder param.  */

    region_model *model = cd.get_model ();
    region_model_context *ctxt = cd.get_ctxt ();

    const region *dst_region = model->deref_rvalue (ptr_sval, ptr_tree, ctxt);
    const svalue *ret_sval = model->get_store_value (dst_region, ctxt);
    model->set_value (dst_region, set_sval, ctxt);
    cd.maybe_set_lhs (ret_sval);
  }
};

/* Handler for:
   type __atomic_fetch_add (type *ptr, type val, int memorder);
   type __atomic_fetch_sub (type *ptr, type val, int memorder);
   type __atomic_fetch_and (type *ptr, type val, int memorder);
   type __atomic_fetch_xor (type *ptr, type val, int memorder);
   type __atomic_fetch_or (type *ptr, type val, int memorder);
*/

class kf_atomic_fetch_op : public internal_known_function
{
public:
  kf_atomic_fetch_op (enum tree_code op): m_op (op) {}

  /* This is effectively:
       RET = *PTR;
       *PTR = RET OP VAL;
       return RET;
  */
  void impl_call_pre (const call_details &cd) const final override
  {
    const svalue *ptr_sval = cd.get_arg_svalue (0);
    tree ptr_tree = cd.get_arg_tree (0);
    const svalue *val_sval = cd.get_arg_svalue (1);
    /* Ignore the memorder param.  */

    region_model *model = cd.get_model ();
    region_model_manager *mgr = cd.get_manager ();
    region_model_context *ctxt = cd.get_ctxt ();

    const region *star_ptr_region
      = model->deref_rvalue (ptr_sval, ptr_tree, ctxt);
    const svalue *old_sval = model->get_store_value (star_ptr_region, ctxt);
    const svalue *new_sval = mgr->get_or_create_binop (old_sval->get_type (),
						       m_op,
						       old_sval, val_sval);
    model->set_value (star_ptr_region, new_sval, ctxt);
    cd.maybe_set_lhs (old_sval);
  }

private:
  enum tree_code m_op;
};

/* Handler for:
   type __atomic_add_fetch (type *ptr, type val, int memorder);
   type __atomic_sub_fetch (type *ptr, type val, int memorder);
   type __atomic_and_fetch (type *ptr, type val, int memorder);
   type __atomic_xor_fetch (type *ptr, type val, int memorder);
   type __atomic_or_fetch (type *ptr, type val, int memorder);
*/

class kf_atomic_op_fetch : public internal_known_function
{
public:
  kf_atomic_op_fetch (enum tree_code op): m_op (op) {}

  /* This is effectively:
       *PTR = RET OP VAL;
       return *PTR;
  */
  void impl_call_pre (const call_details &cd) const final override
  {
    const svalue *ptr_sval = cd.get_arg_svalue (0);
    tree ptr_tree = cd.get_arg_tree (0);
    const svalue *val_sval = cd.get_arg_svalue (1);
    /* Ignore the memorder param.  */

    region_model *model = cd.get_model ();
    region_model_manager *mgr = cd.get_manager ();
    region_model_context *ctxt = cd.get_ctxt ();

    const region *star_ptr_region
      = model->deref_rvalue (ptr_sval, ptr_tree, ctxt);
    const svalue *old_sval = model->get_store_value (star_ptr_region, ctxt);
    const svalue *new_sval = mgr->get_or_create_binop (old_sval->get_type (),
						       m_op,
						       old_sval, val_sval);
    model->set_value (star_ptr_region, new_sval, ctxt);
    cd.maybe_set_lhs (new_sval);
  }

private:
  enum tree_code m_op;
};

/* Handler for __atomic_load.
   Although the user-facing documentation specifies it as having this
   signature:

      void __atomic_load (type *ptr, type *ret, int memorder)

   by the time the C/C++ frontends have acted on it, any calls that
   can't be mapped to a _N variation end up with this signature:

      void __atomic_load (size_t sz, const void *src, void *dst, int memorder);

   as seen in the gimple seen by the analyzer, and as specified
   in sync-builtins.def.  */

class kf_atomic_load : public internal_known_function
{
public:
  /* This is effectively:
       memmove (dst, src, sz);
  */
  void impl_call_pre (const call_details &cd) const final override
  {
    const svalue *num_bytes_sval = cd.get_arg_svalue (0);
    const svalue *src_sval = cd.get_arg_svalue (1);
    tree src_tree = cd.get_arg_tree (1);
    const svalue *dst_sval = cd.get_arg_svalue (2);
    tree dst_tree = cd.get_arg_tree (2);
    /* Ignore the memorder param.  */

    region_model *model = cd.get_model ();
    region_model_context *ctxt = cd.get_ctxt ();

    const region *dst_reg = model->deref_rvalue (dst_sval, dst_tree, ctxt);
    const region *src_reg = model->deref_rvalue (src_sval, src_tree, ctxt);

    const svalue *data_sval
      = model->read_bytes (src_reg, src_tree, num_bytes_sval, ctxt);
    model->write_bytes (dst_reg, num_bytes_sval, data_sval, ctxt);
  }
};

/* Handler for __atomic_store.
   Although the user-facing documentation specifies it as having this
   signature:

      void __atomic_store (type *ptr, type *val, int memorder)

   by the time the C/C++ frontends have acted on it, any calls that
   can't be mapped to a _N variation end up with this signature:

      void __atomic_store (size_t sz, type *dst, type *src, int memorder)

   as seen in the gimple seen by the analyzer, and as specified
   in sync-builtins.def.  */

class kf_atomic_store : public internal_known_function
{
public:
  /* This is effectively:
       memmove (dst, src, sz);
  */
  void impl_call_pre (const call_details &cd) const final override
  {
    const svalue *num_bytes_sval = cd.get_arg_svalue (0);
    const svalue *dst_sval = cd.get_arg_svalue (1);
    tree dst_tree = cd.get_arg_tree (1);
    const svalue *src_sval = cd.get_arg_svalue (2);
    tree src_tree = cd.get_arg_tree (2);
    /* Ignore the memorder param.  */

    region_model *model = cd.get_model ();
    region_model_context *ctxt = cd.get_ctxt ();

    const region *dst_reg = model->deref_rvalue (dst_sval, dst_tree, ctxt);
    const region *src_reg = model->deref_rvalue (src_sval, src_tree, ctxt);

    const svalue *data_sval
      = model->read_bytes (src_reg, src_tree, num_bytes_sval, ctxt);
    model->write_bytes (dst_reg, num_bytes_sval, data_sval, ctxt);
  }
};

/* Handler for:
   type __atomic_load_n (type *ptr, int memorder) */

class kf_atomic_load_n : public internal_known_function
{
public:
  /* This is effectively:
       RET = *PTR;
       return RET;
  */
  void impl_call_pre (const call_details &cd) const final override
  {
    const svalue *ptr_ptr_sval = cd.get_arg_svalue (0);
    tree ptr_ptr_tree = cd.get_arg_tree (0);
    /* Ignore the memorder param.  */

    region_model *model = cd.get_model ();
    region_model_context *ctxt = cd.get_ctxt ();

    const region *ptr_region
      = model->deref_rvalue (ptr_ptr_sval, ptr_ptr_tree, ctxt);
    const svalue *star_ptr_sval = model->get_store_value (ptr_region, ctxt);
    cd.maybe_set_lhs (star_ptr_sval);
  }
};

/* Handler for:
   void __atomic_store_n (type *ptr, type val, int memorder) */

class kf_atomic_store_n : public internal_known_function
{
public:
  /* This is effectively:
       *PTR = VAL;
  */
  void impl_call_pre (const call_details &cd) const final override
  {
    const svalue *ptr_sval = cd.get_arg_svalue (0);
    tree ptr_tree = cd.get_arg_tree (0);
    const svalue *new_sval = cd.get_arg_svalue (1);
    /* Ignore the memorder param.  */

    region_model *model = cd.get_model ();
    region_model_context *ctxt = cd.get_ctxt ();

    const region *star_ptr_region
      = model->deref_rvalue (ptr_sval, ptr_tree, ctxt);
    model->set_value (star_ptr_region, new_sval, ctxt);
  }
};

/* Handler for "__builtin_expect" etc.  */

class kf_expect : public internal_known_function
{
public:
  void impl_call_pre (const call_details &cd) const final override
  {
    /* __builtin_expect's return value is its initial argument.  */
    const svalue *sval = cd.get_arg_svalue (0);
    cd.maybe_set_lhs (sval);
  }
};

/* Handler for "calloc".  */

class kf_calloc : public builtin_known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 2
	    && cd.arg_is_size_p (0)
	    && cd.arg_is_size_p (1));
  }
  enum built_in_function builtin_code () const final override
  {
    return BUILT_IN_CALLOC;
  }

  void impl_call_pre (const call_details &cd) const final override;
};

void
kf_calloc::impl_call_pre (const call_details &cd) const
{
  region_model *model = cd.get_model ();
  region_model_manager *mgr = cd.get_manager ();
  const svalue *nmemb_sval = cd.get_arg_svalue (0);
  const svalue *size_sval = cd.get_arg_svalue (1);
  /* TODO: check for overflow here?  */
  const svalue *prod_sval
    = mgr->get_or_create_binop (size_type_node, MULT_EXPR,
				nmemb_sval, size_sval);
  const region *new_reg
    = model->get_or_create_region_for_heap_alloc (prod_sval, cd.get_ctxt ());
  const region *sized_reg
    = mgr->get_sized_region (new_reg, NULL_TREE, prod_sval);
  model->zero_fill_region (sized_reg, cd.get_ctxt ());
  if (cd.get_lhs_type ())
    {
      const svalue *ptr_sval
	= mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
      cd.maybe_set_lhs (ptr_sval);
    }
}

/* Handler for glibc's "__errno_location".  */

class kf_errno_location : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 0;
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    if (cd.get_lhs_region ())
      {
	region_model_manager *mgr = cd.get_manager ();
	const region *errno_reg = mgr->get_errno_region ();
	const svalue *errno_ptr = mgr->get_ptr_svalue (cd.get_lhs_type (),
						       errno_reg);
	cd.maybe_set_lhs (errno_ptr);
      }
  }
};

/* Handler for "error" and "error_at_line" from GNU's non-standard <error.h>.
   MIN_ARGS identifies the minimum number of expected arguments
   to be consistent with such a call (3 and 5 respectively).  */

class kf_error : public known_function
{
public:
  kf_error (unsigned min_args) : m_min_args (min_args) {}

  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () >= m_min_args
	    && cd.get_arg_type (0) == integer_type_node);
  }

  void impl_call_pre (const call_details &cd) const final override;

private:
  unsigned m_min_args;
};

void
kf_error::impl_call_pre (const call_details &cd) const
{
  /* The process exits if status != 0, so it only continues
     for the case where status == 0.
     Add that constraint, or terminate this analysis path.  */
  tree status = cd.get_arg_tree (0);
  region_model_context *ctxt = cd.get_ctxt ();
  region_model *model = cd.get_model ();
  if (!model->add_constraint (status, EQ_EXPR, integer_zero_node, ctxt))
    if (ctxt)
      ctxt->terminate_path ();

  /* Check "format" arg.  */
  const int fmt_arg_idx = (m_min_args == 3) ? 2 : 4;
  model->check_for_null_terminated_string_arg (cd, fmt_arg_idx);
}

/* Handler for fopen.
     FILE *fopen (const char *filename, const char *mode);
   See e.g. https://en.cppreference.com/w/c/io/fopen
   https://www.man7.org/linux/man-pages/man3/fopen.3.html
   https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/fopen-wfopen?view=msvc-170  */

class kf_fopen : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 2
	    && cd.arg_is_pointer_p (0)
	    && cd.arg_is_pointer_p (1));
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    cd.check_for_null_terminated_string_arg (0);
    cd.check_for_null_terminated_string_arg (1);
    cd.set_any_lhs_with_defaults ();

    /* fopen's mode param is effectively a mini-DSL, but there are various
       non-standard extensions, so we don't bother to check it.  */
  }
};

/* Handler for "free", after sm-handling.

   If the ptr points to an underlying heap region, delete the region,
   poisoning pointers to it and regions within it.

   We delay this until after sm-state has been updated so that the
   sm-handling can transition all of the various casts of the pointer
   to a "freed" state *before* we delete the related region here.

   This has to be done here so that the sm-handling can use the fact
   that they point to the same region to establish that they are equal
   (in region_model::eval_condition), and thus transition
   all pointers to the region to the "freed" state together, regardless
   of casts.  */

class kf_free : public builtin_known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1 && cd.arg_is_pointer_p (0));
  }
  enum built_in_function builtin_code () const final override
  {
    return BUILT_IN_FREE;
  }
  void impl_call_post (const call_details &cd) const final override;
};

void
kf_free::impl_call_post (const call_details &cd) const
{
  const svalue *ptr_sval = cd.get_arg_svalue (0);
  if (const region *freed_reg = ptr_sval->maybe_get_region ())
    {
      /* If the ptr points to an underlying heap region, delete it,
	 poisoning pointers.  */
      region_model *model = cd.get_model ();
      model->unbind_region_and_descendents (freed_reg, POISON_KIND_FREED);
      model->unset_dynamic_extents (freed_reg);
    }
}

/* Handle the on_call_pre part of "malloc".  */

class kf_malloc : public builtin_known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1
	    && cd.arg_is_size_p (0));
  }
  enum built_in_function builtin_code () const final override
  {
    return BUILT_IN_MALLOC;
  }
  void impl_call_pre (const call_details &cd) const final override;
};

void
kf_malloc::impl_call_pre (const call_details &cd) const
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

/* Handler for "memcpy" and "__builtin_memcpy",
   "memmove", and "__builtin_memmove".  */

class kf_memcpy_memmove : public builtin_known_function
{
public:
  enum kf_memcpy_memmove_variant
  {
    KF_MEMCPY,
    KF_MEMCPY_CHK,
    KF_MEMMOVE,
    KF_MEMMOVE_CHK,
  };
  kf_memcpy_memmove (enum kf_memcpy_memmove_variant variant)
    : m_variant (variant) {};
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 3
	    && cd.arg_is_pointer_p (0)
	    && cd.arg_is_pointer_p (1)
	    && cd.arg_is_size_p (2));
  }
  enum built_in_function builtin_code () const final override
  {
    switch (m_variant)
      {
      case KF_MEMCPY:
	return BUILT_IN_MEMCPY;
      case KF_MEMCPY_CHK:
	return BUILT_IN_MEMCPY_CHK;
      case KF_MEMMOVE:
	return BUILT_IN_MEMMOVE;
      case KF_MEMMOVE_CHK:
	return BUILT_IN_MEMMOVE_CHK;
      default:
	gcc_unreachable ();
      }
  }
  void impl_call_pre (const call_details &cd) const final override;
private:
  const enum kf_memcpy_memmove_variant m_variant;
};

void
kf_memcpy_memmove::impl_call_pre (const call_details &cd) const
{
  const svalue *dest_ptr_sval = cd.get_arg_svalue (0);
  const svalue *src_ptr_sval = cd.get_arg_svalue (1);
  const svalue *num_bytes_sval = cd.get_arg_svalue (2);

  region_model *model = cd.get_model ();

  const region *dest_reg
    = model->deref_rvalue (dest_ptr_sval, cd.get_arg_tree (0), cd.get_ctxt ());
  const region *src_reg
    = model->deref_rvalue (src_ptr_sval, cd.get_arg_tree (1), cd.get_ctxt ());

  cd.maybe_set_lhs (dest_ptr_sval);
  /* Check for overlap.  */
  switch (m_variant)
    {
    case KF_MEMCPY:
    case KF_MEMCPY_CHK:
      cd.complain_about_overlap (0, 1, num_bytes_sval);
      break;

    case KF_MEMMOVE:
    case KF_MEMMOVE_CHK:
      /* It's OK for memmove's arguments to overlap.  */
      break;

    default:
	gcc_unreachable ();
    }
  model->copy_bytes (dest_reg,
		     src_reg, cd.get_arg_tree (1),
		     num_bytes_sval,
		     cd.get_ctxt ());
}

/* Handler for "memset" and "__builtin_memset".  */

class kf_memset : public builtin_known_function
{
public:
  kf_memset (bool chk_variant) : m_chk_variant (chk_variant) {}
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 3 && cd.arg_is_pointer_p (0));
  }
  enum built_in_function builtin_code () const final override
  {
    return m_chk_variant ? BUILT_IN_MEMSET_CHK : BUILT_IN_MEMSET;
  }
  void impl_call_pre (const call_details &cd) const final override;
private:
  const bool m_chk_variant;
};

void
kf_memset::impl_call_pre (const call_details &cd) const
{
  const svalue *dest_sval = cd.get_arg_svalue (0);
  const svalue *fill_value_sval = cd.get_arg_svalue (1);
  const svalue *num_bytes_sval = cd.get_arg_svalue (2);

  region_model *model = cd.get_model ();
  region_model_manager *mgr = cd.get_manager ();

  const region *dest_reg
    = model->deref_rvalue (dest_sval, cd.get_arg_tree (0), cd.get_ctxt ());

  const svalue *fill_value_u8
    = mgr->get_or_create_cast (unsigned_char_type_node, fill_value_sval);

  const region *sized_dest_reg = mgr->get_sized_region (dest_reg,
							NULL_TREE,
							num_bytes_sval);
  model->fill_region (sized_dest_reg, fill_value_u8, cd.get_ctxt ());

  cd.maybe_set_lhs (dest_sval);
}

/* A subclass of pending_diagnostic for complaining about 'putenv'
   called on an auto var.  */

class putenv_of_auto_var
: public pending_diagnostic_subclass<putenv_of_auto_var>
{
public:
  putenv_of_auto_var (tree fndecl, const region *reg)
  : m_fndecl (fndecl), m_reg (reg),
    m_var_decl (reg->get_base_region ()->maybe_get_decl ())
  {
  }

  const char *get_kind () const final override
  {
    return "putenv_of_auto_var";
  }

  bool operator== (const putenv_of_auto_var &other) const
  {
    return (m_fndecl == other.m_fndecl
	    && m_reg == other.m_reg
	    && same_tree_p (m_var_decl, other.m_var_decl));
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_putenv_of_auto_var;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    auto_diagnostic_group d;

    /* SEI CERT C Coding Standard: "POS34-C. Do not call putenv() with a
       pointer to an automatic variable as the argument".  */
    diagnostic_metadata::precanned_rule
      rule ("POS34-C", "https://wiki.sei.cmu.edu/confluence/x/6NYxBQ");
    ctxt.add_rule (rule);

    bool warned;
    if (m_var_decl)
      warned = ctxt.warn ("%qE on a pointer to automatic variable %qE",
			  m_fndecl, m_var_decl);
    else
      warned = ctxt.warn ("%qE on a pointer to an on-stack buffer",
			  m_fndecl);
    if (warned)
      {
	if (m_var_decl)
	  inform (DECL_SOURCE_LOCATION (m_var_decl),
		  "%qE declared on stack here", m_var_decl);
	inform (ctxt.get_location (), "perhaps use %qs rather than %qE",
		"setenv", m_fndecl);
      }

    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    if (m_var_decl)
      pp_printf  (&pp,
		  "%qE on a pointer to automatic variable %qE",
		  m_fndecl, m_var_decl);
    else
      pp_printf  (&pp,
		  "%qE on a pointer to an on-stack buffer",
		  m_fndecl);
    return true;
  }

  void mark_interesting_stuff (interesting_t *interest) final override
  {
    if (!m_var_decl)
      interest->add_region_creation (m_reg->get_base_region ());
  }

private:
  tree m_fndecl; // non-NULL
  const region *m_reg; // non-NULL
  tree m_var_decl; // could be NULL
};

/* Handler for calls to "putenv".

   In theory we could try to model the state of the environment variables
   for the process; for now we merely complain about putenv of regions
   on the stack.  */

class kf_putenv : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1 && cd.arg_is_pointer_p (0));
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    tree fndecl = cd.get_fndecl_for_call ();
    gcc_assert (fndecl);
    region_model_context *ctxt = cd.get_ctxt ();
    region_model *model = cd.get_model ();
    model->check_for_null_terminated_string_arg (cd, 0);
    const svalue *ptr_sval = cd.get_arg_svalue (0);
    const region *reg
      = model->deref_rvalue (ptr_sval, cd.get_arg_tree (0), ctxt);
    model->get_store ()->mark_as_escaped (reg);
    enum memory_space mem_space = reg->get_memory_space ();
    switch (mem_space)
      {
      default:
	gcc_unreachable ();
      case MEMSPACE_UNKNOWN:
      case MEMSPACE_CODE:
      case MEMSPACE_GLOBALS:
      case MEMSPACE_HEAP:
      case MEMSPACE_READONLY_DATA:
	break;
      case MEMSPACE_STACK:
	if (ctxt)
	  ctxt->warn (make_unique<putenv_of_auto_var> (fndecl, reg));
	break;
      }
    cd.set_any_lhs_with_defaults ();
  }
};

/* Handler for "realloc":

     void *realloc(void *ptr, size_t size);

   realloc(3) is awkward, since it has various different outcomes
   that are best modelled as separate exploded nodes/edges.

   We first check for sm-state, in
   malloc_state_machine::on_realloc_call, so that we
   can complain about issues such as realloc of a non-heap
   pointer, and terminate the path for such cases (and issue
   the complaints at the call's exploded node).

   Assuming that these checks pass, we split the path here into
   three special cases (and terminate the "standard" path):
   (A) failure, returning NULL
   (B) success, growing the buffer in-place without moving it
   (C) success, allocating a new buffer, copying the content
   of the old buffer to it, and freeing the old buffer.

   Each of these has a custom_edge_info subclass, which updates
   the region_model and sm-state of the destination state.  */

class kf_realloc : public builtin_known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 2
	    && cd.arg_is_pointer_p (0)
	    && cd.arg_is_size_p (1));
  }

  enum built_in_function builtin_code () const final override
  {
    return BUILT_IN_REALLOC;
  }

  void impl_call_post (const call_details &cd) const final override;
};

void
kf_realloc::impl_call_post (const call_details &cd) const
{
  /* Three custom subclasses of custom_edge_info, for handling the various
     outcomes of "realloc".  */

  /* Concrete custom_edge_info: a realloc call that fails, returning NULL.  */
  class failure : public failed_call_info
  {
  public:
    failure (const call_details &cd)
    : failed_call_info (cd)
    {
    }

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      /* Return NULL; everything else is unchanged.  */
      const call_details cd (get_call_details (model, ctxt));
      region_model_manager *mgr = cd.get_manager ();
      if (cd.get_lhs_type ())
	{
	  const svalue *zero
	    = mgr->get_or_create_int_cst (cd.get_lhs_type (), 0);
	  model->set_value (cd.get_lhs_region (),
			    zero,
			    cd.get_ctxt ());
	}
      return true;
    }
  };

  /* Concrete custom_edge_info: a realloc call that succeeds, growing
     the existing buffer without moving it.  */
  class success_no_move : public call_info
  {
  public:
    success_no_move (const call_details &cd)
    : call_info (cd)
    {
    }

    void print_desc (pretty_printer &pp) const final override
    {
      pp_printf (&pp,
		 "when %qE succeeds, without moving buffer",
		 get_fndecl ());
    }

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      /* Update size of buffer and return the ptr unchanged.  */
      const call_details cd (get_call_details (model, ctxt));
      region_model_manager *mgr = cd.get_manager ();
      const svalue *ptr_sval = cd.get_arg_svalue (0);
      const svalue *size_sval = cd.get_arg_svalue (1);

      /* We can only grow in place with a non-NULL pointer.  */
      {
	const svalue *null_ptr
	  = mgr->get_or_create_int_cst (ptr_sval->get_type (), 0);
	if (!model->add_constraint (ptr_sval, NE_EXPR, null_ptr,
				    cd.get_ctxt ()))
	  return false;
      }

      if (const region *buffer_reg = model->deref_rvalue (ptr_sval, NULL_TREE,
							  ctxt))
	if (compat_types_p (size_sval->get_type (), size_type_node))
	  model->set_dynamic_extents (buffer_reg, size_sval, ctxt);
      if (cd.get_lhs_region ())
	{
	  model->set_value (cd.get_lhs_region (), ptr_sval, cd.get_ctxt ());
	  const svalue *zero
	    = mgr->get_or_create_int_cst (cd.get_lhs_type (), 0);
	  return model->add_constraint (ptr_sval, NE_EXPR, zero, ctxt);
	}
      else
	return true;
    }
  };

  /* Concrete custom_edge_info: a realloc call that succeeds, freeing
     the existing buffer and moving the content to a freshly allocated
     buffer.  */
  class success_with_move : public call_info
  {
  public:
    success_with_move (const call_details &cd)
    : call_info (cd)
    {
    }

    void print_desc (pretty_printer &pp) const final override
    {
      pp_printf (&pp,
		 "when %qE succeeds, moving buffer",
		 get_fndecl ());
    }
    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      region_model_manager *mgr = cd.get_manager ();
      const svalue *old_ptr_sval = cd.get_arg_svalue (0);
      const svalue *new_size_sval = cd.get_arg_svalue (1);

      /* Create the new region.  */
      const region *new_reg
	= model->get_or_create_region_for_heap_alloc (new_size_sval, ctxt);
      const svalue *new_ptr_sval
	= mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
      if (!model->add_constraint (new_ptr_sval, NE_EXPR, old_ptr_sval,
				  cd.get_ctxt ()))
	return false;

      if (cd.get_lhs_type ())
	cd.maybe_set_lhs (new_ptr_sval);

      if (const region *freed_reg = model->deref_rvalue (old_ptr_sval,
							 NULL_TREE, ctxt))
	{
	  /* Copy the data.  */
	  const svalue *old_size_sval = model->get_dynamic_extents (freed_reg);
	  if (old_size_sval)
	    {
	      const svalue *copied_size_sval
		= get_copied_size (model, old_size_sval, new_size_sval);
	      const region *copied_old_reg
		= mgr->get_sized_region (freed_reg, NULL, copied_size_sval);
	      const svalue *buffer_content_sval
		= model->get_store_value (copied_old_reg, cd.get_ctxt ());
	      const region *copied_new_reg
		= mgr->get_sized_region (new_reg, NULL, copied_size_sval);
	      model->set_value (copied_new_reg, buffer_content_sval,
				cd.get_ctxt ());
	    }
	  else
	    {
	      /* We don't know how big the old region was;
		 mark the new region as having been touched to avoid uninit
		 issues.  */
	      model->mark_region_as_unknown (new_reg, cd.get_uncertainty ());
	    }

	  /* Free the old region, so that pointers to the old buffer become
	     invalid.  */

	  /* If the ptr points to an underlying heap region, delete it,
	     poisoning pointers.  */
	  model->unbind_region_and_descendents (freed_reg, POISON_KIND_FREED);
	  model->unset_dynamic_extents (freed_reg);
	}

      /* Update the sm-state: mark the old_ptr_sval as "freed",
	 and the new_ptr_sval as "nonnull".  */
      model->on_realloc_with_move (cd, old_ptr_sval, new_ptr_sval);

      if (cd.get_lhs_type ())
	{
	  const svalue *zero
	    = mgr->get_or_create_int_cst (cd.get_lhs_type (), 0);
	  return model->add_constraint (new_ptr_sval, NE_EXPR, zero,
					cd.get_ctxt ());
	}
      else
	return true;
    }

  private:
    /* Return the lesser of OLD_SIZE_SVAL and NEW_SIZE_SVAL.
       If unknown, OLD_SIZE_SVAL is returned.  */
    const svalue *get_copied_size (region_model *model,
				   const svalue *old_size_sval,
				   const svalue *new_size_sval) const
    {
      tristate res
	= model->eval_condition (old_size_sval, GT_EXPR, new_size_sval);
      switch (res.get_value ())
	{
	case tristate::TS_TRUE:
	  return new_size_sval;
	case tristate::TS_FALSE:
	case tristate::TS_UNKNOWN:
	  return old_size_sval;
	default:
	  gcc_unreachable ();
	}
    }
  };

  /* Body of kf_realloc::impl_call_post.  */

  if (cd.get_ctxt ())
    {
      cd.get_ctxt ()->bifurcate (make_unique<failure> (cd));
      cd.get_ctxt ()->bifurcate (make_unique<success_no_move> (cd));
      cd.get_ctxt ()->bifurcate (make_unique<success_with_move> (cd));
      cd.get_ctxt ()->terminate_path ();
    }
}

/* Handler for "strchr" and "__builtin_strchr".  */

class kf_strchr : public builtin_known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 2 && cd.arg_is_pointer_p (0));
  }
  void impl_call_pre (const call_details &cd) const final override
  {
    cd.check_for_null_terminated_string_arg (0);
  }

  enum built_in_function builtin_code () const final override
  {
    return BUILT_IN_STRCHR;
  }
  void impl_call_post (const call_details &cd) const final override;
};

void
kf_strchr::impl_call_post (const call_details &cd) const
{
  class strchr_call_info : public call_info
  {
  public:
    strchr_call_info (const call_details &cd, bool found)
    : call_info (cd), m_found (found)
    {
    }

    void print_desc (pretty_printer &pp) const final override
    {
      if (m_found)
	pp_printf (&pp,
		   "when %qE returns non-NULL",
		   get_fndecl ());
      else
	pp_printf (&pp,
		   "when %qE returns NULL",
		   get_fndecl ());
    }

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      if (tree lhs_type = cd.get_lhs_type ())
	{
	  region_model_manager *mgr = model->get_manager ();
	  const svalue *result;
	  if (m_found)
	    {
	      const svalue *str_sval = cd.get_arg_svalue (0);
	      const region *str_reg
		= model->deref_rvalue (str_sval, cd.get_arg_tree (0),
				       cd.get_ctxt ());
	      /* We want str_sval + OFFSET for some unknown OFFSET.
		 Use a conjured_svalue to represent the offset,
		 using the str_reg as the id of the conjured_svalue.  */
	      const svalue *offset
		= mgr->get_or_create_conjured_svalue (size_type_node,
						      cd.get_call_stmt (),
						      str_reg,
						      conjured_purge (model,
								      ctxt));
	      result = mgr->get_or_create_binop (lhs_type, POINTER_PLUS_EXPR,
						 str_sval, offset);
	    }
	  else
	    result = mgr->get_or_create_int_cst (lhs_type, 0);
	  cd.maybe_set_lhs (result);
	}
      return true;
    }
  private:
    bool m_found;
  };

  /* Body of kf_strchr::impl_call_post.  */
  if (cd.get_ctxt ())
    {
      cd.get_ctxt ()->bifurcate (make_unique<strchr_call_info> (cd, false));
      cd.get_ctxt ()->bifurcate (make_unique<strchr_call_info> (cd, true));
      cd.get_ctxt ()->terminate_path ();
    }
}

/* Handler for "sprintf".
     int sprintf(char *str, const char *format, ...);
*/

class kf_sprintf : public builtin_known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () >= 2
	    && cd.arg_is_pointer_p (0)
	    && cd.arg_is_pointer_p (1));
  }

  enum built_in_function builtin_code () const final override
  {
    return BUILT_IN_SPRINTF;
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    /* For now, merely assume that the destination buffer gets set to a
       new svalue.  */
    region_model *model = cd.get_model ();
    region_model_context *ctxt = cd.get_ctxt ();
    const svalue *dst_ptr = cd.get_arg_svalue (0);
    const region *dst_reg
      = model->deref_rvalue (dst_ptr, cd.get_arg_tree (0), ctxt);
    const svalue *content = cd.get_or_create_conjured_svalue (dst_reg);
    model->set_value (dst_reg, content, ctxt);
    cd.set_any_lhs_with_defaults ();
  }
};

/* Handler for "__builtin_stack_restore".  */

class kf_stack_restore : public pure_known_function_with_default_return
{
public:
  bool matches_call_types_p (const call_details &) const final override
  {
    return true;
  }

  /* Currently a no-op.  */
};

/* Handler for "__builtin_stack_save".  */

class kf_stack_save : public pure_known_function_with_default_return
{
public:
  bool matches_call_types_p (const call_details &) const final override
  {
    return true;
  }

  /* Currently a no-op.  */
};

/* Handler for "strcat" and "__builtin_strcat_chk".  */

class kf_strcat : public builtin_known_function
{
public:
  kf_strcat (unsigned int num_args, bool chk_variant)
    : m_num_args (num_args),
      m_chk_variant (chk_variant) {}
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == m_num_args
	    && cd.arg_is_pointer_p (0)
	    && cd.arg_is_pointer_p (1));
  }

  enum built_in_function builtin_code () const final override
  {
    return m_chk_variant ? BUILT_IN_STRCAT_CHK : BUILT_IN_STRCAT;
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    region_model_manager *mgr = cd.get_manager ();

    const svalue *dest_sval = cd.get_arg_svalue (0);
    const region *dest_reg = model->deref_rvalue (dest_sval, cd.get_arg_tree (0),
						  cd.get_ctxt ());

    const svalue *dst_strlen_sval
      = cd.check_for_null_terminated_string_arg (0, false, nullptr);
    if (!dst_strlen_sval)
      {
	if (cd.get_ctxt ())
	  cd.get_ctxt ()->terminate_path ();
	return;
      }

    const svalue *bytes_to_copy;
    const svalue *num_src_bytes_read_sval
      = cd.check_for_null_terminated_string_arg (1, true, &bytes_to_copy);
    if (!num_src_bytes_read_sval)
      {
	if (cd.get_ctxt ())
	  cd.get_ctxt ()->terminate_path ();
	return;
      }

    cd.maybe_set_lhs (dest_sval);
    cd.complain_about_overlap (0, 1, num_src_bytes_read_sval);

    const region *offset_reg
      = mgr->get_offset_region (dest_reg, NULL_TREE, dst_strlen_sval);
    model->write_bytes (offset_reg,
			num_src_bytes_read_sval,
			bytes_to_copy,
			cd.get_ctxt ());
  }

private:
  unsigned int m_num_args;
  const bool m_chk_variant;
};

/* Handler for "strcpy" and "__builtin_strcpy_chk".  */

class kf_strcpy : public builtin_known_function
{
public:
  kf_strcpy (unsigned int num_args, bool chk_variant)
    : m_num_args (num_args),
      m_chk_variant (chk_variant) {}
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == m_num_args
	    && cd.arg_is_pointer_p (0)
	    && cd.arg_is_pointer_p (1));
  }
  enum built_in_function builtin_code () const final override
  {
    return m_chk_variant ? BUILT_IN_STRCPY_CHK : BUILT_IN_STRCPY;
  }
  void impl_call_pre (const call_details &cd) const final override;

private:
  unsigned int m_num_args;
  const bool m_chk_variant;
};

void
kf_strcpy::impl_call_pre (const call_details &cd) const
{
  region_model *model = cd.get_model ();
  region_model_context *ctxt = cd.get_ctxt ();

  const svalue *dest_sval = cd.get_arg_svalue (0);
  const region *dest_reg = model->deref_rvalue (dest_sval, cd.get_arg_tree (0),
						    ctxt);
  /* strcpy returns the initial param.  */
  cd.maybe_set_lhs (dest_sval);

  const svalue *bytes_to_copy;
  if (const svalue *num_bytes_read_sval
      = cd.check_for_null_terminated_string_arg (1, true, &bytes_to_copy))
    {
      cd.complain_about_overlap (0, 1, num_bytes_read_sval);
      model->write_bytes (dest_reg, num_bytes_read_sval, bytes_to_copy, ctxt);
    }
  else
    {
      if (cd.get_ctxt ())
	cd.get_ctxt ()->terminate_path ();
    }
}

/* Handler for "strdup" and "__builtin_strdup".  */

class kf_strdup : public builtin_known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1 && cd.arg_is_pointer_p (0));
  }
  enum built_in_function builtin_code () const final override
  {
    return BUILT_IN_STRDUP;
  }
  void impl_call_pre (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    region_model_context *ctxt = cd.get_ctxt ();
    region_model_manager *mgr = cd.get_manager ();
    const svalue *bytes_to_copy;
    if (const svalue *num_bytes_read_sval
	= cd.check_for_null_terminated_string_arg (0, true, &bytes_to_copy))
      {
	const region *new_reg
	  = model->get_or_create_region_for_heap_alloc (num_bytes_read_sval,
							ctxt);
	model->write_bytes (new_reg, num_bytes_read_sval, bytes_to_copy, ctxt);
	if (cd.get_lhs_type ())
	  {
	    const svalue *ptr_sval
	      = mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
	    cd.maybe_set_lhs (ptr_sval);
	  }
      }
    else
      {
	if (ctxt)
	  ctxt->terminate_path ();
      }
  }
};

/* Handler for "strlen" and for "__analyzer_get_strlen".  */

class kf_strlen : public builtin_known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1 && cd.arg_is_pointer_p (0));
  }
  enum built_in_function builtin_code () const final override
  {
    return BUILT_IN_STRLEN;
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    if (const svalue *strlen_sval
	  = cd.check_for_null_terminated_string_arg (0, false, nullptr))
      if (strlen_sval->get_kind () != SK_UNKNOWN)
	{
	  cd.maybe_set_lhs (strlen_sval);
	  return;
	}

    /* Use a conjured svalue.  */
    cd.set_any_lhs_with_defaults ();
  }
};

/* Factory function, so that kf-analyzer.cc can use this class.  */

std::unique_ptr<known_function>
make_kf_strlen ()
{
  return make_unique<kf_strlen> ();
}

/* Handler for "strncpy" and "__builtin_strncpy".
   See e.g. https://en.cppreference.com/w/c/string/byte/strncpy

     extern char *strncpy (char *dst, const char *src, size_t count);

   Handle this by splitting into two outcomes:
   (a) truncated read from "src" of "count" bytes,
       writing "count" bytes to "dst"
   (b) read from "src" of up to (and including) the null terminator,
       where the number of bytes read < "count" bytes,
       writing those bytes to "dst", and zero-filling the rest,
       up to "count".  */

class kf_strncpy : public builtin_known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 3
	    && cd.arg_is_pointer_p (0)
	    && cd.arg_is_pointer_p (1)
	    && cd.arg_is_integral_p (2));
  }
  enum built_in_function builtin_code () const final override
  {
    return BUILT_IN_STRNCPY;
  }
  void impl_call_post (const call_details &cd) const final override;
};

void
kf_strncpy::impl_call_post (const call_details &cd) const
{
  class strncpy_call_info : public call_info
  {
  public:
    strncpy_call_info (const call_details &cd,
		       const svalue *num_bytes_with_terminator_sval,
		       bool truncated_read)
    : call_info (cd),
      m_num_bytes_with_terminator_sval (num_bytes_with_terminator_sval),
      m_truncated_read (truncated_read)
    {
    }

    void print_desc (pretty_printer &pp) const final override
    {
      if (m_truncated_read)
	pp_printf (&pp,
		   "when %qE truncates the source string",
		   get_fndecl ());
      else
	pp_printf (&pp,
		   "when %qE copies the full source string",
		   get_fndecl ());
    }

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));

      const svalue *dest_sval = cd.get_arg_svalue (0);
      const region *dest_reg
	= model->deref_rvalue (dest_sval, cd.get_arg_tree (0), ctxt);

      const svalue *src_sval = cd.get_arg_svalue (1);
      const region *src_reg
	= model->deref_rvalue (src_sval, cd.get_arg_tree (1), ctxt);

      const svalue *count_sval = cd.get_arg_svalue (2);

      /* strncpy returns the initial param.  */
      cd.maybe_set_lhs (dest_sval);

      const svalue *num_bytes_read_sval;
      if (m_truncated_read)
	{
	  /* Truncated read.  */
	  num_bytes_read_sval = count_sval;

	  if (m_num_bytes_with_terminator_sval)
	    {
	      /* The terminator is after the limit.  */
	      if (!model->add_constraint (m_num_bytes_with_terminator_sval,
					  GT_EXPR,
					  count_sval,
					  ctxt))
		return false;
	    }
	  else
	    {
	      /* We don't know where the terminator is, or if there is one.
		 In theory we know that the first COUNT bytes are non-zero,
		 but we don't have a way to record that constraint.  */
	    }
	}
      else
	{
	  /* Full read of the src string before reaching the limit,
	     so there must be a terminator and it must be at or before
	     the limit.  */
	  if (m_num_bytes_with_terminator_sval)
	    {
	      if (!model->add_constraint (m_num_bytes_with_terminator_sval,
					  LE_EXPR,
					  count_sval,
					  ctxt))
		return false;
	      num_bytes_read_sval = m_num_bytes_with_terminator_sval;

	      /* First, zero-fill the dest buffer.
		 We don't need to do this for the truncation case, as
		 this fully populates the dest buffer.  */
	      const region *sized_dest_reg
		= model->get_manager ()->get_sized_region (dest_reg,
							   NULL_TREE,
							   count_sval);
	      model->zero_fill_region (sized_dest_reg, ctxt);
	    }
	  else
	    {
	      /* Don't analyze this case; the other case will
		 assume a "truncated" read up to the limit.  */
	      return false;
	    }
	}

      gcc_assert (num_bytes_read_sval);

      const svalue *bytes_to_copy
	= model->read_bytes (src_reg,
			     cd.get_arg_tree (1),
			     num_bytes_read_sval,
			     ctxt);
      cd.complain_about_overlap (0, 1, num_bytes_read_sval);
      model->write_bytes (dest_reg,
			  num_bytes_read_sval,
			  bytes_to_copy,
			  ctxt);

      return true;
    }
  private:
    /* (strlen + 1) of the source string if it has a terminator,
       or NULL for the case where UB would happen before
       finding any terminator.  */
    const svalue *m_num_bytes_with_terminator_sval;

    /* true: if this is the outcome where the limit was reached before
       the null terminator
       false: if the null terminator was reached before the limit.  */
    bool m_truncated_read;
  };

  /* Body of kf_strncpy::impl_call_post.  */
  if (cd.get_ctxt ())
    {
      /* First, scan for a null terminator as if there were no limit,
	 with a null ctxt so no errors are reported.  */
      const region_model *model = cd.get_model ();
      const svalue *ptr_arg_sval = cd.get_arg_svalue (1);
      const region *buf_reg
	= model->deref_rvalue (ptr_arg_sval, cd.get_arg_tree (1), nullptr);
      const svalue *num_bytes_with_terminator_sval
	= model->scan_for_null_terminator (buf_reg,
					   cd.get_arg_tree (1),
					   nullptr,
					   nullptr);
      cd.get_ctxt ()->bifurcate
	(make_unique<strncpy_call_info> (cd, num_bytes_with_terminator_sval,
					 false));
      cd.get_ctxt ()->bifurcate
	(make_unique<strncpy_call_info> (cd, num_bytes_with_terminator_sval,
					 true));
      cd.get_ctxt ()->terminate_path ();
    }
};

/* Handler for "strndup" and "__builtin_strndup".  */

class kf_strndup : public builtin_known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 2 && cd.arg_is_pointer_p (0));
  }
  enum built_in_function builtin_code () const final override
  {
    return BUILT_IN_STRNDUP;
  }
  void impl_call_pre (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    region_model_manager *mgr = cd.get_manager ();
    /* Ideally we'd get the size here, and simulate copying the bytes.  */
    const region *new_reg
      = model->get_or_create_region_for_heap_alloc (NULL, cd.get_ctxt ());
    model->mark_region_as_unknown (new_reg, NULL);
    if (cd.get_lhs_type ())
      {
	const svalue *ptr_sval
	  = mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
	cd.maybe_set_lhs (ptr_sval);
      }
  }
};

/* Handler for "strstr" and "__builtin_strstr".
     extern char *strstr (const char* str, const char* substr);
   See e.g. https://en.cppreference.com/w/c/string/byte/strstr  */

class kf_strstr : public builtin_known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 2
	    && cd.arg_is_pointer_p (0)
	    && cd.arg_is_pointer_p (1));
  }
  enum built_in_function builtin_code () const final override
  {
    return BUILT_IN_STRSTR;
  }
  void impl_call_pre (const call_details &cd) const final override
  {
    cd.check_for_null_terminated_string_arg (0);
    cd.check_for_null_terminated_string_arg (1);
  }
  void impl_call_post (const call_details &cd) const final override;
};

void
kf_strstr::impl_call_post (const call_details &cd) const
{
  class strstr_call_info : public call_info
  {
  public:
    strstr_call_info (const call_details &cd, bool found)
    : call_info (cd), m_found (found)
    {
    }

    void print_desc (pretty_printer &pp) const final override
    {
      if (m_found)
	pp_printf (&pp,
		   "when %qE returns non-NULL",
		   get_fndecl ());
      else
	pp_printf (&pp,
		   "when %qE returns NULL",
		   get_fndecl ());
    }

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      if (tree lhs_type = cd.get_lhs_type ())
	{
	  region_model_manager *mgr = model->get_manager ();
	  const svalue *result;
	  if (m_found)
	    {
	      const svalue *str_sval = cd.get_arg_svalue (0);
	      const region *str_reg
		= model->deref_rvalue (str_sval, cd.get_arg_tree (0),
				       cd.get_ctxt ());
	      /* We want str_sval + OFFSET for some unknown OFFSET.
		 Use a conjured_svalue to represent the offset,
		 using the str_reg as the id of the conjured_svalue.  */
	      const svalue *offset
		= mgr->get_or_create_conjured_svalue (size_type_node,
						      cd.get_call_stmt (),
						      str_reg,
						      conjured_purge (model,
								      ctxt));
	      result = mgr->get_or_create_binop (lhs_type, POINTER_PLUS_EXPR,
						 str_sval, offset);
	    }
	  else
	    result = mgr->get_or_create_int_cst (lhs_type, 0);
	  cd.maybe_set_lhs (result);
	}
      return true;
    }
  private:
    bool m_found;
  };

  /* Body of kf_strstr::impl_call_post.  */
  if (cd.get_ctxt ())
    {
      cd.get_ctxt ()->bifurcate (make_unique<strstr_call_info> (cd, false));
      cd.get_ctxt ()->bifurcate (make_unique<strstr_call_info> (cd, true));
      cd.get_ctxt ()->terminate_path ();
    }
}

/* Handle calls to "strtok".
   See e.g.
     https://en.cppreference.com/w/c/string/byte/strtok
     https://man7.org/linux/man-pages/man3/strtok.3.html  */

class kf_strtok : public known_function
{
public:
  class undefined_behavior : public undefined_function_behavior
  {
  public:
    undefined_behavior (const call_details &cd)
    : undefined_function_behavior (cd)
    {
    }
    int get_controlling_option () const final override
    {
      return OPT_Wanalyzer_undefined_behavior_strtok;
    }

    bool emit (diagnostic_emission_context &ctxt) final override
    {
      /* CWE-476: NULL Pointer Dereference.  */
      ctxt.add_cwe (476);
      if (ctxt.warn ("calling %qD for first time with NULL as argument 1"
		     " has undefined behavior",
		     get_callee_fndecl ()))
	{
	  inform (ctxt.get_location (),
		  "some implementations of %qD may crash on such input",
		  get_callee_fndecl ());
	  return true;
	}
      return false;
    }

    bool
    describe_final_event (pretty_printer &pp,
			  const evdesc::final_event &) final override
    {
      pp_printf (&pp,
		 "calling %qD for first time with NULL as argument 1"
		 " has undefined behavior",
		 get_callee_fndecl ());
      return true;
    }
  };

  /* An outcome of a "strtok" call.
     We have a four-way bifurcation of the analysis via the
     4 combinations of two flags:
     - m_nonnull_str covers whether the "str" param was null or non-null
     - m_found covers whether the result is null or non-null
   */
  class strtok_call_info : public call_info
  {
  public:
    strtok_call_info (const call_details &cd,
		      const private_region &private_reg,
		      bool nonnull_str,
		      bool found)
    : call_info (cd),
      m_private_reg (private_reg),
      m_nonnull_str (nonnull_str),
      m_found (found)
    {
    }

    void print_desc (pretty_printer &pp) const final override
    {
      if (m_nonnull_str)
	{
	  if (m_found)
	    pp_printf (&pp,
		       "when %qE on non-NULL string returns non-NULL",
		       get_fndecl ());
	  else
	    pp_printf (&pp,
		       "when %qE on non-NULL string returns NULL",
		       get_fndecl ());
	}
      else
	{
	  if (m_found)
	    pp_printf (&pp,
		       "when %qE with NULL string (using prior) returns"
		       " non-NULL",
		       get_fndecl ());
	  else
	    pp_printf (&pp,
		       "when %qE with NULL string (using prior) returns NULL",
		       get_fndecl ());
	}
    }

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      region_model_manager *mgr = model->get_manager ();
      const call_details cd (get_call_details (model, ctxt));
      const svalue *str_sval = cd.get_arg_svalue (0);
      /* const svalue *delim_sval = cd.get_arg_svalue (1); */

      cd.check_for_null_terminated_string_arg (1);
      /* We check that either arg 0 or the private region is null
	 terminated below.  */

      const svalue *null_ptr_sval
	= mgr->get_or_create_null_ptr (cd.get_arg_type (0));;
      if (!model->add_constraint (str_sval,
				  m_nonnull_str ? NE_EXPR : EQ_EXPR,
				  null_ptr_sval,
				  cd.get_ctxt ()))
	return false;

      if (m_nonnull_str)
	{
	  /* Update internal buffer.  */
	  model->set_value (&m_private_reg,
			    mgr->get_or_create_unmergeable (str_sval),
			    ctxt);
	}
      else
	{
	  /* Read from internal buffer.  */
	  str_sval = model->get_store_value (&m_private_reg, ctxt);

	  /* The initial value of the private region is NULL when we're
	     on a path from main.  */
	  if (const initial_svalue *initial_sval
		= str_sval->dyn_cast_initial_svalue ())
	    if (initial_sval->get_region () == &m_private_reg
		&& model->called_from_main_p ())
	      {
		/* Implementations of strtok do not necessarily check for NULL
		   here, and may crash; see PR analyzer/107573.
		   Warn for this, if we were definitely passed NULL.  */
		if (cd.get_arg_svalue (0)->all_zeroes_p ())
		  {
		    if (ctxt)
		      ctxt->warn (::make_unique<undefined_behavior> (cd));
		  }

		/* Assume that "str" was actually non-null; terminate
		   this path.  */
		return false;
	      }

	  /* Now assume str_sval is non-null.  */
	  if (!model->add_constraint (str_sval,
				      NE_EXPR,
				      null_ptr_sval,
				      cd.get_ctxt ()))
	    return false;
	}

      const region *buf_reg = model->deref_rvalue (str_sval, NULL_TREE, ctxt);
      model->scan_for_null_terminator (buf_reg,
				       NULL_TREE,
				       nullptr,
				       ctxt);

      if (m_found)
	{
	  const region *str_reg
	    = model->deref_rvalue (str_sval, cd.get_arg_tree (0),
				   cd.get_ctxt ());
	  /* We want to figure out the start and nul terminator
	     for the token.
	     For each, we want str_sval + OFFSET for some unknown OFFSET.
	     Use a conjured_svalue to represent the offset,
	     using the str_reg as the id of the conjured_svalue.  */
	  const svalue *start_offset
	    = mgr->get_or_create_conjured_svalue (size_type_node,
						  cd.get_call_stmt (),
						  str_reg,
						  conjured_purge (model,
								  ctxt),
						  0);
	  const svalue *nul_offset
	    = mgr->get_or_create_conjured_svalue (size_type_node,
						  cd.get_call_stmt (),
						  str_reg,
						  conjured_purge (model,
								  ctxt),
						  1);

	  tree char_ptr_type = build_pointer_type (char_type_node);
	  const svalue *result
	    = mgr->get_or_create_binop (char_ptr_type, POINTER_PLUS_EXPR,
					str_sval, start_offset);
	  cd.maybe_set_lhs (result);

	  /* nul_offset + 1; the offset to use for the next call.  */
	  const svalue *next_offset
	    = mgr->get_or_create_binop (size_type_node, PLUS_EXPR,
					nul_offset,
					mgr->get_or_create_int_cst
					(char_type_node, 1));

	  /* Write '\0' to str_sval[nul_offset].  */
	  const svalue *ptr_to_term
	    = mgr->get_or_create_binop (char_ptr_type, POINTER_PLUS_EXPR,
					str_sval, nul_offset);
	  const region *terminator_reg
	    = model->deref_rvalue (ptr_to_term, NULL_TREE, cd.get_ctxt ());
	  model->set_value (terminator_reg,
			    mgr->get_or_create_unmergeable
			    (mgr->get_or_create_int_cst (char_type_node,
							 0)),
			    cd.get_ctxt ());

	  /* Update saved ptr to be at [nul_offset + 1].  */
	  const svalue *ptr_to_next
	    = mgr->get_or_create_binop (cd.get_lhs_type (), POINTER_PLUS_EXPR,
					str_sval, next_offset);
	  model->set_value (&m_private_reg, ptr_to_next, ctxt);
	}
      else
	if (tree lhs_type = cd.get_lhs_type ())
	  {
	    const svalue *result
	      = mgr->get_or_create_int_cst (lhs_type, 0);
	    cd.maybe_set_lhs (result);
	  }
      return true;
    }
  private:
    const private_region &m_private_reg;
    bool m_nonnull_str;
    bool m_found;
  }; // class strtok_call_info

  kf_strtok (region_model_manager &mgr)
  : m_private_reg (mgr.alloc_symbol_id (),
		   mgr.get_root_region (),
		   get_region_type (),
		   "strtok buffer")
  {
  }

  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 2
	    && POINTER_TYPE_P (cd.get_arg_type (0))
	    && POINTER_TYPE_P (cd.get_arg_type (1)));
  }

  void impl_call_post (const call_details &cd) const final override
  {
    if (cd.get_ctxt ())
      {
	/* Four-way bifurcation, based on whether:
	   - the str is non-null
	   - the result is non-null
	   Typically the str is either null or non-null at a particular site,
	   so hopefully this will generally just lead to two out-edges.  */
	cd.get_ctxt ()->bifurcate
	  (make_unique<strtok_call_info> (cd, m_private_reg, false, false));
	cd.get_ctxt ()->bifurcate
	  (make_unique<strtok_call_info> (cd, m_private_reg, false, true));
	cd.get_ctxt ()->bifurcate
	  (make_unique<strtok_call_info> (cd, m_private_reg, true, false));
	cd.get_ctxt ()->bifurcate
	  (make_unique<strtok_call_info> (cd, m_private_reg, true, true));
	cd.get_ctxt ()->terminate_path ();
      }
  }

private:
  static tree get_region_type ()
  {
    return build_pointer_type (char_type_node);
  }
  const private_region m_private_reg;
};

class kf_ubsan_bounds : public internal_known_function
{
  /* Empty.  */
};

/* Handle calls to functions referenced by
   __attribute__((malloc(FOO))).  */

void
region_model::impl_deallocation_call (const call_details &cd)
{
  kf_free kf;
  kf.impl_call_post (cd);
}

static void
register_atomic_builtins (known_function_manager &kfm)
{
  kfm.add (BUILT_IN_ATOMIC_EXCHANGE, make_unique<kf_atomic_exchange> ());
  kfm.add (BUILT_IN_ATOMIC_EXCHANGE_N, make_unique<kf_atomic_exchange_n> ());
  kfm.add (BUILT_IN_ATOMIC_EXCHANGE_1, make_unique<kf_atomic_exchange_n> ());
  kfm.add (BUILT_IN_ATOMIC_EXCHANGE_2, make_unique<kf_atomic_exchange_n> ());
  kfm.add (BUILT_IN_ATOMIC_EXCHANGE_4, make_unique<kf_atomic_exchange_n> ());
  kfm.add (BUILT_IN_ATOMIC_EXCHANGE_8, make_unique<kf_atomic_exchange_n> ());
  kfm.add (BUILT_IN_ATOMIC_EXCHANGE_16, make_unique<kf_atomic_exchange_n> ());
  kfm.add (BUILT_IN_ATOMIC_LOAD, make_unique<kf_atomic_load> ());
  kfm.add (BUILT_IN_ATOMIC_LOAD_N, make_unique<kf_atomic_load_n> ());
  kfm.add (BUILT_IN_ATOMIC_LOAD_1, make_unique<kf_atomic_load_n> ());
  kfm.add (BUILT_IN_ATOMIC_LOAD_2, make_unique<kf_atomic_load_n> ());
  kfm.add (BUILT_IN_ATOMIC_LOAD_4, make_unique<kf_atomic_load_n> ());
  kfm.add (BUILT_IN_ATOMIC_LOAD_8, make_unique<kf_atomic_load_n> ());
  kfm.add (BUILT_IN_ATOMIC_LOAD_16, make_unique<kf_atomic_load_n> ());
  kfm.add (BUILT_IN_ATOMIC_STORE, make_unique<kf_atomic_store> ());
  kfm.add (BUILT_IN_ATOMIC_STORE_N, make_unique<kf_atomic_store_n> ());
  kfm.add (BUILT_IN_ATOMIC_STORE_1, make_unique<kf_atomic_store_n> ());
  kfm.add (BUILT_IN_ATOMIC_STORE_2, make_unique<kf_atomic_store_n> ());
  kfm.add (BUILT_IN_ATOMIC_STORE_4, make_unique<kf_atomic_store_n> ());
  kfm.add (BUILT_IN_ATOMIC_STORE_8, make_unique<kf_atomic_store_n> ());
  kfm.add (BUILT_IN_ATOMIC_STORE_16, make_unique<kf_atomic_store_n> ());
  kfm.add (BUILT_IN_ATOMIC_ADD_FETCH_1,
	   make_unique<kf_atomic_op_fetch> (PLUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_ADD_FETCH_2,
	   make_unique<kf_atomic_op_fetch> (PLUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_ADD_FETCH_4,
	   make_unique<kf_atomic_op_fetch> (PLUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_ADD_FETCH_8,
	   make_unique<kf_atomic_op_fetch> (PLUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_ADD_FETCH_16,
	   make_unique<kf_atomic_op_fetch> (PLUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_SUB_FETCH_1,
	   make_unique<kf_atomic_op_fetch> (MINUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_SUB_FETCH_2,
	   make_unique<kf_atomic_op_fetch> (MINUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_SUB_FETCH_4,
	   make_unique<kf_atomic_op_fetch> (MINUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_SUB_FETCH_8,
	   make_unique<kf_atomic_op_fetch> (MINUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_SUB_FETCH_16,
	   make_unique<kf_atomic_op_fetch> (MINUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_AND_FETCH_1,
	   make_unique<kf_atomic_op_fetch> (BIT_AND_EXPR));
  kfm.add (BUILT_IN_ATOMIC_AND_FETCH_2,
	   make_unique<kf_atomic_op_fetch> (BIT_AND_EXPR));
  kfm.add (BUILT_IN_ATOMIC_AND_FETCH_4,
	   make_unique<kf_atomic_op_fetch> (BIT_AND_EXPR));
  kfm.add (BUILT_IN_ATOMIC_AND_FETCH_8,
	   make_unique<kf_atomic_op_fetch> (BIT_AND_EXPR));
  kfm.add (BUILT_IN_ATOMIC_AND_FETCH_16,
	   make_unique<kf_atomic_op_fetch> (BIT_AND_EXPR));
  kfm.add (BUILT_IN_ATOMIC_XOR_FETCH_1,
	   make_unique<kf_atomic_op_fetch> (BIT_XOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_XOR_FETCH_2,
	   make_unique<kf_atomic_op_fetch> (BIT_XOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_XOR_FETCH_4,
	   make_unique<kf_atomic_op_fetch> (BIT_XOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_XOR_FETCH_8,
	   make_unique<kf_atomic_op_fetch> (BIT_XOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_XOR_FETCH_16,
	   make_unique<kf_atomic_op_fetch> (BIT_XOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_OR_FETCH_1,
	   make_unique<kf_atomic_op_fetch> (BIT_IOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_OR_FETCH_2,
	   make_unique<kf_atomic_op_fetch> (BIT_IOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_OR_FETCH_4,
	   make_unique<kf_atomic_op_fetch> (BIT_IOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_OR_FETCH_8,
	   make_unique<kf_atomic_op_fetch> (BIT_IOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_OR_FETCH_16,
	   make_unique<kf_atomic_op_fetch> (BIT_IOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_ADD_1,
	   make_unique<kf_atomic_fetch_op> (PLUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_ADD_2,
	   make_unique<kf_atomic_fetch_op> (PLUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_ADD_4,
	   make_unique<kf_atomic_fetch_op> (PLUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_ADD_8,
	   make_unique<kf_atomic_fetch_op> (PLUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_ADD_16,
	   make_unique<kf_atomic_fetch_op> (PLUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_SUB_1,
	   make_unique<kf_atomic_fetch_op> (MINUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_SUB_2,
	   make_unique<kf_atomic_fetch_op> (MINUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_SUB_4,
	   make_unique<kf_atomic_fetch_op> (MINUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_SUB_8,
	   make_unique<kf_atomic_fetch_op> (MINUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_SUB_16,
	   make_unique<kf_atomic_fetch_op> (MINUS_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_AND_1,
	   make_unique<kf_atomic_fetch_op> (BIT_AND_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_AND_2,
	   make_unique<kf_atomic_fetch_op> (BIT_AND_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_AND_4,
	   make_unique<kf_atomic_fetch_op> (BIT_AND_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_AND_8,
	   make_unique<kf_atomic_fetch_op> (BIT_AND_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_AND_16,
	   make_unique<kf_atomic_fetch_op> (BIT_AND_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_XOR_1,
	   make_unique<kf_atomic_fetch_op> (BIT_XOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_XOR_2,
	   make_unique<kf_atomic_fetch_op> (BIT_XOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_XOR_4,
	   make_unique<kf_atomic_fetch_op> (BIT_XOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_XOR_8,
	   make_unique<kf_atomic_fetch_op> (BIT_XOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_XOR_16,
	   make_unique<kf_atomic_fetch_op> (BIT_XOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_OR_1,
	   make_unique<kf_atomic_fetch_op> (BIT_IOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_OR_2,
	   make_unique<kf_atomic_fetch_op> (BIT_IOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_OR_4,
	   make_unique<kf_atomic_fetch_op> (BIT_IOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_OR_8,
	   make_unique<kf_atomic_fetch_op> (BIT_IOR_EXPR));
  kfm.add (BUILT_IN_ATOMIC_FETCH_OR_16,
	   make_unique<kf_atomic_fetch_op> (BIT_IOR_EXPR));
}

/* Handle calls to the various __builtin___ubsan_handle_*.
   These can return, but continuing after such a return
   isn't likely to be interesting to the user of the analyzer.
   Hence we terminate the analysis path at one of these calls.  */

class kf_ubsan_handler : public internal_known_function
{
  void impl_call_post (const call_details &cd) const final override
  {
    if (cd.get_ctxt ())
      cd.get_ctxt ()->terminate_path ();
  }
};

static void
register_sanitizer_builtins (known_function_manager &kfm)
{
  kfm.add (BUILT_IN_UBSAN_HANDLE_NONNULL_ARG,
	   make_unique<kf_ubsan_handler> ());
}

/* Populate KFM with instances of known functions supported by the core of the
   analyzer (as opposed to plugins).  */

void
register_known_functions (known_function_manager &kfm,
			  region_model_manager &rmm)
{
  /* Debugging/test support functions, all  with a "__analyzer_" prefix.  */
  register_known_analyzer_functions (kfm);

  /* Internal fns the analyzer has known_functions for.  */
  {
    kfm.add (IFN_BUILTIN_EXPECT, make_unique<kf_expect> ());
    kfm.add (IFN_UBSAN_BOUNDS, make_unique<kf_ubsan_bounds> ());
  }

  /* GCC built-ins that do not correspond to a function
     in the standard library.  */
  {
    kfm.add (BUILT_IN_EXPECT, make_unique<kf_expect> ());
    kfm.add (BUILT_IN_EXPECT_WITH_PROBABILITY, make_unique<kf_expect> ());
    kfm.add (BUILT_IN_ALLOCA_WITH_ALIGN, make_unique<kf_alloca> ());
    kfm.add (BUILT_IN_STACK_RESTORE, make_unique<kf_stack_restore> ());
    kfm.add (BUILT_IN_STACK_SAVE, make_unique<kf_stack_save> ());

    register_atomic_builtins (kfm);
    register_sanitizer_builtins (kfm);
    register_varargs_builtins (kfm);
  }

  /* Known builtins and C standard library functions
     the analyzer has known functions for.  */
  {
    kfm.add ("alloca", make_unique<kf_alloca> ());
    kfm.add ("__builtin_alloca", make_unique<kf_alloca> ());
    kfm.add ("calloc", make_unique<kf_calloc> ());
    kfm.add ("__builtin_calloc", make_unique<kf_calloc> ());
    kfm.add ("free", make_unique<kf_free> ());
    kfm.add ("__builtin_free", make_unique<kf_free> ());
    kfm.add ("malloc", make_unique<kf_malloc> ());
    kfm.add ("__builtin_malloc", make_unique<kf_malloc> ());
    kfm.add ("memcpy",
	      make_unique<kf_memcpy_memmove> (kf_memcpy_memmove::KF_MEMCPY));
    kfm.add ("__builtin_memcpy",
	      make_unique<kf_memcpy_memmove> (kf_memcpy_memmove::KF_MEMCPY));
    kfm.add ("__memcpy_chk", make_unique<kf_memcpy_memmove>
			      (kf_memcpy_memmove::KF_MEMCPY_CHK));
    kfm.add ("__builtin___memcpy_chk", make_unique<kf_memcpy_memmove>
			      (kf_memcpy_memmove::KF_MEMCPY_CHK));
    kfm.add ("memmove",
	      make_unique<kf_memcpy_memmove> (kf_memcpy_memmove::KF_MEMMOVE));
    kfm.add ("__builtin_memmove",
	      make_unique<kf_memcpy_memmove> (kf_memcpy_memmove::KF_MEMMOVE));
    kfm.add ("__memmove_chk", make_unique<kf_memcpy_memmove>
			      (kf_memcpy_memmove::KF_MEMMOVE_CHK));
    kfm.add ("__builtin___memmove_chk", make_unique<kf_memcpy_memmove>
			      (kf_memcpy_memmove::KF_MEMMOVE_CHK));
    kfm.add ("memset", make_unique<kf_memset> (false));
    kfm.add ("__builtin_memset", make_unique<kf_memset> (false));
    kfm.add ("__memset_chk", make_unique<kf_memset> (true));
    kfm.add ("__builtin___memset_chk", make_unique<kf_memset> (true));
    kfm.add ("realloc", make_unique<kf_realloc> ());
    kfm.add ("__builtin_realloc", make_unique<kf_realloc> ());
    kfm.add ("sprintf", make_unique<kf_sprintf> ());
    kfm.add ("__builtin_sprintf", make_unique<kf_sprintf> ());
    kfm.add ("strchr", make_unique<kf_strchr> ());
    kfm.add ("__builtin_strchr", make_unique<kf_strchr> ());
    kfm.add ("strcpy", make_unique<kf_strcpy> (2, false));
    kfm.add ("__builtin_strcpy", make_unique<kf_strcpy> (2, false));
    kfm.add ("__strcpy_chk", make_unique<kf_strcpy> (3, true));
    kfm.add ("__builtin___strcpy_chk", make_unique<kf_strcpy> (3, true));
    kfm.add ("strcat", make_unique<kf_strcat> (2, false));
    kfm.add ("__builtin_strcat", make_unique<kf_strcat> (2, false));
    kfm.add ("__strcat_chk", make_unique<kf_strcat> (3, true));
    kfm.add ("__builtin___strcat_chk", make_unique<kf_strcat> (3, true));
    kfm.add ("strdup", make_unique<kf_strdup> ());
    kfm.add ("__builtin_strdup", make_unique<kf_strdup> ());
    kfm.add ("strncpy", make_unique<kf_strncpy> ());
    kfm.add ("__builtin_strncpy", make_unique<kf_strncpy> ());
    kfm.add ("strndup", make_unique<kf_strndup> ());
    kfm.add ("__builtin_strndup", make_unique<kf_strndup> ());
    kfm.add ("strlen", make_unique<kf_strlen> ());
    kfm.add ("__builtin_strlen", make_unique<kf_strlen> ());
    kfm.add ("strstr", make_unique<kf_strstr> ());
    kfm.add ("__builtin_strstr", make_unique<kf_strstr> ());

    register_atomic_builtins (kfm);
    register_varargs_builtins (kfm);
  }

  /* Known POSIX functions, and some non-standard extensions.  */
  {
    kfm.add ("fopen", make_unique<kf_fopen> ());
    kfm.add ("putenv", make_unique<kf_putenv> ());
    kfm.add ("strtok", make_unique<kf_strtok> (rmm));

    register_known_fd_functions (kfm);
    register_known_file_functions (kfm);
  }

  /* glibc functions.  */
  {
    kfm.add ("__errno_location", make_unique<kf_errno_location> ());
    kfm.add ("error", make_unique<kf_error> (3));
    kfm.add ("error_at_line", make_unique<kf_error> (5));
    /* Variants of "error" and "error_at_line" seen by the
       analyzer at -O0 (PR analyzer/115724).  */
    kfm.add ("__error_alias", make_unique<kf_error> (3));
    kfm.add ("__error_at_line_alias", make_unique<kf_error> (5));
  }

  /* Other implementations of C standard library.  */
  {
    /* According to PR 107807 comment #2, Solaris implements "errno"
       like this:
	 extern int *___errno(void) __attribute__((__const__));
	 #define errno (*(___errno()))
       and macOS like this:
	 extern int * __error(void);
	 #define errno (*__error())
       and similarly __errno for newlib.
       Add these as synonyms for "__errno_location".  */
    kfm.add ("___errno", make_unique<kf_errno_location> ());
    kfm.add ("__error", make_unique<kf_errno_location> ());
    kfm.add ("__errno", make_unique<kf_errno_location> ());
  }

  /* Language-specific support functions.  */
  register_known_functions_lang_cp (kfm);

  /* Some C++ implementations use the std:: copies of these functions
     from <cstdlib> etc for the C spellings of these headers (e.g. <stdlib.h>),
     so we must match against these too.  */
  {
    kfm.add_std_ns ("malloc", make_unique<kf_malloc> ());
    kfm.add_std_ns ("free", make_unique<kf_free> ());
    kfm.add_std_ns ("realloc", make_unique<kf_realloc> ());
    kfm.add_std_ns ("calloc", make_unique<kf_calloc> ());
    kfm.add_std_ns
      ("memcpy",
       make_unique<kf_memcpy_memmove> (kf_memcpy_memmove::KF_MEMCPY));
    kfm.add_std_ns
      ("memmove",
       make_unique<kf_memcpy_memmove> (kf_memcpy_memmove::KF_MEMMOVE));
    kfm.add_std_ns ("memset", make_unique<kf_memset> (false));
    kfm.add_std_ns ("strcat", make_unique<kf_strcat> (2, false));
    kfm.add_std_ns ("strcpy", make_unique<kf_strcpy> (2, false));
    kfm.add_std_ns ("strlen", make_unique<kf_strlen> ());
    kfm.add_std_ns ("strncpy", make_unique<kf_strncpy> ());
    kfm.add_std_ns ("strtok", make_unique<kf_strtok> (rmm));
  }
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
