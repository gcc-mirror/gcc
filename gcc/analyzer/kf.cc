/* Handling for the known behavior of various specific functions.
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

/* Implementations of specific functions.  */

/* Handler for "alloca".  */

class kf_alloca : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 1;
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

class kf_calloc : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 2
	    && cd.arg_is_size_p (0)
	    && cd.arg_is_size_p (1));
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
  model->zero_fill_region (sized_reg);
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
}

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

class kf_free : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 0 && cd.arg_is_pointer_p (0));
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

class kf_malloc : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1
	    && cd.arg_is_size_p (0));
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
/* TODO: complain about overlapping src and dest for the memcpy
   variants.  */

class kf_memcpy_memmove : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 3
	    && cd.arg_is_pointer_p (0)
	    && cd.arg_is_pointer_p (1)
	    && cd.arg_is_size_p (2));
  }
  void impl_call_pre (const call_details &cd) const final override;
};

void
kf_memcpy_memmove::impl_call_pre (const call_details &cd) const
{
  const svalue *dest_ptr_sval = cd.get_arg_svalue (0);
  const svalue *src_ptr_sval = cd.get_arg_svalue (1);
  const svalue *num_bytes_sval = cd.get_arg_svalue (2);

  region_model *model = cd.get_model ();
  region_model_manager *mgr = cd.get_manager ();

  const region *dest_reg
    = model->deref_rvalue (dest_ptr_sval, cd.get_arg_tree (0), cd.get_ctxt ());
  const region *src_reg
    = model->deref_rvalue (src_ptr_sval, cd.get_arg_tree (1), cd.get_ctxt ());

  cd.maybe_set_lhs (dest_ptr_sval);

  const region *sized_src_reg
    = mgr->get_sized_region (src_reg, NULL_TREE, num_bytes_sval);
  const region *sized_dest_reg
    = mgr->get_sized_region (dest_reg, NULL_TREE, num_bytes_sval);
  const svalue *src_contents_sval
    = model->get_store_value (sized_src_reg, cd.get_ctxt ());
  model->check_for_poison (src_contents_sval, cd.get_arg_tree (1),
			   sized_src_reg, cd.get_ctxt ());
  model->set_value (sized_dest_reg, src_contents_sval, cd.get_ctxt ());
}

/* Handler for "memset" and "__builtin_memset".  */

class kf_memset : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 3 && cd.arg_is_pointer_p (0));
  }

  void impl_call_pre (const call_details &cd) const final override;
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
  model->check_region_for_write (sized_dest_reg, cd.get_ctxt ());
  model->fill_region (sized_dest_reg, fill_value_u8);
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

  bool emit (rich_location *rich_loc) final override
  {
    auto_diagnostic_group d;
    diagnostic_metadata m;

    /* SEI CERT C Coding Standard: "POS34-C. Do not call putenv() with a
       pointer to an automatic variable as the argument".  */
    diagnostic_metadata::precanned_rule
      rule ("POS34-C", "https://wiki.sei.cmu.edu/confluence/x/6NYxBQ");
    m.add_rule (rule);

    bool warned;
    if (m_var_decl)
      warned = warning_meta (rich_loc, m, get_controlling_option (),
			     "%qE on a pointer to automatic variable %qE",
			     m_fndecl, m_var_decl);
    else
      warned = warning_meta (rich_loc, m, get_controlling_option (),
			     "%qE on a pointer to an on-stack buffer",
			     m_fndecl);
    if (warned)
      {
	if (m_var_decl)
	  inform (DECL_SOURCE_LOCATION (m_var_decl),
		  "%qE declared on stack here", m_var_decl);
	inform (rich_loc->get_loc (), "perhaps use %qs rather than %qE",
		"setenv", m_fndecl);
      }

    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_var_decl)
      return ev.formatted_print ("%qE on a pointer to automatic variable %qE",
				 m_fndecl, m_var_decl);
    else
      return ev.formatted_print ("%qE on a pointer to an on-stack buffer",
				 m_fndecl);
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

class kf_realloc : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 2
	    && cd.arg_is_pointer_p (0)
	    && cd.arg_is_size_p (1));
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

    label_text get_desc (bool can_colorize) const final override
    {
      return make_label_text (can_colorize,
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

    label_text get_desc (bool can_colorize) const final override
    {
      return make_label_text (can_colorize,
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

class kf_strchr : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 2 && cd.arg_is_pointer_p (0));
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

    label_text get_desc (bool can_colorize) const final override
    {
      if (m_found)
	return make_label_text (can_colorize,
				"when %qE returns non-NULL",
				get_fndecl ());
      else
	return make_label_text (can_colorize,
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

class kf_sprintf : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () >= 2
	    && cd.arg_is_pointer_p (0)
	    && cd.arg_is_pointer_p (1));
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
  }
};

/* Handler for "__builtin_stack_restore".  */

class kf_stack_restore : public known_function
{
public:
  bool matches_call_types_p (const call_details &) const final override
  {
    return true;
  }

  /* Currently a no-op.  */
};

/* Handler for "__builtin_stack_save".  */

class kf_stack_save : public known_function
{
public:
  bool matches_call_types_p (const call_details &) const final override
  {
    return true;
  }

  /* Currently a no-op.  */
};

/* Handler for "strcpy" and "__builtin_strcpy_chk".  */

class kf_strcpy : public known_function
{
public:
  kf_strcpy (unsigned int num_args) : m_num_args (num_args) {}
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == m_num_args
	    && cd.arg_is_pointer_p (0)
	    && cd.arg_is_pointer_p (1));
  }

  void impl_call_pre (const call_details &cd) const final override;

private:
  unsigned int m_num_args;
};

void
kf_strcpy::impl_call_pre (const call_details &cd) const
{
  region_model *model = cd.get_model ();
  region_model_manager *mgr = cd.get_manager ();

  const svalue *dest_sval = cd.get_arg_svalue (0);
  const region *dest_reg = model->deref_rvalue (dest_sval, cd.get_arg_tree (0),
					 cd.get_ctxt ());
  const svalue *src_sval = cd.get_arg_svalue (1);
  const region *src_reg = model->deref_rvalue (src_sval, cd.get_arg_tree (1),
					cd.get_ctxt ());
  const svalue *src_contents_sval = model->get_store_value (src_reg,
							    cd.get_ctxt ());

  cd.maybe_set_lhs (dest_sval);

  /* Try to get the string size if SRC_REG is a string_region.  */
  const svalue *copied_bytes_sval = model->get_string_size (src_reg);
  /* Otherwise, check if the contents of SRC_REG is a string.  */
  if (copied_bytes_sval->get_kind () == SK_UNKNOWN)
    copied_bytes_sval = model->get_string_size (src_contents_sval);

  const region *sized_dest_reg
    = mgr->get_sized_region (dest_reg, NULL_TREE, copied_bytes_sval);
  model->set_value (sized_dest_reg, src_contents_sval, cd.get_ctxt ());
}

/* Handler for "strdup" and "__builtin_strdup".  */

class kf_strdup : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1 && cd.arg_is_pointer_p (0));
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

/* Handle the on_call_pre part of "strlen".  */

class kf_strlen : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1 && cd.arg_is_pointer_p (0));
  }
  void impl_call_pre (const call_details &cd) const final override;
};

void
kf_strlen::impl_call_pre (const call_details &cd) const
{
  region_model_context *ctxt = cd.get_ctxt ();
  region_model *model = cd.get_model ();
  region_model_manager *mgr = cd.get_manager ();

  const svalue *arg_sval = cd.get_arg_svalue (0);
  const region *buf_reg
    = model->deref_rvalue (arg_sval, cd.get_arg_tree (0), ctxt);
  if (const string_region *str_reg
      = buf_reg->dyn_cast_string_region ())
    {
      tree str_cst = str_reg->get_string_cst ();
      /* TREE_STRING_LENGTH is sizeof, not strlen.  */
      int sizeof_cst = TREE_STRING_LENGTH (str_cst);
      int strlen_cst = sizeof_cst - 1;
      if (cd.get_lhs_type ())
	{
	  tree t_cst = build_int_cst (cd.get_lhs_type (), strlen_cst);
	  const svalue *result_sval
	    = mgr->get_or_create_constant_svalue (t_cst);
	  cd.maybe_set_lhs (result_sval);
	  return;
	}
    }
  /* Otherwise a conjured value.  */
}

/* Handler for "strndup" and "__builtin_strndup".  */

class kf_strndup : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 2 && cd.arg_is_pointer_p (0));
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

/* Populate KFM with instances of known functions supported by the core of the
   analyzer (as opposed to plugins).  */

void
register_known_functions (known_function_manager &kfm)
{
  /* Debugging/test support functions, all  with a "__analyzer_" prefix.  */
  register_known_analyzer_functions (kfm);

  /* Internal fns the analyzer has known_functions for.  */
  {
    kfm.add (IFN_BUILTIN_EXPECT, make_unique<kf_expect> ());
    kfm.add (IFN_UBSAN_BOUNDS, make_unique<kf_ubsan_bounds> ());
  }

  /* Built-ins the analyzer has known_functions for.  */
  {
    kfm.add (BUILT_IN_ALLOCA, make_unique<kf_alloca> ());
    kfm.add (BUILT_IN_ALLOCA_WITH_ALIGN, make_unique<kf_alloca> ());
    kfm.add (BUILT_IN_CALLOC, make_unique<kf_calloc> ());
    kfm.add (BUILT_IN_EXPECT, make_unique<kf_expect> ());
    kfm.add (BUILT_IN_EXPECT_WITH_PROBABILITY, make_unique<kf_expect> ());
    kfm.add (BUILT_IN_FREE, make_unique<kf_free> ());
    kfm.add (BUILT_IN_MALLOC, make_unique<kf_malloc> ());
    kfm.add (BUILT_IN_MEMCPY, make_unique<kf_memcpy_memmove> ());
    kfm.add (BUILT_IN_MEMCPY_CHK, make_unique<kf_memcpy_memmove> ());
    kfm.add (BUILT_IN_MEMMOVE, make_unique<kf_memcpy_memmove> ());
    kfm.add (BUILT_IN_MEMMOVE_CHK, make_unique<kf_memcpy_memmove> ());
    kfm.add (BUILT_IN_MEMSET, make_unique<kf_memset> ());
    kfm.add (BUILT_IN_MEMSET_CHK, make_unique<kf_memset> ());
    kfm.add (BUILT_IN_REALLOC, make_unique<kf_realloc> ());
    kfm.add (BUILT_IN_SPRINTF, make_unique<kf_sprintf> ());
    kfm.add (BUILT_IN_STACK_RESTORE, make_unique<kf_stack_restore> ());
    kfm.add (BUILT_IN_STACK_SAVE, make_unique<kf_stack_save> ());
    kfm.add (BUILT_IN_STRCHR, make_unique<kf_strchr> ());
    kfm.add (BUILT_IN_STRCPY, make_unique<kf_strcpy> (2));
    kfm.add (BUILT_IN_STRCPY_CHK, make_unique<kf_strcpy> (3));
    kfm.add (BUILT_IN_STRDUP, make_unique<kf_strdup> ());
    kfm.add (BUILT_IN_STRNDUP, make_unique<kf_strndup> ());
    kfm.add (BUILT_IN_STRLEN, make_unique<kf_strlen> ());

    register_varargs_builtins (kfm);
  }

  /* Known builtins and C standard library functions.  */
  {
    kfm.add ("memset", make_unique<kf_memset> ());
    kfm.add ("strdup", make_unique<kf_strdup> ());
    kfm.add ("strndup", make_unique<kf_strndup> ());
  }

  /* Known POSIX functions, and some non-standard extensions.  */
  {
    kfm.add ("putenv", make_unique<kf_putenv> ());

    register_known_fd_functions (kfm);
    register_known_file_functions (kfm);
  }

  /* glibc functions.  */
  {
    kfm.add ("__errno_location", make_unique<kf_errno_location> ());
    kfm.add ("error", make_unique<kf_error> (3));
    kfm.add ("error_at_line", make_unique<kf_error> (5));
  }

  /* Other implementations of C standard library.  */
  {
    /* According to PR 107807 comment #2, Solaris implements "errno"
       like this:
	 extern int *___errno(void) __attribute__((__const__));
	 #define errno (*(___errno()))
       and OS X like this:
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
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
