/* Implementation of class ana::poisoned_value_diagnostic.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

#include "analyzer/region-model.h"
#include "analyzer/feasible-graph.h"
#include "diagnostics/sarif-sink.h"

#if ENABLE_ANALYZER

namespace ana {

// struct poison_finder : public symbolic::node_visitor

bool
poison_finder::visit_svalue (const svalue &sval)
{
  if (!symbolic::node_visitor::visit_svalue (sval))
    return false;
  if (auto *poisoned = sval.dyn_cast_poisoned_svalue ())
    {
      m_found_poison = true;
      m_pkind = poisoned->get_poison_kind ();
    }
  return true;
}

bool
poison_finder::visit_bit_vector (const symbolic::bit_vector &bv)
{
  if (!symbolic::node_visitor::visit_bit_vector (bv))
    return false;
  if (auto *poisoned = bv.dyn_cast_poisoned_bit_vector ())
    {
      m_found_poison = true;
      m_pkind = poisoned->get_poison_kind ();
    }
  return true;
}

/* A subclass of pending_diagnostic for complaining about uses of
   poisoned values.  */

class poisoned_value_diagnostic
: public pending_diagnostic_subclass<poisoned_value_diagnostic>
{
public:
  poisoned_value_diagnostic (tree expr, enum poison_kind pkind,
			     const region *src_region,
			     tree check_expr)
  : m_expr (expr), m_pkind (pkind),
    m_src_region (src_region),
    m_check_expr (check_expr)
  {}

  const char *get_kind () const final override { return "poisoned_value_diagnostic"; }

  bool use_of_uninit_p () const final override
  {
    return m_pkind == poison_kind::uninit;
  }

  bool operator== (const poisoned_value_diagnostic &other) const
  {
    return (m_expr == other.m_expr
	    && m_pkind == other.m_pkind
	    && m_src_region == other.m_src_region);
  }

  int get_controlling_option () const final override
  {
    switch (m_pkind)
      {
      default:
	gcc_unreachable ();
      case poison_kind::uninit:
	return OPT_Wanalyzer_use_of_uninitialized_value;
      case poison_kind::freed:
      case poison_kind::deleted:
	return OPT_Wanalyzer_use_after_free;
      case poison_kind::popped_stack:
	return OPT_Wanalyzer_use_of_pointer_in_stale_stack_frame;
      }
  }

  bool terminate_path_p () const final override { return true; }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    switch (m_pkind)
      {
      default:
	gcc_unreachable ();
      case poison_kind::uninit:
	{
	  ctxt.add_cwe (457); /* "CWE-457: Use of Uninitialized Variable".  */
	  return ctxt.warn ("use of uninitialized value %qE",
			    m_expr);
	}
	break;
      case poison_kind::freed:
	{
	  ctxt.add_cwe (416); /* "CWE-416: Use After Free".  */
	  return ctxt.warn ("use after %<free%> of %qE",
			    m_expr);
	}
	break;
      case poison_kind::deleted:
	{
	  ctxt.add_cwe (416); /* "CWE-416: Use After Free".  */
	  return ctxt.warn ("use after %<delete%> of %qE",
			    m_expr);
	}
	break;
      case poison_kind::popped_stack:
	{
	  /* TODO: which CWE?  */
	  return ctxt.warn
	    ("dereferencing pointer %qE to within stale stack frame",
	     m_expr);
	}
	break;
      }
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    switch (m_pkind)
      {
      default:
	gcc_unreachable ();
      case poison_kind::uninit:
	{
	  pp_printf (&pp,
		     "use of uninitialized value %qE here",
		     m_expr);
	  return true;
	}
      case poison_kind::freed:
	{
	  pp_printf (&pp,
		     "use after %<free%> of %qE here",
		     m_expr);
	  return true;
	}
      case poison_kind::deleted:
	{
	  pp_printf (&pp,
		     "use after %<delete%> of %qE here",
		     m_expr);
	  return true;
	}
      case poison_kind::popped_stack:
	{
	  pp_printf (&pp,
		     "dereferencing pointer %qE to within stale stack frame",
		     m_expr);
	  return true;
	}
      }
  }

  void mark_interesting_stuff (interesting_t *interest) final override
  {
    if (m_src_region)
      {
	interest->add_region_creation (m_src_region);
	interest->add_read_region (m_src_region, "poisoned value");
      }
  }

  /* Attempt to suppress false positives.
     Reject paths where the value of the underlying region isn't poisoned.
     This can happen due to state merging when exploring the exploded graph,
     where the more precise analysis during feasibility analysis finds that
     the region is in fact valid.
     To do this we need to get the value from the fgraph.  Unfortunately
     we can't simply query the state of m_src_region (from the enode),
     since it might be a different region in the fnode state (e.g. with
     heap-allocated regions, the numbering could be different).
     Hence we access m_check_expr, if available.  */

  bool check_valid_fpath_p (const feasible_node &fnode)
    const final override
  {
    if (!m_check_expr)
      return true;
    const svalue *fsval = fnode.get_model ().get_rvalue (m_check_expr, nullptr);
    /* Check to see if the expr is also poisoned in FNODE (and in the
       same way).  */
    poison_finder v;
    fsval->accept_node_visitor (v);
    if (!v.m_found_poison)
      return false;
    if (v.m_pkind != m_pkind)
      return false;
    return true;
  }

  void
  maybe_add_sarif_properties (diagnostics::sarif_object &result_obj)
    const final override
  {
    auto &props = result_obj.get_or_create_properties ();
#define PROPERTY_PREFIX "gcc/analyzer/poisoned_value_diagnostic/"
    props.set (PROPERTY_PREFIX "expr", tree_to_json (m_expr));
    props.set_string (PROPERTY_PREFIX "kind", poison_kind_to_str (m_pkind));
    if (m_src_region)
      props.set (PROPERTY_PREFIX "src_region", m_src_region->to_json ());
    props.set (PROPERTY_PREFIX "check_expr", tree_to_json (m_check_expr));
#undef PROPERTY_PREFIX
  }

private:
  tree m_expr;
  enum poison_kind m_pkind;
  const region *m_src_region;
  tree m_check_expr;
};

std::unique_ptr<pending_diagnostic>
make_poisoned_value_diagnostic (tree expr, enum poison_kind pkind,
				const region *src_region,
				tree check_expr)
{
  return std::make_unique<poisoned_value_diagnostic> (expr, pkind,
						      src_region, check_expr);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
