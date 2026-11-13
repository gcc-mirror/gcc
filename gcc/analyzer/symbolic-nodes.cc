/* Base class for svalues and regions.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.
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

#include "selftest.h"

#include "diagnostics/markup.h"

#include "analyzer/symbolic-nodes.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/region-model.h"
#include "analyzer/region-model-manager.h"
#include "analyzer/symbolic-scalars.h"
#include "analyzer/symbolic-bit-vectors.h"
#include "analyzer/analyzer-selftests.h"

#if ENABLE_ANALYZER

namespace ana {

const char *
baseline_state_to_str (baseline_state bs)
{
  switch (bs)
    {
    default:
      gcc_unreachable ();
    case baseline_state::main:
      return "main";
    case baseline_state::after_main:
      return "after_main";
    }
}

/* Compare N1 and N2 by id.  */

int
symbolic::node::cmp_ids (const symbolic::node *n1, const symbolic::node *n2)
{
  return (int)n1->get_id () - (int)n2->get_id ();
}

/* Comparator for use by vec<const symbol *>::qsort.  */

int
symbolic::node::cmp_ptr_ptr (const void *p1, const void *p2)
{
  const symbolic::node *n1 = *(const symbolic::node * const *)p1;
  const symbolic::node *n2 = *(const symbolic::node * const *)p2;
  return cmp_ids (n1, n2);
}

void
symbolic::node::dump_to_pp (pretty_printer *pp,
			    bool show_ids,
			    bool elide_repeated_nodes) const
{
  text_art::theme *theme = global_dc->get_diagram_theme ();
  symbolic::dump_node_tree_to_pp<symbolic::node> (*this, theme, pp,
						  show_ids,
						  elide_repeated_nodes);
}

DEBUG_FUNCTION void
symbolic::node::dump () const
{
  symbolic::dump_node_tree_to_stderr<symbolic::node> (*this);
}

void
symbolic::node::dump_to_logger (logger &log) const
{
  symbolic::dump_node_tree_to_logger<symbolic::node> (*this, log);
}

/* Use NDS to create a text_art::widget describing this node in
   a tree-like form, using PREFIX as a prefix (e.g. for field names).
   We do this via two vfuncs:
   (a) print_dump_widget_label, to populate the text of a tree_widget, and
   (b) add_dump_widget_children, to add children to the tree_widget.  */

std::unique_ptr<text_art::tree_widget>
symbolic::node::make_dump_widget (node_dump_state &nds,
				  const char *prefix) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = true;

  if (prefix)
    pp_printf (&pp, "%s: ", prefix);

  if (nds.m_show_ids)
    {
      start_any_dump_color (&pp);
      pp_printf (&pp, "(%i): ", get_id ());
      end_any_dump_color (&pp);
    }

  print_dump_widget_label (&pp);

  nds.m_tree_style_id = nds.m_sm.get_or_create_id (get_text_art_tree_style ());
  std::unique_ptr<text_art::tree_widget> w
    (text_art::tree_widget::make (nds, &pp));

  if (nds.m_elide_repeated_nodes && nds.m_nodes_seen.contains (this))
    {
      if (get_complexity ().m_num_nodes > 1)
	{
	  pretty_printer complexity_pp;
	  pp_printf (&complexity_pp, "{as above; descendents: %u, depth: %u}",
		     get_complexity ().m_num_nodes,
		     get_complexity ().m_max_depth);
	  w->add_child (text_art::tree_widget::make (nds, &complexity_pp));
	}
    }
  else
    {
      add_dump_widget_children (*w, nds);
      if (nds.m_elide_repeated_nodes)
	nds.m_nodes_seen.add (this);
    }

  return w;
}

std::unique_ptr<json::object>
symbolic::node::make_json_obj (const char *type_name) const
{
  auto obj = std::make_unique<json::object> ();
  obj->set_integer ("id", get_id ());
  obj->set_string ("kind", type_name);
  return obj;
}

void
symbolic::node::start_any_dump_color (pretty_printer *pp) const
{
  if (pp_show_color (pp))
    if (const char *dump_color = get_any_dump_color ())
      pp_string (pp, colorize_start (true, dump_color));
}

void
symbolic::node::end_any_dump_color (pretty_printer *pp) const
{
  if (pp_show_color (pp))
    if (get_any_dump_color ())
      pp_string (pp, colorize_stop (true));
}

text_art::style
symbolic::node::get_text_art_tree_style () const
{
  if (const char *dump_color = get_any_dump_color ())
    return text_art::get_style_from_color_cap_name (dump_color);
  else
    return text_art::get_style_from_color_cap_name ("note");
}

/* Return true iff this node is defined in terms of OTHER
   (or *is* OTHER).  */

bool
symbolic::node::involves_p (const symbolic::node &other) const
{
  /* Subclass of visitor for use in implementing symbolic::node::involves_p.  */

  class involvement_visitor : public symbolic::node_visitor
  {
  public:
    involvement_visitor (const symbolic::node &needle)
    : m_needle (needle), m_found (false) {}

    bool
    visit_node (const symbolic::node &candidate) final override
    {
      if (&candidate == &m_needle)
	m_found = true;
      return true;
    }

    bool found_p () const { return m_found; }

  private:
    const symbolic::node &m_needle;
    bool m_found;
  };

  if (&other == this)
    return true;

  involvement_visitor v (other);
  accept_node_visitor (v);
  return v.found_p ();
}

// class value_node : public node

/* Base implementation of value_node::implicitly_live_p.  */

bool
symbolic::value_node::implicitly_live_p (symbolic::liveness_context &) const
{
  return false;
}

// class pp_element_for_node : public pp_element

void
symbolic::pp_element_for_node::add_to_phase_2 (pp_markup::context &ctxt)
{
  markup_context markup_ctxt (nullptr, m_user_facing);
  auto markup = m_node.make_markup_node (markup_ctxt);
  diagnostics::markup::lowering_options opts;
  auto lowered = markup->lower (opts);
  lowered->print_to_pp (&ctxt.m_pp);
}

// class node_set : public std::set<const NodeClass *>

template <typename NodeClass>
bool
symbolic::node_set<NodeClass>::contains (const NodeClass *n) const
{
  return this->find (n) != this->end ();
}

template
bool
symbolic::node_set<symbolic::value_node>::contains (const symbolic::value_node *n) const;

template <typename NodeClass>
std::unique_ptr<text_art::tree_widget>
symbolic::node_set<NodeClass>::make_dump_widget (node_dump_state &nds,
						 const char *prefix) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = true;

  if (prefix)
    pp_printf (&pp, "%s: ", prefix);

  pp_printf (&pp, "%zu element(s)", this->size ());

  std::unique_ptr<text_art::tree_widget> w
    (text_art::tree_widget::make (nds, &pp));

  for (auto iter : *this)
    w->add_child (iter->make_dump_widget (nds));

  return w;
}

template <typename NodeClass>
void
symbolic::node_set<NodeClass>::dump_to_pp (pretty_printer *pp,
					   bool show_ids,
					   bool elide_repeated_nodes) const
{
  text_art::theme *theme = global_dc->get_diagram_theme ();
  symbolic::dump_node_tree_to_pp<symbolic::node_set<NodeClass>>
    (*this, theme, pp, show_ids, elide_repeated_nodes);
}

template
void
symbolic::node_set<symbolic::value_node>::dump_to_pp (pretty_printer *pp,
						      bool show_ids,
						      bool elide_repeated_nodes) const;

template <typename NodeClass>
void
symbolic::node_set<NodeClass>::dump () const
{
  symbolic::dump_node_tree_to_stderr<symbolic::node_set<NodeClass>> (*this);
}

template
void
symbolic::node_set<symbolic::value_node>::dump () const;

// class node_manager

symbolic::node_manager::node_manager ()
: m_next_node_id (0),
  m_scalar_managers (std::make_unique<symbolic::scalar_managers> (*this)),
  m_rmm (std::make_unique<region_model_manager> (*this)),
  m_bit_contents_manager
    (std::make_unique<symbolic::bit_contents_manager> (*this)),
  m_bit_vector_manager (std::make_unique<symbolic::bit_vector_manager> (*this))
{
}

symbolic::node_manager::~node_manager () = default;

/* Dump the various managers to LOGGER.
   If SHOW_OBJS is true, also dump the objects themselves.  */

void
symbolic::node_manager::log_stats (logger *logger, bool show_objs) const
{
  LOG_SCOPE (logger);

  m_rmm->log_stats (logger, show_objs);

  // TODO: log other managers
}

// class node_visitor

bool
symbolic::node_visitor::visit_bit_contents (const symbolic::bit_contents &n)
{
  return visit_node (n);
}

bool
symbolic::node_visitor::visit_bit_vector (const symbolic::bit_vector &n)
{
  return visit_node (n);
}

bool
symbolic::node_visitor::visit_region (const region &n)
{
  return visit_node (n);
}

bool
symbolic::node_visitor::
visit_scalar_unitless (const symbolic::scalar<integer> &n)
{
  return visit_node (n);
}

bool
symbolic::node_visitor::
visit_scalar_bit (const symbolic::scalar<integer_with_unit<units::bit>> &n)
{
  return visit_node (n);
}

bool
symbolic::node_visitor::
visit_scalar_byte (const symbolic::scalar<integer_with_unit<units::byte>> &n)
{
  return visit_node (n);
}

bool
symbolic::node_visitor::visit_svalue (const svalue &n)
{
  return visit_node (n);
}

// class value_node_visitor : public node_visitor

bool
symbolic::value_node_visitor::
visit_bit_vector (const symbolic::bit_vector &bv)
{
  if (!symbolic::node_visitor::visit_bit_vector (bv))
    return false;
  return (m_cb)(bv, m_user_data);
}

bool
symbolic::value_node_visitor::visit_svalue (const svalue &sval)
{
  if (!symbolic::node_visitor::visit_svalue (sval))
    return false;
  return (m_cb)(sval, m_user_data);
}

#if CHECKING_P

namespace selftest {

/* Implementation detail of ASSERT_SYMBOLIC_NODE_DUMP_EQ.  */

void
assert_symbolic_node_dump_eq (const ::selftest::location &loc,
			      const symbolic::node &node,
			      const char *expected_str)
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;

  text_art::ascii_theme theme;
  symbolic::dump_node_tree_to_pp<symbolic::node> (node, &theme, &pp,
						  false, false);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected_str);
}

/* Implementation detail of ASSERT_SAME_SYMBOLIC_NODE.  */

void
assert_same_symbolic_node (const ::selftest::location &loc,
			   const char *desc_lhs,
			   const char *desc_rhs,
			   const symbolic::node *lhs,
			   const symbolic::node *rhs)
{
  if (lhs != rhs)
    {
      pretty_printer pp;
      pp.set_output_stream (stderr);

      pp_string (&pp, "lhs:");
      pp_newline (&pp);
      if (lhs)
	lhs->dump_to_pp (&pp, true);
      else
	{
	  pp_string (&pp, "  nullptr");
	  pp_newline (&pp);
	}

      pp_string (&pp, "rhs:");
      pp_newline (&pp);
      if (rhs)
	rhs->dump_to_pp (&pp, true);
      else
	{
	  pp_string (&pp, "  nullptr");
	  pp_newline (&pp);
	}

      pp_flush (&pp);

      ::selftest::fail_formatted
	  (loc,
	   "got different symbolic::nodes for:\n"
	   "  lhs: %s\n"
	   "  rhs: %s\n",
	   desc_lhs, desc_rhs);
    }
}

/* Implementation detail of ASSERT_DIFFERENT_SYMBOLIC_NODE.  */

void
assert_different_symbolic_node (const ::selftest::location &loc,
				const char *desc_lhs,
				const char *desc_rhs,
				const symbolic::node *lhs,
				const symbolic::node *rhs)
{
  if (&lhs == &rhs)
    {
      pretty_printer pp;
      pp.set_output_stream (stderr);

      pp_string (&pp, "lhs:");
      pp_newline (&pp);
      if (lhs)
	lhs->dump_to_pp (&pp, true);
      else
	{
	  pp_string (&pp, "  nullptr");
	  pp_newline (&pp);
	}

      pp_string (&pp, "rhs:");
      pp_newline (&pp);
      if (rhs)
	rhs->dump_to_pp (&pp, true);
      else
	{
	  pp_string (&pp, "  nullptr");
	  pp_newline (&pp);
	}

      pp_flush (&pp);

      ::selftest::fail_formatted
	  (loc,
	   "got same symbolic_node for:\n"
	   "  lhs: %s\n"
	   "  rhs: %s\n",
	   desc_lhs, desc_rhs);
    }
}

/* Implementation detail of ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ.  */

void
assert_symbolic_node_markup_text_eq (const ::selftest::location &loc,
				     const symbolic::node &sym_node,
				     const diagnostics::markup::lowering_options &opts,
				     const char *expected_str)
{
  symbolic::markup_context markup_ctxt (nullptr, true);
  auto markup = sym_node.make_markup_node (markup_ctxt);
  ASSERT_MARKUP_TEXT_EQ_AT (loc, *markup, opts, expected_str);
}

static void
test_node_visitor ()
{
  struct recorder : public ana::symbolic::node_visitor
  {
    bool
    visit_node (const ana::symbolic::node &n)
    {
      m_nodes_visited.push_back (&n);
      return true;
    }

    std::vector<const ana::symbolic::node *> m_nodes_visited;
  };

  symbolic::node_manager node_mgr;
  auto &bv_mgr = node_mgr.get_bit_vector_manager ();
  auto &unknown_32_bits = bv_mgr.get_unknown_bits (32_bits);

  recorder r;
  unknown_32_bits.accept_node_visitor (r);
  ASSERT_SAME_SYMBOLIC_NODE (r.m_nodes_visited[0], &unknown_32_bits);
  // FIXME: we also ought to visit the size, but we don't currently
}

/* Run all of the selftests within this file.  */

void
analyzer_symbolic_nodes_cc_tests ()
{
  test_node_visitor ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
