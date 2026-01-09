/* Presentation tweaks for generating .dot from digraphs for GCC CFGs.
   Copyright (C) 2025 Free Software Foundation, Inc.
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

#define INCLUDE_ALGORITHM
#define INCLUDE_MAP
#define INCLUDE_SET
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "graphviz.h"
#include "xml.h"
#include "xml-printer.h"
#include "diagnostics/digraphs.h"
#include "diagnostics/digraphs-to-dot.h"
#include "diagnostics/sarif-sink.h"
#include "custom-sarif-properties/cfg.h"

#include "selftest.h"

namespace diagnostics {
namespace digraphs {
namespace to_dot {

namespace {
  namespace node_properties = custom_sarif_properties::cfg::node;
  namespace edge_properties = custom_sarif_properties::cfg::edge;
}

class converter_from_cfg : public converter
{
public:
  std::unique_ptr<dot::graph>
  make_dot_graph_from_diagnostic_graph (const digraph &dg) final override
  {
    auto dot_graph
      = converter::make_dot_graph_from_diagnostic_graph (dg);

    /* Add an invisible edge from ENTRY to EXIT, to improve the
       graph layout.  */
    if (const digraphs::node *entry_node = get_entry_node (dg))
      if (const digraphs::node *exit_node = get_exit_node (dg))
	{
	  auto extra_edge_stmt
	    = std::make_unique<dot::edge_stmt>
	    (get_node_id_for_node (*entry_node, "s"),
	     get_node_id_for_node (*exit_node, "n"));
	  extra_edge_stmt->set_attr (dot::id ("style"), dot::id ("invis"));
	  extra_edge_stmt->set_attr (dot::id ("constraint"), dot::id ("true"));
	  dot_graph->m_stmt_list.add_stmt (std::move (extra_edge_stmt));
	}

    return dot_graph;
  }

  void
  add_any_subgraph_attrs (const digraph_node &input_node,
			  dot::subgraph &output_subgraph) final override
  {
    if (const char *kind = input_node.get_property (node_properties::kind))
      {
	if (strcmp (kind, "function") == 0)
	  {
	  }
	else if (strcmp (kind, "loop") == 0)
	  {
	    namespace loop_property_names
	      = custom_sarif_properties::cfg::loop;
	    long num;
	    if (input_node.maybe_get_property (loop_property_names::num, num))
	      {
		pretty_printer pp;
		pp_printf (&pp, "loop %li", num);
		output_subgraph.add_attr (dot::id ("label"),
					  dot::id (pp_formatted_text (&pp)));
	      }
	    long depth;
	    if (input_node.maybe_get_property (loop_property_names::depth,
					       depth))
	      {
		const char *fillcolors[3] = { "grey88", "grey77", "grey66" };
		output_subgraph.add_attr
		  (dot::id ("fillcolor"),
		   dot::id (fillcolors[(depth - 1) % 3]));
	      }
	    output_subgraph.add_attr (dot::id ("style"), dot::id ("filled"));
	    output_subgraph.add_attr (dot::id ("color"), dot::id ("darkgreen"));
	    output_subgraph.add_attr (dot::id ("labeljust"), dot::id ("l"));
	    output_subgraph.add_attr (dot::id ("penwidth"), dot::id ("2"));
	  }
      }
  }

  void
  add_any_node_attrs (const digraph_node &input_node,
		      dot::node_stmt &output_node) final override
  {
    if (const char *node_kind = input_node.get_property (node_properties::kind))
      if (strcmp (node_kind, "basic_block") == 0)
	{
	  namespace bb_property_names
	    = custom_sarif_properties::cfg::basic_block;
	  const char *shape = nullptr;
	  const char *fillcolor = "lightgrey";
	  const char *label = nullptr;
	  if (const char *bb_kind
		= input_node.get_property (bb_property_names::kind))
	    {
	      if (strcmp (bb_kind, "entry") == 0)
		{
		  shape = "Mdiamond";
		  fillcolor = "white";
		  label = "ENTRY";
		}
	      else if (strcmp (bb_kind, "exit") == 0)
		{
		  shape = "Mdiamond";
		  fillcolor = "white";
		  label = "EXIT";
		}
	      else if (strcmp (bb_kind, "hot") == 0)
		fillcolor = "lightpink";
	      else if (strcmp (bb_kind, "cold") == 0)
		fillcolor = "lightblue";
	    }

	  if (shape)
	    output_node.set_attr (dot::id ("shape"), dot::id (shape));
	  else
	    {
	      output_node.set_attr (dot::id ("shape"), dot::id ("none"));
	      output_node.set_attr (dot::id ("margin"), dot::id ("0"));
	    }
	  output_node.set_attr (dot::id ("fillcolor"), dot::id (fillcolor));
	  if (label)
	    output_node.set_label (dot::id (label));
	  else
	    {
	      // Create node with table
	      xml::element table ("table", false);
	      xml::printer xp (table);
	      xp.set_attr ("border", "0");
	      xp.set_attr ("cellborder", "1");
	      xp.set_attr ("cellspacing", "0");

	      long bb_index;
	      if (input_node.maybe_get_property (bb_property_names::index,
						 bb_index))
		{
		  xp.push_tag ("tr", true);
		  xp.push_tag ("td", true);
		  xp.set_attr ("align", "left");
		  pretty_printer pp;
		  pp_printf (&pp, "<bb %li>:", bb_index);
		  xp.add_text_from_pp (pp);
		  xp.pop_tag ("td");
		  xp.pop_tag ("tr");
		}

	      if (json::array *arr
		  = input_node.get_property (bb_property_names::gimple::phis))
		print_rows_for_strings (*arr, xp);

	      if (json::array *arr
		    = input_node.get_property
			(bb_property_names::gimple::stmts))
		print_rows_for_strings (*arr, xp);

	      if (json::array *arr
		    = input_node.get_property (bb_property_names::rtl::insns))
		print_rows_for_strings (*arr, xp);

	      // xml must be done by now

	      output_node.m_attrs.add (dot::id ("label"),
				       dot::id (table));
	    }
	}
  }

  virtual void
  add_any_edge_attrs (const digraph_edge &input_edge,
		      dot::edge_stmt &output_edge) final override
  {
    namespace edge_properties = custom_sarif_properties::cfg::edge;

    const char *style = "solid,bold";
    const char *color = "black";
    int weight = 10;

    if (edge_has_flag (input_edge, "FAKE"))
      {
	style = "dotted";
	color = "green";
	weight = 0;
      }
    if (edge_has_flag (input_edge, "DFS_BACK"))
      {
	style = "dotted,bold";
	color = "blue";
	weight = 10;
      }
    else if (edge_has_flag (input_edge, "FALLTHRU"))
      weight = 100;
    else if (edge_has_flag (input_edge, "TRUE_VALUE"))
      color = "forestgreen";
    else if (edge_has_flag (input_edge, "FALSE_VALUE"))
      color = "darkorange";

    if (edge_has_flag (input_edge, "ABNORMAL"))
      color = "red";

    output_edge.set_attr (dot::id ("style"), dot::id (style));
    output_edge.set_attr (dot::id ("color"), dot::id (color));
    output_edge.set_attr (dot::id ("weight"),
			  dot::id (std::to_string (weight)));
    output_edge.set_attr (dot::id ("constraint"),
			  dot::id ((edge_has_flag (input_edge, "FAKE")
				    || edge_has_flag (input_edge, "DFS_BACK"))
				   ? "false" : "true"));

    long probability_pc;
    if (input_edge.maybe_get_property (edge_properties::probability_pc,
				       probability_pc))
      {
	pretty_printer pp;
	pp_printf (&pp, "[%li%%]", probability_pc);
	output_edge.set_label (dot::id (pp_formatted_text (&pp)));
      }
  }

  private:
    bool
    edge_has_flag (const digraph_edge &input_edge,
		   const char *flag_name) const
    {
      auto flags = input_edge.get_property (edge_properties::flags);
      for (auto iter : *flags)
	if (auto str = iter->dyn_cast_string ())
	  if (!strcmp (flag_name, str->get_string ()))
	    return true;
      return false;
    }

    void
    print_rows_for_strings (json::array &arr,
			    xml::printer &xp)
    {
      for (auto iter : arr)
	if (auto js_str = iter->dyn_cast_string ())
	  {
	    xp.push_tag ("tr", true);
	    xp.push_tag ("td", true);
	    xp.set_attr ("align", "left");
	    xp.add_text (js_str->get_string ());
	    xp.pop_tag ("td");
	    xp.pop_tag ("tr");
	  }
    }

  const node *
  get_bb_node_by_kind (const digraph &dg, const char *kind) const
  {
    for (auto &iter : dg.get_all_nodes ())
      {
	const node &input_node = *iter.second;
	if (const char *node_kind = input_node.get_property (node_properties::kind))
	  if (strcmp (node_kind, "basic_block") == 0)
	    {
	      namespace bb_property_names
		= custom_sarif_properties::cfg::basic_block;
	      if (const char *bb_kind
		  = input_node.get_property (bb_property_names::kind))
		{
		  if (strcmp (bb_kind, kind) == 0)
		    return &input_node;
		}
	    }
      }
    return nullptr;
  }

  const node *
  get_entry_node (const digraph &dg) const
  {
    return get_bb_node_by_kind (dg, "entry");
  }

  const node *
  get_exit_node (const digraph &dg) const
  {
    return get_bb_node_by_kind (dg, "exit");
  }
};

std::unique_ptr<converter>
make_converter_from_cfg ()
{
  return std::make_unique<converter_from_cfg> ();
}

} // namespace to_dot
} // namespace digraphs
} // namespace diagnostics
