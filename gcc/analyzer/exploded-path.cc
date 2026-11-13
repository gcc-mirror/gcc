/* Paths within an exploded_graph.
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

#include "analyzer/exploded-path.h"

#if ENABLE_ANALYZER

namespace ana {

// struct diagnostic_state

void
diagnostic_state::dump_to_pp (pretty_printer *pp) const
{
  pp_printf (pp, "%s: {", m_debug_desc.c_str ());
  if (m_region_holding_value)
    {
      pp_string (pp, "region holding value: ");
      m_region_holding_value->dump_to_pp (pp, false);
    }
  pp_string (pp, "}");
}

void
diagnostic_state::dump () const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp);
  pp_newline (&pp);
}

/* class exploded_path.  */

/* Look for the last use of SEARCH_STMT within this path.
   If found write the edge's index to *OUT_IDX and return true, otherwise
   return false.  */

bool
exploded_path::find_stmt_backwards (const gimple *search_stmt,
				    int *out_idx) const
{
  for (int i = m_elements.size () - 1; i >= 0; --i)
    {
      const element_t *element = &m_elements[i];
      const exploded_edge *eedge = element->m_eedge;
      if (search_stmt->code == GIMPLE_PHI)
	{
	  /* Each phis_for_edge_op instance handles multiple phi stmts
	     at once, so we have to special-case the search for a phi stmt.  */
	  if (auto op = eedge->maybe_get_op ())
	    if (auto phis_op = op->dyn_cast_phis_for_edge_op ())
	      if (phis_op->defines_ssa_name_p (gimple_phi_result (search_stmt)))
		{
		  *out_idx = i;
		  return true;
		}
	}
      else
	{
	  /* Non-phi stmt.  */
	  if (const gimple *stmt = eedge->maybe_get_stmt ())
	    if (stmt == search_stmt)
	      {
		*out_idx = i;
		return true;
	      }
	}
    }
  return false;
}

/* Get the final exploded_node in this path, which must be non-empty.  */

exploded_node *
exploded_path::get_final_enode () const
{
  gcc_assert (m_elements.size () > 0);
  return m_elements.back ().m_eedge->m_dest;
}

/* Check state along this path, returning true if it is feasible.
   If OUT is non-NULL, and the path is infeasible, write a new
   feasibility_problem to *OUT.  */

bool
exploded_path::feasible_p (logger *logger,
			   std::unique_ptr<feasibility_problem> *out,
			   engine *eng, const exploded_graph *eg) const
{
  LOG_SCOPE (logger);

  feasibility_state state (eng->get_model_manager (),
			   eg->get_supergraph ());

  /* Traverse the path, updating this state.  */
  for (unsigned edge_idx = 0; edge_idx < m_elements.size (); ++edge_idx)
    {
      const exploded_edge *eedge = m_elements[edge_idx].m_eedge;
      if (logger)
	logger->log ("considering edge %i: EN:%i -> EN:%i",
		     edge_idx,
		     eedge->m_src->m_index,
		     eedge->m_dest->m_index);

      std::unique_ptr <rejected_constraint> rc;
      if (!state.maybe_update_for_edge (logger, eedge, nullptr, &rc))
	{
	  gcc_assert (rc);
	  if (out)
	    *out = std::make_unique<feasibility_problem> (edge_idx, *eedge,
							  std::move (rc));
	  return false;
	}

      if (logger)
	{
	  logger->log ("state after edge %i: EN:%i -> EN:%i",
		       edge_idx,
		       eedge->m_src->m_index,
		       eedge->m_dest->m_index);
	  state.get_model ().dump_to_logger (*logger);
	}
    }

  return true;
}

/* Dump this path in multiline form to PP.
   If EXT_STATE is non-NULL, then show the nodes.  */

void
exploded_path::dump_to_pp (pretty_printer *pp,
			   const extrinsic_state *ext_state) const
{
  for (unsigned i = 0; i < m_elements.size (); ++i)
    {
      const element_t &element = m_elements[i];
      const exploded_edge *eedge = element.m_eedge;
      pp_printf (pp, "m_elements[%i]: EN %i -> EN %i",
		 i,
		 eedge->m_src->m_index,
		 eedge->m_dest->m_index);
      if (element.m_state_transition)
	{
	  pp_string (pp, " {");
	  element.m_state_transition->dump_to_pp (pp);
	  pp_string (pp, "}");
	}
      pp_newline (pp);

      if (ext_state)
	eedge->m_dest->dump_to_pp (pp);
    }
}

/* Dump this path in multiline form to FP.  */

void
exploded_path::dump (FILE *fp, const extrinsic_state *ext_state) const
{
  tree_dump_pretty_printer pp (fp);
  dump_to_pp (&pp, ext_state);
}

/* Dump this path in multiline form to stderr.  */

DEBUG_FUNCTION void
exploded_path::dump (const extrinsic_state *ext_state) const
{
  dump (stderr, ext_state);
}

/* Dump this path verbosely to FILENAME.  */

void
exploded_path::dump_to_file (const char *filename,
			     const extrinsic_state &ext_state) const
{
  FILE *fp = fopen (filename, "w");
  if (!fp)
    return;
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp.set_output_stream (fp);
  dump_to_pp (&pp, &ext_state);
  pp_flush (&pp);
  fclose (fp);
}

/* Print a multiline form of this path to LOGGER, prefixing it with DESC.  */

void
exploded_path::maybe_log (logger *logger, const char *desc) const
{
  if (!logger)
    return;
  logger->start_log_line ();
  logger->log_partial ("%s: ", desc);
  logger->end_log_line ();
  for (unsigned idx = 0; idx < m_elements.size (); idx++)
    {
      const exploded_edge &eedge = *m_elements[idx].m_eedge;
      const exploded_node *src_node = eedge.m_src;
      const program_point &src_point = src_node->get_point ();
      const exploded_node *dst_node = eedge.m_dest;
      const program_point &dst_point = dst_node->get_point ();

      pretty_printer *pp = logger->get_printer ();
      logger->start_log_line ();
      pp_printf (pp, "  [%i] EN %i -> EN %i: ",
		 idx,
		 src_node->m_index,
		 dst_node->m_index);
      src_point.print (pp, format (false));
      pp_string (pp, " -> ");
      dst_point.print (pp, format (false));
      if (auto state_trans = m_elements[idx].m_state_transition.get ())
	{
	  pp_string (pp, " {");
	  state_trans->dump_to_pp (pp);
	  pp_string (pp, "}");
	}
      logger->end_log_line ();
    }
}

void
exploded_path::reverse ()
{
  std::reverse (m_elements.begin (), m_elements.end ());
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
