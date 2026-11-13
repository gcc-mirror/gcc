/* A bundle of information for use when determining if a symbolic::value_node
   is implicitly live.

   Copyright (C) 2026 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_LIVENESS_CONTEXT
#define GCC_ANALYZER_LIVENESS_CONTEXT

namespace ana {
namespace symbolic {

/* A bundle of information for use when determining if a symbolic::value_node
   is implicitly live.  */

class liveness_context
{
public:
  liveness_context (const value_set &known_live_values,
		    const region_model &model);

  bool node_live_p (const value_node &n);
  bool bits_live_p (const bit_vector_from_ubc &);

  const region_model *get_model () const { return &m_model; }

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (symbolic::node_dump_state &nds) const;

  void dump_to_pp (pretty_printer *pp) const;

  DEBUG_FUNCTION void dump () const;

  bool known_live_ptr_to_base_region_p (const region &base_reg) const;

private:
  void
  set_known_liveness_for_node (const symbolic::value_node &n,
			       bool is_live);

  std::map<const symbolic::value_node *, bool> m_known_liveness;

  /* A set of base regions for which m_known_liveness has a live
     pointer_svalue pointing to that region, so that we can then treat
     (T*)ptr as being implicitly live for all T.  */
  std::set<const region *> m_known_base_regions;

  const region_model &m_model;
};

} // namespace ana::symbolic
} // namespace ana

#endif /* GCC_ANALYZER_LIVENESS_CONTEXT */
