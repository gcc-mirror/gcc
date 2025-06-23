/* XML documents for dumping state in an easier-to-read form.
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

#ifndef GCC_ANALYZER_ANA_STATE_TO_XML_STATE_H
#define GCC_ANALYZER_ANA_STATE_TO_XML_STATE_H

#include "xml.h"

namespace ana {

class xml_state : public xml::document
{
public:
  xml_state (const program_state &state,
	     const extrinsic_state &ext_state);

  xml::element &
  get_or_create_element (const region &reg);

private:
  xml::element&
  create_and_add_element (const region &reg);

  static std::unique_ptr<xml::element>
  make_memory_space_element (const char *label);

  std::unique_ptr<xml::element>
  create_element (const region &reg);

  /* Spatially sorted concrete bindings.  */
  typedef std::map<bit_range, const svalue *> concrete_bindings_t;

  void
  create_elements_for_binding_cluster (const binding_cluster &cluster,
				       bool create_all);

  std::unique_ptr<xml::element>
  create_element_for_conc_bindings (const concrete_bindings_t &conc_bindings);

  // Try to get the bit_range of REG within its base region
  bool
  get_bit_range_within_base_region (const region &reg,
				    bit_range &out);

  void
  populate_element_for_typed_region (xml::element &e,
				     const region &reg,
				     const concrete_bindings_t &conc_bindings,
				     bool create_all);

  void
  set_attr_for_dynamic_extents (const region &reg, xml::element &e);

  bool
  show_child_element_for_child_region_p (const region &reg,
					 const concrete_bindings_t &conc_bindings,
					 bool create_all);

  std::unique_ptr<xml::element>
  create_element_for_svalue (const svalue *sval);

  const program_state &m_state;
  const extrinsic_state &m_ext_state;
  region_model_manager &m_mgr;
  xml::element *m_root;
  std::map<const region *, xml::element *> m_region_to_element_map;
  std::map<const region *, tree> m_types_for_untyped_regions;
};

} // namespace ana

#endif /* GCC_ANALYZER_ANA_STATE_TO_XML_STATE_H */
