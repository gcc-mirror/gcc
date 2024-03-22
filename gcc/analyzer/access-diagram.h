/* Text art visualizations within -fanalyzer.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_ACCESS_DIAGRAM_H
#define GCC_ANALYZER_ACCESS_DIAGRAM_H

#include "text-art/canvas.h"
#include "text-art/theme.h"
#include "text-art/widget.h"
#include "analyzer/analyzer.h"
#include "analyzer/store.h"

namespace ana {

class bit_size_expr
{
public:
  bit_size_expr () : m_num_bits (NULL) {}
  bit_size_expr (tree num_bits) : m_num_bits (num_bits) {}

  text_art::styled_string
  get_formatted_str (text_art::style_manager &sm,
		     const char *concrete_single_bit_fmt,
		     const char *concrete_plural_bits_fmt,
		     const char *concrete_single_byte_fmt,
		     const char *concrete_plural_bytes_fmt,
		     const char *symbolic_bits_fmt,
		     const char *symbolic_bytes_fmt) const;
  void print (pretty_printer *pp) const;

  tree maybe_get_as_bytes () const;

private:
  tree m_num_bits;
};

/* A range of bits within a base region, where each endpoint
   could be concrete or symbolic (not necessarily the same).  */

struct access_range
{
  access_range ()
  : m_start (), m_next ()
  {
  }
  access_range (region_offset start, region_offset next)
  : m_start (start), m_next (next)
  {}
  access_range (const region *base_region, const bit_range &bits);
  access_range (const region *base_region, const byte_range &bytes);
  access_range (const region &reg, region_model_manager *);

  bool concrete_p () const
  {
    return m_start.concrete_p () && m_next.concrete_p ();
  }

  bool empty_p () const;

  bool get_size (const region_model &model, bit_size_expr *out) const;

  bool get_size_in_bits (bit_size_t *out) const
  {
    if (concrete_p ())
      {
	*out = m_next.get_bit_offset () - m_start.get_bit_offset ();
	return true;
      }
    return false;
  }

  bool as_concrete_bit_range (bit_range *out) const
  {
    if (!concrete_p ())
      return false;
    bit_size_t size = m_next.get_bit_offset () - m_start.get_bit_offset ();
    *out = bit_range (m_start.get_bit_offset (), size);
    return true;
  }
  bool as_concrete_byte_range (byte_range *out) const
  {
    bit_range bits (0, 0);
    if (!as_concrete_bit_range (&bits))
      return false;
    return bits.as_byte_range (out);
  }

  bool contains_p (const access_range &other) const;

  void dump_to_pp (pretty_printer *pp, bool) const;
  void dump (bool) const;
  void log (const char *title, logger &) const;

  region_offset m_start;
  region_offset m_next;
};

struct access_operation
{
  access_operation (const region_model &model,
		    enum access_direction dir,
		    const region &reg,
		    const svalue *sval_hint)
  : m_model (model),
    m_dir (dir),
    m_reg (reg),
    m_sval_hint (sval_hint),
    m_base_region (reg.get_base_region ())
  {}

  region_model_manager *get_manager () const
  {
    return m_model.get_manager ();
  }

  /* Get the valid bits to access within the base region.  */
  access_range get_valid_bits () const;

  /* Get the actual bits accessed within the base region.  */
  access_range get_actual_bits () const;

  bool maybe_get_invalid_before_bits (access_range *out) const;
  bool maybe_get_invalid_after_bits (access_range *out) const;

  const region_model &m_model;
  enum access_direction m_dir;
  const region &m_reg;
  const svalue *m_sval_hint;
  const region *m_base_region;
};

class access_diagram : public text_art::wrapper_widget
{
public:
  access_diagram (const access_operation &op,
		  diagnostic_event_id_t region_creation_event_id,
		  text_art::style_manager &sm,
		  const text_art::theme &theme,
		  logger *logger);
  const char *get_desc () const override
  {
    return "access_diagram";
  }
};

} // namespace ana

#endif /* GCC_ANALYZER_ACCESS_DIAGRAM_H */
