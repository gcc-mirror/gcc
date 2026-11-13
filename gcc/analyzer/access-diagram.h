/* Text art visualizations within -fanalyzer.
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

#ifndef GCC_ANALYZER_ACCESS_DIAGRAM_H
#define GCC_ANALYZER_ACCESS_DIAGRAM_H

#include "text-art/canvas.h"
#include "text-art/theme.h"
#include "text-art/widget.h"

#include "analyzer/store.h"

namespace ana {

class bit_size_expr
{
public:
  bit_size_expr (const symbolic_bit_size &num_bits) : m_num_bits (num_bits) {}

  std::unique_ptr<text_art::styled_string>
  maybe_get_formatted_str (text_art::style_manager &sm,
			   const region_model &model,
			   const char *concrete_single_bit_fmt,
			   const char *concrete_plural_bits_fmt,
			   const char *concrete_single_byte_fmt,
			   const char *concrete_plural_bytes_fmt,
			   const char *symbolic_bits_fmt,
			   const char *symbolic_bytes_fmt) const;
  bool maybe_print_for_user (pretty_printer *pp,
			     const region_model &model) const;

private:
  const symbolic_bit_size &m_num_bits;
};

struct access_operation
{
  access_operation (const region_model &model,
		    enum access_direction dir,
		    const symbolic::memory_range &attempted_range,
		    const svalue *sval_hint)
  : m_model (model),
    m_dir (dir),
    m_attempted_range (attempted_range),
    m_sval_hint (sval_hint)
  {}

  region_model_manager *get_manager () const
  {
    return m_model.get_manager ();
  }

  /* Get the valid bits to access within the base region.  */
  symbolic_bit_range get_valid_bits () const;

  /* Get the actual bits accessed within the base region.  */
  symbolic_bit_range get_actual_bits () const;

  bool maybe_get_invalid_before_bits (symbolic_bit_range *out) const;
  bool maybe_get_invalid_after_bits (symbolic_bit_range *out) const;

  const region &
  get_base_region () const { return m_attempted_range.get_base_region (); }

  const region_model &m_model;
  enum access_direction m_dir;
  symbolic::memory_range m_attempted_range;
  const svalue *m_sval_hint;
};

class access_diagram : public text_art::wrapper_widget
{
public:
  access_diagram (const access_operation &op,
		  diagnostics::paths::event_id_t region_creation_event_id,
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
