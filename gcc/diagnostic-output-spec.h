/* Support for the DSL of -fdiagnostics-add-output= and
   -fdiagnostics-set-output=.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_DIAGNOSTIC_OUTPUT_SPEC_H
#define GCC_DIAGNOSTIC_OUTPUT_SPEC_H

#include "diagnostic-format.h"
#include "diagnostics/output-file.h"

namespace diagnostics_output_spec {

/* An abstract base class for handling the DSL of -fdiagnostics-add-output=
   and -fdiagnostics-set-output=.  */

class context
{
 public:
  std::unique_ptr<diagnostic_output_format>
  parse_and_make_sink (const char *,
		       diagnostic_context &dc);

  void
  report_error (const char *gmsgid, ...) const
    ATTRIBUTE_GCC_DIAG(2,3);

  void
  report_unknown_key (const char *unparsed_arg,
		      const std::string &key,
		      const std::string &scheme_name,
		      auto_vec<const char *> &known_keys) const;

  void
  report_missing_key (const char *unparsed_arg,
		      const std::string &key,
		      const std::string &scheme_name,
		      const char *metavar) const;

  diagnostics::output_file
  open_output_file (label_text &&filename) const;

  const char *
  get_option_name () const { return m_option_name; }

  line_maps *
  get_affected_location_mgr () const { return m_affected_location_mgr; }

  virtual ~context () {}

  virtual void
  report_error_va (const char *gmsgid, va_list *ap) const = 0;

  virtual const char *
  get_base_filename () const = 0;

protected:
  context (const char *option_name,
	   line_maps *affected_location_mgr)
  : m_option_name (option_name),
    m_affected_location_mgr (affected_location_mgr)
  {
  }

  const char *m_option_name;
  line_maps *m_affected_location_mgr;
};

/* A subclass that implements reporting errors via a diagnostic_context.  */

struct gcc_spec_context : public diagnostics_output_spec::context
{
public:
  gcc_spec_context (diagnostic_context &dc,
		    line_maps *affected_location_mgr,
		    line_maps *control_location_mgr,
		    location_t loc,
		    const char *option_name)
  : context (option_name, affected_location_mgr),
    m_dc (dc),
    m_control_location_mgr (control_location_mgr),
    m_loc (loc)
  {}

  void report_error_va (const char *gmsgid, va_list *ap) const final override
    ATTRIBUTE_GCC_DIAG(2, 0)
  {
    m_dc.begin_group ();
    rich_location richloc (m_control_location_mgr, m_loc);
    m_dc.diagnostic_impl (&richloc, nullptr, -1, gmsgid, ap, DK_ERROR);
    m_dc.end_group ();
  }

  diagnostic_context &m_dc;
  line_maps *m_control_location_mgr;
  location_t m_loc;
};

} // namespace diagnostics_output_spec

#endif
