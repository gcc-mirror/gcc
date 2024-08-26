/* Classic text-based output of diagnostics.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#ifndef GCC_DIAGNOSTIC_FORMAT_TEXT_H
#define GCC_DIAGNOSTIC_FORMAT_TEXT_H

#include "diagnostic-format.h"

/* Subclass of diagnostic_output_format for classic text-based output
   to stderr.

   Uses diagnostic_context.m_text_callbacks to provide client-specific
   textual output (e.g. include paths, macro expansions, etc).  */

class diagnostic_text_output_format : public diagnostic_output_format
{
public:
  diagnostic_text_output_format (diagnostic_context &context)
  : diagnostic_output_format (context)
  {}
  ~diagnostic_text_output_format ();
  void on_begin_group () override {}
  void on_end_group () override {}
  void on_report_diagnostic (const diagnostic_info &,
			     diagnostic_t orig_diag_kind) override;
  void on_diagram (const diagnostic_diagram &diagram) override;
  bool machine_readable_stderr_p () const final override
  {
    return false;
  }

private:
  void print_any_cwe (const diagnostic_info &diagnostic);
  void print_any_rules (const diagnostic_info &diagnostic);
  void print_option_information (const diagnostic_info &diagnostic,
				 diagnostic_t orig_diag_kind);
};

#endif /* ! GCC_DIAGNOSTIC_FORMAT_TEXT_H */
