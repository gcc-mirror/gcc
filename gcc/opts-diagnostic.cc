/* Support for -fdiagnostics-add-output= and -fdiagnostics-set-output=.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.

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


/* This file implements the options -fdiagnostics-add-output=,
   -fdiagnostics-set-output=.  Most of the work is done
   by diagnostics/output-spec.cc so it can be shared by libgdiagnostics.  */

#include "config.h"
#define INCLUDE_ARRAY
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "version.h"
#include "intl.h"
#include "diagnostic.h"
#include "diagnostics/output-spec.h"
#include "diagnostics/logging.h"
#include "opts.h"
#include "options.h"

/* Decls.  */

namespace {

struct opt_spec_context : public diagnostics::output_spec::dc_spec_context
{
public:
  opt_spec_context (const gcc_options &opts,
		    diagnostics::context &dc,
		    line_maps *location_mgr,
		    location_t loc,
		    const char *option_name,
		    const char *option_value)
  : dc_spec_context (option_name,
		     option_value,
		     nullptr,
		     location_mgr,
		     dc,
		     location_mgr,
		     loc),
    m_opts (opts)
  {}

  const char *
  get_base_filename () const final override
  {
    return (m_opts.x_dump_base_name
	    ? m_opts.x_dump_base_name
	    : m_opts.x_main_input_basename);
  }

  const gcc_options &m_opts;
};

} // anon namespace

void
handle_OPT_fdiagnostics_add_output_ (const gcc_options &opts,
				     diagnostics::context &dc,
				     const char *unparsed_spec,
				     location_t loc)
{
  gcc_assert (unparsed_spec);
  gcc_assert (line_table);

  const char *const option_name = "-fdiagnostics-add-output=";
  DIAGNOSTICS_LOG_SCOPE_PRINTF2 (dc.get_logger (),
				 "handling: %s%s", option_name, unparsed_spec);
  opt_spec_context ctxt (opts, dc, line_table, loc, option_name, unparsed_spec);
  auto sink = ctxt.parse_and_make_sink (dc);
  if (!sink)
    return;

  sink->set_main_input_filename (opts.x_main_input_filename);
  dc.add_sink (std::move (sink));
}

void
handle_OPT_fdiagnostics_set_output_ (const gcc_options &opts,
				     diagnostics::context &dc,
				     const char *unparsed_spec,
				     location_t loc)
{
  gcc_assert (unparsed_spec);
  gcc_assert (line_table);

  const char *const option_name = "-fdiagnostics-set-output=";
  DIAGNOSTICS_LOG_SCOPE_PRINTF2 (dc.get_logger (),
				 "handling: %s%s", option_name, unparsed_spec);
  opt_spec_context ctxt (opts, dc, line_table, loc, option_name, unparsed_spec);
  auto sink = ctxt.parse_and_make_sink (dc);
  if (!sink)
    return;

  sink->set_main_input_filename (opts.x_main_input_filename);
  dc.set_sink (std::move (sink));
}
