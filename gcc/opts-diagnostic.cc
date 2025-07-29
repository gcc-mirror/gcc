/* Support for -fdiagnostics-add-output= and -fdiagnostics-set-output=.
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
		    const char *option_name)
  : dc_spec_context (dc,
		     location_mgr,
		     location_mgr,
		     loc,
		     option_name),
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
				     const char *arg,
				     location_t loc)
{
  gcc_assert (arg);
  gcc_assert (line_table);

  const char *const option_name = "-fdiagnostics-add-output=";
  opt_spec_context ctxt (opts, dc, line_table, loc, option_name);
  auto sink = ctxt.parse_and_make_sink (arg, dc);
  if (!sink)
    return;

  sink->set_main_input_filename (opts.x_main_input_filename);
  dc.add_sink (std::move (sink));
}

void
handle_OPT_fdiagnostics_set_output_ (const gcc_options &opts,
				     diagnostics::context &dc,
				     const char *arg,
				     location_t loc)
{
  gcc_assert (arg);
  gcc_assert (line_table);

  const char *const option_name = "-fdiagnostics-set-output=";
  opt_spec_context ctxt (opts, dc, line_table, loc, option_name);
  auto sink = ctxt.parse_and_make_sink (arg, dc);
  if (!sink)
    return;

  sink->set_main_input_filename (opts.x_main_input_filename);
  dc.set_sink (std::move (sink));
}
