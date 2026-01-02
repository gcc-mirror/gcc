/* HTML output for diagnostics.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTICS_HTML_SINK_H
#define GCC_DIAGNOSTICS_HTML_SINK_H

#include "diagnostics/sink.h"
#include "diagnostics/output-file.h"

namespace diagnostics {

struct html_generation_options
{
  html_generation_options ();

  void dump (FILE *out, int indent) const;

  bool m_css;
  bool m_javascript;

  // Debugging options:

  // If true, attempt to show state diagrams at events
  bool m_show_state_diagrams;

  /* If true, show the SARIF form of the state with such diagrams,
     and of other graphs.  */
  bool m_show_graph_sarif;

  // If true, show the .dot source used for such graphs
  bool m_show_graph_dot_src;
};

extern diagnostics::output_file
open_html_output_file (context &dc,
		       line_maps *line_maps,
		       const char *base_file_name);

extern std::unique_ptr<sink>
make_html_sink (context &dc,
		const line_maps &line_maps,
		const html_generation_options &html_gen_opts,
		output_file output_file_);

extern void
print_path_as_html (xml::printer &xp,
		    const paths::path &path,
		    context &dc,
		    html_label_writer *event_label_writer,
		    const source_print_policy &dspp);

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_HTML_SINK_H */
