/* HTML output for diagnostics.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTIC_FORMAT_HTML_H
#define GCC_DIAGNOSTIC_FORMAT_HTML_H

#include "diagnostic-format.h"
#include "diagnostic-output-file.h"

extern diagnostic_output_file
diagnostic_output_format_open_html_file (diagnostic_context &context,
					 line_maps *line_maps,
					 const char *base_file_name);

extern std::unique_ptr<diagnostic_output_format>
make_html_sink (diagnostic_context &context,
		const line_maps &line_maps,
		diagnostic_output_file output_file);

#endif /* ! GCC_DIAGNOSTIC_FORMAT_HTML_H */
