/* Helper code for graphviz output.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "graphviz.h"

/* graphviz_out's ctor, wrapping PP.  */

graphviz_out::graphviz_out (pretty_printer *pp)
: m_pp (pp),
  m_indent (0)
{
}

/* Formatted print of FMT.  */

void
graphviz_out::print (const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  text_info text (fmt, &ap, errno);
  pp_format (m_pp, &text);
  pp_output_formatted_text (m_pp);
  va_end (ap);
}

/* Formatted print of FMT.  The text is indented by the current
   indent, and a newline is added.  */

void
graphviz_out::println (const char *fmt, ...)
{
  va_list ap;

  write_indent ();

  va_start (ap, fmt);
  text_info text (fmt, &ap, errno);
  pp_format (m_pp, &text);
  pp_output_formatted_text (m_pp);
  va_end (ap);

  pp_newline (m_pp);
}

/* Print the current indent to the underlying pp.  */

void
graphviz_out::write_indent ()
{
  for (int i = 0; i < m_indent * 2; ++i)
    pp_space (m_pp);
}

/* Write the start of an HTML-like row via <TR>, writing to the stream
   so that followup text can be escaped.  */

void
graphviz_out::begin_tr ()
{
  pp_string (m_pp, "<TR>");
  pp_write_text_to_stream (m_pp);
}

/* Write the end of an HTML-like row via </TR>, writing to the stream
   so that followup text can be escaped.  */

void
graphviz_out::end_tr ()
{
  pp_string (m_pp, "</TR>");
  pp_write_text_to_stream (m_pp);
}

/* Write the start of an HTML-like <TD>, writing to the stream
   so that followup text can be escaped.  */

void
graphviz_out::begin_td ()
{
  pp_string (m_pp, "<TD ALIGN=\"LEFT\">");
  pp_write_text_to_stream (m_pp);
}

/* Write the end of an HTML-like </TD>, writing to the stream
   so that followup text can be escaped.  */

void
graphviz_out::end_td ()
{
  pp_string (m_pp, "</TD>");
  pp_write_text_to_stream (m_pp);
}

/* Write the start of an HTML-like row via <TR><TD>, writing to the stream
   so that followup text can be escaped.  */

void
graphviz_out::begin_trtd ()
{
  pp_string (m_pp, "<TR><TD ALIGN=\"LEFT\">");
  pp_write_text_to_stream (m_pp);
}

/* Write the end of an HTML-like row via </TD></TR>, writing to the stream
   so that followup text can be escaped.  */

void
graphviz_out::end_tdtr ()
{
  pp_string (m_pp, "</TD></TR>");
  pp_write_text_to_stream (m_pp);
}
