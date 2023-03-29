/* Support for plotting bar charts in dumps.
   Copyright (C) 2020-2023 Free Software Foundation, Inc.
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
#include "pretty-print.h"
#include "analyzer/bar-chart.h"

#if ENABLE_ANALYZER

namespace ana {

/* class bar_chart.  */

/* Add an item, taking a copy of NAME.  */

void
bar_chart::add_item (const char *name, value_t value)
{
  m_items.safe_push (new item (name, value));
}

/* Print the data to PP.  */

void
bar_chart::print (pretty_printer *pp) const
{
  /* Get maximum printing widths and maximum value.  */
  size_t max_width_name = 0;
  size_t max_width_value = 0;
  value_t max_value = 0;
  unsigned i;
  item *item;
  char digit_buffer[128];
  FOR_EACH_VEC_ELT (m_items, i, item)
    {
      max_width_name = MAX (max_width_name, item->m_strlen);
      sprintf (digit_buffer, "%li", item->m_value);
      max_width_value = MAX (max_width_value, strlen (digit_buffer));
      max_value = MAX (max_value, item->m_value);
    }

  /* Print items.  */
  FOR_EACH_VEC_ELT (m_items, i, item)
    {
      /* Print left-aligned name, padding to max_width_name.  */
      pp_string (pp, item->m_name);
      print_padding (pp, max_width_name - item->m_strlen);

      pp_string (pp, ": ");

      /* Print right-aligned name, padding to max_width_value.  */
      sprintf (digit_buffer, "%li", item->m_value);
      const size_t value_width = strlen (digit_buffer);
      print_padding (pp, max_width_value - value_width);
      pp_string (pp, digit_buffer);

      pp_character (pp, '|');

      /* Print bar, scaled in proportion to max value.  */
      const int max_width_bar
	= MIN (max_value, 76 - (max_width_name + max_width_value + 4));
      const int bar_width
	= (max_value > 0 ? (max_width_bar * item->m_value) / max_value : 0);
      for (int j = 0; j < bar_width; j++)
	pp_character (pp, '#');
      print_padding (pp, max_width_bar - bar_width);
      pp_character (pp, '|');
      pp_newline (pp);
    }
}

/* Print COUNT spaces to PP.  */

void
bar_chart::print_padding (pretty_printer *pp, size_t count)
{
  for (size_t i = 0; i < count; i++)
    pp_character (pp, ' ');
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
