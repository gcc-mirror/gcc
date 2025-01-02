/* Support for plotting bar charts in dumps.
   Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_BAR_CHART_H
#define GCC_ANALYZER_BAR_CHART_H

namespace ana {

/* A class for printing bar charts to a pretty_printer.

   TODO(stage1): move to gcc subdir? */

class bar_chart
{
public:
  typedef unsigned long value_t;

  /* Add an item, taking a copy of NAME.  */
  void add_item (const char *name, value_t value);

  /* Print the data to PP.  */
  void print (pretty_printer *pp) const;

private:
  struct item
  {
    item (const char *name, value_t value)
    : m_name (xstrdup (name)), m_strlen (strlen (name)) , m_value (value) {}
    ~item () { free (m_name); }

    char *m_name;
    size_t m_strlen;
    value_t m_value;
  };

  static void print_padding (pretty_printer *pp, size_t count);

  auto_delete_vec<item> m_items;
};

} // namespace ana

#endif /* GCC_ANALYZER_BAR_CHART_H */
