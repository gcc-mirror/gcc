/* Classes for adding special effects when quoting source code.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTIC_LABEL_EFFECTS_H
#define GCC_DIAGNOSTIC_LABEL_EFFECTS_H

/* Abstract base class for describing special effects when printing
   a label when quoting source code.  */

class label_effects
{
public:
  virtual ~label_effects () {}

  /* Adding links between labels, e.g. for visualizing control flow
     in execution paths.  */
  virtual bool has_in_edge (unsigned range_idx) const = 0;
  virtual bool has_out_edge (unsigned range_idx) const = 0;
};

/* A class to hold state when quoting a run of lines of source code.  */

class diagnostic_source_effect_info
{
public:
  diagnostic_source_effect_info ()
  : m_leading_in_edge_column (-1),
    m_trailing_out_edge_column (-1)
  {
  }

  /* The column for an incoming link to the first label,
     or -1 if no such link.  */
  int m_leading_in_edge_column;

  /* The column for an outgoing link from the final label,
     or -1 if no such link.  */
  int m_trailing_out_edge_column;
};

#endif /* GCC_DIAGNOSTIC_LABEL_EFFECTS_H */
