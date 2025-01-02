/* A bundle of location information for a checker_event.
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

#ifndef GCC_ANALYZER_EVENT_LOC_INFO_H
#define GCC_ANALYZER_EVENT_LOC_INFO_H

namespace ana {

/* A bundle of location information for a checker_event.  */

struct event_loc_info
{
  event_loc_info (location_t loc, tree fndecl, int depth)
  : m_loc (loc), m_fndecl (fndecl), m_depth (depth)
  {}

  location_t m_loc;
  tree m_fndecl;
  int m_depth;
};

} // namespace ana

#endif /* GCC_ANALYZER_EVENT_LOC_INFO_H */
