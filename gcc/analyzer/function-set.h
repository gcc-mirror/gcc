/* Sets of function names.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_FUNCTION_SET_H
#define GCC_ANALYZER_FUNCTION_SET_H

namespace ana {

/* A set of names.  */

class function_set
{
public:
  /* Construct from a sorted array NAMES of size COUNT.  */
  function_set (const char * const *names, size_t count)
  : m_names (names), m_count (count)
  {
  }

  bool contains_name_p (const char *name) const;
  bool contains_decl_p (tree fndecl) const;

  void assert_sorted () const;
  void assert_sane () const;

private:
  const char * const *m_names; // must be sorted
  size_t m_count;
};

} // namespace ana

#endif /* GCC_ANALYZER_FUNCTION_SET_H */
