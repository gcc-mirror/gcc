/* Support for plugin-supplied behaviors of known functions.
   Copyright (C) 2022 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_KNOWN_FUNCTION_MANAGER_H
#define GCC_ANALYZER_KNOWN_FUNCTION_MANAGER_H

namespace ana {

class known_function_manager : public log_user
{
public:
  known_function_manager (logger *logger);
  ~known_function_manager ();
  void add (const char *name, known_function *kf);
  const known_function *get_by_identifier (tree identifier);
  const known_function *get_by_fndecl (tree fndecl);

private:
  DISABLE_COPY_AND_ASSIGN (known_function_manager);

  /* Map from identifier to known_function instance.
     Has ownership of the latter.  */
  hash_map<tree, known_function *> m_map_id_to_kf;
};

} // namespace ana

#endif /* GCC_ANALYZER_KNOWN_FUNCTION_MANAGER_H */
