/* Support for plugin-supplied behaviors of known functions.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
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

#include "analyzer/analyzer-logging.h"

namespace ana {

/* Instances of known_function are registered with the known_function_manager
   when the analyzer starts.

   The known_function_manager has responsibility for determining which
   known_function instance (if any) is relevant at a call site, by checking
   name or id, and by calling known_function::matches_call_types_p to ensure
   that the known_function's preconditions hold (typically assumptions about
   types e.g. that "has 3 args, and that arg 0 is of pointer type").

   The known_function subclasses themselves have responsibility for
   determining the outcome(s) of the call.  */

class known_function_manager : public log_user
{
public:
  known_function_manager (logger *logger);
  ~known_function_manager ();

  void add (const char *name, std::unique_ptr<known_function> kf);
  void add (enum built_in_function name, std::unique_ptr<known_function> kf);
  void add (enum internal_fn ifn, std::unique_ptr<known_function> kf);

  const known_function *get_match (tree fndecl, const call_details &cd) const;
  const known_function *get_internal_fn (enum internal_fn) const;

private:
  DISABLE_COPY_AND_ASSIGN (known_function_manager);

  const known_function *get_normal_builtin (enum built_in_function name) const;
  const known_function *
  get_normal_builtin (const builtin_known_function *builtin_kf) const;
  const known_function *get_by_identifier (tree identifier) const;

  /* Map from identifier to known_function instance.
     Has ownership of the latter.  */
  hash_map<tree, known_function *> m_map_id_to_kf;

  /* Array of known builtins.  */
  known_function *m_combined_fns_arr[CFN_LAST];
};

extern std::unique_ptr<known_function> make_kf_strlen ();

} // namespace ana

#endif /* GCC_ANALYZER_KNOWN_FUNCTION_MANAGER_H */
