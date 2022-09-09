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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "analyzer/analyzer.h"
#include "diagnostic-core.h"
#include "analyzer/analyzer-logging.h"
#include "stringpool.h"
#include "analyzer/known-function-manager.h"

#if ENABLE_ANALYZER

namespace ana {

/* class known_function_manager : public log_user.  */

known_function_manager::known_function_manager (logger *logger)
: log_user (logger)
{
}

known_function_manager::~known_function_manager ()
{
  /* Delete all owned kfs.  */
  for (auto iter : m_map_id_to_kf)
    delete iter.second;
}

void
known_function_manager::add (const char *name, known_function *kf)
{
  LOG_FUNC_1 (get_logger (), "registering %s", name);
  tree id = get_identifier (name);
  m_map_id_to_kf.put (id, kf);
}

const known_function *
known_function_manager::get_by_identifier (tree identifier)
{
  known_function **slot = m_map_id_to_kf.get (identifier);
  if (slot)
    return *slot;
  else
    return NULL;
}

const known_function *
known_function_manager::get_by_fndecl (tree fndecl)
{
  if (tree identifier = DECL_NAME (fndecl))
    return get_by_identifier (identifier);
  return NULL;
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
