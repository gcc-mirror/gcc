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

#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "analyzer/analyzer.h"
#include "diagnostic-core.h"
#include "analyzer/analyzer-logging.h"
#include "stringpool.h"
#include "basic-block.h"
#include "gimple.h"
#include "analyzer/known-function-manager.h"
#include "analyzer/region-model.h"
#include "analyzer/call-details.h"

#if ENABLE_ANALYZER

namespace ana {

/* class known_function_manager : public log_user.  */

known_function_manager::known_function_manager (logger *logger)
: log_user (logger)
{
  memset (m_combined_fns_arr, 0, sizeof (m_combined_fns_arr));
}

known_function_manager::~known_function_manager ()
{
  /* Delete all owned kfs.  */
  for (auto iter : m_map_id_to_kf)
    delete iter.second;
  for (auto iter : m_std_ns_map_id_to_kf)
    delete iter.second;
  for (auto iter : m_combined_fns_arr)
    delete iter;
}

void
known_function_manager::add (const char *name,
			     std::unique_ptr<known_function> kf)
{
  LOG_FUNC_1 (get_logger (), "registering %s", name);
  tree id = get_identifier (name);
  m_map_id_to_kf.put (id, kf.release ());
}

void
known_function_manager::add_std_ns (const char *name,
			     std::unique_ptr<known_function> kf)
{
  LOG_FUNC_1 (get_logger (), "registering std::%s", name);
  tree id = get_identifier (name);
  m_std_ns_map_id_to_kf.put (id, kf.release ());
}

void
known_function_manager::add (enum built_in_function name,
			     std::unique_ptr<known_function> kf)
{
  gcc_assert (name < END_BUILTINS);
  delete m_combined_fns_arr[name];
  m_combined_fns_arr[name] = kf.release ();
}

void
known_function_manager::add (enum internal_fn ifn,
			     std::unique_ptr<known_function> kf)
{
  gcc_assert (ifn < IFN_LAST);
  delete m_combined_fns_arr[ifn + END_BUILTINS];
  m_combined_fns_arr[ifn + END_BUILTINS] = kf.release ();
}

/* Get any known_function for FNDECL for call CD.

   The call must match all assumptions made by the known_function (such as
   e.g. "argument 1's type must be a pointer type").

   Return NULL if no known_function is found, or it does not match the
   assumption(s).  */

const known_function *
known_function_manager::get_match (tree fndecl, const call_details &cd) const
{
  /* Look for a matching built-in.  */
  if (fndecl_built_in_p (fndecl, BUILT_IN_NORMAL))
    {
      if (const known_function *candidate
	  = get_normal_builtin (DECL_FUNCTION_CODE (fndecl)))
	if (gimple_builtin_call_types_compatible_p (cd.get_call_stmt (),
						    fndecl))
	  return candidate;
    }

  /* Look for a match by name.  */

  if (is_std_function_p (fndecl))
    {
      if (tree identifier = DECL_NAME (fndecl))
	if (const known_function *candidate
	      = get_by_identifier_in_std_ns (identifier))
	  if (candidate->matches_call_types_p (cd))
	    return candidate;
      return nullptr;
    }

  if (DECL_CONTEXT (fndecl)
      && TREE_CODE (DECL_CONTEXT (fndecl)) != TRANSLATION_UNIT_DECL)
    return NULL;
  if (tree identifier = DECL_NAME (fndecl))
    if (const known_function *candidate = get_by_identifier (identifier))
      if (candidate->matches_call_types_p (cd))
	return candidate;

  return NULL;
}

/* Get any known_function for IFN, or NULL.  */

const known_function *
known_function_manager::get_internal_fn (enum internal_fn ifn) const
{
  gcc_assert (ifn < IFN_LAST);
  return m_combined_fns_arr[ifn + END_BUILTINS];
}

/* Get any known_function for NAME, without type-checking.
   Return NULL if there isn't one.  */

const known_function *
known_function_manager::get_normal_builtin (enum built_in_function name) const
{
  /* The numbers for built-in functions in enum combined_fn are the same as
     for the built_in_function enum.  */
  gcc_assert (name < END_BUILTINS);
  return m_combined_fns_arr[name];
}

const known_function *
known_function_manager::
get_normal_builtin (const builtin_known_function *builtin_kf) const
{
  return get_normal_builtin (builtin_kf->builtin_code ());
}

/* Get any known_function matching IDENTIFIER, without type-checking.
   Return NULL if there isn't one.  */

const known_function *
known_function_manager::get_by_identifier (tree identifier) const
{
  known_function_manager *mut_this = const_cast<known_function_manager *>(this);
  known_function **slot = mut_this->m_map_id_to_kf.get (identifier);
  if (slot)
    return *slot;
  else
    return NULL;
}

/* Get any known_function in C++ std:: namespace matching IDENTIFIER, without
   type-checking.
   Return nullptr if there isn't one.  */

const known_function *
known_function_manager::get_by_identifier_in_std_ns (tree identifier) const
{
  known_function_manager *mut_this = const_cast<known_function_manager *>(this);
  known_function **slot = mut_this->m_std_ns_map_id_to_kf.get (identifier);
  if (slot)
    return *slot;
  else
    return nullptr;
}


} // namespace ana

#endif /* #if ENABLE_ANALYZER */
