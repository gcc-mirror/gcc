/* jit-target.cc -- Target interface for the jit front end.
   Copyright (C) 2023 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define INCLUDE_STRING
#define INCLUDE_ALGORITHM
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "memmodel.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "stor-layout.h"
#include "tm.h"
#include "tm_p.h"
#include "target.h"
#include "calls.h"

#include "jit-playback.h"
#include "jit-target.h"

/* Initialize all variables of the Target structure.  */

void
jit_target_init ()
{
  /* Initialize target info tables, the keys required by the language are added
     last, so that the CPU handler can override.  */
  targetjitm.jit_register_cpu_target_info ();
}

/* Add a target info key:value to JIT_TARGET_INFO for use by
   target_info::has_target_value ().  */

void
jit_add_target_info (const char *key, const char *value)
{
  gcc_assert (gcc::jit::active_playback_ctxt != NULL);
  target_info* jit_target_info
    = gcc::jit::active_playback_ctxt->get_target_info ();
  if (jit_target_info->m_info.find (key) == jit_target_info->m_info.end ())
    jit_target_info->m_info.insert ({key, {value}});
  else
    jit_target_info->m_info[key].insert (value);
}

void
jit_target_set_arch (std::string const& arch)
{
  gcc_assert (gcc::jit::active_playback_ctxt != NULL);
  target_info* jit_target_info
    = gcc::jit::active_playback_ctxt->get_target_info ();
  jit_target_info->m_arch = arch;
}

void
jit_target_add_supported_target_dependent_type (enum gcc_jit_types type_)
{
  gcc_assert (gcc::jit::active_playback_ctxt != NULL);
  target_info* jit_target_info
    = gcc::jit::active_playback_ctxt->get_target_info ();
  jit_target_info->m_supported_target_dependent_types.insert (type_);
}

target_info *
jit_get_target_info ()
{
  gcc_assert (gcc::jit::active_playback_ctxt != NULL);
  target_info* info = gcc::jit::active_playback_ctxt->move_target_info ();
  return info;
}

bool
target_info::has_target_value (const char *key, const char *value)
{
  if (m_info.find (key) == m_info.end ())
    return false;

  auto& set = m_info[key];
  return set.find (value) != set.end ();
}
