/* Internals of libgccjit: implementation of gcc_jit_result
   Copyright (C) 2013-2015 Free Software Foundation, Inc.
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

#include "jit-common.h"
#include "jit-logging.h"
#include "jit-result.h"

namespace gcc {
namespace jit {

/* Constructor for gcc::jit::result.  */

result::
result(logger *logger, void *dso_handle) :
  log_user (logger),
  m_dso_handle (dso_handle)
{
  JIT_LOG_SCOPE (get_logger ());
}

/* gcc::jit::result's destructor.

   Called implicitly by gcc_jit_result_release.  */

result::~result()
{
  JIT_LOG_SCOPE (get_logger ());

  dlclose (m_dso_handle);
}

/* Attempt to locate the given function by name within the
   playback::result, using dlsym.

   Implements the post-error-checking part of
   gcc_jit_result_get_code.  */

void *
result::
get_code (const char *funcname)
{
  JIT_LOG_SCOPE (get_logger ());

  void *code;
  const char *error;

  /* Clear any existing error.  */
  dlerror ();

  code = dlsym (m_dso_handle, funcname);

  if ((error = dlerror()) != NULL)  {
    fprintf(stderr, "%s\n", error);
  }

  return code;
}

} // namespace gcc::jit

} // namespace gcc
