/* Internals of libgccjit: implementation of gcc_jit_result
   Copyright (C) 2013-2025 Free Software Foundation, Inc.
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
#define INCLUDE_DLFCN_H
#include "system.h"
#include "coretypes.h"

#include "jit-common.h"
#include "jit-logging.h"
#include "jit-result.h"
#include "jit-tempdir.h"

#ifdef _WIN32
#include "jit-w32.h"
#endif

namespace gcc {
namespace jit {

/* Constructor for gcc::jit::result.  */

result::
result(logger *logger, handle dso_handle, tempdir *tempdir_) :
  log_user (logger),
  m_dso_handle (dso_handle),
  m_tempdir (tempdir_)
{
  JIT_LOG_SCOPE (get_logger ());
}

/* gcc::jit::result's destructor.

   Called implicitly by gcc_jit_result_release.  */

result::~result()
{
  JIT_LOG_SCOPE (get_logger ());

#ifdef _WIN32
  FreeLibrary(m_dso_handle);
#else
  dlclose (m_dso_handle);
#endif
  /* Responsibility for cleaning up the tempdir (including "fake.so" within
     the filesystem) might have been handed to us by the playback::context,
     so that the cleanup can be delayed (see PR jit/64206).

     If so, clean it up now.  */
  delete m_tempdir;
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

#ifdef _WIN32
  /* Clear any existing error.  */
  SetLastError(0);

  code = (void *)GetProcAddress(m_dso_handle, funcname);
  if (GetLastError() != 0)  {
    print_last_error ();
  }
#else
  const char *error;
  /* Clear any existing error.  */
  dlerror ();

  code = dlsym (m_dso_handle, funcname);

  if ((error = dlerror()) != NULL)  {
    fprintf(stderr, "%s\n", error);
  }
#endif

  return code;
}

/* Attempt to locate the given global by name within the
   playback::result, using dlsym.

   Implements the post-error-checking part of
   gcc_jit_result_get_global.  */

void *
result::
get_global (const char *name)
{
  JIT_LOG_SCOPE (get_logger ());

  void *global;

#ifdef _WIN32
  /* Clear any existing error.  */
  SetLastError(0);

  global = (void *)GetProcAddress(m_dso_handle, name);
  if (GetLastError() != 0)  {
    print_last_error ();
  }
#else
  const char *error;
  /* Clear any existing error.  */
  dlerror ();

  global = dlsym (m_dso_handle, name);

  if ((error = dlerror()) != NULL)  {
    fprintf(stderr, "%s\n", error);
  }
#endif

  return global;
}

} // namespace gcc::jit

} // namespace gcc
