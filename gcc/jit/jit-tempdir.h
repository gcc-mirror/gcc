/* Managing temporary directories and their content within libgccjit.so
   Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

#ifndef JIT_TEMPDIR_H
#define JIT_TEMPDIR_H

#include "jit-logging.h"

namespace gcc {

namespace jit {

/* A class to keep track of the jit::playback::context's tempdir.

   The tempdir has the following layout:

     /tmp/libgccjit-XXXXXX/
			 ./fake.c
			    (doesn't exist, but the rest of the
			     compiler needs a source code filename)

			 ./fake.s
			      (created by toplev::main)

			 ./fake.so
			      (created by playback::context::convert_to_dso).

  It is normally deleted from the filesystem in the playback::context's
  dtor, unless GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES was set.  */

class tempdir : public log_user
{
 public:
  tempdir (logger *logger, int keep_intermediates);
  ~tempdir ();

  bool create ();

  const char * get_path () const { return m_path_tempdir; }
  const char * get_path_c_file () const { return m_path_c_file; }
  const char * get_path_s_file () const { return m_path_s_file; }
  const char * get_path_so_file () const { return m_path_so_file; }

  /* Add PATH to the vec of tempfiles that must be unlinked.
     Take ownership of the buffer PATH; it will be freed.  */
  void add_temp_file (char *path) { m_tempfiles.safe_push (path); }

 private:
  /* Was GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES set?  If so, keep the
     on-disk tempdir around after this wrapper object goes away.  */
  int m_keep_intermediates;

  /* Allocated using xmalloc (by xstrdup).  */
  char *m_path_template;

  /* This either aliases m_path_template, or is NULL.  */
  char *m_path_tempdir;

  /* The following are allocated using xmalloc.  */
  char *m_path_c_file;
  char *m_path_s_file;
  char *m_path_so_file;

  /* Other files within the tempdir to be cleaned up:
     - certain ahead-of-time compilation artifacts (.o and .exe files)
     - dumpfiles that were requested via gcc_jit_context_enable_dump.  */
  auto_vec <char *> m_tempfiles;
};

} // namespace gcc::jit

} // namespace gcc

#endif /* JIT_TEMPDIR_H */
