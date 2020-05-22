/* Managing temporary directories and their content within libgccjit.so
   Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

#include "jit-tempdir.h"

#ifdef _WIN32
#include "jit-w32.h"
#endif

#ifndef _WIN32
/* Construct a tempdir path template suitable for use by mkdtemp
   e.g. "/tmp/libgccjit-XXXXXX", but respecting the rules in
   libiberty's choose_tempdir rather than hardcoding "/tmp/".

   The memory is allocated using malloc and must be freed.
   Aborts the process if allocation fails. */

static char *
make_tempdir_path_template ()
{
  const char *tmpdir_buf;
  size_t tmpdir_len;
  const char *file_template_buf;
  size_t file_template_len;
  char *result;

  /* The result of choose_tmpdir is a cached buffer within libiberty, so
     we must *not* free it.  */
  tmpdir_buf = choose_tmpdir ();

  /* choose_tmpdir aborts on malloc failure.  */
  gcc_assert (tmpdir_buf);

  tmpdir_len = strlen (tmpdir_buf);
  /* tmpdir_buf should now have a dir separator as the final byte.  */
  gcc_assert (tmpdir_len > 0);
  gcc_assert (tmpdir_buf[tmpdir_len - 1] == DIR_SEPARATOR);

  file_template_buf = "libgccjit-XXXXXX";
  file_template_len = strlen (file_template_buf);

  result = XNEWVEC (char, tmpdir_len + file_template_len + 1);
  strcpy (result, tmpdir_buf);
  strcpy (result + tmpdir_len, file_template_buf);

  return result;
}
#endif

/* The constructor for the jit::tempdir object.
   The real work is done by the jit::tempdir::create method.  */

gcc::jit::tempdir::tempdir (logger *logger, int keep_intermediates)
  : log_user (logger),
    m_keep_intermediates (keep_intermediates),
    m_path_template (NULL),
    m_path_tempdir (NULL),
    m_path_c_file (NULL),
    m_path_s_file (NULL),
    m_path_so_file (NULL)
{
  JIT_LOG_SCOPE (get_logger ());
}

/* Do the real work of creating the on-disk tempdir.
   We do this here, rather than in the jit::tempdir constructor
   so that we can handle failure without needing exceptions.  */

bool
gcc::jit::tempdir::create ()
{
  JIT_LOG_SCOPE (get_logger ());

#ifdef _WIN32
  m_path_tempdir = win_mkdtemp ();
#else
  m_path_template = make_tempdir_path_template ();
  if (!m_path_template)
    return false;

  log ("m_path_template: %s", m_path_template);

  /* Create tempdir using mkdtemp.  This is created with 0700 perms and
     is unique.  Hence no other (non-root) users should have access to
     the paths within it.  */
  m_path_tempdir = mkdtemp (m_path_template);
#endif

  if (!m_path_tempdir)
    return false;
  log ("m_path_tempdir: %s", m_path_tempdir);

  m_path_c_file = concat (m_path_tempdir, "/fake.c", NULL);
  m_path_s_file = concat (m_path_tempdir, "/fake.s", NULL);
  m_path_so_file = concat (m_path_tempdir, "/fake.so", NULL);

  /* Success.  */
  return true;
}

/* The destructor for the jit::tempdir object, which
   cleans up the filesystem directory and its contents
   (unless keep_intermediates was set).  */

gcc::jit::tempdir::~tempdir ()
{
  JIT_LOG_SCOPE (get_logger ());

  if (m_keep_intermediates)
    fprintf (stderr, "intermediate files written to %s\n", m_path_tempdir);
  else
    {
      /* Clean up .s/.so.  */
      if (m_path_s_file)
	{
	  log ("unlinking .s file: %s", m_path_s_file);
	  unlink (m_path_s_file);
	}
      if (m_path_so_file)
	{
	  log ("unlinking .so file: %s", m_path_so_file);
	  unlink (m_path_so_file);
	}

      /* Clean up any other tempfiles.  */
      int i;
      char *tempfile;
      FOR_EACH_VEC_ELT (m_tempfiles, i, tempfile)
	{
	  log ("unlinking tempfile: %s", tempfile);
	  unlink (tempfile);
	}

      /* The tempdir should now be empty; remove it.  */
      if (m_path_tempdir)
	{
	  log ("removing tempdir: %s", m_path_tempdir);
	  rmdir (m_path_tempdir);
	}
    }

  free (m_path_template);
  /* m_path_tempdir aliases m_path_template, or is NULL, so don't
     attempt to free it .  */
  free (m_path_c_file);
  free (m_path_s_file);
  free (m_path_so_file);

  int i;
  char *tempfile;
  FOR_EACH_VEC_ELT (m_tempfiles, i, tempfile)
    free (tempfile);
}
