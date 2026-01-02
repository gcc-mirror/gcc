/* C++ wrapper around libiberty's pex API.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

#ifndef GCC_PEX_H
#define GCC_PEX_H

struct file_wrapper
{
  enum class ownership { owned, borrowed };

  file_wrapper (FILE *file, enum ownership ownership)
  : m_file (file),
    m_ownership (ownership)
  {
  }
  ~file_wrapper ()
  {
    if (m_ownership == ownership::owned)
      {
	gcc_assert (m_file);
	fclose (m_file);
      }
  }

  std::unique_ptr<std::vector<char>>
  read_all ();

  FILE *m_file;
  enum ownership m_ownership;
};

// RAII wrapper around pex_obj

struct pex
{
  pex (int flags, const char *pname, const char *tempbase)
  : m_obj (pex_init (flags, pname, tempbase))
  {
  }

  ~pex ()
  {
    pex_free (m_obj);
  }

  const char *
  run (int flags, const char *executable, char * const *argv,
       const char *outname, const char *errname, int *err)
  {
    return pex_run (m_obj, flags, executable, argv, outname, errname, err);
  }

  const char *
  run (int flags, const char *executable, const std::vector<std::string> &args,
       const char *outname, const char *errname, int *err);

  file_wrapper
  input_file (int flags, const char *in_name)
  {
    return file_wrapper (pex_input_file (m_obj, flags, in_name),
			 /* closed on first call to pex_run.  */
			 file_wrapper::ownership::borrowed);
  }

  file_wrapper
  input_pipe (bool binary = true)
  {
    return file_wrapper (pex_input_pipe (m_obj, binary),
			 /* closed on first call to pex_run.  */
			 file_wrapper::ownership::borrowed);
  }

  file_wrapper
  read_output (bool binary = true)
  {
    return file_wrapper (pex_read_output (m_obj, binary),
			 file_wrapper::ownership::borrowed);
  }

  pex_obj *m_obj;
};

#endif /* GCC_PEX_H */
