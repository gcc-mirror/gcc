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

#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "pex.h"

/* Read the contents of FILE into memory, or return nullptr
   if there are any problems.  */

static std::unique_ptr<std::vector<char>>
read_all_of_file (FILE *f_in)
{
  /* Read content, allocating a buffer for it.  */
  auto result = std::make_unique<std::vector<char>> ();
  char buf[4096];
  size_t iter_sz_in;

  while ( (iter_sz_in = fread (buf, 1, sizeof (buf), f_in)) )
    {
      size_t old_total_sz = result->size ();
      size_t new_total_sz = old_total_sz + iter_sz_in;
      size_t old_alloc_sz = result->capacity ();
      if (new_total_sz > old_alloc_sz)
	{
	  size_t new_alloc_sz = std::max (old_alloc_sz * 2, new_total_sz);
	  result->reserve (new_alloc_sz);
	}
      gcc_assert (result->capacity () >= new_total_sz);
      result->resize (new_total_sz);
      memcpy (result->data () + old_total_sz, buf, iter_sz_in);
    }

  if (!feof (f_in))
    return nullptr;

  return result;
}

// struct file_wrapper

std::unique_ptr<std::vector<char>>
file_wrapper::read_all ()
{
  return read_all_of_file (m_file);
}

// struct pex

const char *
pex::run (int flags, const char *executable, const std::vector<std::string> &args,
	  const char *outname, const char *errname, int *err)
{
  std::vector<char *> argv;
  for (auto &iter : args)
    argv.push_back (const_cast<char *> (iter.c_str ()));
  argv.push_back (nullptr);
  return pex_run (m_obj, flags, executable, argv.data (),
		  outname, errname, err);
}
