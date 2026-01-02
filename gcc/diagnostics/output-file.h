/* RAII class for managing FILE * for diagnostic formats.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_DIAGNOSTICS_OUTPUT_FILE_H
#define GCC_DIAGNOSTICS_OUTPUT_FILE_H

#include "label-text.h"

namespace diagnostics {

/* RAII class for wrapping a FILE * that could be borrowed or owned,
   along with the underlying filename.  */

class output_file
{
public:
  output_file ()
  : m_outf (nullptr),
    m_owned (false),
    m_filename ()
  {
  }
  output_file (FILE *outf, bool owned, label_text filename)
  : m_outf (outf),
    m_owned (owned),
    m_filename (std::move (filename))
  {
    gcc_assert (m_filename.get ());
    if (m_owned)
      gcc_assert (m_outf);
  }
  ~output_file ()
  {
    if (m_owned)
      {
	gcc_assert (m_outf);
	fclose (m_outf);
      }
  }
  output_file (const output_file &other) = delete;
  output_file (output_file &&other)
  : m_outf (other.m_outf),
    m_owned (other.m_owned),
    m_filename (std::move (other.m_filename))
  {
    other.m_outf = nullptr;
    other.m_owned = false;

    gcc_assert (m_filename.get ());
    if (m_owned)
      gcc_assert (m_outf);
  }
  output_file &
  operator= (const output_file &other) = delete;
  output_file &
  operator= (output_file &&other)
  {
    if (m_owned)
      {
	gcc_assert (m_outf);
	fclose (m_outf);
      }

    m_outf = other.m_outf;
    other.m_outf = nullptr;

    m_owned = other.m_owned;
    other.m_owned = false;

    m_filename = std::move (other.m_filename);

    if (m_owned)
      gcc_assert (m_outf);
    return *this;
  }

  operator bool () const { return m_outf != nullptr; }
  FILE *get_open_file () const { return m_outf; }
  const char *get_filename () const { return m_filename.get (); }

  static output_file
  try_to_open (context &dc,
	       line_maps *line_maps,
	       const char *base_file_name,
	       const char *extension,
	       bool binary);

private:
  FILE *m_outf;
  bool m_owned;
  label_text m_filename;
};

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_OUTPUT_FILE_H */
