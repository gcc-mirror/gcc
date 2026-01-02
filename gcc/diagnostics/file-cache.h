/* Caching input files for use by diagnostics.
   Copyright (C) 2004-2026 Free Software Foundation, Inc.

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

#ifndef GCC_DIAGNOSTICS_FILE_CACHE_H
#define GCC_DIAGNOSTICS_FILE_CACHE_H

namespace diagnostics {

/* A class capturing the bounds of a buffer, to allow for run-time
   bounds-checking in a checked build.  */

class char_span
{
 public:
  char_span (const char *ptr, size_t n_elts) : m_ptr (ptr), m_n_elts (n_elts) {}

  /* Test for a non-NULL pointer.  */
  operator bool() const { return m_ptr; }

  /* Get length, not including any 0-terminator (which may not be,
     in fact, present).  */
  size_t length () const { return m_n_elts; }

  const char *get_buffer () const { return m_ptr; }

  char operator[] (int idx) const
  {
    gcc_assert (idx >= 0);
    gcc_assert ((size_t)idx < m_n_elts);
    return m_ptr[idx];
  }

  char_span subspan (int offset, int n_elts) const
  {
    gcc_assert (offset >= 0);
    gcc_assert (offset < (int)m_n_elts);
    gcc_assert (n_elts >= 0);
    gcc_assert (offset + n_elts <= (int)m_n_elts);
    return char_span (m_ptr + offset, n_elts);
  }

  char *xstrdup () const
  {
    return ::xstrndup (m_ptr, m_n_elts);
  }

 private:
  const char *m_ptr;
  size_t m_n_elts;
};

/* Forward decl of slot within file_cache, so that the definition doesn't
   need to be in this header.  */
class file_cache_slot;

/* A cache of source files for use when emitting diagnostics
   (and in a few places in the C/C++ frontends).

   Results are only valid until the next call to the cache, as
   slots can be evicted.

   Filenames are stored by pointer, and so must outlive the cache
   instance.  */

class file_cache
{
 public:
  file_cache ();
  ~file_cache ();

  void dump (FILE *out, int indent) const;
  void DEBUG_FUNCTION dump () const;

  file_cache_slot *lookup_or_add_file (const char *file_path);
  void forcibly_evict_file (const char *file_path);

  /* See comments in diagnostic.h about the input conversion context.  */
  struct input_context
  {
    diagnostic_input_charset_callback ccb;
    bool should_skip_bom;
  };
  void initialize_input_context (diagnostic_input_charset_callback ccb,
				 bool should_skip_bom);

  char_span get_source_file_content (const char *file_path);
  char_span get_source_line (const char *file_path, int line);
  bool missing_trailing_newline_p (const char *file_path);

  void add_buffered_content (const char *file_path,
			     const char *buffer,
			     size_t sz);

  void tune (size_t num_file_slots, size_t lines);

 private:
  file_cache_slot *evicted_cache_tab_entry (unsigned *highest_use_count);
  file_cache_slot *add_file (const char *file_path);
  file_cache_slot *lookup_file (const char *file_path);

 private:
  size_t m_num_file_slots;
  file_cache_slot *m_file_slots;
  input_context m_input_context;
};

} // namespace diagnostics

#endif // #ifndef GCC_DIAGNOSTICS_FILE_CACHE_H
