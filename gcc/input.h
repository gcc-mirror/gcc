/* Declarations for variables relating to reading the source file.
   Used by parsers, lexical analyzers, and error message routines.
   Copyright (C) 1993-2022 Free Software Foundation, Inc.

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

#ifndef GCC_INPUT_H
#define GCC_INPUT_H

#include "line-map.h"

extern GTY(()) class line_maps *line_table;
extern GTY(()) class line_maps *saved_line_table;

/* A value which will never be used to represent a real location.  */
#define UNKNOWN_LOCATION ((location_t) 0)

/* The location for declarations in "<built-in>" */
#define BUILTINS_LOCATION ((location_t) 1)

/* line-map.cc reserves RESERVED_LOCATION_COUNT to the user.  Ensure
   both UNKNOWN_LOCATION and BUILTINS_LOCATION fit into that.  */
STATIC_ASSERT (BUILTINS_LOCATION < RESERVED_LOCATION_COUNT);

/* Hasher for 'location_t' values satisfying '!RESERVED_LOCATION_P', thus able
   to use 'UNKNOWN_LOCATION'/'BUILTINS_LOCATION' as spare values for
   'Empty'/'Deleted'.  */
/* Per PR103157 "'gengtype': 'typedef' causing infinite-recursion code to be
   generated", don't use
       typedef int_hash<location_t, UNKNOWN_LOCATION, BUILTINS_LOCATION>
         location_hash;
   here.

   It works for a single-use case, but when using a 'struct'-based variant
       struct location_hash
         : int_hash<location_t, UNKNOWN_LOCATION, BUILTINS_LOCATION> {};
   in more than one place, 'gengtype' generates duplicate functions (thus:
   "error: redefinition of 'void gt_ggc_mx(location_hash&)'" etc.).
   Attempting to mark that one up with GTY options, we run into a 'gengtype'
   "parse error: expected '{', have '<'", which probably falls into category
   "understanding of C++ is limited", as documented in 'gcc/doc/gty.texi'.

   Thus, use a plain ol' '#define':
*/
#define location_hash int_hash<location_t, UNKNOWN_LOCATION, BUILTINS_LOCATION>

extern bool is_location_from_builtin_token (location_t);
extern expanded_location expand_location (location_t);

class cpp_char_column_policy;

extern int
location_compute_display_column (expanded_location exploc,
				 const cpp_char_column_policy &policy);

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

extern char_span location_get_source_line (const char *file_path, int line);
extern char *get_source_text_between (location_t, location_t);

extern bool location_missing_trailing_newline (const char *file_path);

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

 private:
  file_cache_slot *evicted_cache_tab_entry (unsigned *highest_use_count);
  file_cache_slot *add_file (const char *file_path);
  file_cache_slot *lookup_file (const char *file_path);

 private:
  static const size_t num_file_slots = 16;
  file_cache_slot *m_file_slots;
  input_context in_context;
};

extern expanded_location
expand_location_to_spelling_point (location_t,
				   enum location_aspect aspect
				     = LOCATION_ASPECT_CARET);
extern location_t expansion_point_location_if_in_system_header (location_t);
extern location_t expansion_point_location (location_t);

extern location_t input_location;

extern location_t location_with_discriminator (location_t, int);
extern bool has_discriminator (location_t);
extern int get_discriminator_from_loc (location_t);

#define LOCATION_FILE(LOC) ((expand_location (LOC)).file)
#define LOCATION_LINE(LOC) ((expand_location (LOC)).line)
#define LOCATION_COLUMN(LOC)((expand_location (LOC)).column)
#define LOCATION_LOCUS(LOC) \
  ((IS_ADHOC_LOC (LOC)) ? get_location_from_adhoc_loc (line_table, LOC) \
   : (LOC))
#define LOCATION_BLOCK(LOC) \
  ((tree) ((IS_ADHOC_LOC (LOC)) ? get_data_from_adhoc_loc (line_table, (LOC)) \
   : NULL))
#define RESERVED_LOCATION_P(LOC) \
  (LOCATION_LOCUS (LOC) < RESERVED_LOCATION_COUNT)

/* Return a positive value if LOCATION is the locus of a token that is
   located in a system header, O otherwise. It returns 1 if LOCATION
   is the locus of a token that is located in a system header, and 2
   if LOCATION is the locus of a token located in a C system header
   that therefore needs to be extern "C" protected in C++.

   Note that this function returns 1 if LOCATION belongs to a token
   that is part of a macro replacement-list defined in a system
   header, but expanded in a non-system file.  */

static inline int
in_system_header_at (location_t loc)
{
  return linemap_location_in_system_header_p (line_table, loc);
}

/* Return true if LOCATION is the locus of a token that
   comes from a macro expansion, false otherwise.  */

static inline bool
from_macro_expansion_at (location_t loc)
{
  return linemap_location_from_macro_expansion_p (line_table, loc);
}

/* Return true if LOCATION is the locus of a token that comes from
   a macro definition, false otherwise.  This differs from from_macro_expansion_at
   in its treatment of macro arguments, for which this returns false.  */

static inline bool
from_macro_definition_at (location_t loc)
{
  return linemap_location_from_macro_definition_p (line_table, loc);
}

static inline location_t
get_pure_location (location_t loc)
{
  return get_pure_location (line_table, loc);
}

/* Get the start of any range encoded within location LOC.  */

static inline location_t
get_start (location_t loc)
{
  return get_range_from_loc (line_table, loc).m_start;
}

/* Get the endpoint of any range encoded within location LOC.  */

static inline location_t
get_finish (location_t loc)
{
  return get_range_from_loc (line_table, loc).m_finish;
}

extern location_t make_location (location_t caret,
				 location_t start, location_t finish);
extern location_t make_location (location_t caret, source_range src_range);

void dump_line_table_statistics (void);

void dump_location_info (FILE *stream);

void diagnostics_file_cache_fini (void);

void diagnostics_file_cache_forcibly_evict_file (const char *file_path);

class GTY(()) string_concat
{
public:
  string_concat (int num, location_t *locs);

  int m_num;
  location_t * GTY ((atomic)) m_locs;
};

class GTY(()) string_concat_db
{
 public:
  string_concat_db ();
  void record_string_concatenation (int num, location_t *locs);

  bool get_string_concatenation (location_t loc,
				 int *out_num,
				 location_t **out_locs);

 private:
  static location_t get_key_loc (location_t loc);

  /* For the fields to be private, we must grant access to the
     generated code in gtype-desc.cc.  */

  friend void ::gt_ggc_mx_string_concat_db (void *x_p);
  friend void ::gt_pch_nx_string_concat_db (void *x_p);
  friend void ::gt_pch_p_16string_concat_db (void *this_obj, void *x_p,
					     gt_pointer_operator op,
					     void *cookie);

  hash_map <location_hash, string_concat *> *m_table;
};

#endif
