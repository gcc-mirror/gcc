/* MD reader definitions.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

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

#ifndef GCC_READ_MD_H
#define GCC_READ_MD_H

#include "obstack.h"

/* Records a position in the file.  */
struct file_location {
  file_location () {}
  file_location (const char *, int, int);

  const char *filename;
  int lineno;
  int colno;
};

inline file_location::file_location (const char *filename_in, int lineno_in, int colno_in)
: filename (filename_in), lineno (lineno_in), colno (colno_in) {}

/* Holds one symbol or number in the .md file.  */
struct md_name {
  /* The name as it appeared in the .md file.  Names are syntactically
     limited to the length of this buffer.  */
  char buffer[256];

  /* The name that should actually be used by the generator programs.
     This is an expansion of NAME, after things like constant substitution.  */
  char *string;
};

/* This structure represents a constant defined by define_constant,
   define_enum, or such-like.  */
struct md_constant {
  /* The name of the constant.  */
  char *name;

  /* The string to which the constants expands.  */
  char *value;

  /* If the constant is associated with a enumeration, this field
     points to that enumeration, otherwise it is null.  */
  struct enum_type *parent_enum;
};

/* This structure represents one value in an enum_type.  */
struct enum_value {
  /* The next value in the enum, or null if this is the last.  */
  struct enum_value *next;

  /* The name of the value as it appears in the .md file.  */
  char *name;

  /* The definition of the related C value.  */
  struct md_constant *def;
};

/* This structure represents an enum defined by define_enum or the like.  */
struct enum_type {
  /* The C name of the enumeration.  */
  char *name;

  /* True if this is an md-style enum (DEFINE_ENUM) rather than
     a C-style enum (DEFINE_C_ENUM).  */
  bool md_p;

  /* The values of the enumeration.  There is always at least one.  */
  struct enum_value *values;

  /* A pointer to the null terminator in VALUES.  */
  struct enum_value **tail_ptr;

  /* The number of enumeration values.  */
  unsigned int num_values;
};

class rtx_reader
{
 public:
  rtx_reader ();
  virtual ~rtx_reader ();

  bool read_md_files (int, const char **, bool (*) (const char *));

  /* A hook that handles a single .md-file directive, up to but not
     including the closing ')'.  It takes two arguments: the file position
     at which the directive started, and the name of the directive.  The next
     unread character is the optional space after the directive name.  */
  virtual void handle_unknown_directive (file_location, const char *) = 0;

  file_location get_current_location () const;

  /* Defined in read-md.c.  */
  int read_char (void);
  void unread_char (int ch);
  void read_name (struct md_name *name);
  void read_escape ();
  char *read_quoted_string ();
  char *read_braced_string ();
  char *read_string (int star_if_braced);
  void read_skip_construct (int depth, file_location loc);

  void set_md_ptr_loc (const void *ptr, const char *filename, int lineno);
  const struct ptr_loc *get_md_ptr_loc (const void *ptr);
  void copy_md_ptr_loc (const void *new_ptr, const void *old_ptr);
  void fprint_md_ptr_loc (FILE *outf, const void *ptr);
  void print_md_ptr_loc (const void *ptr);

  struct enum_type *lookup_enum_type (const char *name);
  void traverse_enum_types (htab_trav callback, void *info);

  void handle_constants ();
  void traverse_md_constants (htab_trav callback, void *info);
  void handle_enum (file_location loc, bool md_p);

  const char *join_c_conditions (const char *cond1, const char *cond2);
  void fprint_c_condition (FILE *outf, const char *cond);
  void print_c_condition (const char *cond);

  /* Defined in read-rtl.c.  */
  const char *apply_iterator_to_string (const char *string);
  rtx copy_rtx_for_iterators (rtx original);
  void read_conditions ();
  void record_potential_iterator_use (struct iterator_group *group,
				      void *ptr, const char *name);
  struct mapping *read_mapping (struct iterator_group *group, htab_t table);
  bool read_rtx (const char *rtx_name, vec<rtx> *rtxen);
  rtx read_rtx_code (const char *code_name);
  void read_rtx_operand (rtx return_rtx, int idx);
  rtx read_nested_rtx ();
  rtx read_rtx_variadic (rtx form);

  const char *get_top_level_filename () const { return m_toplevel_fname; }
  const char *get_filename () const { return m_read_md_filename; }
  int get_lineno () const { return m_read_md_lineno; }
  int get_colno () const { return m_read_md_colno; }

  struct obstack *get_string_obstack () { return &m_string_obstack; }
  htab_t get_md_constants () { return m_md_constants; }

 private:
  /* A singly-linked list of filenames.  */
  struct file_name_list {
    struct file_name_list *next;
    const char *fname;
  };

 private:
  void handle_file ();
  void handle_toplevel_file ();
  void handle_include (file_location loc);
  void add_include_path (const char *arg);

 private:
  /* The name of the toplevel file that indirectly included
     m_read_md_file.  */
  const char *m_toplevel_fname;

  /* The directory part of m_toplevel_fname
     NULL if m_toplevel_fname is a bare filename.  */
  char *m_base_dir;

  /* The file we are reading.  */
  FILE *m_read_md_file;

  /* The filename of m_read_md_file.  */
  const char *m_read_md_filename;

  /* The current line number in m_read_md_file.  */
  int m_read_md_lineno;

  /* The current column number in m_read_md_file.  */
  int m_read_md_colno;

  /* The column number before the last newline, so that
     we can handle unread_char ('\n') at least once whilst
     retaining column information.  */
  int m_last_line_colno;

  /* The first directory to search.  */
  file_name_list *m_first_dir_md_include;

  /* A pointer to the null terminator of the md include chain.  */
  file_name_list **m_last_dir_md_include_ptr;

  /* Obstack used for allocating MD strings.  */
  struct obstack m_string_obstack;

  /* A table of ptr_locs, hashed on the PTR field.  */
  htab_t m_ptr_locs;

  /* An obstack for the above.  Plain xmalloc is a bit heavyweight for a
     small structure like ptr_loc.  */
  struct obstack m_ptr_loc_obstack;

  /* A hash table of triples (A, B, C), where each of A, B and C is a condition
     and A is equivalent to "B && C".  This is used to keep track of the source
     of conditions that are made up of separate MD strings (such as the split
     condition of a define_insn_and_split).  */
  htab_t m_joined_conditions;

  /* An obstack for allocating joined_conditions entries.  */
  struct obstack m_joined_conditions_obstack;

  /* A table of md_constant structures, hashed by name.  Null if no
     constant expansion should occur.  */
  htab_t m_md_constants;

  /* A table of enum_type structures, hashed by name.  */
  htab_t m_enum_types;
};

/* Global singleton.  */
extern rtx_reader *rtx_reader_ptr;

/* An rtx_reader subclass which skips unknown directives.  */

class noop_reader : public rtx_reader
{
 public:
  noop_reader () : rtx_reader () {}

  /* A dummy implementation which skips unknown directives.  */
  void handle_unknown_directive (file_location, const char *);
};

extern void (*include_callback) (const char *);

/* Read the next character from the MD file.  */

static inline int
read_char (void)
{
  return rtx_reader_ptr->read_char ();
}

/* Put back CH, which was the last character read from the MD file.  */

static inline void
unread_char (int ch)
{
  rtx_reader_ptr->unread_char (ch);
}

extern hashval_t leading_string_hash (const void *);
extern int leading_string_eq_p (const void *, const void *);
extern const char *join_c_conditions (const char *, const char *);
extern void message_at (file_location, const char *, ...) ATTRIBUTE_PRINTF_2;
extern void error_at (file_location, const char *, ...) ATTRIBUTE_PRINTF_2;
extern void fatal_at (file_location, const char *, ...) ATTRIBUTE_PRINTF_2;
extern void fatal_with_file_and_line (const char *, ...)
  ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
extern void fatal_expected_char (int, int) ATTRIBUTE_NORETURN;
extern int read_skip_spaces (void);
extern void require_char_ws (char expected);
extern int n_comma_elts (const char *);
extern const char *scan_comma_elt (const char **);
extern void upcase_string (char *);
extern void traverse_enum_types (htab_trav, void *);
extern struct enum_type *lookup_enum_type (const char *);

#endif /* GCC_READ_MD_H */
