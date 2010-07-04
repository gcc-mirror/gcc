/* MD reader definitions.
   Copyright (C) 1987, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
   Free Software Foundation, Inc.

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

#include "obstack.h"
#include "hashtab.h"

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

/* A callback that handles a single .md-file directive, up to but not
   including the closing ')'.  It takes two arguments: the line number on
   which the directive started, and the name of the directive.  The next
   unread character is the optional space after the directive name.  */
typedef void (*directive_handler_t) (int, const char *);

extern const char *in_fname;
extern FILE *read_md_file;
extern int read_md_lineno;
extern const char *read_md_filename;
extern struct obstack string_obstack;
extern void (*include_callback) (const char *);

/* Read the next character from the MD file.  */

static inline int
read_char (void)
{
  int ch;

  ch = getc (read_md_file);
  if (ch == '\n')
    read_md_lineno++;
  return ch;
}

/* Put back CH, which was the last character read from the MD file.  */

static inline void
unread_char (int ch)
{
  if (ch == '\n')
    read_md_lineno--;
  ungetc (ch, read_md_file);
}

extern hashval_t leading_string_hash (const void *);
extern int leading_string_eq_p (const void *, const void *);
extern void copy_md_ptr_loc (const void *, const void *);
extern void print_md_ptr_loc (const void *);
extern const char *join_c_conditions (const char *, const char *);
extern void print_c_condition (const char *);
extern void message_with_line (int, const char *, ...) ATTRIBUTE_PRINTF_2;
extern void error_with_line (int, const char *, ...) ATTRIBUTE_PRINTF_2;
extern void fatal_with_file_and_line (const char *, ...)
  ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
extern void fatal_expected_char (int, int) ATTRIBUTE_NORETURN;
extern int read_skip_spaces (void);
extern void read_name (struct md_name *);
extern char *read_quoted_string (void);
extern char *read_string (int);
extern void read_skip_construct (int, int);
extern int n_comma_elts (const char *);
extern const char *scan_comma_elt (const char **);
extern void upcase_string (char *);
extern void traverse_md_constants (htab_trav, void *);
extern void traverse_enum_types (htab_trav, void *);
extern struct enum_type *lookup_enum_type (const char *);
extern bool read_md_files (int, char **, bool (*) (const char *),
			   directive_handler_t);
