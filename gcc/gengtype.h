/* Process source files and output type information.
   Copyright (C) 2002, 2003, 2004, 2007, 2008, 2010 
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

#ifndef GCC_GENGTYPE_H
#define GCC_GENGTYPE_H

/* Sets of accepted source languages like C, C++, Ada... are
   represented by a bitmap.  */
typedef unsigned lang_bitmap;

/* Variable length structure representing an input file.  A hash table
   ensure uniqueness for a given input file name.  The only function
   allocating input_file-s is input_file_by_name.  */
struct input_file_st 
{
  struct outf* inpoutf;  /* Cached corresponding output file, computed
                            in get_output_file_with_visibility.  */
  lang_bitmap inpbitmap; /* The set of languages using this file.  */
  char inpname[1];       /* A variable-length array, ended by a null
                            char.  */
};
typedef struct input_file_st input_file;

/* A file position, mostly for error messages.
   The FILE element may be compared using pointer equality.  */
struct fileloc
{
  const input_file *file;
  int line;
};


/* Table of all input files and its size.  */
extern const input_file** gt_files;
extern size_t num_gt_files;

/* A number of places use the name of this "gengtype.c" file for a
   location for things that we can't rely on the source to define.  We
   also need to refer to the "system.h" file specifically.  These two
   pointers are initialized early in main.  */
extern input_file* this_file;
extern input_file* system_h_file;

/* Retrieve or create the input_file for a given name, which is a file
   path.  This is the only function allocating input_file-s and it is
   hash-consing them.  */
input_file* input_file_by_name (const char* name);

/* For F an input_file, return the relative path to F from $(srcdir)
   if the latter is a prefix in F, NULL otherwise.  */
const char *get_file_srcdir_relative_path (const input_file *inpf);

/* Get the name of an input file.  */
static inline const char*
get_input_file_name (const input_file *inpf)
{
  if (inpf)
      return inpf->inpname;
  return NULL;
}

/* Return a bitmap which has bit `1 << BASE_FILE_<lang>' set iff
   INPUT_FILE is used by <lang>.

   This function should be written to assume that a file _is_ used
   if the situation is unclear.  If it wrongly assumes a file _is_ used,
   a linker error will result.  If it wrongly assumes a file _is not_ used,
   some GC roots may be missed, which is a much harder-to-debug problem.
  */

static inline lang_bitmap
get_lang_bitmap (const input_file* inpf)
{
  if (inpf == NULL)
    return 0;
  return inpf->inpbitmap;
}

/* Set the bitmap returned by get_lang_bitmap.  The only legitimate
   callers of this function are read_input_list & read_state_*.  */
static inline void
set_lang_bitmap (input_file* inpf, lang_bitmap n)
{
  gcc_assert (inpf);
  inpf->inpbitmap = n;
}

/* Vector of per-language directories.  */
extern const char **lang_dir_names;
extern size_t num_lang_dirs;

/* Data types handed around within, but opaque to, the lexer and parser.  */
typedef struct pair *pair_p;
typedef struct type *type_p;
typedef const struct type *const_type_p;
typedef struct options *options_p;

/* Variables used to communicate between the lexer and the parser.  */
extern int lexer_toplevel_done;
extern struct fileloc lexer_line;

/* Structure representing an output file.  */
struct outf
{
  struct outf *next;
  const char *name;
  size_t buflength;
  size_t bufused;
  char *buf;
};
typedef struct outf *outf_p;

/* The list of output files.  */
extern outf_p output_files;

/* The output header file that is included into pretty much every
   source file.  */
extern outf_p header_file;

/* Print, like fprintf, to O.  No-op if O is NULL.  */
void
oprintf (outf_p o, const char *S, ...)
  ATTRIBUTE_PRINTF_2;

/* An output file, suitable for definitions, that can see declarations
   made in INPF and is linked into every language that uses INPF.  May
   return NULL in plugin mode.  The INPF argument is almost const, but
   since the result is cached in its inpoutf field it cannot be
   declared const.  */
outf_p get_output_file_with_visibility (input_file* inpf);

/* The name of an output file, suitable for definitions, that can see
   declarations made in INPF and is linked into every language that
   uses INPF.  May return NULL.  */
const char *get_output_file_name (input_file *inpf);


/* Source directory.  */
extern const char *srcdir;	/* (-S) program argument. */

/* Length of srcdir name.  */
extern size_t srcdir_len;

/* Variable used for reading and writing the state.  */
extern const char *read_state_filename; /* (-r) program argument. */
extern const char *write_state_filename; /* (-w) program argument. */

/* Print an error message.  */
extern void error_at_line
(const struct fileloc *pos, const char *msg, ...) ATTRIBUTE_PRINTF_2;

/* Like asprintf, but calls fatal() on out of memory.  */
extern char *xasprintf (const char *, ...) ATTRIBUTE_PRINTF_1;

/* Constructor routines for types.  */
extern void do_typedef (const char *s, type_p t, struct fileloc *pos);
extern void do_scalar_typedef (const char *s, struct fileloc *pos);
extern type_p resolve_typedef (const char *s, struct fileloc *pos);
extern type_p new_structure (const char *name, int isunion,
			     struct fileloc *pos, pair_p fields,
			     options_p o);
extern type_p find_structure (const char *s, int isunion);
extern type_p create_scalar_type (const char *name);
extern type_p create_pointer (type_p t);
extern type_p create_array (type_p t, const char *len);
extern options_p create_option (options_p, const char *name,
				const void *info);
extern options_p create_nested_ptr_option (options_p, type_p t,
					   const char *from,
					   const char *to);
extern pair_p create_field_at (pair_p next, type_p type,
			       const char *name, options_p opt,
			       struct fileloc *pos);
extern pair_p nreverse_pairs (pair_p list);
extern type_p adjust_field_type (type_p, options_p);
extern void note_variable (const char *s, type_p t, options_p o,
			   struct fileloc *pos);
extern void note_def_vec (const char *type_name, bool is_scalar,
			  struct fileloc *pos);
extern void note_def_vec_alloc (const char *type, const char *astrat,
				struct fileloc *pos);

/* Lexer and parser routines.  */
extern int yylex (const char **yylval);
extern void yybegin (const char *fname);
extern void yyend (void);
extern void parse_file (const char *name);
extern bool hit_error;

/* Token codes.  */
enum
  {
    EOF_TOKEN = 0,

    /* Per standard convention, codes in the range (0, UCHAR_MAX]
       represent single characters with those character codes.  */

    CHAR_TOKEN_OFFSET = UCHAR_MAX + 1,
    GTY_TOKEN = CHAR_TOKEN_OFFSET,
    TYPEDEF,
    EXTERN,
    STATIC,
    UNION,
    STRUCT,
    ENUM,
    VEC_TOKEN,
    DEFVEC_OP,
    DEFVEC_I,
    DEFVEC_ALLOC,
    ELLIPSIS,
    PTR_ALIAS,
    NESTED_PTR,
    PARAM_IS,
    NUM,
    SCALAR,
    ID,
    STRING,
    CHAR,
    ARRAY,

    /* print_token assumes that any token >= FIRST_TOKEN_WITH_VALUE may have
       a meaningful value to be printed.  */
    FIRST_TOKEN_WITH_VALUE = PARAM_IS
  };


/* Level for verbose messages, e.g. output file generation...  */
extern int verbosity_level;	/* (-v) program argument.  */

/* For debugging purposes we provide two flags.  */

/* Dump everything to understand gengtype's state. Might be useful to
   gengtype users.  */
extern int do_dump;		/* (-d) program argument. */

/* Trace the execution by many DBGPRINTF (with the position inside
   gengtype source code).  Only useful to debug gengtype itself.  */
extern int do_debug;		/* (-D) program argument. */

#if ENABLE_CHECKING
#define DBGPRINTF(Fmt,...) do {if (do_debug)				\
      fprintf (stderr, "%s:%d: " Fmt "\n",				\
	       lbasename (__FILE__),__LINE__, ##__VA_ARGS__);} while (0)
void dbgprint_count_type_at (const char *, int, const char *, type_p);
#define DBGPRINT_COUNT_TYPE(Msg,Ty) do {if (do_debug)			\
      dbgprint_count_type_at (__FILE__, __LINE__, Msg, Ty);}while (0)
#else
#define DBGPRINTF(Fmt,...) do {/*nodbgrintf*/} while (0)
#define DBGPRINT_COUNT_TYPE(Msg,Ty) do{/*nodbgprint_count_type*/}while (0)
#endif /*ENABLE_CHECKING */

#endif
