/* Part of CPP library.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* This header defines all the internal data structures and functions
   that need to be visible across files.  It's called cpphash.h for
   historical reasons.  */

#ifndef GCC_CPPHASH_H
#define GCC_CPPHASH_H

#include "hashtable.h"

struct directive;		/* Deliberately incomplete.  */

/* Test if a sign is valid within a preprocessing number.  */
#define VALID_SIGN(c, prevc) \
  (((c) == '+' || (c) == '-') && \
   ((prevc) == 'e' || (prevc) == 'E' \
    || (((prevc) == 'p' || (prevc) == 'P') \
        && CPP_OPTION (pfile, extended_numbers))))

#define CPP_OPTION(PFILE, OPTION) ((PFILE)->opts.OPTION)
#define CPP_BUFFER(PFILE) ((PFILE)->buffer)
#define CPP_BUF_COLUMN(BUF, CUR) ((CUR) - (BUF)->line_base + (BUF)->col_adjust)
#define CPP_BUF_COL(BUF) CPP_BUF_COLUMN(BUF, (BUF)->cur)

/* Maximum nesting of cpp_buffers.  We use a static limit, partly for
   efficiency, and partly to limit runaway recursion.  */
#define CPP_STACK_MAX 200

/* A generic memory buffer, and operations on it.  */
typedef struct _cpp_buff _cpp_buff;
struct _cpp_buff
{
  struct _cpp_buff *next;
  unsigned char *base, *cur, *limit;
};

extern _cpp_buff *_cpp_get_buff PARAMS ((cpp_reader *, size_t));
extern void _cpp_release_buff PARAMS ((cpp_reader *, _cpp_buff *));
extern void _cpp_extend_buff PARAMS ((cpp_reader *, _cpp_buff **, size_t));
extern _cpp_buff *_cpp_append_extend_buff PARAMS ((cpp_reader *, _cpp_buff *,
						   size_t));
extern void _cpp_free_buff PARAMS ((_cpp_buff *));
extern unsigned char *_cpp_aligned_alloc PARAMS ((cpp_reader *, size_t));
extern unsigned char *_cpp_unaligned_alloc PARAMS ((cpp_reader *, size_t));

#define BUFF_ROOM(BUFF) (size_t) ((BUFF)->limit - (BUFF)->cur)
#define BUFF_FRONT(BUFF) ((BUFF)->cur)
#define BUFF_LIMIT(BUFF) ((BUFF)->limit)

/* List of directories to look for include files in.  */
struct search_path
{
  struct search_path *next;

  /* NOTE: NAME may not be null terminated for the case of the current
     file's directory!  */
  const char *name;
  unsigned int len;
  /* We use these to tell if the directory mentioned here is a duplicate
     of an earlier directory on the search path.  */
  ino_t ino;
  dev_t dev;
  /* Non-zero if it is a system include directory.  */
  int sysp;
  /* Mapping of file names for this directory.  Only used on MS-DOS
     and related platforms.  */
  struct file_name_map *name_map;
};

/* #include types.  */
enum include_type {IT_INCLUDE, IT_INCLUDE_NEXT, IT_IMPORT, IT_CMDLINE};

union utoken
{
  const cpp_token *token;
  const cpp_token **ptoken;
};

/* A "run" of tokens; part of a chain of runs.  */
typedef struct tokenrun tokenrun;
struct tokenrun
{
  tokenrun *next, *prev;
  cpp_token *base, *limit;
};

typedef struct cpp_context cpp_context;
struct cpp_context
{
  /* Doubly-linked list.  */
  cpp_context *next, *prev;

  /* Contexts other than the base context are contiguous tokens.
     e.g. macro expansions, expanded argument tokens.  */
  union utoken first;
  union utoken last;

  /* If non-NULL, a buffer used for storage related to this context.
     When the context is popped, the buffer is released.  */
  _cpp_buff *buff;

  /* For a macro context, the macro node, otherwise NULL.  */
  cpp_hashnode *macro;

  /* True if utoken element is token, else ptoken.  */
  bool direct_p;
};

struct lexer_state
{
  /* Nonzero if first token on line is CPP_HASH.  */
  unsigned char in_directive;

  /* True if we are skipping a failed conditional group.  */
  unsigned char skipping;

  /* Nonzero if in a directive that takes angle-bracketed headers.  */
  unsigned char angled_headers;

  /* Nonzero to save comments.  Turned off if discard_comments, and in
     all directives apart from #define.  */
  unsigned char save_comments;

  /* Nonzero if we're mid-comment.  */
  unsigned char lexing_comment;

  /* Nonzero if lexing __VA_ARGS__ is valid.  */
  unsigned char va_args_ok;

  /* Nonzero if lexing poisoned identifiers is valid.  */
  unsigned char poisoned_ok;

  /* Nonzero to prevent macro expansion.  */
  unsigned char prevent_expansion;  

  /* Nonzero when parsing arguments to a function-like macro.  */
  unsigned char parsing_args;

  /* Nonzero when in a # NUMBER directive.  */
  unsigned char line_extension;
};

/* Special nodes - identifiers with predefined significance.  */
struct spec_nodes
{
  cpp_hashnode *n_defined;		/* defined operator */
  cpp_hashnode *n_true;			/* C++ keyword true */
  cpp_hashnode *n_false;		/* C++ keyword false */
  cpp_hashnode *n__STRICT_ANSI__;	/* STDC_0_IN_SYSTEM_HEADERS */
  cpp_hashnode *n__CHAR_UNSIGNED__;	/* plain char is unsigned */
  cpp_hashnode *n__VA_ARGS__;		/* C99 vararg macros */
};

/* Represents the contents of a file cpplib has read in.  */
struct cpp_buffer
{
  const unsigned char *cur;	 /* current position */
  const unsigned char *backup_to; /* if peeked character is not wanted */
  const unsigned char *rlimit; /* end of valid data */
  const unsigned char *line_base; /* start of current line */

  struct cpp_buffer *prev;

  const unsigned char *buf;	 /* Entire character buffer.  */

  /* Pointer into the include table; non-NULL if this is a file
     buffer.  Used for include_next and to record control macros.  */
  struct include_file *inc;

  /* Value of if_stack at start of this file.
     Used to prohibit unmatched #endif (etc) in an include file.  */
  struct if_stack *if_stack;

  /* Token column position adjustment owing to tabs in whitespace.  */
  unsigned int col_adjust;

  /* Contains PREV_WHITE and/or AVOID_LPASTE.  */
  unsigned char saved_flags;

  /* Because of the way the lexer works, -Wtrigraphs can sometimes
     warn twice for the same trigraph.  This helps prevent that.  */
  const unsigned char *last_Wtrigraphs;

  /* True if we have already warned about C++ comments in this file.
     The warning happens only for C89 extended mode with -pedantic on,
     or for -Wtraditional, and only once per file (otherwise it would
     be far too noisy).  */
  unsigned char warned_cplusplus_comments;

  /* True if we don't process trigraphs and escaped newlines.  True
     for preprocessed input, command line directives, and _Pragma
     buffers.  */
  unsigned char from_stage3;

  /* Nonzero means that the directory to start searching for ""
     include files has been calculated and stored in "dir" below.  */
  unsigned char search_cached;

  /* At EOF, a buffer is automatically popped.  If RETURN_AT_EOF is
     true, a CPP_EOF token is then returned.  Otherwise, the next
     token from the enclosing buffer is returned.  */
  bool return_at_eof;

  /* The directory of the this buffer's file.  Its NAME member is not
     allocated, so we don't need to worry about freeing it.  */
  struct search_path dir;
};

/* A cpp_reader encapsulates the "state" of a pre-processor run.
   Applying cpp_get_token repeatedly yields a stream of pre-processor
   tokens.  Usually, there is only one cpp_reader object active.  */
struct cpp_reader
{
  /* Top of buffer stack.  */
  cpp_buffer *buffer;

  /* Lexer state.  */
  struct lexer_state state;

  /* Source line tracking.  */
  struct line_maps line_maps;
  const struct line_map *map;
  unsigned int line;

  /* The line of the '#' of the current directive.  */
  unsigned int directive_line;

  /* Memory buffers.  */
  _cpp_buff *a_buff;		/* Aligned permanent storage.  */
  _cpp_buff *u_buff;		/* Unaligned permanent storage.  */
  _cpp_buff *free_buffs;	/* Free buffer chain.  */

  /* Context stack.  */
  struct cpp_context base_context;
  struct cpp_context *context;

  /* If in_directive, the directive if known.  */
  const struct directive *directive;

  /* Multiple inlcude optimisation.  */
  const cpp_hashnode *mi_cmacro;
  const cpp_hashnode *mi_ind_cmacro;
  bool mi_valid;

  /* Lexing.  */
  cpp_token *cur_token;
  tokenrun base_run, *cur_run;
  unsigned int lookaheads;

  /* Non-zero prevents the lexer from re-using the token runs.  */
  unsigned int keep_tokens;

  /* Error counter for exit code.  */
  unsigned int errors;

  /* Line and column where a newline was first seen in a string
     constant (multi-line strings).  */
  unsigned int mls_line;
  unsigned int mls_col;

  /* Buffer to hold macro definition string.  */
  unsigned char *macro_buffer;
  unsigned int macro_buffer_len;

  /* Tree of other included files.  See cppfiles.c.  */
  struct splay_tree_s *all_include_files;

  /* Current maximum length of directory names in the search path
     for include files.  (Altered as we get more of them.)  */
  unsigned int max_include_len;

  /* Date and time tokens.  Calculated together if either is requested.  */
  cpp_token date;
  cpp_token time;

  /* EOF token, and a token forcing paste avoidance.  */
  cpp_token avoid_paste;
  cpp_token eof;

  /* Opaque handle to the dependencies of mkdeps.c.  Used by -M etc.  */
  struct deps *deps;

  /* Obstack holding all macro hash nodes.  This never shrinks.
     See cpphash.c */
  struct obstack hash_ob;

  /* Obstack holding buffer and conditional structures.  This is a
     real stack.  See cpplib.c.  */
  struct obstack buffer_ob;

  /* Pragma table - dynamic, because a library user can add to the
     list of recognized pragmas.  */
  struct pragma_entry *pragmas;

  /* Call backs.  */
  struct cpp_callbacks cb;

  /* Identifier hash table.  */ 
  struct ht *hash_table;

  /* User visible options.  */
  struct cpp_options opts;

  /* Special nodes - identifiers with predefined significance to the
     preprocessor.  */
  struct spec_nodes spec_nodes;

  /* Whether to print our version number.  Done this way so
     we don't get it twice for -v -version.  */
  unsigned char print_version;

  /* Whether cpplib owns the hashtable.  */
  unsigned char our_hashtable;
};

/* Character classes.  Based on the more primitive macros in safe-ctype.h.
   If the definition of `numchar' looks odd to you, please look up the
   definition of a pp-number in the C standard [section 6.4.8 of C99].

   In the unlikely event that characters other than \r and \n enter
   the set is_vspace, the macro handle_newline() in cpplex.c must be
   updated.  */
#define _dollar_ok(x)	((x) == '$' && CPP_OPTION (pfile, dollars_in_ident))

#define is_idchar(x)	(ISIDNUM(x) || _dollar_ok(x))
#define is_numchar(x)	ISIDNUM(x)
#define is_idstart(x)	(ISIDST(x) || _dollar_ok(x))
#define is_numstart(x)	ISDIGIT(x)
#define is_hspace(x)	ISBLANK(x)
#define is_vspace(x)	IS_VSPACE(x)
#define is_nvspace(x)	IS_NVSPACE(x)
#define is_space(x)	IS_SPACE_OR_NUL(x)

/* This table is constant if it can be initialized at compile time,
   which is the case if cpp was compiled with GCC >=2.7, or another
   compiler that supports C99.  */
#if HAVE_DESIGNATED_INITIALIZERS
extern const unsigned char _cpp_trigraph_map[UCHAR_MAX + 1];
#else
extern unsigned char _cpp_trigraph_map[UCHAR_MAX + 1];
#endif

/* Macros.  */

#define CPP_PRINT_DEPS(PFILE) CPP_OPTION (PFILE, print_deps)
#define CPP_IN_SYSTEM_HEADER(PFILE) ((PFILE)->map && (PFILE)->map->sysp)
#define CPP_PEDANTIC(PF) CPP_OPTION (PF, pedantic)
#define CPP_WTRADITIONAL(PF) CPP_OPTION (PF, warn_traditional)

/* In cpperror.c  */
enum error_type { WARNING = 0, WARNING_SYSHDR, PEDWARN, ERROR, FATAL, ICE };
extern int _cpp_begin_message PARAMS ((cpp_reader *, enum error_type,
				       unsigned int, unsigned int));

/* In cppmacro.c */
extern void _cpp_free_definition	PARAMS ((cpp_hashnode *));
extern int _cpp_create_definition	PARAMS ((cpp_reader *, cpp_hashnode *));
extern void _cpp_pop_context		PARAMS ((cpp_reader *));

/* In cpphash.c */
extern void _cpp_init_hashtable		PARAMS ((cpp_reader *, hash_table *));
extern void _cpp_destroy_hashtable	PARAMS ((cpp_reader *));

/* In cppfiles.c */
extern void _cpp_fake_include		PARAMS ((cpp_reader *, const char *));
extern void _cpp_never_reread		PARAMS ((struct include_file *));
extern char *_cpp_simplify_pathname	PARAMS ((char *));
extern bool _cpp_read_file		PARAMS ((cpp_reader *, const char *));
extern bool _cpp_execute_include	PARAMS ((cpp_reader *,
						 const cpp_token *,
						 enum include_type));
extern int _cpp_compare_file_date       PARAMS ((cpp_reader *,
						 const cpp_token *));
extern void _cpp_report_missing_guards	PARAMS ((cpp_reader *));
extern void _cpp_init_includes		PARAMS ((cpp_reader *));
extern void _cpp_cleanup_includes	PARAMS ((cpp_reader *));
extern bool _cpp_pop_file_buffer	PARAMS ((cpp_reader *,
						 struct include_file *));

/* In cppexp.c */
extern int _cpp_parse_expr		PARAMS ((cpp_reader *));

/* In cpplex.c */
extern cpp_token *_cpp_temp_token	PARAMS ((cpp_reader *));
extern const cpp_token *_cpp_lex_token	PARAMS ((cpp_reader *));
extern cpp_token *_cpp_lex_direct	PARAMS ((cpp_reader *));
extern int _cpp_equiv_tokens		PARAMS ((const cpp_token *,
						 const cpp_token *));
extern void _cpp_init_tokenrun		PARAMS ((tokenrun *, unsigned int));

/* In cppinit.c.  */
extern bool _cpp_push_next_buffer	PARAMS ((cpp_reader *));

/* In cpplib.c */
extern int _cpp_test_assertion PARAMS ((cpp_reader *, int *));
extern int _cpp_handle_directive PARAMS ((cpp_reader *, int));
extern void _cpp_define_builtin	PARAMS ((cpp_reader *, const char *));
extern void _cpp_do__Pragma	PARAMS ((cpp_reader *));
extern void _cpp_init_directives PARAMS ((cpp_reader *));
extern void _cpp_init_internal_pragmas PARAMS ((cpp_reader *));
extern void _cpp_do_file_change PARAMS ((cpp_reader *, enum lc_reason,
					 const char *,
					 unsigned int, unsigned int));
extern void _cpp_pop_buffer PARAMS ((cpp_reader *));

/* Utility routines and macros.  */
#define DSC(str) (const U_CHAR *)str, sizeof str - 1
#define xnew(T)		(T *) xmalloc (sizeof(T))
#define xcnew(T)	(T *) xcalloc (1, sizeof(T))
#define xnewvec(T, N)	(T *) xmalloc (sizeof(T) * (N))
#define xcnewvec(T, N)	(T *) xcalloc (N, sizeof(T))
#define xobnew(O, T)	(T *) obstack_alloc (O, sizeof(T))

/* These are inline functions instead of macros so we can get type
   checking.  */
typedef unsigned char U_CHAR;
#define U (const U_CHAR *)  /* Intended use: U"string" */

static inline int ustrcmp	PARAMS ((const U_CHAR *, const U_CHAR *));
static inline int ustrncmp	PARAMS ((const U_CHAR *, const U_CHAR *,
					 size_t));
static inline size_t ustrlen	PARAMS ((const U_CHAR *));
static inline U_CHAR *uxstrdup	PARAMS ((const U_CHAR *));
static inline U_CHAR *ustrchr	PARAMS ((const U_CHAR *, int));
static inline int ufputs	PARAMS ((const U_CHAR *, FILE *));

static inline int
ustrcmp (s1, s2)
     const U_CHAR *s1, *s2;
{
  return strcmp ((const char *)s1, (const char *)s2);
}

static inline int
ustrncmp (s1, s2, n)
     const U_CHAR *s1, *s2;
     size_t n;
{
  return strncmp ((const char *)s1, (const char *)s2, n);
}

static inline size_t
ustrlen (s1)
     const U_CHAR *s1;
{
  return strlen ((const char *)s1);
}

static inline U_CHAR *
uxstrdup (s1)
     const U_CHAR *s1;
{
  return (U_CHAR *) xstrdup ((const char *)s1);
}

static inline U_CHAR *
ustrchr (s1, c)
     const U_CHAR *s1;
     int c;
{
  return (U_CHAR *) strchr ((const char *)s1, c);
}

static inline int
ufputs (s, f)
     const U_CHAR *s;
     FILE *f;
{
  return fputs ((const char *)s, f);
}

#endif /* ! GCC_CPPHASH_H */
