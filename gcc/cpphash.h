/* Part of CPP library.
   Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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
#define CPP_BUF_LINE(BUF) ((BUF)->lineno)
#define CPP_BUF_COLUMN(BUF, CUR) ((CUR) - (BUF)->line_base + (BUF)->col_adjust)
#define CPP_BUF_COL(BUF) CPP_BUF_COLUMN(BUF, (BUF)->cur)

/* Maximum nesting of cpp_buffers.  We use a static limit, partly for
   efficiency, and partly to limit runaway recursion.  */
#define CPP_STACK_MAX 200

/* Memory pools.  */
#define POOL_ALIGN(size, align) (((size) + ((align) - 1)) & ~((align) - 1))
#define POOL_FRONT(p) ((p)->cur->front)
#define POOL_LIMIT(p) ((p)->cur->limit)
#define POOL_BASE(p)  ((p)->cur->base)
#define POOL_SIZE(p)  ((p)->cur->limit - (p)->cur->base)
#define POOL_ROOM(p)  ((p)->cur->limit - (p)->cur->front)
#define POOL_USED(p)  ((p)->cur->front - (p)->cur->base)
#define POOL_COMMIT(p, len) do {\
  ((p)->cur->front += POOL_ALIGN (len, (p)->align));\
  if ((p)->cur->front > (p)->cur->limit) abort ();} while (0)

typedef struct cpp_chunk cpp_chunk;
struct cpp_chunk
{
  cpp_chunk *next;
  unsigned char *front;
  unsigned char *limit;
  unsigned char *base;
};

typedef struct cpp_pool cpp_pool;
struct cpp_pool
{
  struct cpp_chunk *cur, *locked;
  unsigned char *pos;		/* Current position.  */
  unsigned int align;
  unsigned int locks;
};

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

/* Multiple-include optimisation.  */
enum mi_state {MI_FAILED = 0, MI_OUTSIDE};
enum mi_ind {MI_IND_NONE = 0, MI_IND_NOT};

/* #include types.  */
enum include_type {IT_INCLUDE, IT_INCLUDE_NEXT, IT_IMPORT, IT_CMDLINE};

typedef struct toklist toklist;
struct toklist
{
  cpp_token *first;
  cpp_token *limit;
};

typedef struct cpp_context cpp_context;
struct cpp_context
{
  /* Doubly-linked list.  */
  cpp_context *next, *prev;

  /* Contexts other than the base context are contiguous tokens.
     e.g. macro expansions, expanded argument tokens.  */
  struct toklist list;

  /* For a macro context, these are the macro and its arguments.  */
  cpp_macro *macro;
};

struct lexer_state
{
  /* Nonzero if first token on line is CPP_HASH.  */
  unsigned char in_directive;

  /* Nonzero if in a directive that takes angle-bracketed headers.  */
  unsigned char angled_headers;

  /* Nonzero to save comments.  Turned off if discard_comments, and in
     all directives apart from #define.  */
  unsigned char save_comments;

  /* If nonzero the next token is at the beginning of the line.  */
  unsigned char next_bol;

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
  cpp_hashnode *n_L;			/* L"str" */
  cpp_hashnode *n_defined;		/* defined operator */
  cpp_hashnode *n_true;			/* C++ keyword true */
  cpp_hashnode *n_false;		/* C++ keyword false */
  cpp_hashnode *n__Pragma;		/* _Pragma operator */
  cpp_hashnode *n__STRICT_ANSI__;	/* STDC_0_IN_SYSTEM_HEADERS */
  cpp_hashnode *n__CHAR_UNSIGNED__;	/* plain char is unsigned */
  cpp_hashnode *n__VA_ARGS__;		/* C99 vararg macros */
};

struct cpp_buffer
{
  const unsigned char *cur;	 /* current position */
  const unsigned char *rlimit; /* end of valid data */
  const unsigned char *line_base; /* start of current line */
  cppchar_t read_ahead;		/* read ahead character */
  cppchar_t extra_char;		/* extra read-ahead for long tokens.  */

  struct cpp_reader *pfile;	/* Owns this buffer.  */
  struct cpp_buffer *prev;

  const unsigned char *buf;	 /* entire buffer */

  /* Filename specified with #line command.  */
  const char *nominal_fname;

  /* Pointer into the include table.  Used for include_next and
     to record control macros. */
  struct include_file *inc;

  /* Value of if_stack at start of this file.
     Used to prohibit unmatched #endif (etc) in an include file.  */
  struct if_stack *if_stack;

  /* Token column position adjustment owing to tabs in whitespace.  */
  unsigned int col_adjust;

  /* Line number at line_base (above). */
  unsigned int lineno;

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

  /* Temporary storage for pfile->skipping whilst in a directive.  */
  unsigned char was_skipping;

  /* 1 = system header file, 2 = C system header file used for C++.  */
  unsigned char sysp;

  /* Nonzero means we have printed (while error reporting) a list of
     containing files that matches the current status.  */
  unsigned char include_stack_listed;

  /* Nonzero means that the directory to start searching for ""
     include files has been calculated and stored in "dir" below.  */
  unsigned char search_cached;

  /* Buffer type.  */
  ENUM_BITFIELD (cpp_buffer_type) type : 8;

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

  /* The position of the last lexed token and last lexed directive.  */
  cpp_lexer_pos lexer_pos;
  cpp_lexer_pos directive_pos;

  /* Memory pools.  */
  cpp_pool ident_pool;		/* For all identifiers, and permanent
				   numbers and strings.  */
  cpp_pool macro_pool;		/* For macro definitions.  Permanent.  */
  cpp_pool argument_pool;	/* For macro arguments.  Temporary.   */

  /* Context stack.  */
  struct cpp_context base_context;
  struct cpp_context *context;

  /* If in_directive, the directive if known.  */
  const struct directive *directive;

  /* Multiple inlcude optimisation.  */
  enum mi_state mi_state;
  enum mi_ind mi_if_not_defined;
  unsigned int mi_lexed;
  const cpp_hashnode *mi_cmacro;
  const cpp_hashnode *mi_ind_cmacro;

  /* Token lookahead.  */
  struct cpp_lookahead *la_read;	/* Read from this lookahead.  */
  struct cpp_lookahead *la_write;	/* Write to this lookahead.  */
  struct cpp_lookahead *la_unused;	/* Free store.  */
  struct cpp_lookahead *la_saved;	/* Backup when entering directive.  */

  /* Error counter for exit code.  */
  unsigned int errors;

  /* Line and column where a newline was first seen in a string
     constant (multi-line strings).  */
  cpp_lexer_pos mlstring_pos;

  /* Buffer to hold macro definition string.  */
  unsigned char *macro_buffer;
  unsigned int macro_buffer_len;

  /* Current depth in #include directives that use <...>.  */
  unsigned int system_include_depth;

  /* Current depth of buffer stack.  */
  unsigned int buffer_stack_depth;

  /* Current depth in #include directives.  */
  unsigned int include_depth;

  /* Tree of other included files.  See cppfiles.c.  */
  struct splay_tree_s *all_include_files;

  /* Current maximum length of directory names in the search path
     for include files.  (Altered as we get more of them.)  */
  unsigned int max_include_len;

  /* Date and time tokens.  Calculated together if either is requested.  */
  cpp_token date;
  cpp_token time;

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

  /* We're printed a warning recommending against using #import.  */
  unsigned char import_warning;

  /* True if we are skipping a failed conditional group.  */
  unsigned char skipping;

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
#define CPP_IN_SYSTEM_HEADER(PFILE) \
  (CPP_BUFFER (PFILE) && CPP_BUFFER (PFILE)->sysp)
#define CPP_PEDANTIC(PF) CPP_OPTION (PF, pedantic)
#define CPP_WTRADITIONAL(PF) CPP_OPTION (PF, warn_traditional)

/* In cpperror.c  */
enum error_type { WARNING = 0, WARNING_SYSHDR, PEDWARN, ERROR, FATAL, ICE };
extern int _cpp_begin_message PARAMS ((cpp_reader *, enum error_type,
				       const char *, const cpp_lexer_pos *));

/* In cppmacro.c */
extern void _cpp_free_definition	PARAMS ((cpp_hashnode *));
extern int _cpp_create_definition	PARAMS ((cpp_reader *, cpp_hashnode *));
extern void _cpp_pop_context		PARAMS ((cpp_reader *));
extern void _cpp_free_lookaheads	PARAMS ((cpp_reader *));
extern void _cpp_release_lookahead	PARAMS ((cpp_reader *));
extern void _cpp_push_token		PARAMS ((cpp_reader *, const cpp_token *,
						 const cpp_lexer_pos *));

/* In cpphash.c */
extern void _cpp_init_hashtable		PARAMS ((cpp_reader *, hash_table *));
extern void _cpp_destroy_hashtable	PARAMS ((cpp_reader *));

/* In cppfiles.c */
extern void _cpp_fake_include		PARAMS ((cpp_reader *, const char *));
extern void _cpp_never_reread		PARAMS ((struct include_file *));
extern char *_cpp_simplify_pathname	PARAMS ((char *));
extern int _cpp_read_file		PARAMS ((cpp_reader *, const char *));
extern int _cpp_execute_include		PARAMS ((cpp_reader *,
						 const cpp_token *,
						 enum include_type));
extern int _cpp_compare_file_date       PARAMS ((cpp_reader *,
						 const cpp_token *));
extern void _cpp_report_missing_guards	PARAMS ((cpp_reader *));
extern void _cpp_init_includes		PARAMS ((cpp_reader *));
extern void _cpp_cleanup_includes	PARAMS ((cpp_reader *));
extern void _cpp_pop_file_buffer	PARAMS ((cpp_reader *, cpp_buffer *));

/* In cppexp.c */
extern int _cpp_parse_expr		PARAMS ((cpp_reader *));

/* In cpplex.c */
extern void _cpp_lex_token		PARAMS ((cpp_reader *, cpp_token *));
extern int _cpp_equiv_tokens		PARAMS ((const cpp_token *,
						 const cpp_token *));
extern void _cpp_init_pool		PARAMS ((cpp_pool *, unsigned int,
						  unsigned int, unsigned int));
extern void _cpp_free_pool		PARAMS ((cpp_pool *));
extern unsigned char *_cpp_pool_reserve PARAMS ((cpp_pool *, unsigned int));
extern unsigned char *_cpp_pool_alloc	PARAMS ((cpp_pool *, unsigned int));
extern unsigned char *_cpp_next_chunk	PARAMS ((cpp_pool *, unsigned int,
						 unsigned char **));
extern void _cpp_lock_pool		PARAMS ((cpp_pool *));
extern void _cpp_unlock_pool		PARAMS ((cpp_pool *));

/* In cpplib.c */
extern int _cpp_test_assertion PARAMS ((cpp_reader *, int *));
extern int _cpp_handle_directive PARAMS ((cpp_reader *, int));
extern void _cpp_define_builtin	PARAMS ((cpp_reader *, const char *));
extern void _cpp_do__Pragma	PARAMS ((cpp_reader *));
extern void _cpp_init_directives PARAMS ((cpp_reader *));
extern void _cpp_init_internal_pragmas PARAMS ((cpp_reader *));
extern void _cpp_do_file_change PARAMS ((cpp_reader *, enum cpp_fc_reason,
					 const char *, unsigned int));

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
