/* Part of CPP library.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

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

#ifndef __GCC_CPPHASH__
#define __GCC_CPPHASH__

/* Test if a sign is valid within a preprocessing number.  */
#define VALID_SIGN(c, prevc) \
  (((c) == '+' || (c) == '-') && \
   ((prevc) == 'e' || (prevc) == 'E' \
    || (((prevc) == 'p' || (prevc) == 'P') \
        && CPP_OPTION (pfile, extended_numbers))))

/* Memory pools.  */
#define ALIGN(size, align) (((size) + ((align) - 1)) & ~((align) - 1))
#define POOL_FRONT(p) ((p)->cur->front)
#define POOL_LIMIT(p) ((p)->cur->limit)
#define POOL_BASE(p)  ((p)->cur->base)
#define POOL_SIZE(p)  ((p)->cur->limit - (p)->cur->base)
#define POOL_ROOM(p)  ((p)->cur->limit - (p)->cur->front)
#define POOL_USED(p)  ((p)->cur->front - (p)->cur->base)
#define POOL_COMMIT(p, len) do {((p)->cur->front += ALIGN (len, (p)->align));\
  if ((p)->cur->front > (p)->cur->limit) abort ();} while (0)

typedef struct cpp_chunk cpp_chunk;
struct cpp_chunk
{
  cpp_chunk *next;
  unsigned char *front;
  unsigned char *limit;
  unsigned char *base;
};

/* List of directories to look for include files in. */
struct file_name_list
{
  struct file_name_list *next;
  struct file_name_list *alloc; /* for the cache of
				   current directory entries */
  char *name;
  unsigned int nlen;
  /* We use these to tell if the directory mentioned here is a duplicate
     of an earlier directory on the search path. */
  ino_t ino;
  dev_t dev;
  /* If the following is nonzero, it is a C-language system include
     directory.  */
  int sysp;
  /* Mapping of file names for this directory.
     Only used on MS-DOS and related platforms. */
  struct file_name_map *name_map;
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

  /* Actual directory of this file, used only for "" includes */
  struct file_name_list *actual_dir;

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

  /* Buffer type.  */
  ENUM_BITFIELD (cpp_buffer_type) type : 8;
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

#define CPP_PREV_BUFFER(BUFFER) ((BUFFER)->prev)
#define CPP_PRINT_DEPS(PFILE) CPP_OPTION (PFILE, print_deps)
#define CPP_IN_SYSTEM_HEADER(PFILE) \
  (CPP_BUFFER (PFILE) && CPP_BUFFER (PFILE)->sysp)
#define CPP_PEDANTIC(PF) CPP_OPTION (PF, pedantic)
#define CPP_WTRADITIONAL(PF) CPP_OPTION (PF, warn_traditional)

/* Hash step.  The hash calculation is duplicated in cpp_lookup and
   parse_name.  */
#define HASHSTEP(r, c) ((r) * 67 + (c - 113));

/* In cpperror.c  */
enum error_type { WARNING = 0, PEDWARN, ERROR, FATAL, ICE };
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
extern void _cpp_init_hashtable		PARAMS ((cpp_reader *));
extern void _cpp_cleanup_hashtable	PARAMS ((cpp_reader *));
extern cpp_hashnode *_cpp_lookup_with_hash PARAMS ((cpp_reader*, size_t,
						    unsigned int));

/* In cppfiles.c */
extern void _cpp_never_reread		PARAMS ((struct include_file *));
extern void _cpp_simplify_pathname	PARAMS ((char *));
extern int _cpp_read_file		PARAMS ((cpp_reader *, const char *));
extern void _cpp_execute_include	PARAMS ((cpp_reader *,
						 const cpp_token *, int, int));
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
extern void _cpp_init_stacks	PARAMS ((cpp_reader *));
extern void _cpp_cleanup_stacks	PARAMS ((cpp_reader *));
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

#endif
