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
#define ABSOLUTE_PATH ((struct file_name_list *)-1)

/* This structure is used for the table of all includes.  */
struct include_file
{
  const char *name;		/* actual path name of file */
  const cpp_hashnode *cmacro;	/* macro, if any, preventing reinclusion.  */
  const struct file_name_list *foundhere;
				/* location in search path where file was
				   found, for #include_next */
  const unsigned char *buffer;	/* pointer to cached file contents */
  struct stat st;		/* copy of stat(2) data for file */
  int fd;			/* fd open on file (short term storage only) */
  unsigned short include_count;	/* number of times file has been read */
  unsigned short refcnt;	/* number of stacked buffers using this file */
  unsigned char sysp;		/* file is a system header */
  unsigned char mapped;		/* file buffer is mmapped */
  unsigned char defined;	/* cmacro prevents inclusion in this state */
};

/* The cmacro works like this: If it's NULL, the file is to be
   included again.  If it's NEVER_REREAD, the file is never to be
   included again.  Otherwise it is a macro hashnode, and the file is
   to be included again if the macro is defined or not as specified by
   DEFINED.  */
#define NEVER_REREAD ((const cpp_hashnode *)-1)
#define DO_NOT_REREAD(inc) \
((inc)->cmacro && ((inc)->cmacro == NEVER_REREAD \
		   || ((inc)->cmacro->type == NT_MACRO) == (inc)->defined))

/* Character classes.
   If the definition of `numchar' looks odd to you, please look up the
   definition of a pp-number in the C standard [section 6.4.8 of C99].

   In the unlikely event that characters other than \r and \n enter
   the set is_vspace, the macro handle_newline() in cpplex.c must be
   updated.  */
#define ISidnum		0x01	/* a-zA-Z0-9_ */
#define ISidstart	0x02	/* _a-zA-Z */
#define ISnumstart	0x04	/* 0-9 */
#define IShspace	0x08	/* ' ' \t */
#define ISvspace	0x10	/* \r \n */
#define ISspace		0x20	/* ' ' \t \r \n \f \v \0 */

#define _dollar_ok(x)	((x) == '$' && CPP_OPTION (pfile, dollars_in_ident))

#define is_idchar(x)	((_cpp_IStable[x] & ISidnum) || _dollar_ok(x))
#define is_idstart(x)	((_cpp_IStable[x] & ISidstart) || _dollar_ok(x))
#define is_numchar(x)	(_cpp_IStable[x] & ISidnum)
#define is_numstart(x)	(_cpp_IStable[x] & ISnumstart)
#define is_hspace(x)	(_cpp_IStable[x] & IShspace)
#define is_vspace(x)	(_cpp_IStable[x] & ISvspace)
#define is_nvspace(x)	((_cpp_IStable[x] & (ISspace | ISvspace)) == ISspace)
#define is_space(x)	(_cpp_IStable[x] & ISspace)

/* These tables are constant if they can be initialized at compile time,
   which is the case if cpp was compiled with GCC >=2.7, or another
   compiler that supports C99.  */
#if HAVE_DESIGNATED_INITIALIZERS
extern const unsigned char _cpp_IStable[UCHAR_MAX + 1];
extern const unsigned char _cpp_trigraph_map[UCHAR_MAX + 1];
#else
extern unsigned char _cpp_IStable[UCHAR_MAX + 1];
extern unsigned char _cpp_trigraph_map[UCHAR_MAX + 1];
#endif

/* Macros.  */

#define CPP_PREV_BUFFER(BUFFER) ((BUFFER)->prev)
#define CPP_PRINT_DEPS(PFILE) CPP_OPTION (PFILE, print_deps)
#define CPP_IN_SYSTEM_HEADER(PFILE) \
  (CPP_BUFFER (PFILE) && CPP_BUFFER (PFILE)->inc \
   && CPP_BUFFER (PFILE)->inc->sysp)
#define CPP_PEDANTIC(PF) \
  CPP_OPTION (PF, pedantic)
#define CPP_WTRADITIONAL(PF) \
  CPP_OPTION (PF, warn_traditional)

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
extern void _cpp_simplify_pathname	PARAMS ((char *));
extern void _cpp_execute_include	PARAMS ((cpp_reader *,
						 const cpp_token *, int,
						 struct file_name_list *));
extern int _cpp_compare_file_date       PARAMS ((cpp_reader *,
						 const cpp_token *));
extern void _cpp_report_missing_guards	PARAMS ((cpp_reader *));
extern void _cpp_init_includes		PARAMS ((cpp_reader *));
extern void _cpp_cleanup_includes	PARAMS ((cpp_reader *));
extern const char *_cpp_fake_include	PARAMS ((cpp_reader *, const char *));
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

/* Utility routines and macros.  */
#define DSC(str) (const U_CHAR *)str, sizeof str - 1
#define xnew(T)		(T *) xmalloc (sizeof(T))
#define xcnew(T)	(T *) xcalloc (1, sizeof(T))
#define xnewvec(T, N)	(T *) xmalloc (sizeof(T) * (N))
#define xcnewvec(T, N)	(T *) xcalloc (N, sizeof(T))
#define xobnew(O, T)	(T *) obstack_alloc (O, sizeof(T))

#endif
