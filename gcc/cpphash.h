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

typedef unsigned char U_CHAR;
#define U (const U_CHAR *)  /* Intended use: U"string" */

/* Tokens with SPELL_STRING store their spelling in the token list,
   and it's length in the token->val.name.len.  */
enum spell_type
{
  SPELL_OPERATOR = 0,
  SPELL_CHAR,
  SPELL_IDENT,
  SPELL_STRING,
  SPELL_NONE
};

struct token_spelling
{
  ENUM_BITFIELD(spell_type) type : CHAR_BIT;
  const U_CHAR *spelling;
};

extern const struct token_spelling token_spellings[];
#define TOKEN_SPELL(token) (token_spellings[(token)->type].type)

/* Chained list of answers to an assertion.  */
struct answer
{
  struct answer *next;
  cpp_toklist list;
};
#define FREE_ANSWER(answer) do {_cpp_free_toklist (&answer->list); \
				free (answer); } while (0)

/* Values for the origin field of struct directive.  KANDR directives
   come from traditional (K&R) C.  STDC89 directives come from the
   1989 C standard.  EXTENSION directives are extensions.  */
#define KANDR		0
#define STDC89		1
#define EXTENSION	2

/* Values for the flags field of struct directive.  COND indicates a
   conditional.  EXPAND means that macros are to be expanded on the
   directive line.  INCL means to treat "..." and <...> as
   q-char-sequence and h-char-sequence respectively.  COMMENTS means
   preserve comments in the directive if -C.  */
#define COND		(1 << 0)
#define EXPAND   	(1 << 1)
#define INCL		(1 << 2)
#define COMMENTS	(1 << 3)

/* Defines one #-directive, including how to handle it.  */
typedef void (*directive_handler) PARAMS ((cpp_reader *));
struct directive
{
  directive_handler handler;	/* Function to handle directive.  */
  const U_CHAR *name;		/* Name of directive.  */
  unsigned short length;	/* Length of name.  */
  unsigned char origin;		/* Origin of directive.  */
  unsigned char flags;	        /* Flags describing this directive.  */
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
  int fd;			/* file descriptor possibly open on file */
  unsigned short include_count;	/* number of times file has been read */
  unsigned short sysp;		/* file is a system header */
  time_t  date;                 /* modification date of file, if known */
};

/* Special nodes - identifiers with predefined significance.
   Note that the array length of dirs[] must be kept in sync with
   cpplib.c's dtable[].  */
struct spec_nodes
{
  cpp_hashnode *n_L;			/* L"str" */
  cpp_hashnode *n_defined;		/* #if defined */
  cpp_hashnode *n__STRICT_ANSI__;	/* STDC_0_IN_SYSTEM_HEADERS */
  cpp_hashnode *n__CHAR_UNSIGNED__;	/* plain char is unsigned */
  cpp_hashnode *n__VA_ARGS__;		/* C99 vararg macros */
  cpp_hashnode *dirs[19];		/* 19 directives counting #sccs */
};


/* The cmacro works like this: If it's NULL, the file is to be
   included again.  If it's NEVER_REREAD, the file is never to be
   included again.  Otherwise it is a macro hashnode, and the file is
   to be included again if the macro is not defined.  */
#define NEVER_REREAD ((const cpp_hashnode *)-1)
#define DO_NOT_REREAD(inc) \
((inc)->cmacro && \
 ((inc)->cmacro == NEVER_REREAD || (inc)->cmacro->type != T_VOID))

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

/* This table is constant if it can be initialized at compile time,
   which is the case if cpp was compiled with GCC >=2.7, or another
   compiler that supports C99.  */
#if (GCC_VERSION >= 2007) || (__STDC_VERSION__ >= 199901L)
extern const unsigned char _cpp_IStable[256];
#else
extern unsigned char _cpp_IStable[256];
#endif

/* Macros.  */

/* Make sure PFILE->token_buffer has space for at least N more characters. */
#define CPP_RESERVE(PFILE, N) \
  (CPP_WRITTEN (PFILE) + (size_t)(N) > (PFILE)->token_buffer_size \
   && (_cpp_grow_token_buffer (PFILE, N), 0))

/* Append string STR (of length N) to PFILE's output buffer.
   Assume there is enough space. */
#define CPP_PUTS_Q(PFILE, STR, N) \
  (memcpy ((PFILE)->limit, STR, (N)), (PFILE)->limit += (N))
/* Append string STR (of length N) to PFILE's output buffer.  Make space. */
#define CPP_PUTS(PFILE, STR, N) CPP_RESERVE(PFILE, N), CPP_PUTS_Q(PFILE, STR,N)
/* Append character CH to PFILE's output buffer.  Assume sufficient space. */
#define CPP_PUTC_Q(PFILE, CH) (*(PFILE)->limit++ = (CH))
/* Append character CH to PFILE's output buffer.  Make space if need be. */
#define CPP_PUTC(PFILE, CH) (CPP_RESERVE (PFILE, 1), CPP_PUTC_Q (PFILE, CH))

#define CPP_PREV_BUFFER(BUFFER) ((BUFFER)->prev)
#define CPP_PRINT_DEPS(PFILE) CPP_OPTION (PFILE, print_deps)
#define CPP_IN_SYSTEM_HEADER(PFILE) \
  (CPP_BUFFER (PFILE) && CPP_BUFFER (PFILE)->inc \
   && CPP_BUFFER (PFILE)->inc->sysp)
#define CPP_PEDANTIC(PF) \
  (CPP_OPTION (PF, pedantic) && !CPP_IN_SYSTEM_HEADER (PF))
#define CPP_WTRADITIONAL(PF) \
  (CPP_OPTION (PF, warn_traditional) && !CPP_IN_SYSTEM_HEADER (PF))

/* Hash step.  The hash calculation is duplicated in cpp_lookup and
   parse_name.  */
#define HASHSTEP(r, str) ((r) * 67 + (*str - 113));

/* Flags for _cpp_init_toklist.  */
#define DUMMY_TOKEN     0
#define NO_DUMMY_TOKEN	1

/* In cppmacro.c */
extern void _cpp_free_definition	PARAMS ((cpp_hashnode *));
extern int _cpp_create_definition	PARAMS ((cpp_reader *, cpp_hashnode *));
extern void _cpp_dump_definition	PARAMS ((cpp_reader *, cpp_hashnode *));

/* In cpphash.c */
extern void _cpp_init_macros		PARAMS ((cpp_reader *));
extern void _cpp_cleanup_macros		PARAMS ((cpp_reader *));
extern cpp_hashnode *_cpp_lookup_with_hash PARAMS ((cpp_reader*, const U_CHAR *,
						    size_t, unsigned int));

/* In cppfiles.c */
extern void _cpp_simplify_pathname	PARAMS ((char *));
extern void _cpp_execute_include	PARAMS ((cpp_reader *, const U_CHAR *,
						 unsigned int, int,
						 struct file_name_list *,
						 int));
extern int _cpp_compare_file_date       PARAMS ((cpp_reader *, const U_CHAR *,
                                                 unsigned int, int));
extern void _cpp_report_missing_guards	PARAMS ((cpp_reader *));
extern void _cpp_init_includes		PARAMS ((cpp_reader *));
extern void _cpp_cleanup_includes	PARAMS ((cpp_reader *));
extern const char *_cpp_fake_include	PARAMS ((cpp_reader *, const char *));
extern void _cpp_pop_file_buffer	PARAMS ((cpp_reader *, cpp_buffer *));

/* In cppexp.c */
extern int _cpp_parse_expr		PARAMS ((cpp_reader *));

/* In cpplex.c */
extern void _cpp_skip_rest_of_line	PARAMS ((cpp_reader *));
extern void _cpp_free_temp_tokens	PARAMS ((cpp_reader *));
extern void _cpp_init_input_buffer	PARAMS ((cpp_reader *));
extern void _cpp_grow_token_buffer	PARAMS ((cpp_reader *, long));
extern void _cpp_init_toklist		PARAMS ((cpp_toklist *, int));
extern void _cpp_clear_toklist		PARAMS ((cpp_toklist *));
extern void _cpp_free_toklist		PARAMS ((const cpp_toklist *));
extern int _cpp_equiv_tokens		PARAMS ((const cpp_token *,
						 const cpp_token *));
extern int _cpp_equiv_toklists		PARAMS ((const cpp_toklist *,
						 const cpp_toklist *));
extern void _cpp_expand_token_space	PARAMS ((cpp_toklist *, unsigned int));
extern void _cpp_reserve_name_space	PARAMS ((cpp_toklist *, unsigned int));
extern void _cpp_expand_name_space	PARAMS ((cpp_toklist *, unsigned int));
extern void _cpp_dump_list		PARAMS ((cpp_reader *,
						 const cpp_toklist *,
						 const cpp_token *, int));
extern int _cpp_equiv_tokens		PARAMS ((const cpp_token *,
						 const cpp_token *));
extern void _cpp_run_directive		PARAMS ((cpp_reader *,
						 const struct directive *,
						 const char *, size_t));
extern unsigned int _cpp_get_line	PARAMS ((cpp_reader *,
						 unsigned int *));
extern const cpp_token *_cpp_get_token PARAMS ((cpp_reader *));
extern const cpp_token *_cpp_get_raw_token PARAMS ((cpp_reader *));
extern void _cpp_push_token PARAMS ((cpp_reader *, const cpp_token*));
extern const cpp_token *_cpp_glue_header_name PARAMS ((cpp_reader *));
extern const U_CHAR *_cpp_spell_operator PARAMS ((enum cpp_ttype));

/* In cpplib.c */
extern const struct directive *_cpp_check_directive
			PARAMS ((cpp_reader *, const cpp_token *, int));
extern const struct directive *_cpp_check_linemarker
			PARAMS ((cpp_reader *, const cpp_token *, int));
extern cpp_hashnode *_cpp_parse_assertion PARAMS ((cpp_reader *,
						    struct answer **));
extern struct answer **_cpp_find_answer	PARAMS ((cpp_hashnode *,
						 const cpp_toklist *));
extern void _cpp_init_stacks	PARAMS ((cpp_reader *));
extern void _cpp_cleanup_stacks	PARAMS ((cpp_reader *));

/* Utility routines and macros.  */
#define xnew(T)		(T *) xmalloc (sizeof(T))
#define xnewvec(T, N)	(T *) xmalloc (sizeof(T) * (N))
#define xcnewvec(T, N)	(T *) xcalloc (N, sizeof(T))
#define xobnew(O, T)	(T *) obstack_alloc (O, sizeof(T))

/* These are inline functions instead of macros so we can get type
   checking.  */

static inline int ustrcmp	PARAMS ((const U_CHAR *, const U_CHAR *));
static inline int ustrncmp	PARAMS ((const U_CHAR *, const U_CHAR *,
					 size_t));
static inline size_t ustrlen	PARAMS ((const U_CHAR *));
static inline U_CHAR *uxstrdup	PARAMS ((const U_CHAR *));
static inline U_CHAR *ustrchr	PARAMS ((const U_CHAR *, int));

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

#endif
