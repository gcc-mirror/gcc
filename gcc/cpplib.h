/* Definitions for CPP library.
   Copyright (C) 1995, 96-99, 2000 Free Software Foundation, Inc.
   Written by Per Bothner, 1994-95.

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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */
#ifndef __GCC_CPPLIB__
#define __GCC_CPPLIB__

#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct cpp_reader cpp_reader;
typedef struct cpp_buffer cpp_buffer;
typedef struct cpp_options cpp_options;
typedef struct cpp_printer cpp_printer;
typedef struct cpp_token cpp_token;
typedef struct cpp_toklist cpp_toklist;
typedef struct cpp_name cpp_name;

/* The first two groups, apart from '=', can appear in preprocessor
   expressions.  This allows a lookup table to be implemented in
   _cpp_parse_expr.

   The first group, to CPP_LAST_EQ, can be immediately followed by an
   '='.  The lexer needs operators ending in '=', like ">>=", to be in
   the same order as their counterparts without the '=', like ">>".  */

/* Positions in the table.  */
#define CPP_LAST_EQ CPP_LSHIFT
#define CPP_FIRST_DIGRAPH CPP_HASH

#define TTYPE_TABLE				\
  T(CPP_EQ = 0,		"=")			\
  T(CPP_NOT,		"!")			\
  T(CPP_GREATER,	">")	/* compare */	\
  T(CPP_LESS,		"<")			\
  T(CPP_PLUS,		"+")	/* math */	\
  T(CPP_MINUS,		"-")			\
  T(CPP_MULT,		"*")			\
  T(CPP_DIV,		"/")			\
  T(CPP_MOD,		"%")			\
  T(CPP_AND,		"&")	/* bit ops */	\
  T(CPP_OR,		"|")			\
  T(CPP_XOR,		"^")			\
  T(CPP_RSHIFT,		">>")			\
  T(CPP_LSHIFT,		"<<")			\
\
  T(CPP_COMPL,		"~")			\
  T(CPP_AND_AND,	"&&")	/* logical */	\
  T(CPP_OR_OR,		"||")			\
  T(CPP_QUERY,		"?")			\
  T(CPP_COLON,		":")			\
  T(CPP_COMMA,		",")	/* grouping */	\
  T(CPP_OPEN_PAREN,	"(")			\
  T(CPP_CLOSE_PAREN,	")")			\
  T(CPP_EQ_EQ,		"==")	/* compare */	\
  T(CPP_NOT_EQ,		"!=")			\
  T(CPP_GREATER_EQ,	">=")			\
  T(CPP_LESS_EQ,	"<=")			\
\
  T(CPP_PLUS_EQ,	"+=")	/* math */	\
  T(CPP_MINUS_EQ,	"-=")			\
  T(CPP_MULT_EQ,	"*=")			\
  T(CPP_DIV_EQ,		"/=")			\
  T(CPP_MOD_EQ,		"%=")			\
  T(CPP_AND_EQ,		"&=")	/* bit ops */	\
  T(CPP_OR_EQ,		"|=")			\
  T(CPP_XOR_EQ,		"^=")			\
  T(CPP_RSHIFT_EQ,	">>=")			\
  T(CPP_LSHIFT_EQ,	"<<=")			\
  /* Digraphs together, beginning with CPP_FIRST_DIGRAPH.  */	\
  T(CPP_HASH,		"#")	/* digraphs */	\
  T(CPP_PASTE,		"##")			\
  T(CPP_OPEN_SQUARE,	"[")			\
  T(CPP_CLOSE_SQUARE,	"]")			\
  T(CPP_OPEN_BRACE,	"{")			\
  T(CPP_CLOSE_BRACE,	"}")			\
  /* The remainder of the punctuation.  Order is not significant. */	\
  T(CPP_SEMICOLON,	";")	/* structure */	\
  T(CPP_ELLIPSIS,	"...")			\
  T(CPP_BACKSLASH,	"\\")			\
  T(CPP_PLUS_PLUS,	"++")	/* increment */	\
  T(CPP_MINUS_MINUS,	"--")			\
  T(CPP_DEREF,		"->")	/* accessors */	\
  T(CPP_DOT,		".")			\
  T(CPP_SCOPE,		"::")			\
  T(CPP_DEREF_STAR,	"->*")			\
  T(CPP_DOT_STAR,	".*")			\
  T(CPP_MIN,		"<?")	/* extension */	\
  T(CPP_MAX,		">?")			\
  C(CPP_OTHER,		0)	/* stray punctuation */ \
\
  H(CPP_NAME,		spell_name)	/* word */	\
  N(CPP_INT,		0)		/* 23 */	\
  N(CPP_FLOAT,		0)		/* 3.14159 */	\
  H(CPP_NUMBER,		spell_name)	/* 34_be+ta  */	\
  H(CPP_CHAR,		spell_string)	/* 'char' */	\
  H(CPP_WCHAR,		spell_string)	/* L'char' */	\
  H(CPP_STRING,		spell_string)	/* "string" */	\
  H(CPP_WSTRING,	spell_string)	/* L"string" */	\
\
  H(CPP_C_COMMENT,	spell_comment)	/* Only if output comments.  */ \
  H(CPP_CPP_COMMENT,	spell_comment)	/* Only if output comments.  */ \
  H(CPP_CHILL_COMMENT,	spell_comment)	/* Only if output comments.  */ \
  N(CPP_MACRO_ARG,      0)              /* Macro argument.  */          \
  N(CPP_SUBLIST,        0)	        /* Sublist.  */                 \
  E(CPP_VSPACE,		"\n")		/* End of line.  */		\
  N(CPP_EOF,		0)		/* End of file.  */		\
  N(CPP_HEADER_NAME,	0)		/* <stdio.h> in #include */	\
  N(CPP_ASSERTION,	0)		/* (...) in #assert */		\
\
  /* Obsolete - will be removed when no code uses them still.  */	\
  H(CPP_COMMENT,	0)		/* Only if output comments.  */ \
  N(CPP_HSPACE,		0)		/* Horizontal white space.  */	\
  N(CPP_POP,		0)		/* End of buffer.  */		\
  N(CPP_DIRECTIVE,	0)		/* #define and the like */	\
  N(CPP_MACRO,		0)		/* Like a NAME, but expanded.  */

#define T(e, s) e,
#define H(e, s) e,
#define C(e, s) e,
#define N(e, s) e,
#define E(e, s) e,
enum cpp_ttype
{
  TTYPE_TABLE
  N_TTYPES
};
#undef T
#undef H
#undef C
#undef N
#undef E

/* Payload of a NAME, NUMBER, FLOAT, STRING, or COMMENT token.  */
struct cpp_name
{
  unsigned int len;
  unsigned int offset;		/* from list->namebuf */
};

#define TOK_NAME(list, token) ((list)->namebuf + (token)->val.name.offset)

/* Flags for the cpp_token structure.  */
#define PREV_WHITESPACE     1	/* If whitespace before this token.  */
#define DIGRAPH             2	/* If it was a digraph.  */
#define UNSIGNED_INT        4   /* If int preprocessing token unsigned.  */

/* A preprocessing token.
   This has been carefully packed and should occupy 16 bytes on
   both 32- and 64-bit hosts.  */
struct cpp_token
{
  unsigned short col;			/* starting column of this token */
#ifdef ENUM_BITFIELDS_ARE_UNSIGNED
  enum cpp_ttype type : CHAR_BIT;	/* node type */
#else
  unsigned char type;
#endif
  unsigned char flags;			/* flags - see above */
  unsigned int aux;			/* CPP_OTHER character.  Hash of a
					   NAME, or something - see uses
					   in the code */
  union
  {
    struct cpp_name name;		/* a string */
    HOST_WIDEST_INT integer;		/* an integer */
  } val;
};

/* Directive flags.  */
#define SYNTAX_INCLUDE (1 << 8)
#define SYNTAX_ASSERT  (1 << 9)

typedef int (*directive_handler) PARAMS ((cpp_reader *));
typedef int (*parse_cleanup_t) PARAMS ((cpp_buffer *, cpp_reader *));

struct cpp_toklist
{
  cpp_token *tokens;		/* actual tokens as an array */
  unsigned int tokens_used;	/* tokens used */
  unsigned int tokens_cap;	/* tokens allocated */

  unsigned char *namebuf;	/* names buffer */
  unsigned int name_used;	/* _bytes_ used */
  unsigned int name_cap;	/* _bytes_ allocated */

  unsigned int line;		/* starting line number */

  /* Comment copying.  */
  cpp_token *comments;		/* comment tokens.  */
  unsigned int comments_used;	/* comment tokens used.  */
  unsigned int comments_cap;	/* comment token capacity.  */

  /* Only used if tokens[0].type == CPP_DIRECTIVE.  This is the
     handler to call after lexing the rest of this line.  The flags
     indicate whether the rest of the line gets special treatment
     during lexing (#include, #if, #assert, #unassert).  */
  directive_handler dir_handler;
  unsigned short dir_flags;
};

struct cpp_buffer
{
  const unsigned char *cur;	 /* current position */
  const unsigned char *rlimit; /* end of valid data */
  const unsigned char *buf;	 /* entire buffer */
  const unsigned char *line_base; /* start of current line */
  const unsigned char *mark;  /* Saved position for lengthy backtrack. */

  struct cpp_buffer *prev;

  /* Filename specified with #line command.  */
  const char *nominal_fname;
  /* Actual directory of this file, used only for "" includes */
  struct file_name_list *actual_dir;

  /* Pointer into the include hash table.  Used for include_next and
     to record control macros. */
  struct ihash *ihash;

  parse_cleanup_t cleanup;

  /* If the buffer is the expansion of a macro, this points to the
     macro's hash table entry.  */
  struct hashnode *macro;

  /* Value of if_stack at start of this file.
     Used to prohibit unmatched #endif (etc) in an include file.  */
  struct if_stack *if_stack;

  /* Line number at line_base (above). */
  unsigned int lineno;

  /* True if this is a header file included using <FILENAME>.  */
  char system_header_p;

  /* True if end-of-file has already been hit once in this buffer.  */
  char seen_eof;

  /* True if buffer contains escape sequences.
     Currently there are two kinds:
     "\r-" means following identifier should not be macro-expanded.
     "\r " means a token-separator.  This turns into " " in final output
          if not stringizing and needed to separate tokens; otherwise nothing.
     Any other two-character sequence beginning with \r is an error.

     If this is NOT set, then \r is a one-character escape meaning backslash
     newline.  This is guaranteed not to occur in the middle of a token.
     The two interpretations of \r do not conflict, because the two-character
     escapes are used only in macro buffers, and backslash-newline is removed
     from macro expansion text in collect_expansion and/or macarg.  */
  char has_escapes;

  /* Used by the C++ frontend to implement redirected input (such as for
     default argument and/or template parsing).  */
  char manual_pop;

  /* True if we have already warned about C++ comments in this file.
     The warning happens only for C89 extended mode with -pedantic on,
     or for -Wtraditional, and only once per file (otherwise it would
     be far too noisy).  */
  char warned_cplusplus_comments;

  /* True if this buffer's data is mmapped.  */
  char mapped;
};

struct file_name_map_list;
struct htab;

/* Maximum nesting of cpp_buffers.  We use a static limit, partly for
   efficiency, and partly to limit runaway recursion.  */
#define CPP_STACK_MAX 200

/* Values for opts.dump_macros.
  dump_only means inhibit output of the preprocessed text
             and instead output the definitions of all user-defined
             macros in a form suitable for use as input to cpp.
   dump_names means pass #define and the macro name through to output.
   dump_definitions means pass the whole definition (plus #define) through
*/
enum { dump_none = 0, dump_only, dump_names, dump_definitions };

/* This structure is nested inside struct cpp_reader, and
   carries all the options visible to the command line.  */
struct cpp_options
{
  /* Name of input and output files.  */
  const char *in_fname;
  const char *out_fname;

  /* Pending options - -D, -U, -A, -I, -ixxx. */
  struct cpp_pending *pending;

  /* File name which deps are being written to.  This is 0 if deps are
     being written to stdout.  */
  const char *deps_file;

  /* Target-name to write with the dependency information.  */
  char *deps_target;

  /* Search paths for include files.  */
  struct file_name_list *quote_include;	 /* First dir to search for "file" */
  struct file_name_list *bracket_include;/* First dir to search for <file> */

  /* Map between header names and file names, used only on DOS where
     file names are limited in length.  */
  struct file_name_map_list *map_list;

  /* Directory prefix that should replace `/usr/lib/gcc-lib/TARGET/VERSION'
     in the standard include file directories.  */
  const char *include_prefix;
  unsigned int include_prefix_len;

  /* Non-0 means -v, so print the full set of include dirs.  */
  unsigned char verbose;

  /* Nonzero means use extra default include directories for C++.  */
  unsigned char cplusplus;

  /* Nonzero means handle cplusplus style comments */
  unsigned char cplusplus_comments;

  /* Nonzero means handle #import, for objective C.  */
  unsigned char objc;

  /* Nonzero means this is an assembly file, so ignore unrecognized
     directives and the "# 33" form of #line, both of which are
     probably comments.  Also, permit unbalanced ' strings (again,
     likely to be in comments).  */
  unsigned char lang_asm;

  /* Nonzero means this is Fortran, and we don't know where the
     comments are, so permit unbalanced ' strings.  Unlike lang_asm,
     this does not ignore unrecognized directives.  */
  unsigned char lang_fortran;

  /* Nonzero means handle CHILL comment syntax and output CHILL string
     delimiters for __DATE__ etc. */
  unsigned char chill;

  /* Nonzero means don't copy comments into the output file.  */
  unsigned char discard_comments;

  /* Nonzero means process the ANSI trigraph sequences.  */
  unsigned char trigraphs;

  /* Nonzero means print the names of included files rather than the
     preprocessed output.  1 means just the #include "...", 2 means
     #include <...> as well.  */
  unsigned char print_deps;

  /* Nonzero if missing .h files in -M output are assumed to be
     generated files and not errors.  */
  unsigned char print_deps_missing_files;

  /* If true, fopen (deps_file, "a") else fopen (deps_file, "w"). */
  unsigned char print_deps_append;

  /* Nonzero means print names of header files (-H).  */
  unsigned char print_include_names;

  /* Nonzero means cpp_pedwarn causes a hard error.  */
  unsigned char pedantic_errors;

  /* Nonzero means don't print warning messages.  */
  unsigned char inhibit_warnings;

  /* Nonzero means don't print error messages.  Has no option to
     select it, but can be set by a user of cpplib (e.g. fix-header).  */
  unsigned char inhibit_errors;

  /* Nonzero means warn if slash-star appears in a comment.  */
  unsigned char warn_comments;

  /* Nonzero means warn if there are any trigraphs.  */
  unsigned char warn_trigraphs;

  /* Nonzero means warn if #import is used.  */
  unsigned char warn_import;

  /* Nonzero means warn if a macro argument is (or would be)
     stringified with -traditional, and warn about directives
     with the # indented from the beginning of the line.  */
  unsigned char warn_traditional;

  /* Nonzero means turn warnings into errors.  */
  unsigned char warnings_are_errors;

  /* Nonzero causes output not to be done, but directives such as
     #define that have side effects are still obeyed.  */
  unsigned char no_output;

  /* Nonzero means we should look for header.gcc files that remap file
     names.  */
  unsigned char remap;

  /* Nonzero means don't output line number information.  */
  unsigned char no_line_commands;

  /* Nonzero means -I- has been seen, so don't look for #include "foo"
     the source-file directory.  */
  unsigned char ignore_srcdir;

  /* Zero means dollar signs are punctuation. */
  unsigned char dollars_in_ident;

  /* Nonzero means try to imitate old fashioned non-ANSI preprocessor.  */
  unsigned char traditional;

  /* Nonzero means warn if undefined identifiers are evaluated in an #if.  */
  unsigned char warn_undef;

  /* Nonzero for the 1989 C Standard, including corrigenda and amendments.  */
  unsigned char c89;

  /* Nonzero for the 1999 C Standard, including corrigenda and amendments.  */
  unsigned char c99;

  /* Nonzero means give all the error messages the ANSI standard requires.  */
  unsigned char pedantic;

  /* Nonzero means we're looking at already preprocessed code, so don't
     bother trying to do macro expansion and whatnot.  */
  unsigned char preprocessed;

  /* Nonzero disables all the standard directories for headers.  */
  unsigned char no_standard_includes;

  /* Nonzero disables the C++-specific standard directories for headers.  */
  unsigned char no_standard_cplusplus_includes;

  /* Nonzero means dump macros in some fashion - see above.  */
  unsigned char dump_macros;

  /* Nonzero means pass all #define and #undef directives which we
     actually process through to the output stream.  This feature is
     used primarily to allow cc1 to record the #defines and #undefs
     for the sake of debuggers which understand about preprocessor
     macros, but it may also be useful with -E to figure out how
     symbols are defined, and where they are defined.  */
  unsigned char debug_output;

  /* Nonzero means pass #include lines through to the output.  */
  unsigned char dump_includes;

  /* Print column number in error messages.  */
  unsigned char show_column;
};


/* A cpp_reader encapsulates the "state" of a pre-processor run.
   Applying cpp_get_token repeatedly yields a stream of pre-processor
   tokens.  Usually, there is only one cpp_reader object active. */

struct cpp_reader
{
  /* Top of buffer stack.  */
  cpp_buffer *buffer;

  /* Token list used by get_directive_token.  */
  cpp_toklist directbuf;

  /* A buffer used for both for cpp_get_token's output, and also internally. */
  unsigned char *token_buffer;
  /* Allocated size of token_buffer.  CPP_RESERVE allocates space.  */
  unsigned int token_buffer_size;
  /* End of the written part of token_buffer. */
  unsigned char *limit;

  /* Error counter for exit code */
  unsigned int errors;

  /* Line where a newline was first seen in a string constant.  */
  unsigned int multiline_string_line;

  /* Current depth in #include directives that use <...>.  */
  unsigned int system_include_depth;

  /* Current depth of buffer stack. */
  unsigned int buffer_stack_depth;

  /* Hash table of macros and assertions.  See cpphash.c */
  struct htab *hashtab;

  /* Hash table of other included files.  See cppfiles.c */
  struct htab *all_include_files;

  /* Chain of `actual directory' file_name_list entries,
     for "" inclusion. */
  struct file_name_list *actual_dirs;

  /* Current maximum length of directory names in the search path
     for include files.  (Altered as we get more of them.)  */
  unsigned int max_include_len;

  struct if_stack *if_stack;
  const unsigned char *potential_control_macro;

  /* Buffer of -M output.  */
  struct deps *deps;

  /* A buffer used only by read_and_prescan (in cppfiles.c), which is
     allocated once per cpp_reader object to keep it off the stack.  */
  unsigned char *input_buffer;
  size_t input_buffer_len;

  /* User visible options.  */
  struct cpp_options opts;

  /* Nonzero means we have printed (while error reporting) a list of
     containing files that matches the current status.  */
  unsigned char input_stack_listing_current;

  /* If non-zero, macros are not expanded.  */
  unsigned char no_macro_expand;

  /* If non-zero, directives cause a hard error.  Used when parsing
     macro arguments.  */
  unsigned char no_directives;

  /* We're printed a warning recommending against using #import.  */
  unsigned char import_warning;

  /* If true, characters between '<' and '>' are a single (string) token.  */
  unsigned char parsing_include_directive;

  /* If true, # introduces an assertion (see do_assert) */
  unsigned char parsing_if_directive;

  /* If true, # and ## are the STRINGIZE and TOKPASTE operators */
  unsigned char parsing_define_directive;

  /* True if escape sequences (as described for has_escapes in
     parse_buffer) should be emitted.  */
  unsigned char output_escapes;

  /* 0: Have seen non-white-space on this line.
     1: Only seen white space so far on this line.
     2: Only seen white space so far in this file.  */
  unsigned char only_seen_white;

  /* True after cpp_start_read completes.  Used to inhibit some
     warnings while parsing the command line.  */
  unsigned char done_initializing;
};

/* struct cpp_printer encapsulates state used to convert the stream of
   tokens coming from cpp_get_token back into a text file.  Not
   everyone wants to do that, hence we separate the function.  */

struct cpp_printer
{
  FILE *outf;			/* stream to write to */
  const char *last_fname;	/* previous file name */
  unsigned int last_bsd;	/* did we just push? */
  unsigned int lineno;		/* line currently being written */
  unsigned int written;		/* low water mark in token buffer */
};

#define CPP_FATAL_LIMIT 1000
/* True if we have seen a "fatal" error. */
#define CPP_FATAL_ERRORS(READER) ((READER)->errors >= CPP_FATAL_LIMIT)

/* Macros for manipulating the token_buffer. */

/* Number of characters currently in PFILE's output buffer. */
#define CPP_WRITTEN(PFILE) ((size_t)((PFILE)->limit - (PFILE)->token_buffer))
#define CPP_PWRITTEN(PFILE) ((PFILE)->limit)
#define CPP_ADJUST_WRITTEN(PFILE,DELTA) ((PFILE)->limit += (DELTA))
#define CPP_SET_WRITTEN(PFILE,N) ((PFILE)->limit = (PFILE)->token_buffer + (N))

#define CPP_OPTION(PFILE, OPTION) ((PFILE)->opts.OPTION)
#define CPP_BUFFER(PFILE) ((PFILE)->buffer)
#define CPP_BUF_LINE(BUF) ((BUF)->lineno)
#define CPP_BUF_COL(BUF) ((BUF)->cur - (BUF)->line_base)

/* Name under which this program was invoked.  */
extern const char *progname;

extern void _cpp_lex_file PARAMS((cpp_reader *));
extern int cpp_handle_options PARAMS ((cpp_reader *, int, char **));
extern enum cpp_ttype cpp_get_token PARAMS ((cpp_reader *));
extern enum cpp_ttype cpp_get_non_space_token PARAMS ((cpp_reader *));

extern void cpp_reader_init PARAMS ((cpp_reader *));
extern cpp_printer *cpp_printer_init PARAMS ((cpp_reader *, cpp_printer *));
extern int cpp_start_read PARAMS ((cpp_reader *, cpp_printer *, const char *));
extern void cpp_output_tokens PARAMS ((cpp_reader *, cpp_printer *));
extern void cpp_output_list PARAMS ((cpp_reader *, cpp_printer *,
				     const cpp_toklist *));
extern void cpp_finish PARAMS ((cpp_reader *, cpp_printer *));
extern void cpp_cleanup PARAMS ((cpp_reader *));

extern cpp_buffer *cpp_file_buffer PARAMS((cpp_reader *));
extern void cpp_define PARAMS ((cpp_reader *, const char *));
extern void cpp_assert PARAMS ((cpp_reader *, const char *));
extern void cpp_undef  PARAMS ((cpp_reader *, const char *));
extern void cpp_unassert PARAMS ((cpp_reader *, const char *));

extern void cpp_free_token_list PARAMS ((cpp_toklist *));

/* N.B. The error-message-printer prototypes have not been nicely
   formatted because exgettext needs to see 'msgid' on the same line
   as the name of the function in order to work properly.  Only the
   string argument gets a name in an effort to keep the lines from
   getting ridiculously oversized.  */

extern void cpp_ice PARAMS ((cpp_reader *, const char *msgid, ...))
  ATTRIBUTE_PRINTF_2;
extern void cpp_fatal PARAMS ((cpp_reader *, const char *msgid, ...))
  ATTRIBUTE_PRINTF_2;
extern void cpp_error PARAMS ((cpp_reader *, const char *msgid, ...))
  ATTRIBUTE_PRINTF_2;
extern void cpp_warning PARAMS ((cpp_reader *, const char *msgid, ...))
  ATTRIBUTE_PRINTF_2;
extern void cpp_pedwarn PARAMS ((cpp_reader *, const char *msgid, ...))
  ATTRIBUTE_PRINTF_2;
extern void cpp_notice PARAMS ((cpp_reader *, const char *msgid, ...))
  ATTRIBUTE_PRINTF_2;
extern void cpp_error_with_line PARAMS ((cpp_reader *, int, int, const char *msgid, ...))
  ATTRIBUTE_PRINTF_4;
extern void cpp_warning_with_line PARAMS ((cpp_reader *, int, int, const char *msgid, ...))
  ATTRIBUTE_PRINTF_4;
extern void cpp_pedwarn_with_line PARAMS ((cpp_reader *, int, int, const char *msgid, ...))
  ATTRIBUTE_PRINTF_4;
extern void cpp_pedwarn_with_file_and_line PARAMS ((cpp_reader *, const char *, int, int, const char *msgid, ...))
  ATTRIBUTE_PRINTF_5;
extern void cpp_error_from_errno	PARAMS ((cpp_reader *, const char *));
extern void cpp_notice_from_errno	PARAMS ((cpp_reader *, const char *));

/* In cpplex.c */
extern cpp_buffer *cpp_push_buffer	PARAMS ((cpp_reader *,
						 const unsigned char *, long));
extern cpp_buffer *cpp_pop_buffer	PARAMS ((cpp_reader *));
extern void cpp_scan_buffer		PARAMS ((cpp_reader *, cpp_printer *));
extern void cpp_scan_buffer_nooutput	PARAMS ((cpp_reader *));



/* In cpphash.c */
extern int cpp_defined			PARAMS ((cpp_reader *,
						 const unsigned char *, int));

/* In cppfiles.c */
extern int cpp_included			PARAMS ((cpp_reader *, const char *));
extern int cpp_read_file		PARAMS ((cpp_reader *, const char *));

#ifdef __cplusplus
}
#endif
#endif /* __GCC_CPPLIB__ */
