/* C Compatible Compiler Preprocessor (CCCP)
Copyright (C) 1986, 1987, 1989, 2000, 2001 Free Software Foundation, Inc.
                    Written by Paul Rubin, June 1986
		    Adapted to ANSI C, Richard Stallman, Jan 1987
		    Dusted off, polished, and adapted for use as traditional
		    preprocessor only, Zack Weinberg, Jul 2000

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

#include "config.h"
#include "system.h"
#include "version.h"
#include "cppdefault.h"
#include "tradcpp.h"
#include "mkdeps.h"
#include "intl.h"

typedef unsigned char U_CHAR;

/* Name under which this program was invoked.  */

static const char *progname;

/* Current maximum length of directory names in the search path
   for include files.  (Altered as we get more of them.)  */

size_t max_include_len;

/* Nonzero means copy comments into the output file.  */

int put_out_comments = 0;

/* mkdeps.h opaque structure that encapsulates dependency information.  */
struct deps *deps;

/* Nonzero means print the names of included files rather than
   the preprocessed output.  1 means just the #include "...",
   2 means #include <...> as well.  */

int print_deps = 0;

/* Nonzero means print dummy targets for each header file.  */

int print_deps_phony_targets = 0;

/* If true, fopen (deps_file, "a") else fopen (deps_file, "w").  */

int deps_append = 0;

/* File name which deps are being written to.  This is 0 if deps are
   being written to stdout.  */

const char *deps_file = 0;

/* Nonzero if missing .h files in -M output are assumed to be
   generated files and not errors.  */

int deps_missing_files = 0;
       
/* Nonzero means don't output line number information.  */

int no_line_commands;

/* Nonzero means inhibit output of the preprocessed text
   and instead output the definitions of all user-defined macros
   in a form suitable for use as input to cccp.  */

int dump_macros;

/* Nonzero means don't print warning messages.  -w.  */

int inhibit_warnings = 0;

/* Non-0 means don't output the preprocessed program.  */
int inhibit_output = 0;

/* Nonzero means chars are signed.  */
#if DEFAULT_SIGNED_CHAR
int flag_signed_char = 1;
#else
int flag_signed_char = 0;
#endif

/* Nonzero means warn if slash-star appears in a comment.  */

int warn_comments;

/* Nonzero causes output not to be done,
   but directives such as #define that have side effects
   are still obeyed.  */

int no_output;

/* Value of __USER_LABEL_PREFIX__.  Target-dependent, also controlled
   by -f(no-)leading-underscore.  */
static const char *user_label_prefix;

/* I/O buffer structure.
   The `fname' field is nonzero for source files and #include files
   and for the dummy text used for -D and -U.
   It is zero for rescanning results of macro expansion
   and for expanding macro arguments.  */
#define INPUT_STACK_MAX 200
struct file_name_list;
struct file_buf {
  const char *fname;
  int lineno;
  int length;
  U_CHAR *buf;
  U_CHAR *bufp;
  /* Macro that this level is the expansion of.
     Included so that we can reenable the macro
     at the end of this level.  */
  struct hashnode *macro;
  /* Value of if_stack at start of this file.
     Used to prohibit unmatched #endif (etc) in an include file.  */
  struct if_stack *if_stack;
  /* Object to be freed at end of input at this level.  */
  U_CHAR *free_ptr;
  /* Position to start scanning for #include_next in this file.  */
  struct file_name_list *next_header_dir;
} instack[INPUT_STACK_MAX];

typedef struct file_buf FILE_BUF;

/* Current nesting level of input sources.
   `instack[indepth]' is the level currently being read.  */
int indepth = -1;
#define CHECK_DEPTH(code) \
  if (indepth >= (INPUT_STACK_MAX - 1))					\
    {									\
      error_with_line (line_for_error (instack[indepth].lineno),	\
		       "macro or #include recursion too deep");		\
      code;								\
    }

/* Current depth in #include directives that use <...>.  */
int system_include_depth = 0;

/* The output buffer.  Its LENGTH field is the amount of room allocated
   for the buffer, not the number of chars actually present.  To get
   that, subtract outbuf.buf from outbuf.bufp. */

#define OUTBUF_SIZE 10	/* initial size of output buffer */
FILE_BUF outbuf;

/* Grow output buffer OBUF points at
   so it can hold at least NEEDED more chars.  */

#define check_expand(OBUF, NEEDED) do { \
  if ((OBUF)->length - ((OBUF)->bufp - (OBUF)->buf) <= (NEEDED)) \
    grow_outbuf ((OBUF), (NEEDED)); \
 } while (0)

struct file_name_list
  {
    struct file_name_list *next;
    const char *fname;
  };

struct file_name_list *include = 0;	/* First dir to search */
	/* First dir to search for <file> */
struct file_name_list *first_bracket_include = 0;
struct file_name_list *last_include = 0;	/* Last in chain */

/* List of included files that contained #once.  */
struct file_name_list *dont_repeat_files = 0;

/* List of other included files.  */
struct file_name_list *all_include_files = 0;

/* Structure allocated for every #define.  For a simple replacement
   such as
   	#define foo bar ,
   nargs = -1, the `pattern' list is null, and the expansion is just
   the replacement text.  Nargs = 0 means a functionlike macro with no args,
   e.g.,
       #define getchar() getc (stdin) .
   When there are args, the expansion is the replacement text with the
   args squashed out, and the reflist is a list describing how to
   build the output from the input: e.g., "3 chars, then the 1st arg,
   then 9 chars, then the 3rd arg, then 0 chars, then the 2nd arg".
   The chars here come from the expansion.  Whatever is left of the
   expansion after the last arg-occurrence is copied after that arg.
   Note that the reflist can be arbitrarily long---
   its length depends on the number of times the arguments appear in
   the replacement text, not how many args there are.  Example:
   #define f(x) x+x+x+x+x+x+x would have replacement text "++++++" and
   pattern list
     { (0, 1), (1, 1), (1, 1), ..., (1, 1), NULL }
   where (x, y) means (nchars, argno). */

typedef struct definition DEFINITION;
struct definition {
  int nargs;
  int length;			/* length of expansion string */
  U_CHAR *expansion;
  struct reflist {
    struct reflist *next;
    char stringify;		/* nonzero if this arg was preceded by a
				   # operator. */
    char raw_before;		/* Nonzero if a ## operator before arg. */
    char raw_after;		/* Nonzero if a ## operator after arg. */
    int nchars;			/* Number of literal chars to copy before
				   this arg occurrence.  */
    int argno;			/* Number of arg to substitute (origin-0) */
  } *pattern;
  /* Names of macro args, concatenated in reverse order
     with comma-space between them.
     The only use of this is that we warn on redefinition
     if this differs between the old and new definitions.  */
  const U_CHAR *argnames;
};

/* Chained list of answers to an assertion.  */
struct answer
{
  struct answer *next;
  const unsigned char *answer;
  size_t len;
};

/* different kinds of things that can appear in the value field
   of a hash node.  Actually, this may be useless now. */
union hashval {
  const char *cpval;
  DEFINITION *defn;
  struct answer *answers;
};

/* The structure of a node in the hash table.  The hash table
   has entries for all tokens defined by #define commands (type T_MACRO),
   plus some special tokens like __LINE__ (these each have their own
   type, and the appropriate code is run when that type of node is seen.
   It does not contain control words like "#define", which are recognized
   by a separate piece of code. */

/* different flavors of hash nodes --- also used in keyword table */
enum node_type {
 T_DEFINE = 1,	/* `#define' */
 T_INCLUDE,	/* `#include' */
 T_INCLUDE_NEXT,/* `#include_next' */
 T_IFDEF,	/* `#ifdef' */
 T_IFNDEF,	/* `#ifndef' */
 T_IF,		/* `#if' */
 T_ELSE,	/* `#else' */
 T_ELIF,	/* `#elif' */
 T_UNDEF,	/* `#undef' */
 T_LINE,	/* `#line' */
 T_ENDIF,	/* `#endif' */
 T_ERROR,	/* `#error' */
 T_WARNING,	/* `#warning' */
 T_ASSERT,	/* `#assert' */
 T_UNASSERT,	/* `#unassert' */
 T_SPECLINE,	/* special symbol `__LINE__' */
 T_DATE,	/* `__DATE__' */
 T_FILE,	/* `__FILE__' */
 T_BASE_FILE,	/* `__BASE_FILE__' */
 T_INCLUDE_LEVEL, /* `__INCLUDE_LEVEL__' */
 T_VERSION,	/* `__VERSION__' */
 T_TIME,	/* `__TIME__' */
 T_CONST,	/* Constant value, used by `__STDC__' */
 T_MACRO,	/* macro defined by `#define' */
 T_SPEC_DEFINED, /* special `defined' macro for use in #if statements */
 T_UNUSED	/* Used for something not defined.  */
};

struct hashnode {
  struct hashnode *next;	/* double links for easy deletion */
  struct hashnode *prev;
  struct hashnode **bucket_hdr;	/* also, a back pointer to this node's hash
				   chain is kept, in case the node is the head
				   of the chain and gets deleted. */
  enum node_type type;		/* type of special token */
  int length;			/* length of token, for quick comparison */
  U_CHAR *name;			/* the actual name */
  union hashval value;		/* pointer to expansion, or whatever */
};

typedef struct hashnode HASHNODE;

static HASHNODE *parse_assertion PARAMS ((const unsigned char *,
					  const unsigned char *,
					  struct answer **, int));
static struct answer **find_answer PARAMS ((HASHNODE *,
					    const struct answer *));
static int parse_answer PARAMS ((const unsigned char *, const unsigned char *,
				 struct answer **, int));
static unsigned char *canonicalize_text PARAMS ((const unsigned char *,
						 const unsigned char *,
						 const unsigned char **));

/* Some definitions for the hash table.  The hash function MUST be
   computed as shown in hashf () below.  That is because the rescan
   loop computes the hash value `on the fly' for most tokens,
   in order to avoid the overhead of a lot of procedure calls to
   the hashf () function.  Hashf () only exists for the sake of
   politeness, for use when speed isn't so important. */

#define HASHSIZE 1403
HASHNODE *hashtab[HASHSIZE];
#define HASHSTEP(old, c) ((old << 2) + c)
#define MAKE_POS(v) (v & 0x7fffffff) /* make number positive */

/* `struct directive' defines one #-directive, including how to handle it.  */

struct directive {
  const int length;		/* Length of name */
  void (*const func) PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
  				/* Function to handle directive */
  const char *const name;	/* Name of directive */
  const enum node_type type;	/* Code which describes which directive. */
};

/* Last arg to output_line_command.  */
enum file_change_code {same_file, enter_file, leave_file};

/* This structure represents one parsed argument in a macro call.
   `raw' points to the argument text as written (`raw_length' is its length).
   `expanded' points to the argument's macro-expansion
   (its length is `expand_length').
   `stringified_length' is the length the argument would have
   if stringified.
   `free1' and `free2', if nonzero, point to blocks to be freed
   when the macro argument data is no longer needed.  */

struct argdata {
  U_CHAR *raw, *expanded;
  int raw_length, expand_length;
  int stringified_length;
  U_CHAR *free1, *free2;
  char newlines;
  char comments;
};

/* The arglist structure is built by do_define to tell
   collect_definition where the argument names begin.  That
   is, for a define like "#define f(x,y,z) foo+x-bar*y", the arglist
   would contain pointers to the strings x, y, and z.
   Collect_definition would then build a DEFINITION node,
   with reflist nodes pointing to the places x, y, and z had
   appeared.  So the arglist is just convenience data passed
   between these two routines.  It is not kept around after
   the current #define has been processed and entered into the
   hash table. */

struct arglist {
  struct arglist *next;
  U_CHAR *name;
  int length;
  int argno;
};

/* Function prototypes.  */

static void do_define	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_error	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_warning	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_line	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_include	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_include_next	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_undef	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_if	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_ifdef	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_ifndef	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_else	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_elif	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_endif	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_assert	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_unassert	PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *));
static void do_xifdef	PARAMS ((U_CHAR *, U_CHAR *, enum node_type));

static struct hashnode *install PARAMS ((const U_CHAR *, int, enum node_type, int));
static int hashf		 PARAMS ((const U_CHAR *, int, int));
static int compare_defs	 PARAMS ((DEFINITION *, DEFINITION *));
static int comp_def_part	 PARAMS ((int, const U_CHAR *, int,
					  const U_CHAR *, int, int));
static void delete_macro	 PARAMS ((HASHNODE *));

/* First arg to v_message.  */
enum msgtype { MT_WARNING = 0, MT_ERROR, MT_FATAL };
static void v_message		 PARAMS ((enum msgtype mtype, int line,
					  const char *msgid, va_list ap))
     ATTRIBUTE_PRINTF (3, 0);

static int line_for_error	 PARAMS ((int));

/* We know perfectly well which file this is, so we don't need to
   use __FILE__.  */
#undef abort
#if (GCC_VERSION >= 2007)
#define abort()	fancy_abort(__LINE__, __FUNCTION__)
#else
#define abort() fancy_abort(__LINE__, 0);
#endif

static void macroexpand 	PARAMS ((HASHNODE *, FILE_BUF *));
static void special_symbol	PARAMS ((HASHNODE *, FILE_BUF *));
static void dump_all_macros 	PARAMS ((void));
static void dump_defn_1		PARAMS ((const U_CHAR *, int, int));
static void dump_arg_n		PARAMS ((DEFINITION *, int));
static void conditional_skip 	PARAMS ((FILE_BUF *, int, enum node_type));
static void skip_if_group 	PARAMS ((FILE_BUF *, int));
static void output_line_command PARAMS ((FILE_BUF *, FILE_BUF *,
					 int, enum file_change_code));

static int eval_if_expression	PARAMS ((const U_CHAR *, int));

static void output_deps		PARAMS ((void));
static void initialize_builtins	PARAMS ((void));
static void run_directive	PARAMS ((const char *, size_t,
					 enum node_type));
static void make_definition	PARAMS ((const char *));
static void make_undef		PARAMS ((const char *));
static void make_assertion	PARAMS ((const char *));

static void grow_outbuf 	PARAMS ((FILE_BUF *, int));
static int handle_directive 	PARAMS ((FILE_BUF *, FILE_BUF *));
static void process_include	PARAMS ((struct file_name_list *,
					 const U_CHAR *, int, int, FILE_BUF *));
static void fixup_newlines	PARAMS ((FILE_BUF *));
static void finclude		PARAMS ((int, const char *,
					 struct file_name_list *, FILE_BUF *));
static void init_dependency_output PARAMS ((void));
static void rescan		PARAMS ((FILE_BUF *, int));
static void newline_fix		PARAMS ((U_CHAR *));
static void name_newline_fix	PARAMS ((U_CHAR *));
static U_CHAR *macarg1		PARAMS ((U_CHAR *, const U_CHAR *, int *,
					 int *, int *));
static const char *macarg	PARAMS ((struct argdata *));
static int discard_comments	PARAMS ((U_CHAR *, int, int));
static int file_size_and_mode	PARAMS ((int, int *, long *));

static U_CHAR *skip_to_end_of_comment PARAMS ((FILE_BUF *, int *));
static U_CHAR *skip_quoted_string     PARAMS ((const U_CHAR *, const U_CHAR *,
					       int, int *, int *, int *));

int main		PARAMS ((int, char **));

/* Convenience.  Write U"string" to get an unsigned string constant.  */
#define U (const unsigned char *)

/* Here is the actual list of #-directives, most-often-used first.  */

static const struct directive directive_table[] = {
  {  6, do_define,  "define",  T_DEFINE  },
  {  7, do_include, "include", T_INCLUDE },
  {  5, do_endif,   "endif",   T_ENDIF   },
  {  5, do_ifdef,   "ifdef",   T_IFDEF   },
  {  2, do_if,      "if",      T_IF,     },
  {  4, do_else,    "else",    T_ELSE    },
  {  6, do_ifndef,  "ifndef",  T_IFNDEF  },
  {  5, do_undef,   "undef",   T_UNDEF   },
  {  4, do_line,    "line",    T_LINE    },
  {  4, do_elif,    "elif",    T_ELIF    },
  {  5, do_error,   "error",   T_ERROR   },
  {  7, do_warning, "warning", T_WARNING },
  { 12, do_include_next, "include_next", T_INCLUDE_NEXT },
  {  6, do_assert,  "assert",  T_ASSERT  },
  {  8, do_unassert,"unassert",T_UNASSERT},
  {  -1, 0, "", T_UNUSED},
};

#define SKIP_WHITE_SPACE(p) do { while (is_nvspace(*p)) p++; } while (0)
#define SKIP_ALL_WHITE_SPACE(p) do { while (is_space(*p)) p++; } while (0)
  
int errors = 0;			/* Error counter for exit code */

static FILE_BUF expand_to_temp_buffer PARAMS ((const U_CHAR *, const U_CHAR *, int));
static DEFINITION *collect_expansion  PARAMS ((U_CHAR *, U_CHAR *, int,
					       struct arglist *));

/* Stack of conditionals currently in progress
   (including both successful and failing conditionals).  */

struct if_stack {
  struct if_stack *next;	/* for chaining to the next stack frame */
  const char *fname;		/* copied from input when frame is made */
  int lineno;			/* similarly */
  int if_succeeded;		/* true if a leg of this if-group
				    has been passed through rescan */
  enum node_type type;		/* type of last directive seen in this group */
};
typedef struct if_stack IF_STACK_FRAME;
IF_STACK_FRAME *if_stack = NULL;

/* Nonzero means -I- has been seen,
   so don't look for #include "foo" the source-file directory.  */
int ignore_srcdir;

/* Pending directives.  */
enum pending_dir_t {PD_NONE = 0, PD_DEFINE, PD_UNDEF, PD_ASSERTION, PD_FILE};

typedef struct pending_dir pending_dir;
struct pending_dir
{
  const char *arg;
  enum pending_dir_t type;
};

int
main (argc, argv)
     int argc;
     char **argv;
{
  int st_mode;
  long st_size;
  const char *in_fname, *out_fname;
  int f, i;
  FILE_BUF *fp;
  pending_dir *pend = (pending_dir *) xcalloc (argc, sizeof (pending_dir));
  int no_standard_includes = 0;

  hex_init ();

#ifdef RLIMIT_STACK
  /* Get rid of any avoidable limit on stack size.  */
  {
    struct rlimit rlim;

    /* Set the stack limit huge so that alloca (particularly stringtab
     * in dbxread.c) does not fail. */
    getrlimit (RLIMIT_STACK, &rlim);
    rlim.rlim_cur = rlim.rlim_max;
    setrlimit (RLIMIT_STACK, &rlim);
  }
#endif /* RLIMIT_STACK defined */

  progname = argv[0];

  in_fname = NULL;
  out_fname = NULL;

  no_line_commands = 0;
  dump_macros = 0;
  no_output = 0;

  max_include_len = cpp_GCC_INCLUDE_DIR_len + 7;  /* ??? */

  gcc_init_libintl ();

  /* It's simplest to just create this struct whether or not it will
     be needed.  */
  deps = deps_init ();

  /* Process switches and find input file name.  */

  for (i = 1; i < argc; i++) {
    if (argv[i][0] != '-') {
      if (out_fname != NULL)
	fatal ("usage: %s [switches] input output", argv[0]);
      else if (in_fname != NULL)
	out_fname = argv[i];
      else
	in_fname = argv[i];
    } else {
      int c = argv[i][1];

      switch (c) {
      case 'E':
      case '$':
	break;  /* Ignore for compatibility with ISO/extended cpp.  */

      case 'l':
	if (!strcmp (argv[i], "-lang-c++")
	    || !strcmp (argv[i], "-lang-objc++"))
	  fatal ("-traditional is not supported in C++");
	else if (!strcmp (argv[i], "-lang-c89"))
	  fatal ("-traditional and -ansi are mutually exclusive");
	else if (!strcmp (argv[i], "-lang-objc"))
	  pend[i].type = PD_DEFINE, pend[i].arg = "__OBJC__";
	else if (!strcmp (argv[i], "-lang-asm"))
	  pend[i].type = PD_DEFINE, pend[i].arg = "__ASSEMBLER__";
	else if (!strcmp (argv[i], "-lang-fortran"))
	  pend[i].type = PD_DEFINE, pend[i].arg = "_LANGUAGE_FORTRAN";
	/* All other possibilities ignored.  */
	break;

      case 'i':
	if (!strcmp (argv[i], "-include"))
	  {
	    if (i + 1 == argc)
	      fatal ("filename missing after -i option");
	    else
	      pend[i].type = PD_FILE, pend[i].arg = argv[i + 1], i++;
	  }
	else if (!strcmp (argv[i], "-iprefix"))
	  i++; /* Ignore for compatibility */
	else if (!strcmp (argv[i], "-isystem")
		 || !strcmp (argv[i], "-iwithprefix")
		 || !strcmp (argv[i], "-iwithprefixbefore")
		 || !strcmp (argv[i], "-idirafter"))
	  goto add_include;  /* best we can do */
	  
	break;

      case 'o':
	if (out_fname != NULL)
	  fatal ("output filename specified twice");
	if (i + 1 == argc)
	  fatal ("filename missing after -o option");
	out_fname = argv[++i];
	if (!strcmp (out_fname, "-"))
	  out_fname = "";
	break;

      case 'w':
	inhibit_warnings = 1;
	break;

      case 'W':
	if (!strcmp (argv[i], "-Wcomments"))
	  warn_comments = 1;
	else if (!strcmp (argv[i], "-Wcomment"))
	  warn_comments = 1;
	else if (!strcmp (argv[i], "-Wall")) {
	  warn_comments = 1;
	}
	break;

      case 'f':
	if (!strcmp (argv[i], "-fleading-underscore"))
	  user_label_prefix = "_";
	else if (!strcmp (argv[i], "-fno-leading-underscore"))
	  user_label_prefix = "";
	else if (!strcmp (argv[i], "-fsigned-char"))
	  flag_signed_char = 1;
	else if (!strcmp (argv[i], "-funsigned-char"))
	  flag_signed_char = 0;
	break;

      case 'M':
	{
	  char *p = NULL;

	  /* -MD and -MMD for tradcpp are deprecated and undocumented
	     (use -M or -MM with -MF instead), and probably should be
	     removed with the next major GCC version.  For the moment
	     we allow these for the benefit of Automake 1.4, which
	     uses these when dependency tracking is enabled.  Automake
	     1.5 will fix this.  */
	  if (!strncmp (argv[i], "-MD", 3)) {
	    p = argv[i] + 3;
	    print_deps = 2;
	  } else if (!strncmp (argv[i], "-MMD", 4)) {
	    p = argv[i] + 4;
	    print_deps = 1;
	  } else if (!strcmp (argv[i], "-M")) {
	    print_deps = 2;
	  } else if (!strcmp (argv[i], "-MM")) {
	    print_deps = 1;
	  } else if (!strcmp (argv[i], "-MG")) {
	    deps_missing_files = 1;
	  } else if (!strcmp (argv[i], "-MF")) {
	    p = argv[i] + 3;
	  } else if (!strcmp (argv[i], "-MP")) {
	    print_deps_phony_targets = 1;
	  } else if (!strcmp (argv[i], "-MQ") || !strcmp (argv[i], "-MT")) {
	    /* Add a target.  -MQ quotes for Make.  */
	    const char *tgt = argv[i] + 3;
	    int quoted = argv[i][2] == 'Q';

	    if (*tgt == '\0' && i + 1 == argc)
	      fatal ("target missing after %s option", argv[i]);
	    else
	      {
		if (*tgt == '\0')
		  tgt = argv[++i];
	      
		deps_add_target (deps, tgt, quoted);
	      }
	  }

	  if (p) {
	    if (*p)
	      deps_file = p;
	    else if (i + 1 == argc)
	      fatal ("filename missing after %s option", argv[i]);
	    else
	      deps_file = argv[++i];
	  }
	}
	break;

      case 'd':
	dump_macros = 1;
	no_output = 1;
	break;

      case 'v':
	fprintf (stderr, "GNU traditional CPP version %s\n", version_string);
	break;

      case 'D':
      case 'U':
      case 'A':
	{
	  char *p;

	  if (argv[i][2] != 0)
	    p = argv[i] + 2;
	  else if (i + 1 == argc)
	    fatal ("macro name missing after -%c option", c);
	  else
	    p = argv[++i];

	  if (c == 'D')
	    pend[i].type = PD_DEFINE;
	  else if (c == 'U')
	    pend[i].type = PD_UNDEF;
	  else
	    pend[i].type = PD_ASSERTION;
	  pend[i].arg = p;
	}
	break;

      case 'C':
	put_out_comments = 1;
	break;

      case 'p':
	if (!strcmp (argv[i], "-pedantic"))
	  fatal ("-pedantic and -traditional are mutually exclusive");
	break;

      case 't':
	if (!strcmp (argv[i], "-trigraphs"))
	  fatal ("-trigraphs and -traditional are mutually exclusive");
	break;

      case 'P':
	no_line_commands = 1;
	break;

      case 'I':			/* Add directory to path for includes.  */
      add_include:
	{
	  struct file_name_list *dirtmp;

	  if (! ignore_srcdir && !strcmp (argv[i] + 2, "-"))
	    ignore_srcdir = 1;
	  else {
	    dirtmp = (struct file_name_list *)
	      xmalloc (sizeof (struct file_name_list));
	    dirtmp->next = 0;		/* New one goes on the end */
	    if (include == 0)
	      include = dirtmp;
	    else
	      last_include->next = dirtmp;
	    last_include = dirtmp;	/* Tail follows the last one */
	    if (argv[i][1] == 'I' && argv[i][2] != 0)
	      dirtmp->fname = argv[i] + 2;
	    else if (i + 1 == argc)
	      fatal ("directory name missing after -I option");
	    else
	      dirtmp->fname = argv[++i];
	    if (strlen (dirtmp->fname) > max_include_len)
	      max_include_len = strlen (dirtmp->fname);
	    if (ignore_srcdir && first_bracket_include == 0)
	      first_bracket_include = dirtmp;
	    }
	}
	break;

      case 'n':
	/* -nostdinc causes no default include directories.
	   You must specify all include-file directories with -I.  */
	no_standard_includes = 1;
	break;

      case '\0': /* JF handle '-' as file name meaning stdin or stdout */
	if (in_fname == NULL) {
	  in_fname = "";
	  break;
	} else if (out_fname == NULL) {
	  out_fname = "";
	  break;
	}	/* else fall through into error */

      default:
	fatal ("invalid option `%s'", argv[i]);
      }
    }
  }

  init_dependency_output ();

  /* After checking the environment variables, check if -M or -MM has
     not been specified, but other -M options have.  */
  if (print_deps == 0
      && (deps_missing_files || deps_file || print_deps_phony_targets))
    fatal ("you must additionally specify either -M or -MM");

  if (user_label_prefix == 0)
    user_label_prefix = USER_LABEL_PREFIX;

  if (print_deps)
    {
      /* Set the default target (if there is none already), and
	 the dependency on the main file.  */
      deps_add_default_target (deps, in_fname);

      deps_add_dep (deps, in_fname);
    }

  /* Install __LINE__, etc.  Must follow option processing.  */
  initialize_builtins ();

  /* Do defines specified with -D and undefines specified with -U.  */
  for (i = 1; i < argc; i++)
    if (pend[i].type == PD_DEFINE)
      make_definition (pend[i].arg);
    else if (pend[i].type == PD_UNDEF)
      make_undef (pend[i].arg);
    else if (pend[i].type == PD_ASSERTION)
      make_assertion (pend[i].arg);

  /* Unless -fnostdinc,
     tack on the standard include file dirs to the specified list */
  if (!no_standard_includes) {
    const struct default_include *di;
    struct file_name_list *old_last_include = last_include;
    struct file_name_list *dirtmp;
    for (di = cpp_include_defaults; di->fname; di++) {
      if (di->cplusplus)
	continue;
      dirtmp = (struct file_name_list *)
	xmalloc (sizeof (struct file_name_list));
      dirtmp->next = 0;		/* New one goes on the end */
      if (include == 0)
	include = dirtmp;
      else
	last_include->next = dirtmp;
      last_include = dirtmp;	/* Tail follows the last one */
      dirtmp->fname = di->fname;
      if (strlen (dirtmp->fname) > max_include_len)
	max_include_len = strlen (dirtmp->fname);
    }

    if (ignore_srcdir && first_bracket_include == 0)
      first_bracket_include = old_last_include->next;
  }

  /* Initialize output buffer */

  outbuf.buf = (U_CHAR *) xmalloc (OUTBUF_SIZE);
  outbuf.bufp = outbuf.buf;
  outbuf.length = OUTBUF_SIZE;

  /* Scan the -i files before the main input.
     Much like #including them, but with no_output set
     so that only their macro definitions matter.  */

  no_output++;
  indepth++;
  for (i = 1; i < argc; i++)
    if (pend[i].type == PD_FILE)
      {
	int fd = open (pend[i].arg, O_RDONLY, 0666);
	if (fd < 0)
	  {
	    perror_with_name (pend[i].arg);
	    return FATAL_EXIT_CODE;
	  }

	/* For -M, add this file to the dependencies.  */
	if (print_deps)
	  deps_add_dep (deps, pend[i].arg);

	finclude (fd, pend[i].arg, 0, &outbuf);
      }
  indepth--;
  no_output--;

  /* Pending directives no longer needed.  */
  free ((PTR) pend);

  /* Create an input stack level for the main input file
     and copy the entire contents of the file into it.  */

  fp = &instack[++indepth];

  /* JF check for stdin */
  if (in_fname == NULL || *in_fname == 0) {
    in_fname = "";
    f = 0;
  } else if ((f = open (in_fname, O_RDONLY, 0666)) < 0)
    goto sys_error;

  if (file_size_and_mode (f, &st_mode, &st_size))
    goto sys_error;
  fp->fname = in_fname;
  fp->lineno = 1;
  /* JF all this is mine about reading pipes and ttys */
  if (!S_ISREG (st_mode)) {
    /* Read input from a file that is not a normal disk file.
       We cannot preallocate a buffer with the correct size,
       so we must read in the file a piece at the time and make it bigger.  */
    int size;
    int bsize;
    int cnt;
    U_CHAR *bufp;

    bsize = 2000;
    size = 0;
    fp->buf = (U_CHAR *) xmalloc (bsize + 2);
    bufp = fp->buf;
    for (;;) {
      cnt = read (f, bufp, bsize - size);
      if (cnt < 0) goto sys_error;	/* error! */
      if (cnt == 0) break;		/* End of file */
      size += cnt;
      bufp += cnt;
      if (bsize == size) {		/* Buffer is full! */
        bsize *= 2;
        fp->buf = (U_CHAR *) xrealloc (fp->buf, bsize + 2);
	bufp = fp->buf + size;	/* May have moved */
      }
    }
    fp->length = size;
  } else {
    /* Read a file whose size we can determine in advance.
       For the sake of VMS, st_size is just an upper bound.  */
    long i;
    fp->length = 0;
    fp->buf = (U_CHAR *) xmalloc (st_size + 2);

    while (st_size > 0) {
      i = read (f, fp->buf + fp->length, st_size);
      if (i <= 0) {
        if (i == 0) break;
	goto sys_error;
      }
      fp->length += i;
      st_size -= i;
    }
  }
  fp->bufp = fp->buf;
  fp->if_stack = if_stack;
  fixup_newlines (fp);

  /* Make sure data ends with a newline.  And put a null after it.  */

  if (fp->length > 0 && fp->buf[fp->length-1] != '\n')
    fp->buf[fp->length++] = '\n';
  fp->buf[fp->length] = '\0';
  
  /* Now that we know the input file is valid, open the output.  */

  if (!out_fname || !strcmp (out_fname, ""))
    out_fname = "stdout";
  else if (! freopen (out_fname, "w", stdout))
    pfatal_with_name (out_fname);

  output_line_command (fp, &outbuf, 0, same_file);

  /* Scan the input, processing macros and directives.  */

  rescan (&outbuf, 0);

  /* Now we have processed the entire input
     Write whichever kind of output has been requested.  */


  if (dump_macros)
    dump_all_macros ();
  else if (! inhibit_output)
    if (write (fileno (stdout), outbuf.buf, outbuf.bufp - outbuf.buf) < 0)
      fatal ("I/O error on output");

  /* Don't write the deps file if preprocessing has failed.  */
  if (print_deps && errors == 0)
    output_deps ();

  /* Destruct the deps object.  */
  deps_free (deps);

  if (ferror (stdout))
    fatal ("I/O error on output");

  if (errors)
    exit (FATAL_EXIT_CODE);
  exit (SUCCESS_EXIT_CODE);

 sys_error:
  pfatal_with_name (in_fname);
}

/* Set up dependency-file output.  */
static void
init_dependency_output ()
{
  char *spec, *s, *output_file;

  /* Either of two environment variables can specify output of deps.
     Its value is either "OUTPUT_FILE" or "OUTPUT_FILE DEPS_TARGET",
     where OUTPUT_FILE is the file to write deps info to
     and DEPS_TARGET is the target to mention in the deps.  */

  if (print_deps == 0)
    {
      spec = getenv ("DEPENDENCIES_OUTPUT");
      if (spec)
	print_deps = 1;
      else
	{
	  spec = getenv ("SUNPRO_DEPENDENCIES");
	  if (spec)
	    print_deps = 2;
	  else
	    return;
	}

      /* Find the space before the DEPS_TARGET, if there is one.  */
      s = strchr (spec, ' ');
      if (s)
	{
	  /* Let the caller perform MAKE quoting.  */
	  deps_add_target (deps, s + 1, 0);
	  output_file = (char *) xmalloc (s - spec + 1);
	  memcpy (output_file, spec, s - spec);
	  output_file[s - spec] = 0;
	}
      else
	output_file = spec;

      /* Command line overrides environment variables.  */
      if (deps_file == 0)
	deps_file = output_file;
      deps_append = 1;
    }

  /* If dependencies go to standard output, or -MG is used, we should
     suppress output.  The user may be requesting other stuff to
     stdout, with -dM, -v etc.  We let them shoot themselves in the
     foot.  */
  if (deps_file == 0 || deps_missing_files)
    inhibit_output = 1;
}

/* Use mkdeps.c to output dependency information.  */
static void
output_deps ()
{
  /* Stream on which to print the dependency information.  */
  FILE *deps_stream = 0;
  const char *const deps_mode = deps_append ? "a" : "w";

  if (deps_file == 0)
    deps_stream = stdout;
  else
    {
      deps_stream = fopen (deps_file, deps_mode);
      if (deps_stream == 0)
	{
	  error_from_errno (deps_file);
	  return;
	}
    }

  deps_write (deps, deps_stream, 72);

  if (print_deps_phony_targets)
    deps_phony_targets (deps, deps_stream);

  /* Don't close stdout.  */
  if (deps_file)
    {
      if (ferror (deps_stream) || fclose (deps_stream) != 0)
	fatal ("I/O error on output");
    }
}

/* Move all backslash-newline pairs out of embarrassing places.
   Exchange all such pairs following BP
   with any potentially-embarrasing characters that follow them.
   Potentially-embarrassing characters are / and *
   (because a backslash-newline inside a comment delimiter
   would cause it not to be recognized).  */
static void
newline_fix (bp)
     U_CHAR *bp;
{
  U_CHAR *p = bp;
  int count = 0;

  /* First count the backslash-newline pairs here.  */

  while (*p++ == '\\' && *p++ == '\n')
    count++;

  p = bp + count * 2;

  /* Exit if what follows the backslash-newlines is not embarrassing.  */

  if (count == 0 || (*p != '/' && *p != '*'))
    return;

  /* Copy all potentially embarrassing characters
     that follow the backslash-newline pairs
     down to where the pairs originally started.  */

  while (*p == '*' || *p == '/')
    *bp++ = *p++;

  /* Now write the same number of pairs after the embarrassing chars.  */
  while (count-- > 0) {
    *bp++ = '\\';
    *bp++ = '\n';
  }
}

/* Like newline_fix but for use within a directive-name.
   Move any backslash-newlines up past any following symbol constituents.  */
static void
name_newline_fix (bp)
     U_CHAR *bp;
{
  U_CHAR *p = bp;
  int count = 0;

  /* First count the backslash-newline pairs here.  */

  while (*p++ == '\\' && *p++ == '\n')
    count++;

  p = bp + count * 2;

  /* What follows the backslash-newlines is not embarrassing.  */

  if (count == 0 || !is_idchar (*p))
    return;

  /* Copy all potentially embarrassing characters
     that follow the backslash-newline pairs
     down to where the pairs originally started.  */

  while (is_idchar (*p))
    *bp++ = *p++;

  /* Now write the same number of pairs after the embarrassing chars.  */
  while (count-- > 0) {
    *bp++ = '\\';
    *bp++ = '\n';
  }
}

/*
 * The main loop of the program.
 *
 * Read characters from the input stack, transferring them to the
 * output buffer OP.
 *
 * Macros are expanded and push levels on the input stack.
 * At the end of such a level it is popped off and we keep reading.
 * At the end of any other kind of level, we return.
 * #-directives are handled, except within macros.
 *
 * If OUTPUT_MARKS is nonzero, keep Newline markers found in the input
 * and insert them when appropriate.  This is set while scanning macro
 * arguments before substitution.  It is zero when scanning for final output.
 *   There are three types of Newline markers:
 *   * Newline -  follows a macro name that was not expanded
 *     because it appeared inside an expansion of the same macro.
 *     This marker prevents future expansion of that identifier.
 *     When the input is rescanned into the final output, these are deleted.
 *     These are also deleted by ## concatenation.
 *   * Newline Space (or Newline and any other whitespace character)
 *     stands for a place that tokens must be separated or whitespace
 *     is otherwise desirable, but where the ANSI standard specifies there
 *     is no whitespace.  This marker turns into a Space (or whichever other
 *     whitespace char appears in the marker) in the final output,
 *     but it turns into nothing in an argument that is stringified with #.
 *     Such stringified arguments are the only place where the ANSI standard
 *     specifies with precision that whitespace may not appear.
 *
 * During this function, IP->bufp is kept cached in IBP for speed of access.
 * Likewise, OP->bufp is kept in OBP.  Before calling a subroutine
 * IBP, IP and OBP must be copied back to memory.  IP and IBP are
 * copied back with the RECACHE macro.  OBP must be copied back from OP->bufp
 * explicitly, and before RECACHE, since RECACHE uses OBP.
 */

static void
rescan (op, output_marks)
     FILE_BUF *op;
     int output_marks;
{
  /* Character being scanned in main loop.  */
  U_CHAR c;

  /* Length of pending accumulated identifier.  */
  int ident_length = 0;

  /* Hash code of pending accumulated identifier.  */
  int hash = 0;

  /* Current input level (&instack[indepth]).  */
  FILE_BUF *ip;

  /* Pointer for scanning input.  */
  U_CHAR *ibp;

  /* Pointer to end of input.  End of scan is controlled by LIMIT.  */
  U_CHAR *limit;

  /* Pointer for storing output.  */
  U_CHAR *obp;

  /* REDO_CHAR is nonzero if we are processing an identifier
     after backing up over the terminating character.
     Sometimes we process an identifier without backing up over
     the terminating character, if the terminating character
     is not special.  Backing up is done so that the terminating character
     will be dispatched on again once the identifier is dealt with.  */
  int redo_char = 0;

  /* 1 if within an identifier inside of which a concatenation
     marker (Newline -) has been seen.  */
  int concatenated = 0;

  /* While scanning a comment or a string constant,
     this records the line it started on, for error messages.  */
  int start_line;

  /* Record position of last `real' newline.  */
  U_CHAR *beg_of_line;

  /* This has to be a global bacause of RECACHE.  */
  U_CHAR *obufp_before_macroname = NULL;

/* Pop the innermost input stack level, assuming it is a macro expansion.  */

#define POPMACRO \
do { ip->macro->type = T_MACRO;		\
     if (ip->free_ptr) free (ip->free_ptr);	\
     --indepth; } while (0)

/* Reload `rescan's local variables that describe the current
   level of the input stack.  */

#define RECACHE  \
do { ip = &instack[indepth];		\
     ibp = ip->bufp;			\
     limit = ip->buf + ip->length;	\
     op->bufp = obp;			\
     check_expand (op, limit - ibp);	\
     beg_of_line = 0;			\
     obufp_before_macroname += op->bufp - obp;  \
     obp = op->bufp; } while (0)

  if (no_output && instack[indepth].fname != 0)
    skip_if_group (&instack[indepth], 1);

  obp = op->bufp;
  RECACHE;
  beg_of_line = ibp;

  /* Our caller must always put a null after the end of
     the input at each input stack level.  */
  if (*limit != 0)
    abort ();

  while (1) {
    c = *ibp++;
    *obp++ = c;

    switch (c) {
    case '\\':
      if (ibp >= limit)
	break;
      if (*ibp == '\n') {
	/* Always merge lines ending with backslash-newline,
	   even in middle of identifier.  */
	++ibp;
	++ip->lineno;
	--obp;		/* remove backslash from obuf */
	break;
      }
      /* Otherwise, backslash suppresses specialness of following char,
	 so copy it here to prevent the switch from seeing it.
	 But first get any pending identifier processed.  */
      if (ident_length > 0)
	goto specialchar;
      *obp++ = *ibp++;
      break;

    case '#':
      /* If this is expanding a macro definition, don't recognize
	 preprocessor directives.  */
      if (ip->macro != 0)
	goto randomchar;
      if (ident_length)
	goto specialchar;

      /* # keyword: a # must be the first char on the line */
      if (beg_of_line == 0)
	goto randomchar;
      if (beg_of_line + 1 != ibp)
	goto randomchar;

      /* This # can start a directive.  */

      --obp;		/* Don't copy the '#' */

      ip->bufp = ibp;
      op->bufp = obp;
      if (! handle_directive (ip, op)) {
#ifdef USE_C_ALLOCA
	alloca (0);
#endif
	/* Not a known directive: treat it as ordinary text.
	   IP, OP, IBP, etc. have not been changed.  */
	if (no_output && instack[indepth].fname) {
	  /* If not generating expanded output,
	     what we do with ordinary text is skip it.
	     Discard everything until next # directive.  */
	  skip_if_group (&instack[indepth], 1);
	  RECACHE;
	  beg_of_line = ibp;
	  break;
	}
	++obp;		/* Copy the '#' after all */
	goto randomchar;
      }
#ifdef USE_C_ALLOCA
      alloca (0);
#endif
      /* A # directive has been successfully processed.  */
      /* If not generating expanded output, ignore everything until
	 next # directive.  */
      if (no_output && instack[indepth].fname)
	skip_if_group (&instack[indepth], 1);
      obp = op->bufp;
      RECACHE;
      beg_of_line = ibp;
      break;

    case '\"':			/* skip quoted string */
    case '\'':
      /* A single quoted string is treated like a double -- some
	 programs (e.g., troff) are perverse this way */

      if (ident_length)
	goto specialchar;

      start_line = ip->lineno;

      /* Skip ahead to a matching quote.  */

      while (1) {
	if (ibp >= limit) {
	  if (ip->macro != 0) {
	    /* try harder: this string crosses a macro expansion boundary */
	    POPMACRO;
	    RECACHE;
	    continue;
	  }
	  break;
	}
	*obp++ = *ibp;
	switch (*ibp++) {
	case '\n':
	  ++ip->lineno;
	  ++op->lineno;
	  /* Traditionally, end of line ends a string constant with no error.
	     So exit the loop and record the new line.  */
	  beg_of_line = ibp;
	  goto while2end;

	case '\\':
	  if (ibp >= limit)
	    break;
	  if (*ibp == '\n') {
	    /* Backslash newline is replaced by nothing at all,
	       but keep the line counts correct.  */
	    --obp;
	    ++ibp;
	    ++ip->lineno;
	  } else {
	    /* ANSI stupidly requires that in \\ the second \
	       is *not* prevented from combining with a newline.  */
	    while (*ibp == '\\' && ibp[1] == '\n') {
	      ibp += 2;
	      ++ip->lineno;
	    }
	    *obp++ = *ibp++;
	  }
	  break;

	case '\"':
	case '\'':
	  if (ibp[-1] == c)
	    goto while2end;
	  break;
	}
      }
    while2end:
      break;

    case '/':
      if (*ibp == '\\' && ibp[1] == '\n')
	newline_fix (ibp);
      /* Don't look for comments inside a macro definition.  */
      if (ip->macro != 0)
	goto randomchar;
      /* A comment constitutes white space, so it can terminate an identifier.
	 Process the identifier, if any.  */
      if (ident_length)
	goto specialchar;

      if (*ibp != '*')
	goto randomchar;

      /* We have a comment.  Skip it, optionally copying it to output.  */

      start_line = ip->lineno;

      ++ibp;			/* Skip the star. */

      /* In K+R C, a comment is equivalent to nothing.  Note that we
	  already output the slash; we might not want it.  */
      if (! put_out_comments)
	obp--;
      else
	*obp++ = '*';

      {
	U_CHAR *before_bp = ibp;

	while (ibp < limit) {
	  switch (*ibp++) {
	  case '/':
	    if (warn_comments && ibp < limit && *ibp == '*')
	      warning("`/*' within comment");
	    break;
	  case '*':
	    if (*ibp == '\\' && ibp[1] == '\n')
	      newline_fix (ibp);
	    if (ibp >= limit || *ibp == '/')
	      goto comment_end;
	    break;
	  case '\n':
	    ++ip->lineno;
	    /* Copy the newline into the output buffer, in order to
	       avoid the pain of a #line every time a multiline comment
	       is seen.  */
	    if (!put_out_comments)
	      *obp++ = '\n';
	    ++op->lineno;
	  }
	}
      comment_end:

	if (ibp >= limit)
	  error_with_line (line_for_error (start_line),
			   "unterminated comment");
	else {
	  ibp++;
	  if (put_out_comments) {
	    memcpy (obp, before_bp, ibp - before_bp);
	    obp += ibp - before_bp;
	  }
	}
      }
      break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      /* If digit is not part of identifier, it starts a number,
	 which means that following letters are not an identifier.
	 "0x5" does not refer to an identifier "x5".
	 So copy all alphanumerics that follow without accumulating
	 as an identifier.  Periods also, for sake of "3.e7".  */

      if (ident_length == 0) {
	while (ibp < limit) {
	  while (ibp < limit && ibp[0] == '\\' && ibp[1] == '\n') {
	    ++ip->lineno;
	    ibp += 2;
	  }
	  c = *ibp++;
	  if (! ISIDNUM (c) && c != '.') {
	    --ibp;
	    break;
	  }
	  *obp++ = c;
	  /* A sign can be part of a preprocessing number
	     if it follows an e.  */
	  if (c == 'e' || c == 'E') {
	    while (ibp < limit && ibp[0] == '\\' && ibp[1] == '\n') {
	      ++ip->lineno;
	      ibp += 2;
	    }
	    if (ibp < limit && (*ibp == '+' || *ibp == '-')) {
	      *obp++ = *ibp++;
	      /* Traditional C does not let the token go past the sign.  */
	      break;
	    }
	  }
	}
	break;
      }
      /* fall through */

    case '_':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
    case 's': case 't': case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z':
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
    case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z':
      ident_length++;
      /* Compute step of hash function, to avoid a proc call on every token */
      hash = HASHSTEP (hash, c);
      break;

    case '\n':
      /* If reprocessing a macro expansion, newline is a special marker.  */
      if (ip->macro != 0) {
	/* Newline White is a "funny space" to separate tokens that are
	   supposed to be separate but without space between.
	   Here White means any horizontal whitespace character.
	   Newline - marks a recursive macro use that is not
	   supposed to be expandable.  */

	if (*ibp == '-') {
	  /* Newline - inhibits expansion of preceding token.
	     If expanding a macro arg, we keep the newline -.
	     In final output, it is deleted.  */
	  if (! concatenated) {
	    ident_length = 0;
	    hash = 0;
	  }
	  ibp++;
	  if (!output_marks) {
	    obp--;
	  } else {
	    /* If expanding a macro arg, keep the newline -.  */
	    *obp++ = '-';
	  }
	} else if (is_space (*ibp)) {
	  /* Newline Space does not prevent expansion of preceding token
	     so expand the preceding token and then come back.  */
	  if (ident_length > 0)
	    goto specialchar;

	  /* If generating final output, newline space makes a space.  */
	  if (!output_marks) {
	    obp[-1] = *ibp++;
	    /* And Newline Newline makes a newline, so count it.  */
	    if (obp[-1] == '\n')
	      op->lineno++;
	  } else {
	    /* If expanding a macro arg, keep the newline space.
	       If the arg gets stringified, newline space makes nothing.  */
	    *obp++ = *ibp++;
	  }
	} else abort ();	/* Newline followed by something random?  */
	break;
      }

      /* If there is a pending identifier, handle it and come back here.  */
      if (ident_length > 0)
	goto specialchar;

      beg_of_line = ibp;

      /* Update the line counts and output a #line if necessary.  */
      ++ip->lineno;
      ++op->lineno;
      if (ip->lineno != op->lineno) {
	op->bufp = obp;
	output_line_command (ip, op, 1, same_file);
	check_expand (op, ip->length - (ip->bufp - ip->buf));
	obp = op->bufp;
      }
      break;

      /* Come here either after (1) a null character that is part of the input
	 or (2) at the end of the input, because there is a null there.  */
    case 0:
      if (ibp <= limit)
	/* Our input really contains a null character.  */
	goto randomchar;

      /* At end of a macro-expansion level, pop it and read next level.  */
      if (ip->macro != 0) {
	obp--;
	ibp--;
	/* If we have an identifier that ends here, process it now, so
	   we get the right error for recursion.  */
	if (ident_length && ! is_idchar (*instack[indepth - 1].bufp)) {
	  redo_char = 1;
	  goto randomchar;
	}
	POPMACRO;
	RECACHE;
	break;
      }

      /* If we don't have a pending identifier,
	 return at end of input.  */
      if (ident_length == 0) {
	obp--;
	ibp--;
	op->bufp = obp;
	ip->bufp = ibp;
	goto ending;
      }

      /* If we do have a pending identifier, just consider this null
	 a special character and arrange to dispatch on it again.
	 The second time, IDENT_LENGTH will be zero so we will return.  */

      /* Fall through */

specialchar:

      /* Handle the case of a character such as /, ', " or null
	 seen following an identifier.  Back over it so that
	 after the identifier is processed the special char
	 will be dispatched on again.  */

      ibp--;
      obp--;
      redo_char = 1;

    default:

randomchar:

      if (ident_length > 0) {
	HASHNODE *hp;

	/* We have just seen an identifier end.  If it's a macro, expand it.

	   IDENT_LENGTH is the length of the identifier
	   and HASH is its hash code.

	   The identifier has already been copied to the output,
	   so if it is a macro we must remove it.

	   If REDO_CHAR is 0, the char that terminated the identifier
	   has been skipped in the output and the input.
	   OBP-IDENT_LENGTH-1 points to the identifier.
	   If the identifier is a macro, we must back over the terminator.

	   If REDO_CHAR is 1, the terminating char has already been
	   backed over.  OBP-IDENT_LENGTH points to the identifier.  */

	for (hp = hashtab[MAKE_POS (hash) % HASHSIZE]; hp != NULL;
	     hp = hp->next) {

	  if (hp->length == ident_length) {
	    /* obufp_before_macroname is used only in this block,
               but it has to be global because of RECACHE.  */
	    int op_lineno_before_macroname;
	    int i = ident_length;
	    U_CHAR *p = hp->name;
	    U_CHAR *q = obp - i;

	    if (! redo_char)
	      q--;

	    do {		/* All this to avoid a strncmp () */
	      if (*p++ != *q++)
		goto hashcollision;
	    } while (--i);

	    /* We found a use of a macro name.
	       see if the context shows it is a macro call.  */

	    /* Back up over terminating character if not already done.  */
	    if (! redo_char) {
	      ibp--;
	      obp--;
	    }

	    obufp_before_macroname = obp - ident_length;
	    op_lineno_before_macroname = op->lineno;

	    /* If macro wants an arglist, verify that a '(' follows.
	       first skip all whitespace, copying it to the output
	       after the macro name.  Then, if there is no '(',
	       decide this is not a macro call and leave things that way.  */
	    if (hp->type == T_MACRO && hp->value.defn->nargs >= 0)
	      {
		while (1) {
		  /* Scan forward over whitespace, copying it to the output.  */
		  if (ibp == limit && ip->macro != 0) {
		    POPMACRO;
		    RECACHE;
		  }
		  /* A comment: copy it unchanged or discard it.  */
		  else if (*ibp == '/' && ibp+1 != limit && ibp[1] == '*') {
		    if (put_out_comments) {
		      *obp++ = '/';
		      *obp++ = '*';
		    }
		    ibp += 2;
		    while (ibp + 1 != limit
			   && !(ibp[0] == '*' && ibp[1] == '/')) {
		      /* We need not worry about newline-marks,
			 since they are never found in comments.  */
		      if (*ibp == '\n') {
			/* Newline in a file.  Count it.  */
			++ip->lineno;
			++op->lineno;
		      }
		      if (put_out_comments)
			*obp++ = *ibp++;
		      else
			ibp++;
		    }
		    ibp += 2;
		    if (put_out_comments) {
		      *obp++ = '*';
		      *obp++ = '/';
		    }
		  }
		  else if (is_space (*ibp)) {
		    *obp++ = *ibp++;
		    if (ibp[-1] == '\n') {
		      if (ip->macro == 0) {
			/* Newline in a file.  Count it.  */
			++ip->lineno;
			++op->lineno;
		      } else if (!output_marks) {
			/* A newline mark, and we don't want marks
			   in the output.  If it is newline-hyphen,
			   discard it entirely.  Otherwise, it is
			   newline-whitechar, so keep the whitechar.  */
			obp--;
			if (*ibp == '-')
			  ibp++;
			else {
			  if (*ibp == '\n')
			    ++op->lineno;
			  *obp++ = *ibp++;
			}
		      } else {
			/* A newline mark; copy both chars to the output.  */
			*obp++ = *ibp++;
		      }
		    }
		  }
		  else break;
		}
		if (*ibp != '(')
		  break;
	      }

	    /* This is now known to be a macro call.
	       Discard the macro name from the output,
	       along with any following whitespace just copied.  */
	    obp = obufp_before_macroname;
	    op->lineno = op_lineno_before_macroname;

	    /* Expand the macro, reading arguments as needed,
	       and push the expansion on the input stack.  */
	    ip->bufp = ibp;
	    op->bufp = obp;
	    macroexpand (hp, op);

	    /* Reexamine input stack, since macroexpand has pushed
	       a new level on it.  */
	    obp = op->bufp;
	    RECACHE;
	    break;
	  }
hashcollision:
	       ;
	}			/* End hash-table-search loop */
	ident_length = hash = 0; /* Stop collecting identifier */
	redo_char = 0;
	concatenated = 0;
      }				/* End if (ident_length > 0) */
    }				/* End switch */
  }				/* End per-char loop */

  /* Come here to return -- but first give an error message
     if there was an unterminated successful conditional.  */
 ending:
  if (if_stack != ip->if_stack) {
    const char *str;
    switch (if_stack->type) {
    case T_IF:
      str = "if";
      break;
    case T_IFDEF:
      str = "ifdef";
      break;
    case T_IFNDEF:
      str = "ifndef";
      break;
    case T_ELSE:
      str = "else";
      break;
    case T_ELIF:
      str = "elif";
      break;
    default:
      abort ();
    }
    error_with_line (line_for_error (if_stack->lineno),
		     "unterminated #%s conditional", str);
  }
  if_stack = ip->if_stack;
}

/*
 * Rescan a string into a temporary buffer and return the result
 * as a FILE_BUF.  Note this function returns a struct, not a pointer.
 *
 * OUTPUT_MARKS nonzero means keep Newline markers found in the input
 * and insert such markers when appropriate.  See `rescan' for details.
 * OUTPUT_MARKS is 1 for macroexpanding a macro argument separately
 * before substitution; it is 0 for other uses.
 */
static FILE_BUF
expand_to_temp_buffer (buf, limit, output_marks)
     const U_CHAR *buf, *limit;
     int output_marks;
{
  FILE_BUF *ip;
  FILE_BUF obuf;
  int length = limit - buf;
  U_CHAR *buf1;
  int odepth = indepth;

  if (length < 0)
    abort ();

  /* Set up the input on the input stack.  */

  buf1 = (U_CHAR *) alloca (length + 1);
  {
    const U_CHAR *p1 = buf;
    U_CHAR *p2 = buf1;

    while (p1 != limit)
      *p2++ = *p1++;
  }
  buf1[length] = 0;

  /* Set up to receive the output.  */

  obuf.length = length * 2 + 100; /* Usually enough.  Why be stingy?  */
  obuf.bufp = obuf.buf = (U_CHAR *) xmalloc (obuf.length);
  obuf.fname = 0;
  obuf.macro = 0;
  obuf.free_ptr = 0;

  CHECK_DEPTH ({return obuf;});

  ++indepth;

  ip = &instack[indepth];
  ip->fname = 0;
  ip->macro = 0;
  ip->free_ptr = 0;
  ip->length = length;
  ip->buf = ip->bufp = buf1;
  ip->if_stack = if_stack;

  ip->lineno = obuf.lineno = 1;

  /* Scan the input, create the output.  */

  rescan (&obuf, output_marks);

  /* Pop input stack to original state.  */
  --indepth;

  if (indepth != odepth)
    abort ();

  /* Record the output.  */
  obuf.length = obuf.bufp - obuf.buf;

  return obuf;
}

/*
 * Process a # directive.  Expects IP->bufp to point to the '#', as in
 * `#define foo bar'.  Passes to the command handler
 * (do_define, do_include, etc.): the addresses of the 1st and
 * last chars of the command (starting immediately after the #
 * keyword), plus op and the keyword table pointer.  If the command
 * contains comments it is copied into a temporary buffer sans comments
 * and the temporary buffer is passed to the command handler instead.
 * Likewise for backslash-newlines.
 *
 * Returns nonzero if this was a known # directive.
 * Otherwise, returns zero, without advancing the input pointer.
 */

static int
handle_directive (ip, op)
     FILE_BUF *ip, *op;
{
  U_CHAR *bp, *cp;
  const struct directive *kt;
  int ident_length;
  U_CHAR *resume_p;

  /* Nonzero means we must copy the entire command
     to get rid of comments or backslash-newlines.  */
  int copy_command = 0;

  U_CHAR *ident, *after_ident;

  bp = ip->bufp;
  /* Skip whitespace and \-newline.  */
  while (1) {
    if (is_nvspace (*bp))
      bp++;
    else if (*bp == '/' && (newline_fix (bp + 1), bp[1]) == '*') {
      ip->bufp = bp;
      skip_to_end_of_comment (ip, &ip->lineno);
      bp = ip->bufp;
    } else if (*bp == '\\' && bp[1] == '\n') {
      bp += 2; ip->lineno++;
    } else break;
  }

  /* Now find end of directive name.
     If we encounter a backslash-newline, exchange it with any following
     symbol-constituents so that we end up with a contiguous name.  */

  cp = bp;
  while (1) {
    if (is_idchar (*cp))
      cp++;
    else {
      if (*cp == '\\' && cp[1] == '\n')
	name_newline_fix (cp);
      if (is_idchar (*cp))
	cp++;
      else break;
    }
  }
  ident_length = cp - bp;
  ident = bp;
  after_ident = cp;

  /* A line of just `#' becomes blank.  */

  if (ident_length == 0 && *after_ident == '\n') {
    ip->bufp = after_ident;
    return 1;
  }

  /*
   * Decode the keyword and call the appropriate expansion
   * routine, after moving the input pointer up to the next line.
   */
  for (kt = directive_table; kt->length > 0; kt++) {
    if (kt->length == ident_length
	&& !strncmp (kt->name, (const char *)ident, ident_length)) {
      U_CHAR *buf;
      U_CHAR *limit = ip->buf + ip->length;
      int unterminated = 0;

      /* Nonzero means do not delete comments within the directive.
	 #define needs this to detect traditional token paste.  */
      int keep_comments = kt->type == T_DEFINE;

      /* Find the end of this command (first newline not backslashed
	 and not in a string or comment).
	 Set COPY_COMMAND if the command must be copied
	 (it contains a backslash-newline or a comment).  */

      buf = bp = after_ident;
      while (bp < limit) {
	U_CHAR c = *bp++;
	switch (c) {
	case '\\':
	  if (bp < limit) {
	    if (*bp == '\n') {
	      ip->lineno++;
	      copy_command = 1;
	    }
	    bp++;
	  }
	  break;

	case '\'':
	case '\"':
	  bp = skip_quoted_string (bp - 1, limit, ip->lineno, &ip->lineno, &copy_command, &unterminated);
	  if (unterminated) {
	    /* Traditional preprocessing permits unterminated strings.  */
	    ip->bufp = bp;
	    goto endloop1;
	  }
	  break;

	  /* <...> is special for #include.  */
	case '<':
	  if (kt->type != T_INCLUDE)
	    break;
	  while (*bp && *bp != '>') bp++;
	  break;

	case '/':
	  if (*bp == '\\' && bp[1] == '\n')
	    newline_fix (bp);
	  if (*bp == '*') {
	    U_CHAR *obp = bp - 1;
	    ip->bufp = bp + 1;
	    skip_to_end_of_comment (ip, &ip->lineno);
	    bp = ip->bufp;
	    /* No need to copy the command because of a comment at the end;
	       just don't include the comment in the directive.  */
	    if (bp == limit || *bp == '\n') {
	      bp = obp;
	      goto endloop1;
	    }
	    /* Don't remove the comments if this is #define.  */
	    if (! keep_comments)
	      copy_command++;
	  }
	  break;

	case '\n':
	  --bp;		/* Point to the newline */
	  ip->bufp = bp;
	  goto endloop1;
	}
      }
      ip->bufp = bp;

    endloop1:
      resume_p = ip->bufp;
      /* BP is the end of the directive.
	 RESUME_P is the next interesting data after the directive.
	 A comment may come between.  */

      if (copy_command) {
	U_CHAR *xp = buf;
	/* Need to copy entire command into temp buffer before dispatching */

	cp = (U_CHAR *) alloca (bp - buf + 5); /* room for cmd plus
						  some slop */
	buf = cp;

	/* Copy to the new buffer, deleting comments
	   and backslash-newlines (and whitespace surrounding the latter).  */

	while (xp < bp) {
	  U_CHAR c = *xp++;
	  *cp++ = c;

	  switch (c) {
	  case '\n':
	    break;

	    /* <...> is special for #include.  */
	  case '<':
	    if (kt->type != T_INCLUDE)
	      break;
	    while (xp < bp && c != '>') {
	      c = *xp++;
	      if (c == '\\' && xp < bp && *xp == '\n')
		xp++, ip->lineno++;
	      else
		*cp++ = c;
	    }
	    break;

	  case '\\':
	    if (*xp == '\n') {
	      xp++;
	      cp--;
	      if (cp != buf && is_space (cp[-1])) {
		while (cp != buf && is_space(cp[-1])) cp--;
		cp++;
		SKIP_WHITE_SPACE (xp);
	      } else if (is_nvspace (*xp)) {
		*cp++ = *xp++;
		SKIP_WHITE_SPACE (xp);
	      }
	    } else {
	      *cp++ = *xp++;
	    }
	    break;

	  case '\'':
	  case '\"':
	    {
	      const U_CHAR *bp1
		= skip_quoted_string (xp - 1, limit, ip->lineno, 0, 0, 0);
	      while (xp != bp1)
		*cp++ = *xp++;
	    }
	    break;

	  case '/':
	    if (*xp == '*') {
	      ip->bufp = xp + 1;
	      skip_to_end_of_comment (ip, 0);
	      if (keep_comments)
		while (xp != ip->bufp)
		  *cp++ = *xp++;
	      /* Delete the slash.  */
	      else
		cp--;
	      xp = ip->bufp;
	    }
	  }
	}

	/* Null-terminate the copy.  */

	*cp = 0;
      }
      else
	cp = bp;

      ip->bufp = resume_p;

      /* Call the appropriate command handler.  buf now points to
	 either the appropriate place in the input buffer, or to
	 the temp buffer if it was necessary to make one.  cp
	 points to the first char after the contents of the (possibly
	 copied) command, in either case. */
      (*kt->func) (buf, cp, op);
      check_expand (op, ip->length - (ip->bufp - ip->buf));

      return 1;
    }
  }

  return 0;
}

static const char *const
monthnames[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

/*
 * expand things like __FILE__.  Place the expansion into the output
 * buffer *without* rescanning.
 */
static void
special_symbol (hp, op)
     HASHNODE *hp;
     FILE_BUF *op;
{
  const char *buf;
  time_t t;
  int i, len;
  int true_indepth;
  FILE_BUF *ip = NULL;
  static struct tm *timebuf = NULL;

  int paren = 0;		/* For special `defined' keyword */

  for (i = indepth; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      break;
    }
  if (ip == NULL)
    fatal ("not in any file?!");

  switch (hp->type) {
  case T_FILE:
  case T_BASE_FILE:
    {
      const char *string;
      if (hp->type == T_FILE)
	string = ip->fname;
      else
	string = instack[0].fname;

      if (string)
	{
	  char *tmp = (char *) alloca (3 + strlen (string));
	  sprintf (tmp, "\"%s\"", string);
	  buf = tmp;
	}
      else
	buf = "";

      break;
    }

  case T_INCLUDE_LEVEL:
    {
      char *tmp = (char *) alloca (8);	/* Eigth bytes ought to be more than enough */
      true_indepth = 0;
      for (i = indepth; i >= 0; i--)
	if (instack[i].fname != NULL)
	  true_indepth++;

    sprintf (tmp, "%d", true_indepth - 1);
    buf = tmp;
    break;
    }

  case T_VERSION:
    {
      char *tmp = (char *) alloca (3 + strlen (version_string));
      sprintf (tmp, "\"%s\"", version_string);
      buf = tmp;
      break;
    }

  case T_CONST:
    buf = hp->value.cpval;
    break;

  case T_SPECLINE:
    {
      char *tmp = (char *) alloca (10);
      sprintf (tmp, "%d", ip->lineno);
      buf = tmp;
      break;
    }

  case T_DATE:
  case T_TIME:
    {
      char *tmp = (char *) alloca (20);

      if (timebuf == NULL) {
	t = time (0);
	timebuf = localtime (&t);
      }
      if (hp->type == T_DATE)
	sprintf (tmp, "\"%s %2d %4d\"", monthnames[timebuf->tm_mon],
		 timebuf->tm_mday, timebuf->tm_year + 1900);
      else
	sprintf (tmp, "\"%02d:%02d:%02d\"", timebuf->tm_hour, timebuf->tm_min,
		 timebuf->tm_sec);
      buf = tmp;
      break;
    }

  case T_SPEC_DEFINED:
    buf = " 0 ";			/* Assume symbol is not defined */
    ip = &instack[indepth];
    SKIP_WHITE_SPACE (ip->bufp);
    if (*ip->bufp == '(') {
      paren++;
      ip->bufp++;			/* Skip over the paren */
      SKIP_WHITE_SPACE (ip->bufp);
    }

    if (!is_idstart (*ip->bufp))
      goto oops;
    {
      HASHNODE *hp = lookup (ip->bufp, -1, -1);

      if (hp && hp->type != T_UNUSED && hp->type != T_SPEC_DEFINED)
	buf = " 1 ";
    }
    while (is_idchar (*ip->bufp))
      ++ip->bufp;
    SKIP_WHITE_SPACE (ip->bufp);
    if (paren) {
      if (*ip->bufp != ')')
	goto oops;
      ++ip->bufp;
    }
    break;

oops:

    error ("`defined' must be followed by ident or (ident)");
    break;

  default:
    error ("cccp error: invalid special hash type"); /* time for gdb */
    abort ();
  }
  len = strlen (buf);
  check_expand (op, len);
  memcpy (op->bufp, buf, len);
  op->bufp += len;
}


/* Routines to handle #directives */

/*
 * Process include file by reading it in and calling rescan.
 * Expects to see "fname" or <fname> on the input.
 */
static void
do_include (buf, limit, op)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
{
  U_CHAR *fbeg, *fend;		/* Beginning and end of fname */

  struct file_name_list *stackp = include; /* Chain of dirs to search */
  struct file_name_list dsp[1];	/* First in chain, if #include "..." */
  int flen;

  int retried = 0;		/* Have already tried macro
				   expanding the include line*/
  FILE_BUF trybuf;		/* It got expanded into here */
  int system_header_p = 0;	/* 0 for "...", 1 for <...> */

get_filename:

  fbeg = buf;
  SKIP_WHITE_SPACE (fbeg);
  /* Discard trailing whitespace so we can easily see
     if we have parsed all the significant chars we were given.  */
  while (limit != fbeg && is_nvspace (limit[-1])) limit--;

  switch (*fbeg++) {
  case '\"':
    fend = fbeg;
    while (fend != limit && *fend != '\"')
      fend++;
    if (*fend == '\"' && fend + 1 == limit) {
      FILE_BUF *fp;

      /* We have "filename".  Figure out directory this source
	 file is coming from and put it on the front of the list. */

      /* If -I- was specified, don't search current dir, only spec'd ones. */
      if (ignore_srcdir) break;

      for (fp = &instack[indepth]; fp >= instack; fp--)
	{
	  size_t n;
	  const char *ep, *nam;

	  if ((nam = fp->fname) != NULL) {
	    /* Found a named file.  Figure out dir of the file,
	       and put it in front of the search list.  */
	    dsp[0].next = stackp;
	    stackp = dsp;
	    ep = strrchr (nam, '/');
	    if (ep != NULL) {
	      char *f; 
	      n = ep - nam;
	      f = (char *) alloca (n + 1);
	      strncpy (f, nam, n);
	      f[n] = '\0';
	      dsp[0].fname = f;
	      if (n > max_include_len) max_include_len = n;
	    } else {
	      dsp[0].fname = 0; /* Current directory */
	    }
	    break;
	  }
	}
      break;
    }
    goto fail;

  case '<':
    fend = fbeg;
    while (fend != limit && *fend != '>') fend++;
    if (*fend == '>' && fend + 1 == limit) {
      system_header_p = 1;
      /* If -I-, start with the first -I dir after the -I-.  */
      if (first_bracket_include)
	stackp = first_bracket_include;
      break;
    }
    goto fail;

  default:
  fail:
    if (retried) {
      error ("#include expects \"fname\" or <fname>");
      return;
    } else {
      trybuf = expand_to_temp_buffer (buf, limit, 0);
      buf = (U_CHAR *) alloca (trybuf.bufp - trybuf.buf + 1);
      memcpy (buf, trybuf.buf, trybuf.bufp - trybuf.buf);
      limit = buf + (trybuf.bufp - trybuf.buf);
      free (trybuf.buf);
      retried++;
      goto get_filename;
    }
  }

  flen = fend - fbeg;
  process_include (stackp, fbeg, flen, system_header_p, op);
}

static void
do_include_next (buf, limit, op)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
{
  U_CHAR *fbeg, *fend;		/* Beginning and end of fname */

  struct file_name_list *stackp; /* Chain of dirs to search */
  int flen;

  int retried = 0;		/* Have already tried macro
				   expanding the include line*/
  FILE_BUF trybuf;		/* It got expanded into here */
  int system_header_p = 0;	/* 0 for "...", 1 for <...> */

  /* Treat as plain #include if we don't know where to start
     looking.  */
  stackp = instack[indepth].next_header_dir;
  if (stackp == 0)
    {
      do_include (buf, limit, op);
      return;
    }

get_filename:

  fbeg = buf;
  SKIP_WHITE_SPACE (fbeg);
  /* Discard trailing whitespace so we can easily see
     if we have parsed all the significant chars we were given.  */
  while (limit != fbeg && is_nvspace (limit[-1])) limit--;

  switch (*fbeg++) {
  case '\"':
    fend = fbeg;
    while (fend != limit && *fend != '\"')
      fend++;
    if (*fend == '\"' && fend + 1 == limit)
      break;
    goto fail;

  case '<':
    fend = fbeg;
    while (fend != limit && *fend != '>') fend++;
    if (*fend == '>' && fend + 1 == limit) {
      system_header_p = 1;
      break;
    }
    goto fail;

  default:
  fail:
    if (retried) {
      error ("#include expects \"fname\" or <fname>");
      return;
    } else {
      trybuf = expand_to_temp_buffer (buf, limit, 0);
      buf = (U_CHAR *) alloca (trybuf.bufp - trybuf.buf + 1);
      memcpy (buf, trybuf.buf, trybuf.bufp - trybuf.buf);
      limit = buf + (trybuf.bufp - trybuf.buf);
      free (trybuf.buf);
      retried++;
      goto get_filename;
    }
  }

  flen = fend - fbeg;
  process_include (stackp, fbeg, flen, system_header_p, op);
}

static void
process_include (stackp, fbeg, flen, system_header_p, op)
     struct file_name_list *stackp;
     const U_CHAR *fbeg;
     int flen;
     int system_header_p;
     FILE_BUF *op;
{
  char *fname;
  int f = -1;			/* file number */

  fname = (char *) alloca (max_include_len + flen + 2);
  /* + 2 above for slash and terminating null.  */

  /* If specified file name is absolute, just open it.  */

  if (IS_ABSOLUTE_PATHNAME (fbeg)) {
    strncpy (fname, (const char *)fbeg, flen);
    fname[flen] = 0;
    f = open (fname, O_RDONLY, 0666);
  } else {
    /* Search directory path, trying to open the file.
       Copy each filename tried into FNAME.  */

    for (; stackp; stackp = stackp->next) {
      if (stackp->fname) {
	strcpy (fname, stackp->fname);
	strcat (fname, "/");
	fname[strlen (fname) + flen] = 0;
      } else {
	fname[0] = 0;
      }
      strncat (fname, (const char *)fbeg, flen);
      if ((f = open (fname, O_RDONLY, 0666)) >= 0)
	break;
    }
  }

  if (f < 0) {
    strncpy (fname, (const char *)fbeg, flen);
    fname[flen] = 0;
    if (deps_missing_files
	&& print_deps > (system_header_p || (system_include_depth > 0))) {

      /* If requested as a system header, assume it belongs in
	 the first system header directory. */
      if (first_bracket_include)
	stackp = first_bracket_include;
      else
	stackp = include;

      if (!system_header_p || IS_ABSOLUTE_PATHNAME (fbeg) || !stackp->fname)
	deps_add_dep (deps, fname);
      else {
	char *p;
	int len = strlen(stackp->fname);

	p = (char *) alloca (len + flen + 2);
	memcpy (p, stackp->fname, len);
	p[len++] = '/';
	memcpy (p + len, fbeg, flen);
	len += flen;
	p[len] = '\0';
	deps_add_dep (deps, p);
      }
    } else if (print_deps
	       && print_deps <= (system_header_p
				 || (system_include_depth > 0)))
      warning ("no include path in which to find %.*s", flen, fbeg);
    else
      error_from_errno (fname);

  } else {

    /* Check to see if this include file is a once-only include file.
       If so, give up.  */

    struct file_name_list* ptr;

    for (ptr = dont_repeat_files; ptr; ptr = ptr->next) {
      if (!strcmp (ptr->fname, fname)) {
	close (f);
        return;				/* This file was once'd. */
      }
    }

    for (ptr = all_include_files; ptr; ptr = ptr->next) {
      if (!strcmp (ptr->fname, fname))
        break;				/* This file was included before. */
    }

    if (ptr == 0) {
      /* This is the first time for this file.  */
      /* Add it to list of files included.  */

      ptr = (struct file_name_list *) xmalloc (sizeof (struct file_name_list));
      ptr->next = all_include_files;
      all_include_files = ptr;
      ptr->fname = xstrdup (fname);

      /* For -M, add this file to the dependencies.  */
      if (print_deps > (system_header_p || (system_include_depth > 0)))
	deps_add_dep (deps, fname);
    }   

    if (system_header_p)
      system_include_depth++;

    /* Actually process the file.  */
    finclude (f, fname, stackp->next, op);

    if (system_header_p)
      system_include_depth--;

    close (f);
  }
}

/* Replace all CR NL, NL CR and CR sequences with NL.  */

static void
fixup_newlines (fp)
     FILE_BUF *fp;
{
  U_CHAR *p, *q, *end;

  if (fp->length <= 0)
    return;

  end = fp->buf + fp->length;
  *end = '\r';
  p = (U_CHAR *) strchr ((const char *) fp->buf, '\r');
  *end = '\0';
  if (p == end)
    return;

  if (p > fp->buf && p[-1] == '\n')
    p--;
  q = p;
  while (p < end)
    switch (*p)
      {
      default:
	*q++ = *p++;
	break;
      case '\n':
      case '\r':
	p += 1 + (p[0] + p[1] == '\n' + '\r');
	*q++ = '\n';
	break;
      }

  fp->length = q - fp->buf;
}

/* Process the contents of include file FNAME, already open on descriptor F,
   with output to OP.  */

static void
finclude (f, fname, nhd, op)
     int f;
     const char *fname;
     struct file_name_list *nhd;
     FILE_BUF *op;
{
  int st_mode;
  long st_size;
  long i;
  FILE_BUF *fp;			/* For input stack frame */

  CHECK_DEPTH (return;);

  if (file_size_and_mode (f, &st_mode, &st_size))
    goto nope;

  fp = &instack[indepth + 1];
  memset (fp, 0, sizeof (FILE_BUF));
  fp->fname = fname;
  fp->length = 0;
  fp->lineno = 1;
  fp->if_stack = if_stack;
  fp->next_header_dir = nhd;

  if (S_ISREG (st_mode)) {
    fp->buf = (U_CHAR *) xmalloc (st_size + 2);
    fp->bufp = fp->buf;

    /* Read the file contents, knowing that st_size is an upper bound
       on the number of bytes we can read.  */
    while (st_size > 0) {
      i = read (f, fp->buf + fp->length, st_size);
      if (i <= 0) {
	if (i == 0) break;
	goto nope;
      }
      fp->length += i;
      st_size -= i;
    }
  }
  else {
    /* Cannot count its file size before reading.  */

    U_CHAR *bufp;
    U_CHAR *basep;
    int bsize = 2000;

    st_size = 0;
    basep = (U_CHAR *) xmalloc (bsize + 2);
    bufp = basep;

    for (;;) {
      i = read (f, bufp, bsize - st_size);
      if (i < 0)
	goto nope;      /* error! */
      if (i == 0)
	break;	/* End of file */
      st_size += i;
      bufp += i;
      if (bsize == st_size) {	/* Buffer is full! */
	  bsize *= 2;
	  basep = (U_CHAR *) xrealloc (basep, bsize + 2);
	  bufp = basep + st_size;	/* May have moved */
	}
    }
    fp->buf = basep;
    fp->bufp = fp->buf;
    fp->length = st_size;
  }
  close (f);
  fixup_newlines (fp);

  /* Make sure data ends with a newline.  And put a null after it.  */

  if (fp->length > 0 && fp->buf[fp->length-1] != '\n')
    fp->buf[fp->length++] = '\n';
  fp->buf[fp->length] = '\0';

  indepth++;
  output_line_command (fp, op, 0, enter_file);
  rescan (op, 0);
  indepth--;
  instack[indepth].lineno++;
  instack[indepth].bufp++;	/* Skip the new line.  */
  output_line_command (&instack[indepth], op, 0, leave_file);
  free (fp->buf);
  return;

nope:
  perror_with_name (fname);
  close (f);
}


/* Process a #define command.
BUF points to the contents of the #define command, as a continguous string.
LIMIT points to the first character past the end of the definition.
KEYWORD is the keyword-table entry for #define.  */

static void
do_define (buf, limit, op)
     U_CHAR *buf, *limit;
     FILE_BUF *op ATTRIBUTE_UNUSED;
{
  U_CHAR *bp;			/* temp ptr into input buffer */
  U_CHAR *symname;		/* remember where symbol name starts */
  int sym_length;		/* and how long it is */

  DEFINITION *defn;
  int arglengths = 0;		/* Accumulate lengths of arg names
				   plus number of args.  */
  int hashcode;

  bp = buf;

  while (is_nvspace (*bp))
    bp++;

  symname = bp;			/* remember where it starts */
  while (is_idchar (*bp) && bp < limit) {
    bp++;
  }
  sym_length = bp - symname;
  if (sym_length == 0)
    {
      error ("invalid macro name");
      return;
    }
  else if (!is_idstart (*symname)) {
    U_CHAR *msg;			/* what pain... */
    msg = (U_CHAR *) alloca (sym_length + 1);
    memcpy (msg, symname, sym_length);
    msg[sym_length] = 0;
    error ("invalid macro name `%s'", msg);
    return;
  } else {
    if (! strncmp ((const char *)symname, "defined", 7) && sym_length == 7)
      {
	error ("\"defined\" cannot be used as a macro name");
	return;
      }
  }

  /* lossage will occur if identifiers or control keywords are broken
     across lines using backslash.  This is not the right place to take
     care of that. */

  if (*bp == '(') {
    struct arglist *arg_ptrs = NULL;
    int argno = 0;

    bp++;			/* skip '(' */
    SKIP_WHITE_SPACE (bp);

    /* Loop over macro argument names.  */
    while (*bp != ')') {
      struct arglist *temp;

      temp = (struct arglist *) alloca (sizeof (struct arglist));
      temp->name = bp;
      temp->next = arg_ptrs;
      temp->argno = argno++;
      arg_ptrs = temp;

      if (!is_idstart (*bp))
	warning ("parameter name starts with a digit in #define");

      /* Find the end of the arg name.  */
      while (is_idchar (*bp)) {
	bp++;
      }
      temp->length = bp - temp->name;
      arglengths += temp->length + 2;
      SKIP_WHITE_SPACE (bp);
      if (temp->length == 0 || (*bp != ',' && *bp != ')')) {
	error ("badly punctuated parameter list in #define");
	return;
      }
      if (*bp == ',') {
	bp++;
	SKIP_WHITE_SPACE (bp);
      }
      if (bp >= limit) {
	error ("unterminated parameter list in #define");
	return;
      }
    }

    ++bp;					/* skip paren */
    while (is_nvspace (*bp) && bp < limit)	/* and leading whitespace */
      ++bp;
    /* now everything from bp before limit is the definition. */
    defn = collect_expansion (bp, limit, argno, arg_ptrs);

    /* Now set defn->argnames to the result of concatenating
       the argument names in reverse order
       with comma-space between them.  */
    {
      struct arglist *temp;
      int i = 0;
      U_CHAR *tmp = (U_CHAR *) xmalloc (arglengths + 1);

      for (temp = arg_ptrs; temp; temp = temp->next) {
	memcpy (&tmp[i], temp->name, temp->length);
	i += temp->length;
	if (temp->next != 0) {
	  tmp[i++] = ',';
	  tmp[i++] = ' ';
	}
      }
      tmp[i] = 0;
      defn->argnames = tmp;
      
    }
  } else {
    /* simple expansion or empty definition; skip leading whitespace */
    while (is_nvspace (*bp) && bp < limit)
      ++bp;
    /* now everything from bp before limit is the definition. */
    defn = collect_expansion (bp, limit, -1, 0);
    defn->argnames = (const U_CHAR *) "";
  }

  hashcode = hashf (symname, sym_length, HASHSIZE);

  {
    HASHNODE *hp;
    if ((hp = lookup (symname, sym_length, hashcode)) == NULL)
      hp = install (symname, sym_length, T_MACRO, hashcode);
    else {
      if (hp->type != T_MACRO || compare_defs (defn, hp->value.defn))
	warning ("\"%.*s\" redefined", sym_length, symname);

      /* Replace the old definition.  */
      hp->type = T_MACRO;
    }

    hp->value.defn = defn;
  }
}

/*
 * return zero if two DEFINITIONs are isomorphic
 */
static int
compare_defs (d1, d2)
     DEFINITION *d1, *d2;
{
  struct reflist *a1, *a2;
  U_CHAR *p1 = d1->expansion;
  U_CHAR *p2 = d2->expansion;
  int first = 1;

  if (d1->nargs != d2->nargs)
    return 1;
  if (strcmp ((const char *)d1->argnames, (const char *)d2->argnames))
    return 1;
  for (a1 = d1->pattern, a2 = d2->pattern; a1 && a2;
       a1 = a1->next, a2 = a2->next) {
    if (!((a1->nchars == a2->nchars
	   && ! strncmp ((const char *)p1, (const char *)p2, a1->nchars))
	  || ! comp_def_part (first, p1, a1->nchars, p2, a2->nchars, 0))
	|| a1->argno != a2->argno
	|| a1->stringify != a2->stringify
	|| a1->raw_before != a2->raw_before
	|| a1->raw_after != a2->raw_after)
      return 1;
    first = 0;
    p1 += a1->nchars;
    p2 += a2->nchars;
  }
  if (a1 != a2)
    return 1;
  if (comp_def_part (first, p1, d1->length - (p1 - d1->expansion),
		     p2, d2->length - (p2 - d2->expansion), 1))
    return 1;
  return 0;
}

/* Return 1 if two parts of two macro definitions are effectively different.
   One of the parts starts at BEG1 and has LEN1 chars;
   the other has LEN2 chars at BEG2.
   Any sequence of whitespace matches any other sequence of whitespace.
   FIRST means these parts are the first of a macro definition;
    so ignore leading whitespace entirely.
   LAST means these parts are the last of a macro definition;
    so ignore trailing whitespace entirely.  */
static int
comp_def_part (first, beg1, len1, beg2, len2, last)
     int first;
     const U_CHAR *beg1, *beg2;
     int len1, len2;
     int last;
{
  const U_CHAR *end1 = beg1 + len1;
  const U_CHAR *end2 = beg2 + len2;
  if (first) {
    while (beg1 != end1 && is_space (*beg1)) beg1++;
    while (beg2 != end2 && is_space (*beg2)) beg2++;
  }
  if (last) {
    while (beg1 != end1 && is_space (end1[-1])) end1--;
    while (beg2 != end2 && is_space (end2[-1])) end2--;
  }
  while (beg1 != end1 && beg2 != end2) {
    if (is_space (*beg1) && is_space (*beg2)) {
      while (beg1 != end1 && is_space (*beg1)) beg1++;
      while (beg2 != end2 && is_space (*beg2)) beg2++;
    } else if (*beg1 == *beg2) {
      beg1++; beg2++;
    } else break;
  }
  return (beg1 != end1) || (beg2 != end2);
}

/* Read a replacement list for a macro with parameters.
   Build the DEFINITION structure.
   Reads characters of text starting at BUF until LIMIT.
   ARGLIST specifies the formal parameters to look for
   in the text of the definition; NARGS is the number of args
   in that list, or -1 for a macro name that wants no argument list.
   MACRONAME is the macro name itself (so we can avoid recursive expansion)
   and NAMELEN is its length in characters.
   
Note that comments and backslash-newlines have already been deleted
from the argument.  */

/* Leading and trailing Space, Tab, etc. are converted to markers
   Newline Space, Newline Tab, etc.
   Newline Space makes a space in the final output
   but is discarded if stringified.  (Newline Tab is similar but
   makes a Tab instead.)

   If there is no trailing whitespace, a Newline Space is added at the end
   to prevent concatenation that would be contrary to the standard.  */

static DEFINITION *
collect_expansion (buf, end, nargs, arglist)
     U_CHAR *buf, *end;
     int nargs;
     struct arglist *arglist;
{
  DEFINITION *defn;
  U_CHAR *p, *limit, *lastp, *exp_p;
  struct reflist *endpat = NULL;
  /* Pointer to first nonspace after last ## seen.  */
  U_CHAR *concat = 0;
  /* Pointer to first nonspace after last single-# seen.  */
  U_CHAR *stringify = 0;
  int maxsize;
  int expected_delimiter = '\0';

  /* Scan thru the replacement list, ignoring comments and quoted
     strings, picking up on the macro calls.  It does a linear search
     thru the arg list on every potential symbol.  Profiling might say
     that something smarter should happen. */

  if (end < buf)
    abort ();

  /* Find the beginning of the trailing whitespace.  */
  /* Find end of leading whitespace.  */
  limit = end;
  p = buf;
  while (p < limit && is_space (limit[-1])) limit--;
  while (p < limit && is_space (*p)) p++;

  /* Allocate space for the text in the macro definition.
     Leading and trailing whitespace chars need 2 bytes each.
     Each other input char may or may not need 1 byte,
     so this is an upper bound.
     The extra 2 are for invented trailing newline-marker and final null.  */
  maxsize = (sizeof (DEFINITION)
	     + 2 * (end - limit) + 2 * (p - buf)
	     + (limit - p) + 3);
  defn = (DEFINITION *) xcalloc (1, maxsize);

  defn->nargs = nargs;
  exp_p = defn->expansion = (U_CHAR *) defn + sizeof (DEFINITION);
  lastp = exp_p;

  p = buf;

  /* Convert leading whitespace to Newline-markers.  */
  while (p < limit && is_space (*p)) {
    *exp_p++ = '\n';
    *exp_p++ = *p++;
  }

  /* Process the main body of the definition.  */
  while (p < limit) {
    int skipped_arg = 0;
    U_CHAR c = *p++;

    *exp_p++ = c;

    /* In -traditional mode, recognize arguments inside strings and
       and character constants, and ignore special properties of #.
       Arguments inside strings are considered "stringified", but no
       extra quote marks are supplied.  */
    switch (c) {
    case '\'':
    case '\"':
      if (expected_delimiter != '\0') {
	if (c == expected_delimiter)
	  expected_delimiter = '\0';
      } else
	expected_delimiter = c;
      break;

    case '\\':
      /* Backslash quotes delimiters and itself, but not macro args.  */
      if (expected_delimiter != 0 && p < limit
	  && (*p == expected_delimiter || *p == '\\')) {
	*exp_p++ = *p++;
	continue;
      }
      break;

    case '/':
      if (expected_delimiter != '\0') /* No comments inside strings.  */
	break;
      if (*p == '*') {
	/* If we find a comment that wasn't removed by handle_directive,
	   this must be -traditional.  So replace the comment with
	   nothing at all.  */
	exp_p--;
	p += 1;
	while (p < limit && !(p[-2] == '*' && p[-1] == '/'))
	  p++;
      }
      break;
    }

    if (is_idchar (c) && nargs > 0) {
      U_CHAR *id_beg = p - 1;
      int id_len;

      --exp_p;
      while (p != limit && is_idchar (*p)) p++;
      id_len = p - id_beg;

      if (is_idstart (c)) {
	struct arglist *arg;

	for (arg = arglist; arg != NULL; arg = arg->next) {
	  struct reflist *tpat;

	  if (arg->name[0] == c
	      && arg->length == id_len
	      && strncmp ((const char *)arg->name,
			  (const char *)id_beg, id_len) == 0) {
	    /* make a pat node for this arg and append it to the end of
	       the pat list */
	    tpat = (struct reflist *) xmalloc (sizeof (struct reflist));
	    tpat->next = NULL;
	    tpat->raw_before = concat == id_beg;
	    tpat->raw_after = 0;
	    tpat->stringify = expected_delimiter != '\0';

	    if (endpat == NULL)
	      defn->pattern = tpat;
	    else
	      endpat->next = tpat;
	    endpat = tpat;

	    tpat->argno = arg->argno;
	    tpat->nchars = exp_p - lastp;
	    {
	      U_CHAR *p1 = p;
	      SKIP_WHITE_SPACE (p1);
	      if (p1 + 2 <= limit && p1[0] == '#' && p1[1] == '#')
		tpat->raw_after = 1;
	    }
	    lastp = exp_p;	/* place to start copying from next time */
	    skipped_arg = 1;
	    break;
	  }
	}
      }

      /* If this was not a macro arg, copy it into the expansion.  */
      if (! skipped_arg) {
	U_CHAR *lim1 = p;
	p = id_beg;
	while (p != lim1)
	  *exp_p++ = *p++;
	if (stringify == id_beg)
	  error ("# operator should be followed by a macro argument name");
      }
    }
  }

  if (limit < end) {
    /* Convert trailing whitespace to Newline-markers.  */
    while (limit < end && is_space (*limit)) {
      *exp_p++ = '\n';
      *exp_p++ = *limit++;
    }
  }
  *exp_p = '\0';

  defn->length = exp_p - defn->expansion;

  /* Crash now if we overrun the allocated size.  */
  if (defn->length + 1 > maxsize)
    abort ();

  return defn;
}

/*
 * interpret #line command.  Remembers previously seen fnames
 * in its very own hash table.
 */
#define FNAME_HASHSIZE 37
static void
do_line (buf, limit, op)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
{
  U_CHAR *bp;
  FILE_BUF *ip = &instack[indepth];
  FILE_BUF tem;
  int new_lineno;
  enum file_change_code file_change = same_file;

  /* Expand any macros.  */
  tem = expand_to_temp_buffer (buf, limit, 0);

  /* Point to macroexpanded line, which is null-terminated now.  */
  bp = tem.buf;
  SKIP_WHITE_SPACE (bp);

  if (!ISDIGIT (*bp)) {
    error ("invalid format #line command");
    return;
  }

  /* The Newline at the end of this line remains to be processed.
     To put the next line at the specified line number,
     we must store a line number now that is one less.  */
  new_lineno = atoi ((const char *)bp);

  /* skip over the line number.  */
  while (ISDIGIT (*bp))
    bp++;

  SKIP_WHITE_SPACE (bp);

  if (*bp == '\"') {
    static HASHNODE *fname_table[FNAME_HASHSIZE];
    HASHNODE *hp, **hash_bucket;
    U_CHAR *fname;
    int fname_length;

    fname = ++bp;

    while (*bp && *bp != '\"')
      bp++;
    if (*bp != '\"') {
      error ("invalid format #line command");
      return;
    }

    fname_length = bp - fname;

    bp++;
    SKIP_WHITE_SPACE (bp);
    if (*bp) {
      if (*bp == '1')
	file_change = enter_file;
      else if (*bp == '2')
	file_change = leave_file;
      else {
	error ("invalid format #line command");
	return;
      }

      bp++;
      SKIP_WHITE_SPACE (bp);
      if (*bp) {
	error ("invalid format #line command");
	return;
      }
    }

    hash_bucket =
      &fname_table[hashf (fname, fname_length, FNAME_HASHSIZE)];
    for (hp = *hash_bucket; hp != NULL; hp = hp->next)
      if (hp->length == fname_length &&
	  strncmp (hp->value.cpval, (const char *)fname, fname_length) == 0) {
	ip->fname = hp->value.cpval;
	break;
      }
    if (hp == 0) {
      char *q;
      /* Didn't find it; cons up a new one.  */
      hp = (HASHNODE *) xcalloc (1, sizeof (HASHNODE) + fname_length + 1);
      hp->next = *hash_bucket;
      *hash_bucket = hp;

      hp->length = fname_length;
      ip->fname = hp->value.cpval = q = ((char *) hp) + sizeof (HASHNODE);
      memcpy (q, fname, fname_length);
    }
  } else if (*bp) {
    error ("invalid format #line command");
    return;
  }

  ip->lineno = new_lineno;
  output_line_command (ip, op, 0, file_change);
  ip->bufp++;			/* Skip the new line.  */
  check_expand (op, ip->length - (ip->bufp - ip->buf));
}

/*
 * remove all definitions of symbol from symbol table.
 * according to un*x /lib/cpp, it is not an error to undef
 * something that has no definitions, so it isn't one here either.
 */
static void
do_undef (buf, limit, op)
     U_CHAR *buf;
     U_CHAR *limit ATTRIBUTE_UNUSED;
     FILE_BUF *op ATTRIBUTE_UNUSED;
{
  HASHNODE *hp;

  SKIP_WHITE_SPACE (buf);

  if (! strncmp ((const char *)buf, "defined", 7) && ! is_idchar (buf[7]))
    warning ("undefining `defined'");

  while ((hp = lookup (buf, -1, -1)) != NULL) {
    if (hp->type != T_MACRO)
      warning ("undefining `%s'", hp->name);
    delete_macro (hp);
  }
}

/* Read the tokens of the answer into the macro pool.  Only commit the
   memory if we intend it as permanent storage, i.e. the #assert case.
   Returns 0 on success.  */

static int
parse_answer (buf, limit, answerp, type)
     const unsigned char *buf, *limit;
     struct answer **answerp;
     int type;
{
  const unsigned char *start;

  /* Skip leading whitespace.  */
  if (buf < limit && *buf == ' ')
    buf++;

  /* Parentheses are optional here.  */
  if (buf == limit && type == T_UNASSERT)
    return 0;

  if (buf == limit || *buf++ != '(')
    {
      if (type == T_IF)
	return 0;

      error ("missing '(' after predicate");
      return 1;
    }

  /* Drop whitespace at start.  */
  while (buf < limit && *buf == ' ')
    buf++;

  start = buf;
  while (buf < limit && *buf != ')')
    buf++;

  if (buf == limit)
    {
      error ("missing ')' to complete answer");
      return 1;
    }

  if (buf == start)
    {
      error ("predicate's answer is empty");
      return 1;
    }

  if ((type == T_ASSERT || type == T_UNASSERT) && buf + 1 != limit)
    {
      error ("extra text at end of directive");
      return 1;
    }

  /* Lose trailing whitespace.  */
  if (buf[-1] == ' ')
    buf--;

  *answerp = (struct answer *) xmalloc (sizeof (struct answer));
  (*answerp)->answer = start;
  (*answerp)->len = buf - start;

  return 0;
}

/* Parses an assertion, returning a pointer to the hash node of the
   predicate, or 0 on error.  If an answer was supplied, it is placed
   in ANSWERP, otherwise it is set to 0.  */
static HASHNODE *
parse_assertion (buf, limit, answerp, type)
     const unsigned char *buf, *limit;
     struct answer **answerp;
     int type;
{
  HASHNODE *result = 0;
  const unsigned char *climit;
  unsigned char *bp, *symname = canonicalize_text (buf, limit, &climit);
  unsigned int len;

  bp = symname;
  if (bp < climit && is_idstart (*bp))
    {
      do
	bp++;
      while (bp < climit && is_idchar (*bp));
    }
  len = bp - symname;

  *answerp = 0;
  if (len == 0)
    {
      if (symname == climit)
	error ("assertion without predicate");
      else
	error ("predicate must be an identifier");
    }
  /* Unfortunately, because of the way we handle #if, we don't avoid
     macro expansion in answers.  This is not easy to fix.  */
  else if (parse_answer (bp, climit, answerp, type) == 0)
    {
      unsigned char *sym = alloca (len + 1);
      int hashcode;
      
      /* Prefix '#' to get it out of macro namespace.  */
      sym[0] = '#';
      memcpy (sym + 1, symname, len);

      hashcode = hashf (sym, len + 1, HASHSIZE);
      result = lookup (sym, len + 1, hashcode);
      if (result == 0)
	result = install (sym, len + 1, T_UNUSED, hashcode);
    }

  return result;
}

/* Test an assertion within a preprocessor conditional.  Returns zero
   on error or failure, one on success.  */
int
test_assertion (pbuf)
     unsigned char **pbuf;	/* NUL-terminated.  */
{
  unsigned char *buf = *pbuf;
  unsigned char *limit = buf + strlen ((char *) buf);
  struct answer *answer;
  HASHNODE *node;
  int result = 0;

  node = parse_assertion (buf, limit, &answer, T_IF);
  if (node)
    {
      result = (node->type == T_ASSERT &&
		(answer == 0 || *find_answer (node, answer) != 0));

      /* Yuk.  We update pbuf to point after the assertion test.
	 First, move past the identifier.  */
      if (is_space (*buf))
	buf++;
      while (is_idchar (*buf))
	buf++;
      /* If we have an answer, we need to move past the parentheses.  */
      if (answer)
	while (*buf++ != ')')
	  ;
      *pbuf = buf;
    }

  return result;
}

/* Handle a #error directive.  */
static void
do_error (buf, limit, op)
     U_CHAR *buf;
     U_CHAR *limit;
     FILE_BUF *op ATTRIBUTE_UNUSED;
{
  error ("#error%.*s", (int) (limit - buf), buf);
}

/* Handle a #warning directive.  */
static void
do_warning (buf, limit, op)
     U_CHAR *buf;
     U_CHAR *limit;
     FILE_BUF *op ATTRIBUTE_UNUSED;
{
  warning ("#warning%.*s", (int) (limit - buf), buf);
}

/* Handle a #assert directive.  */
static void
do_assert (buf, limit, op)
     U_CHAR *buf;
     U_CHAR *limit;
     FILE_BUF *op ATTRIBUTE_UNUSED;
{
  struct answer *new_answer;
  HASHNODE *node;
  
  node = parse_assertion (buf, limit, &new_answer, T_ASSERT);
  if (node)
    {
      /* Place the new answer in the answer list.  First check there
         is not a duplicate.  */
      new_answer->next = 0;
      if (node->type == T_ASSERT)
	{
	  if (*find_answer (node, new_answer))
	    {
	      free (new_answer);
	      warning ("\"%s\" re-asserted", node->name + 1);
	      return;
	    }
	  new_answer->next = node->value.answers;
	}
      node->type = T_ASSERT;
      node->value.answers = new_answer;
    }
}

/* Function body to be provided later.  */
static void
do_unassert (buf, limit, op)
     U_CHAR *buf;
     U_CHAR *limit;
     FILE_BUF *op ATTRIBUTE_UNUSED;
{
  HASHNODE *node;
  struct answer *answer;
  
  node = parse_assertion (buf, limit, &answer, T_UNASSERT);
  /* It isn't an error to #unassert something that isn't asserted.  */
  if (node)
    {
      if (node->type == T_ASSERT)
	{
	  if (answer)
	    {
	      struct answer **p = find_answer (node, answer), *temp;

	      /* Remove the answer from the list.  */
	      temp = *p;
	      if (temp)
		*p = temp->next;

	      /* Did we free the last answer?  */
	      if (node->value.answers == 0)
		delete_macro (node);
	    }
	  else
	    delete_macro (node);
	}

      free (answer);
    }
}

/* Returns a pointer to the pointer to the answer in the answer chain,
   or a pointer to NULL if the answer is not in the chain.  */
static struct answer **
find_answer (node, candidate)
     HASHNODE *node;
     const struct answer *candidate;
{
  struct answer **result;

  for (result = &node->value.answers; *result; result = &(*result)->next)
    {
      struct answer *answer = *result;

      if (answer->len == candidate->len
	  && !memcmp (answer->answer, candidate->answer, answer->len))
	break;
    }

  return result;
}

/* Return a malloced buffer with leading and trailing whitespace
   removed, and all instances of internal whitespace reduced to a
   single space.  */
static unsigned char *
canonicalize_text (buf, limit, climit)
     const unsigned char *buf, *limit, **climit;
{
  unsigned int len = limit - buf;
  unsigned char *result = (unsigned char *) xmalloc (len), *dest;

  for (dest = result; buf < limit;)
    {
      if (! is_space (*buf))
	*dest++ = *buf++;
      else
	{
	  while (++buf < limit && is_space (*buf))
	    ;
	  if (dest != result && buf != limit)
	    *dest++ = ' ';
	}
    }

  *climit = dest;
  return result;
}

/*
 * handle #if command by
 *   1) inserting special `defined' keyword into the hash table
 *	that gets turned into 0 or 1 by special_symbol (thus,
 *	if the luser has a symbol called `defined' already, it won't
 *      work inside the #if command)
 *   2) rescan the input into a temporary output buffer
 *   3) pass the output buffer to the yacc parser and collect a value
 *   4) clean up the mess left from steps 1 and 2.
 *   5) call conditional_skip to skip til the next #endif (etc.),
 *      or not, depending on the value from step 3.
 */
static void
do_if (buf, limit, op)
     U_CHAR *buf, *limit;
     FILE_BUF *op ATTRIBUTE_UNUSED;
{
  int value;
  FILE_BUF *ip = &instack[indepth];

  value = eval_if_expression (buf, limit - buf);
  conditional_skip (ip, value == 0, T_IF);
}

/*
 * handle a #elif directive by not changing  if_stack  either.
 * see the comment above do_else.
 */
static void
do_elif (buf, limit, op)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
{
  int value;
  FILE_BUF *ip = &instack[indepth];

  if (if_stack == instack[indepth].if_stack) {
    error ("#elif not within a conditional");
    return;
  } else {
    if (if_stack->type != T_IF && if_stack->type != T_ELIF) {
      error ("#elif after #else");
      fprintf (stderr, " (matches line %d", if_stack->lineno);
      if (if_stack->fname != NULL && ip->fname != NULL &&
	  strcmp (if_stack->fname, ip->fname) != 0)
	fprintf (stderr, ", file %s", if_stack->fname);
      fprintf (stderr, ")\n");
    }
    if_stack->type = T_ELIF;
  }

  if (if_stack->if_succeeded)
    skip_if_group (ip, 0);
  else {
    value = eval_if_expression (buf, limit - buf);
    if (value == 0)
      skip_if_group (ip, 0);
    else {
      ++if_stack->if_succeeded;	/* continue processing input */
      output_line_command (ip, op, 1, same_file);
    }
  }
}

/*
 * evaluate a #if expression in BUF, of length LENGTH,
 * then parse the result as a C expression and return the value as an int.
 */
static int
eval_if_expression (buf, length)
     const U_CHAR *buf;
     int length;
{
  FILE_BUF temp_obuf;
  HASHNODE *save_defined;
  int value;

  save_defined = install (U"defined", -1, T_SPEC_DEFINED, -1);
  temp_obuf = expand_to_temp_buffer (buf, buf + length, 0);
  delete_macro (save_defined);	/* clean up special symbol */

  value = parse_c_expression ((const char *)temp_obuf.buf);

  free (temp_obuf.buf);

  return value;
}

/*
 * routine to handle ifdef/ifndef.  Try to look up the symbol,
 * then do or don't skip to the #endif/#else/#elif depending
 * on what directive is actually being processed.
 */
static void
do_xifdef (buf, limit, type)
     U_CHAR *buf, *limit;
     enum node_type type;     
{
  int skip;
  FILE_BUF *ip = &instack[indepth];
  U_CHAR *end; 

  /* Discard leading and trailing whitespace.  */
  SKIP_WHITE_SPACE (buf);
  while (limit != buf && is_nvspace (limit[-1])) limit--;

  /* Find the end of the identifier at the beginning.  */
  for (end = buf; is_idchar (*end); end++);

  if (end == buf)
    skip = (type == T_IFDEF);
  else
    skip = (lookup (buf, end-buf, -1) == NULL) ^ (type == T_IFNDEF);

  conditional_skip (ip, skip, T_IF);
}

static void
do_ifdef (buf, limit, op)
     U_CHAR *buf, *limit;
     FILE_BUF *op ATTRIBUTE_UNUSED;
{
  do_xifdef (buf, limit, T_IFDEF);
}

static void
do_ifndef (buf, limit, op)
     U_CHAR *buf, *limit;
     FILE_BUF *op ATTRIBUTE_UNUSED;
{
  do_xifdef (buf, limit, T_IFNDEF);
}

/*
 * push TYPE on stack; then, if SKIP is nonzero, skip ahead.
 */
static void
conditional_skip (ip, skip, type)
     FILE_BUF *ip;
     int skip;
     enum node_type type;
{
  IF_STACK_FRAME *temp;

  temp = (IF_STACK_FRAME *) xcalloc (1, sizeof (IF_STACK_FRAME));
  temp->fname = ip->fname;
  temp->lineno = ip->lineno;
  temp->next = if_stack;
  if_stack = temp;

  if_stack->type = type;

  if (skip != 0) {
    skip_if_group (ip, 0);
    return;
  } else {
    ++if_stack->if_succeeded;
    output_line_command (ip, &outbuf, 1, same_file);
  }
}

/*
 * skip to #endif, #else, or #elif.  adjust line numbers, etc.
 * leaves input ptr at the sharp sign found.
 * If ANY is nonzero, return at next directive of any sort.
 */
static void
skip_if_group (ip, any)
     FILE_BUF *ip;
     int any;
{
  U_CHAR *bp = ip->bufp, *cp;
  U_CHAR *endb = ip->buf + ip->length;
  const struct directive *kt;
  IF_STACK_FRAME *save_if_stack = if_stack; /* don't pop past here */
  U_CHAR *beg_of_line = bp;

  while (bp < endb) {
    switch (*bp++) {
    case '/':			/* possible comment */
      if (*bp == '\\' && bp[1] == '\n')
	newline_fix (bp);
      if (*bp == '*') {
	ip->bufp = ++bp;
	bp = skip_to_end_of_comment (ip, &ip->lineno);
      }
      break;
    case '\"':
    case '\'':
      bp = skip_quoted_string (bp - 1, endb, ip->lineno, &ip->lineno, 0, 0);
      break;
    case '\\':
      /* Char after backslash loses its special meaning.  */
      if (bp < endb) {
	if (*bp == '\n')
	  ++ip->lineno;		/* But do update the line-count.  */
	bp++;
      }
      break;
    case '\n':
      ++ip->lineno;
      beg_of_line = bp;
      break;
    case '#':
      ip->bufp = bp - 1;

      /* # keyword: a # must be first nonblank char on the line */
      if (beg_of_line == 0)
	break;
      /* Scan from start of line, skipping whitespace, comments
	 and backslash-newlines, and see if we reach this #.
	 If not, this # is not special.  */
      bp = beg_of_line;
      while (1) {
	if (is_nvspace (*bp))
	  bp++;
	else if (*bp == '\\' && bp[1] == '\n')
	  bp += 2;
	else if (*bp == '/' && bp[1] == '*') {
	  bp += 2;
	  while (!(*bp == '*' && bp[1] == '/')) {
	    if (*bp == '\n')
	      ip->lineno++;
	    bp++;
	  }
	  bp += 2;
	}
	else break;
      }
      if (bp != ip->bufp) {
	bp = ip->bufp + 1;	/* Reset bp to after the #.  */
	break;
      }

      bp = ip->bufp + 1;	/* Point after '#'.  */

      /* Skip whitespace and \-newline.  */
      while (1) {
	if (is_nvspace (*bp))
	  bp++;
	else if (*bp == '\\' && bp[1] == '\n')
	  bp += 2;
	else if (*bp == '/' && bp[1] == '*') {
	  bp += 2;
	  while (!(*bp == '*' && bp[1] == '/'))
	    bp++;
	  bp += 2;
	}
	else break;
      }

      cp = bp;

      /* Now find end of directive name.
	 If we encounter a backslash-newline, exchange it with any following
	 symbol-constituents so that we end up with a contiguous name.  */

      while (1) {
	if (is_idchar (*bp))
	  bp++;
	else {
	  if (*bp == '\\' && bp[1] == '\n')
	    name_newline_fix (bp);
	  if (is_idchar (*bp))
	    bp++;
	  else break;
	}
      }

      for (kt = directive_table; kt->length >= 0; kt++) {
	IF_STACK_FRAME *temp;
	if (strncmp ((const char *)cp, kt->name, kt->length) == 0
	    && !is_idchar (cp[kt->length])) {

	  /* If we are asked to return on next directive,
	     do so now.  */
	  if (any)
	    return;

	  switch (kt->type) {
	  case T_IF:
	  case T_IFDEF:
	  case T_IFNDEF:
	    temp = (IF_STACK_FRAME *) xcalloc (1, sizeof (IF_STACK_FRAME));
	    temp->next = if_stack;
	    if_stack = temp;
	    temp->lineno = ip->lineno;
	    temp->fname = ip->fname;
	    temp->type = kt->type;
	    break;
	  case T_ELSE:
	  case T_ENDIF:
	  case T_ELIF:
	    if (if_stack == instack[indepth].if_stack) {
	      error ("#%s not within a conditional", kt->name);
	      break;
	    }
	    else if (if_stack == save_if_stack)
	      return;		/* found what we came for */

	    if (kt->type != T_ENDIF) {
	      if (if_stack->type == T_ELSE)
		error ("#else or #elif after #else");
	      if_stack->type = kt->type;
	      break;
	    }

	    temp = if_stack;
	    if_stack = if_stack->next;
	    free (temp);
	    break;

	  default:
	    /* Anything else is ignored.  */
	    break;
	  }
	  break;
	}
      }
    }
  }
  ip->bufp = bp;
  /* after this returns, rescan will exit because ip->bufp
     now points to the end of the buffer.
     rescan is responsible for the error message also.  */
}

/*
 * handle a #else directive.  Do this by just continuing processing
 * without changing  if_stack ;  this is so that the error message
 * for missing #endif's etc. will point to the original #if.  It
 * is possible that something different would be better.
 */
static void
do_else (buf, limit, op)
     U_CHAR *buf ATTRIBUTE_UNUSED;
     U_CHAR *limit ATTRIBUTE_UNUSED;
     FILE_BUF *op;
{
  FILE_BUF *ip = &instack[indepth];

  if (if_stack == instack[indepth].if_stack) {
    error ("#else not within a conditional");
    return;
  } else {
    if (if_stack->type != T_IF && if_stack->type != T_ELIF) {
      error ("#else after #else");
      fprintf (stderr, " (matches line %d", if_stack->lineno);
      if (strcmp (if_stack->fname, ip->fname) != 0)
	fprintf (stderr, ", file %s", if_stack->fname);
      fprintf (stderr, ")\n");
    }
    if_stack->type = T_ELSE;
  }

  if (if_stack->if_succeeded)
    skip_if_group (ip, 0);
  else {
    ++if_stack->if_succeeded;	/* continue processing input */
    output_line_command (ip, op, 1, same_file);
  }
}

/*
 * unstack after #endif command
 */
static void
do_endif (buf, limit, op)
     U_CHAR *buf ATTRIBUTE_UNUSED;
     U_CHAR *limit ATTRIBUTE_UNUSED;
     FILE_BUF *op;
{
  if (if_stack == instack[indepth].if_stack)
    error ("unbalanced #endif");
  else {
    IF_STACK_FRAME *temp = if_stack;
    if_stack = if_stack->next;
    free (temp);
    output_line_command (&instack[indepth], op, 1, same_file);
  }
}

/*
 * Skip a comment, assuming the input ptr immediately follows the
 * initial slash-star.  Bump line counter as necessary.
 * (The canonical line counter is &ip->lineno).
 * Don't use this routine (or the next one) if bumping the line
 * counter is not sufficient to deal with newlines in the string.
 */
static U_CHAR *
skip_to_end_of_comment (ip, line_counter)
     FILE_BUF *ip;
     int *line_counter;		/* place to remember newlines, or NULL */
{
  U_CHAR *limit = ip->buf + ip->length;
  U_CHAR *bp = ip->bufp;
  FILE_BUF *op = &outbuf;	/* JF */
  int output = put_out_comments && !line_counter;

	/* JF this line_counter stuff is a crock to make sure the
	   comment is only put out once, no matter how many times
	   the comment is skipped.  It almost works */
  if (output) {
    *op->bufp++ = '/';
    *op->bufp++ = '*';
  }
  while (bp < limit) {
    if (output)
      *op->bufp++ = *bp;
    switch (*bp++) {
    case '/':
      if (warn_comments && bp < limit && *bp == '*')
	warning("`/*' within comment");
      break;
    case '\n':
      if (line_counter != NULL)
	++*line_counter;
      if (output)
	++op->lineno;
      break;
    case '*':
      if (*bp == '\\' && bp[1] == '\n')
	newline_fix (bp);
      if (*bp == '/') {
        if (output)
	  *op->bufp++ = '/';
	ip->bufp = ++bp;
	return bp;
      }
      break;
    }
  }
  ip->bufp = bp;
  return bp;
}

/*
 * Skip over a quoted string.  BP points to the opening quote.
 * Returns a pointer after the closing quote.  Don't go past LIMIT.
 * START_LINE is the line number of the starting point (but it need
 * not be valid if the starting point is inside a macro expansion).
 *
 * The input stack state is not changed.
 *
 * If COUNT_NEWLINES is nonzero, it points to an int to increment
 * for each newline passed.
 *
 * If BACKSLASH_NEWLINES_P is nonzero, store 1 thru it
 * if we pass a backslash-newline.
 *
 * If EOFP is nonzero, set *EOFP to 1 if the string is unterminated.
 */
static U_CHAR *
skip_quoted_string (bp, limit, start_line, count_newlines, backslash_newlines_p, eofp)
     const U_CHAR *bp;
     const U_CHAR *limit;
     int start_line;
     int *count_newlines;
     int *backslash_newlines_p;
     int *eofp;
{
  U_CHAR c, match;

  match = *bp++;
  while (1) {
    if (bp >= limit) {
      error_with_line (line_for_error (start_line),
		       "unterminated string or character constant");
      if (eofp)
	*eofp = 1;
      break;
    }
    c = *bp++;
    if (c == '\\') {
      while (*bp == '\\' && bp[1] == '\n') {
	if (backslash_newlines_p)
	  *backslash_newlines_p = 1;
	if (count_newlines)
	  ++*count_newlines;
	bp += 2;
      }
      if (*bp == '\n' && count_newlines) {
	if (backslash_newlines_p)
	  *backslash_newlines_p = 1;
	++*count_newlines;
      }
      bp++;
    } else if (c == '\n') {
      /* Unterminated strings and character constants are 'legal'.  */
      bp--;	/* Don't consume the newline. */
      if (eofp)
	*eofp = 1;
      break;
    } else if (c == match)
      break;
  }
  return (U_CHAR *) bp;
}

/*
 * write out a #line command, for instance, after an #include file.
 * If CONDITIONAL is nonzero, we can omit the #line if it would
 * appear to be a no-op, and we can output a few newlines instead
 * if we want to increase the line number by a small amount.
 * FILE_CHANGE says whether we are entering a file, leaving, or neither.
 */

static void
output_line_command (ip, op, conditional, file_change)
     FILE_BUF *ip, *op;
     int conditional;
     enum file_change_code file_change;
{
  int len;
  char line_cmd_buf[500];

  if (no_line_commands
      || ip->fname == NULL
      || no_output) {
    op->lineno = ip->lineno;
    return;
  }

  if (conditional) {
    if (ip->lineno == op->lineno)
      return;

    /* If the inherited line number is a little too small,
       output some newlines instead of a #line command.  */
    if (ip->lineno > op->lineno && ip->lineno < op->lineno + 8) {
      check_expand (op, 10);
      while (ip->lineno > op->lineno) {
	*op->bufp++ = '\n';
	op->lineno++;
      }
      return;
    }
  }

  sprintf (line_cmd_buf, "# %d \"%s\"", ip->lineno, ip->fname);
  if (file_change != same_file)
    strcat (line_cmd_buf, file_change == enter_file ? " 1" : " 2");
  if (system_include_depth > 0)
    strcat (line_cmd_buf, " 3");
  len = strlen (line_cmd_buf);
  line_cmd_buf[len++] = '\n';
  check_expand (op, len + 1);
  if (op->bufp > op->buf && op->bufp[-1] != '\n')
    *op->bufp++ = '\n';
  memcpy (op->bufp, line_cmd_buf, len);
  op->bufp += len;
  op->lineno = ip->lineno;
}


/* Expand a macro call.
   HP points to the symbol that is the macro being called.
   Put the result of expansion onto the input stack
   so that subsequent input by our caller will use it.

   If macro wants arguments, caller has already verified that
   an argument list follows; arguments come from the input stack.  */

static void
macroexpand (hp, op)
     HASHNODE *hp;
     FILE_BUF *op;
{
  int nargs;
  DEFINITION *defn = hp->value.defn;
  U_CHAR *xbuf;
  int xbuf_len;
  int start_line = instack[indepth].lineno;

  CHECK_DEPTH (return;);

  /* it might not actually be a macro.  */
  if (hp->type != T_MACRO) {
    special_symbol (hp, op);
    return;
  }

  nargs = defn->nargs;

  if (nargs >= 0) {
    int i;
    struct argdata *args;
    const char *parse_error = 0;

    args = (struct argdata *) alloca ((nargs + 1) * sizeof (struct argdata));

    for (i = 0; i < nargs; i++) {
      args[i].raw = args[i].expanded = (U_CHAR *) "";
      args[i].raw_length = args[i].expand_length
	= args[i].stringified_length = 0;
      args[i].free1 = args[i].free2 = 0;
    }

    /* Parse all the macro args that are supplied.  I counts them.
       The first NARGS args are stored in ARGS.
       The rest are discarded.  */
    i = 0;
    do {
      /* Discard the open-parenthesis or comma before the next arg.  */
      ++instack[indepth].bufp;
      parse_error
	= macarg ((i < nargs || (nargs == 0 && i == 0)) ? &args[i] : 0);
      if (parse_error)
	{
	  error_with_line (line_for_error (start_line), "%s", parse_error);
	  break;
	}
      i++;
    } while (*instack[indepth].bufp != ')');

    /* If we got one arg but it was just whitespace, call that 0 args.  */
    if (i == 1) {
      const U_CHAR *bp = args[0].raw;
      const U_CHAR *lim = bp + args[0].raw_length;
      while (bp != lim && is_space (*bp)) bp++;
      if (bp == lim)
	i = 0;
    }

    if (nargs == 0 && i > 0)
      error ("arguments given to macro `%s'", hp->name);
    else if (i < nargs) {
      /* traditional C allows foo() if foo wants one argument.  */
      if (nargs == 1 && i == 0)
	;
      else if (i == 0)
	error ("no args to macro `%s'", hp->name);
      else if (i == 1)
	error ("only 1 arg to macro `%s'", hp->name);
      else
	error ("only %d args to macro `%s'", i, hp->name);
    } else if (i > nargs)
      error ("too many (%d) args to macro `%s'", i, hp->name);

    /* Swallow the closeparen.  */
    ++instack[indepth].bufp;

    /* If macro wants zero args, we parsed the arglist for checking only.
       Read directly from the macro definition.  */
    if (nargs == 0) {
      xbuf = defn->expansion;
      xbuf_len = defn->length;
    } else {
      U_CHAR *exp = defn->expansion;
      int offset;	/* offset in expansion,
				   copied a piece at a time */
      int totlen;	/* total amount of exp buffer filled so far */

      struct reflist *ap;

      /* Macro really takes args.  Compute the expansion of this call.  */

      /* Compute length in characters of the macro's expansion.  */
      xbuf_len = defn->length;
      for (ap = defn->pattern; ap != NULL; ap = ap->next) {
	if (ap->stringify)
	  xbuf_len += args[ap->argno].stringified_length;
	else 
	  xbuf_len += args[ap->argno].raw_length;
      }

      xbuf = (U_CHAR *) xmalloc (xbuf_len + 1);

      /* Generate in XBUF the complete expansion
	 with arguments substituted in.
	 TOTLEN is the total size generated so far.
	 OFFSET is the index in the definition
	 of where we are copying from.  */
      offset = totlen = 0;
      for (ap = defn->pattern; ap != NULL; ap = ap->next) {
	struct argdata *arg = &args[ap->argno];

	for (i = 0; i < ap->nchars; i++)
	  xbuf[totlen++] = exp[offset++];

	if (ap->stringify != 0) {
	  int arglen = arg->raw_length;
	  int escaped = 0;
	  int in_string = 0;
	  int c;
	  i = 0;
	  while (i < arglen
		 && (c = arg->raw[i], is_space (c)))
	    i++;
	  while (i < arglen
		 && (c = arg->raw[arglen - 1], is_space (c)))
	    arglen--;
	  for (; i < arglen; i++) {
	    c = arg->raw[i];

	    /* Special markers Newline Space
	       generate nothing for a stringified argument.  */
	    if (c == '\n' && arg->raw[i+1] != '\n') {
	      i++;
	      continue;
	    }

	    /* Internal sequences of whitespace are replaced by one space
	       except within an string or char token.  */
	    if (! in_string
		&& (c == '\n' ? arg->raw[i+1] == '\n' : is_space (c))) {
	      while (1) {
		/* Note that Newline Space does occur within whitespace
		   sequences; consider it part of the sequence.  */
		if (c == '\n' && is_space (arg->raw[i+1]))
		  i += 2;
		else if (c != '\n' && is_space (c))
		  i++;
		else break;
		c = arg->raw[i];
	      }
	      i--;
	      c = ' ';
	    }

	    if (escaped)
	      escaped = 0;
	    else {
	      if (c == '\\')
		escaped = 1;
	      if (in_string) {
		if (c == in_string)
		  in_string = 0;
	      } else if (c == '\"' || c == '\'')
		in_string = c;
	    }

	    /* Escape these chars */
	    if (c == '\"' || (in_string && c == '\\'))
	      xbuf[totlen++] = '\\';
	    if (ISPRINT (c))
	      xbuf[totlen++] = c;
	    else {
	      sprintf ((char *) &xbuf[totlen], "\\%03o", (unsigned int) c);
	      totlen += 4;
	    }
	  }
	} else {
	  const U_CHAR *p1 = arg->raw;
	  const U_CHAR *l1 = p1 + arg->raw_length;

	  if (ap->raw_before) {
	    while (p1 != l1 && is_space (*p1)) p1++;
	    while (p1 != l1 && is_idchar (*p1))
	      xbuf[totlen++] = *p1++;
	    /* Delete any no-reexpansion marker that follows
	       an identifier at the beginning of the argument
	       if the argument is concatenated with what precedes it.  */
	    if (p1[0] == '\n' && p1[1] == '-')
	      p1 += 2;
	  }
	  if (ap->raw_after) {
	    /* Arg is concatenated after: delete trailing whitespace,
	       whitespace markers, and no-reexpansion markers.  */
	    while (p1 != l1) {
	      if (is_space (l1[-1])) l1--;
	      else if (l1[-1] == '-') {
		const U_CHAR *p2 = l1 - 1;
		/* If a `-' is preceded by an odd number of newlines then it
		   and the last newline are a no-reexpansion marker.  */
		while (p2 != p1 && p2[-1] == '\n') p2--;
		if ((l1 - 1 - p2) & 1) {
		  l1 -= 2;
		}
		else break;
	      }
	      else break;
	    }
	  }
	  memmove (xbuf + totlen, p1, l1 - p1);
	  totlen += l1 - p1;
	}

	if (totlen > xbuf_len)
	  abort ();
      }

      /* if there is anything left of the definition
	 after handling the arg list, copy that in too. */

      for (i = offset; i < defn->length; i++)
	xbuf[totlen++] = exp[i];

      xbuf[totlen] = 0;
      xbuf_len = totlen;

      for (i = 0; i < nargs; i++) {
	if (args[i].free1 != 0)
	  free (args[i].free1);
	if (args[i].free2 != 0)
	  free (args[i].free2);
      }
    }
  } else {
    xbuf = defn->expansion;
    xbuf_len = defn->length;
  }

  /* Now put the expansion on the input stack
     so our caller will commence reading from it.  */
  {
    FILE_BUF *ip2;

    ip2 = &instack[++indepth];

    ip2->fname = 0;
    ip2->lineno = 0;
    ip2->buf = xbuf;
    ip2->length = xbuf_len;
    ip2->bufp = xbuf;
    ip2->free_ptr = (nargs > 0) ? xbuf : 0;
    ip2->macro = hp;
    ip2->if_stack = if_stack;
  }
}

/*
 * Parse a macro argument and store the info on it into *ARGPTR.
 * Return nonzero to indicate a syntax error.
 */

static const char *
macarg (argptr)
     struct argdata *argptr;
{
  FILE_BUF *ip = &instack[indepth];
  int paren = 0;
  int newlines = 0;
  int comments = 0;

  /* Try to parse as much of the argument as exists at this
     input stack level.  */
  U_CHAR *bp = macarg1 (ip->bufp, ip->buf + ip->length,
			&paren, &newlines, &comments);

  /* If we find the end of the argument at this level,
     set up *ARGPTR to point at it in the input stack.  */
  if (!(ip->fname != 0 && (newlines != 0 || comments != 0))
      && bp != ip->buf + ip->length) {
    if (argptr != 0) {
      argptr->raw = ip->bufp;
      argptr->raw_length = bp - ip->bufp;
    }
    ip->bufp = bp;
  } else {
    /* This input stack level ends before the macro argument does.
       We must pop levels and keep parsing.
       Therefore, we must allocate a temporary buffer and copy
       the macro argument into it.  */
    int bufsize = bp - ip->bufp;
    int extra = newlines;
    U_CHAR *buffer = (U_CHAR *) xmalloc (bufsize + extra + 1);
    int final_start = 0;

    memcpy (buffer, ip->bufp, bufsize);
    ip->bufp = bp;
    ip->lineno += newlines;

    while (bp == ip->buf + ip->length) {
      if (instack[indepth].macro == 0) {
	free (buffer);
	return "unterminated macro call";
      }
      ip->macro->type = T_MACRO;
      if (ip->free_ptr)
	free (ip->free_ptr);
      ip = &instack[--indepth];
      newlines = 0;
      comments = 0;
      bp = macarg1 (ip->bufp, ip->buf + ip->length, &paren,
		    &newlines, &comments);
      final_start = bufsize;
      bufsize += bp - ip->bufp;
      extra += newlines;
      buffer = (U_CHAR *) xrealloc (buffer, bufsize + extra + 1);
      memcpy (buffer + bufsize - (bp - ip->bufp), ip->bufp, bp - ip->bufp);
      ip->bufp = bp;
      ip->lineno += newlines;
    }

    /* Now, if arg is actually wanted, record its raw form,
       discarding comments and duplicating newlines in whatever
       part of it did not come from a macro expansion.
       EXTRA space has been preallocated for duplicating the newlines.
       FINAL_START is the index of the start of that part.  */
    if (argptr != 0) {
      argptr->raw = buffer;
      argptr->raw_length = bufsize;
      argptr->free1 = buffer;
      argptr->newlines = newlines;
      argptr->comments = comments;
      if ((newlines || comments) && ip->fname != 0)
	argptr->raw_length
	  = final_start +
	    discard_comments (argptr->raw + final_start,
			      argptr->raw_length - final_start,
			      newlines);
      argptr->raw[argptr->raw_length] = 0;
      if (argptr->raw_length > bufsize + extra)
	abort ();
    }
  }

  /* If we are not discarding this argument,
     macroexpand it and compute its length as stringified.
     All this info goes into *ARGPTR.  */

  if (argptr != 0) {
    FILE_BUF obuf;
    const U_CHAR *buf, *lim;
    int totlen;

    obuf = expand_to_temp_buffer (argptr->raw,
				  argptr->raw + argptr->raw_length,
				  1);

    argptr->expanded = obuf.buf;
    argptr->expand_length = obuf.length;
    argptr->free2 = obuf.buf;

    buf = argptr->raw;
    lim = buf + argptr->raw_length;

    totlen = 0;
    while (buf != lim) {
      U_CHAR c = *buf++;
      totlen++;
      /* Internal sequences of whitespace are replaced by one space
	 in most cases, but not always.  So count all the whitespace
	 in case we need to keep it all.  */
      if (c == '\"' || c == '\\') /* escape these chars */
	totlen++;
      else if (!ISPRINT (c))
	totlen += 3;
    }
    argptr->stringified_length = totlen;
  }
  return 0;
}

/* Scan text from START (inclusive) up to LIMIT (exclusive),
   counting parens in *DEPTHPTR,
   and return if reach LIMIT
   or before a `)' that would make *DEPTHPTR negative
   or before a comma when *DEPTHPTR is zero.
   Single and double quotes are matched and termination
   is inhibited within them.  Comments also inhibit it.
   Value returned is pointer to stopping place.

   Increment *NEWLINES each time a newline is passed.
   Set *COMMENTS to 1 if a comment is seen.  */

static U_CHAR *
macarg1 (start, limit, depthptr, newlines, comments)
     U_CHAR *start;
     const U_CHAR *limit;
     int *depthptr, *newlines, *comments;
{
  U_CHAR *bp = start;

  while (bp < limit) {
    switch (*bp) {
    case '(':
      (*depthptr)++;
      break;
    case ')':
      if (--(*depthptr) < 0)
	return bp;
      break;
    case '\\':
      /* Traditionally, backslash makes following char not special.  */
      if (bp + 1 < limit)
	{
	  bp++;
	  /* But count source lines anyway.  */
	  if (*bp == '\n')
	    ++*newlines;
	}
      break;
    case '\n':
      ++*newlines;
      break;
    case '/':
      if (bp[1] == '\\' && bp[2] == '\n')
	newline_fix (bp + 1);
      if (bp[1] != '*' || bp + 1 >= limit)
	break;
      *comments = 1;
      bp += 2;
      while (bp + 1 < limit) {
	if (bp[0] == '*'
	    && bp[1] == '\\' && bp[2] == '\n')
	  newline_fix (bp + 1);
	if (bp[0] == '*' && bp[1] == '/')
	  break;
	if (*bp == '\n') ++*newlines;
	bp++;
      }
      bp += 1;
      break;
    case '\'':
    case '\"':
      {
	int quotec;
	for (quotec = *bp++; bp + 1 < limit && *bp != quotec; bp++) {
	  if (*bp == '\\') {
	    bp++;
	    if (*bp == '\n')
	      ++*newlines;
	    while (*bp == '\\' && bp[1] == '\n') {
	      bp += 2;
	    }
	  } else if (*bp == '\n') {
	    ++*newlines;
	    if (quotec == '\'')
	      break;
	  }
	}
      }
      break;
    case ',':
      if ((*depthptr) == 0)
	return bp;
      break;
    }
    bp++;
  }

  return bp;
}

/* Discard comments and duplicate newlines
   in the string of length LENGTH at START,
   except inside of string constants.
   The string is copied into itself with its beginning staying fixed.  

   NEWLINES is the number of newlines that must be duplicated.
   We assume that that much extra space is available past the end
   of the string.  */

static int
discard_comments (start, length, newlines)
     U_CHAR *start;
     int length;
     int newlines;
{
  U_CHAR *ibp;
  U_CHAR *obp;
  const U_CHAR *limit;
  int c;

  /* If we have newlines to duplicate, copy everything
     that many characters up.  Then, in the second part,
     we will have room to insert the newlines
     while copying down.
     NEWLINES may actually be too large, because it counts
     newlines in string constants, and we don't duplicate those.
     But that does no harm.  */
  if (newlines > 0) {
    ibp = start + length;
    obp = ibp + newlines;
    limit = start;
    while (limit != ibp)
      *--obp = *--ibp;
  }

  ibp = start + newlines;
  limit = start + length + newlines;
  obp = start;

  while (ibp < limit) {
    *obp++ = c = *ibp++;
    switch (c) {
    case '\n':
      /* Duplicate the newline.  */
      *obp++ = '\n';
      break;

    case '\\':
      if (*ibp == '\n') {
	obp--;
	ibp++;
      }
      break;

    case '/':
      if (*ibp == '\\' && ibp[1] == '\n')
	newline_fix (ibp);
      /* Delete any comment.  */
      if (ibp[0] != '*' || ibp + 1 >= limit)
	break;
      obp--;
      ibp++;
      while (ibp + 1 < limit) {
	if (ibp[0] == '*'
	    && ibp[1] == '\\' && ibp[2] == '\n')
	  newline_fix (ibp + 1);
	if (ibp[0] == '*' && ibp[1] == '/')
	  break;
	ibp++;
      }
      ibp += 2;
      break;

    case '\'':
    case '\"':
      /* Notice and skip strings, so that we don't
	 think that comments start inside them,
	 and so we don't duplicate newlines in them.  */
      {
	int quotec = c;
	while (ibp < limit) {
	  *obp++ = c = *ibp++;
	  if (c == quotec)
	    break;
	  if (c == '\n' && quotec == '\'')
	    break;
	  if (c == '\\' && ibp < limit) {
	    while (*ibp == '\\' && ibp[1] == '\n')
	      ibp += 2;
	    *obp++ = *ibp++;
	  }
	}
      }
      break;
    }
  }

  return obp - start;
}


/* Core error handling routine.  */
static void
v_message (mtype, line, msgid, ap)
     enum msgtype mtype;
     int line;
     const char *msgid;
     va_list ap;
{
  const char *fname = 0;
  int i;

  if (mtype == MT_WARNING && inhibit_warnings)
    return;

  for (i = indepth; i >= 0; i--)
    if (instack[i].fname != NULL) {
      if (line == 0)
	line = instack[i].lineno;
      fname = instack[i].fname;
      break;
    }

  if (fname)
    fprintf (stderr, "%s:%d: ", fname, line);
  else
    fprintf (stderr, "%s: ", progname);

  if (mtype == MT_WARNING)
    fputs (_("warning: "), stderr);

  vfprintf (stderr, _(msgid), ap);
  putc ('\n', stderr);

  if (mtype == MT_ERROR)
    errors++;
}

/*
 * error - print error message and increment count of errors.
 */
void
error VPARAMS ((const char *msgid, ...))
{
  VA_OPEN(ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  v_message (MT_ERROR, 0, msgid, ap);
  VA_CLOSE (ap);
}

void
error_with_line VPARAMS ((int line, const char *msgid, ...))
{
  VA_OPEN(ap, msgid);
  VA_FIXEDARG (ap, int, line);
  VA_FIXEDARG (ap, const char *, msgid);

  v_message (MT_ERROR, line, msgid, ap);
  VA_CLOSE (ap);
}

/* Error including a message from `errno'.  */
void
error_from_errno (name)
     const char *name;
{
  error ("%s: %s", name, strerror (errno));
}

/* Print error message but don't count it.  */
void
warning VPARAMS ((const char *msgid, ...))
{
  VA_OPEN(ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  v_message (MT_WARNING, 0, msgid, ap);
  VA_CLOSE (ap);
}

void
fatal VPARAMS ((const char *msgid, ...))
{
  VA_OPEN(ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  v_message (MT_FATAL, 0, msgid, ap);
  VA_CLOSE (ap);
  exit (FATAL_EXIT_CODE);
}

/* More 'friendly' abort that prints the location at which we died.  */
void
fancy_abort (line, func)
     int line;
     const char *func;
{
  fatal ("internal error in %s, at tradcpp.c:%d\n\
Please submit a full bug report.\n\
See %s for instructions.", func, line, GCCBUGURL);
}

void
perror_with_name (name)
     const char *name;
{
  fprintf (stderr, "%s: %s: %s\n", progname, name, strerror (errno));
  errors++;
}

void
pfatal_with_name (name)
     const char *name;
{
  perror_with_name (name);
  exit (FATAL_EXIT_CODE);
}

/* Return the line at which an error occurred.
   The error is not necessarily associated with the current spot
   in the input stack, so LINE says where.  LINE will have been
   copied from ip->lineno for the current input level.
   If the current level is for a file, we return LINE.
   But if the current level is not for a file, LINE is meaningless.
   In that case, we return the lineno of the innermost file.  */
static int
line_for_error (line)
     int line;
{
  int i;
  int line1 = line;

  for (i = indepth; i >= 0; ) {
    if (instack[i].fname != 0)
      return line1;
    i--;
    if (i < 0)
      return 0;
    line1 = instack[i].lineno;
  }
  return 0;
}

/*
 * If OBUF doesn't have NEEDED bytes after OPTR, make it bigger.
 *
 * As things stand, nothing is ever placed in the output buffer to be
 * removed again except when it's KNOWN to be part of an identifier,
 * so flushing and moving down everything left, instead of expanding,
 * should work ok.
 */

static void
grow_outbuf (obuf, needed)
     FILE_BUF *obuf;
     int needed;
{
  U_CHAR *p;
  int minsize;

  if (obuf->length - (obuf->bufp - obuf->buf) > needed)
    return;

  /* Make it at least twice as big as it is now.  */
  obuf->length *= 2;
  /* Make it have at least 150% of the free space we will need.  */
  minsize = (3 * needed) / 2 + (obuf->bufp - obuf->buf);
  if (minsize > obuf->length)
    obuf->length = minsize;

  p = (U_CHAR *) xrealloc (obuf->buf, obuf->length);
  obuf->bufp = p + (obuf->bufp - obuf->buf);
  obuf->buf = p;
}

/* Symbol table for macro names and special symbols */

/*
 * install a name in the main hash table, even if it is already there.
 *   name stops with first non alphanumeric, except leading '#'.
 * caller must check against redefinition if that is desired.
 * delete_macro () removes things installed by install () in fifo order.
 * this is important because of the `defined' special symbol used
 * in #if, and also if pushdef/popdef directives are ever implemented.
 *
 * If LEN is >= 0, it is the length of the name.
 * Otherwise, compute the length by scanning the entire name.
 *
 * If HASH is >= 0, it is the precomputed hash code.
 * Otherwise, compute the hash code.
 *
 * caller must set the value, if any is desired.
 */
static HASHNODE *
install (name, len, type, hash)
     const U_CHAR *name;
     int len;
     enum node_type type;
     int hash;
        /* watch out here if sizeof (U_CHAR *) != sizeof (int) */
{
  HASHNODE *hp;
  int bucket;
  const U_CHAR *p;
  U_CHAR *q;

  if (len < 0) {
    p = name;
    while (is_idchar (*p))
      p++;
    len = p - name;
  }

  if (hash < 0)
    hash = hashf (name, len, HASHSIZE);

  hp = (HASHNODE *) xmalloc (sizeof (HASHNODE) + len + 1);
  bucket = hash;
  hp->bucket_hdr = &hashtab[bucket];
  hp->next = hashtab[bucket];
  hashtab[bucket] = hp;
  hp->prev = NULL;
  if (hp->next != NULL)
    hp->next->prev = hp;
  hp->type = type;
  hp->length = len;
  hp->name = q = ((U_CHAR *) hp) + sizeof (HASHNODE);
  memcpy (q, name, len);
  q[len] = 0;
  return hp;
}

/*
 * find the most recent hash node for name name (ending with first
 * non-identifier char) installed by install
 *
 * If LEN is >= 0, it is the length of the name.
 * Otherwise, compute the length by scanning the entire name.
 *
 * If HASH is >= 0, it is the precomputed hash code.
 * Otherwise, compute the hash code.
 */
HASHNODE *
lookup (name, len, hash)
     const U_CHAR *name;
     int len;
     int hash;
{
  const U_CHAR *bp;
  HASHNODE *bucket;

  if (len < 0) {
    for (bp = name; is_idchar (*bp); bp++) ;
    len = bp - name;
  }

  if (hash < 0)
    hash = hashf (name, len, HASHSIZE);

  bucket = hashtab[hash];
  while (bucket) {
    if (bucket->length == len
	&& strncmp ((const char *)bucket->name, (const char *)name, len) == 0)
      return bucket;
    bucket = bucket->next;
  }
  return NULL;
}

/*
 * Delete a hash node.  Some weirdness to free junk from macros.
 * More such weirdness will have to be added if you define more hash
 * types that need it.
 */

/* Note that the DEFINITION of a macro is removed from the hash table
   but its storage is not freed.  This would be a storage leak
   except that it is not reasonable to keep undefining and redefining
   large numbers of macros many times.
   In any case, this is necessary, because a macro can be #undef'd
   in the middle of reading the arguments to a call to it.
   If #undef freed the DEFINITION, that would crash.  */
static void
delete_macro (hp)
     HASHNODE *hp;
{

  if (hp->prev != NULL)
    hp->prev->next = hp->next;
  if (hp->next != NULL)
    hp->next->prev = hp->prev;

  /* make sure that the bucket chain header that
     the deleted guy was on points to the right thing afterwards. */
  if (hp == *hp->bucket_hdr)
    *hp->bucket_hdr = hp->next;

  free (hp);
}

/*
 * return hash function on name.  must be compatible with the one
 * computed a step at a time, elsewhere
 */
static int
hashf (name, len, hashsize)
     const U_CHAR *name;
     int len;
     int hashsize;
{
  int r = 0;

  while (len--)
    r = HASHSTEP (r, *name++);

  return MAKE_POS (r) % hashsize;
}

/* Dump all macro definitions as #defines to stdout.  */

static void
dump_all_macros ()
{
  int bucket;

  for (bucket = 0; bucket < HASHSIZE; bucket++) {
    HASHNODE *hp;

    for (hp = hashtab[bucket]; hp; hp= hp->next) {
      if (hp->type == T_MACRO) {
	DEFINITION *defn = hp->value.defn;
	struct reflist *ap;
	int offset;
	int concat;


	/* Print the definition of the macro HP.  */

	printf ("#define %s", hp->name);
	if (defn->nargs >= 0) {
	  int i;

	  printf ("(");
	  for (i = 0; i < defn->nargs; i++) {
	    dump_arg_n (defn, i);
	    if (i + 1 < defn->nargs)
	      printf (", ");
	  }
	  printf (")");
	}

	printf (" ");

	offset = 0;
	concat = 0;
	for (ap = defn->pattern; ap != NULL; ap = ap->next) {
	  dump_defn_1 (defn->expansion, offset, ap->nchars);
	  if (ap->nchars != 0)
	    concat = 0;
	  offset += ap->nchars;
	  if (ap->stringify)
	    printf (" #");
	  if (ap->raw_before && !concat)
	    printf (" ## ");
	  concat = 0;
	  dump_arg_n (defn, ap->argno);
	  if (ap->raw_after) {
	    printf (" ## ");
	    concat = 1;
	  }
	}
	dump_defn_1 (defn->expansion, offset, defn->length - offset);
	printf ("\n");
      }
    }
  }
}

/* Output to stdout a substring of a macro definition.
   BASE is the beginning of the definition.
   Output characters START thru LENGTH.
   Discard newlines outside of strings, thus
   converting funny-space markers to ordinary spaces.  */
static void
dump_defn_1 (base, start, length)
     const U_CHAR *base;
     int start;
     int length;
{
  const U_CHAR *p = base + start;
  const U_CHAR *limit = base + start + length;

  while (p < limit) {
    if (*p != '\n')
      putchar (*p);
    else if (*p == '\"' || *p =='\'') {
      const U_CHAR *p1 = skip_quoted_string (p, limit, 0, 0, 0, 0);
      fwrite (p, p1 - p, 1, stdout);
      p = p1 - 1;
    }
    p++;
  }
}

/* Print the name of argument number ARGNUM of macro definition DEFN.
   Recall that DEFN->argnames contains all the arg names
   concatenated in reverse order with comma-space in between.  */
static void
dump_arg_n (defn, argnum)
     DEFINITION *defn;
     int argnum;
{
  const U_CHAR *p = defn->argnames;
  while (argnum + 1 < defn->nargs) {
    p = (const U_CHAR *) strchr ((const char *)p, ' ') + 1;
    argnum++;
  }

  while (*p && *p != ',') {
    putchar (*p);
    p++;
  }
}

/* Initialize the built-in macros.  */
#define DSC(x) U x, sizeof x - 1
#define install_spec(name, type) \
 install(DSC(name), type, -1);
#define install_value(name, val) \
 hp = install(DSC(name), T_CONST, -1); hp->value.cpval = val;
static void
initialize_builtins ()
{
  HASHNODE *hp;

  install_spec ("__BASE_FILE__",     T_BASE_FILE);
  install_spec ("__DATE__",          T_DATE);
  install_spec ("__FILE__",          T_FILE);
  install_spec ("__TIME__",          T_TIME);
  install_spec ("__VERSION__",       T_VERSION);
  install_spec ("__INCLUDE_LEVEL__", T_INCLUDE_LEVEL);
  install_spec ("__LINE__",          T_SPECLINE);

#ifndef NO_BUILTIN_SIZE_TYPE
  install_value ("__SIZE_TYPE__",         SIZE_TYPE);
#endif
#ifndef NO_BUILTIN_PTRDIFF_TYPE
  install_value ("__PTRDIFF_TYPE__",      PTRDIFF_TYPE);
#endif
#ifndef NO_BUILTIN_WCHAR_TYPE
  install_value ("__WCHAR_TYPE__",        WCHAR_TYPE);
#endif
#ifndef NO_BUILTIN_WINT_TYPE
  install_value ("__WINT_TYPE__",         WINT_TYPE);
#endif
  install_value ("__REGISTER_PREFIX__",   REGISTER_PREFIX);
  install_value ("__USER_LABEL_PREFIX__", user_label_prefix);

  if (flag_signed_char == 0)
    install_value ("__CHAR_UNSIGNED__", "1");
}
#undef DSC
#undef install_spec
#undef install_value

/* Common handler of command line directives -U, -D and -A.  */
static void
run_directive (str, len, type)
     const char *str;
     size_t len;
     enum node_type type;
{
  const struct directive *kt;
  FILE_BUF *ip = &instack[++indepth];
  ip->fname = "*command line*";

  ip->buf = ip->bufp = (U_CHAR *) str;
  ip->length = len;
  ip->lineno = 1;
  ip->macro = 0;
  ip->free_ptr = 0;
  ip->if_stack = if_stack;

  for (kt = directive_table; kt->type != type; kt++)
    ;

  (*kt->func) ((U_CHAR *) str, (U_CHAR *) str + len, NULL);
  --indepth;
}

/* Handle the -D option.  If STR is just an identifier, define it with
 * value 1.  If STR has anything after the identifier, then it should
 * be identifier-space-definition.  */
static void
make_definition (str)
     const char *str;
{
  char *buf, *p;
  size_t count;

  /* Copy the entire option so we can modify it. 
     Change the first "=" in the string to a space.  If there is none,
     tack " 1" on the end.  */

  /* Length including the null.  */  
  count = strlen (str);
  buf = (char *) alloca (count + 2);
  memcpy (buf, str, count);

  p = strchr (str, '=');
  if (p)
    buf[p - str] = ' ';
  else
    {
      buf[count++] = ' ';
      buf[count++] = '1';
    }

  run_directive (buf, count, T_DEFINE);
}

/* Handle the -U option.  */
static void
make_undef (str)
     const char *str;
{
  run_directive (str, strlen (str), T_UNDEF);
}

/* Handles the #assert (-A) and #unassert (-A-) command line options.  */
static void
make_assertion (str)
     const char *str;
{
  enum node_type type = T_ASSERT;
  size_t count;
  const char *p;

  if (*str == '-')
    {
      str++;
      type = T_UNASSERT;
    }
  
  count = strlen (str);
  p = strchr (str, '=');
  if (p)
    {
      /* Copy the entire option so we can modify it.  Change the first
	 "=" in the string to a '(', and tack a ')' on the end.  */
      char *buf = (char *) alloca (count + 1);

      memcpy (buf, str, count);
      buf[p - str] = '(';
      buf[count++] = ')';
      str = buf;
    }

  run_directive (str, count, type);
}

/* Get the file-mode and data size of the file open on FD
   and store them in *MODE_POINTER and *SIZE_POINTER.  */

static int
file_size_and_mode (fd, mode_pointer, size_pointer)
     int fd;
     int *mode_pointer;
     long *size_pointer;
{
  struct stat sbuf;

  if (fstat (fd, &sbuf) < 0) return -1;
  if (mode_pointer) *mode_pointer = sbuf.st_mode;
  if (size_pointer) *size_pointer = sbuf.st_size;
  return 0;
}
