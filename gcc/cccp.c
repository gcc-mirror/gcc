/* C Compatible Compiler Preprocessor (CCCP)
   Copyright (C) 1986, 87, 89, 92-99, 2000 Free Software Foundation, Inc.
   Written by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

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
Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */

#include "config.h"

#include "system.h"
#include <signal.h>

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

typedef unsigned char U_CHAR;

#include "pcp.h"
#include "intl.h"
#include "prefix.h"

#ifdef MULTIBYTE_CHARS
#include "mbchar.h"
#include <locale.h>
#endif /* MULTIBYTE_CHARS */

#ifndef GET_ENV_PATH_LIST
#define GET_ENV_PATH_LIST(VAR,NAME)	do { (VAR) = getenv (NAME); } while (0)
#endif

#ifndef STANDARD_INCLUDE_DIR
# define STANDARD_INCLUDE_DIR "/usr/include"
#endif

/* By default, the suffix for object files is ".o".  */
#ifdef OBJECT_SUFFIX
# define HAVE_OBJECT_SUFFIX
#else
# define OBJECT_SUFFIX ".o"
#endif

/* VMS-specific definitions */
#ifdef VMS
#include <descrip.h>
#include <ssdef.h>
#include <syidef.h>
#define open(fname,mode,prot)	VMS_open (fname,mode,prot)
#define fopen(fname,mode)	VMS_fopen (fname,mode)
#define freopen(fname,mode,ofile) VMS_freopen (fname,mode,ofile)
#define fstat(fd,stbuf)		VMS_fstat (fd,stbuf)
#define fwrite(ptr,size,nitems,stream) VMS_fwrite (ptr,size,nitems,stream)
static int VMS_fstat (), VMS_stat ();
static int VMS_open ();
static FILE *VMS_fopen ();
static FILE *VMS_freopen ();
static size_t VMS_fwrite ();
static void hack_vms_include_specification ();
#define INO_T_EQ(a, b) (!bcmp((char *) &(a), (char *) &(b), sizeof (a)))
#define INO_T_HASH(a) 0
#define INCLUDE_LEN_FUDGE 12	/* leave room for VMS syntax conversion */
#endif /* VMS */

/* Windows does not natively support inodes, and neither does MSDOS. 
   Cygwin's emulation can generate non-unique inodes, so don't use it. */
#if (defined (_WIN32) && ! defined (_UWIN)) \
  || defined (__MSDOS__)
#define INO_T_EQ(a, b) 0
#endif

#ifndef INO_T_EQ
#define INO_T_EQ(a, b) ((a) == (b))
#endif

#ifndef INO_T_HASH
#define INO_T_HASH(a) (a)
#endif

#ifndef INCLUDE_LEN_FUDGE
#define INCLUDE_LEN_FUDGE 0
#endif

/* External declarations.  */

extern char *version_string;
HOST_WIDEST_INT parse_escape PARAMS ((char **, HOST_WIDEST_INT));
HOST_WIDEST_INT parse_c_expression PARAMS ((char *, int));

/* Name under which this program was invoked.  */

static const char *progname;

/* Nonzero means use extra default include directories for C++.  */

static int cplusplus;

/* Nonzero means handle cplusplus style comments */

static int cplusplus_comments;

/* Nonzero means handle #import, for objective C.  */

static int objc;

/* Nonzero means this is an assembly file, and allow
   unknown directives, which could be comments.  */

static int lang_asm;

/* Current maximum length of directory names in the search path
   for include files.  (Altered as we get more of them.)  */

static int max_include_len;

/* Nonzero means turn NOTREACHED into #pragma NOTREACHED etc */

static int for_lint = 0;

/* Nonzero means copy comments into the output file.  */

static int put_out_comments = 0;

/* Nonzero means don't process the ANSI trigraph sequences.  */

static int no_trigraphs = 0;

/* Nonzero means print the names of included files rather than
   the preprocessed output.  1 means just the #include "...",
   2 means #include <...> as well.  */

static int print_deps = 0;

/* Nonzero if missing .h files in -M output are assumed to be generated
   files and not errors.  */

static int print_deps_missing_files = 0;

/* Nonzero means print names of header files (-H).  */

static int print_include_names = 0;

/* Nonzero means don't output line number information.  */

static int no_line_directives;

/* Nonzero means output the text in failing conditionals,
   inside #failed ... #endfailed.  */

static int output_conditionals;

/* dump_only means inhibit output of the preprocessed text
             and instead output the definitions of all user-defined
             macros in a form suitable for use as input to cccp.
   dump_names means pass #define and the macro name through to output.
   dump_definitions means pass the whole definition (plus #define) through
*/

static enum {dump_none, dump_only, dump_names, dump_definitions}
     dump_macros = dump_none;

/* Nonzero means pass all #define and #undef directives which we actually
   process through to the output stream.  This feature is used primarily
   to allow cc1 to record the #defines and #undefs for the sake of
   debuggers which understand about preprocessor macros, but it may
   also be useful with -E to figure out how symbols are defined, and
   where they are defined.  */
static int debug_output = 0;

/* Nonzero means pass #include lines through to the output,
   even if they are ifdefed out.  */
static int dump_includes;

/* Nonzero indicates special processing used by the pcp program.  The
   special effects of this mode are: 
     
     Inhibit all macro expansion, except those inside #if directives.

     Process #define directives normally, and output their contents 
     to the output file.

     Output preconditions to pcp_outfile indicating all the relevant
     preconditions for use of this file in a later cpp run.
*/
static FILE *pcp_outfile;

/* Nonzero means we are inside an IF during a -pcp run.  In this mode
   macro expansion is done, and preconditions are output for all macro
   uses requiring them.  */
static int pcp_inside_if;

/* Nonzero means never to include precompiled files.
   This is 1 since there's no way now to make precompiled files,
   so it's not worth testing for them.  */
static int no_precomp = 1;

/* Nonzero means give all the error messages the ANSI standard requires.  */

int pedantic;

/* Nonzero means try to make failure to fit ANSI C an error.  */

static int pedantic_errors;

/* Nonzero means don't print warning messages.  -w.  */

static int inhibit_warnings = 0;

/* Nonzero means warn if slash-star appears in a slash-star comment,
   or if newline-backslash appears in a slash-slash comment.  */

static int warn_comments;

/* Nonzero means warn if a macro argument is (or would be)
   stringified with -traditional.  */

static int warn_stringify;

/* Nonzero means warn if there are any trigraphs.  */

static int warn_trigraphs;

/* Nonzero means warn if undefined identifiers are evaluated in an #if.  */

static int warn_undef;

/* Nonzero means warn if we find white space where it doesn't belong.  */

static int warn_white_space;

/* Nonzero means warn if #import is used.  */

static int warn_import = 1;

/* Nonzero means turn warnings into errors.  */

static int warnings_are_errors;

/* Nonzero means try to imitate old fashioned non-ANSI preprocessor.  */

int traditional;

/* Nonzero for the 1989 C Standard, including corrigenda and amendments.  */

int c89;

/* Nonzero for the 199x C Standard.  */

int c9x;

/* Nonzero causes output not to be done,
   but directives such as #define that have side effects
   are still obeyed.  */

static int no_output;

/* Nonzero means we should look for header.gcc files that remap file names.  */
static int remap;

/* Nonzero means this file was included with a -imacros or -include
   command line and should not be recorded as an include file.  */

static int no_record_file;

/* Nonzero means that we have finished processing the command line options.
   This flag is used to decide whether or not to issue certain errors
   and/or warnings.  */

static int done_initializing = 0;

/* Line where a newline was first seen in a string constant.  */

static int multiline_string_line = 0;

/* I/O buffer structure.
   The `fname' field is nonzero for source files and #include files
   and for the dummy text used for -D and -U.
   It is zero for rescanning results of macro expansion
   and for expanding macro arguments.  */
#define INPUT_STACK_MAX 400
static struct file_buf {
  const char *fname;
  /* Filename specified with #line directive.  */
  const char *nominal_fname;
  /* The length of nominal_fname, which may contain embedded NULs.  */
  size_t nominal_fname_len;
  /* Include file description.  */
  struct include_file *inc;
  /* Record where in the search path this file was found.
     For #include_next.  */
  struct file_name_list *dir;
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
  /* True if this is a system header file; see is_system_include.  */
  char system_header_p;
} instack[INPUT_STACK_MAX];

static int last_error_tick;	   /* Incremented each time we print it.  */
static int input_file_stack_tick;  /* Incremented when the status changes.  */

/* Current nesting level of input sources.
   `instack[indepth]' is the level currently being read.  */
static int indepth = -1;
#define CHECK_DEPTH(code) \
  if (indepth >= (INPUT_STACK_MAX - 1))					\
    {									\
      error_with_line (line_for_error (instack[indepth].lineno),	\
		       "macro or `#include' recursion too deep");	\
      code;								\
    }

/* Current depth in #include directives that use <...>.  */
static int system_include_depth = 0;

typedef struct file_buf FILE_BUF;

/* The output buffer.  Its LENGTH field is the amount of room allocated
   for the buffer, not the number of chars actually present.  To get
   that, subtract outbuf.buf from outbuf.bufp.  */

#define OUTBUF_SIZE 10	/* initial size of output buffer */
static FILE_BUF outbuf;

/* Grow output buffer OBUF points at
   so it can hold at least NEEDED more chars.  */

#define check_expand(OBUF, NEEDED)  \
  (((OBUF)->length - ((OBUF)->bufp - (OBUF)->buf) <= (NEEDED))   \
   ? grow_outbuf ((OBUF), (NEEDED)) : 0)

struct file_name_list
  {
    struct file_name_list *next;
    /* If the following is 1, it is a C-language system include
       directory.  */
    int c_system_include_path;
    /* Mapping of file names for this directory.  */
    struct file_name_map *name_map;
    /* Non-zero if name_map is valid.  */
    int got_name_map;
    /* The include directory status.  */
    struct stat st;
    /* The include prefix: "" denotes the working directory,
       otherwise fname must end in '/'.
       The actual size is dynamically allocated.  */
    char fname[1];
  };

/* #include "file" looks in source file dir, then stack.  */
/* #include <file> just looks in the stack.  */
/* -I directories are added to the end, then the defaults are added.  */
/* The */
static struct default_include {
  const char *fname;		/* The name of the directory.  */
  const char *component;	/* The component containing the directory */
  int cplusplus;		/* Only look here if we're compiling C++.  */
  int cxx_aware;		/* Includes in this directory don't need to
				   be wrapped in extern "C" when compiling
				   C++.  */
  int included;                 /* Set if the directory is acceptable.  */
} include_defaults_array[]
#ifdef INCLUDE_DEFAULTS
  = INCLUDE_DEFAULTS;
#else
  = {
    /* Pick up GNU C++ specific include files.  */
    { GPLUSPLUS_INCLUDE_DIR, "G++", 1, 1, 0 },
#ifdef CROSS_COMPILE
    /* This is the dir for fixincludes.  Put it just before
       the files that we fix.  */
    { GCC_INCLUDE_DIR, "GCC", 0, 0, 0 },
    /* For cross-compilation, this dir name is generated
       automatically in Makefile.in.  */
    { CROSS_INCLUDE_DIR, "GCC", 0, 0, 0 },
#ifdef TOOL_INCLUDE_DIR
    /* This is another place that the target system's headers might be.  */
    { TOOL_INCLUDE_DIR, "BINUTILS", 0, 0, 0 },
#endif
#else /* not CROSS_COMPILE */
#ifdef LOCAL_INCLUDE_DIR
    /* This should be /usr/local/include and should come before
       the fixincludes-fixed header files.  */
    { LOCAL_INCLUDE_DIR, 0, 0, 1, 0 },
#endif
#ifdef TOOL_INCLUDE_DIR
    /* This is here ahead of GCC_INCLUDE_DIR because assert.h goes here.
       Likewise, behind LOCAL_INCLUDE_DIR, where glibc puts its assert.h.  */
    { TOOL_INCLUDE_DIR, "BINUTILS", 0, 0, 0 },
#endif
    /* This is the dir for fixincludes.  Put it just before
       the files that we fix.  */
    { GCC_INCLUDE_DIR, "GCC", 0, 0, 0 },
    /* Some systems have an extra dir of include files.  */
#ifdef SYSTEM_INCLUDE_DIR
    { SYSTEM_INCLUDE_DIR, 0, 0, 0, 0 },
#endif
#ifndef STANDARD_INCLUDE_COMPONENT
#define STANDARD_INCLUDE_COMPONENT 0
#endif
    { STANDARD_INCLUDE_DIR, STANDARD_INCLUDE_COMPONENT, 0, 0, 0 },
#endif /* not CROSS_COMPILE */
    { 0, 0, 0, 0, 0 }
    };
#endif /* no INCLUDE_DEFAULTS */

/* The code looks at the defaults through this pointer, rather than through
   the constant structure above.  This pointer gets changed if an environment
   variable specifies other defaults.  */
static struct default_include *include_defaults = include_defaults_array;

static struct file_name_list *include = 0;	/* First dir to search */
	/* First dir to search for <file> */
/* This is the first element to use for #include <...>.
   If it is 0, use the entire chain for such includes.  */
static struct file_name_list *first_bracket_include = 0;
/* This is the first element in the chain that corresponds to
   a directory of system header files.  */
static struct file_name_list *first_system_include = 0;
static struct file_name_list *last_include = 0;	/* Last in chain */

/* Chain of include directories to put at the end of the other chain.  */
static struct file_name_list *after_include = 0;
static struct file_name_list *last_after_include = 0;	/* Last in chain */

/* Chain to put at the start of the system include files.  */
static struct file_name_list *before_system = 0;
static struct file_name_list *last_before_system = 0;	/* Last in chain */

/* Directory prefix that should replace `/usr' in the standard
   include file directories.  */
static char *include_prefix;

/* Maintain and search list of included files.  */

struct include_file {
  struct include_file *next; /* for include_hashtab */
  struct include_file *next_ino; /* for include_ino_hashtab */
  char *fname;
  /* If the following is the empty string, it means #pragma once
     was seen in this include file, or #import was applied to the file.
     Otherwise, if it is nonzero, it is a macro name.
     Don't include the file again if that macro is defined.  */
  const U_CHAR *control_macro;
  /* Nonzero if the dependency on this include file has been output.  */
  int deps_output;
  struct stat st;
};

/* Hash tables of files already included with #include or #import.
   include_hashtab is by full name; include_ino_hashtab is by inode number.  */

#define INCLUDE_HASHSIZE 61
static struct include_file *include_hashtab[INCLUDE_HASHSIZE];
static struct include_file *include_ino_hashtab[INCLUDE_HASHSIZE];

/* Global list of strings read in from precompiled files.  This list
   is kept in the order the strings are read in, with new strings being
   added at the end through stringlist_tailp.  We use this list to output
   the strings at the end of the run. 
*/
static STRINGDEF *stringlist;
static STRINGDEF **stringlist_tailp = &stringlist;


/* Structure returned by create_definition */
typedef struct macrodef MACRODEF;
struct macrodef
{
  struct definition *defn;
  const U_CHAR *symnam;
  int symlen;
};

enum sharp_token_type {
  NO_SHARP_TOKEN = 0,		/* token not present */

  SHARP_TOKEN = '#',		/* token spelled with # only */
  WHITE_SHARP_TOKEN,		/* token spelled with # and white space */

  PERCENT_COLON_TOKEN = '%',	/* token spelled with %: only */
  WHITE_PERCENT_COLON_TOKEN	/* token spelled with %: and white space */
};

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
   where (x, y) means (nchars, argno).  */

typedef struct definition DEFINITION;
struct definition {
  int nargs;
  int length;			/* length of expansion string */
  int predefined;		/* True if the macro was builtin or */
				/* came from the command line */
  U_CHAR *expansion;
  int line;			/* Line number of definition */
  const char *file;		/* File of definition */
  size_t file_len;		/* Length of file (which can contain NULs) */
  char rest_args;		/* Nonzero if last arg. absorbs the rest */
  struct reflist {
    struct reflist *next;

    enum sharp_token_type stringify;	/* set if a # operator before arg */
    enum sharp_token_type raw_before;	/* set if a ## operator before arg */
    enum sharp_token_type raw_after;	/* set if a ## operator after arg */

    char rest_args;		/* Nonzero if this arg. absorbs the rest */
    int nchars;			/* Number of literal chars to copy before
				   this arg occurrence.  */
    int argno;			/* Number of arg to substitute (origin-0) */
  } *pattern;
  union {
    /* Names of macro args, concatenated in reverse order
       with comma-space between them.
       The only use of this is that we warn on redefinition
       if this differs between the old and new definitions.  */
    U_CHAR *argnames;
  } args;
};

/* different kinds of things that can appear in the value field
   of a hash node.  Actually, this may be useless now.  */
union hashval {
  const char *cpval;
  DEFINITION *defn;
  KEYDEF *keydef;
};

/*
 * special extension string that can be added to the last macro argument to 
 * allow it to absorb the "rest" of the arguments when expanded.  Ex:
 * 		#define wow(a, b...)		process (b, a, b)
 *		{ wow (1, 2, 3); }	->	{ process (2, 3, 1, 2, 3); }
 *		{ wow (one, two); }	->	{ process (two, one, two); }
 * if this "rest_arg" is used with the concat token '##' and if it is not
 * supplied then the token attached to with ## will not be outputted.  Ex:
 * 		#define wow (a, b...)		process (b ## , a, ## b)
 *		{ wow (1, 2); }		->	{ process (2, 1, 2); }
 *		{ wow (one); }		->	{ process (one); {
 */
static char rest_extension[] = "...";
#define REST_EXTENSION_LENGTH	(sizeof (rest_extension) - 1)

/* This is the implicit parameter name when using variable number of
   parameters for macros using the ISO C 9x extension.  */
static char va_args_name[] = "__VA_ARGS__";
#define VA_ARGS_NAME_LENGTH	(sizeof (va_args_name) - 1)

/* The structure of a node in the hash table.  The hash table
   has entries for all tokens defined by #define directives (type T_MACRO),
   plus some special tokens like __LINE__ (these each have their own
   type, and the appropriate code is run when that type of node is seen.
   It does not contain control words like "#define", which are recognized
   by a separate piece of code.  */

/* different flavors of hash nodes --- also used in keyword table */
enum node_type {
 T_DEFINE = 1,	/* the `#define' keyword */
 T_INCLUDE,	/* the `#include' keyword */
 T_INCLUDE_NEXT, /* the `#include_next' keyword */
 T_IMPORT,      /* the `#import' keyword */
 T_IFDEF,	/* the `#ifdef' keyword */
 T_IFNDEF,	/* the `#ifndef' keyword */
 T_IF,		/* the `#if' keyword */
 T_ELSE,	/* `#else' */
 T_PRAGMA,	/* `#pragma' */
 T_ELIF,	/* `#elif' */
 T_UNDEF,	/* `#undef' */
 T_LINE,	/* `#line' */
 T_ERROR,	/* `#error' */
 T_WARNING,	/* `#warning' */
 T_ENDIF,	/* `#endif' */
 T_SCCS,	/* `#sccs', used on system V.  */
 T_IDENT,	/* `#ident', used on system V.  */
 T_ASSERT,	/* `#assert', taken from system V.  */
 T_UNASSERT,	/* `#unassert', taken from system V.  */
 T_SPECLINE,	/* special symbol `__LINE__' */
 T_DATE,	/* `__DATE__' */
 T_FILE,	/* `__FILE__' */
 T_BASE_FILE,	/* `__BASE_FILE__' */
 T_INCLUDE_LEVEL, /* `__INCLUDE_LEVEL__' */
 T_VERSION,	/* `__VERSION__' */
 T_SIZE_TYPE,   /* `__SIZE_TYPE__' */
 T_PTRDIFF_TYPE,   /* `__PTRDIFF_TYPE__' */
 T_WCHAR_TYPE,   /* `__WCHAR_TYPE__' */
 T_USER_LABEL_PREFIX_TYPE, /* `__USER_LABEL_PREFIX__' */
 T_REGISTER_PREFIX_TYPE,   /* `__REGISTER_PREFIX__' */
 T_IMMEDIATE_PREFIX_TYPE,  /* `__IMMEDIATE_PREFIX__' */
 T_TIME,	/* `__TIME__' */
 T_CONST,	/* Constant value, used by `__STDC__' */
 T_MACRO,	/* macro defined by `#define' */
 T_DISABLED,	/* macro temporarily turned off for rescan */
 T_SPEC_DEFINED, /* special `defined' macro for use in #if statements */
 T_PCSTRING,	/* precompiled string (hashval is KEYDEF *) */
 T_POISON,	/* defined with `#pragma poison' */
 T_UNUSED	/* Used for something not defined.  */
 };

struct hashnode {
  struct hashnode *next;	/* double links for easy deletion */
  struct hashnode *prev;
  struct hashnode **bucket_hdr;	/* also, a back pointer to this node's hash
				   chain is kept, in case the node is the head
				   of the chain and gets deleted.  */
  enum node_type type;		/* type of special token */
  int length;			/* length of token, for quick comparison */
  U_CHAR *name;			/* the actual name */
  union hashval value;		/* pointer to expansion, or whatever */
};

typedef struct hashnode HASHNODE;

/* Some definitions for the hash table.  The hash function MUST be
   computed as shown in hashf () below.  That is because the rescan
   loop computes the hash value `on the fly' for most tokens,
   in order to avoid the overhead of a lot of procedure calls to
   the hashf () function.  Hashf () only exists for the sake of
   politeness, for use when speed isn't so important.  */

#define HASHSIZE 1403
static HASHNODE *hashtab[HASHSIZE];
#define HASHSTEP(old, c) ((old << 2) + c)
#define MAKE_POS(v) (v & 0x7fffffff) /* make number positive */


/* We let tm.h override the types used here, to handle trivial differences
   such as the choice of unsigned int or long unsigned int for size_t.
   When machines start needing nontrivial differences in the size type,
   it would be best to do something here to figure out automatically
   from other information what type to use.  */

/* The string value for __SIZE_TYPE__.  */

#ifndef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"
#endif

/* The string value for __PTRDIFF_TYPE__.  */

#ifndef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"
#endif

/* The string value for __WCHAR_TYPE__.  */

#ifndef WCHAR_TYPE
#define WCHAR_TYPE "int"
#endif
static const char * wchar_type = WCHAR_TYPE;
#undef WCHAR_TYPE

/* The string value for __USER_LABEL_PREFIX__ */

#ifndef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""
#endif
static const char * user_label_prefix = USER_LABEL_PREFIX;
#undef USER_LABEL_PREFIX

/* The string value for __REGISTER_PREFIX__ */

#ifndef REGISTER_PREFIX
#define REGISTER_PREFIX ""
#endif

/* The string value for __IMMEDIATE_PREFIX__ */

#ifndef IMMEDIATE_PREFIX
#define IMMEDIATE_PREFIX ""
#endif

/* In the definition of a #assert name, this structure forms
   a list of the individual values asserted.
   Each value is itself a list of "tokens".
   These are strings that are compared by name.  */

struct tokenlist_list {
  struct tokenlist_list *next;
  struct arglist *tokens;
};

struct assertion_hashnode {
  struct assertion_hashnode *next;	/* double links for easy deletion */
  struct assertion_hashnode *prev;
  /* also, a back pointer to this node's hash
     chain is kept, in case the node is the head
     of the chain and gets deleted.  */
  struct assertion_hashnode **bucket_hdr;
  int length;			/* length of token, for quick comparison */
  U_CHAR *name;			/* the actual name */
  /* List of token-sequences.  */
  struct tokenlist_list *value;
};

typedef struct assertion_hashnode ASSERTION_HASHNODE;

/* Some definitions for the hash table.  The hash function MUST be
   computed as shown in hashf below.  That is because the rescan
   loop computes the hash value `on the fly' for most tokens,
   in order to avoid the overhead of a lot of procedure calls to
   the hashf function.  hashf only exists for the sake of
   politeness, for use when speed isn't so important.  */

#define ASSERTION_HASHSIZE 37
static ASSERTION_HASHNODE *assertion_hashtab[ASSERTION_HASHSIZE];

/* Nonzero means inhibit macroexpansion of what seem to be
   assertion tests, in rescan.  For #if.  */
static int assertions_flag;

/* `struct directive' defines one #-directive, including how to handle it.  */

#define DO_PROTO PARAMS ((U_CHAR *, U_CHAR *, FILE_BUF *, struct directive *))

struct directive {
  int length;			/* Length of name */
  int (*func) DO_PROTO;	/* Function to handle directive */
  const char *name;		/* Name of directive */
  enum node_type type;		/* Code which describes which directive.  */
};

#define IS_INCLUDE_DIRECTIVE_TYPE(t) \
((int) T_INCLUDE <= (int) (t) && (int) (t) <= (int) T_IMPORT)

/* These functions are declared to return int instead of void since they
   are going to be placed in the table and some old compilers have trouble with
   pointers to functions returning void.  */

static int do_assert DO_PROTO;
static int do_define DO_PROTO;
static int do_elif DO_PROTO;
static int do_else DO_PROTO;
static int do_endif DO_PROTO;
static int do_error DO_PROTO;
static int do_ident DO_PROTO;
static int do_if DO_PROTO;
static int do_include DO_PROTO;
static int do_line DO_PROTO;
static int do_pragma DO_PROTO;
#ifdef SCCS_DIRECTIVE
static int do_sccs DO_PROTO;
#endif
static int do_unassert DO_PROTO;
static int do_undef DO_PROTO;
static int do_xifdef DO_PROTO;

/* Here is the actual list of #-directives, most-often-used first.  */

static struct directive directive_table[] = {
  {  6, do_define, "define", T_DEFINE},
  {  2, do_if, "if", T_IF},
  {  5, do_xifdef, "ifdef", T_IFDEF},
  {  6, do_xifdef, "ifndef", T_IFNDEF},
  {  5, do_endif, "endif", T_ENDIF},
  {  4, do_else, "else", T_ELSE},
  {  4, do_elif, "elif", T_ELIF},
  {  4, do_line, "line", T_LINE},
  {  7, do_include, "include", T_INCLUDE},
  { 12, do_include, "include_next", T_INCLUDE_NEXT},
  {  6, do_include, "import", T_IMPORT},
  {  5, do_undef, "undef", T_UNDEF},
  {  5, do_error, "error", T_ERROR},
  {  7, do_error, "warning", T_WARNING},
#ifdef SCCS_DIRECTIVE
  {  4, do_sccs, "sccs", T_SCCS},
#endif
  {  6, do_pragma, "pragma", T_PRAGMA},
  {  5, do_ident, "ident", T_IDENT},
  {  6, do_assert, "assert", T_ASSERT},
  {  8, do_unassert, "unassert", T_UNASSERT},
  {  -1, 0, "", T_UNUSED},
};

/* When a directive handler is called,
   this points to the # (or the : of the %:) that started the directive.  */
U_CHAR *directive_start;

/* table to tell if char can be part of a C identifier.  */
U_CHAR is_idchar[256];
/* table to tell if char can be first char of a c identifier.  */
U_CHAR is_idstart[256];
/* table to tell if c is horizontal space.  */
static U_CHAR is_hor_space[256];
/* table to tell if c is horizontal or vertical space.  */
U_CHAR is_space[256];

#define SKIP_WHITE_SPACE(p) do { while (is_hor_space[*p]) p++; } while (0)
#define SKIP_ALL_WHITE_SPACE(p) do { while (is_space[*p]) p++; } while (0)
  
static int errors = 0;			/* Error counter for exit code */

/* Name of output file, for error messages.  */
static const char *out_fname;

/* Nonzero to ignore \ in string constants.  Use to treat #line 1 "A:\file.h
   as a non-form feed.  If you want it to be a form feed, you must use
   # 1 "\f".  */
static int ignore_escape_flag = 1;

/* Stack of conditionals currently in progress
   (including both successful and failing conditionals).  */

struct if_stack {
  struct if_stack *next;	/* for chaining to the next stack frame */
  const char *fname;		/* copied from input when frame is made */
  size_t fname_len;		/* similarly */
  int lineno;			/* similarly */
  int if_succeeded;		/* true if a leg of this if-group
				    has been passed through rescan */
  const U_CHAR *control_macro;	/* For #ifndef at start of file,
				   this is the macro name tested.  */
  enum node_type type;		/* type of last directive seen in this group */
};
typedef struct if_stack IF_STACK_FRAME;
static IF_STACK_FRAME *if_stack = NULL;

/* Buffer of -M output.  */
static char *deps_buffer;

/* Number of bytes allocated in above.  */
static int deps_allocated_size;

/* Number of bytes used.  */
static int deps_size;

/* Number of bytes since the last newline.  */
static int deps_column;

/* Nonzero means -I- has been seen,
   so don't look for #include "foo" the source-file directory.  */
static int ignore_srcdir;

static int safe_read PARAMS ((int, char *, int));
static void safe_write PARAMS ((int, const char *, int));

int main PARAMS ((int, char **));

static void path_include PARAMS ((char *));

static const U_CHAR *index0 PARAMS ((const U_CHAR *, int, size_t));

static void trigraph_pcp PARAMS ((FILE_BUF *));
static void check_white_space PARAMS ((FILE_BUF *));

static void newline_fix PARAMS ((U_CHAR *));
static void name_newline_fix PARAMS ((U_CHAR *));

static const char *get_lintcmd PARAMS ((const U_CHAR *, const U_CHAR *,
					const U_CHAR **, int *, int *));

static void rescan PARAMS ((FILE_BUF *, int));

static FILE_BUF expand_to_temp_buffer PARAMS ((const U_CHAR *, const U_CHAR *,
					       int, int));

static int handle_directive PARAMS ((FILE_BUF *, FILE_BUF *));

static struct tm *timestamp PARAMS ((void));
static void special_symbol PARAMS ((HASHNODE *, FILE_BUF *));

static int is_system_include PARAMS ((const char *));
static char *base_name PARAMS ((const char *));
static int absolute_filename PARAMS ((const char *));
static size_t simplify_filename PARAMS ((char *));

static char *read_filename_string PARAMS ((int, FILE *));
static struct file_name_map *read_name_map PARAMS ((const char *));
static int open_include_file PARAMS ((char *, struct file_name_list *,
				      const U_CHAR *, struct include_file **));
static char *remap_include_file PARAMS ((char *, struct file_name_list *));
static int lookup_ino_include PARAMS ((struct include_file *));

static void finclude PARAMS ((int, struct include_file *, FILE_BUF *, int,
			      struct file_name_list *));
static void record_control_macro PARAMS ((struct include_file *,
					  const U_CHAR *));

static char *check_precompiled PARAMS ((int, struct stat *, const char *,
					const char **));
static int check_preconditions PARAMS ((const char *));
static void pcfinclude PARAMS ((U_CHAR *, const U_CHAR *, FILE_BUF *));
static void pcstring_used PARAMS ((HASHNODE *));
static void write_output PARAMS ((void));
static void pass_thru_directive PARAMS ((const U_CHAR *, const U_CHAR *,
					 FILE_BUF *, struct directive *));

static MACRODEF create_definition PARAMS ((const U_CHAR *, const U_CHAR *,
					   FILE_BUF *));

static int check_macro_name PARAMS ((const U_CHAR *, int));
static int compare_defs PARAMS ((DEFINITION *, DEFINITION *));
static int comp_def_part PARAMS ((int, const U_CHAR *, int, const U_CHAR *,
				  int, int));

static DEFINITION *collect_expansion  PARAMS ((const U_CHAR *, const U_CHAR *,
					       int, struct arglist *));

int check_assertion PARAMS ((const U_CHAR *, int, int, struct arglist *));
static int compare_token_lists PARAMS ((struct arglist *, struct arglist *));

static struct arglist *read_token_list PARAMS ((const U_CHAR **,
						const U_CHAR *, int *));
static void free_token_list PARAMS ((struct arglist *));

static ASSERTION_HASHNODE *assertion_install PARAMS ((const U_CHAR *, int, int));
static ASSERTION_HASHNODE *assertion_lookup PARAMS ((const U_CHAR *, int, int));
static void delete_assertion PARAMS ((ASSERTION_HASHNODE *));

static void do_once PARAMS ((void));

static HOST_WIDEST_INT eval_if_expression PARAMS ((const U_CHAR *, int));
static void conditional_skip PARAMS ((FILE_BUF *, int, enum node_type,
				      const U_CHAR *, FILE_BUF *));
static void skip_if_group PARAMS ((FILE_BUF *, int, FILE_BUF *));
static void validate_else PARAMS ((const U_CHAR *, const U_CHAR *));

static U_CHAR *skip_to_end_of_comment PARAMS ((FILE_BUF *, int *, int));
static U_CHAR *skip_quoted_string PARAMS ((const U_CHAR *, const U_CHAR *,
					   int, int *, int *, int *));
static char *quote_string PARAMS ((char *, const char *, size_t));
static U_CHAR *skip_paren_group PARAMS ((FILE_BUF *));

/* Last arg to output_line_directive.  */
enum file_change_code {same_file, enter_file, leave_file};
static void output_line_directive PARAMS ((FILE_BUF *, FILE_BUF *, int, enum file_change_code));

static void macroexpand PARAMS ((HASHNODE *, FILE_BUF *));

struct argdata;
static int macarg PARAMS ((struct argdata *, int));

static U_CHAR *macarg1 PARAMS ((U_CHAR *, const U_CHAR *, struct hashnode *, int *, int *, int *, int));

static int discard_comments PARAMS ((U_CHAR *, int, int));

static void change_newlines PARAMS ((struct argdata *));

static void notice PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;
static void vnotice PARAMS ((const char *, va_list));
void error PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;
void verror PARAMS ((const char *, va_list));
static void error_from_errno PARAMS ((const char *));
void warning PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;
static void vwarning PARAMS ((const char *, va_list));
static void error_with_line PARAMS ((int, const char *, ...)) ATTRIBUTE_PRINTF_2;
static void verror_with_line PARAMS ((int, const char *, va_list));
static void vwarning_with_line PARAMS ((int, const char *, va_list));
static void warning_with_line PARAMS ((int, const char *, ...)) ATTRIBUTE_PRINTF_2;
void pedwarn PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;
void pedwarn_with_line PARAMS ((int, const char *, ...)) ATTRIBUTE_PRINTF_2;
static void pedwarn_with_file_and_line PARAMS ((const char *, size_t, int, const char *, ...)) ATTRIBUTE_PRINTF_4;
static void pedwarn_strange_white_space PARAMS ((int));

static void print_containing_files PARAMS ((void));

static int line_for_error PARAMS ((int));
static int grow_outbuf PARAMS ((FILE_BUF *, int));

static HASHNODE *install PARAMS ((const U_CHAR *, int, enum node_type,
				  const char *, int));
HASHNODE *lookup PARAMS ((const U_CHAR *, int, int));
static void delete_macro PARAMS ((HASHNODE *));
static int hashf PARAMS ((const U_CHAR *, int, int));

static void dump_single_macro PARAMS ((HASHNODE *, FILE *));
static void dump_all_macros PARAMS ((void));
static void dump_defn_1 PARAMS ((const U_CHAR *, int, int, FILE *));
static void dump_arg_n PARAMS ((DEFINITION *, int, FILE *));

static void initialize_char_syntax PARAMS ((void));
static void initialize_builtins PARAMS ((FILE_BUF *, FILE_BUF *));

static void make_definition PARAMS ((char *));
static void make_undef PARAMS ((char *, FILE_BUF *));

static void make_assertion PARAMS ((const char *, const char *));

static struct file_name_list *new_include_prefix PARAMS ((struct file_name_list *, const char *, const char *, const char *));
static void append_include_chain PARAMS ((struct file_name_list *, struct file_name_list *));

static int quote_string_for_make PARAMS ((char *, const char *));
static void deps_output PARAMS ((const char *, int));

void fatal PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
void fancy_abort PARAMS ((void)) ATTRIBUTE_NORETURN;
static void perror_with_name PARAMS ((const char *));
static void pfatal_with_name PARAMS ((const char *)) ATTRIBUTE_NORETURN;
static void pipe_closed PARAMS ((int)) ATTRIBUTE_NORETURN;

static void memory_full PARAMS ((void)) ATTRIBUTE_NORETURN;
static void print_help PARAMS ((void));

/* Read LEN bytes at PTR from descriptor DESC, for file FILENAME,
   retrying if necessary.  If MAX_READ_LEN is defined, read at most
   that bytes at a time.  Return a negative value if an error occurs,
   otherwise return the actual number of bytes read,
   which must be LEN unless end-of-file was reached.  */

static int
safe_read (desc, ptr, len)
     int desc;
     char *ptr;
     int len;
{
  int left, rcount, nchars;

  left = len;
  while (left > 0) {
    rcount = left;
#ifdef MAX_READ_LEN
    if (rcount > MAX_READ_LEN)
      rcount = MAX_READ_LEN;
#endif
    nchars = read (desc, ptr, rcount);
    if (nchars < 0)
      {
#ifdef EINTR
	if (errno == EINTR)
	  continue;
#endif
	return nchars;
      }
    if (nchars == 0)
      break;
    ptr += nchars;
    left -= nchars;
  }
  return len - left;
}

/* Write LEN bytes at PTR to descriptor DESC,
   retrying if necessary, and treating any real error as fatal.
   If MAX_WRITE_LEN is defined, write at most that many bytes at a time.  */

static void
safe_write (desc, ptr, len)
     int desc;
     const char *ptr;
     int len;
{
  int wcount, written;

  while (len > 0) {
    wcount = len;
#ifdef MAX_WRITE_LEN
    if (wcount > MAX_WRITE_LEN)
      wcount = MAX_WRITE_LEN;
#endif
    written = write (desc, ptr, wcount);
    if (written < 0)
      {
#ifdef EINTR
	if (errno == EINTR)
	  continue;
#endif
	pfatal_with_name (out_fname);
      }
    ptr += written;
    len -= written;
  }
}


static void
print_help ()
{
  printf ("Usage: %s [switches] input output\n", progname);
  printf ("Switches:\n");
  printf ("  -include <file>           Include the contents of <file> before other files\n");
  printf ("  -imacros <file>           Accept definition of macros in <file>\n");
  printf ("  -iprefix <path>           Specify <path> as a prefix for next two options\n");
  printf ("  -iwithprefix <dir>        Add <dir> to the end of the system include paths\n");
  printf ("  -iwithprefixbefore <dir>  Add <dir> to the end of the main include paths\n");
  printf ("  -isystem <dir>            Add <dir> to the start of the system include paths\n");
  printf ("  -idirafter <dir>          Add <dir> to the end of the system include paths\n");
  printf ("  -I <dir>                  Add <dir> to the end of the main include paths\n");
  printf ("  -nostdinc                 Do not search the system include directories\n");
  printf ("  -nostdinc++               Do not search the system include directories for C++\n");
  printf ("  -o <file>                 Put output into <file>\n");
  printf ("  -pedantic                 Issue all warnings demanded by strict ANSI C\n");
  printf ("  -traditional              Follow K&R pre-processor behaviour\n");
  printf ("  -trigraphs                Support ANSI C trigraphs\n");
  printf ("  -lang-c                   Assume that the input sources are in C\n");
  printf ("  -lang-c89                 Assume that the input is C89; depricated\n");
  printf ("  -lang-c++                 Assume that the input sources are in C++\n");
  printf ("  -lang-objc                Assume that the input sources are in ObjectiveC\n");
  printf ("  -lang-objc++              Assume that the input sources are in ObjectiveC++\n");
  printf ("  -lang-asm                 Assume that the input sources are in assembler\n");
  printf ("  -lang-fortran	       Assume that the input sources are in Fortran\n");
  printf ("  -lang-chill               Assume that the input sources are in Chill\n");
  printf ("  -std=<std name>           Specify the conformance standard; one of:\n");
  printf ("                            gnu89, gnu9x, c89, c9x, iso9899:1990,\n");
  printf ("                            iso9899:199409, iso9899:199x\n");
  printf ("  -+                        Allow parsing of C++ style features\n");
  printf ("  -w                        Inhibit warning messages\n");
  printf ("  -Wtrigraphs               Warn if trigraphs are encountered\n");
  printf ("  -Wno-trigraphs            Do not warn about trigraphs\n");
  printf ("  -Wcomment{s}              Warn if one comment starts inside another\n");
  printf ("  -Wno-comment{s}           Do not warn about comments\n");
  printf ("  -Wtraditional             Warn if a macro argument is/would be turned into\n");
  printf ("                             a string if -traditional is specified\n");
  printf ("  -Wno-traditional          Do not warn about stringification\n");
  printf ("  -Wundef                   Warn if an undefined macro is used by #if\n");
  printf ("  -Wno-undef                Do not warn about testing undefined macros\n");
  printf ("  -Wimport                  Warn about the use of the #import directive\n");
  printf ("  -Wno-import               Do not warn about the use of #import\n");
  printf ("  -Werror                   Treat all warnings as errors\n");
  printf ("  -Wno-error                Do not treat warnings as errors\n");
  printf ("  -Wall                     Enable all preprocessor warnings\n");
  printf ("  -M                        Generate make dependencies\n");
  printf ("  -MM                       As -M, but ignore system header files\n");
  printf ("  -MD                       As -M, but put output in a .d file\n");
  printf ("  -MMD                      As -MD, but ignore system header files\n");
  printf ("  -MG                       Treat missing header file as generated files\n");
  printf ("  -g                        Include #define and #undef directives in the output\n");
  printf ("  -D<macro>                 Define a <macro> with string '1' as its value\n");
  printf ("  -D<macro>=<val>           Define a <macro> with <val> as its value\n");
  printf ("  -A<question> (<answer>)   Assert the <answer> to <question>\n");
  printf ("  -U<macro>                 Undefine <macro> \n");
  printf ("  -u or -undef              Do not predefine any macros\n");
  printf ("  -v                        Display the version number\n");
  printf ("  -H                        Print the name of header files as they are used\n");
  printf ("  -C                        Do not discard comments\n");
  printf ("  -dM                       Display a list of macro definitions active at end\n");
  printf ("  -dD                       Preserve macro definitions in output\n");
  printf ("  -dN                       As -dD except that only the names are preserved\n");
  printf ("  -dI                       Include #include directives in the output\n");
  printf ("  -ifoutput                 Describe skipped code blocks in output \n");
  printf ("  -P                        Do not generate #line directives\n");
  printf ("  -$                        Do not include '$' in identifiers\n");
  printf ("  -remap                    Remap file names when including files.\n");
  printf ("  -h or --help              Display this information\n");
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  struct stat st;
  const char *in_fname;
  char *cp;
  int f, i;
  FILE_BUF *fp;

  char **pend_files;
  char **pend_defs;
  char **pend_undefs;
  char **pend_assertions;
  char **pend_includes;

  /* Record the option used with each element of pend_assertions.
     This is preparation for supporting more than one option for making
     an assertion.  */
  const char **pend_assertion_options;
  int no_standard_includes = 0;
  int no_standard_cplusplus_includes = 0;
  int missing_newline = 0;

  /* Non-0 means don't output the preprocessed program.  */
  int inhibit_output = 0;
  /* Non-0 means -v, so print the full set of include dirs.  */
  int verbose = 0;

  /* File name which deps are being written to.
     This is 0 if deps are being written to stdout.  */
  char *deps_file = 0;
  /* Fopen file mode to open deps_file with.  */
  const char *deps_mode = "a";
  /* Stream on which to print the dependency information.  */
  FILE *deps_stream = 0;
  /* Target-name to write with the dependency information.  */
  char *deps_target = 0;

#if defined (RLIMIT_STACK) && defined (HAVE_GETRLIMIT) && defined (HAVE_SETRLIMIT)
  /* Get rid of any avoidable limit on stack size.  */
  {
    struct rlimit rlim;

    /* Set the stack limit huge so that alloca (particularly stringtab
       in dbxread.c) does not fail.  */
    getrlimit (RLIMIT_STACK, &rlim);
    rlim.rlim_cur = rlim.rlim_max;
    setrlimit (RLIMIT_STACK, &rlim);
  }
#endif

#ifdef SIGPIPE
  signal (SIGPIPE, pipe_closed);
#endif

#ifdef HAVE_LC_MESSAGES
  setlocale (LC_MESSAGES, "");
#endif
  (void) bindtextdomain (PACKAGE, localedir);
  (void) textdomain (PACKAGE);

  progname = base_name (argv[0]);

#ifdef VMS
  {
    /* Remove extension from PROGNAME.  */
    char *p;
    char *s = xstrdup (progname);
    progname = s;

    if ((p = rindex (s, ';')) != 0) *p = '\0';	/* strip version number */
    if ((p = rindex (s, '.')) != 0		/* strip type iff ".exe" */
	&& (p[1] == 'e' || p[1] == 'E')
	&& (p[2] == 'x' || p[2] == 'X')
	&& (p[3] == 'e' || p[3] == 'E')
	&& !p[4])
      *p = '\0';
  }
#endif

  /* Do not invoke xmalloc before this point, since locale and
     progname need to be set first, in case a diagnostic is issued.  */
     
  pend_files = (char **) xmalloc (argc * sizeof (char *));
  pend_defs = (char **) xmalloc ((2 * argc) * sizeof (char *));
  pend_undefs = (char **) xmalloc (argc * sizeof (char *));
  pend_assertions = (char **) xmalloc (argc * sizeof (char *));
  pend_includes = (char **) xmalloc (argc * sizeof (char *));
  pend_assertion_options = (const char **) xmalloc (argc * sizeof (char *));

  in_fname = NULL;
  out_fname = NULL;

  /* Initialize is_idchar.  */
  initialize_char_syntax ();

  no_line_directives = 0;
  no_trigraphs = 1;
  dump_macros = dump_none;
  no_output = 0;
  cplusplus = 0;
  cplusplus_comments = 1;

  bzero ((char *) pend_files, argc * sizeof (char *));
  bzero ((char *) pend_defs, (2 * argc) * sizeof (char *));
  bzero ((char *) pend_undefs, argc * sizeof (char *));
  bzero ((char *) pend_assertions, argc * sizeof (char *));
  bzero ((char *) pend_includes, argc * sizeof (char *));

#ifdef MULTIBYTE_CHARS
  /* Change to the native locale for multibyte conversions.  */
  setlocale (LC_CTYPE, "");
  literal_codeset = getenv ("LANG");
#endif

  /* Process switches and find input file name.  */

  for (i = 1; i < argc; i++) {
    if (argv[i][0] != '-') {
      if (out_fname != NULL)
	{
	  print_help ();
	  fatal ("Too many arguments");
	}
      else if (in_fname != NULL)
	out_fname = argv[i];
      else
	in_fname = argv[i];
    } else {
      switch (argv[i][1]) {

      case 'i':
	if (!strcmp (argv[i], "-include")) {
	  if (i + 1 == argc)
	    fatal ("Filename missing after `-include' option");
	  else {
	    i++;
	    simplify_filename (pend_includes[i] = argv[i]);
	  }
	}
	if (!strcmp (argv[i], "-imacros")) {
	  if (i + 1 == argc)
	    fatal ("Filename missing after `-imacros' option");
	  else {
	    i++;
	    simplify_filename (pend_files[i] = argv[i]);
	  }
	}
	if (!strcmp (argv[i], "-iprefix")) {
	  if (i + 1 == argc)
	    fatal ("Filename missing after `-iprefix' option");
	  else
	    include_prefix = argv[++i];
	}
	if (!strcmp (argv[i], "-ifoutput")) {
	  output_conditionals = 1;
	}
	if (!strcmp (argv[i], "-isystem")) {
	  struct file_name_list *dirtmp;

	  if (! (dirtmp = new_include_prefix (NULL_PTR, NULL_PTR,
					      "", argv[++i])))
	    break;
	  dirtmp->c_system_include_path = 1;

	  if (before_system == 0)
	    before_system = dirtmp;
	  else
	    last_before_system->next = dirtmp;
	  last_before_system = dirtmp; /* Tail follows the last one */
	}
	/* Add directory to end of path for includes,
	   with the default prefix at the front of its name.  */
	if (!strcmp (argv[i], "-iwithprefix")) {
	  struct file_name_list *dirtmp;
	  char *prefix;

	  if (include_prefix != 0)
	    prefix = include_prefix;
	  else {
	    prefix = xstrdup (GCC_INCLUDE_DIR);
	    /* Remove the `include' from /usr/local/lib/gcc.../include.  */
	    if (!strcmp (prefix + strlen (prefix) - 8, "/include"))
	      prefix[strlen (prefix) - 7] = 0;
	  }

	  if (! (dirtmp = new_include_prefix (NULL_PTR, NULL_PTR,
					      prefix, argv[++i])))
	    break;

	  if (after_include == 0)
	    after_include = dirtmp;
	  else
	    last_after_include->next = dirtmp;
	  last_after_include = dirtmp; /* Tail follows the last one */
	}
	/* Add directory to main path for includes,
	   with the default prefix at the front of its name.  */
	if (!strcmp (argv[i], "-iwithprefixbefore")) {
	  struct file_name_list *dirtmp;
	  char *prefix;

	  if (include_prefix != 0)
	    prefix = include_prefix;
	  else {
	    prefix = xstrdup (GCC_INCLUDE_DIR);
	    /* Remove the `include' from /usr/local/lib/gcc.../include.  */
	    if (!strcmp (prefix + strlen (prefix) - 8, "/include"))
	      prefix[strlen (prefix) - 7] = 0;
	  }

	  dirtmp = new_include_prefix (NULL_PTR, NULL_PTR, prefix, argv[++i]);
	  append_include_chain (dirtmp, dirtmp);
	}
	/* Add directory to end of path for includes.  */
	if (!strcmp (argv[i], "-idirafter")) {
	  struct file_name_list *dirtmp;

	  if (! (dirtmp = new_include_prefix (NULL_PTR, NULL_PTR,
					      "", argv[++i])))
	    break;

	  if (after_include == 0)
	    after_include = dirtmp;
	  else
	    last_after_include->next = dirtmp;
	  last_after_include = dirtmp; /* Tail follows the last one */
	}
	break;

      case 'o':
	if (out_fname != NULL)
	  fatal ("Output filename specified twice");
	if (i + 1 == argc)
	  fatal ("Filename missing after -o option");
	out_fname = argv[++i];
	if (!strcmp (out_fname, "-"))
	  out_fname = "";
	break;

      case 'p':
	if (!strcmp (argv[i], "-pedantic"))
	  pedantic = 1;
	else if (!strcmp (argv[i], "-pedantic-errors")) {
	  pedantic = 1;
	  pedantic_errors = 1;
	} else if (!strcmp (argv[i], "-pcp")) {
	  char *pcp_fname;
	  if (i + 1 == argc)
	    fatal ("Filename missing after -pcp option");
	  pcp_fname = argv[++i];
	  pcp_outfile
	    = ((pcp_fname[0] != '-' || pcp_fname[1] != '\0')
	       ? fopen (pcp_fname, "w")
	       : stdout);
	  if (pcp_outfile == 0)
	    pfatal_with_name (pcp_fname);
	  no_precomp = 1;
	}
	break;

      case 't':
	if (!strcmp (argv[i], "-traditional")) {
	  traditional = 1;
	  cplusplus_comments = 0;
	} else if (!strcmp (argv[i], "-trigraphs")) {
	  no_trigraphs = 0;
	}
	break;

      case 'l':
	if (! strcmp (argv[i], "-lang-c"))
	  cplusplus = 0, cplusplus_comments = 1, c89 = 0, c9x = 1, objc = 0;
	else if (! strcmp (argv[i], "-lang-c89"))
	  {
	    cplusplus = 0, cplusplus_comments = 0, c89 = 1, c9x = 0, objc = 0;
	    no_trigraphs = 0;
	    pend_defs[2*i] = "__STRICT_ANSI__";
	  }
	else if (! strcmp (argv[i], "-lang-c++"))
	  cplusplus = 1, cplusplus_comments = 1, c89 = 0, c9x = 0, objc = 0;
	else if (! strcmp (argv[i], "-lang-objc"))
	  cplusplus = 0, cplusplus_comments = 1, c89 = 0, c9x = 0, objc = 1;
	else if (! strcmp (argv[i], "-lang-objc++"))
	  cplusplus = 1, cplusplus_comments = 1, c89 = 0, c9x = 0, objc = 1;
 	else if (! strcmp (argv[i], "-lang-asm"))
 	  lang_asm = 1;
	else if (! strcmp (argv[i], "-lang-fortran"))
	  /* Doesn't actually do anything.  */ ;
 	else if (! strcmp (argv[i], "-lint"))
 	  for_lint = 1;
	break;

      case '+':
	cplusplus = 1, cplusplus_comments = 1;
	break;

      case 's':
	if (!strcmp (argv[i], "-std=gnu89"))
	  {
	    cplusplus = 0, cplusplus_comments = 0, c89 = 1, c9x = 0, objc = 0;
	  }
	else if (!strcmp (argv[i], "-std=gnu9x")
		 || !strcmp (argv[i], "-std=gnu99"))
	  {
	    cplusplus = 0, cplusplus_comments = 1, c89 = 0, c9x = 1, objc = 0;
	    pend_defs[2*i+1] = "__STDC_VERSION__=199901L";
	  }
	else if (!strcmp (argv[i], "-std=iso9899:1990")
		 || !strcmp (argv[i], "-std=c89"))
	  {
	    cplusplus = 0, cplusplus_comments = 0, c89 = 1, c9x = 0, objc = 0;
	    no_trigraphs = 0;
	    pend_defs[2*i] = "__STRICT_ANSI__";
	  }
	else if (!strcmp (argv[i], "-std=iso9899:199409"))
	  {
	    cplusplus = 0, cplusplus_comments = 0, c89 = 1, c9x = 0, objc = 0;
	    no_trigraphs = 0;
	    pend_defs[2*i] = "__STRICT_ANSI__";
	    pend_defs[2*i+1] = "__STDC_VERSION__=199409L";
	  }
        else if (!strcmp (argv[i], "-std=iso9899:199x")
		 || !strcmp (argv[i], "-std=iso9899:1999")
		 || !strcmp (argv[i], "-std=c9x")
		 || !strcmp (argv[i], "-std=c99"))
	  {
	    cplusplus = 0, cplusplus_comments = 1, c89 = 0, c9x = 1, objc = 0;
	    no_trigraphs = 0;
	    pend_defs[2*i] = "__STRICT_ANSI__";
	    pend_defs[2*i+1] = "__STDC_VERSION__=199901L";
	  }
	break;

      case 'w':
	inhibit_warnings = 1;
	break;

      case 'W':
	if (!strcmp (argv[i], "-Wtrigraphs"))
	  warn_trigraphs = 1;
	else if (!strcmp (argv[i], "-Wno-trigraphs"))
	  warn_trigraphs = 0;
	else if (!strcmp (argv[i], "-Wcomment"))
	  warn_comments = 1;
	else if (!strcmp (argv[i], "-Wno-comment"))
	  warn_comments = 0;
	else if (!strcmp (argv[i], "-Wcomments"))
	  warn_comments = 1;
	else if (!strcmp (argv[i], "-Wno-comments"))
	  warn_comments = 0;
	else if (!strcmp (argv[i], "-Wtraditional"))
	  warn_stringify = 1;
	else if (!strcmp (argv[i], "-Wno-traditional"))
	  warn_stringify = 0;
	else if (!strcmp (argv[i], "-Wwhite-space"))
	  warn_white_space = 1;
	else if (!strcmp (argv[i], "-Wno-white-space"))
	  warn_white_space = 0;
	else if (!strcmp (argv[i], "-Wundef"))
	  warn_undef = 1;
	else if (!strcmp (argv[i], "-Wno-undef"))
	  warn_undef = 0;
	else if (!strcmp (argv[i], "-Wimport"))
	  warn_import = 1;
	else if (!strcmp (argv[i], "-Wno-import"))
	  warn_import = 0;
	else if (!strcmp (argv[i], "-Werror"))
	  warnings_are_errors = 1;
	else if (!strcmp (argv[i], "-Wno-error"))
	  warnings_are_errors = 0;
	else if (!strcmp (argv[i], "-Wall"))
	  {
	    warn_trigraphs = 1;
	    warn_comments = 1;
	    warn_white_space = 1;
	  }
	break;

      case 'f':
	if (!strcmp (argv[i], "-fleading-underscore"))
	  user_label_prefix = "_";
	else if (!strcmp (argv[i], "-fno-leading-underscore"))
	  user_label_prefix = "";
	break;

      case 'M':
	/* The style of the choices here is a bit mixed.
	   The chosen scheme is a hybrid of keeping all options in one string
	   and specifying each option in a separate argument:
	   -M|-MM|-MD file|-MMD file [-MG].  An alternative is:
	   -M|-MM|-MD file|-MMD file|-MG|-MMG; or more concisely:
	   -M[M][G][D file].  This is awkward to handle in specs, and is not
	   as extensible.  */
	/* ??? -MG must be specified in addition to one of -M or -MM.
	   This can be relaxed in the future without breaking anything.
	   The converse isn't true.  */

	/* -MG isn't valid with -MD or -MMD.  This is checked for later.  */
	if (!strcmp (argv[i], "-MG"))
	  {
	    print_deps_missing_files = 1;
	    break;
	  }
	if (!strcmp (argv[i], "-M"))
	  print_deps = 2;
	else if (!strcmp (argv[i], "-MM"))
	  print_deps = 1;
	else if (!strcmp (argv[i], "-MD"))
	  print_deps = 2;
	else if (!strcmp (argv[i], "-MMD"))
	  print_deps = 1;
	/* For -MD and -MMD options, write deps on file named by next arg.  */
	if (!strcmp (argv[i], "-MD")
	    || !strcmp (argv[i], "-MMD")) {
	  if (i + 1 == argc)
	    fatal ("Filename missing after %s option", argv[i]);
	  i++;
	  deps_file = argv[i];
	  deps_mode = "w";
	} else {
	  /* For -M and -MM, write deps on standard output
	     and suppress the usual output.  */
	  deps_stream = stdout;
	  inhibit_output = 1;
	}	  
	break;

      case 'd':
	{
	  char *p = argv[i] + 2;
	  char c;
	  while ((c = *p++)) {
	    /* Arg to -d specifies what parts of macros to dump */
	    switch (c) {
	    case 'M':
	      dump_macros = dump_only;
	      no_output = 1;
	      break;
	    case 'N':
	      dump_macros = dump_names;
	      break;
	    case 'D':
	      dump_macros = dump_definitions;
	      break;
	    case 'I':
	      dump_includes = 1;
	      break;
	    }
	  }
	}
	break;

      case 'g':
	if (argv[i][2] == '3')
	  debug_output = 1;
	break;

      case '-':
	if (strcmp (argv[i], "--help") != 0)
	  return i;
	print_help ();
	exit (0);
	break;

      case 'v':
	notice ("GNU CPP version %s", version_string);
#ifdef TARGET_VERSION
	TARGET_VERSION;
#endif
	fprintf (stderr, "\n");
	verbose = 1;
	break;

      case 'H':
	print_include_names = 1;
	break;

      case 'D':
	if (argv[i][2] != 0)
	  pend_defs[2*i] = argv[i] + 2;
	else if (i + 1 == argc)
	  fatal ("Macro name missing after -D option");
	else
	  i++, pend_defs[2*i] = argv[i];
	break;

      case 'A':
	{
	  char *p;

	  if (argv[i][2] != 0)
	    p = argv[i] + 2;
	  else if (i + 1 == argc)
	    fatal ("Assertion missing after -A option");
	  else
	    p = argv[++i];

	  if (!strcmp (p, "-")) {
	    /* -A- eliminates all predefined macros and assertions.
	       Let's include also any that were specified earlier
	       on the command line.  That way we can get rid of any
	       that were passed automatically in from GCC.  */
	    int j;
	    for (j = 0; j < i; j++)
	      pend_defs[2*j] = pend_assertions[j] = 0;
	  } else {
	    pend_assertions[i] = p;
	    pend_assertion_options[i] = "-A";
	  }
	}
	break;

      case 'U':		/* JF #undef something */
	if (argv[i][2] != 0)
	  pend_undefs[i] = argv[i] + 2;
	else if (i + 1 == argc)
	  fatal ("Macro name missing after -U option");
	else
	  pend_undefs[i] = argv[i+1], i++;
	break;

      case 'C':
	put_out_comments = 1;
	break;

      case 'E':			/* -E comes from cc -E; ignore it.  */
	break;

      case 'P':
	no_line_directives = 1;
	break;

      case '$':			/* Don't include $ in identifiers.  */
	is_idchar['$'] = is_idstart['$'] = 0;
	break;

      case 'I':			/* Add directory to path for includes.  */
	{
	  struct file_name_list *dirtmp;
	  char *dir = argv[i][2] ? argv[i] + 2 : argv[++i];

	  if (! ignore_srcdir && !strcmp (dir, "-")) {
	    ignore_srcdir = 1;
	    /* Don't use any preceding -I directories for #include <...>.  */
	    first_bracket_include = 0;
	  }
	  else {
	    dirtmp = new_include_prefix (last_include, NULL_PTR, "", dir);
	    append_include_chain (dirtmp, dirtmp);
	  }
	}
	break;

      case 'n':
	if (!strcmp (argv[i], "-nostdinc"))
	  /* -nostdinc causes no default include directories.
	     You must specify all include-file directories with -I.  */
	  no_standard_includes = 1;
	else if (!strcmp (argv[i], "-nostdinc++"))
	  /* -nostdinc++ causes no default C++-specific include directories. */
	  no_standard_cplusplus_includes = 1;
	else if (!strcmp (argv[i], "-noprecomp"))
	  no_precomp = 1;
	break;

      case 'r':
	if (!strcmp (argv[i], "-remap"))
	  remap = 1;
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
	fatal ("Invalid option `%s'", argv[i]);
      }
    }
  }

  /* Add dirs from CPATH after dirs from -I.  */
  /* There seems to be confusion about what CPATH should do,
     so for the moment it is not documented.  */
  /* Some people say that CPATH should replace the standard include dirs,
     but that seems pointless: it comes before them, so it overrides them
     anyway.  */
  GET_ENV_PATH_LIST (cp, "CPATH");
  if (cp && ! no_standard_includes)
    path_include (cp);

  /* Initialize output buffer */

  outbuf.buf = (U_CHAR *) xmalloc (OUTBUF_SIZE);
  outbuf.bufp = outbuf.buf;
  outbuf.length = OUTBUF_SIZE;

  /* Do partial setup of input buffer for the sake of generating
     early #line directives (when -g is in effect).  */

  fp = &instack[++indepth];
  if (in_fname == NULL)
    in_fname = "";
  fp->nominal_fname = fp->fname = in_fname;
  fp->nominal_fname_len = strlen (in_fname);
  fp->lineno = 0;

  /* In C++, wchar_t is a distinct basic type, and we can expect
     __wchar_t to be defined by cc1plus.  */
  if (cplusplus)
    wchar_type = "__wchar_t";

  /* Install __LINE__, etc.  Must follow initialize_char_syntax
     and option processing.  */
  initialize_builtins (fp, &outbuf);

  /* Now handle the command line options.  */

  /* Do -U's, -D's and -A's in the order they were seen.  */
  for (i = 1; i < argc; i++) {
    if (pend_undefs[i]) {
      if (debug_output)
        output_line_directive (fp, &outbuf, 0, same_file);
      make_undef (pend_undefs[i], &outbuf);
    }
    if (pend_defs[2*i]) {
      if (debug_output)
        output_line_directive (fp, &outbuf, 0, same_file);
      make_definition (pend_defs[2*i]);
    }
    if (pend_defs[2*i+1]) {
      if (debug_output)
        output_line_directive (fp, &outbuf, 0, same_file);
      make_definition (pend_defs[2*i+1]);
    }
    if (pend_assertions[i])
      make_assertion (pend_assertion_options[i], pend_assertions[i]);
  }

  done_initializing = 1;

  { /* Read the appropriate environment variable and if it exists
       replace include_defaults with the listed path.  */
    char *epath = 0;
    switch ((objc << 1) + cplusplus)
      {
      case 0:
	GET_ENV_PATH_LIST (epath, "C_INCLUDE_PATH");
	break;
      case 1:
	GET_ENV_PATH_LIST (epath, "CPLUS_INCLUDE_PATH");
	break;
      case 2:
	GET_ENV_PATH_LIST (epath, "OBJC_INCLUDE_PATH");
	break;
      case 3:
	GET_ENV_PATH_LIST (epath, "OBJCPLUS_INCLUDE_PATH");
	break;
      }
    /* If the environment var for this language is set,
       add to the default list of include directories.  */
    if (epath) {
      int num_dirs;
      char *startp, *endp;

      for (num_dirs = 1, startp = epath; *startp; startp++)
	if (*startp == PATH_SEPARATOR)
	  num_dirs++;
      include_defaults
	= (struct default_include *) xmalloc ((num_dirs
					       * sizeof (struct default_include))
					      + sizeof (include_defaults_array));
      startp = endp = epath;
      num_dirs = 0;
      while (1) {
	char c = *endp++;
	if (c == PATH_SEPARATOR || !c) {
	  endp[-1] = 0;
	  include_defaults[num_dirs].fname
	    = startp == endp ? "." : xstrdup (startp);
	  endp[-1] = c;
	  include_defaults[num_dirs].component = 0;
	  include_defaults[num_dirs].cplusplus = cplusplus;
	  include_defaults[num_dirs].cxx_aware = 1;
	  num_dirs++;
	  if (!c)
	    break;
	  startp = endp;
	}
      }
      /* Put the usual defaults back in at the end.  */
      bcopy ((const PTR) include_defaults_array,
	     (PTR) &include_defaults[num_dirs],
	     sizeof (include_defaults_array));
    }
  }

  append_include_chain (before_system, last_before_system);
  first_system_include = before_system;

  /* Unless -fnostdinc,
     tack on the standard include file dirs to the specified list */
  if (!no_standard_includes) {
    struct default_include *p = include_defaults;
    char *specd_prefix = include_prefix;
    char *default_prefix = xstrdup (GCC_INCLUDE_DIR);
    int default_len = 0;
    /* Remove the `include' from /usr/local/lib/gcc.../include.  */
    if (!strcmp (default_prefix + strlen (default_prefix) - 8, "/include")) {
      default_len = strlen (default_prefix) - 7;
      default_prefix[default_len] = 0;
    }
    /* Search "translated" versions of GNU directories.
       These have /usr/local/lib/gcc... replaced by specd_prefix.  */
    if (specd_prefix != 0 && default_len != 0)
      for (p = include_defaults; p->fname; p++) {
	/* Some standard dirs are only for C++.  */
	if (!p->cplusplus || (cplusplus && !no_standard_cplusplus_includes)) {
	  /* Does this dir start with the prefix?  */
	  if (!strncmp (p->fname, default_prefix, default_len)) {
	    /* Yes; change prefix and add to search list.  */
	    struct file_name_list *new
	      = new_include_prefix (NULL_PTR, NULL_PTR, specd_prefix,
				    p->fname + default_len);
	    if (new) {
	      new->c_system_include_path = !p->cxx_aware;
	      append_include_chain (new, new);
	      if (first_system_include == 0)
		first_system_include = new;
	      p->included = 1;
	    }
	  }
	}
      }
    /* Search ordinary names for GNU include directories.  */
    for (p = include_defaults; p->fname; p++) {
      /* Some standard dirs are only for C++.  */
      if (!p->cplusplus || (cplusplus && !no_standard_cplusplus_includes)) {
	struct file_name_list *new
	  = new_include_prefix (NULL_PTR, p->component, "", p->fname);
	if (new) {
	  new->c_system_include_path = !p->cxx_aware;
	  append_include_chain (new, new);
	  if (first_system_include == 0)
	    first_system_include = new;
	  p->included = 1;
	}
      }
    }
  }

  /* Tack the after_include chain at the end of the include chain.  */
  append_include_chain (after_include, last_after_include);
  if (first_system_include == 0)
    first_system_include = after_include;

  /* With -v, print the list of dirs to search.  */
  if (verbose) {
    struct file_name_list *p;
    notice ("#include \"...\" search starts here:\n");
    for (p = include; p; p = p->next) {
      if (p == first_bracket_include)
	notice ("#include <...> search starts here:\n");
      if (!p->fname[0])
	fprintf (stderr, " .\n");
      else if (!strcmp (p->fname, "/") || !strcmp (p->fname, "//"))
	fprintf (stderr, " %s\n", p->fname);
      else
	/* Omit trailing '/'.  */
	fprintf (stderr, " %.*s\n", (int) strlen (p->fname) - 1, p->fname);
    }
    notice ("End of search list.\n");
    {
      struct default_include * d;
      notice ("The following default directories have been omitted from the search path:\n");
      for (d = include_defaults; d->fname; d++)
	if (! d->included)
	  fprintf (stderr, " %s\n", d->fname);
      notice ("End of omitted list.\n");
    }
  }

  /* -MG doesn't select the form of output and must be specified with one of
     -M or -MM.  -MG doesn't make sense with -MD or -MMD since they don't
     inhibit compilation.  */
  if (print_deps_missing_files && (print_deps == 0 || !inhibit_output))
    fatal ("-MG must be specified with one of -M or -MM");

  /* Either of two environment variables can specify output of deps.
     Its value is either "OUTPUT_FILE" or "OUTPUT_FILE DEPS_TARGET",
     where OUTPUT_FILE is the file to write deps info to
     and DEPS_TARGET is the target to mention in the deps.  */

  if (print_deps == 0
      && (getenv ("SUNPRO_DEPENDENCIES") != 0
	  || getenv ("DEPENDENCIES_OUTPUT") != 0)) {
    char *spec = getenv ("DEPENDENCIES_OUTPUT");
    char *s;
    char *output_file;

    if (spec == 0) {
      spec = getenv ("SUNPRO_DEPENDENCIES");
      print_deps = 2;
    }
    else
      print_deps = 1;

    /* Find the space before the DEPS_TARGET, if there is one.  */
    s = index (spec, ' ');
    if (s) {
      deps_target = s + 1;
      output_file = xmalloc (s - spec + 1);
      bcopy (spec, output_file, s - spec);
      output_file[s - spec] = 0;
    } else {
      deps_target = 0;
      output_file = spec;
    }
      
    deps_file = output_file;
    deps_mode = "a";
  }

  /* For -M, print the expected object file name
     as the target of this Make-rule.  */
  if (print_deps) {
    deps_allocated_size = 200;
    deps_buffer = xmalloc (deps_allocated_size);
    deps_buffer[0] = 0;
    deps_size = 0;
    deps_column = 0;

    if (deps_target) {
      deps_output (deps_target, ':');
    } else if (*in_fname == 0) {
      deps_output ("-", ':');
    } else {
      char *p, *q;
      int len;

      q = base_name (in_fname);

      /* Copy remainder to mungable area.  */
      p = (char *) alloca (strlen(q) + 8);
      strcpy (p, q);

      /* Output P, but remove known suffixes.  */
      len = strlen (p);
      q = p + len;
      if (len >= 2
	  && p[len - 2] == '.'
	  && index("cCsSm", p[len - 1]))
	q = p + (len - 2);
      else if (len >= 3
	       && p[len - 3] == '.'
	       && p[len - 2] == 'c'
	       && p[len - 1] == 'c')
	q = p + (len - 3);
      else if (len >= 4
	       && p[len - 4] == '.'
	       && p[len - 3] == 'c'
	       && p[len - 2] == 'x'
	       && p[len - 1] == 'x')
	q = p + (len - 4);
      else if (len >= 4
	       && p[len - 4] == '.'
	       && p[len - 3] == 'c'
	       && p[len - 2] == 'p'
	       && p[len - 1] == 'p')
	q = p + (len - 4);

      /* Supply our own suffix.  */
      strcpy (q, OBJECT_SUFFIX);

      deps_output (p, ':');
    }

    deps_output (in_fname, ' ');
  }

  /* Scan the -imacros files before the main input.
     Much like #including them, but with no_output set
     so that only their macro definitions matter.  */

  no_output++; no_record_file++;
  for (i = 1; i < argc; i++)
    if (pend_files[i]) {
      struct include_file *inc;
      int fd = open_include_file (pend_files[i], NULL_PTR, NULL_PTR, &inc);
      if (fd < 0) {
	perror_with_name (pend_files[i]);
	return FATAL_EXIT_CODE;
      }
      finclude (fd, inc, &outbuf, 0, NULL_PTR);
    }
  no_output--; no_record_file--;

  /* Copy the entire contents of the main input file into
     the stacked input buffer previously allocated for it.  */

  /* JF check for stdin */
  if (in_fname == NULL || *in_fname == 0) {
    in_fname = "";
    f = 0;
  } else if ((f = open (in_fname, O_RDONLY, 0666)) < 0)
    goto perror;

  if (fstat (f, &st) != 0)
    pfatal_with_name (in_fname);
  fp->nominal_fname = fp->fname = in_fname;
  fp->nominal_fname_len = strlen (in_fname);
  fp->lineno = 1;
  fp->system_header_p = 0;
  /* JF all this is mine about reading pipes and ttys */
  if (! S_ISREG (st.st_mode)) {
    /* Read input from a file that is not a normal disk file.
       We cannot preallocate a buffer with the correct size,
       so we must read in the file a piece at the time and make it bigger.  */
    int size;
    int bsize;
    int cnt;

    if (S_ISDIR (st.st_mode))
      fatal ("Input file `%s' is a directory", in_fname);

    bsize = 2000;
    size = 0;
    fp->buf = (U_CHAR *) xmalloc (bsize + 2);
    for (;;) {
      cnt = safe_read (f, (char *) fp->buf + size, bsize - size);
      if (cnt < 0) goto perror;	/* error! */
      size += cnt;
      if (size != bsize) break;	/* End of file */
      bsize *= 2;
      fp->buf = (U_CHAR *) xrealloc (fp->buf, bsize + 2);
    }
    fp->length = size;
  } else {
    /* Read a file whose size we can determine in advance.
       For the sake of VMS, st.st_size is just an upper bound.  */
    size_t s = (size_t) st.st_size;
    if (s != st.st_size || s + 2 < s)
      memory_full ();
    fp->buf = (U_CHAR *) xmalloc (s + 2);
    fp->length = safe_read (f, (char *) fp->buf, s);
    if (fp->length < 0) goto perror;
  }
  fp->bufp = fp->buf;
  fp->if_stack = if_stack;

  /* Make sure data ends with a newline.  And put a null after it.  */

  if ((fp->length > 0 && fp->buf[fp->length - 1] != '\n')
      /* Backslash-newline at end is not good enough.  */
      || (fp->length > 1 && fp->buf[fp->length - 2] == '\\')) {
    fp->buf[fp->length++] = '\n';
    missing_newline = 1;
  }
  fp->buf[fp->length] = '\0';

  /* Unless inhibited, convert trigraphs in the input.  */

  if (!no_trigraphs)
    trigraph_pcp (fp);

  if (warn_white_space)
    check_white_space (fp);

  /* Now that we know the input file is valid, open the output.  */

  if (!out_fname || !strcmp (out_fname, ""))
    out_fname = "stdout";
  else if (! freopen (out_fname, "w", stdout))
    pfatal_with_name (out_fname);

  output_line_directive (fp, &outbuf, 0, same_file);

  /* Scan the -include files before the main input.  */

  no_record_file++;
  for (i = 1; i < argc; i++)
    if (pend_includes[i]) {
      struct include_file *inc;
      int fd = open_include_file (pend_includes[i], NULL_PTR, NULL_PTR, &inc);
      if (fd < 0) {
	perror_with_name (pend_includes[i]);
	return FATAL_EXIT_CODE;
      }
      finclude (fd, inc, &outbuf, 0, NULL_PTR);
    }
  no_record_file--;

  /* Scan the input, processing macros and directives.  */

  rescan (&outbuf, 0);

  if (missing_newline)
    fp->lineno--;

  if (pedantic && missing_newline)
    pedwarn ("file does not end in newline");

  /* Now we have processed the entire input
     Write whichever kind of output has been requested.  */

  if (dump_macros == dump_only)
    dump_all_macros ();
  else if (! inhibit_output) {
    write_output ();
  }

  if (print_deps) {
    /* Don't actually write the deps file if compilation has failed.  */
    if (errors == 0) {
      if (deps_file && ! (deps_stream = fopen (deps_file, deps_mode)))
	pfatal_with_name (deps_file);
      fputs (deps_buffer, deps_stream);
      putc ('\n', deps_stream);
      if (deps_file) {
	if (ferror (deps_stream) || fclose (deps_stream) != 0)
	  fatal ("I/O error on output");
      }
    }
  }

  if (pcp_outfile && pcp_outfile != stdout
      && (ferror (pcp_outfile) || fclose (pcp_outfile) != 0))
    fatal ("I/O error on `-pcp' output");

  if (ferror (stdout) || fclose (stdout) != 0)
    fatal ("I/O error on output");

  if (errors)
    exit (FATAL_EXIT_CODE);
  exit (SUCCESS_EXIT_CODE);

 perror:
  pfatal_with_name (in_fname);
  return 0;
}

/* Given a colon-separated list of file names PATH,
   add all the names to the search path for include files.  */

static void
path_include (path)
     char *path;
{
  char *p;

  p = path;

  if (*p)
    while (1) {
      char *q = p;
      char c;
      struct file_name_list *dirtmp;

      /* Find the end of this name.  */
      while ((c = *q++) != PATH_SEPARATOR && c)
	continue;

      q[-1] = 0;
      dirtmp = new_include_prefix (last_include, NULL_PTR,
				   "", p == q ? "." : p);
      q[-1] = c;
      append_include_chain (dirtmp, dirtmp);

      /* Advance past this name.  */
      p = q;
      if (! c)
	break;
    }
}

/* Return the address of the first character in S that equals C.
   S is an array of length N, possibly containing '\0's, and followed by '\0'.
   Return 0 if there is no such character.  Assume that C itself is not '\0'.
   If we knew we could use memchr, we could just invoke memchr (S, C, N),
   but unfortunately memchr isn't autoconfigured yet.  */

static const U_CHAR *
index0 (s, c, n)
     const U_CHAR *s;
     int c;
     size_t n;
{
  const char *p = (const char *) s;
  for (;;) {
    const char *q = index (p, c);
    if (q)
      return (const U_CHAR *) q;
    else {
      size_t l = strlen (p);
      if (l == n)
	return 0;
      l++;
      p += l;
      n -= l;
    }
  }
}

/* Pre-C-Preprocessor to translate ANSI trigraph idiocy in BUF
   before main CCCP processing.  Name `pcp' is also in honor of the
   drugs the trigraph designers must have been on.

   Using an extra pass through the buffer takes a little extra time,
   but is infinitely less hairy than trying to handle trigraphs inside
   strings, etc. everywhere, and also makes sure that trigraphs are
   only translated in the top level of processing.  */

static void
trigraph_pcp (buf)
     FILE_BUF *buf;
{
  register U_CHAR c, *bptr;
  register const U_CHAR *fptr, *sptr, *lptr;
  int len;

  fptr = sptr = bptr = buf->buf;
  lptr = fptr + buf->length;
  while ((sptr = index0 (sptr, '?', (size_t) (lptr - sptr))) != NULL) {
    if (*++sptr != '?')
      continue;
    switch (*++sptr) {
      case '=':
      c = '#';
      break;
    case '(':
      c = '[';
      break;
    case '/':
      c = '\\';
      break;
    case ')':
      c = ']';
      break;
    case '\'':
      c = '^';
      break;
    case '<':
      c = '{';
      break;
    case '!':
      c = '|';
      break;
    case '>':
      c = '}';
      break;
    case '-':
      c  = '~';
      break;
    case '?':
      sptr--;
      continue;
    default:
      continue;
    }
    len = sptr - fptr - 2;

    /* BSD doc says bcopy () works right for overlapping strings.  In ANSI
       C, this will be memmove ().  */
    if (bptr != fptr && len > 0)
      bcopy ((const PTR) fptr, (PTR) bptr, len);

    bptr += len;
    *bptr++ = c;
    fptr = ++sptr;
  }
  len = buf->length - (fptr - buf->buf);
  if (bptr != fptr && len > 0)
    bcopy ((const PTR) fptr, (PTR) bptr, len);
  buf->length -= fptr - bptr;
  buf->buf[buf->length] = '\0';
  if (warn_trigraphs && fptr != bptr)
    warning_with_line (0, "%lu trigraph(s) encountered",
		       (unsigned long) (fptr - bptr) / 2);
}

/* Warn about white space between backslash and end of line.  */

static void
check_white_space (buf)
     FILE_BUF *buf;
{
  register const U_CHAR *sptr = buf->buf;
  register const U_CHAR *lptr = sptr + buf->length;
  register const U_CHAR *nptr;
  int line = 0;

  nptr = sptr = buf->buf;
  lptr = sptr + buf->length;
  for (nptr = sptr;
       (nptr = index0 (nptr, '\n', (size_t) (lptr - nptr))) != NULL;
       nptr ++) {
    register const U_CHAR *p = nptr;
    line++;
    for (p = nptr; sptr < p; p--) {
      if (! is_hor_space[p[-1]]) {
	if (p[-1] == '\\' && p != nptr)
	  warning_with_line (line, 
			     "`\\' followed by white space at end of line");
	break;
      }
    }
  }
}

/* Move all backslash-newline pairs out of embarrassing places.
   Exchange all such pairs following BP
   with any potentially-embarrassing characters that follow them.
   Potentially-embarrassing characters are / and *
   (because a backslash-newline inside a comment delimiter
   would cause it not to be recognized).
   We assume that *BP == '\\'.  */

static void
newline_fix (bp)
     U_CHAR *bp;
{
  register U_CHAR *p = bp;

  /* First count the backslash-newline pairs here.  */
  do {
    if (p[1] != '\n')
      break;
    p += 2;
  } while (*p == '\\');

  /* What follows the backslash-newlines is not embarrassing.  */

  if (*p != '/' && *p != '*')
    /* What follows the backslash-newlines is not embarrassing.  */
    return;

  /* Copy all potentially embarrassing characters
     that follow the backslash-newline pairs
     down to where the pairs originally started.  */
  do
    *bp++ = *p++;
  while (*p == '*' || *p == '/');

  /* Now write the same number of pairs after the embarrassing chars.  */
  while (bp < p) {
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
  register U_CHAR *p = bp;

  /* First count the backslash-newline pairs here.  */
  do {
    if (p[1] != '\n')
      break;
    p += 2;
  } while (*p == '\\');

  /* What follows the backslash-newlines is not embarrassing.  */

  if (!is_idchar[*p])
    /* What follows the backslash-newlines is not embarrassing.  */
    return;

  /* Copy all potentially embarrassing characters
     that follow the backslash-newline pairs
     down to where the pairs originally started.  */
  do
    *bp++ = *p++;
  while (is_idchar[*p]);

  /* Now write the same number of pairs after the embarrassing chars.  */
  while (bp < p) {
    *bp++ = '\\';
    *bp++ = '\n';
  }
}

/* Look for lint commands in comments.

   When we come in here, ibp points into a comment.  Limit is as one expects.
   scan within the comment -- it should start, after lwsp, with a lint command.
   If so that command is returned as a (constant) string.

   Upon return, any arg will be pointed to with argstart and will be
   arglen long.  Note that we don't parse that arg since it will just
   be printed out again.  */

static const char *
get_lintcmd (ibp, limit, argstart, arglen, cmdlen)
     register const U_CHAR *ibp;
     register const U_CHAR *limit;
     const U_CHAR **argstart;	/* point to command arg */
     int *arglen, *cmdlen;	/* how long they are */
{
  HOST_WIDEST_INT linsize;
  register const U_CHAR *numptr;	/* temp for arg parsing */

  *arglen = 0;

  SKIP_WHITE_SPACE (ibp);

  if (ibp >= limit) return NULL;

  linsize = limit - ibp;
  
  /* Oh, I wish C had lexical functions... hell, I'll just open-code the set */
  if ((linsize >= 10) && !bcmp (ibp, "NOTREACHED", 10)) {
    *cmdlen = 10;
    return "NOTREACHED";
  }
  if ((linsize >= 8) && !bcmp (ibp, "ARGSUSED", 8)) {
    *cmdlen = 8;
    return "ARGSUSED";
  }
  if ((linsize >= 11) && !bcmp (ibp, "LINTLIBRARY", 11)) {
    *cmdlen = 11;
    return "LINTLIBRARY";
  }
  if ((linsize >= 7) && !bcmp (ibp, "VARARGS", 7)) {
    *cmdlen = 7;
    ibp += 7; linsize -= 7;
    if ((linsize == 0) || ! ISDIGIT (*ibp)) return "VARARGS";

    /* OK, read a number */
    for (numptr = *argstart = ibp; (numptr < limit) && ISDIGIT (*numptr);
	 numptr++);
    *arglen = numptr - *argstart;
    return "VARARGS";
  }
  return NULL;
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
 *   There are two types of Newline markers:
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
  register U_CHAR c;

  /* Length of pending accumulated identifier.  */
  register int ident_length = 0;

  /* Hash code of pending accumulated identifier.  */
  register int hash = 0;

  /* Current input level (&instack[indepth]).  */
  FILE_BUF *ip;

  /* Pointer for scanning input.  */
  register U_CHAR *ibp;

  /* Pointer to end of input.  End of scan is controlled by LIMIT.  */
  register U_CHAR *limit;

  /* Pointer for storing output.  */
  register U_CHAR *obp;

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
     obp = op->bufp; } while (0)

  if (no_output && instack[indepth].fname != 0)
    skip_if_group (&instack[indepth], 1, NULL);

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
      if (*ibp == '\n' && !ip->macro) {
	/* At the top level, always merge lines ending with backslash-newline,
	   even in middle of identifier.  But do not merge lines in a macro,
	   since backslash might be followed by a newline-space marker.  */
	++ibp;
	++ip->lineno;
	--obp;		/* remove backslash from obuf */
	break;
      }
      /* If ANSI, backslash is just another character outside a string.  */
      if (!traditional)
	goto randomchar;
      /* Otherwise, backslash suppresses specialness of following char,
	 so copy it here to prevent the switch from seeing it.
	 But first get any pending identifier processed.  */
      if (ident_length > 0)
	goto specialchar;
      if (ibp < limit)
	*obp++ = *ibp++;
      break;

    case '%':
      if (ident_length || ip->macro || traditional)
	goto randomchar;
      while (*ibp == '\\' && ibp[1] == '\n') {
	ibp += 2;
	++ip->lineno;
      }
      if (*ibp != ':')
	break;
      /* Treat this %: digraph as if it were #.  */
      /* Fall through.  */

    case '#':
      if (assertions_flag) {
	if (ident_length)
	  goto specialchar;
	/* Copy #foo (bar lose) without macro expansion.  */
	obp[-1] = '#';	/* In case it was '%'.  */
	SKIP_WHITE_SPACE (ibp);
	while (is_idchar[*ibp])
	  *obp++ = *ibp++;
	SKIP_WHITE_SPACE (ibp);
	if (*ibp == '(') {
	  ip->bufp = ibp;
	  skip_paren_group (ip);
	  bcopy ((const PTR) ibp, (PTR) obp, ip->bufp - ibp);
	  obp += ip->bufp - ibp;
	  ibp = ip->bufp;
	}
	break;
      }

      /* If this is expanding a macro definition, don't recognize
	 preprocessing directives.  */
      if (ip->macro != 0)
	goto randomchar;
      /* If this is expand_into_temp_buffer,
	 don't recognize them either.  Warn about them
	 only after an actual newline at this level,
	 not at the beginning of the input level.  */
      if (! ip->fname) {
	if (ip->buf != beg_of_line)
	  warning ("preprocessing directive not recognized within macro arg");
	goto randomchar;
      }
      if (ident_length)
	goto specialchar;

      
      /* # keyword: a # must be first nonblank char on the line */
      if (beg_of_line == 0)
	goto randomchar;
      {
	U_CHAR *bp;

	/* Scan from start of line, skipping whitespace, comments
	   and backslash-newlines, and see if we reach this #.
	   If not, this # is not special.  */
	bp = beg_of_line;
	/* If -traditional, require # to be at beginning of line.  */
	if (!traditional) {
	  while (1) {
	    if (is_hor_space[*bp])
	      bp++;
	    else if (*bp == '\\' && bp[1] == '\n')
	      bp += 2;
	    else if (*bp == '/' && bp[1] == '*') {
	      bp += 2;
	      while (1)
		{
		  if (*bp == '*')
		    {
		      if (bp[1] == '/')
			{
			  bp += 2;
			  break;
			}
		    }
		  else
		    {
#ifdef MULTIBYTE_CHARS
		      int length;
		      length = local_mblen (bp, limit - bp);
		      if (length > 1)
			bp += (length - 1);
#endif
		    }
		  bp++;
		}
	    }
	    /* There is no point in trying to deal with C++ // comments here,
	       because if there is one, then this # must be part of the
	       comment and we would never reach here.  */
	    else break;
	  }
	  if (c == '%') {
	    if (bp[0] != '%')
	      break;
	    while (bp[1] == '\\' && bp[2] == '\n')
	      bp += 2;
	    if (bp + 1 != ibp)
	      break;
	    /* %: appears at start of line; skip past the ':' too.  */
	    bp++;
	    ibp++;
	  }
	}
	if (bp + 1 != ibp)
	  goto randomchar;
      }

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
	  skip_if_group (&instack[indepth], 1, 0);
	  RECACHE;
	  beg_of_line = ibp;
	  break;
	}
	*obp++ = '#';	/* Copy # (even if it was originally %:).  */
	/* Don't expand an identifier that could be a macro directive.
	   (Section 3.8.3 of the ANSI C standard)			*/
	SKIP_WHITE_SPACE (ibp);
	if (is_idstart[*ibp])
	  {
	    *obp++ = *ibp++;
	    while (is_idchar[*ibp])
	      *obp++ = *ibp++;
	  }
	goto randomchar;
      }
#ifdef USE_C_ALLOCA
      alloca (0);
#endif
      /* A # directive has been successfully processed.  */
      /* If not generating expanded output, ignore everything until
	 next # directive.  */
      if (no_output && instack[indepth].fname)
	skip_if_group (&instack[indepth], 1, 0);
      obp = op->bufp;
      RECACHE;
      beg_of_line = ibp;
      break;

    case '\"':			/* skip quoted string */
    case '\'':
      /* A single quoted string is treated like a double -- some
	 programs (e.g., troff) are perverse this way */

      /* Handle any pending identifier;
	 but the L in L'...' or L"..." is not an identifier.  */
      if (ident_length) {
	if (! (ident_length == 1 && hash == HASHSTEP (0, 'L')))
	  goto specialchar;
	ident_length = hash = 0;
      }

      start_line = ip->lineno;

      /* Skip ahead to a matching quote.  */

      while (1) {
	if (ibp >= limit) {
	  if (ip->macro != 0) {
	    /* try harder: this string crosses a macro expansion boundary.
	       This can happen naturally if -traditional.
	       Otherwise, only -D can make a macro with an unmatched quote.  */
	    POPMACRO;
	    RECACHE;
	    continue;
	  }
	  if (!traditional) {
	    error_with_line (line_for_error (start_line),
			     "unterminated string or character constant");
	    if (multiline_string_line) {
	      error_with_line (multiline_string_line,
			       "possible real start of unterminated constant");
	      multiline_string_line = 0;
	    }
	  }
	  break;
	}
	*obp++ = *ibp;
	switch (*ibp++) {
	case '\n':
	  if (warn_white_space && ip->fname && is_hor_space[ibp[-2]])
	    warning ("white space at end of line in string");
	  ++ip->lineno;
	  ++op->lineno;
	  /* Traditionally, end of line ends a string constant with no error.
	     So exit the loop and record the new line.  */
	  if (traditional) {
	    beg_of_line = ibp;
	    goto while2end;
	  }
	  if (c == '\'') {
	    error_with_line (line_for_error (start_line),
			     "unterminated character constant");
	    goto while2end;
	  }
	  if (multiline_string_line == 0) {
	    if (pedantic)
	      pedwarn_with_line (line_for_error (start_line),
				 "string constant runs past end of line");
	    multiline_string_line = ip->lineno - 1;
	  }
	  break;

	case '\\':
	  if (*ibp == '\n') {
	    /* Backslash newline is replaced by nothing at all, but
	       keep the line counts correct.  But if we are reading
	       from a macro, keep the backslash newline, since backslash
	       newlines have already been processed.  */
	    if (ip->macro) {
	      *obp++ = '\n';
	      ++op->lineno;
	    } else
	      --obp;
	    ++ibp;
	    ++ip->lineno;
	  } else {
	    /* ANSI stupidly requires that in \\ the second \
	       is *not* prevented from combining with a newline.  */
	    if (!ip->macro) {
	      while (*ibp == '\\' && ibp[1] == '\n') {
		*obp++ = *ibp++;
		*obp++ = *ibp++;
		++ip->lineno;
		++op->lineno;
	      }
	    }
	    *obp++ = *ibp++;
	  }
	  break;

	case '\"':
	case '\'':
	  if (ibp[-1] == c)
	    goto while2end;
	  break;
#ifdef MULTIBYTE_CHARS
	default:
	  {
	    int length;
	    --ibp;
	    length = local_mblen (ibp, limit - ibp);
	    if (length > 0)
	      {
		--obp;
		bcopy (ibp, obp, length);
		obp += length;
		ibp += length;
	      }
	    else
	      ++ibp;
	  }
	  break;
#endif
	}
      }
    while2end:
      break;

    case '/':
      if (ip->macro != 0)
	goto randomchar;
      if (*ibp == '\\')
	newline_fix (ibp);
      if (*ibp != '*'
	  && !(cplusplus_comments && *ibp == '/'))
	goto randomchar;
      if (ident_length)
	goto specialchar;

      if (*ibp == '/') {
	/* C++ style comment...  */
	start_line = ip->lineno;

	/* Comments are equivalent to spaces.  */
	if (! put_out_comments)
	  obp[-1] = ' ';

	{
	  U_CHAR *before_bp = ibp;

	  while (++ibp < limit) {
	    if (*ibp == '\n')
	      {
		if (put_out_comments) {
		  bcopy ((const PTR) before_bp, (PTR) obp, ibp - before_bp);
		  obp += ibp - before_bp;
		}
		break;
	      }
	    if (*ibp == '\\')
	      {
		if (ibp + 1 < limit && ibp[1] == '\n')
		  {
		    if (warn_comments)
		      warning ("multiline `//' comment");
		    ++ip->lineno;
		    /* Copy the newline into the output buffer, in order to
		       avoid the pain of a #line every time a multiline comment
		       is seen.  */
		    if (!put_out_comments)
		      *obp++ = '\n';
		    ++op->lineno;
		    ++ibp;
		  }
	      }
	    else
	      {
#ifdef MULTIBYTE_CHARS
		int length;
		length = local_mblen (ibp, limit - ibp);
		if (length > 1)
		  ibp += (length - 1);
#endif
	      }
	  }
	  break;
	}
      }

      /* Ordinary C comment.  Skip it, optionally copying it to output.  */

      start_line = ip->lineno;

      ++ibp;			/* Skip the star.  */

      /* If this cpp is for lint, we peek inside the comments: */
      if (for_lint) {
	const U_CHAR *argbp;
	int cmdlen, arglen;
	const char *lintcmd =
	  get_lintcmd (ibp, limit, &argbp, &arglen, &cmdlen);

	if (lintcmd != NULL) {
	  op->bufp = obp;
	  check_expand (op, cmdlen + arglen + 14);
	  obp = op->bufp;
	  /* I believe it is always safe to emit this newline: */
	  obp[-1] = '\n';
	  bcopy ("#pragma lint ", (char *) obp, 13);
	  obp += 13;
	  bcopy (lintcmd, (char *) obp, cmdlen);
	  obp += cmdlen;

	  if (arglen != 0) {
	    *(obp++) = ' ';
	    bcopy (argbp, (char *) obp, arglen);
	    obp += arglen;
	  }

	  /* OK, now bring us back to the state we were in before we entered
	     this branch.  We need #line because the #pragma's newline always
	     messes up the line count.  */
	  op->bufp = obp;
	  output_line_directive (ip, op, 0, same_file);
	  check_expand (op, limit - ibp + 2);
	  obp = op->bufp;
	  *(obp++) = '/';
	}
      }

      /* Comments are equivalent to spaces.
	 Note that we already output the slash; we might not want it.
	 For -traditional, a comment is equivalent to nothing.  */
      if (! put_out_comments) {
	if (traditional)
	  obp--;
	else
	  obp[-1] = ' ';
      }
      else
	*obp++ = '*';

      {
	U_CHAR *before_bp = ibp;

	for (;;) {
	  switch (*ibp++) {
	  case '*':
	    if (ibp[-2] == '/' && warn_comments)
	      warning ("`/*' within comment");
	    if (*ibp == '\\')
	      newline_fix (ibp);
	    if (*ibp == '/')
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
	    break;

	  case 0:
	    if (limit < ibp) {
	      error_with_line (line_for_error (start_line),
			       "unterminated comment");
	      goto limit_reached;
	    }
	    break;
#ifdef MULTIBYTE_CHARS
	  default:
	    {
	      int length;
	      length = local_mblen (ibp, limit - ibp);
	      if (length > 1)
		ibp += (length - 1);
	    }
	    break;
#endif
	  }
	}
      comment_end:

	ibp++;
	if (put_out_comments) {
	  bcopy ((const PTR) before_bp, (PTR) obp, ibp - before_bp);
	  obp += ibp - before_bp;
	}
      }
      break;

    case '$':
      if (! is_idchar['$'])
	goto randomchar;
      if (pedantic)
	pedwarn ("`$' in identifier");
      goto letter;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      /* If digit is not part of identifier, it starts a number,
	 which means that following letters are not an identifier.
	 "0x5" does not refer to an identifier "x5".
	 So copy all alphanumerics that follow without accumulating
	 as an identifier.  Periods also, for sake of "3.e7".  */

      if (ident_length == 0) {
	for (;;) {
	  if (!ip->macro) {
	    while (ibp[0] == '\\' && ibp[1] == '\n') {
	      ++ip->lineno;
	      ibp += 2;
	    }
	  }
	  c = *ibp++;
	  if (!is_idchar[c] && c != '.') {
	    --ibp;
	    break;
	  }
	  *obp++ = c;
	  /* A sign can be part of a preprocessing number
	     if it follows an `e' or `p'.  */
	  if (c == 'e' || c == 'E' || c == 'p' || c == 'P') {
	    if (!ip->macro) {
	      while (ibp[0] == '\\' && ibp[1] == '\n') {
		++ip->lineno;
		ibp += 2;
	      }
	    }
	    if (*ibp == '+' || *ibp == '-') {
	      *obp++ = *ibp++;
	      /* But traditional C does not let the token go past the sign,
		 and C89 does not allow `p'.  */
	      if (traditional || (c89 && (c == 'p' || c == 'P')))
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
    letter:
      ident_length++;
      /* Compute step of hash function, to avoid a proc call on every token */
      hash = HASHSTEP (hash, c);
      break;

    case '\n':
      if (ip->fname == 0 && *ibp == '-') {
	/* Newline - inhibits expansion of preceding token.
	   If expanding a macro arg, we keep the newline -.
	   In final output, it is deleted.
	   We recognize Newline - in macro bodies and macro args.  */
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
	break;
      }

      /* If reprocessing a macro expansion, newline is a special marker.  */
      else if (ip->macro != 0) {
	/* Newline White is a "funny space" to separate tokens that are
	   supposed to be separate but without space between.
	   Here White means any whitespace character.
	   Newline - marks a recursive macro use that is not
	   supposed to be expandable.  */

	if (is_space[*ibp]) {
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
	output_line_directive (ip, op, 1, same_file);
	check_expand (op, limit - ibp);
	obp = op->bufp;
      }
      break;

      /* Come here either after (1) a null character that is part of the input
	 or (2) at the end of the input, because there is a null there.  */
    case 0:
      if (ibp <= limit)
	/* Our input really contains a null character.  */
	goto randomchar;

    limit_reached:
      /* At end of a macro-expansion level, pop it and read next level.  */
      if (ip->macro != 0) {
	obp--;
	ibp--;
	/* If traditional, and we have an identifier that ends here,
	   process it now, so we get the right error for recursion.  */
	if (traditional && ident_length
	    && ! is_idchar[*instack[indepth - 1].bufp]) {
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
	register HASHNODE *hp;

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

	if (!pcp_outfile || pcp_inside_if) {
	  for (hp = hashtab[MAKE_POS (hash) % HASHSIZE]; hp != NULL;
	       hp = hp->next) {
	    
	    if (hp->length == ident_length) {
	      int obufp_before_macroname;
	      int op_lineno_before_macroname;
	      register int i = ident_length;
	      register U_CHAR *p = hp->name;
	      register U_CHAR *q = obp - i;
	      int disabled;
	      
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
	      
	      /* Save this as a displacement from the beginning of the output
		 buffer.  We can not save this as a position in the output
		 buffer, because it may get realloc'ed by RECACHE.  */
	      obufp_before_macroname = (obp - op->buf) - ident_length;
	      op_lineno_before_macroname = op->lineno;
	      
	      if (hp->type == T_PCSTRING) {
		pcstring_used (hp); /* Mark the definition of this key
				       as needed, ensuring that it
				       will be output.  */
		break;		/* Exit loop, since the key cannot have a
				   definition any longer.  */
	      }

	      /* Record whether the macro is disabled.  */
	      disabled = hp->type == T_DISABLED;
	      
	      /* This looks like a macro ref, but if the macro was disabled,
		 just copy its name and put in a marker if requested.  */
	      
	      if (disabled) {
#if 0
		/* This error check caught useful cases such as
		   #define foo(x,y) bar (x (y,0), y)
		   foo (foo, baz)  */
		if (traditional)
		  error ("recursive use of macro `%s'", hp->name);
#endif
		
		if (output_marks) {
		  op->bufp = obp;
		  check_expand (op, limit - ibp + 2);
		  obp = op->bufp;
		  *obp++ = '\n';
		  *obp++ = '-';
		}
		break;
	      }
	      
	      /* If macro wants an arglist, verify that a '(' follows.
		 first skip all whitespace, copying it to the output
		 after the macro name.  Then, if there is no '(',
		 decide this is not a macro call and leave things that way.  */
	      if ((hp->type == T_MACRO || hp->type == T_DISABLED)
		  && hp->value.defn->nargs >= 0)
		{
		  U_CHAR *old_ibp = ibp;
		  U_CHAR *old_obp = obp;
		  int old_iln = ip->lineno;
		  int old_oln = op->lineno;
		  
		  while (1) {
		    /* Scan forward over whitespace, copying it to the output.  */
		    if (ibp == limit && ip->macro != 0) {
		      POPMACRO;
		      RECACHE;
		      old_ibp = ibp;
		      old_obp = obp;
		      old_iln = ip->lineno;
		      old_oln = op->lineno;
		    }
		    else if (is_space[*ibp]) {
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
		    else if (ip->macro)
		      break;
		    else if (*ibp == '/') {
		      /* If a comment, copy it unchanged or discard it.  */
		      if (ibp[1] == '\\')
			newline_fix (ibp + 1);
		      if (ibp[1] == '*') {
			if (put_out_comments) {
			  *obp++ = '/';
			  *obp++ = '*';
			} else if (! traditional) {
			  *obp++ = ' ';
			}
			for (ibp += 2; ibp < limit; ibp++) {
			  /* We need not worry about newline-marks,
			     since they are never found in comments.  */
			  if (ibp[0] == '*') {
			    if (ibp[1] == '\\')
			      newline_fix (ibp + 1);
			    if (ibp[1] == '/') {
			      ibp += 2;
			      if (put_out_comments) {
				*obp++ = '*';
				*obp++ = '/';
			      }
			      break;
			    }
			  }
			  else if (*ibp == '\n') {
			    /* Newline in a file.  Count it.  */
			    ++ip->lineno;
			    ++op->lineno;
			  }
			  else
			    {
#ifdef MULTIBYTE_CHARS
			      int length;
			      length = local_mblen (ibp, limit - ibp);
			      if (length > 1)
				{
				  if (put_out_comments)
				    {
				      bcopy (ibp, obp, length - 1);
				      obp += length - 1;
				    }
				  ibp += (length - 1);
				}
#endif
			    }
			  if (put_out_comments)
			    *obp++ = *ibp;
			}
		      } else if (ibp[1] == '/' && cplusplus_comments) {
			if (put_out_comments) {
			  *obp++ = '/';
			  *obp++ = '/';
			} else if (! traditional) {
			  *obp++ = ' ';
			}
			for (ibp += 2; ; ibp++)
			  {
			    if (*ibp == '\n')
			      break;
			    if (*ibp == '\\' && ibp[1] == '\n')
			      {
				if (put_out_comments)
				  *obp++ = *ibp++;
			      }
			    else
			      {
#ifdef MULTIBYTE_CHARS
				int length;
				length = local_mblen (ibp, limit - ibp);
				if (length > 1)
				  {
				    if (put_out_comments)
				      {
					bcopy (ibp, obp, length - 1);
					obp += length - 1;
				      }
				    ibp += (length - 1);
				  }
#endif
			      }
			    if (put_out_comments)
			      *obp++ = *ibp;
			  }
		      } else
			break;
		    }
		    else if (ibp[0] == '\\' && ibp[1] == '\n') {
		      ibp += 2;
		      ++ip->lineno;
		    }
		    else break;
		  }
		  if (*ibp != '(') {
		    /* It isn't a macro call.
		       Put back the space that we just skipped.  */
		    ibp = old_ibp;
		    obp = old_obp;
		    ip->lineno = old_iln;
		    op->lineno = old_oln;
		    /* Exit the for loop.  */
		    break;
		  }
		}
	      
	      /* This is now known to be a macro call.
		 Discard the macro name from the output,
		 along with any following whitespace just copied,
		 but preserve newlines if not outputting marks since this
		 is more likely to do the right thing with line numbers.  */
	      obp = op->buf + obufp_before_macroname;
	      if (output_marks)
		op->lineno = op_lineno_before_macroname;
	      else {
		int newlines = op->lineno - op_lineno_before_macroname;
		while (0 < newlines--)
		  *obp++ = '\n';
	      }

	      /* Prevent accidental token-pasting with a character
		 before the macro call.  */
	      if (!traditional && obp != op->buf) {
		switch (obp[-1]) {
		case '!':  case '%':  case '&':  case '*':
		case '+':  case '-':  case '.':  case '/':
		case ':':  case '<':  case '=':  case '>':
		case '^':  case '|':
		  /* If we are expanding a macro arg, make a newline marker
		     to separate the tokens.  If we are making real output,
		     a plain space will do.  */
		  if (output_marks)
		    *obp++ = '\n';
		  *obp++ = ' ';
		}
	      }

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
	}
	ident_length = hash = 0; /* Stop collecting identifier */
	redo_char = 0;
	concatenated = 0;
      }				/* End if (ident_length > 0) */
    }				/* End switch */
  }				/* End per-char loop */

  /* Come here to return -- but first give an error message
     if there was an unterminated successful conditional.  */
 ending:
  if (if_stack != ip->if_stack)
    {
      const char *str;

      switch (if_stack->type)
	{
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
		       "unterminated `#%s' conditional", str);
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
expand_to_temp_buffer (buf, limit, output_marks, assertions)
     const U_CHAR *buf;
     const U_CHAR *limit;
     int output_marks, assertions;
{
  register FILE_BUF *ip;
  FILE_BUF obuf;
  int length = limit - buf;
  U_CHAR *buf1;
  int odepth = indepth;
  int save_assertions_flag = assertions_flag;

  assertions_flag = assertions;

  if (length < 0)
    abort ();

  /* Set up the input on the input stack.  */

  buf1 = (U_CHAR *) alloca (length + 1);
  {
    register const U_CHAR *p1 = buf;
    register U_CHAR *p2 = buf1;

    while (p1 != limit)
      *p2++ = *p1++;
  }
  buf1[length] = 0;

  /* Set up to receive the output.  */

  obuf.length = length * 2 + 100; /* Usually enough.  Why be stingy?  */
  obuf.bufp = obuf.buf = (U_CHAR *) xmalloc (obuf.length);
  obuf.nominal_fname = 0;
  obuf.inc = 0;
  obuf.dir = 0;
  obuf.fname = 0;
  obuf.macro = 0;
  obuf.if_stack = 0;
  obuf.free_ptr = 0;
  obuf.system_header_p = 0;

  CHECK_DEPTH ({return obuf;});

  ++indepth;

  ip = &instack[indepth];
  ip->fname = 0;
  ip->nominal_fname = 0;
  ip->nominal_fname_len = 0;
  ip->inc = 0;
  ip->system_header_p = 0;
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

  assertions_flag = save_assertions_flag;
  return obuf;
}

/*
 * Process a # directive.  Expects IP->bufp to point after the '#', as in
 * `#define foo bar'.  Passes to the directive handler
 * (do_define, do_include, etc.): the addresses of the 1st and
 * last chars of the directive (starting immediately after the #
 * keyword), plus op and the keyword table pointer.  If the directive
 * contains comments it is copied into a temporary buffer sans comments
 * and the temporary buffer is passed to the directive handler instead.
 * Likewise for backslash-newlines.
 *
 * Returns nonzero if this was a known # directive.
 * Otherwise, returns zero, without advancing the input pointer.
 */

static int
handle_directive (ip, op)
     FILE_BUF *ip, *op;
{
  register U_CHAR *bp, *cp;
  register struct directive *kt;
  register int ident_length;
  U_CHAR *resume_p;

  /* Nonzero means we must copy the entire directive
     to get rid of comments or backslash-newlines.  */
  int copy_directive = 0;

  U_CHAR *ident, *after_ident;

  bp = ip->bufp;

  /* Record where the directive started.  do_xifdef needs this.  */
  directive_start = bp - 1;

  ignore_escape_flag = 1;

  /* Skip whitespace and \-newline.  */
  while (1) {
    if (is_hor_space[*bp]) {
      if (*bp != ' ' && *bp != '\t' && pedantic)
	pedwarn_strange_white_space (*bp);
      bp++;
    } else if (*bp == '/') {
      if (bp[1] == '\\')
	newline_fix (bp + 1);
      if (! (bp[1] == '*' || (cplusplus_comments && bp[1] == '/')))
	break;
      ip->bufp = bp + 2;
      skip_to_end_of_comment (ip, &ip->lineno, 0);
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
    if (is_idchar[*cp])
      cp++;
    else {
      if (*cp == '\\')
	name_newline_fix (cp);
      if (is_idchar[*cp])
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

  if (ident_length == 0 || !is_idstart[*ident]) {
    U_CHAR *p = ident;
    while (is_idchar[*p]) {
      if (*p < '0' || *p > '9')
	break;
      p++;
    }
    /* Handle # followed by a line number.  */
    if (p != ident && !is_idchar[*p]) {
      static struct directive line_directive_table[] = {
	{  4, do_line, "line", T_LINE},
      };
      if (pedantic)
	pedwarn ("`#' followed by integer");
      after_ident = ident;
      kt = line_directive_table;
      ignore_escape_flag = 0;
      goto old_linenum;
    }

    /* Avoid error for `###' and similar cases unless -pedantic.  */
    if (p == ident) {
      while (*p == '#' || is_hor_space[*p]) p++;
      if (*p == '\n') {
	if (pedantic && !lang_asm)
	  warning ("invalid preprocessing directive");
	return 0;
      }
    }

    if (!lang_asm)
      error ("invalid preprocessing directive name");

    return 0;
  }

  /*
   * Decode the keyword and call the appropriate expansion
   * routine, after moving the input pointer up to the next line.
   */
  for (kt = directive_table; kt->length > 0; kt++) {
    if (kt->length == ident_length && !bcmp (kt->name, ident, ident_length)) {
      register U_CHAR *buf;
      register U_CHAR *limit;
      int unterminated;
      int junk;
      int *already_output;

      /* Nonzero means do not delete comments within the directive.
	 #define needs this when -traditional.  */
      int keep_comments;

    old_linenum:

      limit = ip->buf + ip->length;
      unterminated = 0;
      already_output = 0;
      keep_comments = traditional && kt->type == T_DEFINE;
      /* #import is defined only in Objective C, or when on the NeXT.  */
      if (kt->type == T_IMPORT
	  && !(objc || lookup ((const U_CHAR *) "__NeXT__", -1, -1)))
	break;

      /* Find the end of this directive (first newline not backslashed
	 and not in a string or comment).
	 Set COPY_DIRECTIVE if the directive must be copied
	 (it contains a backslash-newline or a comment).  */

      buf = bp = after_ident;
      while (bp < limit) {
	register U_CHAR c = *bp++;
	switch (c) {
	case '\\':
	  if (*bp == '\n') {
	    ip->lineno++;
	    copy_directive = 1;
	    bp++;
	  } else if (traditional && bp < limit)
	    bp++;
	  break;

	case '"':
	  /* "..." is special for #include.  */
	  if (IS_INCLUDE_DIRECTIVE_TYPE (kt->type)) {
	    while (bp < limit && *bp != '\n') {
	      if (*bp == '"') {
		bp++;
		break;
	      }
	      if (*bp == '\\' && bp[1] == '\n') {
		ip->lineno++;
		copy_directive = 1;
		bp++;
	      }
	      bp++;
	    }
	    break;
	  }
	  /* Fall through.  */
	case '\'':
	  bp = skip_quoted_string (bp - 1, limit, ip->lineno, &ip->lineno, &copy_directive, &unterminated);
	  /* Don't bother calling the directive if we already got an error
	     message due to unterminated string.  Skip everything and pretend
	     we called the directive.  */
	  if (unterminated) {
	    if (traditional) {
	      /* Traditional preprocessing permits unterminated strings.  */
	      ip->bufp = bp;
	      goto endloop1;
	    }
	    ip->bufp = bp;
	    return 1;
	  }
	  break;

	  /* <...> is special for #include.  */
	case '<':
	  if (! IS_INCLUDE_DIRECTIVE_TYPE (kt->type))
	    break;
	  while (bp < limit && *bp != '>' && *bp != '\n') {
	    if (*bp == '\\' && bp[1] == '\n') {
	      ip->lineno++;
	      copy_directive = 1;
	      bp++;
	    }
	    bp++;
	  }
	  break;

	case '/':
	  if (*bp == '\\')
	    newline_fix (bp);
	  if (*bp == '*'
	      || (cplusplus_comments && *bp == '/')) {
	    U_CHAR *obp = bp - 1;
	    ip->bufp = bp + 1;
	    skip_to_end_of_comment (ip, &ip->lineno, 0);
	    bp = ip->bufp;
	    /* No need to copy the directive because of a comment at the end;
	       just don't include the comment in the directive.  */
	    if (!put_out_comments) {
	      U_CHAR *p;
	      for (p = bp;  *p == ' ' || *p == '\t';  p++)
		continue;
	      if (*p == '\n') {
		bp = obp;
		goto endloop1;
	      }
	    }
	    /* Don't remove the comments if -traditional.  */
	    if (! keep_comments)
	      copy_directive++;
	  }
	  break;

	case '\f':
	case '\r':
	case '\v':
	  if (pedantic)
	    pedwarn_strange_white_space (c);
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

      /* If a directive should be copied through, and -C was given,
	 pass it through before removing comments.  */
      if (!no_output && put_out_comments
	  && ((kt->type == T_DEFINE || kt->type == T_UNDEF)
	      ? dump_macros == dump_definitions
	      : IS_INCLUDE_DIRECTIVE_TYPE (kt->type) ? dump_includes
	      : kt->type == T_PRAGMA)) {
        int len;

	/* Output directive name.  */
        check_expand (op, kt->length + 2);
	/* Make sure # is at the start of a line */
	if (op->bufp > op->buf && op->bufp[-1] != '\n') {
	  op->lineno++;
	  *op->bufp++ = '\n';
	}
        *op->bufp++ = '#';
        bcopy (kt->name, op->bufp, kt->length);
        op->bufp += kt->length;

	/* Output arguments.  */
	len = (bp - buf);
	check_expand (op, len);
	bcopy (buf, (char *) op->bufp, len);
	op->bufp += len;
	/* Take account of any (escaped) newlines just output.  */
	while (--len >= 0)
	  if (buf[len] == '\n')
	    op->lineno++;

	already_output = &junk;
      }				/* Don't we need a newline or #line? */

      if (copy_directive) {
	register U_CHAR *xp = buf;
	/* Need to copy entire directive into temp buffer before dispatching */

	/* room for directive plus some slop */
	cp = (U_CHAR *) alloca (2 * (bp - buf) + 5);
	buf = cp;

	/* Copy to the new buffer, deleting comments
	   and backslash-newlines (and whitespace surrounding the latter
	   if outside of char and string constants).  */

	while (xp < bp) {
	  register U_CHAR c = *xp++;
	  *cp++ = c;

	  switch (c) {
	  case '\n':
	    abort ();  /* A bare newline should never part of the line.  */
	    break;

	    /* <...> is special for #include.  */
	  case '<':
	    if (! IS_INCLUDE_DIRECTIVE_TYPE (kt->type))
	      break;
	    while (xp < bp && c != '>') {
	      c = *xp++;
	      if (c == '\\' && xp < bp && *xp == '\n')
		xp++;
	      else
		*cp++ = c;
	    }
	    break;

	  case '\\':
	    if (*xp == '\n') {
	      xp++;
	      cp--;
	      if (cp != buf && is_hor_space[cp[-1]]) {
		while (cp - 1 != buf && is_hor_space[cp[-2]])
		  cp--;
		SKIP_WHITE_SPACE (xp);
	      } else if (is_hor_space[*xp]) {
		*cp++ = *xp++;
		SKIP_WHITE_SPACE (xp);
	      }
	    } else if (traditional && xp < bp) {
	      *cp++ = *xp++;
	    }
	    break;

	  case '\'':
	  case '\"':
	    {
	      int backslash_newlines_p;

	      register const U_CHAR *bp1
		= skip_quoted_string (xp - 1, bp, ip->lineno,
				      NULL_PTR, &backslash_newlines_p, 
				      NULL_PTR);
	      if (backslash_newlines_p)
		while (xp != bp1)
		  {
		    /* With something like:

			 #define X "a\
			 b"

		       we should still remove the backslash-newline
		       pair as part of phase two.  */
		    if (xp[0] == '\\' && xp[1] == '\n')
		      xp += 2;
		    else
		      *cp++ = *xp++;
		  }
	      else
		/* This is the same as the loop above, but taking
		   advantage of the fact that we know there are no
		   backslash-newline pairs.  */
		while (xp != bp1)
		  *cp++ = *xp++;
	    }
	    break;

	  case '/':
	    if (*xp == '*'
		|| (cplusplus_comments && *xp == '/')) {
	      ip->bufp = xp + 1;
	      /* If we already copied the directive through,
		 already_output != 0 prevents outputting comment now.  */
	      skip_to_end_of_comment (ip, already_output, 0);
	      if (keep_comments)
		while (xp != ip->bufp)
		  *cp++ = *xp++;
	      /* Delete or replace the slash.  */
	      else if (traditional)
		cp--;
	      else
		cp[-1] = ' ';
	      xp = ip->bufp;
	    }
	  }
	}

	/* Null-terminate the copy.  */

	*cp = 0;
      } else
	cp = bp;

      ip->bufp = resume_p;

      /* Some directives should be written out for cc1 to process,
	 just as if they were not defined.  And sometimes we're copying
	 directives through.  */

      if (!no_output && already_output == 0
	  && ((kt->type == T_DEFINE || kt->type == T_UNDEF)
	      ? (int) dump_names <= (int) dump_macros
	      : IS_INCLUDE_DIRECTIVE_TYPE (kt->type) ? dump_includes
	      : kt->type == T_PRAGMA)) {
        int len;

	/* Output directive name.  */
        check_expand (op, kt->length + 1);
        *op->bufp++ = '#';
        bcopy (kt->name, (char *) op->bufp, kt->length);
        op->bufp += kt->length;

	if (kt->type == T_DEFINE && dump_macros == dump_names) {
	  /* Output `#define name' only.  */
	  U_CHAR *xp = buf;
	  U_CHAR *yp;
	  SKIP_WHITE_SPACE (xp);
	  yp = xp;
	  while (is_idchar[*xp]) xp++;
	  len = (xp - yp);
	  check_expand (op, len + 1);
	  *op->bufp++ = ' ';
	  bcopy (yp, (char *) op->bufp, len);
	} else {
	  /* Output entire directive.  */
	  len = (cp - buf);
	  check_expand (op, len);
	  bcopy (buf, (char *) op->bufp, len);
	}
	op->bufp += len;
      }

      /* Call the appropriate directive handler.  buf now points to
	 either the appropriate place in the input buffer, or to
	 the temp buffer if it was necessary to make one.  cp
	 points to the first char after the contents of the (possibly
	 copied) directive, in either case.  */
      (*kt->func) (buf, cp, op, kt);
      check_expand (op, ip->length - (ip->bufp - ip->buf));

      return 1;
    }
  }

  /* It is deliberate that we don't warn about undefined directives.
     That is the responsibility of cc1.  */
  return 0;
}

static struct tm *
timestamp ()
{
  static struct tm tmbuf;
  if (! tmbuf.tm_mday) {
    time_t t = time ((time_t *) 0);
    struct tm *tm = localtime (&t);
    if (tm)
      tmbuf = *tm;
    else {
      /* Use 0000-01-01 00:00:00 if local time is not available.  */
      tmbuf.tm_year = -1900;
      tmbuf.tm_mday = 1;
    }
  }
  return &tmbuf;
}

static const  char * const monthnames[] = {
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
};

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
  int i, len;
  int true_indepth;
  FILE_BUF *ip = NULL;
  struct tm *timebuf;

  int paren = 0;		/* For special `defined' keyword */

  if (pcp_outfile && pcp_inside_if
      && hp->type != T_SPEC_DEFINED && hp->type != T_CONST)
    error ("Predefined macro `%s' used inside `#if' during precompilation",
	   hp->name);
    
  for (i = indepth; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      break;
    }
  if (ip == NULL) {
    error ("cccp error: not in any file?!");
    return;			/* the show must go on */
  }

  switch (hp->type) {
  case T_FILE:
  case T_BASE_FILE:
    {
      FILE_BUF *p = hp->type == T_FILE ? ip : &instack[0];
      const char *string = p->nominal_fname;

      if (string)
	{
	  size_t string_len = p->nominal_fname_len;
	  char *newbuf = (char *) alloca (3 + 4 * string_len);
	  quote_string (newbuf, string, string_len);
	  buf = newbuf;
	}
      else
	buf = "\"\"";

      break;
    }

  case T_INCLUDE_LEVEL:
    {
      /* Eight bytes ought to be more than enough */
      char *newbuf =  (char *) alloca (8);
      true_indepth = 0;
      for (i = indepth; i >= 0; i--)
	if (instack[i].fname != NULL)
	  true_indepth++;
      sprintf (newbuf, "%d", true_indepth - 1);
      buf = newbuf;
    }
    break;

  case T_VERSION:
    {
      char *newbuf = (char *) alloca (3 + strlen (version_string));
      sprintf (newbuf, "\"%s\"", version_string);
      buf = newbuf;
    }
    break;

#ifndef NO_BUILTIN_SIZE_TYPE
  case T_SIZE_TYPE:
    buf = SIZE_TYPE;
    break;
#endif

#ifndef NO_BUILTIN_PTRDIFF_TYPE
  case T_PTRDIFF_TYPE:
    buf = PTRDIFF_TYPE;
    break;
#endif

  case T_WCHAR_TYPE:
    buf = wchar_type;
    break;

  case T_USER_LABEL_PREFIX_TYPE:
    buf = user_label_prefix;
    break;

  case T_REGISTER_PREFIX_TYPE:
    buf = REGISTER_PREFIX;
    break;

  case T_IMMEDIATE_PREFIX_TYPE:
    buf = IMMEDIATE_PREFIX;
    break;

  case T_CONST:
    buf = hp->value.cpval;
#ifdef STDC_0_IN_SYSTEM_HEADERS
    if (ip->system_header_p
	&& hp->length == 8 && bcmp (hp->name, "__STDC__", 8) == 0
	&& !lookup ((const U_CHAR *) "__STRICT_ANSI__", -1, -1))
      buf = "0";
#endif
    if (pcp_inside_if && pcp_outfile)
      /* Output a precondition for this macro use */
      fprintf (pcp_outfile, "#define %s %s\n", hp->name, buf);
    break;

  case T_SPECLINE:
    {
      char *newbuf = (char *) alloca (10);
      sprintf (newbuf, "%d", ip->lineno);
      buf = newbuf;
    }
    break;

  case T_DATE:
  case T_TIME:
    {
      char *newbuf = (char *) alloca (20);
      timebuf = timestamp ();
      if (hp->type == T_DATE)
	sprintf (newbuf, "\"%s %2d %4d\"", monthnames[timebuf->tm_mon],
		 timebuf->tm_mday, timebuf->tm_year + 1900);
      else
	sprintf (newbuf, "\"%02d:%02d:%02d\"", timebuf->tm_hour,
		 timebuf->tm_min, timebuf->tm_sec);
      buf = newbuf;
    }
    break;

  case T_SPEC_DEFINED:
    buf = " 0 ";		/* Assume symbol is not defined */
    ip = &instack[indepth];
    SKIP_WHITE_SPACE (ip->bufp);
    if (*ip->bufp == '(') {
      paren++;
      ip->bufp++;			/* Skip over the paren */
      SKIP_WHITE_SPACE (ip->bufp);
    }

    if (!is_idstart[*ip->bufp])
      goto oops;
    if (ip->bufp[0] == 'L' && (ip->bufp[1] == '\'' || ip->bufp[1] == '"'))
      goto oops;
    if ((hp = lookup (ip->bufp, -1, -1))) {
      if (pcp_outfile && pcp_inside_if
	  && (hp->type == T_CONST
	      || (hp->type == T_MACRO && hp->value.defn->predefined)))
	/* Output a precondition for this macro use.  */
	fprintf (pcp_outfile, "#define %s\n", hp->name);
      if (hp->type == T_POISON) {
	error("attempt to use poisoned `%s'.", hp->name);
	buf = " 0 ";
      } else {
	buf = " 1 ";
      }
    }
    else
      if (pcp_outfile && pcp_inside_if)	{
	/* Output a precondition for this macro use */
	U_CHAR *cp = ip->bufp;
	fprintf (pcp_outfile, "#undef ");
	while (is_idchar[*cp]) /* Ick! */
	  fputc (*cp++, pcp_outfile);
	putc ('\n', pcp_outfile);
      }
    while (is_idchar[*ip->bufp])
      ++ip->bufp;
    SKIP_WHITE_SPACE (ip->bufp);
    if (paren) {
      if (*ip->bufp != ')')
	goto oops;
      ++ip->bufp;
    }
    break;

  case T_POISON:
    error("attempt to use poisoned `%s'.", hp->name);
    buf = " 0 ";	/* Consider poisoned symbol to not be defined */
    break;

oops:

    error ("`defined' without an identifier");
    break;

  default:
    error ("cccp error: invalid special hash type"); /* time for gdb */
    abort ();
  }
  len = strlen (buf);
  check_expand (op, len);
  bcopy (buf, (char *) op->bufp, len);
  op->bufp += len;

  return;
}


/* Routines to handle #directives */

/* Handle #include and #import.
   This function expects to see "fname" or <fname> on the input.  */

static int
do_include (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  const U_CHAR *importing =
    keyword->type == T_IMPORT ? (const U_CHAR *) "" : (const U_CHAR *) 0;
  int skip_dirs = (keyword->type == T_INCLUDE_NEXT);
  static int import_warning = 0;
  char *fname;		/* Dynamically allocated fname buffer */
  char *pcftry;
  char *pcfname;
  char *fbeg, *fend;		/* Beginning and end of fname */
  U_CHAR *fin;

  struct file_name_list *search_start = include; /* Chain of dirs to search */
  struct file_name_list *dsp;	/* First in chain, if #include "..." */
  struct file_name_list *searchptr = 0;
  size_t flen;

  int f = -3;			/* file number */
  struct include_file *inc = 0;

  int retried = 0;		/* Have already tried macro
				   expanding the include line*/
  int angle_brackets = 0;	/* 0 for "...", 1 for <...> */
#ifdef VMS
  int vaxc_include = 0;		/* 1 for token without punctuation */
#endif
  int pcf = -1;
  char *pcfbuf;
  const char *pcfbuflimit;
  int pcfnum;

  if (pedantic && !instack[indepth].system_header_p)
    {
      if (importing)
	pedwarn ("ANSI C does not allow `#import'");
      if (skip_dirs)
	pedwarn ("ANSI C does not allow `#include_next'");
    }

  if (importing && warn_import && !inhibit_warnings
      && !instack[indepth].system_header_p && !import_warning) {
    import_warning = 1;
    warning ("using `#import' is not recommended");
    notice ("The fact that a certain header file need not be processed more than once\n\
should be indicated in the header file, not where it is used.\n\
The best way to do this is with a conditional of this form:\n\
\n\
  #ifndef _FOO_H_INCLUDED\n\
  #define _FOO_H_INCLUDED\n\
  ... <real contents of file> ...\n\
  #endif /* Not _FOO_H_INCLUDED */\n\
\n\
Then users can use `#include' any number of times.\n\
GNU C automatically avoids processing the file more than once\n\
when it is equipped with such a conditional.\n");
  }

get_filename:

  fin = buf;
  SKIP_WHITE_SPACE (fin);
  /* Discard trailing whitespace so we can easily see
     if we have parsed all the significant chars we were given.  */
  while (limit != fin && is_hor_space[limit[-1]]) limit--;
  fbeg = fend = (char *) alloca (limit - fin);

  switch (*fin++) {
  case '\"':
    {
      FILE_BUF *fp;
      /* Copy the operand text, concatenating the strings.  */
      {
	for (;;) {
	  for (;;) {
	    if (fin == limit)
	      goto invalid_include_file_name;
	    *fend = *fin++;
	    if (*fend == '"')
	      break;
	    fend++;
	  }
	  if (fin == limit)
	    break;
	  /* If not at the end, there had better be another string.  */
	  /* Skip just horiz space, and don't go past limit.  */
	  while (fin != limit && is_hor_space[*fin]) fin++;
	  if (fin != limit && *fin == '\"')
	    fin++;
	  else
	    goto fail;
	}
      }

      /* We have "filename".  Figure out directory this source
	 file is coming from and put it on the front of the list.  */

      /* If -I- was specified, don't search current dir, only spec'd ones.  */
      if (ignore_srcdir) break;

      for (fp = &instack[indepth]; fp >= instack; fp--)
	{
	  int n;

	  if ((fp->nominal_fname) != NULL) {
	    char *nam;
	    /* Found a named file.  Figure out dir of the file,
	       and put it in front of the search list.  */
	    dsp = ((struct file_name_list *)
		   alloca (sizeof (struct file_name_list)
			   + fp->nominal_fname_len));
	    strcpy (dsp->fname, fp->nominal_fname);
	    simplify_filename (dsp->fname);
	    nam = base_name (dsp->fname);
	    *nam = 0;
#ifdef VMS
	    /* for hack_vms_include_specification(), a local
	       dir specification must start with "./" on VMS.  */
	    if (nam == dsp->fname)
	      {    
		*nam++ = '.';
		*nam++ = '/';
		*nam = 0;
	      }
#endif
	    /* But for efficiency's sake, do not insert the dir
	       if it matches the search list's first dir.  */
	    dsp->next = search_start;
	    if (!search_start || strcmp (dsp->fname, search_start->fname)) {
	      search_start = dsp;
	      n = nam - dsp->fname;
	      if (n + INCLUDE_LEN_FUDGE > max_include_len)
		max_include_len = n + INCLUDE_LEN_FUDGE;
	    }
	    dsp[0].got_name_map = 0;
	    break;
	  }
	}
      break;
    }

  case '<':
    while (fin != limit && *fin != '>')
      *fend++ = *fin++;
    if (*fin == '>' && fin + 1 == limit) {
      angle_brackets = 1;
      /* If -I-, start with the first -I dir after the -I-.  */
      search_start = first_bracket_include;
      break;
    }
    goto fail;

  default:
#ifdef VMS
    /*
     * Support '#include xyz' like VAX-C to allow for easy use of all the
     * decwindow include files. It defaults to '#include <xyz.h>' (so the
     * code from case '<' is repeated here) and generates a warning.
     * (Note: macro expansion of `xyz' takes precedence.)
     */
    /* Note: The argument of ISALPHA() can be evaluated twice, so do
       the pre-decrement outside of the macro. */
    if (retried && (--fin, ISALPHA(*(U_CHAR *) (fin)))) {
      while (fin != limit && (!ISSPACE(*fin)))
	*fend++ = *fin++;
      warning ("VAX-C-style include specification found, use '#include <filename.h>' !");
      vaxc_include = 1;
      if (fin == limit) {
	angle_brackets = 1;
	/* If -I-, start with the first -I dir after the -I-.  */
	search_start = first_bracket_include;
	break;
      }
    }
#endif

  fail:
    if (! retried) {
      /* Expand buffer and then remove any newline markers.
	 We can't just tell expand_to_temp_buffer to omit the markers,
	 since it would put extra spaces in include file names.  */
      U_CHAR *src;
      int errors_before_expansion = errors;
      FILE_BUF trybuf;

      trybuf = expand_to_temp_buffer (buf, limit, 1, 0);
      if (errors != errors_before_expansion) {
	free (trybuf.buf);
	goto invalid_include_file_name;
      }
      src = trybuf.buf;
      buf = (U_CHAR *) alloca (trybuf.bufp - trybuf.buf + 1);
      limit = buf;
      while (src != trybuf.bufp) {
	switch ((*limit++ = *src++)) {
	  case '\n':
	    limit--;
	    src++;
	    break;

	  case '\'':
	  case '\"':
	    {
	      const U_CHAR *src1 = skip_quoted_string (src - 1, trybuf.bufp, 0,
						 NULL_PTR, NULL_PTR, NULL_PTR);
	      while (src != src1)
		*limit++ = *src++;
	    }
	    break;
	}
      }
      *limit = 0;
      free (trybuf.buf);
      retried = 1;
      goto get_filename;
    }

  invalid_include_file_name:
    error ("`#%s' expects \"FILENAME\" or <FILENAME>", keyword->name);
    return 0;
  }

  /* For #include_next, skip in the search path
     past the dir in which the containing file was found.  */
  if (skip_dirs) {
    FILE_BUF *fp;
    for (fp = &instack[indepth]; fp >= instack; fp--)
      if (fp->fname != NULL) {
	/* fp->dir is null if the containing file was specified
	   with an absolute file name.  In that case, don't skip anything.  */
	if (fp->dir)
	  search_start = fp->dir->next;
	break;
      }
  }

  *fend = 0;
  flen = simplify_filename (fbeg);

  if (flen == 0)
    {
      error ("empty file name in `#%s'", keyword->name);
      return 0;
    }

  /* Allocate this permanently, because it gets stored in the definitions
     of macros.  */
  fname = xmalloc (max_include_len + flen + 1);
  /* + 1 above for terminating null.  */

  system_include_depth += angle_brackets;

  /* If specified file name is absolute, just open it.  */

  if (absolute_filename (fbeg)) {
    strcpy (fname, fbeg);
    f = open_include_file (fname, NULL_PTR, importing, &inc);
  } else {

    struct bypass_dir {
      struct bypass_dir *next;
      char *fname;
      struct file_name_list *searchptr;
    } **bypass_slot = 0;

    /* Search directory path, trying to open the file.
       Copy each filename tried into FNAME.  */

    for (searchptr = search_start; searchptr; searchptr = searchptr->next) {

      if (searchptr == first_bracket_include) {
	/* Go to bypass directory if we know we've seen this file before.  */
	static struct bypass_dir *bypass_hashtab[INCLUDE_HASHSIZE];
	struct bypass_dir *p;
	bypass_slot = &bypass_hashtab[hashf ((U_CHAR *) fbeg, flen,
					     INCLUDE_HASHSIZE)];
	for (p = *bypass_slot; p; p = p->next)
	  if (!strcmp (fbeg, p->fname)) {
	    searchptr = p->searchptr;
	    bypass_slot = 0;
	    break;
	  }
      }

#ifdef VMS
      /* Change this 1/2 Unix 1/2 VMS file specification into a
         full VMS file specification */
      if (searchptr->fname[0])
	{
	  strcpy (fname, searchptr->fname);
	  if (fname[strlen (fname) - 1] == ':')
	    {
	      char *slashp;
	      slashp = strchr (fbeg, '/');

	      /* start at root-dir of logical device if no path given.  */
	      if (slashp == 0)
		strcat (fname, "[000000]");
	    }
	  strcat (fname, fbeg);

	  /* Fix up the filename */
	  hack_vms_include_specification (fname, vaxc_include);
	}
      else
	{
	  /* This is a normal VMS filespec, so use it unchanged.  */
	  strcpy (fname, fbeg);
	  /* if it's '#include filename', add the missing .h */
	  if (vaxc_include && index(fname,'.')==NULL)
	    strcat (fname, ".h");
	}
#else
      strcpy (fname, searchptr->fname);
      strcat (fname, fbeg);
#endif /* VMS */
      f = open_include_file (fname, searchptr, importing, &inc);
      if (f != -1) {
	if (bypass_slot && searchptr != first_bracket_include) {
	  /* This is the first time we found this include file,
	     and we found it after first_bracket_include.
	     Record its location so that we can bypass to here next time.  */
	  struct bypass_dir *p
	    = (struct bypass_dir *) xmalloc (sizeof (struct bypass_dir));
	  p->next = *bypass_slot;
	  p->fname = fname + strlen (searchptr->fname);
	  p->searchptr = searchptr;
	  *bypass_slot = p;
	}
	break;
      }
#ifdef VMS
      /* Our VMS hacks can produce invalid filespecs, so don't worry
	 about errors other than EACCES.  */
      if (errno == EACCES)
	break;
#else
      if (errno != ENOENT && errno != ENOTDIR)
	break;
#endif
    }
  }


  if (f < 0) {

    if (f == -2) {
      /* The file was already included.  */

    /* If generating dependencies and -MG was specified, we assume missing
       files are leaf files, living in the same directory as the source file
       or other similar place; these missing files may be generated from
       other files and may not exist yet (eg: y.tab.h).  */
    } else if (print_deps_missing_files
	       && (system_include_depth != 0) < print_deps)
      {
	/* If it was requested as a system header file,
	   then assume it belongs in the first place to look for such.  */
	if (angle_brackets)
	  {
	    if (search_start) {
	      char *p = (char *) alloca (strlen (search_start->fname)
					 + strlen (fbeg) + 1);
	      strcpy (p, search_start->fname);
	      strcat (p, fbeg);
	      deps_output (p, ' ');
	    }
	  }
	else
	  {
	    /* Otherwise, omit the directory, as if the file existed
	       in the directory with the source.  */
	    deps_output (fbeg, ' ');
	  }
      }
    /* If -M was specified, and this header file won't be added to the
       dependency list, then don't count this as an error, because we can
       still produce correct output.  Otherwise, we can't produce correct
       output, because there may be dependencies we need inside the missing
       file, and we don't know what directory this missing file exists in.  */
    else if (0 < print_deps  &&  print_deps <= (system_include_depth != 0))
      warning ("No include path in which to find %s", fbeg);
    else if (f != -3)
      error_from_errno (fbeg);
    else
      error ("No include path in which to find %s", fbeg);

  } else {

    /* Actually process the file.  */

    pcftry = (char *) alloca (strlen (fname) + 30);
    pcfbuf = 0;
    pcfnum = 0;

    if (!no_precomp)
      {
	do {
	  sprintf (pcftry, "%s%d", fname, pcfnum++);

	  pcf = open (pcftry, O_RDONLY, 0666);
	  if (pcf != -1)
	    {
	      struct stat s;

	      if (fstat (pcf, &s) != 0)
		pfatal_with_name (pcftry);
	      if (! INO_T_EQ (inc->st.st_ino, s.st_ino)
		  || inc->st.st_dev != s.st_dev)
		{
		  pcfbuf = check_precompiled (pcf, &s, fname, &pcfbuflimit);
		  /* Don't need it any more.  */
		  close (pcf);
		}
	      else
		{
		  /* Don't need it at all.  */
		  close (pcf);
		  break;
		}
	    }
	} while (pcf != -1 && !pcfbuf);
      }
    
    /* Actually process the file */
    if (pcfbuf) {
      pcfname = xstrdup (pcftry);
      pcfinclude ((U_CHAR *) pcfbuf, (U_CHAR *) fname, op);
    }
    else
      finclude (f, inc, op, is_system_include (fname), searchptr);
  }

  system_include_depth -= angle_brackets;

  return 0;
}

/* Return nonzero if the given FILENAME is an absolute pathname which
   designates a file within one of the known "system" include file
   directories.  We assume here that if the given FILENAME looks like
   it is the name of a file which resides either directly in a "system"
   include file directory, or within any subdirectory thereof, then the
   given file must be a "system" include file.  This function tells us
   if we should suppress pedantic errors/warnings for the given FILENAME.

   The value is 2 if the file is a C-language system header file
   for which C++ should (on most systems) assume `extern "C"'.  */

static int
is_system_include (filename)
    register const char *filename;
{
  struct file_name_list *searchptr;

  for (searchptr = first_system_include; searchptr;
       searchptr = searchptr->next)
    if (! strncmp (searchptr->fname, filename, strlen (searchptr->fname)))
      return searchptr->c_system_include_path + 1;
  return 0;
}

/* Yield the non-directory suffix of a file name.  */

static char *
base_name (fname)
     const char *fname;
{
  const char *s = fname;
  const char *p;
#if defined (__MSDOS__) || defined (_WIN32)
  if (ISALPHA (s[0]) && s[1] == ':') s += 2;
#endif
#ifdef VMS
  if ((p = rindex (s, ':'))) s = p + 1;	/* Skip device.  */
  if ((p = rindex (s, ']'))) s = p + 1;	/* Skip directory.  */
  if ((p = rindex (s, '>'))) s = p + 1;	/* Skip alternate (int'n'l) dir.  */
  if (s != fname)
    return (char *) s;
#endif
  if ((p = rindex (s, '/'))) s = p + 1;
#ifdef DIR_SEPARATOR
  if ((p = rindex (s, DIR_SEPARATOR))) s = p + 1;
#endif
  return (char *) s;
}

/* Yield nonzero if FILENAME is absolute (i.e. not relative).  */

static int
absolute_filename (filename)
     const char *filename;
{
#if defined (__MSDOS__) \
  || (defined (_WIN32) && !defined (__CYGWIN__) && !defined (_UWIN))
  if (ISALPHA (filename[0]) && filename[1] == ':') filename += 2;
#endif
#if defined (__CYGWIN__)
  /* At present, any path that begins with a drive spec is absolute.  */
  if (ISALPHA (filename[0]) && filename[1] == ':') return 1;
#endif
#ifdef VMS
  if (index (filename, ':') != 0) return 1;
#endif
  if (filename[0] == '/') return 1;
#ifdef DIR_SEPARATOR
  if (filename[0] == DIR_SEPARATOR) return 1;
#endif
  return 0;
}

/* Returns whether or not a given character is a directory separator.
   Used by simplify_filename.  */
static inline int is_dir_separator PARAMS ((int));

static inline
int
is_dir_separator(ch)
     char ch;
{
  return (ch == DIR_SEPARATOR)
#if defined (DIR_SEPARATOR_2)
          || (ch == DIR_SEPARATOR_2)
#endif
         ;
}

/* Remove unnecessary characters from FILENAME in place,
   to avoid unnecessary filename aliasing.
   Return the length of the resulting string.

   Do only the simplifications allowed by Posix.
   It is OK to miss simplifications on non-Posix hosts,
   since this merely leads to suboptimal results.  */

static size_t
simplify_filename (filename)
     char *filename;
{
  register char *from = filename;
  register char *to = filename;
  char *to0;

  /* Remove redundant initial /s.  */
  if (is_dir_separator (*from))
    {
      *to++ = DIR_SEPARATOR;
      if (is_dir_separator (*++from))
        {
          if (is_dir_separator (*++from))
            {
              /* 3 or more initial /s are equivalent to 1 /.  */
              while (is_dir_separator (*++from))
                continue;
            }
          else
            {
	      /* On some hosts // differs from /; Posix allows this.  */
	      *to++ = DIR_SEPARATOR;
            }
        }
    }

  to0 = to;

  for (;;)
    {
#ifndef VMS
      if (from[0] == '.' && from[1] == '/')
        from += 2;
      else
#endif
        {
          /* Copy this component and trailing DIR_SEPARATOR, if any.  */
          while (!is_dir_separator (*to++ = *from++))
            {
              if (!to[-1])
                {
                  /* Trim . component at end of nonempty name.  */
                  to -= filename <= to - 3 && to[-3] == DIR_SEPARATOR && to[-2] == '.';

                  /* Trim unnecessary trailing /s.  */
                  while (to0 < --to && to[-1] == DIR_SEPARATOR)
                    continue;

                  *to = 0;
                  return to - filename;
                }
            }
#if defined(DIR_SEPARATOR_2)
          /* Simplify to one directory separator.  */
          to[-1] = DIR_SEPARATOR;
#endif
        }

    /* Skip /s after a /.  */
    while (is_dir_separator (*from))
      from++;
  }
}

/* The file_name_map structure holds a mapping of file names for a
   particular directory.  This mapping is read from the file named
   FILE_NAME_MAP_FILE in that directory.  Such a file can be used to
   map filenames on a file system with severe filename restrictions,
   such as DOS.  The format of the file name map file is just a series
   of lines with two tokens on each line.  The first token is the name
   to map, and the second token is the actual name to use.  */

struct file_name_map
{
  struct file_name_map *map_next;
  char *map_from;
  char *map_to;
};

#define FILE_NAME_MAP_FILE "header.gcc"

/* Read a space delimited string of unlimited length from a stdio
   file.  */

static char *
read_filename_string (ch, f)
     int ch;
     FILE *f;
{
  char *alloc, *set;
  int len;

  len = 20;
  set = alloc = xmalloc (len + 1);
  if (! is_space[ch])
    {
      *set++ = ch;
      while ((ch = getc (f)) != EOF && ! is_space[ch])
	{
	  if (set - alloc == len)
	    {
	      len *= 2;
	      alloc = xrealloc (alloc, len + 1);
	      set = alloc + len / 2;
	    }
	  *set++ = ch;
	}
    }
  *set = '\0';
  ungetc (ch, f);
  return alloc;
}

/* Read the file name map file for DIRNAME.
   If DIRNAME is empty, read the map file for the working directory;
   otherwise DIRNAME must end in '/'.  */

static struct file_name_map *
read_name_map (dirname)
     const char *dirname;
{
  /* This structure holds a linked list of file name maps, one per
     directory.  */
  struct file_name_map_list
    {
      struct file_name_map_list *map_list_next;
      char *map_list_name;
      struct file_name_map *map_list_map;
    };
  static struct file_name_map_list *map_list;
  register struct file_name_map_list *map_list_ptr;
  char *name;
  FILE *f;
  size_t dirlen;

  for (map_list_ptr = map_list; map_list_ptr;
       map_list_ptr = map_list_ptr->map_list_next)
    if (! strcmp (map_list_ptr->map_list_name, dirname))
      return map_list_ptr->map_list_map;

  map_list_ptr = ((struct file_name_map_list *)
		  xmalloc (sizeof (struct file_name_map_list)));
  map_list_ptr->map_list_name = xstrdup (dirname);
  map_list_ptr->map_list_map = NULL;

  dirlen = strlen (dirname);
  name = (char *) alloca (dirlen + strlen (FILE_NAME_MAP_FILE) + 1);
  strcpy (name, dirname);
  strcat (name, FILE_NAME_MAP_FILE);
  f = fopen (name, "r");
  if (!f)
    map_list_ptr->map_list_map = NULL;
  else
    {
      int ch;

      while ((ch = getc (f)) != EOF)
	{
	  char *from, *to;
	  struct file_name_map *ptr;
	  size_t tolen;

	  if (is_space[ch])
	    continue;
	  from = read_filename_string (ch, f);
	  while ((ch = getc (f)) != EOF && is_hor_space[ch])
	    ;
	  to = read_filename_string (ch, f);

	  simplify_filename (from);
	  tolen = simplify_filename (to);

	  ptr = ((struct file_name_map *)
		 xmalloc (sizeof (struct file_name_map)));
	  ptr->map_from = from;

	  /* Make the real filename absolute.  */
	  if (absolute_filename (to))
	    ptr->map_to = to;
	  else
	    {
	      ptr->map_to = xmalloc (dirlen + tolen + 1);
	      strcpy (ptr->map_to, dirname);
	      strcat (ptr->map_to, to);
	      free (to);
	    }	      

	  ptr->map_next = map_list_ptr->map_list_map;
	  map_list_ptr->map_list_map = ptr;

	  while ((ch = getc (f)) != '\n')
	    if (ch == EOF)
	      break;
	}
      fclose (f);
    }
  
  map_list_ptr->map_list_next = map_list;
  map_list = map_list_ptr;

  return map_list_ptr->map_list_map;
}  

/* Try to open include file FILENAME.  SEARCHPTR is the directory
   being tried from the include file search path.
   IMPORTING is "" if we are importing, null otherwise.
   Return -2 if found, either a matching name or a matching inode.
   Otherwise, open the file and return a file descriptor if successful
   or -1 if unsuccessful.
   Unless unsuccessful, put a descriptor of the included file into *PINC.
   This function maps filenames on file systems based on information read by
   read_name_map.  */

static int
open_include_file (filename, searchptr, importing, pinc)
     char *filename;
     struct file_name_list *searchptr;
     const U_CHAR *importing;
     struct include_file **pinc;
{
  char *fname = remap ? remap_include_file (filename, searchptr) : filename;
  int fd = -2;

  /* Look up FNAME in include_hashtab.  */
  struct include_file **phead = &include_hashtab[hashf ((U_CHAR *) fname,
							strlen (fname),
							INCLUDE_HASHSIZE)];
  struct include_file *inc, *head = *phead;
  for (inc = head; inc; inc = inc->next)
    if (!strcmp (fname, inc->fname))
      break;

  if (!inc
      || ! inc->control_macro
      || (inc->control_macro[0] && ! lookup (inc->control_macro, -1, -1))) {

    fd = open (fname, O_RDONLY, 0);

    if (fd < 0)
      {
#ifdef VMS
	/* if #include <dir/file> fails, try again with hacked spec.  */
	if (!hack_vms_include_specification (fname, 0))
	  return fd;
	fd = open (fname, O_RDONLY, 0);
	if (fd < 0)
#endif
	  return fd;
      }

    if (!inc) {
      /* FNAME was not in include_hashtab; insert a new entry.  */
      inc = (struct include_file *) xmalloc (sizeof (struct include_file));
      inc->next = head;
      inc->fname = fname;
      inc->control_macro = 0;
      inc->deps_output = 0;
      if (fstat (fd, &inc->st) != 0)
	pfatal_with_name (fname);
      *phead = inc;

      /* Look for another file with the same inode and device.  */
      if (lookup_ino_include (inc)
	  && inc->control_macro
	  && (!inc->control_macro[0] || lookup (inc->control_macro, -1, -1))) {
	close (fd);
	fd = -2;
      }
    }

    /* For -M, add this file to the dependencies.  */
    if (! inc->deps_output  &&  (system_include_depth != 0) < print_deps) {
      inc->deps_output = 1;
      deps_output (fname, ' ');
    }   

    /* Handle -H option.  */
    if (print_include_names)
      fprintf (stderr, "%*s%s\n", indepth, "", fname);
  }

  if (importing)
    inc->control_macro = importing;

  *pinc = inc;
  return fd;
}

/* Return the remapped name of the include file FILENAME.
   SEARCHPTR is the directory being tried from the include file path.  */

static char *
remap_include_file (filename, searchptr)
     char *filename;
     struct file_name_list *searchptr;
{
  register struct file_name_map *map;
  register const char *from;

  if (searchptr)
    {
      if (! searchptr->got_name_map)
	{
	  searchptr->name_map = read_name_map (searchptr->fname);
	  searchptr->got_name_map = 1;
	}

      /* Check the mapping for the directory we are using.  */
      from = filename + strlen (searchptr->fname);
      for (map = searchptr->name_map; map; map = map->map_next)
	if (! strcmp (map->map_from, from))
	  return map->map_to;
    }

  from = base_name (filename);

  if (from != filename || !searchptr)
    {
      /* Try to find a mapping file for the particular directory we are
	 looking in.  Thus #include <sys/types.h> will look up sys/types.h
	 in /usr/include/header.gcc and look up types.h in
	 /usr/include/sys/header.gcc.  */

      char *dir = (char *) alloca (from - filename + 1);
      bcopy (filename, dir, from - filename);
      dir[from - filename] = '\0';

      for (map = read_name_map (dir); map; map = map->map_next)
	if (! strcmp (map->map_from, from))
	  return map->map_to;
    }

  return filename;
}

/* Insert INC into the include file table, hashed by device and inode number.
   If a file with different name but same dev+ino was already in the table,
   return 1 and set INC's control macro to the already-known macro.  */

static int
lookup_ino_include (inc)
     struct include_file *inc;
{
  int hash = ((unsigned) (inc->st.st_dev + INO_T_HASH (inc->st.st_ino))
	      % INCLUDE_HASHSIZE);
  struct include_file *i = include_ino_hashtab[hash];
  inc->next_ino = i;
  include_ino_hashtab[hash] = inc;

  for (; i; i = i->next_ino)
    if (INO_T_EQ (inc->st.st_ino, i->st.st_ino)
	&& inc->st.st_dev == i->st.st_dev) {
      inc->control_macro = i->control_macro;
      return 1;
    }

  return 0;
}

/* Process file descriptor F, which corresponds to include file INC,
   with output to OP.
   SYSTEM_HEADER_P is 1 if this file resides in any one of the known
   "system" include directories (as decided by the `is_system_include'
   function above).
   DIRPTR is the link in the dir path through which this file was found,
   or 0 if the file name was absolute.  */

static void
finclude (f, inc, op, system_header_p, dirptr)
     int f;
     struct include_file *inc;
     FILE_BUF *op;
     int system_header_p;
     struct file_name_list *dirptr;
{
  char *fname = inc->fname;
  int i;
  FILE_BUF *fp;			/* For input stack frame */
  int missing_newline = 0;

  CHECK_DEPTH (return;);

  fp = &instack[indepth + 1];
  bzero ((char *) fp, sizeof (FILE_BUF));
  fp->nominal_fname = fp->fname = fname;
  fp->nominal_fname_len = strlen (fname);
  fp->inc = inc;
  fp->length = 0;
  fp->lineno = 1;
  fp->if_stack = if_stack;
  fp->system_header_p = system_header_p;
  fp->dir = dirptr;

  if (S_ISREG (inc->st.st_mode)) {
    size_t s = (size_t) inc->st.st_size;
    if (s != inc->st.st_size || s + 2 < s)
      memory_full ();
    fp->buf = (U_CHAR *) xmalloc (s + 2);
    fp->bufp = fp->buf;

    /* Read the file contents, knowing that s is an upper bound
       on the number of bytes we can read.  */
    fp->length = safe_read (f, (char *) fp->buf, s);
    if (fp->length < 0) goto nope;
  }
  else if (S_ISDIR (inc->st.st_mode)) {
    error ("directory `%s' specified in #include", fname);
    close (f);
    return;
  } else {
    /* Cannot count its file size before reading.
       First read the entire file into heap and
       copy them into buffer on stack.  */

    int bsize = 2000;
    int st_size = 0;

    fp->buf = (U_CHAR *) xmalloc (bsize + 2);

    for (;;) {
      i = safe_read (f, (char *) fp->buf + st_size, bsize - st_size);
      if (i < 0)
	goto nope;      /* error! */
      st_size += i;
      if (st_size != bsize)
	break;	/* End of file */
      bsize *= 2;
      fp->buf = (U_CHAR *) xrealloc (fp->buf, bsize + 2);
    }
    fp->bufp = fp->buf;
    fp->length = st_size;
  }

  if ((fp->length > 0 && fp->buf[fp->length - 1] != '\n')
      /* Backslash-newline at end is not good enough.  */
      || (fp->length > 1 && fp->buf[fp->length - 2] == '\\')) {
    fp->buf[fp->length++] = '\n';
    missing_newline = 1;
  }
  fp->buf[fp->length] = '\0';

  /* Close descriptor now, so nesting does not use lots of descriptors.  */
  close (f);

  /* Must do this before calling trigraph_pcp, so that the correct file name
     will be printed in warning messages.  */

  indepth++;
  input_file_stack_tick++;

  if (!no_trigraphs)
    trigraph_pcp (fp);

  if (warn_white_space)
    check_white_space (fp);

  output_line_directive (fp, op, 0, enter_file);
  rescan (op, 0);

  if (missing_newline)
    fp->lineno--;

  if (pedantic && missing_newline)
    pedwarn ("file does not end in newline");

  indepth--;
  input_file_stack_tick++;
  output_line_directive (&instack[indepth], op, 0, leave_file);
  free (fp->buf);
  return;

 nope:

  perror_with_name (fname);
  close (f);
  free (fp->buf);
}

/* Record that inclusion of the include file INC
   should be controlled by the macro named MACRO_NAME.
   This means that trying to include the file again
   will do something if that macro is defined.  */

static void
record_control_macro (inc, macro_name)
     struct include_file *inc;
     const U_CHAR *macro_name;
{
  if (!inc->control_macro || inc->control_macro[0])
    inc->control_macro = macro_name;
}

/* Load the specified precompiled header into core, and verify its
   preconditions.  PCF indicates the file descriptor to read, which must
   be a regular file.  *ST is its file status.
   FNAME indicates the file name of the original header.
   *LIMIT will be set to an address one past the end of the file.
   If the preconditions of the file are not satisfied, the buffer is 
   freed and we return 0.  If the preconditions are satisfied, return
   the address of the buffer following the preconditions.  The buffer, in
   this case, should never be freed because various pieces of it will
   be referred to until all precompiled strings are output at the end of
   the run.  */

static char *
check_precompiled (pcf, st, fname, limit)
     int pcf;
     struct stat *st;
     const char *fname ATTRIBUTE_UNUSED;
     const char **limit;
{
  int length = 0;
  char *buf;
  char *cp;

  if (pcp_outfile)
    return 0;

  if (S_ISREG (st->st_mode))
    {
      size_t s = (size_t) st->st_size;
      if (s != st->st_size || s + 2 < s)
	memory_full ();
      buf = xmalloc (s + 2);
      length = safe_read (pcf, buf, s);
      if (length < 0)
	goto nope;
    }
  else
    abort ();
    
  if (length > 0 && buf[length-1] != '\n')
    buf[length++] = '\n';
  buf[length] = '\0';
  
  *limit = buf + length;

  /* File is in core.  Check the preconditions.  */
  if (!check_preconditions (buf))
    goto nope;
  for (cp = buf; *cp; cp++)
    ;
#ifdef DEBUG_PCP
  fprintf (stderr, "Using preinclude %s\n", fname);
#endif
  return cp + 1;

 nope:
#ifdef DEBUG_PCP
  fprintf (stderr, "Cannot use preinclude %s\n", fname);
#endif
  free (buf);
  return 0;
}

/* PREC (null terminated) points to the preconditions of a
   precompiled header.  These are a series of #define and #undef
   lines which must match the current contents of the hash
   table.  */

static int 
check_preconditions (prec)
     const char *prec;
{
  MACRODEF mdef;
  const char *lineend;
  
  while (*prec) {
    lineend = index (prec, '\n');
    
    if (*prec++ != '#') {
      error ("Bad format encountered while reading precompiled file");
      return 0;
    }
    if (!strncmp (prec, "define", 6)) {
      HASHNODE *hp;
      
      prec += 6;
      mdef = create_definition ((const U_CHAR *) prec,
				(const U_CHAR *) lineend, NULL_PTR);

      if (mdef.defn == 0)
	abort ();
      
      if ((hp = lookup (mdef.symnam, mdef.symlen, -1)) == NULL
	  || (hp->type != T_MACRO && hp->type != T_CONST)
	  || (hp->type == T_MACRO
	      && !compare_defs (mdef.defn, hp->value.defn)
	      && (mdef.defn->length != 2
		  || mdef.defn->expansion[0] != '\n'
		  || mdef.defn->expansion[1] != ' ')))
	return 0;
    } else if (!strncmp (prec, "undef", 5)) {
      const char *name;
      int len;
      
      prec += 5;
      while (is_hor_space[(U_CHAR) *prec])
	prec++;
      name = prec;
      while (is_idchar[(U_CHAR) *prec])
	prec++;
      len = prec - name;
      
      if (lookup ((const U_CHAR *) name, len, -1))
	return 0;
    } else {
      error ("Bad format encountered while reading precompiled file");
      return 0;
    }
    prec = lineend + 1;
  }
  /* They all passed successfully */
  return 1;
}

/* Process the main body of a precompiled file.  BUF points to the
   string section of the file, following the preconditions.  LIMIT is one
   character past the end.  NAME is the name of the file being read
   in.  OP is the main output buffer.  */

static void
pcfinclude (buf, name, op)
     U_CHAR *buf;
     const U_CHAR *name;
     FILE_BUF *op;
{
  FILE_BUF tmpbuf;
  int nstrings;
  U_CHAR *cp = buf;

  /* First in the file comes 4 bytes indicating the number of strings, */
  /* in network byte order. (MSB first).  */
  nstrings = *cp++;
  nstrings = (nstrings << 8) | *cp++;
  nstrings = (nstrings << 8) | *cp++;
  nstrings = (nstrings << 8) | *cp++;
  
  /* Looping over each string...  */
  while (nstrings--) {
    U_CHAR *string_start;
    U_CHAR *endofthiskey;
    STRINGDEF *str;
    int nkeys;
    
    /* Each string starts with a STRINGDEF structure (str), followed */
    /* by the text of the string (string_start) */

    /* First skip to a longword boundary */
    /* ??? Why a 4-byte boundary?  On all machines? */
    /* NOTE: This works correctly even if size_t
       is narrower than a pointer.
       Do not try risky measures here to get another type to use!
       Do not include stddef.h--it will fail!  */
    if ((size_t) cp & 3)
      cp += 4 - ((size_t) cp & 3);
    
    /* Now get the string.  */
    str = (STRINGDEF *) (PTR) cp;
    string_start = cp += sizeof (STRINGDEF);
    
    for (; *cp; cp++)		/* skip the string */
      ;
    
    /* We need to macro expand the string here to ensure that the
       proper definition environment is in place.  If it were only
       expanded when we find out it is needed, macros necessary for
       its proper expansion might have had their definitions changed.  */
    tmpbuf = expand_to_temp_buffer (string_start, cp++, 0, 0);
    /* Lineno is already set in the precompiled file */
    str->contents = tmpbuf.buf;
    str->len = tmpbuf.bufp - tmpbuf.buf;
    str->writeflag = 0;
    str->filename = name;
    str->output_mark = outbuf.bufp - outbuf.buf;
    
    str->chain = 0;
    *stringlist_tailp = str;
    stringlist_tailp = &str->chain;
    
    /* Next comes a fourbyte number indicating the number of keys
       for this string.  */
    nkeys = *cp++;
    nkeys = (nkeys << 8) | *cp++;
    nkeys = (nkeys << 8) | *cp++;
    nkeys = (nkeys << 8) | *cp++;

    /* If this number is -1, then the string is mandatory.  */
    if (nkeys == -1)
      str->writeflag = 1;
    else
      /* Otherwise, for each key, */
      for (; nkeys--; free (tmpbuf.buf), cp = endofthiskey + 1) {
	KEYDEF *kp = (KEYDEF *) (PTR) cp;
	HASHNODE *hp;
	U_CHAR *bp;
	
	/* It starts with a KEYDEF structure */
	cp += sizeof (KEYDEF);
	
	/* Find the end of the key.  At the end of this for loop we
	   advance CP to the start of the next key using this variable.  */
	endofthiskey = cp + strlen ((char *) cp);
	kp->str = str;
	
	/* Expand the key, and enter it into the hash table.  */
	tmpbuf = expand_to_temp_buffer (cp, endofthiskey, 0, 0);
	bp = tmpbuf.buf;
	
	while (is_hor_space[*bp])
	  bp++;
	if (!is_idstart[*bp] || bp == tmpbuf.bufp) {
	  str->writeflag = 1;
	  continue;
	}
	    
	hp = lookup (bp, -1, -1);
	if (hp == NULL) {
	  kp->chain = 0;
	  install (bp, -1, T_PCSTRING, (char *) kp, -1);
	}
	else if (hp->type == T_PCSTRING) {
	  kp->chain = hp->value.keydef;
	  hp->value.keydef = kp;
	}
	else
	  str->writeflag = 1;
      }
  }
  /* This output_line_directive serves to switch us back to the current
     input file in case some of these strings get output (which will 
     result in line directives for the header file being output).   */
  output_line_directive (&instack[indepth], op, 0, enter_file);
}

/* Called from rescan when it hits a key for strings.  Mark them all
   used and clean up.  */

static void
pcstring_used (hp)
     HASHNODE *hp;
{
  KEYDEF *kp;
  
  for (kp = hp->value.keydef; kp; kp = kp->chain)
    kp->str->writeflag = 1;
  delete_macro (hp);
}

/* Write the output, interspersing precompiled strings in their
   appropriate places.  */

static void
write_output ()
{
  STRINGDEF *next_string;
  U_CHAR *cur_buf_loc;
  int line_directive_len = 80;
  char *line_directive = xmalloc (line_directive_len);
  int len;

  /* In each run through the loop, either cur_buf_loc ==
     next_string_loc, in which case we print a series of strings, or
     it is less than next_string_loc, in which case we write some of
     the buffer.  */
  cur_buf_loc = outbuf.buf; 
  next_string = stringlist;
  
  while (cur_buf_loc < outbuf.bufp || next_string) {
    if (next_string
	&& cur_buf_loc - outbuf.buf == next_string->output_mark) {
      if (next_string->writeflag) {
	len = 4 * strlen ((const char *) next_string->filename) + 32;
	while (len > line_directive_len)
	  line_directive = xrealloc (line_directive, 
				     line_directive_len *= 2);
	sprintf (line_directive, "\n# %d ", next_string->lineno);
	strcpy (quote_string (line_directive + strlen (line_directive),
			      (const char *) next_string->filename,
			      strlen ((const char *) next_string->filename)),
		"\n");
	safe_write (fileno (stdout), line_directive, strlen (line_directive));
	safe_write (fileno (stdout),
		    (char *) next_string->contents, next_string->len);
      }	      
      next_string = next_string->chain;
    }
    else {
      len = (next_string
	     ? (next_string->output_mark 
		- (cur_buf_loc - outbuf.buf))
	     : outbuf.bufp - cur_buf_loc);
      
      safe_write (fileno (stdout), (char *) cur_buf_loc, len);
      cur_buf_loc += len;
    }
  }
  free (line_directive);
}

/* Pass a directive through to the output file.
   BUF points to the contents of the directive, as a contiguous string.
   LIMIT points to the first character past the end of the directive.
   KEYWORD is the keyword-table entry for the directive.  */

static void
pass_thru_directive (buf, limit, op, keyword)
     const U_CHAR *buf;
     const U_CHAR *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  register int keyword_length = keyword->length;

  check_expand (op, 1 + keyword_length + (limit - buf));
  *op->bufp++ = '#';
  bcopy (keyword->name, (char *) op->bufp, keyword_length);
  op->bufp += keyword_length;
  if (limit != buf && buf[0] != ' ')
    *op->bufp++ = ' ';
  bcopy ((const PTR) buf, (PTR) op->bufp, limit - buf);
  op->bufp += (limit - buf);
#if 0
  *op->bufp++ = '\n';
  /* Count the line we have just made in the output,
     to get in sync properly.  */
  op->lineno++;
#endif
}

/* The arglist structure is built by do_define to tell
   collect_definition where the argument names begin.  That
   is, for a define like "#define f(x,y,z) foo+x-bar*y", the arglist
   would contain pointers to the strings x, y, and z.
   Collect_definition would then build a DEFINITION node,
   with reflist nodes pointing to the places x, y, and z had
   appeared.  So the arglist is just convenience data passed
   between these two routines.  It is not kept around after
   the current #define has been processed and entered into the
   hash table.  */

struct arglist {
  struct arglist *next;
  const U_CHAR *name;
  int length;
  int argno;
  char rest_args;
};

/* Create a DEFINITION node from a #define directive.  Arguments are 
   as for do_define.  */

static MACRODEF
create_definition (buf, limit, op)
     const U_CHAR *buf, *limit;
     FILE_BUF *op;
{
  const U_CHAR *bp;		/* temp ptr into input buffer */
  const U_CHAR *symname;	/* remember where symbol name starts */
  int sym_length;		/* and how long it is */
  int line = instack[indepth].lineno;
  const char *file = instack[indepth].nominal_fname;
  size_t file_len = instack[indepth].nominal_fname_len;
  int rest_args = 0;

  DEFINITION *defn;
  int arglengths = 0;		/* Accumulate lengths of arg names
				   plus number of args.  */
  MACRODEF mdef;

  bp = buf;

  while (is_hor_space[*bp])
    bp++;

  symname = bp;			/* remember where it starts */
  sym_length = check_macro_name (bp, 0);
  bp += sym_length;

  /* Lossage will occur if identifiers or control keywords are broken
     across lines using backslash.  This is not the right place to take
     care of that.  */

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
      temp->rest_args = 0;
      arg_ptrs = temp;

      if (rest_args)
	pedwarn ("another parameter follows `%s'",
		 rest_extension);

      if (!is_idstart[*bp])
	{
	  if (c9x && limit - bp > (long) REST_EXTENSION_LENGTH
	      && bcmp (rest_extension, bp, REST_EXTENSION_LENGTH) == 0)
	    {
	      /* This is the ISO C 9x way to write macros with variable
		 number of arguments.  */
	      rest_args = 1;
	      temp->rest_args = 1;
	    }
	  else
	pedwarn ("invalid character in macro parameter name");
	}
      
      /* Find the end of the arg name.  */
      while (is_idchar[*bp]) {
	bp++;
	/* do we have a "special" rest-args extension here? */
	if (limit - bp > (long) REST_EXTENSION_LENGTH
	    && bcmp (rest_extension, bp, REST_EXTENSION_LENGTH) == 0) {
	  if (pedantic && !instack[indepth].system_header_p)
	    pedwarn ("ANSI C does not allow macro with variable arguments");
	  rest_args = 1;
	  temp->rest_args = 1;
	  break;
	}
      }
      if (bp == temp->name && rest_args == 1)
	{
	  /* This is the ISO C 9x style.  */
	  temp->name = (U_CHAR *) va_args_name;
	  temp->length = VA_ARGS_NAME_LENGTH;
	}
      else
      temp->length = bp - temp->name;
      if (rest_args == 1)
	bp += REST_EXTENSION_LENGTH;
      arglengths += temp->length + 2;
      SKIP_WHITE_SPACE (bp);
      if (temp->length == 0 || (*bp != ',' && *bp != ')')) {
	error ("badly punctuated parameter list in `#define'");
	goto nope;
      }
      if (*bp == ',') {
	bp++;
	SKIP_WHITE_SPACE (bp);
	/* A comma at this point can only be followed by an identifier.  */
	if (!is_idstart[*bp]
	    && !(c9x && limit - bp > (long) REST_EXTENSION_LENGTH
		&&  bcmp (rest_extension, bp, REST_EXTENSION_LENGTH) == 0)) {
	  error ("badly punctuated parameter list in `#define'");
	  goto nope;
	}
      }
      if (bp >= limit) {
	error ("unterminated parameter list in `#define'");
	goto nope;
      }
      {
	struct arglist *otemp;

	for (otemp = temp->next; otemp != NULL; otemp = otemp->next)
	  if (temp->length == otemp->length
	      && bcmp (temp->name, otemp->name, temp->length) == 0)
	    {
	      error ("duplicate argument name `%.*s' in `#define'",
		     temp->length, temp->name);
	      goto nope;
	  }
	if (rest_args == 0 && temp->length == VA_ARGS_NAME_LENGTH
	    && bcmp (temp->name, va_args_name, VA_ARGS_NAME_LENGTH) == 0)
	  {
	    error ("\
reserved name `%s' used as argument name in `#define'", va_args_name);
	    goto nope;
	  }
      }
    }

    ++bp;			/* skip paren */
    SKIP_WHITE_SPACE (bp);
    /* now everything from bp before limit is the definition.  */
    defn = collect_expansion (bp, limit, argno, arg_ptrs);
    defn->rest_args = rest_args;

    /* Now set defn->args.argnames to the result of concatenating
       the argument names in reverse order
       with comma-space between them.  */
    defn->args.argnames = (U_CHAR *) xmalloc (arglengths + 1);
    {
      struct arglist *temp;
      int i = 0;
      for (temp = arg_ptrs; temp; temp = temp->next) {
	bcopy (temp->name, &defn->args.argnames[i], temp->length);
	i += temp->length;
	if (temp->next != 0) {
	  defn->args.argnames[i++] = ',';
	  defn->args.argnames[i++] = ' ';
	}
      }
      defn->args.argnames[i] = 0;
    }
  } else {
    /* Simple expansion or empty definition.  */

    if (bp < limit)
      {
	if (is_hor_space[*bp]) {
	  bp++;
	  SKIP_WHITE_SPACE (bp);
	} else if (sym_length) {
	  switch (*bp) {
	    case '!':  case '"':  case '#':  case '%':  case '&':  case '\'':
	    case ')':  case '*':  case '+':  case ',':  case '-':  case '.':
	    case '/':  case ':':  case ';':  case '<':  case '=':  case '>':
	    case '?':  case '[':  case '\\': case ']':  case '^':  case '{':
	    case '|':  case '}':  case '~':
	      warning ("missing white space after `#define %.*s'",
		       sym_length, symname);
	      break;

	    default:
	      pedwarn ("missing white space after `#define %.*s'",
		       sym_length, symname);
	      break;
	  }
	}
      }
    /* Now everything from bp before limit is the definition.  */
    defn = collect_expansion (bp, limit, -1, NULL_PTR);
    defn->args.argnames = (U_CHAR *) "";
  }

  defn->line = line;
  defn->file = file;
  defn->file_len = file_len;

  /* OP is null if this is a predefinition */
  defn->predefined = !op;
  mdef.defn = defn;
  mdef.symnam = symname;
  mdef.symlen = sym_length;

  return mdef;

 nope:
  mdef.defn = 0;
  return mdef;
}
 
/* Process a #define directive.
BUF points to the contents of the #define directive, as a contiguous string.
LIMIT points to the first character past the end of the definition.
KEYWORD is the keyword-table entry for #define.  */

static int
do_define (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  int hashcode;
  MACRODEF mdef;
  enum node_type newtype = keyword->type == T_DEFINE ? T_MACRO : T_POISON;

  /* If this is a precompiler run (with -pcp) pass thru #define directives.  */
  if (pcp_outfile && op)
    pass_thru_directive (buf, limit, op, keyword);

  mdef = create_definition (buf, limit, op);
  if (mdef.defn == 0)
    goto nope;

  hashcode = hashf (mdef.symnam, mdef.symlen, HASHSIZE);

  {
    HASHNODE *hp;
    if ((hp = lookup (mdef.symnam, mdef.symlen, hashcode)) != NULL) {
      int ok = 0;
      /* Redefining a precompiled key is ok.  */
      if (hp->type == T_PCSTRING)
	ok = 1;
      /* Redefining a poisoned identifier is even worse than `not ok'.  */
      else if (hp->type == T_POISON)
        ok = -1;
      /* Poisoning anything else is not ok.  
	 The poison should always come first.  */
      else if (newtype == T_POISON)
	ok = 0;
      /* Redefining a macro is ok if the definitions are the same.  */
      else if (hp->type == T_MACRO)
	ok = ! compare_defs (mdef.defn, hp->value.defn);
      /* Redefining a constant is ok with -D.  */
      else if (hp->type == T_CONST)
        ok = ! done_initializing;
      
      /* Print the warning or error if it's not ok.  */
      if (ok <= 0) 
	{
	  /* If we are passing through #define and #undef directives, do
	     that for this re-definition now.  */
	  if (debug_output && op)
	    pass_thru_directive (buf, limit, op, keyword);
	  
	  if (hp->type == T_POISON)
	    error ("redefining poisoned `%.*s'", mdef.symlen, mdef.symnam);
	  else
	    pedwarn ("`%.*s' redefined", mdef.symlen, mdef.symnam);
	  if (hp->type == T_MACRO)
	    pedwarn_with_file_and_line (hp->value.defn->file,
					hp->value.defn->file_len,
					hp->value.defn->line,
					"this is the location of the previous definition");
	}
      if (hp->type != T_POISON)
	{
	  /* Replace the old definition.  */
	  hp->type = newtype;
	  hp->value.defn = mdef.defn;
	}
    } else {
      /* If we are passing through #define and #undef directives, do
	 that for this new definition now.  */
      if (debug_output && op)
	pass_thru_directive (buf, limit, op, keyword);
      install (mdef.symnam, mdef.symlen, newtype,
	       (char *) mdef.defn, hashcode);
    }
  }

  return 0;

nope:

  return 1;
}

/* Check a purported macro name SYMNAME, and yield its length.
   ASSERTION is nonzero if this is really for an assertion name.  */

static int
check_macro_name (symname, assertion)
     const U_CHAR *symname;
     int assertion;
{
  const U_CHAR *p;
  int sym_length;

  for (p = symname; is_idchar[*p]; p++)
    ;
  sym_length = p - symname;
  if (sym_length == 0
      || (sym_length == 1 && *symname == 'L' && (*p == '\'' || *p == '"')))
    {
      if (assertion)
	error ("invalid assertion name");
      else
	error ("invalid macro name");
    }
  else if (!is_idstart[*symname]
	   || (sym_length == 7 && ! bcmp (symname, "defined", 7)))
    {
      if (assertion)
	error ("invalid assertion name `%.*s'", sym_length, symname);
      else
	error ("invalid macro name `%.*s'", sym_length, symname);
    }
  return sym_length;
}

/* Return zero if two DEFINITIONs are isomorphic.  */
     
static int
compare_defs (d1, d2)
     DEFINITION *d1, *d2;
{
  register struct reflist *a1, *a2;
  register U_CHAR *p1 = d1->expansion;
  register U_CHAR *p2 = d2->expansion;
  int first = 1;

  if (d1->nargs != d2->nargs)
    return 1;
  if (pedantic
      && strcmp ((char *)d1->args.argnames, (char *)d2->args.argnames))
    return 1;
  for (a1 = d1->pattern, a2 = d2->pattern; a1 && a2;
       a1 = a1->next, a2 = a2->next) {
    if (!((a1->nchars == a2->nchars && ! bcmp (p1, p2, a1->nchars))
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
  register const U_CHAR *end1 = beg1 + len1;
  register const U_CHAR *end2 = beg2 + len2;
  if (first) {
    while (beg1 != end1 && is_space[*beg1]) beg1++;
    while (beg2 != end2 && is_space[*beg2]) beg2++;
  }
  if (last) {
    while (beg1 != end1 && is_space[end1[-1]]) end1--;
    while (beg2 != end2 && is_space[end2[-1]]) end2--;
  }
  while (beg1 != end1 && beg2 != end2) {
    if (is_space[*beg1] && is_space[*beg2]) {
      while (beg1 != end1 && is_space[*beg1]) beg1++;
      while (beg2 != end2 && is_space[*beg2]) beg2++;
    } else if (*beg1 == *beg2) {
      beg1++; beg2++;
    } else break;
  }
  return (beg1 != end1) || (beg2 != end2);
}

/* Read a replacement list for a macro with parameters.
   Build the DEFINITION structure.
   Reads characters of text starting at BUF until END.
   ARGLIST specifies the formal parameters to look for
   in the text of the definition; NARGS is the number of args
   in that list, or -1 for a macro name that wants no argument list.
   MACRONAME is the macro name itself (so we can avoid recursive expansion)
   and NAMELEN is its length in characters.
   
Note that comments, backslash-newlines, and leading white space
have already been deleted from the argument.  */

/* If there is no trailing whitespace, a Newline Space is added at the end
   to prevent concatenation that would be contrary to the standard.  */

static DEFINITION *
collect_expansion (buf, end, nargs, arglist)
     const U_CHAR *buf;
     const U_CHAR *end;
     int nargs;
     struct arglist *arglist;
{
  DEFINITION *defn;
  register const U_CHAR *p;
  register const U_CHAR *limit;
  register U_CHAR *lastp, *exp_p;
  struct reflist *endpat = NULL;
  /* Pointer to first nonspace after last ## seen.  */
  const U_CHAR *concat = 0;
  /* Pointer to first nonspace after last single-# seen.  */
  const U_CHAR *stringify = 0;
  /* How those tokens were spelled.  */
  enum sharp_token_type concat_sharp_token_type = NO_SHARP_TOKEN;
  enum sharp_token_type stringify_sharp_token_type = NO_SHARP_TOKEN;
  int maxsize;
  int expected_delimiter = '\0';

  /* Scan thru the replacement list, ignoring comments and quoted
     strings, picking up on the macro calls.  It does a linear search
     thru the arg list on every potential symbol.  Profiling might say
     that something smarter should happen.  */

  if (end < buf)
    abort ();

  /* Find the beginning of the trailing whitespace.  */
  limit = end;
  p = buf;
  while (p < limit && is_space[limit[-1]]) limit--;

  /* Allocate space for the text in the macro definition.
     Each input char may or may not need 1 byte,
     so this is an upper bound.
     The extra 3 are for invented trailing newline-marker and final null.  */
  maxsize = (sizeof (DEFINITION)
	     + (limit - p) + 3);
  defn = (DEFINITION *) xcalloc (1, maxsize);

  defn->nargs = nargs;
  exp_p = defn->expansion = (U_CHAR *) defn + sizeof (DEFINITION);
  lastp = exp_p;

  if (p[0] == '#'
      ? p[1] == '#'
      : p[0] == '%' && p[1] == ':' && p[2] == '%' && p[3] == ':') {
    error ("`##' at start of macro definition");
    p += p[0] == '#' ? 2 : 4;
  }

  /* Process the main body of the definition.  */
  while (p < limit) {
    int skipped_arg = 0;
    register U_CHAR c = *p++;

    *exp_p++ = c;

    if (!traditional) {
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
	if (expected_delimiter) {
	  /* In a string, backslash goes through
	     and makes next char ordinary.  */
	  *exp_p++ = *p++;
	}
	break;

      case '%':
	if (!expected_delimiter && *p == ':') {
	  /* %: is not a digraph if preceded by an odd number of '<'s.  */
	  const U_CHAR *p0 = p - 1;
	  while (buf < p0 && p0[-1] == '<')
	    p0--;
	  if ((p - p0) & 1) {
	    /* Treat %:%: as ## and %: as #.  */
	    if (p[1] == '%' && p[2] == ':') {
	      p += 2;
	      goto sharp_sharp_token;
	    }
	    if (nargs >= 0) {
	      p++;
	      goto sharp_token;
	    }
	  }
	}
	break;

      case '#':
	/* # is ordinary inside a string.  */
	if (expected_delimiter)
	  break;
	if (*p == '#') {
	sharp_sharp_token:
	  /* ##: concatenate preceding and following tokens.  */
	  /* Take out the first #, discard preceding whitespace.  */
	  exp_p--;
	  while (exp_p > lastp && is_hor_space[exp_p[-1]])
	    --exp_p;
	  /* Skip the second #.  */
	  p++;
	  concat_sharp_token_type = c;
	  if (is_hor_space[*p]) {
	    concat_sharp_token_type = c + 1;
	    p++;
	    SKIP_WHITE_SPACE (p);
	  }
	  concat = p;
	  if (p == limit)
	    error ("`##' at end of macro definition");
	} else if (nargs >= 0) {
	  /* Single #: stringify following argument ref.
	     Don't leave the # in the expansion.  */
	sharp_token:
	  exp_p--;
	  stringify_sharp_token_type = c;
	  if (is_hor_space[*p]) {
	    stringify_sharp_token_type = c + 1;
	    p++;
	    SKIP_WHITE_SPACE (p);
	  }
	  if (! is_idstart[*p] || nargs == 0
	      || (*p == 'L' && (p[1] == '\'' || p[1] == '"')))
	    error ("`#' operator is not followed by a macro argument name");
	  else
	    stringify = p;
	}
	break;
      }
    } else {
      /* In -traditional mode, recognize arguments inside strings and
	 character constants, and ignore special properties of #.
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
	  while (++p < limit) {
	    if (p[0] == '*' && p[1] == '/') {
	      p += 2;
	      break;
	    }
	  }
#if 0
	  /* Mark this as a concatenation-point, as if it had been ##.  */
	  concat = p;
#endif
	}
	break;
      }
    }

#ifdef MULTIBYTE_CHARS
    /* Handle multibyte characters inside string and character literals.  */
    if (expected_delimiter != '\0')
      {
	int length;
	--p;
	length = local_mblen (p, limit - p);
	if (length > 1)
	  {
	    --exp_p;
	    bcopy (p, exp_p, length);
	    p += length;
	    exp_p += length;
	    continue;
	  }
	++p;
      }
#endif

    /* Handle the start of a symbol.  */
    if (is_idchar[c] && nargs > 0) {
      const U_CHAR *id_beg = p - 1;
      int id_len;

      --exp_p;
      while (p != limit && is_idchar[*p]) p++;
      id_len = p - id_beg;

      if (is_idstart[c]
	  && ! (id_len == 1 && c == 'L' && (*p == '\'' || *p == '"'))) {
	register struct arglist *arg;

	for (arg = arglist; arg != NULL; arg = arg->next) {
	  struct reflist *tpat;

	  if (arg->name[0] == c
	      && arg->length == id_len
	      && bcmp (arg->name, id_beg, id_len) == 0) {
	    enum sharp_token_type tpat_stringify;
	    if (expected_delimiter) {
	      if (warn_stringify) {
		if (traditional) {
		  warning ("macro argument `%.*s' is stringified.",
			   id_len, arg->name);
		} else {
		  warning ("macro arg `%.*s' would be stringified with -traditional.",
			   id_len, arg->name);
		}
	      }
	      /* If ANSI, don't actually substitute inside a string.  */
	      if (!traditional)
		break;
	      tpat_stringify = SHARP_TOKEN;
	    } else {
	      tpat_stringify
		= (stringify == id_beg
		   ? stringify_sharp_token_type : NO_SHARP_TOKEN);
	    }
	    /* make a pat node for this arg and append it to the end of
	       the pat list */
	    tpat = (struct reflist *) xmalloc (sizeof (struct reflist));
	    tpat->next = NULL;
	    tpat->raw_before
	      = concat == id_beg ? concat_sharp_token_type : NO_SHARP_TOKEN;
	    tpat->raw_after = NO_SHARP_TOKEN;
	    tpat->rest_args = arg->rest_args;
	    tpat->stringify = tpat_stringify;

	    if (endpat == NULL)
	      defn->pattern = tpat;
	    else
	      endpat->next = tpat;
	    endpat = tpat;

	    tpat->argno = arg->argno;
	    tpat->nchars = exp_p - lastp;
	    {
	      register const U_CHAR *p1 = p;
	      SKIP_WHITE_SPACE (p1);
	      if (p1[0]=='#'
	          ? p1[1]=='#'
		  : p1[0]=='%' && p1[1]==':' && p1[2]=='%' && p1[3]==':')
		tpat->raw_after = p1[0] + (p != p1);
	    }
	    lastp = exp_p;	/* place to start copying from next time */
	    skipped_arg = 1;
	    break;
	  }
	}
      }

      /* If this was not a macro arg, copy it into the expansion.  */
      if (! skipped_arg) {
	register const U_CHAR *lim1 = p;
	p = id_beg;
	while (p != lim1)
	  *exp_p++ = *p++;
	if (stringify == id_beg)
	  error ("`#' operator should be followed by a macro argument name");
      }
    }
  }

  if (!traditional && expected_delimiter == 0) {
    /* If ANSI, put in a newline-space marker to prevent token pasting.
       But not if "inside a string" (which in ANSI mode happens only for
       -D option).  */
    *exp_p++ = '\n';
    *exp_p++ = ' ';
  }

  *exp_p = '\0';

  defn->length = exp_p - defn->expansion;

  /* Crash now if we overrun the allocated size.  */
  if (defn->length + 1 > maxsize)
    abort ();

#if 0
/* This isn't worth the time it takes.  */
  /* give back excess storage */
  defn->expansion = (U_CHAR *) xrealloc (defn->expansion, defn->length + 1);
#endif

  return defn;
}

static int
do_assert (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op ATTRIBUTE_UNUSED;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  const U_CHAR *bp;		/* temp ptr into input buffer */
  const U_CHAR *symname;	/* remember where symbol name starts */
  int sym_length;		/* and how long it is */
  struct arglist *tokens = NULL;

  if (pedantic && done_initializing && !instack[indepth].system_header_p)
    pedwarn ("ANSI C does not allow `#assert'");

  bp = buf;

  while (is_hor_space[*bp])
    bp++;

  symname = bp;			/* remember where it starts */
  sym_length = check_macro_name (bp, 1);
  bp += sym_length;
  /* #define doesn't do this, but we should.  */
  SKIP_WHITE_SPACE (bp);

  /* Lossage will occur if identifiers or control tokens are broken
     across lines using backslash.  This is not the right place to take
     care of that.  */

  if (*bp != '(') {
    error ("missing token-sequence in `#assert'");
    return 1;
  }

  {
    int error_flag = 0;

    bp++;			/* skip '(' */
    SKIP_WHITE_SPACE (bp);

    tokens = read_token_list (&bp, limit, &error_flag);
    if (error_flag)
      return 1;
    if (tokens == 0) {
      error ("empty token-sequence in `#assert'");
      return 1;
    }

    ++bp;			/* skip paren */
    SKIP_WHITE_SPACE (bp);
  }

  /* If this name isn't already an assertion name, make it one.
     Error if it was already in use in some other way.  */

  {
    ASSERTION_HASHNODE *hp;
    int hashcode = hashf (symname, sym_length, ASSERTION_HASHSIZE);
    struct tokenlist_list *value
      = (struct tokenlist_list *) xmalloc (sizeof (struct tokenlist_list));

    hp = assertion_lookup (symname, sym_length, hashcode);
    if (hp == NULL) {
      if (sym_length == 7 && ! bcmp (symname, "defined", 7))
	error ("`defined' redefined as assertion");
      hp = assertion_install (symname, sym_length, hashcode);
    }

    /* Add the spec'd token-sequence to the list of such.  */
    value->tokens = tokens;
    value->next = hp->value;
    hp->value = value;
  }

  return 0;
}

static int
do_unassert (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op ATTRIBUTE_UNUSED;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  const U_CHAR *bp;		/* temp ptr into input buffer */
  const U_CHAR *symname;	/* remember where symbol name starts */
  int sym_length;		/* and how long it is */

  struct arglist *tokens = NULL;
  int tokens_specified = 0;

  if (pedantic && done_initializing && !instack[indepth].system_header_p)
    pedwarn ("ANSI C does not allow `#unassert'");

  bp = buf;

  while (is_hor_space[*bp])
    bp++;

  symname = bp;			/* remember where it starts */
  sym_length = check_macro_name (bp, 1);
  bp += sym_length;
  /* #define doesn't do this, but we should.  */
  SKIP_WHITE_SPACE (bp);

  /* Lossage will occur if identifiers or control tokens are broken
     across lines using backslash.  This is not the right place to take
     care of that.  */

  if (*bp == '(') {
    int error_flag = 0;

    bp++;			/* skip '(' */
    SKIP_WHITE_SPACE (bp);

    tokens = read_token_list (&bp, limit, &error_flag);
    if (error_flag)
      return 1;
    if (tokens == 0) {
      error ("empty token list in `#unassert'");
      return 1;
    }

    tokens_specified = 1;

    ++bp;			/* skip paren */
    SKIP_WHITE_SPACE (bp);
  }

  {
    ASSERTION_HASHNODE *hp;
    int hashcode = hashf (symname, sym_length, ASSERTION_HASHSIZE);
    struct tokenlist_list *tail, *prev;

    hp = assertion_lookup (symname, sym_length, hashcode);
    if (hp == NULL)
      return 1;

    /* If no token list was specified, then eliminate this assertion
       entirely.  */
    if (! tokens_specified) {
      struct tokenlist_list *next;
      for (tail = hp->value; tail; tail = next) {
	next = tail->next;
	free_token_list (tail->tokens);
	free (tail);
      }
      delete_assertion (hp);
    } else {
      /* If a list of tokens was given, then delete any matching list.  */

      tail = hp->value;
      prev = 0;
      while (tail) {
	struct tokenlist_list *next = tail->next;
	if (compare_token_lists (tail->tokens, tokens)) {
	  if (prev)
	    prev->next = next;
	  else
	    hp->value = tail->next;
	  free_token_list (tail->tokens);
	  free (tail);
	} else {
	  prev = tail;
	}
	tail = next;
      }
    }
  }

  return 0;
}

/* Test whether there is an assertion named NAME
   and optionally whether it has an asserted token list TOKENS.
   NAME is not null terminated; its length is SYM_LENGTH.
   If TOKENS_SPECIFIED is 0, then don't check for any token list.  */

int
check_assertion (name, sym_length, tokens_specified, tokens)
     const U_CHAR *name;
     int sym_length;
     int tokens_specified;
     struct arglist *tokens;
{
  ASSERTION_HASHNODE *hp;
  int hashcode = hashf (name, sym_length, ASSERTION_HASHSIZE);

  if (pedantic && !instack[indepth].system_header_p)
    pedwarn ("ANSI C does not allow testing assertions");

  hp = assertion_lookup (name, sym_length, hashcode);
  if (hp == NULL)
    /* It is not an assertion; just return false.  */
    return 0;

  /* If no token list was specified, then value is 1.  */
  if (! tokens_specified)
    return 1;

  {
    struct tokenlist_list *tail;

    tail = hp->value;

    /* If a list of tokens was given,
       then succeed if the assertion records a matching list.  */

    while (tail) {
      if (compare_token_lists (tail->tokens, tokens))
	return 1;
      tail = tail->next;
    }

    /* Fail if the assertion has no matching list.  */
    return 0;
  }
}

/* Compare two lists of tokens for equality including order of tokens.  */

static int
compare_token_lists (l1, l2)
     struct arglist *l1, *l2;
{
  while (l1 && l2) {
    if (l1->length != l2->length)
      return 0;
    if (bcmp (l1->name, l2->name, l1->length))
      return 0;
    l1 = l1->next;
    l2 = l2->next;
  }

  /* Succeed if both lists end at the same time.  */
  return l1 == l2;
}

/* Read a space-separated list of tokens ending in a close parenthesis.
   Return a list of strings, in the order they were written.
   (In case of error, return 0 and store -1 in *ERROR_FLAG.)
   Parse the text starting at *BPP, and update *BPP.
   Don't parse beyond LIMIT.  */

static struct arglist *
read_token_list (bpp, limit, error_flag)
     const U_CHAR **bpp;
     const U_CHAR *limit;
     int *error_flag;
{
  struct arglist *token_ptrs = 0;
  const U_CHAR *bp = *bpp;
  int depth = 1;

  *error_flag = 0;

  /* Loop over the assertion value tokens.  */
  while (depth > 0) {
    struct arglist *temp;
    U_CHAR *temp2;
    int eofp = 0;
    const U_CHAR *beg = bp;

    /* Find the end of the token.  */
    if (*bp == '(') {
      bp++;
      depth++;
    } else if (*bp == ')') {
      depth--;
      if (depth == 0)
	break;
      bp++;
    } else if (*bp == '"' || *bp == '\'')
      bp = skip_quoted_string (bp, limit, 0, NULL_PTR, NULL_PTR, &eofp);
    else
      while (! is_hor_space[*bp] && *bp != '(' && *bp != ')'
	     && *bp != '"' && *bp != '\'' && bp != limit)
	bp++;

    temp = (struct arglist *) xmalloc (sizeof (struct arglist));
    temp2 = (U_CHAR *) xmalloc (bp - beg + 1);
    bcopy ((const PTR) beg, (PTR) temp2, bp - beg);
    temp2[bp - beg] = 0;
    temp->name = temp2;
    temp->next = token_ptrs;
    token_ptrs = temp;
    temp->length = bp - beg;

    SKIP_WHITE_SPACE (bp);

    if (bp >= limit) {
      error ("unterminated token sequence in `#assert' or `#unassert'");
      *error_flag = -1;
      return 0;
    }
  }
  *bpp = bp;

  /* We accumulated the names in reverse order.
     Now reverse them to get the proper order.  */
  {
    register struct arglist *prev = 0, *this, *next;
    for (this = token_ptrs; this; this = next) {
      next = this->next;
      this->next = prev;
      prev = this;
    }
    return prev;
  }
}

static void
free_token_list (tokens)
     struct arglist *tokens;
{
  while (tokens) {
    struct arglist *next = tokens->next;
    free ((PTR) tokens->name);
    free (tokens);
    tokens = next;
  }
}

/* Install a name in the assertion hash table.

   If LEN is >= 0, it is the length of the name.
   Otherwise, compute the length by scanning the entire name.

   If HASH is >= 0, it is the precomputed hash code.
   Otherwise, compute the hash code.  */

static ASSERTION_HASHNODE *
assertion_install (name, len, hash)
     const U_CHAR *name;
     int len;
     int hash;
{
  register ASSERTION_HASHNODE *hp;
  register int i, bucket;
  register U_CHAR *p;
  register const U_CHAR *q;

  i = sizeof (ASSERTION_HASHNODE) + len + 1;
  hp = (ASSERTION_HASHNODE *) xmalloc (i);
  bucket = hash;
  hp->bucket_hdr = &assertion_hashtab[bucket];
  hp->next = assertion_hashtab[bucket];
  assertion_hashtab[bucket] = hp;
  hp->prev = NULL;
  if (hp->next != NULL)
    hp->next->prev = hp;
  hp->length = len;
  hp->value = 0;
  hp->name = ((U_CHAR *) hp) + sizeof (ASSERTION_HASHNODE);
  p = hp->name;
  q = name;
  for (i = 0; i < len; i++)
    *p++ = *q++;
  hp->name[len] = 0;
  return hp;
}

/* Find the most recent hash node for name "name" (ending with first
   non-identifier char) installed by install

   If LEN is >= 0, it is the length of the name.
   Otherwise, compute the length by scanning the entire name.

   If HASH is >= 0, it is the precomputed hash code.
   Otherwise, compute the hash code.  */

static ASSERTION_HASHNODE *
assertion_lookup (name, len, hash)
     const U_CHAR *name;
     int len;
     int hash;
{
  register ASSERTION_HASHNODE *bucket;

  bucket = assertion_hashtab[hash];
  while (bucket) {
    if (bucket->length == len && bcmp (bucket->name, name, len) == 0)
      return bucket;
    bucket = bucket->next;
  }
  return NULL;
}

static void
delete_assertion (hp)
     ASSERTION_HASHNODE *hp;
{

  if (hp->prev != NULL)
    hp->prev->next = hp->next;
  if (hp->next != NULL)
    hp->next->prev = hp->prev;

  /* Make sure that the bucket chain header that the deleted guy was
     on points to the right thing afterwards.  */
  if (hp == *hp->bucket_hdr)
    *hp->bucket_hdr = hp->next;

  free (hp);
}

/*
 * interpret #line directive.  Remembers previously seen fnames
 * in its very own hash table.
 */
#define FNAME_HASHSIZE 37

static int
do_line (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  register U_CHAR *bp;
  FILE_BUF *ip = &instack[indepth];
  FILE_BUF tem;
  int new_lineno;
  enum file_change_code file_change = same_file;

  /* Expand any macros.  */
  tem = expand_to_temp_buffer (buf, limit, 0, 0);

  /* Point to macroexpanded line, which is null-terminated now.  */
  bp = tem.buf;
  limit = tem.bufp;
  SKIP_WHITE_SPACE (bp);

  if (!ISDIGIT (*bp)) {
    error ("invalid format `#line' directive");
    return 0;
  }

  /* The Newline at the end of this line remains to be processed.
     To put the next line at the specified line number,
     we must store a line number now that is one less.  */
  new_lineno = atoi ((char *) bp) - 1;

  /* NEW_LINENO is one less than the actual line number here.  */
  if (pedantic && new_lineno < 0)
    pedwarn ("line number out of range in `#line' directive");

  /* skip over the line number.  */
  while (ISDIGIT (*bp))
    bp++;

#if 0 /* #line 10"foo.c" is supposed to be allowed.  */
  if (*bp && !is_space[*bp]) {
    error ("invalid format `#line' directive");
    return;
  }
#endif

  SKIP_WHITE_SPACE (bp);

  if (*bp == '\"') {
    static HASHNODE *fname_table[FNAME_HASHSIZE];
    HASHNODE *hp, **hash_bucket;
    U_CHAR *fname, *p;
    int fname_length;

    fname = ++bp;

    /* Turn the file name, which is a character string literal,
       into a null-terminated string.  Do this in place.  */
    p = bp;
    for (;;)
      switch ((*p++ = *bp++)) {
      case '\\':
	if (! ignore_escape_flag)
	  {
	    char *bpc = (char *) bp;
	    HOST_WIDEST_INT c = parse_escape (&bpc, (HOST_WIDEST_INT) (U_CHAR) (-1));
	    bp = (U_CHAR *) bpc;
	    if (c < 0)
	      p--;
	    else
	      p[-1] = c;
	  }
	break;

      case '\"':
	*--p = 0;
	goto fname_done;
      }
  fname_done:
    fname_length = p - fname;

    SKIP_WHITE_SPACE (bp);
    if (*bp) {
      if (pedantic)
	pedwarn ("garbage at end of `#line' directive");
      if (*bp == '1')
	file_change = enter_file;
      else if (*bp == '2')
	file_change = leave_file;
      else if (*bp == '3')
	ip->system_header_p = 1;
      else if (*bp == '4')
	ip->system_header_p = 2;
      else {
	error ("invalid format `#line' directive");
	return 0;
      }

      bp++;
      SKIP_WHITE_SPACE (bp);
      if (*bp == '3') {
	ip->system_header_p = 1;
	bp++;
	SKIP_WHITE_SPACE (bp);
      }
      if (*bp == '4') {
	ip->system_header_p = 2;
	bp++;
	SKIP_WHITE_SPACE (bp);
      }
      if (*bp) {
	error ("invalid format `#line' directive");
	return 0;
      }
    }

    hash_bucket = &fname_table[hashf (fname, fname_length, FNAME_HASHSIZE)];
    for (hp = *hash_bucket; hp != NULL; hp = hp->next)
      if (hp->length == fname_length &&
	  bcmp (hp->value.cpval, fname, fname_length) == 0) {
	ip->nominal_fname = hp->value.cpval;
	ip->nominal_fname_len = fname_length;
	break;
      }
    if (hp == 0) {
      /* Didn't find it; cons up a new one.  */
      hp = (HASHNODE *) xcalloc (1, sizeof (HASHNODE) + fname_length + 1);
      hp->next = *hash_bucket;
      *hash_bucket = hp;

      ip->nominal_fname = hp->value.cpval = ((char *) hp) + sizeof (HASHNODE);
      ip->nominal_fname_len = hp->length = fname_length;
      bcopy (fname, ((char *) hp) + sizeof (HASHNODE), fname_length + 1);
    }
  } else if (*bp) {
    error ("invalid format `#line' directive");
    return 0;
  }

  ip->lineno = new_lineno;
  output_line_directive (ip, op, 0, file_change);
  check_expand (op, ip->length - (ip->bufp - ip->buf));
  return 0;
}

/* Remove the definition of a symbol from the symbol table.
   according to un*x /lib/cpp, it is not an error to undef
   something that has no definitions, so it isn't one here either.  */

static int
do_undef (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  int sym_length;
  HASHNODE *hp;
  U_CHAR *orig_buf = buf;

  /* If this is a precompiler run (with -pcp) pass thru #undef directives.  */
  if (pcp_outfile && op)
    pass_thru_directive (buf, limit, op, keyword);

  SKIP_WHITE_SPACE (buf);
  sym_length = check_macro_name (buf, 0);

  while ((hp = lookup (buf, sym_length, -1)) != NULL) {
    /* If we are generating additional info for debugging (with -g) we
       need to pass through all effective #undef directives.  */
    if (debug_output && op)
      pass_thru_directive (orig_buf, limit, op, keyword);
    if (hp->type == T_POISON)
      error ("cannot undefine poisoned `%s'", hp->name);
    else {
      if (hp->type != T_MACRO)
        warning ("undefining `%s'", hp->name);
      delete_macro (hp);
    }
  }

  if (pedantic) {
    buf += sym_length;
    SKIP_WHITE_SPACE (buf);
    if (buf != limit)
      pedwarn ("garbage after `#undef' directive");
  }
  return 0;
}


/* Report an error detected by the program we are processing.
   Use the text of the line in the error message.  */

static int
do_error (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op ATTRIBUTE_UNUSED;
     struct directive *keyword;
{
  int length = limit - buf;
  U_CHAR *copy = (U_CHAR *) alloca (length + 1);
  bcopy ((const PTR) buf, (PTR) copy, length);
  copy[length] = 0;
  SKIP_WHITE_SPACE (copy);

  switch (keyword->type) {
  case T_ERROR:
    error ("#error %s", copy);
    break;

  case T_WARNING:
    if (pedantic && !instack[indepth].system_header_p)
      pedwarn ("ANSI C does not allow `#warning'");
    warning ("#warning %s", copy);
    break;

  default:
    abort ();
  }

  return 0;
}
/* Remember the name of the current file being read from so that we can
   avoid ever including it again.  */

static void
do_once ()
{
  int i;

  for (i = indepth; i >= 0; i--)
    if (instack[i].inc) {
      record_control_macro (instack[i].inc, (const U_CHAR *) "");
      break;
    }
}

/* Report program identification.  */

static int
do_ident (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  FILE_BUF trybuf;
  int len;

  /* Allow #ident in system headers, since that's not user's fault.  */
  if (pedantic && !instack[indepth].system_header_p)
    pedwarn ("ANSI C does not allow `#ident'");

  trybuf = expand_to_temp_buffer (buf, limit, 0, 0);
  buf = trybuf.buf;
  len = trybuf.bufp - buf;

  /* Output expanded directive.  */
  check_expand (op, 7 + len);
  bcopy ("#ident ", (char *) op->bufp, 7);
  op->bufp += 7;
  bcopy ((const PTR) buf, (PTR) op->bufp, len);
  op->bufp += len;

  free (buf);
  return 0;
}

/* #pragma and its argument line have already been copied to the output file.
   Just check for some recognized pragmas that need validation here.  */

static int
do_pragma (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  SKIP_WHITE_SPACE (buf);
  if (!strncmp ((char *) buf, "once", 4)) {
    /* Allow #pragma once in system headers, since that's not the user's
       fault.  */
    if (!instack[indepth].system_header_p)
      warning ("`#pragma once' is obsolete");
    do_once ();
  }

  if (!strncmp ((char *) buf, "poison", 6)) {
    /* Poison these symbols so that all subsequent usage produces an
       error message.  */
    U_CHAR *p = buf + 6;

    SKIP_WHITE_SPACE (p);
    while (p < limit)
      {
	U_CHAR *end = p;
	
	while (end < limit && is_idchar[*end])
	  end++;
	if (end < limit && !is_space[*end])
	  {
	    error ("invalid #pragma poison");
	    return 0;
	  }
	do_define(p, end, op, keyword);
	p = end;
	SKIP_WHITE_SPACE (p);
      }
  }

  if (!strncmp ((char *) buf, "implementation", 14)) {
    /* Be quiet about `#pragma implementation' for a file only if it hasn't
       been included yet.  */

    int h;
    U_CHAR *p = buf + 14, *fname;
    SKIP_WHITE_SPACE (p);
    if (*p != '\"')
      return 0;

    fname = p + 1;
    p = skip_quoted_string (p, limit, 0, NULL_PTR, NULL_PTR, NULL_PTR);
    if (p[-1] == '"')
      *--p = '\0';
    
    for (h = 0; h < INCLUDE_HASHSIZE; h++) {
      struct include_file *inc;
      for (inc = include_hashtab[h]; inc; inc = inc->next) {
	if (!strcmp (base_name (inc->fname), (char *) fname)) {
	  warning ("`#pragma implementation' for \"%s\" appears after its #include",fname);
	  return 0;
	}
      }
    }
  }
  return 0;
}

#if 0
/* This was a fun hack, but #pragma seems to start to be useful.
   By failing to recognize it, we pass it through unchanged to cc1.  */

/* The behavior of the #pragma directive is implementation defined.
   this implementation defines it as follows.  */

static int
do_pragma ()
{
  close (0);
  if (open ("/dev/tty", O_RDONLY, 0666) != 0)
    goto nope;
  close (1);
  if (open ("/dev/tty", O_WRONLY, 0666) != 1)
    goto nope;
  execl ("/usr/games/hack", "#pragma", 0);
  execl ("/usr/games/rogue", "#pragma", 0);
  execl ("/usr/new/emacs", "-f", "hanoi", "9", "-kill", 0);
  execl ("/usr/local/emacs", "-f", "hanoi", "9", "-kill", 0);
nope:
  fatal ("You are in a maze of twisty compiler features, all different");
}
#endif

#ifdef SCCS_DIRECTIVE

/* Just ignore #sccs, on systems where we define it at all.  */

static int
do_sccs (buf, limit, op, keyword)
     U_CHAR *buf ATTRIBUTE_UNUSED;
     U_CHAR *limit ATTRIBUTE_UNUSED;
     FILE_BUF *op ATTRIBUTE_UNUSED;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  if (pedantic)
    pedwarn ("ANSI C does not allow `#sccs'");
  return 0;
}

#endif /* defined (SCCS_DIRECTIVE) */

/* Handle #if directive by
     1) inserting special `defined' keyword into the hash table
  	that gets turned into 0 or 1 by special_symbol (thus,
  	if the luser has a symbol called `defined' already, it won't
        work inside the #if directive)
     2) rescan the input into a temporary output buffer
     3) pass the output buffer to the yacc parser and collect a value
     4) clean up the mess left from steps 1 and 2.
     5) call conditional_skip to skip til the next #endif (etc.),
        or not, depending on the value from step 3.  */

static int
do_if (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  HOST_WIDEST_INT value;
  FILE_BUF *ip = &instack[indepth];

  value = eval_if_expression (buf, limit - buf);
  conditional_skip (ip, value == 0, T_IF, NULL_PTR, op);
  return 0;
}

/* Handle a #elif directive by not changing  if_stack  either.
   see the comment above do_else.  */

static int
do_elif (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  HOST_WIDEST_INT value;
  FILE_BUF *ip = &instack[indepth];

  if (if_stack == instack[indepth].if_stack) {
    error ("`#elif' not within a conditional");
    return 0;
  } else {
    if (if_stack->type != T_IF && if_stack->type != T_ELIF) {
      error ("`#elif' after `#else'");
      fprintf (stderr, " (matches line %d", if_stack->lineno);
      if (! (if_stack->fname_len == ip->nominal_fname_len
	     && !bcmp (if_stack->fname, ip->nominal_fname,
		       if_stack->fname_len))) {
	fprintf (stderr, ", file ");
	fwrite (if_stack->fname, sizeof if_stack->fname[0],
		if_stack->fname_len, stderr);
      }
      fprintf (stderr, ")\n");
    }
    if_stack->type = T_ELIF;
  }

  if (if_stack->if_succeeded)
    skip_if_group (ip, 0, op);
  else {
    value = eval_if_expression (buf, limit - buf);
    if (value == 0)
      skip_if_group (ip, 0, op);
    else {
      ++if_stack->if_succeeded;	/* continue processing input */
      output_line_directive (ip, op, 1, same_file);
    }
  }
  return 0;
}

/* Evaluate a #if expression in BUF, of length LENGTH, then parse the
   result as a C expression and return the value as an int.  */

static HOST_WIDEST_INT
eval_if_expression (buf, length)
     const U_CHAR *buf;
     int length;
{
  FILE_BUF temp_obuf;
  HASHNODE *save_defined;
  HOST_WIDEST_INT value;

  save_defined = install ((const U_CHAR *) "defined", -1, T_SPEC_DEFINED,
			  NULL_PTR, -1);
  pcp_inside_if = 1;
  temp_obuf = expand_to_temp_buffer (buf, buf + length, 0, 1);
  pcp_inside_if = 0;
  delete_macro (save_defined);	/* clean up special symbol */

  *temp_obuf.bufp = '\n';
  value = parse_c_expression ((char *) temp_obuf.buf,
			      warn_undef && !instack[indepth].system_header_p);

  free (temp_obuf.buf);

  return value;
}

/* routine to handle ifdef/ifndef.  Try to look up the symbol, then do
   or don't skip to the #endif/#else/#elif depending on what directive
   is actually being processed.  */

static int
do_xifdef (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  int skip;
  FILE_BUF *ip = &instack[indepth];
  U_CHAR *end; 
  int start_of_file = 0;
  U_CHAR *control_macro = 0;

  /* Detect a #ifndef at start of file (not counting comments).  */
  if (ip->fname != 0 && keyword->type == T_IFNDEF) {
    U_CHAR *p = ip->buf;
    while (p != directive_start) {
      U_CHAR c = *p++;
      if (is_space[c])
	;
      /* Make no special provision for backslash-newline here; this is
	 slower if backslash-newlines are present, but it's correct,
	 and it's not worth it to tune for the rare backslash-newline.  */
      else if (c == '/'
	       && (*p == '*' || (cplusplus_comments && *p == '/'))) {
	/* Skip this comment.  */
	int junk = 0;
	U_CHAR *save_bufp = ip->bufp;
	ip->bufp = p + 1;
	p = skip_to_end_of_comment (ip, &junk, 1);
	ip->bufp = save_bufp;
      } else {
	goto fail;
      }
    }
    /* If we get here, this conditional is the beginning of the file.  */
    start_of_file = 1;
  fail: ;
  }

  /* Discard leading and trailing whitespace.  */
  SKIP_WHITE_SPACE (buf);
  while (limit != buf && is_hor_space[limit[-1]]) limit--;

  /* Find the end of the identifier at the beginning.  */
  for (end = buf; is_idchar[*end]; end++);

  if (end == buf) {
    skip = (keyword->type == T_IFDEF);
    if (! traditional)
      {
	if (end == limit)
	  pedwarn ("`#%s' with no argument", keyword->name);
	else
	  pedwarn ("`#%s' argument starts with punctuation", keyword->name);
      }
  } else {
    HASHNODE *hp;

    if (! traditional) {
      if (ISDIGIT (buf[0]))
	pedwarn ("`#%s' argument starts with a digit", keyword->name);
      else if (end != limit)
	pedwarn ("garbage at end of `#%s' argument", keyword->name);
    }

    hp = lookup (buf, end-buf, -1);

    if (pcp_outfile) {
      /* Output a precondition for this macro.  */
      if (hp
	  && (hp->type == T_CONST
	      || (hp->type == T_MACRO && hp->value.defn->predefined)))
	fprintf (pcp_outfile, "#define %s\n", hp->name);
      else {
	U_CHAR *cp = buf;
	fprintf (pcp_outfile, "#undef ");
	while (is_idchar[*cp]) /* Ick! */
	  fputc (*cp++, pcp_outfile);
	putc ('\n', pcp_outfile);
      }
    }

    if ((hp != NULL) && (hp->type == T_POISON)) {
      error("attempt to use poisoned `%s'.", hp->name);
      hp = NULL;
    }
    skip = (hp == NULL) ^ (keyword->type == T_IFNDEF);
    if (start_of_file && !skip) {
      control_macro = (U_CHAR *) xmalloc (end - buf + 1);
      bcopy ((const PTR) buf, (PTR) control_macro, end - buf);
      control_macro[end - buf] = 0;
    }
  }
  
  conditional_skip (ip, skip, T_IF, control_macro, op);
  return 0;
}

/* Push TYPE on stack; then, if SKIP is nonzero, skip ahead.
   If this is a #ifndef starting at the beginning of a file,
   CONTROL_MACRO is the macro name tested by the #ifndef.
   Otherwise, CONTROL_MACRO is 0.  */

static void
conditional_skip (ip, skip, type, control_macro, op)
     FILE_BUF *ip;
     int skip;
     enum node_type type;
     const U_CHAR *control_macro;
     FILE_BUF *op;
{
  IF_STACK_FRAME *temp;

  temp = (IF_STACK_FRAME *) xcalloc (1, sizeof (IF_STACK_FRAME));
  temp->fname = ip->nominal_fname;
  temp->fname_len = ip->nominal_fname_len;
  temp->lineno = ip->lineno;
  temp->next = if_stack;
  temp->control_macro = control_macro;
  if_stack = temp;

  if_stack->type = type;

  if (skip != 0) {
    skip_if_group (ip, 0, op);
    return;
  } else {
    ++if_stack->if_succeeded;
    output_line_directive (ip, &outbuf, 1, same_file);
  }
}

/* Skip to #endif, #else, or #elif.  adjust line numbers, etc.
   Leaves input ptr at the sharp sign found.
   If ANY is nonzero, return at next directive of any sort.  */
     
static void
skip_if_group (ip, any, op)
     FILE_BUF *ip;
     int any;
     FILE_BUF *op;
{
  register U_CHAR *bp = ip->bufp, *cp;
  register U_CHAR *endb = ip->buf + ip->length;
  struct directive *kt;
  IF_STACK_FRAME *save_if_stack = if_stack; /* don't pop past here */
  U_CHAR *beg_of_line = bp;
  register int ident_length;
  U_CHAR *ident, *after_ident;
  /* Save info about where the group starts.  */
  U_CHAR *beg_of_group = bp;
  int beg_lineno = ip->lineno;
  int skipping_include_directive = 0;

  if (output_conditionals && op != 0) {
    static const char * const ptr = "#failed\n";
    int len = strlen (ptr);

    if (op->bufp > op->buf && op->bufp[-1] != '\n')
      {
	*op->bufp++ = '\n';
	op->lineno++;
      }
    check_expand (op, len);
    bcopy (ptr, (char *) op->bufp, len);
    op->bufp += len;
    op->lineno++;
    output_line_directive (ip, op, 1, 0);
  }

  while (bp < endb) {
    switch (*bp++) {
    case '/':			/* possible comment */
      if (*bp == '\\')
	newline_fix (bp);
      if (*bp == '*'
	  || (cplusplus_comments && *bp == '/')) {
	ip->bufp = ++bp;
	bp = skip_to_end_of_comment (ip, &ip->lineno, 0);
      }
      break;
    case '<':
      if (skipping_include_directive) {
	while (bp < endb && *bp != '>' && *bp != '\n') {
	  if (*bp == '\\' && bp[1] == '\n') {
	    ip->lineno++;
	    bp++;
	  }
	  bp++;
	}
      }
      break;
    case '\"':
      if (skipping_include_directive) {
	while (bp < endb && *bp != '\n') {
	  if (*bp == '"') {
	    bp++;
	    break;
	  }
	  if (*bp == '\\' && bp[1] == '\n') {
	    ip->lineno++;
	    bp++;
	  }
	  bp++;
	}
	break;
      }
      /* Fall through.  */
    case '\'':
      bp = skip_quoted_string (bp - 1, endb, ip->lineno, &ip->lineno,
			       NULL_PTR, NULL_PTR);
      break;
    case '\\':
      /* Char after backslash loses its special meaning in some cases.  */
      if (*bp == '\n') {
	++ip->lineno;
	bp++;
      } else if (traditional && bp < endb)
	bp++;
      break;
    case '\n':
      ++ip->lineno;
      beg_of_line = bp;
      skipping_include_directive = 0;
      break;
    case '%':
      if (beg_of_line == 0 || traditional)
	break;
      ip->bufp = bp - 1;
      while (bp[0] == '\\' && bp[1] == '\n')
	bp += 2;
      if (*bp == ':')
	goto sharp_token;
      break;
    case '#':
      /* # keyword: a # must be first nonblank char on the line */
      if (beg_of_line == 0)
	break;
      ip->bufp = bp - 1;
    sharp_token:
      /* Scan from start of line, skipping whitespace, comments
	 and backslash-newlines, and see if we reach this #.
	 If not, this # is not special.  */
      bp = beg_of_line;
      /* If -traditional, require # to be at beginning of line.  */
      if (!traditional) {
	while (1) {
	  if (is_hor_space[*bp])
	    bp++;
	  else if (*bp == '\\' && bp[1] == '\n')
	    bp += 2;
	  else if (*bp == '/' && bp[1] == '*') {
	    bp += 2;
	    while (1)
	      {
		if (*bp == '*')
		  {
		    if (bp[1] == '/')
		      {
			bp += 2;
			break;
		      }
		  }
		else
		  {
#ifdef MULTIBYTE_CHARS
		    int length;
		    length = local_mblen (bp, endb - bp);
		    if (length > 1)
		      bp += (length - 1);
#endif
		  }
		bp++;
	      }
	  }
	  /* There is no point in trying to deal with C++ // comments here,
	     because if there is one, then this # must be part of the
	     comment and we would never reach here.  */
	  else break;
	}
      }
      if (bp != ip->bufp) {
	bp = ip->bufp + 1;	/* Reset bp to after the #.  */
	break;
      }

      bp = ip->bufp + 1;	/* Point after the '#' */
      if (ip->bufp[0] == '%') {
	/* Skip past the ':' again.  */
	while (*bp == '\\') {
	  ip->lineno++;
	  bp += 2;
	}
	bp++;
      }

      /* Skip whitespace and \-newline.  */
      while (1) {
	if (is_hor_space[*bp])
	  bp++;
	else if (*bp == '\\' && bp[1] == '\n')
	  bp += 2;
	else if (*bp == '/') {
	  if (bp[1] == '\\')
	    newline_fix (bp + 1);
	  if (bp[1] == '*') {
	    for (bp += 2; ; bp++) {
	      if (*bp == '\n')
		ip->lineno++;
	      else if (*bp == '*') {
		if (bp[-1] == '/' && warn_comments)
		  warning ("`/*' within comment");
		if (bp[1] == '\\')
		  newline_fix (bp + 1);
		if (bp[1] == '/')
		  break;
	      }
	      else
		{
#ifdef MULTIBYTE_CHARS
		  int length;
		  length = local_mblen (bp, endb - bp);
		  if (length > 1)
		    bp += (length - 1);
#endif
		}
	    }
	    bp += 2;
	  } else if (bp[1] == '/' && cplusplus_comments) {
	    for (bp += 2; ; bp++) {
	      if (*bp == '\n')
		break;
	      if (*bp == '\\' && bp[1] == '\n')
		{
		  if (warn_comments)
		    warning ("multiline `//' comment");
		  ip->lineno++;
		  bp++;
		}
	      else
		{
#ifdef MULTIBYTE_CHARS
		  int length;
		  length = local_mblen (bp, endb - bp);
		  if (length > 1)
		    bp += (length - 1);
#endif
		}
	    }
	  } else
	    break;
        } else
	  break;
      }

      cp = bp;

      /* Now find end of directive name.
	 If we encounter a backslash-newline, exchange it with any following
	 symbol-constituents so that we end up with a contiguous name.  */

      while (1) {
	if (is_idchar[*bp])
	  bp++;
	else {
	  if (*bp == '\\')
	    name_newline_fix (bp);
	  if (is_idchar[*bp])
	    bp++;
	  else break;
	}
      }
      ident_length = bp - cp;
      ident = cp;
      after_ident = bp;

      /* A line of just `#' becomes blank.  */

      if (ident_length == 0 && *after_ident == '\n') {
	continue;
      }

      if (ident_length == 0 || !is_idstart[*ident]) {
	U_CHAR *p = ident;
	while (is_idchar[*p]) {
	  if (*p < '0' || *p > '9')
	    break;
	  p++;
	}
	/* Handle # followed by a line number.  */
	if (p != ident && !is_idchar[*p]) {
	  if (pedantic)
	    pedwarn ("`#' followed by integer");
	  continue;
	}

	/* Avoid error for `###' and similar cases unless -pedantic.  */
	if (p == ident) {
	  while (*p == '#' || is_hor_space[*p]) p++;
	  if (*p == '\n') {
	    if (pedantic && !lang_asm)
	      pedwarn ("invalid preprocessing directive");
	    continue;
	  }
	}

	if (!lang_asm && pedantic)
	  pedwarn ("invalid preprocessing directive name");
	continue;
      }

      for (kt = directive_table; kt->length >= 0; kt++) {
	IF_STACK_FRAME *temp;
	if (ident_length == kt->length
	    && bcmp (cp, kt->name, kt->length) == 0) {
	  /* If we are asked to return on next directive, do so now.  */
	  if (any)
	    goto done;

	  switch (kt->type) {
	  case T_IF:
	  case T_IFDEF:
	  case T_IFNDEF:
	    temp = (IF_STACK_FRAME *) xcalloc (1, sizeof (IF_STACK_FRAME));
	    temp->next = if_stack;
	    if_stack = temp;
	    temp->lineno = ip->lineno;
	    temp->fname = ip->nominal_fname;
	    temp->fname_len = ip->nominal_fname_len;
	    temp->type = kt->type;
	    break;
	  case T_ELSE:
	  case T_ENDIF:
	    if (pedantic && if_stack != save_if_stack)
	      validate_else (bp, endb);
	  case T_ELIF:
	    if (if_stack == instack[indepth].if_stack) {
	      error ("`#%s' not within a conditional", kt->name);
	      break;
	    }
	    else if (if_stack == save_if_stack)
	      goto done;		/* found what we came for */

	    if (kt->type != T_ENDIF) {
	      if (if_stack->type == T_ELSE)
		error ("`#else' or `#elif' after `#else'");
	      if_stack->type = kt->type;
	      break;
	    }

	    temp = if_stack;
	    if_stack = if_stack->next;
	    free (temp);
	    break;

	  case T_INCLUDE:
	  case T_INCLUDE_NEXT:
	  case T_IMPORT:
	    skipping_include_directive = 1;
	    break;

	  default:
	    break;
	  }
	  break;
	}
      }
      /* Don't let erroneous code go by.  */
      if (kt->length < 0 && !lang_asm && pedantic)
	pedwarn ("invalid preprocessing directive name");
    }
  }

  ip->bufp = bp;
  /* after this returns, rescan will exit because ip->bufp
     now points to the end of the buffer.
     rescan is responsible for the error message also.  */

 done:
  if (output_conditionals && op != 0) {
    static const char * const ptr = "#endfailed\n";
    int len = strlen (ptr);

    if (op->bufp > op->buf && op->bufp[-1] != '\n')
      {
	*op->bufp++ = '\n';
	op->lineno++;
      }
    check_expand (op, beg_of_line - beg_of_group);
    bcopy ((const PTR) beg_of_group, (PTR) op->bufp,
	   beg_of_line - beg_of_group);
    op->bufp += beg_of_line - beg_of_group;
    op->lineno += ip->lineno - beg_lineno;
    check_expand (op, len);
    bcopy (ptr, (char *) op->bufp, len);
    op->bufp += len;
    op->lineno++;
  }
}

/* Handle a #else directive.  Do this by just continuing processing
   without changing  if_stack ;  this is so that the error message
   for missing #endif's etc. will point to the original #if.  It
   is possible that something different would be better.  */

static int
do_else (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  FILE_BUF *ip = &instack[indepth];

  if (pedantic) {
    SKIP_WHITE_SPACE (buf);
    if (buf != limit)
      pedwarn ("text following `#else' violates ANSI standard");
  }

  if (if_stack == instack[indepth].if_stack) {
    error ("`#else' not within a conditional");
    return 0;
  } else {
    /* #ifndef can't have its special treatment for containing the whole file
       if it has a #else clause.  */
    if_stack->control_macro = 0;

    if (if_stack->type != T_IF && if_stack->type != T_ELIF) {
      error ("`#else' after `#else'");
      fprintf (stderr, " (matches line %d", if_stack->lineno);
      if (! (if_stack->fname_len == ip->nominal_fname_len
	     && !bcmp (if_stack->fname, ip->nominal_fname,
		       if_stack->fname_len))) {
	fprintf (stderr, ", file ");
	fwrite (if_stack->fname, sizeof if_stack->fname[0],
		if_stack->fname_len, stderr);
      }
      fprintf (stderr, ")\n");
    }
    if_stack->type = T_ELSE;
  }

  if (if_stack->if_succeeded)
    skip_if_group (ip, 0, op);
  else {
    ++if_stack->if_succeeded;	/* continue processing input */
    output_line_directive (ip, op, 1, same_file);
  }
  return 0;
}

/* Unstack after #endif directive.  */

static int
do_endif (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  if (pedantic) {
    SKIP_WHITE_SPACE (buf);
    if (buf != limit)
      pedwarn ("text following `#endif' violates ANSI standard");
  }

  if (if_stack == instack[indepth].if_stack)
    error ("unbalanced `#endif'");
  else {
    IF_STACK_FRAME *temp = if_stack;
    if_stack = if_stack->next;
    if (temp->control_macro != 0) {
      /* This #endif matched a #ifndef at the start of the file.
	 See if it is at the end of the file.  */
      FILE_BUF *ip = &instack[indepth];
      U_CHAR *p = ip->bufp;
      U_CHAR *ep = ip->buf + ip->length;

      while (p != ep) {
	U_CHAR c = *p++;
	if (!is_space[c]) {
	  if (c == '/'
	      && (*p == '*' || (cplusplus_comments && *p == '/'))) {
	    /* Skip this comment.  */
	    int junk = 0;
	    U_CHAR *save_bufp = ip->bufp;
	    ip->bufp = p + 1;
	    p = skip_to_end_of_comment (ip, &junk, 1);
	    ip->bufp = save_bufp;
	  } else
	    goto fail;
	}
      }
      /* If we get here, this #endif ends a #ifndef
	 that contains all of the file (aside from whitespace).
	 Arrange not to include the file again
	 if the macro that was tested is defined.

	 Do not do this for the top-level file in a -include or any
	 file in a -imacros.  */
      if (indepth != 0
	  && ! (indepth == 1 && no_record_file)
	  && ! (no_record_file && no_output))
	record_control_macro (ip->inc, temp->control_macro);
    fail: ;
    }
    free (temp);
    output_line_directive (&instack[indepth], op, 1, same_file);
  }
  return 0;
}

/* When an #else or #endif is found while skipping failed conditional,
   if -pedantic was specified, this is called to warn about text after
   the directive name.  P points to the first char after the directive
   name.  */

static void
validate_else (p, limit)
     register const U_CHAR *p;
     register const U_CHAR *limit;
{
  /* Advance P over whitespace and comments.  */
  while (1) {
    while (*p == '\\' && p[1] == '\n')
      p += 2;
    if (is_hor_space[*p])
      p++;
    else if (*p == '/') {
      while (p[1] == '\\' && p[2] == '\n')
	p += 2;
      if (p[1] == '*') {
	/* Don't bother warning about unterminated comments
	   since that will happen later.  Just be sure to exit.  */
	for (p += 2; ; p++) {
	  if (p == limit)
	    return;
	  if (*p == '*') {
	    while (p[1] == '\\' && p[2] == '\n')
	      p += 2;
	    if (p[1] == '/') {
	      p += 2;
	      break;
	    }
	  }
	  else
	    {
#ifdef MULTIBYTE_CHARS
	      int length;
	      length = local_mblen (p, limit - p);
	      if (length > 1)
		p += (length - 1);
#endif
	    }
	}
      }
      else if (cplusplus_comments && p[1] == '/')
	return;
      else break;
    } else break;
  }
  if (*p != '\n')
    pedwarn ("text following `#else' or `#endif' violates ANSI standard");
}

/* Skip a comment, assuming the input ptr immediately follows the
   initial slash-star.  Bump *LINE_COUNTER for each newline.
   (The canonical line counter is &ip->lineno.)
   Don't use this routine (or the next one) if bumping the line
   counter is not sufficient to deal with newlines in the string.

   If NOWARN is nonzero, don't warn about slash-star inside a comment.
   This feature is useful when processing a comment that is going to
   be processed or was processed at another point in the preprocessor,
   to avoid a duplicate warning.  Likewise for unterminated comment
   errors.  */

static U_CHAR *
skip_to_end_of_comment (ip, line_counter, nowarn)
     register FILE_BUF *ip;
     int *line_counter;		/* place to remember newlines, or NULL */
     int nowarn;
{
  register U_CHAR *limit = ip->buf + ip->length;
  register U_CHAR *bp = ip->bufp;
  FILE_BUF *op = put_out_comments && !line_counter ? &outbuf : (FILE_BUF *) 0;
  int start_line = line_counter ? *line_counter : 0;

	/* JF this line_counter stuff is a crock to make sure the
	   comment is only put out once, no matter how many times
	   the comment is skipped.  It almost works */
  if (op) {
    *op->bufp++ = '/';
    *op->bufp++ = bp[-1];
  }
  if (cplusplus_comments && bp[-1] == '/') {
    for (; bp < limit; bp++) {
      if (*bp == '\n')
	break;
      if (*bp == '\\' && bp + 1 < limit && bp[1] == '\n')
	{
	  if (!nowarn && warn_comments)
	    warning ("multiline `//' comment");
	  if (line_counter)
	    ++*line_counter;
	  if (op)
	    {
	      ++op->lineno;
	      *op->bufp++ = *bp;
	    }
	  ++bp;
	}
      else
	{
#ifdef MULTIBYTE_CHARS
	  int length;
	  length = local_mblen (bp, limit - bp);
	  if (length > 1)
	    {
	      if (op)
		{
		  bcopy (bp, op->bufp, length - 1);
		  op->bufp += (length - 1);
		}
	      bp += (length - 1);
	    }
#endif
	}
      if (op)
	*op->bufp++ = *bp;
    }
    ip->bufp = bp;
    return bp;
  }
  while (bp < limit) {
    if (op)
      *op->bufp++ = *bp;
    switch (*bp++) {
    case '\n':
      /* If this is the end of the file, we have an unterminated comment.
	 Don't swallow the newline.  We are guaranteed that there will be a
	 trailing newline and various pieces assume it's there.  */
      if (bp == limit)
	{
	  --bp;
	  --limit;
	  break;
	}
      if (line_counter != NULL)
	++*line_counter;
      if (op)
	++op->lineno;
      break;
    case '*':
      if (bp[-2] == '/' && !nowarn && warn_comments)
	warning ("`/*' within comment");
      if (*bp == '\\')
	newline_fix (bp);
      if (*bp == '/') {
        if (op)
	  *op->bufp++ = '/';
	ip->bufp = ++bp;
	return bp;
      }
      break;
#ifdef MULTIBYTE_CHARS
    default:
      {
	int length;
	bp--;
	length = local_mblen (bp, limit - bp);
	if (length <= 0)
	  length = 1;
	if (op)
	  {
	    op->bufp--;
	    bcopy (bp, op->bufp, length);
	    op->bufp += length;
	  }
	bp += length;
      }
#endif
    }
  }

  if (!nowarn)
    error_with_line (line_for_error (start_line), "unterminated comment");
  ip->bufp = bp;
  return bp;
}

/* Skip over a quoted string.  BP points to the opening quote.
   Returns a pointer after the closing quote.  Don't go past LIMIT.
   START_LINE is the line number of the starting point (but it need
   not be valid if the starting point is inside a macro expansion).

   The input stack state is not changed.

   If COUNT_NEWLINES is nonzero, it points to an int to increment
   for each newline passed; also, warn about any white space
   just before line end.

   If BACKSLASH_NEWLINES_P is nonzero, store 1 thru it
   if we pass a backslash-newline.

   If EOFP is nonzero, set *EOFP to 1 if the string is unterminated.  */

static U_CHAR *
skip_quoted_string (bp, limit, start_line, count_newlines, backslash_newlines_p, eofp)
     register const U_CHAR *bp;
     register const U_CHAR *limit;
     int start_line;
     int *count_newlines;
     int *backslash_newlines_p;
     int *eofp;
{
  register U_CHAR c, match;

  match = *bp++;
  while (1) {
    if (bp >= limit) {
      error_with_line (line_for_error (start_line),
		       "unterminated string or character constant");
      error_with_line (multiline_string_line,
		       "possible real start of unterminated constant");
      multiline_string_line = 0;
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
      if (*bp == '\n') {
	if (backslash_newlines_p)
	  *backslash_newlines_p = 1;
	if (count_newlines)
	  ++*count_newlines;
      }
      bp++;
    } else if (c == '\n') {
      if (traditional) {
 	/* Unterminated strings and character constants are 'valid'.  */
 	bp--;	/* Don't consume the newline.  */
 	if (eofp)
 	  *eofp = 1;
 	break;
      }
      if (match == '\'') {
	error_with_line (line_for_error (start_line),
			 "unterminated character constant");
	bp--;
	if (eofp)
	  *eofp = 1;
	break;
      }
      /* If not traditional, then allow newlines inside strings.  */
      if (count_newlines) {
	if (warn_white_space && is_hor_space[bp[-2]])
	  warning ("white space at end of line in string");
	++*count_newlines;
      }
      if (multiline_string_line == 0) {
	if (pedantic)
	  pedwarn_with_line (line_for_error (start_line),
			     "string constant runs past end of line");
	multiline_string_line = start_line;
      }
    } else if (c == match)
      break;
#ifdef MULTIBYTE_CHARS
    {
      int length;
      --bp;
      length = local_mblen (bp, limit - bp);
      if (length <= 0)
	length = 1;
      bp += length;
    }
#endif
  }
  return (U_CHAR *) bp;
}

/* Place into DST a quoted string representing the string SRC.
   SRCLEN is the length of SRC; SRC may contain null bytes.
   Return the address of DST's terminating null.  */

static char *
quote_string (dst, src, srclen)
     char *dst;
     const char *src;
     size_t srclen;
{
  U_CHAR c;
  const char *srclim = src + srclen;

  *dst++ = '\"';
  while (src != srclim)
    switch ((c = *src++))
      {
      default:
        if (ISPRINT (c))
	  *dst++ = c;
	else
	  {
	    sprintf (dst, "\\%03o", c);
	    dst += 4;
	  }
	break;

      case '\"':
      case '\\':
	*dst++ = '\\';
	*dst++ = c;
	break;
      }
      
  *dst++ = '\"';
  *dst = '\0';
  return dst;
}

/* Skip across a group of balanced parens, starting from IP->bufp.
   IP->bufp is updated.  Use this with IP->bufp pointing at an open-paren.

   This does not handle newlines, because it's used for the arg of #if,
   where there aren't any newlines.  Also, backslash-newline can't appear.  */

static U_CHAR *
skip_paren_group (ip)
     register FILE_BUF *ip;
{
  U_CHAR *limit = ip->buf + ip->length;
  U_CHAR *p = ip->bufp;
  int depth = 0;
  int lines_dummy = 0;

  while (p != limit) {
    int c = *p++;
    switch (c) {
    case '(':
      depth++;
      break;

    case ')':
      depth--;
      if (depth == 0)
	return ip->bufp = p;
      break;

    case '/':
      if (*p == '*') {
	ip->bufp = p;
	p = skip_to_end_of_comment (ip, &lines_dummy, 0);
	p = ip->bufp;
      }

    case '"':
    case '\'':
      {
	int eofp = 0;
	p = skip_quoted_string (p - 1, limit, 0, NULL_PTR, NULL_PTR, &eofp);
	if (eofp)
	  return ip->bufp = p;
      }
      break;
    }
  }

  ip->bufp = p;
  return p;
}

/* Write out a #line directive, for instance, after an #include file.
   If CONDITIONAL is nonzero, we can omit the #line if it would
   appear to be a no-op, and we can output a few newlines instead
   if we want to increase the line number by a small amount.
   FILE_CHANGE says whether we are entering a file, leaving, or neither.  */

static void
output_line_directive (ip, op, conditional, file_change)
     FILE_BUF *ip, *op;
     int conditional;
     enum file_change_code file_change;
{
  int len;
  char *line_directive_buf, *line_end;

  if (no_line_directives
      || ip->fname == NULL
      || no_output) {
    op->lineno = ip->lineno;
    return;
  }

  if (conditional) {
    if (ip->lineno == op->lineno)
      return;

    /* If the inherited line number is a little too small,
       output some newlines instead of a #line directive.  */
    if (ip->lineno > op->lineno && ip->lineno < op->lineno + 8) {
      check_expand (op, 10);
      while (ip->lineno > op->lineno) {
	*op->bufp++ = '\n';
	op->lineno++;
      }
      return;
    }
  }

  /* Output a positive line number if possible.  */
  while (ip->lineno <= 0 && ip->bufp - ip->buf < ip->length
	 && *ip->bufp == '\n') {
    ip->lineno++;
    ip->bufp++;
  }

  line_directive_buf = (char *) alloca (4 * ip->nominal_fname_len + 100);
  sprintf (line_directive_buf, "# %d ", ip->lineno);
  line_end = quote_string (line_directive_buf + strlen (line_directive_buf),
			   ip->nominal_fname, ip->nominal_fname_len);
  if (file_change != same_file) {
    *line_end++ = ' ';
    *line_end++ = file_change == enter_file ? '1' : '2';
  }
  /* Tell cc1 if following text comes from a system header file.  */
  if (ip->system_header_p) {
    *line_end++ = ' ';
    *line_end++ = '3';
  }
#ifndef NO_IMPLICIT_EXTERN_C
  /* Tell cc1plus if following text should be treated as C.  */
  if (ip->system_header_p == 2 && cplusplus) {
    *line_end++ = ' ';
    *line_end++ = '4';
  }
#endif
  *line_end++ = '\n';
  len = line_end - line_directive_buf;
  check_expand (op, len + 1);
  if (op->bufp > op->buf && op->bufp[-1] != '\n')
    *op->bufp++ = '\n';
  bcopy ((const PTR) line_directive_buf, (PTR) op->bufp, len);
  op->bufp += len;
  op->lineno = ip->lineno;
}

/* This structure represents one parsed argument in a macro call.
   `raw' points to the argument text as written (`raw_length' is its length).
   `expanded' points to the argument's macro-expansion
   (its length is `expand_length', and its allocated size is `expand_size').
   `stringified_length_bound' is an upper bound on the length
   the argument would have if stringified.
   `use_count' is the number of times this macro arg is substituted
   into the macro.  If the actual use count exceeds 10, 
   the value stored is 10.
   `free1' and `free2', if nonzero, point to blocks to be freed
   when the macro argument data is no longer needed.
   `free_ptr', if nonzero, points to a value of instack[i].free_ptr
   where the raw field points somewhere into this string.  The purpose
   of this is to hold onto instack[i].buf for macro arguments, even
   when the element has been popped off the input stack.
*/

struct argdata {
  U_CHAR *raw, *expanded;
  int raw_length, expand_length, expand_size;
  int stringified_length_bound;
  U_CHAR *free1, *free2;
  U_CHAR *free_ptr;
  char newlines;
  char use_count;
};

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
  register U_CHAR *xbuf;
  int xbuf_len;
  int start_line = instack[indepth].lineno;
  int rest_args, rest_zero;

  CHECK_DEPTH (return;);

  /* it might not actually be a macro.  */
  if (hp->type != T_MACRO) {
    special_symbol (hp, op);
    return;
  }

  /* This macro is being used inside a #if, which means it must be */
  /* recorded as a precondition.  */
  if (pcp_inside_if && pcp_outfile && defn->predefined)
    dump_single_macro (hp, pcp_outfile);
  
  nargs = defn->nargs;

  if (nargs >= 0) {
    register int i;
    struct argdata *args;
    int parse_error = 0;

    args = (struct argdata *) alloca ((nargs + 1) * sizeof (struct argdata));

    for (i = 0; i < nargs; i++) {
      args[i].raw = (U_CHAR *) "";
      args[i].expanded = 0;
      args[i].raw_length = args[i].expand_length = args[i].expand_size
	= args[i].stringified_length_bound = 0;
      args[i].free1 = args[i].free2 = 0;
      args[i].free_ptr = 0;
      args[i].use_count = 0;
    }

    /* Parse all the macro args that are supplied.  I counts them.
       The first NARGS args are stored in ARGS.
       The rest are discarded.
       If rest_args is set then we assume macarg absorbed the rest of the args.
       */
    i = 0;
    rest_args = 0;
    do {
      /* Discard the open-parenthesis or comma before the next arg.  */
      ++instack[indepth].bufp;
      if (rest_args)
	continue;
      if (i < nargs || (nargs == 0 && i == 0)) {
	/* If we are working on last arg which absorbs rest of args...  */
	if (i == nargs - 1 && defn->rest_args)
	  rest_args = 1;
	parse_error = macarg (&args[i], rest_args);
      }
      else
	parse_error = macarg (NULL_PTR, 0);
      if (parse_error) {
	error_with_line (line_for_error (start_line),
			 "unterminated macro call");
	break;
      }
      i++;
    } while (*instack[indepth].bufp != ')');

    /* If we got one arg but it was just whitespace, call that 0 args.  */
    if (i == 1) {
      register const U_CHAR *bp = args[0].raw;
      register const U_CHAR *lim = bp + args[0].raw_length;
      /* cpp.texi says for foo ( ) we provide one argument.
	 However, if foo wants just 0 arguments, treat this as 0.  */
      if (nargs == 0)
	while (bp != lim && is_space[*bp]) bp++;
      if (bp == lim)
	i = 0;
    }

    /* Don't output an error message if we have already output one for
       a parse error above.  */
    rest_zero = 0;
    if (nargs == 0 && i > 0) {
      if (! parse_error)
	error ("arguments given to macro `%s'", hp->name);
    } else if (i < nargs) {
      /* traditional C allows foo() if foo wants one argument.  */
      if (nargs == 1 && i == 0 && traditional)
	;
      /* the rest args token is allowed to absorb 0 tokens */
      else if (i == nargs - 1 && defn->rest_args)
	rest_zero = 1;
      else if (parse_error)
	;
      else if (i == 0)
	error ("macro `%s' used without args", hp->name);
      else if (i == 1)
	error ("macro `%s' used with just one arg", hp->name);
      else
	error ("macro `%s' used with only %d args", hp->name, i);
    } else if (i > nargs) {
      if (! parse_error)
	error ("macro `%s' used with too many (%d) args", hp->name, i);
    }

    /* Swallow the closeparen.  */
    ++instack[indepth].bufp;

    /* If macro wants zero args, we parsed the arglist for checking only.
       Read directly from the macro definition.  */
    if (nargs == 0) {
      xbuf = defn->expansion;
      xbuf_len = defn->length;
    } else {
      register U_CHAR *exp = defn->expansion;
      register int offset;	/* offset in expansion,
				   copied a piece at a time */
      register int totlen;	/* total amount of exp buffer filled so far */

      register struct reflist *ap, *last_ap;

      /* Macro really takes args.  Compute the expansion of this call.  */

      /* Compute length in characters of the macro's expansion.
	 Also count number of times each arg is used.  */
      xbuf_len = defn->length;
      for (ap = defn->pattern; ap != NULL; ap = ap->next) {
	if (ap->stringify && args[ap->argno].stringified_length_bound == 0)
	  /* macarg is not called for omitted arguments, as a result
	     stringified_length_bound will be zero.  We need to make
	     enough space for "".  */
	  xbuf_len += 2;
	else if (ap->stringify)
	  xbuf_len += args[ap->argno].stringified_length_bound;
	else if (ap->raw_before != 0 || ap->raw_after != 0 || traditional)
	  /* Add 4 for two newline-space markers to prevent
	     token concatenation.  */
	  xbuf_len += args[ap->argno].raw_length + 4;
	else {
	  /* We have an ordinary (expanded) occurrence of the arg.
	     So compute its expansion, if we have not already.  */
	  if (args[ap->argno].expanded == 0) {
	    FILE_BUF obuf;
	    obuf = expand_to_temp_buffer (args[ap->argno].raw,
					  args[ap->argno].raw + args[ap->argno].raw_length,
					  1, 0);

	    args[ap->argno].expanded = obuf.buf;
	    args[ap->argno].expand_length = obuf.bufp - obuf.buf;
	    args[ap->argno].expand_size = obuf.length;
	    args[ap->argno].free2 = obuf.buf;

	    xbuf_len += args[ap->argno].expand_length;
	  } else {
	    /* If the arg appears more than once, its later occurrences
	       may have newline turned into backslash-'n', which is a
	       factor of 2 expansion.  */
	    xbuf_len += 2 * args[ap->argno].expand_length;
	  }
	  /* Add 4 for two newline-space markers to prevent
	     token concatenation.  */
	  xbuf_len += 4;
	}
	if (args[ap->argno].use_count < 10)
	  args[ap->argno].use_count++;
      }

      xbuf = (U_CHAR *) xmalloc (xbuf_len + 1);

      /* Generate in XBUF the complete expansion
	 with arguments substituted in.
	 TOTLEN is the total size generated so far.
	 OFFSET is the index in the definition
	 of where we are copying from.  */
      offset = totlen = 0;
      for (last_ap = NULL, ap = defn->pattern; ap != NULL;
	   last_ap = ap, ap = ap->next) {
	register struct argdata *arg = &args[ap->argno];
	int count_before = totlen;

	/* Add chars to XBUF.  */
	for (i = 0; i < ap->nchars; i++, offset++)
	  xbuf[totlen++] = exp[offset];

	/* If followed by an empty rest arg with concatenation,
	   delete the last run of nonwhite chars.  */
	if (rest_zero && totlen > count_before
	    && ((ap->rest_args && ap->raw_before != 0)
		|| (last_ap != NULL && last_ap->rest_args
		    && last_ap->raw_after != 0))) {
	  /* Delete final whitespace.  */
	  while (totlen > count_before && is_space[xbuf[totlen - 1]]) {
	    totlen--;
	  }

	  /* Delete the nonwhites before them.  */
	  while (totlen > count_before && ! is_space[xbuf[totlen - 1]]) {
	    totlen--;
	  }
	}

	if (ap->stringify != 0) {
	  int arglen = arg->raw_length;
	  int escaped = 0;
	  int in_string = 0;
	  int c;
	  i = 0;
	  while (i < arglen
		 && (c = arg->raw[i], is_space[c]))
	    i++;
	  while (i < arglen
		 && (c = arg->raw[arglen - 1], is_space[c]))
	    arglen--;
	  if (!traditional)
	    xbuf[totlen++] = '\"'; /* insert beginning quote */
	  for (; i < arglen; i++) {
	    c = arg->raw[i];

	    if (in_string) {
	      /* Generate nothing for backslash-newline in a string.  */
	      if (c == '\\' && arg->raw[i + 1] == '\n') {
		i++;
		continue;
	      }
	    } else {
	      /* Special markers
		 generate nothing for a stringified argument.  */
	      if (c == '\n') {
		i++;
		continue;
	      }

	      /* Internal sequences of whitespace are replaced by one space
		 except within a string or char token.  */
	      if (is_space[c]) {
		i++;
		while (is_space[(c = arg->raw[i])])
		  /* Newline markers can occur within a whitespace sequence;
		     consider them part of the sequence.  */
		  i += (c == '\n') + 1;
		i--;
		c = ' ';
	      }
	    }

	    if (escaped)
	      escaped = 0;
	    else {
	      if (c == '\\')
		escaped = 1;
	      else if (in_string) {
		if (c == in_string)
		  in_string = 0;
		else
		  {
#ifdef MULTIBYTE_CHARS
		    int length;
		    length = local_mblen (arg->raw + i, arglen - i);
		    if (length > 1)
		      {
			bcopy (arg->raw + i, xbuf + totlen, length);
			i += length - 1;
			totlen += length;
			continue;
		      }
#endif
		  }
	      } else if (c == '\"' || c == '\'')
		in_string = c;
	    }

	    /* Escape double-quote, and backslashes in strings.
	       Newlines in strings are best escaped as \n, since
	       otherwise backslash-backslash-newline-newline is
	       mishandled.  The C Standard doesn't allow newlines in
	       strings, so we can escape newlines as we please.  */
	    if (c == '\"'
		|| (in_string
		    && (c == '\\'
			|| (c == '\n' ? (c = 'n', 1) : 0))))
	      xbuf[totlen++] = '\\';
	    /* We used to output e.g. \008 for control characters here,
	       but this doesn't conform to the C Standard.
	       Just output the characters as-is.  */
	    xbuf[totlen++] = c;
	  }
	  if (!traditional)
	    xbuf[totlen++] = '\"'; /* insert ending quote */
	} else if (ap->raw_before != 0 || ap->raw_after != 0 || traditional) {
	  const U_CHAR *p1 = arg->raw;
	  const U_CHAR *l1 = p1 + arg->raw_length;
	  if (ap->raw_before != 0) {
	    while (p1 != l1 && is_space[*p1]) p1++;
	    while (p1 != l1 && is_idchar[*p1])
	      xbuf[totlen++] = *p1++;
	    /* Delete any no-reexpansion marker that follows
	       an identifier at the beginning of the argument
	       if the argument is concatenated with what precedes it.  */
	    if (p1[0] == '\n' && p1[1] == '-')
	      p1 += 2;
	  } else if (!traditional) {
	  /* Ordinary expanded use of the argument.
	     Put in newline-space markers to prevent token pasting.  */
	    xbuf[totlen++] = '\n';
	    xbuf[totlen++] = ' ';
	  }
	  if (ap->raw_after != 0) {
	    /* Arg is concatenated after: delete trailing whitespace,
	       whitespace markers, and no-reexpansion markers.  */
	    while (p1 != l1) {
	      if (is_space[l1[-1]]) l1--;
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

	  bcopy ((const PTR) p1, (PTR) (xbuf + totlen), l1 - p1);
	  totlen += l1 - p1;
	  if (!traditional && ap->raw_after == 0) {
	    /* Ordinary expanded use of the argument.
	       Put in newline-space markers to prevent token pasting.  */
	    xbuf[totlen++] = '\n';
	    xbuf[totlen++] = ' ';
	  }
	} else {
	  /* Ordinary expanded use of the argument.
	     Put in newline-space markers to prevent token pasting.  */
	  if (!traditional) {
	    xbuf[totlen++] = '\n';
	    xbuf[totlen++] = ' ';
	  }
	  bcopy ((const PTR) arg->expanded, (PTR) (xbuf + totlen),
		 arg->expand_length);
	  totlen += arg->expand_length;
	  if (!traditional) {
	    xbuf[totlen++] = '\n';
	    xbuf[totlen++] = ' ';
	  }
	  /* If a macro argument with newlines is used multiple times,
	     then only expand the newlines once.  This avoids creating output
	     lines which don't correspond to any input line, which confuses
	     gdb and gcov.  */
	  if (arg->use_count > 1 && arg->newlines > 0) {
	    /* Don't bother doing change_newlines for subsequent
	       uses of arg.  */
	    arg->use_count = 1;
	    change_newlines (arg);
	  }
	}

	if (totlen > xbuf_len)
	  abort ();
      }

      /* If there is anything left of the definition after handling
	 the arg list, copy that in too.  */

      for (i = offset; i < defn->length; i++) {
	/* if we've reached the end of the macro */
	if (exp[i] == ')')
	  rest_zero = 0;
	if (! (rest_zero && last_ap != NULL && last_ap->rest_args
	       && last_ap->raw_after != 0))
	  xbuf[totlen++] = exp[i];
      }

      xbuf[totlen] = 0;
      xbuf_len = totlen;

      for (i = 0; i < nargs; i++) {
        if (args[i].free_ptr != 0) {
          U_CHAR *buf = args[i].free_ptr;
          int d;
          for (d = indepth; d >= 0; --d) {
            if (instack[d].buf == buf) {
              instack[d].free_ptr = buf; /* Give ownership back to instack */
              goto no_free;
            }
          }
          free (buf); /* buf is not on the stack; must have been popped */
        no_free:;
        }
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
    register FILE_BUF *ip2;

    ip2 = &instack[++indepth];

    ip2->fname = 0;
    ip2->nominal_fname = 0;
    ip2->nominal_fname_len = 0;
    ip2->inc = 0;
    /* This may not be exactly correct, but will give much better error
       messages for nested macro calls than using a line number of zero.  */
    ip2->lineno = start_line;
    ip2->buf = xbuf;
    ip2->length = xbuf_len;
    ip2->bufp = xbuf;
    ip2->free_ptr = (nargs > 0) ? xbuf : 0;
    ip2->macro = hp;
    ip2->if_stack = if_stack;
    ip2->system_header_p = 0;

    /* Recursive macro use sometimes works traditionally.
       #define foo(x,y) bar (x (y,0), y)
       foo (foo, baz)  */

    if (!traditional)
      hp->type = T_DISABLED;
  }
}

/* Parse a macro argument and store the info on it into *ARGPTR.
   REST_ARGS is passed to macarg1 to make it absorb the rest of the args.
   Return nonzero to indicate a syntax error.  */

static int
macarg (argptr, rest_args)
     register struct argdata *argptr;
     int rest_args;
{
  FILE_BUF *ip = &instack[indepth];
  int paren = 0;
  int lineno0 = ip->lineno;
  int comments = 0;
  int result = 0;

  /* Try to parse as much of the argument as exists at this
     input stack level.  */
  U_CHAR *bp = macarg1 (ip->bufp, ip->buf + ip->length, ip->macro,
			&paren, &ip->lineno, &comments, rest_args);

  /* If we find the end of the argument at this level,
     set up *ARGPTR to point at it in the input stack.  */
  if (!(ip->fname != 0 && (ip->lineno != lineno0 || comments != 0))
      && bp != ip->buf + ip->length) {
    if (argptr != 0) {
      argptr->raw = ip->bufp;
      argptr->raw_length = bp - ip->bufp;
      argptr->newlines = ip->lineno - lineno0;
      /* The next two statements transfer ownership of the the buffer
	 from ip to argptr.  Note that the second statement ensures that
	 a given free_ptr is owned by at most one macro argument. */
      argptr->free_ptr = ip->free_ptr;
      ip->free_ptr     = 0;
    }
    ip->bufp = bp;
  } else {
    /* This input stack level ends before the macro argument does.
       We must pop levels and keep parsing.
       Therefore, we must allocate a temporary buffer and copy
       the macro argument into it.  */
    int bufsize = bp - ip->bufp;
    int extra = ip->lineno - lineno0;
    U_CHAR *buffer = (U_CHAR *) xmalloc (bufsize + extra + 1);
    int final_start = 0;

    bcopy ((const PTR) ip->bufp, (PTR) buffer, bufsize);
    ip->bufp = bp;

    while (bp == ip->buf + ip->length) {
      if (instack[indepth].macro == 0) {
	result = 1;
	break;
      }
      ip->macro->type = T_MACRO;
      if (ip->free_ptr)
	free (ip->free_ptr);
      ip = &instack[--indepth];
      lineno0 = ip->lineno;
      comments = 0;
      bp = macarg1 (ip->bufp, ip->buf + ip->length, ip->macro, &paren,
		    &ip->lineno, &comments, rest_args);
      final_start = bufsize;
      bufsize += bp - ip->bufp;
      extra += ip->lineno - lineno0;
      buffer = (U_CHAR *) xrealloc (buffer, bufsize + extra + 1);
      bcopy ((const PTR) ip->bufp, (PTR) (buffer + bufsize - (bp - ip->bufp)),
	     bp - ip->bufp);
      ip->bufp = bp;
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
      argptr->newlines = ip->lineno - lineno0;
      if ((argptr->newlines || comments) && ip->fname != 0)
	argptr->raw_length
	  = final_start +
	    discard_comments (argptr->raw + final_start,
			      argptr->raw_length - final_start,
			      argptr->newlines);
      argptr->raw[argptr->raw_length] = 0;
      if (argptr->raw_length > bufsize + extra)
	abort ();
    }
  }

  /* If we are not discarding this argument,
     macroexpand it and compute its length as stringified.
     All this info goes into *ARGPTR.  */

  if (argptr != 0) {
    register const U_CHAR *buf, *lim;
    register int totlen;

    buf = argptr->raw;
    lim = buf + argptr->raw_length;

    while (buf != lim && is_space[*buf])
      buf++;
    while (buf != lim && is_space[lim[-1]])
      lim--;
    totlen = traditional ? 0 : 2;	/* Count opening and closing quote.  */
    while (buf != lim) {
      register U_CHAR c = *buf++;
      totlen++;
      /* Internal sequences of whitespace are replaced by one space
	 in most cases, but not always.  So count all the whitespace
	 in case we need to keep it all.  */
#if 0
      if (is_space[c])
	SKIP_ALL_WHITE_SPACE (buf);
      else
#endif
      if (c == '\"' || c == '\\' || c == '\n') /* escape these chars */
	totlen++;
    }
    argptr->stringified_length_bound = totlen;
  }
  return result;
}

/* Scan text from START (inclusive) up to LIMIT (exclusive),
   taken from the expansion of MACRO,
   counting parens in *DEPTHPTR,
   and return if reach LIMIT
   or before a `)' that would make *DEPTHPTR negative
   or before a comma when *DEPTHPTR is zero.
   Single and double quotes are matched and termination
   is inhibited within them.  Comments also inhibit it.
   Value returned is pointer to stopping place.

   Increment *NEWLINES each time a newline is passed.
   REST_ARGS notifies macarg1 that it should absorb the rest of the args.
   Set *COMMENTS to 1 if a comment is seen.  */

static U_CHAR *
macarg1 (start, limit, macro, depthptr, newlines, comments, rest_args)
     U_CHAR *start;
     register const U_CHAR *limit;
     struct hashnode *macro;
     int *depthptr, *newlines, *comments;
     int rest_args;
{
  register U_CHAR *bp = start;

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
      if (traditional && bp + 1 < limit && bp[1] != '\n')
	bp++;
      break;
    case '\n':
      ++*newlines;
      break;
    case '/':
      if (macro)
	break;
      if (bp[1] == '\\')
	newline_fix (bp + 1);
      if (bp[1] == '*') {
	*comments = 1;
	for (bp += 2; bp < limit; bp++) {
	  if (*bp == '\n')
	    ++*newlines;
	  else if (*bp == '*') {
	    if (bp[-1] == '/' && warn_comments)
	      warning ("`/*' within comment");
	    if (bp[1] == '\\')
	      newline_fix (bp + 1);
	    if (bp[1] == '/') {
	      bp++;
	      break;
	    }
	  }
	  else
	    {
#ifdef MULTIBYTE_CHARS
	      int length;
	      length = local_mblen (bp, limit - bp);
	      if (length > 1)
		bp += (length - 1);
#endif
	    }
	}
      } else if (bp[1] == '/' && cplusplus_comments) {
	*comments = 1;
	for (bp += 2; bp < limit; bp++) {
	  if (*bp == '\n') {
	    ++*newlines;
	    break;
	  }
	  if (*bp == '\\' && bp + 1 < limit && bp[1] == '\n')
	    {
	      ++*newlines;
	      if (warn_comments)
		warning ("multiline `//' comment");
	      ++bp;
	    }
	  else
	    {
#ifdef MULTIBYTE_CHARS
	      int length;
	      length = local_mblen (bp, limit - bp);
	      if (length > 1)
		bp += (length - 1);
#endif
	    }
	}
      }
      break;
    case '\'':
    case '\"':
      {
	int quotec;
	for (quotec = *bp++; bp < limit && *bp != quotec; bp++) {
	  if (*bp == '\\') {
	    bp++;
	    if (*bp == '\n')
	      ++*newlines;
	    while (*bp == '\\' && bp[1] == '\n') {
	      bp += 2;
	      ++*newlines;
	    }
	  } else if (*bp == '\n') {
	    if (warn_white_space && is_hor_space[bp[-1]] && ! macro)
	      warning ("white space at end of line in string");
	    ++*newlines;
	    if (quotec == '\'')
	      break;
	  }
	  else
	    {
#ifdef MULTIBYTE_CHARS
	      int length;
	      length = local_mblen (bp, limit - bp);
	      if (length > 1)
		bp += (length - 1);
#endif
	    }
	}
      }
      break;
    case ',':
      /* if we've returned to lowest level and we aren't absorbing all args */
      if ((*depthptr) == 0 && rest_args == 0)
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
  register U_CHAR *ibp;
  register U_CHAR *obp;
  register U_CHAR *limit;
  register int c;

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
      if (*ibp == '\\')
	newline_fix (ibp);
      /* Delete any comment.  */
      if (cplusplus_comments && ibp[0] == '/') {
	/* Comments are equivalent to spaces.  */
	obp[-1] = ' ';
	ibp++;
	while (ibp < limit)
	  {
	    if (*ibp == '\n')
	      break;
	    if (*ibp == '\\' && ibp + 1 < limit && ibp[1] == '\n')
	      ibp++;
	    else
	      {
#ifdef MULTIBYTE_CHARS
		int length = local_mblen (ibp, limit - ibp);
		if (length > 1)
		  ibp += (length - 1);
#endif
	      }
	    ibp++;
	  }
	break;
      }
      if (ibp[0] != '*' || ibp + 1 >= limit)
	break;
      /* Comments are equivalent to spaces.
	 For -traditional, a comment is equivalent to nothing.  */
      if (traditional)
	obp--;
      else
	obp[-1] = ' ';
      while (++ibp < limit) {
	if (ibp[0] == '*') {
	  if (ibp[1] == '\\')
	    newline_fix (ibp + 1);
	  if (ibp[1] == '/') {
	    ibp += 2;
	    break;
	  }
	}
	else
	  {
#ifdef MULTIBYTE_CHARS
	    int length = local_mblen (ibp, limit - ibp);
	    if (length > 1)
	      ibp += (length - 1);
#endif
	  }
      }
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
	  if (c == '\n')
	    {
	      if (quotec == '\'')
		break;
	    }
	  else if (c == '\\') {
	    if (ibp < limit && *ibp == '\n') {
	      ibp++;
	      obp--;
	    } else {
	      while (*ibp == '\\' && ibp[1] == '\n')
		ibp += 2;
	      if (ibp < limit)
		*obp++ = *ibp++;
	    }
	  }
	  else
	    {
#ifdef MULTIBYTE_CHARS
	      int length;
	      ibp--;
	      length = local_mblen (ibp, limit - ibp);
	      if (length > 1)
		{
		  obp--;
		  bcopy (ibp, obp, length);
		  ibp += length;
		  obp += length;
		}
	      else
		ibp++;
#endif
	    }
	}
      }
      break;
    }
  }

  return obp - start;
}

/* Turn newlines to spaces in the macro argument ARG.
   Remove backslash-newline from string constants,
   and turn other newlines in string constants to backslash-'n'.  */

static void
change_newlines (arg)
     struct argdata *arg;
{
  U_CHAR *start = arg->expanded;
  int length = arg->expand_length;
  register U_CHAR *ibp;
  register U_CHAR *obp;
  register const U_CHAR *limit;
  register int c;

  ibp = start;
  limit = start + length;
  obp = start;

  while (ibp < limit) {
    *obp++ = c = *ibp++;
    switch (c) {
    case '\n':
      /* If this is a NEWLINE NEWLINE, then this is a real newline in the
	 string.  Skip past the newline and its duplicate.
	 Put a space in the output.  */
      if (*ibp == '\n')
	{
	  ibp++;
	  obp--;
	  *obp++ = ' ';
	}
      break;

    case '\'':
    case '\"':
      /* Notice and skip strings, so that we don't delete newlines in them.  */
      {
	int quotec = c;
	while (ibp < limit) {
	  *obp++ = c = *ibp++;
	  if (c == quotec)
	    break;
	  else if (c == '\\' && ibp < limit && *ibp == '\n')
	    *obp++ = *ibp++;
	  else if (c == '\n')
	    {
	      if (quotec == '\'')
		break;
	    }
	  else
	    {
#ifdef MULTIBYTE_CHARS
	      int length;
	      ibp--;
	      length = local_mblen (ibp, limit - ibp);
	      if (length > 1)
		{
		  obp--;
		  bcopy (ibp, obp, length);
		  ibp += length;
		  obp += length;
		}
	      else
		ibp++;
#endif
	    }
	}
      }
      break;
    }
  }

  arg->expand_length = obp - arg->expanded;

  if (start != arg->expanded)
    free (start);
}

/* notice - output message to stderr */

static void
notice VPARAMS ((const char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, const char *);
#endif
 
  vnotice (msgid, args);
  va_end (args);
}

static void
vnotice (msgid, args)
     const char *msgid;
     va_list args;
{
  vfprintf (stderr, _(msgid), args);
}

/* error - print error message and increment count of errors.  */

void
error VPARAMS ((const char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, const char *);
#endif
 
  verror (msgid, args);
  va_end (args);
}

void
verror (msgid, args)
     const char *msgid;
     va_list args;
{
  int i;
  FILE_BUF *ip = NULL;

  print_containing_files ();

  for (i = indepth; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      break;
    }

  if (ip != NULL) {
    fwrite (ip->nominal_fname, sizeof ip->nominal_fname[0],
	    ip->nominal_fname_len, stderr);
    fprintf (stderr, ":%d: ", ip->lineno);
  }
  vnotice (msgid, args);
  fprintf (stderr, "\n");
  errors++;
}

/* Error including a message from `errno'.  */

static void
error_from_errno (name)
     const char *name;
{
  int e = errno;
  int i;
  FILE_BUF *ip = NULL;

  print_containing_files ();

  for (i = indepth; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      break;
    }

  if (ip != NULL) {
    fwrite (ip->nominal_fname, sizeof ip->nominal_fname[0],
	    ip->nominal_fname_len, stderr);
    fprintf (stderr, ":%d: ", ip->lineno);
  }

  fprintf (stderr, "%s: %s\n", name, xstrerror (e));

  errors++;
}

/* Print error message but don't count it.  */

void
warning VPARAMS ((const char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, const char *);
#endif

  vwarning (msgid, args);
  va_end (args);
}

static void
vwarning (msgid, args)
     const char *msgid;
     va_list args;
{
  int i;
  FILE_BUF *ip = NULL;

  if (inhibit_warnings)
    return;

  if (warnings_are_errors)
    errors++;

  print_containing_files ();

  for (i = indepth; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      break;
    }

  if (ip != NULL) {
    fwrite (ip->nominal_fname, sizeof ip->nominal_fname[0],
	    ip->nominal_fname_len, stderr);
    fprintf (stderr, ":%d: ", ip->lineno);
  }
  notice ("warning: ");
  vnotice (msgid, args);
  fprintf (stderr, "\n");
}

static void
error_with_line VPARAMS ((int line, const char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  int line;
  const char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  line = va_arg (args, int);
  msgid = va_arg (args, const char *);
#endif

  verror_with_line (line, msgid, args);
  va_end (args);
}


static void
verror_with_line (line, msgid, args)
     int line;
     const char *msgid;
     va_list args;
{
  int i;
  FILE_BUF *ip = NULL;

  print_containing_files ();

  for (i = indepth; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      break;
    }

  if (ip != NULL) {
    fwrite (ip->nominal_fname, sizeof ip->nominal_fname[0],
	    ip->nominal_fname_len, stderr);
    fprintf (stderr, ":%d: ", line);
  }
  vnotice (msgid, args);
  fprintf (stderr, "\n");
  errors++;
}

static void
warning_with_line VPARAMS ((int line, const char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  int line;
  const char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  line = va_arg (args, int);
  msgid = va_arg (args, const char *);
#endif

  vwarning_with_line (line, msgid, args);
  va_end (args);
}

static void
vwarning_with_line (line, msgid, args)
     int line;
     const char *msgid;
     va_list args;
{
  int i;
  FILE_BUF *ip = NULL;

  if (inhibit_warnings)
    return;

  if (warnings_are_errors)
    errors++;

  print_containing_files ();

  for (i = indepth; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      break;
    }

  if (ip != NULL) {
    fwrite (ip->nominal_fname, sizeof ip->nominal_fname[0],
	    ip->nominal_fname_len, stderr);
    if (line)
      fprintf (stderr, ":%d: ", line);
    else
      fputs (": ", stderr);
  }
  notice ("warning: ");
  vnotice (msgid, args);
  fprintf (stderr, "\n");
}

/* Print an error message and maybe count it.  */

void
pedwarn VPARAMS ((const char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, const char *);
#endif

  if (pedantic_errors)
    verror (msgid, args);
  else
    vwarning (msgid, args);
  va_end (args);
}

void
pedwarn_with_line VPARAMS ((int line, const char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  int line;
  const char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  line = va_arg (args, int);
  msgid = va_arg (args, const char *);
#endif

  if (pedantic_errors)
    verror_with_line (line, msgid, args);
  else
    vwarning_with_line (line, msgid, args);
  va_end (args);
}

/* Report a warning (or an error if pedantic_errors)
   giving specified file name and line number, not current.  */

static void
pedwarn_with_file_and_line VPARAMS ((const char *file, size_t file_len,
				     int line, const char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *file;
  size_t file_len;
  int line;
  const char * msgid;
#endif
  va_list args;

  if (!pedantic_errors && inhibit_warnings)
    return;

  VA_START (args, msgid);
 
#ifndef ANSI_PROTOTYPES
  file = va_arg (args, const char *);
  file_len = va_arg (args, size_t);
  line = va_arg (args, int);
  msgid = va_arg (args, const char *);
#endif
 
  if (file) {
    fwrite (file, sizeof file[0], file_len, stderr);
    fprintf (stderr, ":%d: ", line);
  }
  if (pedantic_errors)
    errors++;
  if (!pedantic_errors)
    notice ("warning: ");
  vnotice (msgid, args);
  va_end (args);
  fprintf (stderr, "\n");
}

static void
pedwarn_strange_white_space (ch)
     int ch;
{
  switch (ch)
    {
    case '\f': pedwarn ("formfeed in preprocessing directive"); break;
    case '\r': pedwarn ("carriage return in preprocessing directive"); break;
    case '\v': pedwarn ("vertical tab in preprocessing directive"); break;
    default: abort ();
    }
}

/* Print the file names and line numbers of the #include
   directives which led to the current file.  */

static void
print_containing_files ()
{
  FILE_BUF *ip = NULL;
  int i;
  int first = 1;

  /* If stack of files hasn't changed since we last printed
     this info, don't repeat it.  */
  if (last_error_tick == input_file_stack_tick)
    return;

  for (i = indepth; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      break;
    }

  /* Give up if we don't find a source file.  */
  if (ip == NULL)
    return;

  /* Find the other, outer source files.  */
  for (i--; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      if (first) {
	first = 0;
	notice (   "In file included from ");
      } else {
	notice (",\n                 from ");
      }

      fwrite (ip->nominal_fname, sizeof ip->nominal_fname[0],
	      ip->nominal_fname_len, stderr);
      fprintf (stderr, ":%d", ip->lineno);
    }
  if (! first)
    fprintf (stderr, ":\n");

  /* Record we have printed the status as of this time.  */
  last_error_tick = input_file_stack_tick;
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
  abort ();
  /*NOTREACHED*/
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

/* You might think void was cleaner for the return type,
   but that would get type mismatch in check_expand in strict ANSI.  */

static int
grow_outbuf (obuf, needed)
     register FILE_BUF *obuf;
     register int needed;
{
  register U_CHAR *p;
  int minsize;

  if (obuf->length - (obuf->bufp - obuf->buf) > needed)
    return 0;

  /* Make it at least twice as big as it is now.  */
  obuf->length *= 2;
  /* Make it have at least 150% of the free space we will need.  */
  minsize = (3 * needed) / 2 + (obuf->bufp - obuf->buf);
  if (minsize > obuf->length)
    obuf->length = minsize;

  p = (U_CHAR *) xrealloc (obuf->buf, obuf->length);

  obuf->bufp = p + (obuf->bufp - obuf->buf);
  obuf->buf = p;

  return 0;
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
 */

static HASHNODE *
install (name, len, type, value, hash)
     const U_CHAR *name;
     int len;
     enum node_type type;
     const char *value;
     int hash;
{
  register HASHNODE *hp;
  register int i, bucket;
  register U_CHAR *p;
  register const U_CHAR *q;

  if (len < 0) {
    q = name;
    while (is_idchar[*q])
      q++;
    len = q - name;
  }

  if (hash < 0)
    hash = hashf (name, len, HASHSIZE);

  i = sizeof (HASHNODE) + len + 1;
  hp = (HASHNODE *) xmalloc (i);
  bucket = hash;
  hp->bucket_hdr = &hashtab[bucket];
  hp->next = hashtab[bucket];
  hashtab[bucket] = hp;
  hp->prev = NULL;
  if (hp->next != NULL)
    hp->next->prev = hp;
  hp->type = type;
  hp->length = len;
  hp->value.cpval = value;
  hp->name = ((U_CHAR *) hp) + sizeof (HASHNODE);
  p = hp->name;
  q = name;
  for (i = 0; i < len; i++)
    *p++ = *q++;
  hp->name[len] = 0;
  return hp;
}

/*
 * find the most recent hash node for name "name" (ending with first
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
  register const U_CHAR *bp;
  register HASHNODE *bucket;

  if (len < 0) {
    for (bp = name; is_idchar[*bp]; bp++) ;
    len = bp - name;
  }

  if (hash < 0)
    hash = hashf (name, len, HASHSIZE);

  bucket = hashtab[hash];
  while (bucket) {
    if (bucket->length == len && bcmp (bucket->name, name, len) == 0)
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

  /* Make sure that the bucket chain header that the deleted guy was
     on points to the right thing afterwards.  */
  if (hp == *hp->bucket_hdr)
    *hp->bucket_hdr = hp->next;

#if 0
  if (hp->type == T_MACRO) {
    DEFINITION *d = hp->value.defn;
    struct reflist *ap, *nextap;

    for (ap = d->pattern; ap != NULL; ap = nextap) {
      nextap = ap->next;
      free (ap);
    }
    free (d);
  }
#endif
  free (hp);
}

/*
 * return hash function on name.  must be compatible with the one
 * computed a step at a time, elsewhere
 */

static int
hashf (name, len, hashsize)
     register const U_CHAR *name;
     register int len;
     int hashsize;
{
  register int r = 0;

  while (len--)
    r = HASHSTEP (r, *name++);

  return MAKE_POS (r) % hashsize;
}


/* Dump the definition of a single macro HP to OF.  */

static void
dump_single_macro (hp, of)
     register HASHNODE *hp;
     FILE *of;
{
  register DEFINITION *defn = hp->value.defn;
  struct reflist *ap;
  int offset;
  int concat;


  /* Print the definition of the macro HP.  */

  fprintf (of, "#define %s", hp->name);

  if (defn->nargs >= 0) {
    int i;

    fprintf (of, "(");
    for (i = 0; i < defn->nargs; i++) {
      dump_arg_n (defn, i, of);
      if (i + 1 < defn->nargs)
	fprintf (of, ", ");
    }
    fprintf (of, ")");
  }

  fprintf (of, " ");

  offset = 0;
  concat = 0;
  for (ap = defn->pattern; ap != NULL; ap = ap->next) {
    dump_defn_1 (defn->expansion, offset, ap->nchars, of);
    offset += ap->nchars;
    if (!traditional) {
      if (ap->nchars != 0)
	concat = 0;
      if (ap->stringify) {
	switch (ap->stringify) {
	 case SHARP_TOKEN: fprintf (of, "#"); break;
	 case WHITE_SHARP_TOKEN: fprintf (of, "# "); break;
	 case PERCENT_COLON_TOKEN: fprintf (of, "%%:"); break;
	 case WHITE_PERCENT_COLON_TOKEN: fprintf (of, "%%: "); break;
	 default: abort ();
	}
      }
      if (ap->raw_before != 0) {
	if (concat) {
	  switch (ap->raw_before) {
	   case WHITE_SHARP_TOKEN:
	   case WHITE_PERCENT_COLON_TOKEN:
	    fprintf (of, " ");
	    break;
	   default:
	    break;
	  }
	} else {
	  switch (ap->raw_before) {
	   case SHARP_TOKEN: fprintf (of, "##"); break;
	   case WHITE_SHARP_TOKEN: fprintf (of, "## "); break;
	   case PERCENT_COLON_TOKEN: fprintf (of, "%%:%%:"); break;
	   case WHITE_PERCENT_COLON_TOKEN: fprintf (of, "%%:%%: "); break;
	   default: abort ();
	  }
	}
      }
      concat = 0;
    }
    dump_arg_n (defn, ap->argno, of);
    if (!traditional && ap->raw_after != 0) {
      switch (ap->raw_after) {
       case SHARP_TOKEN: fprintf (of, "##"); break;
       case WHITE_SHARP_TOKEN: fprintf (of, " ##"); break;
       case PERCENT_COLON_TOKEN: fprintf (of, "%%:%%:"); break;
       case WHITE_PERCENT_COLON_TOKEN: fprintf (of, " %%:%%:"); break;
       default: abort ();
      }
      concat = 1;
    }
  }
  dump_defn_1 (defn->expansion, offset, defn->length - offset, of);
  fprintf (of, "\n");
}

/* Dump all macro definitions as #defines to stdout.  */

static void
dump_all_macros ()
{
  int bucket;

  for (bucket = 0; bucket < HASHSIZE; bucket++) {
    register HASHNODE *hp;

    for (hp = hashtab[bucket]; hp; hp= hp->next) {
      if (hp->type == T_MACRO)
	dump_single_macro (hp, stdout);
    }
  }
}

/* Output to OF a substring of a macro definition.
   BASE is the beginning of the definition.
   Output characters START thru LENGTH.
   Unless traditional, discard newlines outside of strings, thus
   converting funny-space markers to ordinary spaces.  */

static void
dump_defn_1 (base, start, length, of)
     const U_CHAR *base;
     int start;
     int length;
     FILE *of;
{
  const U_CHAR *p = base + start;
  const U_CHAR *limit = base + start + length;

  if (traditional)
    fwrite (p, sizeof (*p), length, of);
  else {
    while (p < limit) {
      if (*p == '\"' || *p =='\'') {
	const U_CHAR *p1 = skip_quoted_string (p, limit, 0, NULL_PTR,
					       NULL_PTR, NULL_PTR);
	fwrite (p, sizeof (*p), p1 - p, of);
	p = p1;
      } else {
	if (*p != '\n')
	  putc (*p, of);
	p++;
      }
    }
  }
}

/* Print the name of argument number ARGNUM of macro definition DEFN
   to OF.
   Recall that DEFN->args.argnames contains all the arg names
   concatenated in reverse order with comma-space in between.  */

static void
dump_arg_n (defn, argnum, of)
     DEFINITION *defn;
     int argnum;
     FILE *of;
{
  register U_CHAR *p = defn->args.argnames;
  while (argnum + 1 < defn->nargs) {
    p = (U_CHAR *) index ((char *) p, ' ') + 1;
    argnum++;
  }

  while (*p && *p != ',') {
    putc (*p, of);
    p++;
  }
}

/* Initialize syntactic classifications of characters.  */

static void
initialize_char_syntax ()
{
  register int i;

  /*
   * Set up is_idchar and is_idstart tables.  These should be
   * faster than saying (is_alpha (c) || c == '_'), etc.
   * Set up these things before calling any routines tthat
   * refer to them.
   */
  for (i = 'a'; i <= 'z'; i++) {
    is_idchar[TOUPPER(i)] = 1;
    is_idchar[i] = 1;
    is_idstart[TOUPPER(i)] = 1;
    is_idstart[i] = 1;
  }
  for (i = '0'; i <= '9'; i++)
    is_idchar[i] = 1;
  is_idchar['_'] = 1;
  is_idstart['_'] = 1;
  is_idchar['$'] = 1;
  is_idstart['$'] = 1;

  /* horizontal space table */
  is_hor_space[' '] = 1;
  is_hor_space['\t'] = 1;
  is_hor_space['\v'] = 1;
  is_hor_space['\f'] = 1;
  is_hor_space['\r'] = 1;

  is_space[' '] = 1;
  is_space['\t'] = 1;
  is_space['\v'] = 1;
  is_space['\f'] = 1;
  is_space['\n'] = 1;
  is_space['\r'] = 1;
}

/* Initialize the built-in macros.  */

static void
initialize_builtins (inp, outp)
     FILE_BUF *inp;
     FILE_BUF *outp;
{
  install ((const U_CHAR *) "__LINE__", -1, T_SPECLINE, NULL_PTR, -1);
  install ((const U_CHAR *) "__DATE__", -1, T_DATE, NULL_PTR, -1);
  install ((const U_CHAR *) "__FILE__", -1, T_FILE, NULL_PTR, -1);
  install ((const U_CHAR *) "__BASE_FILE__", -1, T_BASE_FILE, NULL_PTR, -1);
  install ((const U_CHAR *) "__INCLUDE_LEVEL__", -1, T_INCLUDE_LEVEL, NULL_PTR, -1);
  install ((const U_CHAR *) "__VERSION__", -1, T_VERSION, NULL_PTR, -1);
#ifndef NO_BUILTIN_SIZE_TYPE
  install ((const U_CHAR *) "__SIZE_TYPE__", -1, T_SIZE_TYPE, NULL_PTR, -1);
#endif
#ifndef NO_BUILTIN_PTRDIFF_TYPE
  install ((const U_CHAR *) "__PTRDIFF_TYPE__ ", -1, T_PTRDIFF_TYPE, NULL_PTR, -1);
#endif
  install ((const U_CHAR *) "__WCHAR_TYPE__", -1, T_WCHAR_TYPE, NULL_PTR, -1);
  install ((const U_CHAR *) "__USER_LABEL_PREFIX__", -1, T_USER_LABEL_PREFIX_TYPE,
	   NULL_PTR, -1);
  install ((const U_CHAR *) "__REGISTER_PREFIX__", -1, T_REGISTER_PREFIX_TYPE,
	   NULL_PTR, -1);
  install ((const U_CHAR *) "__IMMEDIATE_PREFIX__", -1, T_IMMEDIATE_PREFIX_TYPE,
	   NULL_PTR, -1);
  install ((const U_CHAR *) "__TIME__", -1, T_TIME, NULL_PTR, -1);
  if (!traditional) {
    install ((const U_CHAR *) "__STDC__", -1, T_CONST, "1", -1);
    install ((const U_CHAR *) "__STDC_VERSION__", -1, T_CONST, "199409L", -1);
  }
/*  This is supplied using a -D by the compiler driver
    so that it is present only when truly compiling with GNU C.  */
/*  install ((U_CHAR *) "__GNUC__", -1, T_CONST, "2", -1);  */
  install ((const U_CHAR *) "__HAVE_BUILTIN_SETJMP__", -1, T_CONST, "1", -1);

  if (debug_output)
    {
      char directive[2048];
      U_CHAR *udirective = (U_CHAR *) directive;
      register struct directive *dp = &directive_table[0];
      struct tm *timebuf = timestamp ();

      sprintf (directive, " __BASE_FILE__ \"%s\"\n",
	       instack[0].nominal_fname);
      output_line_directive (inp, outp, 0, same_file);
      pass_thru_directive (udirective, &udirective[strlen (directive)],
			   outp, dp);

      sprintf (directive, " __VERSION__ \"%s\"\n", version_string);
      output_line_directive (inp, outp, 0, same_file);
      pass_thru_directive (udirective, &udirective[strlen (directive)],
			   outp, dp);

#ifndef NO_BUILTIN_SIZE_TYPE
      sprintf (directive, " __SIZE_TYPE__ %s\n", SIZE_TYPE);
      output_line_directive (inp, outp, 0, same_file);
      pass_thru_directive (udirective, &udirective[strlen (directive)],
			   outp, dp);
#endif

#ifndef NO_BUILTIN_PTRDIFF_TYPE
      sprintf (directive, " __PTRDIFF_TYPE__ %s\n", PTRDIFF_TYPE);
      output_line_directive (inp, outp, 0, same_file);
      pass_thru_directive (udirective, &udirective[strlen (directive)],
			   outp, dp);
#endif

      sprintf (directive, " __WCHAR_TYPE__ %s\n", wchar_type);
      output_line_directive (inp, outp, 0, same_file);
      pass_thru_directive (udirective, &udirective[strlen (directive)],
			   outp, dp);

      sprintf (directive, " __DATE__ \"%s %2d %4d\"\n",
	       monthnames[timebuf->tm_mon],
	       timebuf->tm_mday, timebuf->tm_year + 1900);
      output_line_directive (inp, outp, 0, same_file);
      pass_thru_directive (udirective, &udirective[strlen (directive)],
			   outp, dp);

      sprintf (directive, " __TIME__ \"%02d:%02d:%02d\"\n",
	       timebuf->tm_hour, timebuf->tm_min, timebuf->tm_sec);
      output_line_directive (inp, outp, 0, same_file);
      pass_thru_directive (udirective, &udirective[strlen (directive)],
			   outp, dp);

      if (!traditional)
	{
          sprintf (directive, " __STDC__ 1");
          output_line_directive (inp, outp, 0, same_file);
          pass_thru_directive (udirective, &udirective[strlen (directive)],
			       outp, dp);
	}
      if (objc)
	{
          sprintf (directive, " __OBJC__ 1");
          output_line_directive (inp, outp, 0, same_file);
          pass_thru_directive (udirective, &udirective[strlen (directive)],
			       outp, dp);
	}
    }
}

/*
 * process a given definition string, for initialization
 * If STR is just an identifier, define it with value 1.
 * If STR has anything after the identifier, then it should
 * be identifier=definition.
 */

static void
make_definition (str)
     char *str;
{
  FILE_BUF *ip;
  struct directive *kt;
  U_CHAR *buf, *p;

  p = buf = (U_CHAR *) str;
  if (!is_idstart[*p]) {
    error ("malformed option `-D %s'", str);
    return;
  }
  while (is_idchar[*++p])
    ;
  if (*p == '(') {
    while (is_idchar[*++p] || *p == ',' || is_hor_space[*p])
      ;
    if (*p++ != ')')
      p = (U_CHAR *) str;			/* Error */
  }
  if (*p == 0) {
    buf = (U_CHAR *) alloca (p - buf + 4);
    strcpy ((char *)buf, str);
    strcat ((char *)buf, " 1");
  } else if (*p != '=') {
    error ("malformed option `-D %s'", str);
    return;
  } else {
    U_CHAR *q;
    /* Copy the entire option so we can modify it.  */
    buf = (U_CHAR *) alloca (2 * strlen (str) + 1);
    strncpy ((char *) buf, str, p - (U_CHAR *) str);
    /* Change the = to a space.  */
    buf[p - (U_CHAR *) str] = ' ';
    /* Scan for any backslash-newline and remove it.  */
    p++;
    q = &buf[p - (U_CHAR *) str];
    while (*p) {
      if (*p == '\"' || *p == '\'') {
	int unterminated = 0;
	const U_CHAR *p1 = skip_quoted_string (p, p + strlen ((char *) p), 0,
					 NULL_PTR, NULL_PTR, &unterminated);
	if (unterminated)
	  return;
	while (p != p1) {
	  if (*p == '\\' && p[1] == '\n')
	    p += 2;
	  else if (*p == '\n')
	    {
	      *q++ = '\\';
	      *q++ = 'n';
	      p++;
	    }
	  else
	    *q++ = *p++;
	}
      } else if (*p == '\\' && p[1] == '\n')
	p += 2;
      /* Change newline chars into newline-markers.  */
      else if (*p == '\n')
	{
	  *q++ = '\n';
	  *q++ = '\n';
	  p++;
	}
      else
	*q++ = *p++;
    }
    *q = 0;
  }
  
  ip = &instack[++indepth];
  ip->nominal_fname = ip->fname = "*Initialization*";
  ip->nominal_fname_len = strlen (ip->nominal_fname);

  ip->buf = ip->bufp = buf;
  ip->length = strlen ((char *) buf);
  ip->lineno = 1;
  ip->macro = 0;
  ip->free_ptr = 0;
  ip->if_stack = if_stack;
  ip->system_header_p = 0;

  for (kt = directive_table; kt->type != T_DEFINE; kt++)
    ;

  /* Pass NULL instead of OP, since this is a "predefined" macro.  */
  do_define (buf, buf + strlen ((char *) buf), NULL_PTR, kt);
  --indepth;
}

/* JF, this does the work for the -U option */

static void
make_undef (str, op)
     char *str;
     FILE_BUF *op;
{
  FILE_BUF *ip;
  struct directive *kt;

  ip = &instack[++indepth];
  ip->nominal_fname = ip->fname = "*undef*";
  ip->nominal_fname_len = strlen (ip->nominal_fname);

  ip->buf = ip->bufp = (U_CHAR *) str;
  ip->length = strlen (str);
  ip->lineno = 1;
  ip->macro = 0;
  ip->free_ptr = 0;
  ip->if_stack = if_stack;
  ip->system_header_p = 0;

  for (kt = directive_table; kt->type != T_UNDEF; kt++)
    ;

  do_undef ((U_CHAR *) str, (U_CHAR *) str + strlen (str), op, kt);
  --indepth;
}

/* Process the string STR as if it appeared as the body of a #assert.
   OPTION is the option name for which STR was the argument.  */

static void
make_assertion (option, str)
     const char *option;
     const char *str;
{
  FILE_BUF *ip;
  struct directive *kt;
  U_CHAR *buf, *p, *q;

  /* Copy the entire option so we can modify it.  */
  buf = (U_CHAR *) alloca (strlen (str) + 1);
  strcpy ((char *) buf, str);
  /* Scan for any backslash-newline and remove it.  */
  p = q = buf;
  while (*p) {
    if (*p == '\\' && p[1] == '\n')
      p += 2;
    else
      *q++ = *p++;
  }
  *q = 0;

  p = buf;
  if (!is_idstart[*p]) {
    error ("malformed option `%s %s'", option, str);
    return;
  }
  while (is_idchar[*++p])
    ;
  SKIP_WHITE_SPACE (p);
  if (! (*p == 0 || *p == '(')) {
    error ("malformed option `%s %s'", option, str);
    return;
  }
  
  ip = &instack[++indepth];
  ip->nominal_fname = ip->fname = "*Initialization*";
  ip->nominal_fname_len = strlen (ip->nominal_fname);

  ip->buf = ip->bufp = buf;
  ip->length = strlen ((char *) buf);
  ip->lineno = 1;
  ip->macro = 0;
  ip->free_ptr = 0;
  ip->if_stack = if_stack;
  ip->system_header_p = 0;

  for (kt = directive_table; kt->type != T_ASSERT; kt++)
    ;

  /* Pass NULL as output ptr to do_define since we KNOW it never does
     any output....  */
  do_assert (buf, buf + strlen ((char *) buf) , NULL_PTR, kt);
  --indepth;
}

/* The previous include prefix, if any, is PREV_FILE_NAME.
   Translate any pathnames with COMPONENT.
   Allocate a new include prefix whose name is the
   simplified concatenation of PREFIX and NAME,
   with a trailing / added if needed.
   But return 0 if the include prefix should be ignored,
   e.g. because it is a duplicate of PREV_FILE_NAME.  */

static struct file_name_list *
new_include_prefix (prev_file_name, component, prefix, name)
     struct file_name_list *prev_file_name;
     const char *component;
     const char *prefix;
     const char *name;
{
  if (name == 0)
    fatal ("Directory name missing after command line option");

  if (*name == 0)
    /* Ignore the empty string.  */
    return 0;

  prefix = update_path (prefix, component);
  name = update_path (name, component);

  {
    struct file_name_list *dir
      = ((struct file_name_list *)
	 xmalloc (sizeof (struct file_name_list)
		  + strlen (prefix) + strlen (name) + 2));
    size_t len;
    strcpy (dir->fname, prefix);
    strcat (dir->fname, name);
    len = simplify_filename (dir->fname);

    /* Convert directory name to a prefix.  */
    if (len && dir->fname[len - 1] != DIR_SEPARATOR) {
      if (len == 1 && dir->fname[len - 1] == '.')
	len = 0;
      else
#ifdef VMS
	/* must be '/', hack_vms_include_specification triggers on it.  */
	dir->fname[len++] = '/';
#else
	dir->fname[len++] = DIR_SEPARATOR;
#endif
      dir->fname[len] = 0;
    }

    /* Ignore a directory whose name matches the previous one.  */
    if (prev_file_name && !strcmp (prev_file_name->fname, dir->fname)) {
      /* But treat `-Idir -I- -Idir' as `-I- -Idir'.  */
      if (!first_bracket_include)
	first_bracket_include = prev_file_name;
      free (dir);
      return 0;
    }

#ifndef VMS
    /* VMS can't stat dir prefixes, so skip these optimizations in VMS.  */

    /* Add a trailing "." if there is a filename.  This increases the number
       of systems that can stat directories.  We remove it below.  */
    if (len != 0)
      {
	dir->fname[len] = '.';
	dir->fname[len + 1] = 0;
      }

    /* Ignore a nonexistent directory.  */
    if (stat (len ? dir->fname : ".", &dir->st) != 0) {
      if (errno != ENOENT && errno != ENOTDIR)
	error_from_errno (dir->fname);
      free (dir);
      return 0;
    }

    if (len != 0)
      dir->fname[len] = 0;

    /* Ignore a directory whose identity matches the previous one.  */
    if (prev_file_name
	&& INO_T_EQ (prev_file_name->st.st_ino, dir->st.st_ino)
	&& prev_file_name->st.st_dev == dir->st.st_dev) {
      /* But treat `-Idir -I- -Idir' as `-I- -Idir'.  */
      if (!first_bracket_include)
	first_bracket_include = prev_file_name;
      free (dir);
      return 0;
    }
#endif /* ! VMS */

    dir->next = 0;
    dir->c_system_include_path = 0;
    dir->got_name_map = 0;

    return dir;
  }
}

/* Append a chain of `struct file_name_list's
   to the end of the main include chain.
   FIRST is the beginning of the chain to append, and LAST is the end.  */

static void
append_include_chain (first, last)
     struct file_name_list *first, *last;
{
  struct file_name_list *dir;

  if (!first || !last)
    return;

  if (include == 0)
    include = first;
  else
    last_include->next = first;

  if (first_bracket_include == 0)
    first_bracket_include = first;

  for (dir = first; ; dir = dir->next) {
    int len = strlen (dir->fname) + INCLUDE_LEN_FUDGE;
    if (len > max_include_len)
      max_include_len = len;
    if (dir == last)
      break;
  }

  last->next = NULL;
  last_include = last;
}

/* Place into DST a representation of the file named SRC that is suitable
   for `make'.  Do not null-terminate DST.  Return its length.  */
static int
quote_string_for_make (dst, src)
     char *dst;
     const char *src;
{
  const char *p = src;
  int i = 0;
  for (;;)
    {
      char c = *p++;
      switch (c)
	{
	case '\0':
	case ' ':
	case '\t':
	  {
	    /* GNU make uses a weird quoting scheme for white space.
	       A space or tab preceded by 2N+1 backslashes represents
	       N backslashes followed by space; a space or tab
	       preceded by 2N backslashes represents N backslashes at
	       the end of a file name; and backslashes in other
	       contexts should not be doubled.  */
	    const char *q;
	    for (q = p - 1; src < q && q[-1] == '\\';  q--)
	      {
		if (dst)
		  dst[i] = '\\';
		i++;
	      }
	  }
	  if (!c)
	    return i;
	  if (dst)
	    dst[i] = '\\';
	  i++;
	  goto ordinary_char;
	  
	case '$':
	  if (dst)
	    dst[i] = c;
	  i++;
	  /* Fall through.  This can mishandle things like "$(" but
	     there's no easy fix.  */
	default:
	ordinary_char:
	  /* This can mishandle characters in the string "\0\n%*?[\\~";
	     exactly which chars are mishandled depends on the `make' version.
	     We know of no portable solution for this;
	     even GNU make 3.76.1 doesn't solve the problem entirely.
	     (Also, '\0' is mishandled due to our calling conventions.)  */
	  if (dst)
	    dst[i] = c;
	  i++;
	  break;
	}
    }
}


/* Add output to `deps_buffer' for the -M switch.
   STRING points to the text to be output.
   SPACER is ':' for targets, ' ' for dependencies.  */

static void
deps_output (string, spacer)
     const char *string;
     int spacer;
{
  int size = quote_string_for_make ((char *) 0, string);

  if (size == 0)
    return;

#ifndef MAX_OUTPUT_COLUMNS
#define MAX_OUTPUT_COLUMNS 72
#endif
  if (MAX_OUTPUT_COLUMNS - 1 /*spacer*/ - 2 /*` \'*/ < deps_column + size
      && 1 < deps_column) {
    bcopy (" \\\n ", &deps_buffer[deps_size], 4);
    deps_size += 4;
    deps_column = 1;
    if (spacer == ' ')
      spacer = 0;
  }

  if (deps_size + 2 * size + 8 > deps_allocated_size) {
    deps_allocated_size = (deps_size + 2 * size + 50) * 2;
    deps_buffer = xrealloc (deps_buffer, deps_allocated_size);
  }
  if (spacer == ' ') {
    deps_buffer[deps_size++] = ' ';
    deps_column++;
  }
  quote_string_for_make (&deps_buffer[deps_size], string);
  deps_size += size;
  deps_column += size;
  if (spacer == ':') {
    deps_buffer[deps_size++] = ':';
    deps_column++;
  }
  deps_buffer[deps_size] = 0;
}

void
fatal VPARAMS ((const char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char * msgid;
#endif
  va_list args;

  fprintf (stderr, "%s: ", progname);
  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, const char *);
#endif
  vnotice (msgid, args);
  va_end (args);
  fprintf (stderr, "\n");
  exit (FATAL_EXIT_CODE);
}

/* More 'friendly' abort that prints the line and file.
   config.h can #define abort fancy_abort if you like that sort of thing.  */

void
fancy_abort ()
{
  fatal ("Internal gcc abort.");
}

static void
perror_with_name (name)
     const char *name;
{
  fprintf (stderr, "%s: %s: %s\n", progname, name, xstrerror (errno));
  errors++;
}

static void
pfatal_with_name (name)
     const char *name;
{
  perror_with_name (name);
#ifdef VMS
  exit (vaxc$errno);
#else
  exit (FATAL_EXIT_CODE);
#endif
}

/* Handler for SIGPIPE.  */

static void
pipe_closed (signo)
     /* If this is missing, some compilers complain.  */
     int signo ATTRIBUTE_UNUSED;
{
  fatal ("output pipe has been closed");
}

static void
memory_full ()
{
  fatal ("Memory exhausted.");
}

#ifdef VMS

/* Under VMS we need to fix up the "include" specification filename.

   Rules for possible conversions

	fullname		tried paths

	name			name
	./dir/name		[.dir]name
	/dir/name		dir:name
	/name			[000000]name, name
	dir/name		dir:[000000]name, dir:name, dir/name
	dir1/dir2/name		dir1:[dir2]name, dir1:[000000.dir2]name
	path:/name		path:[000000]name, path:name
	path:/dir/name		path:[000000.dir]name, path:[dir]name
	path:dir/name		path:[dir]name
	[path]:[dir]name	[path.dir]name
	path/[dir]name		[path.dir]name

   The path:/name input is constructed when expanding <> includes.

   return 1 if name was changed, 0 else.  */

static int
hack_vms_include_specification (fullname, vaxc_include)
     char *fullname;
     int vaxc_include;
{
  register char *basename, *unixname, *local_ptr, *first_slash;
  int f, check_filename_before_returning, must_revert;
  char Local[512];

  check_filename_before_returning = 0;
  must_revert = 0;
  /* See if we can find a 1st slash. If not, there's no path information.  */
  first_slash = index (fullname, '/');
  if (first_slash == 0)
    return 0;				/* Nothing to do!!! */

  /* construct device spec if none given.  */

  if (index (fullname, ':') == 0)
    {

      /* If fullname has a slash, take it as device spec.  */

      if (first_slash == fullname)
	{
	  first_slash = index (fullname+1, '/');	/* 2nd slash ? */
	  if (first_slash)
	    *first_slash = ':';				/* make device spec  */
	  for (basename = fullname; *basename != 0; basename++)
	    *basename = *(basename+1);			/* remove leading slash  */
	}
      else if ((first_slash[-1] != '.')		/* keep ':/', './' */
	    && (first_slash[-1] != ':')
	    && (first_slash[-1] != ']'))	/* or a vms path  */
	{
	  *first_slash = ':';
	}
      else if ((first_slash[1] == '[')		/* skip './' in './[dir'  */
	    && (first_slash[-1] == '.'))
	fullname += 2;
    }

  /* Get part after first ':' (basename[-1] == ':')
     or last '/' (basename[-1] == '/').  */

  basename = base_name (fullname);

  /*
   * Check if we have a vax-c style '#include filename'
   * and add the missing .h
   */

  if (vaxc_include && !index (basename,'.'))
    strcat (basename, ".h");

  local_ptr = Local;			/* initialize */

  /* We are trying to do a number of things here.  First of all, we are
     trying to hammer the filenames into a standard format, such that later
     processing can handle them.
     
     If the file name contains something like [dir.], then it recognizes this
     as a root, and strips the ".]".  Later processing will add whatever is
     needed to get things working properly.
     
     If no device is specified, then the first directory name is taken to be
     a device name (or a rooted logical).  */

  /* Point to the UNIX filename part (which needs to be fixed!)
     but skip vms path information.
     [basename != fullname since first_slash != 0].  */

  if ((basename[-1] == ':')		/* vms path spec.  */
      || (basename[-1] == ']')
      || (basename[-1] == '>'))
    unixname = basename;
  else
    unixname = fullname;

  if (*unixname == '/')
    unixname++;

  /* If the directory spec is not rooted, we can just copy
     the UNIX filename part and we are done.  */

  if (((basename - fullname) > 1)
     && (  (basename[-1] == ']')
        || (basename[-1] == '>')))
    {
      if (basename[-2] != '.')
	{

	/* The VMS part ends in a `]', and the preceding character is not a `.'.
	   -> PATH]:/name (basename = '/name', unixname = 'name')
	   We strip the `]', and then splice the two parts of the name in the
	   usual way.  Given the default locations for include files in cccp.c,
	   we will only use this code if the user specifies alternate locations
	   with the /include (-I) switch on the command line.  */

	  basename -= 1;	/* Strip "]" */
	  unixname--;		/* backspace */
	}
      else
	{

	/* The VMS part has a ".]" at the end, and this will not do.  Later
	   processing will add a second directory spec, and this would be a syntax
	   error.  Thus we strip the ".]", and thus merge the directory specs.
	   We also backspace unixname, so that it points to a '/'.  This inhibits the
	   generation of the 000000 root directory spec (which does not belong here
	   in this case).  */

	  basename -= 2;	/* Strip ".]" */
	  unixname--;		/* backspace */
	}
    }

  else

    {

      /* We drop in here if there is no VMS style directory specification yet.
         If there is no device specification either, we make the first dir a
         device and try that.  If we do not do this, then we will be essentially
         searching the users default directory (as if they did a #include "asdf.h").
        
         Then all we need to do is to push a '[' into the output string. Later
         processing will fill this in, and close the bracket.  */

      if ((unixname != fullname)	/* vms path spec found.  */
	 && (basename[-1] != ':'))
	*local_ptr++ = ':';		/* dev not in spec.  take first dir */

      *local_ptr++ = '[';		/* Open the directory specification */
    }

    if (unixname == fullname)		/* no vms dir spec.  */
      {
	must_revert = 1;
	if ((first_slash != 0)		/* unix dir spec.  */
	    && (*unixname != '/')	/* not beginning with '/'  */
	    && (*unixname != '.'))	/* or './' or '../'  */
	  *local_ptr++ = '.';		/* dir is local !  */
      }

  /* at this point we assume that we have the device spec, and (at least
     the opening "[" for a directory specification.  We may have directories
     specified already.

     If there are no other slashes then the filename will be
     in the "root" directory.  Otherwise, we need to add
     directory specifications.  */

  if (index (unixname, '/') == 0)
    {
      /* if no directories specified yet and none are following.  */
      if (local_ptr[-1] == '[')
	{
	  /* Just add "000000]" as the directory string */
	  strcpy (local_ptr, "000000]");
	  local_ptr += strlen (local_ptr);
	  check_filename_before_returning = 1; /* we might need to fool with this later */
	}
    }
  else
    {

      /* As long as there are still subdirectories to add, do them.  */
      while (index (unixname, '/') != 0)
	{
	  /* If this token is "." we can ignore it
	       if it's not at the beginning of a path.  */
	  if ((unixname[0] == '.') && (unixname[1] == '/'))
	    {
	      /* remove it at beginning of path.  */
	      if (  ((unixname == fullname)		/* no device spec  */
		    && (fullname+2 != basename))	/* starts with ./ */
							/* or  */
		 || ((basename[-1] == ':')		/* device spec  */
		    && (unixname-1 == basename)))	/* and ./ afterwards  */
		*local_ptr++ = '.';		 	/* make '[.' start of path.  */
	      unixname += 2;
	      continue;
	    }

	  /* Add a subdirectory spec. Do not duplicate "." */
	  if (  local_ptr[-1] != '.'
	     && local_ptr[-1] != '['
	     && local_ptr[-1] != '<')
	    *local_ptr++ = '.';

	  /* If this is ".." then the spec becomes "-" */
	  if (  (unixname[0] == '.')
	     && (unixname[1] == '.')
	     && (unixname[2] == '/'))
	    {
	      /* Add "-" and skip the ".." */
	      if ((local_ptr[-1] == '.')
		  && (local_ptr[-2] == '['))
		local_ptr--;			/* prevent [.-  */
	      *local_ptr++ = '-';
	      unixname += 3;
	      continue;
	    }

	  /* Copy the subdirectory */
	  while (*unixname != '/')
	    *local_ptr++= *unixname++;

	  unixname++;			/* Skip the "/" */
	}

      /* Close the directory specification */
      if (local_ptr[-1] == '.')		/* no trailing periods */
	local_ptr--;

      if (local_ptr[-1] == '[')		/* no dir needed */
	local_ptr--;
      else
	*local_ptr++ = ']';
    }

  /* Now add the filename.  */

  while (*unixname)
    *local_ptr++ = *unixname++;
  *local_ptr = 0;

  /* Now append it to the original VMS spec.  */

  strcpy ((must_revert==1)?fullname:basename, Local);

  /* If we put a [000000] in the filename, try to open it first. If this fails,
     remove the [000000], and return that name.  This provides flexibility
     to the user in that they can use both rooted and non-rooted logical names
     to point to the location of the file.  */

  if (check_filename_before_returning)
    {
      f = open (fullname, O_RDONLY, 0666);
      if (f >= 0)
	{
	  /* The file name is OK as it is, so return it as is.  */
	  close (f);
	  return 1;
	}

      /* The filename did not work.  Try to remove the [000000] from the name,
	 and return it.  */

      basename = index (fullname, '[');
      local_ptr = index (fullname, ']') + 1;
      strcpy (basename, local_ptr);		/* this gets rid of it */

    }

  return 1;
}
#endif	/* VMS */

#ifdef	VMS

/* The following wrapper functions supply additional arguments to the VMS
   I/O routines to optimize performance with file handling.  The arguments
   are:
     "mbc=16" - Set multi-block count to 16 (use a 8192 byte buffer).
     "deq=64" - When extending the file, extend it in chunks of 32Kbytes.
     "fop=tef"- Truncate unused portions of file when closing file.
     "shr=nil"- Disallow file sharing while file is open.  */

static FILE *
VMS_freopen (fname, type, oldfile)
     char *fname;
     char *type;
     FILE *oldfile;
{
#undef freopen	/* Get back the real freopen routine.  */
  if (strcmp (type, "w") == 0)
    return freopen (fname, type, oldfile,
			 "mbc=16", "deq=64", "fop=tef", "shr=nil");
  return freopen (fname, type, oldfile, "mbc=16");
}

static FILE *
VMS_fopen (fname, type)
     char *fname;
     char *type;
{
#undef fopen	/* Get back the real fopen routine.  */
  /* The gcc-vms-1.42 distribution's header files prototype fopen with two
     fixed arguments, which matches ANSI's specification but not VAXCRTL's
     pre-ANSI implementation.  This hack circumvents the mismatch problem.  */
  FILE *(*vmslib_fopen)() = (FILE *(*)()) fopen;

  if (*type == 'w')
    return (*vmslib_fopen) (fname, type, "mbc=32",
			    "deq=64", "fop=tef", "shr=nil");
  else
    return (*vmslib_fopen) (fname, type, "mbc=32");
}

static int 
VMS_open (fname, flags, prot)
     char *fname;
     int flags;
     int prot;
{
#undef open	/* Get back the real open routine.  */
  return open (fname, flags, prot, "mbc=16", "deq=64", "fop=tef");
}

/* more VMS hackery */
#include <fab.h>
#include <nam.h>

extern unsigned long SYS$PARSE(), SYS$SEARCH();

/* Work around another library bug.  If a file is located via a searchlist,
   and if the device it's on is not the same device as the one specified
   in the first element of that searchlist, then both stat() and fstat()
   will fail to return info about it.  `errno' will be set to EVMSERR, and
   `vaxc$errno' will be set to SS$_NORMAL due yet another bug in stat()!
   We can get around this by fully parsing the filename and then passing
   that absolute name to stat().

   Without this fix, we can end up failing to find header files, which is
   bad enough, but then compounding the problem by reporting the reason for
   failure as "normal successful completion."  */

#undef fstat	/* Get back to the library version.  */

static int
VMS_fstat (fd, statbuf)
     int fd;
     struct stat *statbuf;
{
  int result = fstat (fd, statbuf);

  if (result < 0)
    {
      FILE *fp;
      char nambuf[NAM$C_MAXRSS+1];

      if ((fp = fdopen (fd, "r")) != 0 && fgetname (fp, nambuf) != 0)
	result = VMS_stat (nambuf, statbuf);
      /* No fclose(fp) here; that would close(fd) as well.  */
    }

  return result;
}

static int
VMS_stat (name, statbuf)
     const char *name;
     struct stat *statbuf;
{
  int result = stat (name, statbuf);

  if (result < 0)
    {
      struct FAB fab;
      struct NAM nam;
      char exp_nam[NAM$C_MAXRSS+1],  /* expanded name buffer for SYS$PARSE */
	   res_nam[NAM$C_MAXRSS+1];  /* resultant name buffer for SYS$SEARCH */

      fab = cc$rms_fab;
      fab.fab$l_fna = (char *) name;
      fab.fab$b_fns = (unsigned char) strlen (name);
      fab.fab$l_nam = (void *) &nam;
      nam = cc$rms_nam;
      nam.nam$l_esa = exp_nam,  nam.nam$b_ess = sizeof exp_nam - 1;
      nam.nam$l_rsa = res_nam,  nam.nam$b_rss = sizeof res_nam - 1;
      nam.nam$b_nop = NAM$M_PWD | NAM$M_NOCONCEAL;
      if (SYS$PARSE (&fab) & 1)
	{
	  if (SYS$SEARCH (&fab) & 1)
	    {
	      res_nam[nam.nam$b_rsl] = '\0';
	      result = stat (res_nam, statbuf);
	    }
	  /* Clean up searchlist context cached by the system.  */
	  nam.nam$b_nop = NAM$M_SYNCHK;
	  fab.fab$l_fna = 0,  fab.fab$b_fns = 0;
	  (void) SYS$PARSE (&fab);
	}
    }

  return result;
}

static size_t
VMS_fwrite (ptr, size, nitems, stream)
     void const *ptr;
     size_t size;
     size_t nitems;
     FILE *stream;
{
  /* VMS fwrite has undesirable results
     if STREAM happens to be a record oriented file.
     Work around this problem by writing each character individually.  */
  char const *p = ptr;
  size_t bytes = size * nitems;
  char *lim = p + bytes;

  while (p < lim)
    if (putc (*p++, stream) == EOF)
      return 0;

  return bytes;
}
#endif /* VMS */
