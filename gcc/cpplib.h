/* Definitions for CPP library.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
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
#ifndef GCC_CPPLIB_H
#define GCC_CPPLIB_H

#include <sys/types.h>
#include "hashtable.h"
#include "line-map.h"

#ifdef __cplusplus
extern "C" {
#endif

/* For complex reasons, cpp_reader is also typedefed in c-pragma.h.  */
#ifndef GCC_C_PRAGMA_H
typedef struct cpp_reader cpp_reader;
#endif
typedef struct cpp_buffer cpp_buffer;
typedef struct cpp_options cpp_options;
typedef struct cpp_token cpp_token;
typedef struct cpp_string cpp_string;
typedef struct cpp_hashnode cpp_hashnode;
typedef struct cpp_macro cpp_macro;
typedef struct cpp_callbacks cpp_callbacks;

struct answer;
struct file_name_map_list;

/* The first three groups, apart from '=', can appear in preprocessor
   expressions (+= and -= are used to indicate unary + and - resp.).
   This allows a lookup table to be implemented in _cpp_parse_expr.

   The first group, to CPP_LAST_EQ, can be immediately followed by an
   '='.  The lexer needs operators ending in '=', like ">>=", to be in
   the same order as their counterparts without the '=', like ">>".  */

/* Positions in the table.  */
#define CPP_LAST_EQ CPP_MAX
#define CPP_FIRST_DIGRAPH CPP_HASH
#define CPP_LAST_PUNCTUATOR CPP_DOT_STAR
#define CPP_LAST_CPP_OP CPP_LESS_EQ

#define TTYPE_TABLE				\
  OP(CPP_EQ = 0,	"=")			\
  OP(CPP_NOT,		"!")			\
  OP(CPP_GREATER,	">")	/* compare */	\
  OP(CPP_LESS,		"<")			\
  OP(CPP_PLUS,		"+")	/* math */	\
  OP(CPP_MINUS,		"-")			\
  OP(CPP_MULT,		"*")			\
  OP(CPP_DIV,		"/")			\
  OP(CPP_MOD,		"%")			\
  OP(CPP_AND,		"&")	/* bit ops */	\
  OP(CPP_OR,		"|")			\
  OP(CPP_XOR,		"^")			\
  OP(CPP_RSHIFT,	">>")			\
  OP(CPP_LSHIFT,	"<<")			\
  OP(CPP_MIN,		"<?")	/* extension */	\
  OP(CPP_MAX,		">?")			\
\
  OP(CPP_COMPL,		"~")			\
  OP(CPP_AND_AND,	"&&")	/* logical */	\
  OP(CPP_OR_OR,		"||")			\
  OP(CPP_QUERY,		"?")			\
  OP(CPP_COLON,		":")			\
  OP(CPP_COMMA,		",")	/* grouping */	\
  OP(CPP_OPEN_PAREN,	"(")			\
  OP(CPP_CLOSE_PAREN,	")")			\
  TK(CPP_EOF,		SPELL_NONE)		\
  OP(CPP_EQ_EQ,		"==")	/* compare */	\
  OP(CPP_NOT_EQ,	"!=")			\
  OP(CPP_GREATER_EQ,	">=")			\
  OP(CPP_LESS_EQ,	"<=")			\
\
  /* These two are unary + / - in preprocessor expressions.  */ \
  OP(CPP_PLUS_EQ,	"+=")	/* math */	\
  OP(CPP_MINUS_EQ,	"-=")			\
\
  OP(CPP_MULT_EQ,	"*=")			\
  OP(CPP_DIV_EQ,	"/=")			\
  OP(CPP_MOD_EQ,	"%=")			\
  OP(CPP_AND_EQ,	"&=")	/* bit ops */	\
  OP(CPP_OR_EQ,		"|=")			\
  OP(CPP_XOR_EQ,	"^=")			\
  OP(CPP_RSHIFT_EQ,	">>=")			\
  OP(CPP_LSHIFT_EQ,	"<<=")			\
  OP(CPP_MIN_EQ,	"<?=")	/* extension */	\
  OP(CPP_MAX_EQ,	">?=")			\
  /* Digraphs together, beginning with CPP_FIRST_DIGRAPH.  */	\
  OP(CPP_HASH,		"#")	/* digraphs */	\
  OP(CPP_PASTE,		"##")			\
  OP(CPP_OPEN_SQUARE,	"[")			\
  OP(CPP_CLOSE_SQUARE,	"]")			\
  OP(CPP_OPEN_BRACE,	"{")			\
  OP(CPP_CLOSE_BRACE,	"}")			\
  /* The remainder of the punctuation.  Order is not significant.  */	\
  OP(CPP_SEMICOLON,	";")	/* structure */	\
  OP(CPP_ELLIPSIS,	"...")			\
  OP(CPP_PLUS_PLUS,	"++")	/* increment */	\
  OP(CPP_MINUS_MINUS,	"--")			\
  OP(CPP_DEREF,		"->")	/* accessors */	\
  OP(CPP_DOT,		".")			\
  OP(CPP_SCOPE,		"::")			\
  OP(CPP_DEREF_STAR,	"->*")			\
  OP(CPP_DOT_STAR,	".*")			\
  OP(CPP_ATSIGN,	"@")  /* used in Objective-C */ \
\
  TK(CPP_NAME,		SPELL_IDENT)	/* word */			\
  TK(CPP_NUMBER,	SPELL_NUMBER)	/* 34_be+ta  */			\
\
  TK(CPP_CHAR,		SPELL_STRING)	/* 'char' */			\
  TK(CPP_WCHAR,		SPELL_STRING)	/* L'char' */			\
  TK(CPP_OTHER,		SPELL_CHAR)	/* stray punctuation */		\
\
  TK(CPP_STRING,	SPELL_STRING)	/* "string" */			\
  TK(CPP_WSTRING,	SPELL_STRING)	/* L"string" */			\
  TK(CPP_HEADER_NAME,	SPELL_STRING)	/* <stdio.h> in #include */	\
\
  TK(CPP_COMMENT,	SPELL_NUMBER)	/* Only if output comments.  */ \
                                        /* SPELL_NUMBER happens to DTRT.  */ \
  TK(CPP_MACRO_ARG,	SPELL_NONE)	/* Macro argument.  */		\
  TK(CPP_PADDING,	SPELL_NONE)	/* Whitespace for cpp0.  */

#define OP(e, s) e,
#define TK(e, s) e,
enum cpp_ttype
{
  TTYPE_TABLE
  N_TTYPES
};
#undef OP
#undef TK

/* C language kind, used when calling cpp_reader_init.  */
enum c_lang {CLK_GNUC89 = 0, CLK_GNUC99, CLK_STDC89, CLK_STDC94, CLK_STDC99,
	     CLK_GNUCXX, CLK_CXX98, CLK_ASM};

/* Payload of a NUMBER, STRING, CHAR or COMMENT token.  */
struct cpp_string
{
  unsigned int len;
  const unsigned char *text;
};

/* Flags for the cpp_token structure.  */
#define PREV_WHITE	(1 << 0) /* If whitespace before this token.  */
#define DIGRAPH		(1 << 1) /* If it was a digraph.  */
#define STRINGIFY_ARG	(1 << 2) /* If macro argument to be stringified.  */
#define PASTE_LEFT	(1 << 3) /* If on LHS of a ## operator.  */
#define NAMED_OP	(1 << 4) /* C++ named operators.  */
#define NO_EXPAND	(1 << 5) /* Do not macro-expand this token.  */
#define BOL		(1 << 6) /* Token at beginning of line.  */

/* A preprocessing token.  This has been carefully packed and should
   occupy 16 bytes on 32-bit hosts and 24 bytes on 64-bit hosts.  */
struct cpp_token
{
  unsigned int line;		/* Logical line of first char of token.  */
  unsigned short col;		/* Column of first char of token.  */
  ENUM_BITFIELD(cpp_ttype) type : CHAR_BIT;  /* token type */
  unsigned char flags;		/* flags - see above */

  union
  {
    cpp_hashnode *node;		/* An identifier.  */
    const cpp_token *source;	/* Inherit padding from this token.  */
    struct cpp_string str;	/* A string, or number.  */
    unsigned int arg_no;	/* Argument no. for a CPP_MACRO_ARG.  */
    unsigned char c;		/* Character represented by CPP_OTHER.  */
  } val;
};

/* A type wide enough to hold any multibyte source character.
   cpplib's character constant interpreter requires an unsigned type.
   Also, a typedef for the signed equivalent.  */
#ifndef MAX_WCHAR_TYPE_SIZE
# define MAX_WCHAR_TYPE_SIZE WCHAR_TYPE_SIZE
#endif
#if CHAR_BIT * SIZEOF_INT >= MAX_WCHAR_TYPE_SIZE
# define CPPCHAR_SIGNED_T int
#else
# if CHAR_BIT * SIZEOF_LONG >= MAX_WCHAR_TYPE_SIZE || !HAVE_LONG_LONG
#  define CPPCHAR_SIGNED_T long
# else
#  define CPPCHAR_SIGNED_T long long
# endif
#endif
typedef unsigned CPPCHAR_SIGNED_T cppchar_t;
typedef CPPCHAR_SIGNED_T cppchar_signed_t;

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
  /* Characters between tab stops.  */
  unsigned int tabstop;

  /* Pending options - -D, -U, -A, -I, -ixxx.  */
  struct cpp_pending *pending;

  /* Search paths for include files.  */
  struct search_path *quote_include;	/* "" */
  struct search_path *bracket_include;  /* <> */

  /* Map between header names and file names, used only on DOS where
     file names are limited in length.  */
  struct file_name_map_list *map_list;

  /* Directory prefix that should replace `/usr/lib/gcc-lib/TARGET/VERSION'
     in the standard include file directories.  */
  const char *include_prefix;
  unsigned int include_prefix_len;

  /* The language we're preprocessing.  */
  enum c_lang lang;

  /* Non-0 means -v, so print the full set of include dirs.  */
  unsigned char verbose;

  /* Nonzero means use extra default include directories for C++.  */
  unsigned char cplusplus;

  /* Nonzero means handle cplusplus style comments */
  unsigned char cplusplus_comments;

  /* Nonzero means define __OBJC__, treat @ as a special token, and
     use the OBJC[PLUS]_INCLUDE_PATH environment variable.  */
  unsigned char objc;

  /* Nonzero means don't copy comments into the output file.  */
  unsigned char discard_comments;

  /* Nonzero means don't copy comments into the output file during
     macro expansion.  */
  unsigned char discard_comments_in_macro_exp;

  /* Nonzero means process the ISO trigraph sequences.  */
  unsigned char trigraphs;

  /* Nonzero means process the ISO digraph sequences.  */
  unsigned char digraphs;

  /* Nonzero means to allow hexadecimal floats and LL suffixes.  */
  unsigned char extended_numbers;

  /* Nonzero means print names of header files (-H).  */
  unsigned char print_include_names;

  /* Nonzero means cpp_pedwarn causes a hard error.  */
  unsigned char pedantic_errors;

  /* Nonzero means don't print warning messages.  */
  unsigned char inhibit_warnings;

  /* Nonzero means don't suppress warnings from system headers.  */
  unsigned char warn_system_headers;

  /* Nonzero means don't print error messages.  Has no option to
     select it, but can be set by a user of cpplib (e.g. fix-header).  */
  unsigned char inhibit_errors;

  /* Nonzero means warn if slash-star appears in a comment.  */
  unsigned char warn_comments;

  /* Nonzero means warn if there are any trigraphs.  */
  unsigned char warn_trigraphs;

  /* Nonzero means warn if #import is used.  */
  unsigned char warn_import;

  /* Nonzero means warn about multicharacter charconsts.  */
  unsigned char warn_multichar;

  /* Nonzero means warn about various incompatibilities with
     traditional C.  */
  unsigned char warn_traditional;

  /* Nonzero means warn about long long numeric constants.  */
  unsigned char warn_long_long;

  /* Nonzero means warn about text after an #endif (or #else).  */
  unsigned char warn_endif_labels;

  /* Nonzero means warn about implicit sign changes owing to integer
     promotions.  */
  unsigned char warn_num_sign_change;

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

  /* Zero means dollar signs are punctuation.  */
  unsigned char dollars_in_ident;

  /* Nonzero means warn if undefined identifiers are evaluated in an #if.  */
  unsigned char warn_undef;

  /* Nonzero means warn of unused macros from the main file.  */
  unsigned char warn_unused_macros;

  /* Nonzero for the 1999 C Standard, including corrigenda and amendments.  */
  unsigned char c99;

  /* Nonzero if we are conforming to a specific C or C++ standard.  */
  unsigned char std;

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

  /* Nonzero means pass #include lines through to the output.  */
  unsigned char dump_includes;

  /* Print column number in error messages.  */
  unsigned char show_column;

  /* Nonzero means handle C++ alternate operator names.  */
  unsigned char operator_names;

  /* True for traditional preprocessing.  */
  unsigned char traditional;

  /* Dependency generation.  */
  struct
  {
    /* Style of header dependencies to generate.  */
    enum {DEPS_NONE = 0, DEPS_USER, DEPS_SYSTEM } style;

    /* Assume missing files are generated files.  */
    bool missing_files;

    /* Generate phony targets for each dependency apart from the first
       one.  */
    bool phony_targets;

    /* If true, no dependency is generated on the main file.  */
    bool ignore_main_file;
  } deps;

  /* Target-specific features set by the front end or client.  */

  /* Precision for target CPP arithmetic, target characters, target
     ints and target wide characters, respectively.  */
  size_t precision, char_precision, int_precision, wchar_precision;

  /* True means chars (wide chars) are unsigned.  */
  bool unsigned_char, unsigned_wchar;

  /* Nonzero means __STDC__ should have the value 0 in system headers.  */
  unsigned char stdc_0_in_system_headers;
};

/* Call backs.  */
struct cpp_callbacks
{
  /* Called when a new line of preprocessed output is started.  */
  void (*line_change) PARAMS ((cpp_reader *, const cpp_token *, int));
  void (*file_change) PARAMS ((cpp_reader *, const struct line_map *));
  void (*include) PARAMS ((cpp_reader *, unsigned int,
			   const unsigned char *, const cpp_token *));
  void (*define) PARAMS ((cpp_reader *, unsigned int, cpp_hashnode *));
  void (*undef) PARAMS ((cpp_reader *, unsigned int, cpp_hashnode *));
  void (*ident) PARAMS ((cpp_reader *, unsigned int, const cpp_string *));
  void (*def_pragma) PARAMS ((cpp_reader *, unsigned int));
  /* Called when the client has a chance to properly register
     built-ins with cpp_define() and cpp_assert().  */
  void (*register_builtins) PARAMS ((cpp_reader *));
};

/* Name under which this program was invoked.  */
extern const char *progname;

/* The structure of a node in the hash table.  The hash table has
   entries for all identifiers: either macros defined by #define
   commands (type NT_MACRO), assertions created with #assert
   (NT_ASSERTION), or neither of the above (NT_VOID).  Builtin macros
   like __LINE__ are flagged NODE_BUILTIN.  Poisioned identifiers are
   flagged NODE_POISONED.  NODE_OPERATOR (C++ only) indicates an
   identifier that behaves like an operator such as "xor".
   NODE_DIAGNOSTIC is for speed in lex_token: it indicates a
   diagnostic may be required for this node.  Currently this only
   applies to __VA_ARGS__ and poisoned identifiers.  */

/* Hash node flags.  */
#define NODE_OPERATOR	(1 << 0)	/* C++ named operator.  */
#define NODE_POISONED	(1 << 1)	/* Poisoned identifier.  */
#define NODE_BUILTIN	(1 << 2)	/* Builtin macro.  */
#define NODE_DIAGNOSTIC (1 << 3)	/* Possible diagnostic when lexed.  */
#define NODE_WARN	(1 << 4)	/* Warn if redefined or undefined.  */
#define NODE_DISABLED	(1 << 5)	/* A disabled macro.  */

/* Different flavors of hash node.  */
enum node_type
{
  NT_VOID = 0,	   /* No definition yet.  */
  NT_MACRO,	   /* A macro of some form.  */
  NT_ASSERTION	   /* Predicate for #assert.  */
};

/* Different flavors of builtin macro.  _Pragma is an operator, but we
   handle it with the builtin code for efficiency reasons.  */
enum builtin_type
{
  BT_SPECLINE = 0,		/* `__LINE__' */
  BT_DATE,			/* `__DATE__' */
  BT_FILE,			/* `__FILE__' */
  BT_BASE_FILE,			/* `__BASE_FILE__' */
  BT_INCLUDE_LEVEL,		/* `__INCLUDE_LEVEL__' */
  BT_TIME,			/* `__TIME__' */
  BT_STDC,			/* `__STDC__' */
  BT_PRAGMA			/* `_Pragma' operator */
};

#define CPP_HASHNODE(HNODE)	((cpp_hashnode *) (HNODE))
#define HT_NODE(NODE)		((ht_identifier *) (NODE))
#define NODE_LEN(NODE)		HT_LEN (&(NODE)->ident)
#define NODE_NAME(NODE)		HT_STR (&(NODE)->ident)

/* The common part of an identifier node shared amongst all 3 C front
   ends.  Also used to store CPP identifiers, which are a superset of
   identifiers in the grammatical sense.  */
struct cpp_hashnode
{
  struct ht_identifier ident;
  unsigned short arg_index;		/* Macro argument index.  */
  unsigned char directive_index;	/* Index into directive table.  */
  unsigned char rid_code;		/* Rid code - for front ends.  */
  ENUM_BITFIELD(node_type) type : 8;	/* CPP node type.  */
  unsigned char flags;			/* CPP flags.  */

  union
  {
    cpp_macro *macro;			/* If a macro.  */
    struct answer *answers;		/* Answers to an assertion.  */
    enum cpp_ttype operator;		/* Code for a named operator.  */
    enum builtin_type builtin;		/* Code for a builtin macro.  */
  } value;
};

/* Call this first to get a handle to pass to other functions.  */
extern cpp_reader *cpp_create_reader PARAMS ((enum c_lang));

/* Call this to change the selected language standard (e.g. because of
   command line options).  */
extern void cpp_set_lang PARAMS ((cpp_reader *, enum c_lang));

/* Add a dependency TARGET.  Quote it for "make" if QUOTE.  Can be
   called any number of times before cpp_read_main_file().  If no
   targets have been added before cpp_read_main_file(), then the
   default target is used.  */
extern void cpp_add_dependency_target PARAMS ((cpp_reader *,
					       const char * target,
					       int quote));

/* Call these to get pointers to the options and callback structures
   for a given reader.  These pointers are good until you call
   cpp_finish on that reader.  You can either edit the callbacks
   through the pointer returned from cpp_get_callbacks, or set them
   with cpp_set_callbacks.  */
extern cpp_options *cpp_get_options PARAMS ((cpp_reader *));
extern const struct line_maps *cpp_get_line_maps PARAMS ((cpp_reader *));
extern cpp_callbacks *cpp_get_callbacks PARAMS ((cpp_reader *));
extern void cpp_set_callbacks PARAMS ((cpp_reader *, cpp_callbacks *));

/* Now call cpp_handle_option[s] to handle 1[or more] switches.  The
   return value is the number of arguments used.  If
   cpp_handle_options returns without using all arguments, it couldn't
   understand the next switch.  Options processing is not completed
   until you call cpp_finish_options.  */
extern int cpp_handle_options PARAMS ((cpp_reader *, int, char **));
extern int cpp_handle_option PARAMS ((cpp_reader *, int, char **));

/* This function reads the file, but does not start preprocessing.  It
   returns the name of the original file; this is the same as the
   input file, except for preprocessed input.  This will generate at
   least one file change callback, and possibly a line change callback
   too.  If there was an error opening the file, it returns NULL.

   If you want cpplib to manage its own hashtable, pass in a NULL
   pointer.  Otherise you should pass in an initialized hash table
   that cpplib will share; this technique is used by the C front
   ends.  */
extern const char *cpp_read_main_file PARAMS ((cpp_reader *, const char *,
					       struct ht *));

/* Deferred handling of command line options that can generate debug
   callbacks, such as -D and -imacros.  Call this after
   cpp_read_main_file.  The front ends need this separation so they
   can initialize debug output with the original file name, returned
   from cpp_read_main_file, before they get debug callbacks.  */
extern void cpp_finish_options PARAMS ((cpp_reader *));

/* Call this to finish preprocessing.  If you requested dependency
   generation, pass an open stream to write the information to,
   otherwise NULL.  It is your responsibility to close the stream.

   Returns cpp_errors (pfile).  */
extern int cpp_finish PARAMS ((cpp_reader *, FILE *deps_stream));

/* Call this to release the handle at the end of preprocessing.  Any
   use of the handle after this function returns is invalid.  Returns
   cpp_errors (pfile).  */
extern void cpp_destroy PARAMS ((cpp_reader *));

/* Error count.  */
extern unsigned int cpp_errors PARAMS ((cpp_reader *));

extern unsigned int cpp_token_len PARAMS ((const cpp_token *));
extern unsigned char *cpp_token_as_text PARAMS ((cpp_reader *,
						 const cpp_token *));
extern unsigned char *cpp_spell_token PARAMS ((cpp_reader *, const cpp_token *,
					       unsigned char *));
extern void cpp_register_pragma PARAMS ((cpp_reader *,
					 const char *, const char *,
					 void (*) PARAMS ((cpp_reader *))));

extern int cpp_avoid_paste PARAMS ((cpp_reader *, const cpp_token *,
				    const cpp_token *));
extern const cpp_token *cpp_get_token PARAMS ((cpp_reader *));
extern const unsigned char *cpp_macro_definition PARAMS ((cpp_reader *,
						  const cpp_hashnode *));
extern void _cpp_backup_tokens PARAMS ((cpp_reader *, unsigned int));

/* Evaluate a CPP_CHAR or CPP_WCHAR token.  */
extern cppchar_t
cpp_interpret_charconst PARAMS ((cpp_reader *, const cpp_token *,
				 unsigned int *, int *));

/* Used to register builtins during the register_builtins callback.
   The text is the same as the command line argument.  */
extern void cpp_define PARAMS ((cpp_reader *, const char *));
extern void cpp_assert PARAMS ((cpp_reader *, const char *));
extern void cpp_undef  PARAMS ((cpp_reader *, const char *));
extern void cpp_unassert PARAMS ((cpp_reader *, const char *));

extern cpp_buffer *cpp_push_buffer PARAMS ((cpp_reader *,
					    const unsigned char *, size_t,
					    int, int));
extern int cpp_defined PARAMS ((cpp_reader *, const unsigned char *, int));

/* A preprocessing number.  Code assumes that any unused high bits of
   the double integer are set to zero.  */
typedef unsigned HOST_WIDE_INT cpp_num_part;
typedef struct cpp_num cpp_num;
struct cpp_num
{
  cpp_num_part high;
  cpp_num_part low;
  bool unsignedp;  /* True if value should be treated as unsigned.  */
  bool overflow;   /* True if the most recent calculation overflowed.  */
};

/* cpplib provides two interfaces for interpretation of preprocessing
   numbers.

   cpp_classify_number categorizes numeric constants according to
   their field (integer, floating point, or invalid), radix (decimal,
   octal, hexadecimal), and type suffixes.  */

#define CPP_N_CATEGORY  0x000F
#define CPP_N_INVALID	0x0000
#define CPP_N_INTEGER	0x0001
#define CPP_N_FLOATING	0x0002

#define CPP_N_WIDTH	0x00F0
#define CPP_N_SMALL	0x0010	/* int, float.  */
#define CPP_N_MEDIUM	0x0020	/* long, double.  */
#define CPP_N_LARGE	0x0040	/* long long, long double.  */

#define CPP_N_RADIX	0x0F00
#define CPP_N_DECIMAL	0x0100
#define CPP_N_HEX	0x0200
#define CPP_N_OCTAL	0x0400

#define CPP_N_UNSIGNED	0x1000	/* Properties.  */
#define CPP_N_IMAGINARY	0x2000

/* Classify a CPP_NUMBER token.  The return value is a combination of
   the flags from the above sets.  */
extern unsigned cpp_classify_number PARAMS ((cpp_reader *, const cpp_token *));

/* Evaluate a token classified as category CPP_N_INTEGER.  */
extern cpp_num cpp_interpret_integer PARAMS ((cpp_reader *, const cpp_token *,
					      unsigned int type));

/* Sign extend a number, with PRECISION significant bits and all
   others assumed clear, to fill out a cpp_num structure.  */
cpp_num cpp_num_sign_extend PARAMS ((cpp_num, size_t));

/* Diagnostic levels.  To get a dianostic without associating a
   position in the translation unit with it, use cpp_error_with_line
   with a line number of zero.  */

/* Warning, an error with -Werror.  */
#define DL_WARNING		0x00
/* Same as DL_WARNING, except it is not suppressed in system headers.  */
#define DL_WARNING_SYSHDR	0x01
/* Warning, an error with -pedantic-errors or -Werror.  */
#define DL_PEDWARN		0x02
/* An error.  */
#define DL_ERROR		0x03
/* An internal consistency check failed.  Prints "internal error: ",
   otherwise the same as DL_ERROR.  */
#define DL_ICE			0x04
/* Extracts a diagnostic level from an int.  */
#define DL_EXTRACT(l)		(l & 0xf)
/* Nonzero if a diagnostic level is one of the warnings.  */
#define DL_WARNING_P(l)		(DL_EXTRACT (l) >= DL_WARNING \
				 && DL_EXTRACT (l) <= DL_PEDWARN)

/* N.B. The error-message-printer prototypes have not been nicely
   formatted because exgettext needs to see 'msgid' on the same line
   as the name of the function in order to work properly.  Only the
   string argument gets a name in an effort to keep the lines from
   getting ridiculously oversized.  */

/* Output a diagnostic of some kind.  */
extern void cpp_error PARAMS ((cpp_reader *, int, const char *msgid, ...))
  ATTRIBUTE_PRINTF_3;

/* Output a diagnostic of severity LEVEL, with "MSG: " preceding the
   error string of errno.  No location is printed.  */
extern void cpp_errno PARAMS ((cpp_reader *, int level, const char *msg));

/* Same as cpp_error, except additionally specifies a position as a
   (translation unit) physical line and physical column.  If the line is
   zero, then no location is printed.  */
extern void cpp_error_with_line PARAMS ((cpp_reader *, int, unsigned, unsigned, const char *msgid, ...))
  ATTRIBUTE_PRINTF_5;

/* In cpplex.c */
extern int cpp_ideq			PARAMS ((const cpp_token *,
						 const char *));
extern void cpp_output_line		PARAMS ((cpp_reader *, FILE *));
extern void cpp_output_token		PARAMS ((const cpp_token *, FILE *));
extern const char *cpp_type2name	PARAMS ((enum cpp_ttype));
/* Returns the value of an escape sequence, truncated to the correct
   target precision.  PSTR points to the input pointer, which is just
   after the backslash.  LIMIT is how much text we have.  WIDE is true
   if the escape sequence is part of a wide character constant or
   string literal.  Handles all relevant diagnostics.  */
extern cppchar_t cpp_parse_escape	PARAMS ((cpp_reader *,
						 const unsigned char ** pstr,
						 const unsigned char *limit,
						 int wide));

/* In cpphash.c */

/* Lookup an identifier in the hashtable.  Puts the identifier in the
   table if it is not already there.  */
extern cpp_hashnode *cpp_lookup		PARAMS ((cpp_reader *,
						 const unsigned char *,
						 unsigned int));

typedef int (*cpp_cb) PARAMS ((cpp_reader *, cpp_hashnode *, void *));
extern void cpp_forall_identifiers	PARAMS ((cpp_reader *,
						 cpp_cb, void *));

/* In cppmacro.c */
extern void cpp_scan_nooutput		PARAMS ((cpp_reader *));
extern int  cpp_sys_macro_p		PARAMS ((cpp_reader *));
extern unsigned char *cpp_quote_string	PARAMS ((unsigned char *,
						 const unsigned char *,
						 unsigned int));

/* In cppfiles.c */
extern int cpp_included	PARAMS ((cpp_reader *, const char *));
extern void cpp_make_system_header PARAMS ((cpp_reader *, int, int));

/* In cppmain.c */
extern void cpp_preprocess_file PARAMS ((cpp_reader *, const char *, FILE *));

#ifdef __cplusplus
}
#endif

#endif /* ! GCC_CPPLIB_H */
