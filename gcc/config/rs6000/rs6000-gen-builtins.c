/* Generate built-in function initialization and recognition for Power.
   Copyright (C) 2020-21 Free Software Foundation, Inc.
   Contributed by Bill Schmidt, IBM <wschmidt@linux.ibm.com>

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

/* This program generates built-in function initialization and
   recognition code for Power targets, based on text files that
   describe the built-in functions and vector overloads:

     rs6000-builtin-new.def     Table of built-in functions
     rs6000-overload.def        Table of overload functions

   Both files group similar functions together in "stanzas," as
   described below.

   Each stanza in the built-in function file starts with a line
   identifying the circumstances in which the group of functions is
   permitted, with the gating predicate in square brackets.  For
   example, this could be

     [altivec]

   or it could be

     [power9]

   The bracketed gating predicate is the only information allowed on
   the stanza header line, other than whitespace.

   Following the stanza header are two lines for each function: the
   prototype line and the attributes line.  The prototype line has
   this format, where the square brackets indicate optional
   information and angle brackets indicate required information:

     [kind] <return-type> <bif-name> (<argument-list>);

   Here [kind] can be one of "const", "pure", or "fpmath";
   <return-type> is a legal type for a built-in function result;
   <bif-name> is the name by which the function can be called;
   and <argument-list> is a comma-separated list of legal types
   for built-in function arguments.  The argument list may be
   empty, but the parentheses and semicolon are required.

   The attributes line looks like this:

     <bif-id> <bif-pattern> {<attribute-list>}

   Here <bif-id> is a unique internal identifier for the built-in
   function that will be used as part of an enumeration of all
   built-in functions; <bif-pattern> is the define_expand or
   define_insn that will be invoked when the call is expanded;
   and <attribute-list> is a comma-separated list of special
   conditions that apply to the built-in function.  The attribute
   list may be empty, but the braces are required.

   Attributes are strings, such as these:

     init     Process as a vec_init function
     set      Process as a vec_set function
     extract  Process as a vec_extract function
     nosoft   Not valid with -msoft-float
     ldvec    Needs special handling for vec_ld semantics
     stvec    Needs special handling for vec_st semantics
     reve     Needs special handling for element reversal
     pred     Needs special handling for comparison predicates
     htm      Needs special handling for transactional memory
     htmspr   HTM function using an SPR
     htmcr    HTM function using a CR
     mma      Needs special handling for MMA instructions
     quad     MMA instruction using a register quad as an input operand
     pair     MMA instruction using a register pair as an input operand
     no32bit  Not valid for TARGET_32BIT
     32bit    Requires different handling for TARGET_32BIT
     cpu      This is a "cpu_is" or "cpu_supports" builtin
     ldstmask Altivec mask for load or store
     lxvrse   Needs special handling for load-rightmost, sign-extended
     lxvrze   Needs special handling for load-rightmost, zero-extended
     endian   Needs special handling for endianness

   An example stanza might look like this:

[altivec]
  const vsc __builtin_altivec_abs_v16qi (vsc);
    ABS_V16QI absv16qi2 {}
  const vss __builtin_altivec_abs_v8hi (vss);
    ABS_V8HI absv8hi2 {}

   Here "vsc" and "vss" are shorthand for "vector signed char" and
   "vector signed short" to shorten line lengths and improve readability.
   Note the use of indentation, which is recommended but not required.

   The overload file has more complex stanza headers.  Here the stanza
   represents all functions with the same overloaded function name:

     [<overload-id>, <abi-name>, <builtin-name>[[, <ifdef>]] ]

   Here the single square brackets are part of the syntax, <overload-id>
   is a unique internal identifier for the overload that will be used as
   part of an enumeration of all overloaded functions; <abi-name> is the
   name that will appear as a #define in rs6000-vecdefines.h;
   <builtin-name> is the name that is overloaded in the back end; and
   <ifdef> is an optional token used to guard the #define with an #ifdef
   in rs6000-vecdefines.h.

   Each function entry again has two lines.  The first line is again a
   prototype line (this time without [kind]):

     <return-type> <internal-name> (<argument-list>);

   The second line contains the <bif-id> that this particular instance of
   the overloaded function maps to.  It must match a token that appears in
   rs6000-builtin-new.def.  Optionally, a second token may appear.  If only
   one token is on the line, it is also used to build the unique identifier
   for the overloaded function.  If a second token is present, the second
   token is used instead for this purpose.  This is necessary in cases
   where a built-in function accepts more than one type signature.  It is
   common to have a built-in function that, for example, specifies a
   "vector signed char" argument, but accepts "vector unsigned char" and
   "vector bool char" as well because only the mode matters.  Note that
   the overload resolution mechanism has always handled these cases by
   performing fold_convert on vector arguments to hide type mismatches,
   and it will continue to do so.

   As a concrete example, __builtin_altivec_mtvscr uses an opaque argument
   type for the source operand.  Its built-in function id is MTVSCR.  The
   overloaded function __builtin_vec_mtvscr takes a variety of specific
   types, but not all vector types.  Each of these maps to the same
   __builtin_altivec_mtvscr built-in function, but the overload ID must
   be unique, so we must specify the second token as shown here.

    [VEC_MTVSCR, vec_mtvscr, __builtin_vec_mtvscr]
      void __builtin_vec_mtvscr (vbc);
	MTVSCR  MTVSCR_VBC
      void __builtin_vec_mtvscr (vsc);
	MTVSCR  MTVSCR_VSC
      ...

  Blank lines may be used as desired in these files between the lines as
  defined above; that is, you can introduce as many extra newlines as you
  like after a required newline, but nowhere else.  Lines beginning with
  a semicolon are also treated as blank lines.  */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>

/* Input and output file descriptors and pathnames.  */
static FILE *bif_file;
static FILE *ovld_file;
static FILE *header_file;
static FILE *init_file;
static FILE *defines_file;

static const char *pgm_path;
static const char *bif_path;
static const char *ovld_path;
static const char *header_path;
static const char *init_path;
static const char *defines_path;

/* Position information.  Note that "pos" is zero-indexed, but users
   expect one-indexed column information, so representations of "pos"
   as columns in diagnostic messages must be adjusted.  */
#define LINELEN 1024
static char linebuf[LINELEN];
static int line;
static int pos;

/* Used to determine whether a type can be void (only return types).  */
enum void_status
{
 VOID_NOTOK,
 VOID_OK
};

/* Legal base types for an argument or return type.  */
enum basetype
{
  BT_CHAR,
  BT_SHORT,
  BT_INT,
  BT_LONG,
  BT_LONGLONG,
  BT_FLOAT,
  BT_DOUBLE,
  BT_LONGDOUBLE,
  BT_INT128,
  BT_FLOAT128,
  BT_BOOL,
  BT_STRING,
  BT_DECIMAL32,
  BT_DECIMAL64,
  BT_DECIMAL128,
  BT_IBM128,
  BT_VPAIR,
  BT_VQUAD
};

/* Type modifiers for an argument or return type.  */
struct typeinfo
{
  char isvoid;
  char isconst;
  char isvector;
  char issigned;
  char isunsigned;
  char isbool;
  char ispixel;
  char ispointer;
  basetype base;
  char *val1;
  char *val2;
};

/* Pointer to a diagnostic function.  */
static void (*diag) (const char *, ...)
  __attribute__ ((format (printf, 1, 2)));

/* Custom diagnostics.  */
static void __attribute__ ((format (printf, 1, 2)))
bif_diag (const char * fmt, ...)
{
  va_list args;
  fprintf (stderr, "%s:%d: ", bif_path, line);
  va_start (args, fmt);
  vfprintf (stderr, fmt, args);
  va_end (args);
}

static void __attribute__ ((format (printf, 1, 2)))
ovld_diag (const char * fmt, ...)
{
  va_list args;
  fprintf (stderr, "%s:%d: ", ovld_path, line);
  va_start (args, fmt);
  vfprintf (stderr, fmt, args);
  va_end (args);
}

/* Pass over whitespace (other than a newline, which terminates the scan).  */
static void
consume_whitespace (void)
{
  while (pos < LINELEN && isspace(linebuf[pos]) && linebuf[pos] != '\n')
    pos++;
  return;
}

/* Get the next nonblank, noncomment line, returning 0 on EOF, 1 otherwise.  */
static int
advance_line (FILE *file)
{
  while (1)
    {
      /* Read ahead one line and check for EOF.  */
      if (!fgets (linebuf, sizeof linebuf, file))
	return 0;
      line++;
      size_t len = strlen (linebuf);
      if (linebuf[len - 1] != '\n')
	(*diag) ("line doesn't terminate with newline\n");
      pos = 0;
      consume_whitespace ();
      if (linebuf[pos] != '\n' && linebuf[pos] != ';')
	return 1;
    }
}

static inline void
safe_inc_pos (void)
{
  if (pos++ >= LINELEN)
    {
      (*diag) ("line length overrun.\n");
      exit (1);
    }
}

/* Match an identifier, returning NULL on failure, else a pointer to a
   buffer containing the identifier.  */
static char *
match_identifier (void)
{
  int lastpos = pos - 1;
  while (isalnum (linebuf[lastpos + 1]) || linebuf[lastpos + 1] == '_')
    ++lastpos;

  if (lastpos < pos)
    return 0;

  char *buf = (char *) malloc (lastpos - pos + 2);
  memcpy (buf, &linebuf[pos], lastpos - pos + 1);
  buf[lastpos - pos + 1] = '\0';

  pos = lastpos + 1;
  return buf;
}

/* Match an integer and return the string representing its value,
   or a null string on failure.  */
static char *
match_integer (void)
{
  int startpos = pos;
  if (linebuf[pos] == '-')
    safe_inc_pos ();

  int lastpos = pos - 1;
  while (isdigit (linebuf[lastpos + 1]))
    ++lastpos;

  if (lastpos < pos)
    return NULL;

  pos = lastpos + 1;
  char *buf = (char *) malloc (lastpos - startpos + 2);
  memcpy (buf, &linebuf[startpos], lastpos - startpos + 1);
  buf[lastpos - startpos + 1] = '\0';
  return buf;
}

/* Match a string up to but not including a ']', and return its value,
   or zero if there is nothing before the ']'.  Error if we don't find
   such a character.  */
static const char *
match_to_right_bracket (void)
{
  int lastpos = pos - 1;
  while (linebuf[lastpos + 1] != ']')
    {
      if (linebuf[lastpos + 1] == '\n')
	{
	  (*diag) ("no ']' found before end of line.\n");
	  exit (1);
	}
      ++lastpos;
    }

  if (lastpos < pos)
    return 0;

  char *buf = (char *) malloc (lastpos - pos + 2);
  memcpy (buf, &linebuf[pos], lastpos - pos + 1);
  buf[lastpos - pos + 1] = '\0';

  pos = lastpos + 1;
  return buf;
}

static inline void
handle_pointer (typeinfo *typedata)
{
  consume_whitespace ();
  if (linebuf[pos] == '*')
    {
      typedata->ispointer = 1;
      safe_inc_pos ();
    }
}

/* Match one of the allowable base types.  Consumes one token unless the
   token is "long", which must be paired with a second "long".  Optionally
   consumes a following '*' token for pointers.  Return 1 for success,
   0 for failure.  */
static int
match_basetype (typeinfo *typedata)
{
  consume_whitespace ();
  int oldpos = pos;
  char *token = match_identifier ();
  if (!token)
    {
      (*diag) ("missing base type in return type at column %d\n", pos + 1);
      return 0;
    }

  if (!strcmp (token, "char"))
    typedata->base = BT_CHAR;
  else if (!strcmp (token, "short"))
    typedata->base = BT_SHORT;
  else if (!strcmp (token, "int"))
    typedata->base = BT_INT;
  else if (!strcmp (token, "long"))
    {
      consume_whitespace ();
      oldpos = pos;
      char *mustbelongordbl = match_identifier ();
      if (!mustbelongordbl)
	typedata->base = BT_LONG;
      else if (!strcmp (mustbelongordbl, "long"))
	typedata->base = BT_LONGLONG;
      else if (!strcmp (mustbelongordbl, "double"))
	typedata->base = BT_LONGDOUBLE;
      else
	/* Speculatively accept "long" here and push back the token.
	   This occurs when "long" is a return type and the next token
	   is the function name.  */
	{
	  typedata->base = BT_LONG;
	  pos = oldpos;
	}
    }
  else if (!strcmp (token, "float"))
    typedata->base = BT_FLOAT;
  else if (!strcmp (token, "double"))
    typedata->base = BT_DOUBLE;
  else if (!strcmp (token, "__int128"))
    typedata->base = BT_INT128;
  else if (!strcmp (token, "_Float128"))
    typedata->base = BT_FLOAT128;
  else if (!strcmp (token, "bool"))
    typedata->base = BT_BOOL;
  /* A "string" is a special "const char *" -- we need it because it
     cannot match either signed or unsigned char *.  */
  else if (!strcmp (token, "string"))
    typedata->base = BT_STRING;
  else if (!strcmp (token, "_Decimal32"))
    typedata->base = BT_DECIMAL32;
  else if (!strcmp (token, "_Decimal64"))
    typedata->base = BT_DECIMAL64;
  else if (!strcmp (token, "_Decimal128"))
    typedata->base = BT_DECIMAL128;
  else if (!strcmp (token, "__ibm128"))
    typedata->base = BT_IBM128;
  else
    {
      (*diag) ("unrecognized base type at column %d\n", oldpos + 1);
      return 0;
    }

  handle_pointer (typedata);
  return 1;
}

/* A const int argument may be restricted to certain values.  This is
   indicated by one of the following occurring after the "int' token:

     <x>   restricts the constant to x bits, interpreted as unsigned
     <x,y> restricts the constant to the inclusive range [x,y]
     [x,y] restricts the constant to the inclusive range [x,y],
	   but only applies if the argument is constant.
     {x,y} restricts the constant to one of two values, x or y.

   Here x and y are integer tokens.  Note that the "const" token is a
   lie when the restriction is [x,y], but this simplifies the parsing
   significantly and is hopefully forgivable.

   Return 1 for success, else 0.  */
static int
match_const_restriction (typeinfo *typedata)
{
  return 1;
}

/* Look for a type, which can be terminated by a token that is not part of
   a type, a comma, or a closing parenthesis.  Place information about the
   type in TYPEDATA.  Return 1 for success, 0 for failure.  */
static int
match_type (typeinfo *typedata, int voidok)
{
  /* A legal type is of the form:

       [const] [[signed|unsigned] <basetype> | <vectype>] [*]

     Legal values of <basetype> are (for now):

       char
       short
       int
       long
       long double
       long long
       float
       double
       __int128
       _Float128
       bool
       string
       _Decimal32
       _Decimal64
       _Decimal128
       __ibm128

     Legal values of <vectype> are as follows, and are shorthand for
     the associated meaning:

       vsc	vector signed char
       vuc	vector unsigned char
       vbc	vector bool char
       vss	vector signed short
       vus	vector unsigned short
       vbs	vector bool short
       vsi	vector signed int
       vui	vector unsigned int
       vbi	vector bool int
       vsll	vector signed long long
       vull	vector unsigned long long
       vbll	vector bool long long
       vsq	vector signed __int128
       vuq	vector unsigned __int128
       vbq	vector bool __int128
       vp	vector pixel
       vf	vector float
       vd	vector double
       v256	__vector_pair
       v512	__vector_quad

     For simplicity, We don't support "short int" and "long long int".
     We don't currently support a <basetype> of "_Float16".  "signed"
     and "unsigned" only apply to integral base types.  The optional *
     indicates a pointer type.  */

  consume_whitespace ();
  memset (typedata, 0, sizeof *typedata);
  int oldpos = pos;

  char *token = match_identifier ();
  if (!token)
    return 0;

  if (!strcmp (token, "const"))
    {
      typedata->isconst = 1;
      consume_whitespace ();
      oldpos = pos;
      token = match_identifier ();
    }

  if (!strcmp (token, "void"))
    typedata->isvoid = 1;

  if (!strcmp (token, "vsc"))
    {
      typedata->isvector = 1;
      typedata->issigned = 1;
      typedata->base = BT_CHAR;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vuc"))
    {
      typedata->isvector = 1;
      typedata->isunsigned = 1;
      typedata->base = BT_CHAR;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vbc"))
    {
      typedata->isvector = 1;
      typedata->isbool = 1;
      typedata->base = BT_CHAR;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vss"))
    {
      typedata->isvector = 1;
      typedata->issigned = 1;
      typedata->base = BT_SHORT;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vus"))
    {
      typedata->isvector = 1;
      typedata->isunsigned = 1;
      typedata->base = BT_SHORT;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vbs"))
    {
      typedata->isvector = 1;
      typedata->isbool = 1;
      typedata->base = BT_SHORT;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vsi"))
    {
      typedata->isvector = 1;
      typedata->issigned = 1;
      typedata->base = BT_INT;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vui"))
    {
      typedata->isvector = 1;
      typedata->isunsigned = 1;
      typedata->base = BT_INT;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vbi"))
    {
      typedata->isvector = 1;
      typedata->isbool = 1;
      typedata->base = BT_INT;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vsll"))
    {
      typedata->isvector = 1;
      typedata->issigned = 1;
      typedata->base = BT_LONGLONG;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vull"))
    {
      typedata->isvector = 1;
      typedata->isunsigned = 1;
      typedata->base = BT_LONGLONG;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vbll"))
    {
      typedata->isvector = 1;
      typedata->isbool = 1;
      typedata->base = BT_LONGLONG;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vsq"))
    {
      typedata->isvector = 1;
      typedata->issigned = 1;
      typedata->base = BT_INT128;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vuq"))
    {
      typedata->isvector = 1;
      typedata->isunsigned = 1;
      typedata->base = BT_INT128;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vbq"))
    {
      typedata->isvector = 1;
      typedata->isbool = 1;
      typedata->base = BT_INT128;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vp"))
    {
      typedata->isvector = 1;
      typedata->ispixel = 1;
      typedata->base = BT_SHORT;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vf"))
    {
      typedata->isvector = 1;
      typedata->base = BT_FLOAT;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "vd"))
    {
      typedata->isvector = 1;
      typedata->base = BT_DOUBLE;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "v256"))
    {
      typedata->isvector = 1;
      typedata->base = BT_VPAIR;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "v512"))
    {
      typedata->isvector = 1;
      typedata->base = BT_VQUAD;
      handle_pointer (typedata);
      return 1;
    }
  else if (!strcmp (token, "signed"))
    typedata->issigned = 1;
  else if (!strcmp (token, "unsigned"))
    typedata->isunsigned = 1;
  else if (!typedata->isvoid && !typedata->isconst)
    {
      /* Push back token.  */
      pos = oldpos;
      return match_basetype (typedata);
    }

  if (typedata->isvoid)
    {
      consume_whitespace ();
      if (linebuf[pos] == '*')
	{
	  typedata->ispointer = 1;
	  safe_inc_pos ();
	}
      else if (!voidok)
	return 0;
      return 1;
    }

  if (!typedata->issigned && !typedata->isunsigned)
    pos = oldpos;
  if (!match_basetype (typedata))
    return 0;

  if (typedata->isconst)
    {
      if (typedata->ispointer)
	return 1;
      if (typedata->base != BT_INT)
	{
	  (*diag)("'const' at %d requires pointer or integer type",
		  oldpos + 1);
	  return 0;
	}
      consume_whitespace ();
      if (linebuf[pos] == '<' || linebuf[pos] == '{' || linebuf[pos] == '[')
	return match_const_restriction (typedata);
    }

  return 1;
}
