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
#include "rbtree.h"

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

/* Stanzas are groupings of built-in functions and overloads by some
   common feature/attribute.  These definitions are for built-in function
   stanzas.  */
enum bif_stanza
{
 BSTZ_ALWAYS,
 BSTZ_P5,
 BSTZ_P6,
 BSTZ_ALTIVEC,
 BSTZ_CELL,
 BSTZ_VSX,
 BSTZ_P7,
 BSTZ_P7_64,
 BSTZ_P8,
 BSTZ_P8V,
 BSTZ_P9,
 BSTZ_P9_64,
 BSTZ_P9V,
 BSTZ_IEEE128_HW,
 BSTZ_DFP,
 BSTZ_CRYPTO,
 BSTZ_HTM,
 BSTZ_P10,
 BSTZ_P10_64,
 BSTZ_MMA,
 NUMBIFSTANZAS
};

static bif_stanza curr_bif_stanza;

struct stanza_entry
{
  const char *stanza_name;
  bif_stanza stanza;
};

static stanza_entry stanza_map[NUMBIFSTANZAS] =
  {
    { "always",		BSTZ_ALWAYS	},
    { "power5",		BSTZ_P5		},
    { "power6",		BSTZ_P6		},
    { "altivec",	BSTZ_ALTIVEC	},
    { "cell",		BSTZ_CELL	},
    { "vsx",		BSTZ_VSX	},
    { "power7",		BSTZ_P7		},
    { "power7-64",	BSTZ_P7_64	},
    { "power8",		BSTZ_P8		},
    { "power8-vector",	BSTZ_P8V	},
    { "power9",		BSTZ_P9		},
    { "power9-64",	BSTZ_P9_64	},
    { "power9-vector",	BSTZ_P9V	},
    { "ieee128-hw",	BSTZ_IEEE128_HW	},
    { "dfp",		BSTZ_DFP	},
    { "crypto",		BSTZ_CRYPTO	},
    { "htm",		BSTZ_HTM	},
    { "power10",	BSTZ_P10	},
    { "power10-64",	BSTZ_P10_64	},
    { "mma",		BSTZ_MMA	}
  };

static const char *enable_string[NUMBIFSTANZAS] =
  {
    "ENB_ALWAYS",
    "ENB_P5",
    "ENB_P6",
    "ENB_ALTIVEC",
    "ENB_CELL",
    "ENB_VSX",
    "ENB_P7",
    "ENB_P7_64",
    "ENB_P8",
    "ENB_P8V",
    "ENB_P9",
    "ENB_P9_64",
    "ENB_P9V",
    "ENB_IEEE128_HW",
    "ENB_DFP",
    "ENB_CRYPTO",
    "ENB_HTM",
    "ENB_P10",
    "ENB_P10_64",
    "ENB_MMA"
  };

/* Function modifiers provide special handling for const, pure, and fpmath
   functions.  These are mutually exclusive, and therefore kept separate
   from other bif attributes.  */
enum fnkinds
{
  FNK_NONE,
  FNK_CONST,
  FNK_PURE,
  FNK_FPMATH
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

/* Ways in which a const int value can be restricted.  RES_BITS indicates
   that the integer is restricted to val1 bits, interpreted as an unsigned
   number.  RES_RANGE indicates that the integer is restricted to values
   between val1 and val2, inclusive.  RES_VAR_RANGE is like RES_RANGE, but
   the argument may be variable, so it can only be checked if it is constant.
   RES_VALUES indicates that the integer must have one of the values val1
   or val2.  */
enum restriction
{
  RES_NONE,
  RES_BITS,
  RES_RANGE,
  RES_VAR_RANGE,
  RES_VALUES
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
  restriction restr;
  char *val1;
  char *val2;
};

/* A list of argument types.  */
struct typelist
{
  typeinfo info;
  typelist *next;
};

/* Attributes of a builtin function.  */
struct attrinfo
{
  bool isinit;
  bool isset;
  bool isextract;
  bool isnosoft;
  bool isldvec;
  bool isstvec;
  bool isreve;
  bool ispred;
  bool ishtm;
  bool ishtmspr;
  bool ishtmcr;
  bool ismma;
  bool isquad;
  bool ispair;
  bool isno32bit;
  bool is32bit;
  bool iscpu;
  bool isldstmask;
  bool islxvrse;
  bool islxvrze;
  bool isendian;
};

/* Fields associated with a function prototype (bif or overload).  */
#define MAXRESTROPNDS 3
struct prototype
{
  typeinfo rettype;
  char *bifname;
  int nargs;
  typelist *args;
  int restr_opnd[MAXRESTROPNDS];
  restriction restr[MAXRESTROPNDS];
  char *restr_val1[MAXRESTROPNDS];
  char *restr_val2[MAXRESTROPNDS];
};

/* Data associated with a builtin function, and a table of such data.  */
#define MAXBIFS 16384
struct bifdata
{
  int stanza;
  fnkinds kind;
  prototype proto;
  char *idname;
  char *patname;
  attrinfo attrs;
  char *fndecl;
};

static bifdata bifs[MAXBIFS];
static int num_bifs;
static int curr_bif;

/* Array used to track the order in which built-ins appeared in the
   built-in file.  We reorder them alphabetically but sometimes need
   this information.  */
static int *bif_order;
static int bif_index = 0;

static int num_ovld_stanzas;
static int num_ovlds;

/* Return codes for parsing routines.  */
enum parse_codes
{
  PC_OK,
  PC_EOFILE,
  PC_EOSTANZA,
  PC_PARSEFAIL
};

/* The red-black trees for built-in function identifiers, built-in
   overload identifiers, and function type descriptors.  */
static rbt_strings bif_rbt;
static rbt_strings ovld_rbt;
static rbt_strings fntype_rbt;

/* Another red-black tree containing a mapping from built-in function
   identifiers to the order in which they were encountered.  */
static rbt_strings bifo_rbt;

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

/* Produce a fatal error message.  */
static void
fatal (const char *msg)
{
  fprintf (stderr, "FATAL: %s\n", msg);
  abort ();
}

static bif_stanza
stanza_name_to_stanza (const char *stanza_name)
{
  for (int i = 0; i < NUMBIFSTANZAS; i++)
    if (!strcmp (stanza_name, stanza_map[i].stanza_name))
      return stanza_map[i].stanza;
  fatal ("Stanza mapping is inconsistent.");
  /* Unreachable.  */
  return BSTZ_ALWAYS;
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

/* Helper routine for match_const_restriction.  */
static int
match_bracketed_pair (typeinfo *typedata, char open, char close,
		      restriction restr)
{
  if (linebuf[pos] == open)
    {
      safe_inc_pos ();
      int oldpos = pos;
      char *x = match_integer ();
      if (x == NULL)
	{
	  (*diag) ("malformed integer at column %d.\n", oldpos + 1);
	  return 0;
	}
      consume_whitespace ();
      if (linebuf[pos] != ',')
	{
	  (*diag) ("missing comma at column %d.\n", pos + 1);
	  return 0;
	}
      safe_inc_pos ();
      consume_whitespace ();
      oldpos = pos;
      char *y = match_integer ();
      if (y == NULL)
	{
	  (*diag) ("malformed integer at column %d.\n", oldpos + 1);
	  return 0;
	}
      typedata->restr = restr;
      typedata->val1 = x;
      typedata->val2 = y;

      consume_whitespace ();
      if (linebuf[pos] != close)
	{
	  (*diag) ("malformed restriction at column %d.\n", pos + 1);
	  return 0;
	}
      safe_inc_pos ();
      return 1;
    }

  return 0;
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
  int oldpos = pos;
  if (linebuf[pos] == '<')
    {
      safe_inc_pos ();
      oldpos = pos;
      char *x = match_integer ();
      if (x == NULL)
	{
	  (*diag) ("malformed integer at column %d.\n", oldpos + 1);
	  return 0;
	}
      consume_whitespace ();
      if (linebuf[pos] == '>')
	{
	  typedata->restr = RES_BITS;
	  typedata->val1 = x;
	  safe_inc_pos ();
	  return 1;
	}
      else if (linebuf[pos] != ',')
	{
	  (*diag) ("malformed restriction at column %d.\n", pos + 1);
	  return 0;
	}
      safe_inc_pos ();
      oldpos = pos;
      char *y = match_integer ();
      if (y == NULL)
	{
	  (*diag) ("malformed integer at column %d.\n", oldpos + 1);
	  return 0;
	}
      typedata->restr = RES_RANGE;
      typedata->val1 = x;
      typedata->val2 = y;

      consume_whitespace ();
      if (linebuf[pos] != '>')
	{
	  (*diag) ("malformed restriction at column %d.\n", pos + 1);
	  return 0;
	}
      safe_inc_pos ();
      return 1;
    }
  else if (match_bracketed_pair (typedata, '{', '}', RES_VALUES)
	   || match_bracketed_pair (typedata, '[', ']', RES_VAR_RANGE))
    return 1;

  return 0;
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

/* Parse the argument list.  */
static parse_codes
parse_args (prototype *protoptr)
{
  typelist **argptr = &protoptr->args;
  int *nargs = &protoptr->nargs;
  int *restr_opnd = protoptr->restr_opnd;
  restriction *restr = protoptr->restr;
  char **val1 = protoptr->restr_val1;
  char **val2 = protoptr->restr_val2;
  int restr_cnt = 0;

  int success;
  *nargs = 0;

  /* Start the argument list.  */
  consume_whitespace ();
  if (linebuf[pos] != '(')
    {
      (*diag) ("missing '(' at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }
  safe_inc_pos ();

  do {
    consume_whitespace ();
    int oldpos = pos;
    typelist *argentry = (typelist *) malloc (sizeof (typelist));
    memset (argentry, 0, sizeof *argentry);
    typeinfo *argtype = &argentry->info;
    success = match_type (argtype, VOID_NOTOK);
    if (success)
      {
	if (argtype->restr)
	  {
	    if (restr_cnt >= MAXRESTROPNDS)
	      {
		(*diag) ("More than two %d operands\n", MAXRESTROPNDS);
		return PC_PARSEFAIL;
	      }
	    restr_opnd[restr_cnt] = *nargs + 1;
	    restr[restr_cnt] = argtype->restr;
	    val1[restr_cnt] = argtype->val1;
	    val2[restr_cnt] = argtype->val2;
	    restr_cnt++;
	  }
	(*nargs)++;
	*argptr = argentry;
	argptr = &argentry->next;
	consume_whitespace ();
	if (linebuf[pos] == ',')
	  safe_inc_pos ();
	else if (linebuf[pos] != ')')
	  {
	    (*diag) ("arg not followed by ',' or ')' at column %d.\n",
		     pos + 1);
	    return PC_PARSEFAIL;
	  }

#ifdef DEBUG
	(*diag) ("argument type: isvoid = %d, isconst = %d, isvector = %d, "
		 "issigned = %d, isunsigned = %d, isbool = %d, ispixel = %d, "
		 "ispointer = %d, base = %d, restr = %d, val1 = \"%s\", "
		 "val2 = \"%s\", pos = %d.\n",
		 argtype->isvoid, argtype->isconst, argtype->isvector,
		 argtype->issigned, argtype->isunsigned, argtype->isbool,
		 argtype->ispixel, argtype->ispointer, argtype->base,
		 argtype->restr, argtype->val1, argtype->val2, pos + 1);
#endif
      }
    else
      {
	free (argentry);
	*argptr = NULL;
	pos = oldpos;
	if (linebuf[pos] != ')')
	  {
	    (*diag) ("badly terminated arg list at column %d.\n", pos + 1);
	    return PC_PARSEFAIL;
	  }
	safe_inc_pos ();
      }
  } while (success);

  return PC_OK;
}

/* Parse the attribute list.  */
static parse_codes
parse_bif_attrs (attrinfo *attrptr)
{
  consume_whitespace ();
  if (linebuf[pos] != '{')
    {
      (*diag) ("missing attribute set at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }
  safe_inc_pos ();

  memset (attrptr, 0, sizeof *attrptr);
  char *attrname = NULL;

  do {
    consume_whitespace ();
    int oldpos = pos;
    attrname = match_identifier ();
    if (attrname)
      {
	if (!strcmp (attrname, "init"))
	  attrptr->isinit = 1;
	else if (!strcmp (attrname, "set"))
	  attrptr->isset = 1;
	else if (!strcmp (attrname, "extract"))
	  attrptr->isextract = 1;
	else if (!strcmp (attrname, "nosoft"))
	  attrptr->isnosoft = 1;
	else if (!strcmp (attrname, "ldvec"))
	  attrptr->isldvec = 1;
	else if (!strcmp (attrname, "stvec"))
	  attrptr->isstvec = 1;
	else if (!strcmp (attrname, "reve"))
	  attrptr->isreve = 1;
	else if (!strcmp (attrname, "pred"))
	  attrptr->ispred = 1;
	else if (!strcmp (attrname, "htm"))
	  attrptr->ishtm = 1;
	else if (!strcmp (attrname, "htmspr"))
	  attrptr->ishtmspr = 1;
	else if (!strcmp (attrname, "htmcr"))
	  attrptr->ishtmcr = 1;
	else if (!strcmp (attrname, "mma"))
	  attrptr->ismma = 1;
	else if (!strcmp (attrname, "quad"))
	  attrptr->isquad = 1;
	else if (!strcmp (attrname, "pair"))
	  attrptr->ispair = 1;
	else if (!strcmp (attrname, "no32bit"))
	  attrptr->isno32bit = 1;
	else if (!strcmp (attrname, "32bit"))
	  attrptr->is32bit = 1;
	else if (!strcmp (attrname, "cpu"))
	  attrptr->iscpu = 1;
	else if (!strcmp (attrname, "ldstmask"))
	  attrptr->isldstmask = 1;
	else if (!strcmp (attrname, "lxvrse"))
	  attrptr->islxvrse = 1;
	else if (!strcmp (attrname, "lxvrze"))
	  attrptr->islxvrze = 1;
	else if (!strcmp (attrname, "endian"))
	  attrptr->isendian = 1;
	else
	  {
	    (*diag) ("unknown attribute at column %d.\n", oldpos + 1);
	    return PC_PARSEFAIL;
	  }

	consume_whitespace ();
	if (linebuf[pos] == ',')
	  safe_inc_pos ();
	else if (linebuf[pos] != '}')
	  {
	    (*diag) ("arg not followed by ',' or '}' at column %d.\n",
		     pos + 1);
	    return PC_PARSEFAIL;
	  }
      }
    else
      {
	pos = oldpos;
	if (linebuf[pos] != '}')
	  {
	    (*diag) ("badly terminated attr set at column %d.\n", pos + 1);
	    return PC_PARSEFAIL;
	  }
	safe_inc_pos ();
      }
  } while (attrname);

#ifdef DEBUG
  (*diag) ("attribute set: init = %d, set = %d, extract = %d, nosoft = %d, "
	   "ldvec = %d, stvec = %d, reve = %d, pred = %d, htm = %d, "
	   "htmspr = %d, htmcr = %d, mma = %d, quad = %d, pair = %d, "
	   "no32bit = %d, 32bit = %d, cpu = %d, ldstmask = %d, lxvrse = %d, "
	   "lxvrze = %d, endian = %d.\n",
	   attrptr->isinit, attrptr->isset, attrptr->isextract,
	   attrptr->isnosoft, attrptr->isldvec, attrptr->isstvec,
	   attrptr->isreve, attrptr->ispred, attrptr->ishtm, attrptr->ishtmspr,
	   attrptr->ishtmcr, attrptr->ismma, attrptr->isquad, attrptr->ispair,
	   attrptr->isno32bit, attrptr->is32bit, attrptr->iscpu,
	   attrptr->isldstmask, attrptr->islxvrse, attrptr->islxvrze,
	   attrptr->isendian);
#endif

  return PC_OK;
}

/* Parse a function prototype.  This code is shared by the bif and overload
   file processing.  */
static parse_codes
parse_prototype (prototype *protoptr)
{
  typeinfo *ret_type = &protoptr->rettype;
  char **bifname = &protoptr->bifname;

  /* Get the return type.  */
  consume_whitespace ();
  int oldpos = pos;
  int success = match_type (ret_type, VOID_OK);
  if (!success)
    {
      (*diag) ("missing or badly formed return type at column %d.\n",
	       oldpos + 1);
      return PC_PARSEFAIL;
    }

#ifdef DEBUG
  (*diag) ("return type: isvoid = %d, isconst = %d, isvector = %d, "
	   "issigned = %d, isunsigned = %d, isbool = %d, ispixel = %d, "
	   "ispointer = %d, base = %d, restr = %d, val1 = \"%s\", "
	   "val2 = \"%s\", pos = %d.\n",
	   ret_type->isvoid, ret_type->isconst, ret_type->isvector,
	   ret_type->issigned, ret_type->isunsigned, ret_type->isbool,
	   ret_type->ispixel, ret_type->ispointer, ret_type->base,
	   ret_type->restr, ret_type->val1, ret_type->val2, pos + 1);
#endif

  /* Get the bif name.  */
  consume_whitespace ();
  oldpos = pos;
  *bifname = match_identifier ();
  if (!*bifname)
    {
      (*diag) ("missing function name at column %d.\n", oldpos + 1);
      return PC_PARSEFAIL;
    }

#ifdef DEBUG
  (*diag) ("function name is '%s'.\n", *bifname);
#endif

  /* Process arguments.  */
  if (parse_args (protoptr) == PC_PARSEFAIL)
    return PC_PARSEFAIL;

  /* Process terminating semicolon.  */
  consume_whitespace ();
  if (linebuf[pos] != ';')
    {
      (*diag) ("missing semicolon at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }
  safe_inc_pos ();
  consume_whitespace ();
  if (linebuf[pos] != '\n')
    {
      (*diag) ("garbage at end of line at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }

  return PC_OK;
}

/* Parse a two-line entry for a built-in function.  */
static parse_codes
parse_bif_entry (void)
{
  /* Check for end of stanza.  */
  pos = 0;
  consume_whitespace ();
  if (linebuf[pos] == '[')
    return PC_EOSTANZA;

  /* Allocate an entry in the bif table.  */
  if (num_bifs >= MAXBIFS - 1)
    {
      (*diag) ("too many built-in functions.\n");
      return PC_PARSEFAIL;
    }

  curr_bif = num_bifs++;
  bifs[curr_bif].stanza = curr_bif_stanza;

  /* Read the first token and see if it is a function modifier.  */
  consume_whitespace ();
  int oldpos = pos;
  char *token = match_identifier ();
  if (!token)
    {
      (*diag) ("malformed entry at column %d\n", oldpos + 1);
      return PC_PARSEFAIL;
    }

  if (!strcmp (token, "const"))
    bifs[curr_bif].kind = FNK_CONST;
  else if (!strcmp (token, "pure"))
    bifs[curr_bif].kind = FNK_PURE;
  else if (!strcmp (token, "fpmath"))
    bifs[curr_bif].kind = FNK_FPMATH;
  else
    {
      /* No function modifier, so push the token back.  */
      pos = oldpos;
      bifs[curr_bif].kind = FNK_NONE;
    }

  if (parse_prototype (&bifs[curr_bif].proto) == PC_PARSEFAIL)
    return PC_PARSEFAIL;

  /* Now process line 2.  First up is the builtin id.  */
  if (!advance_line (bif_file))
    {
      (*diag) ("unexpected EOF.\n");
      return PC_PARSEFAIL;
    }

  pos = 0;
  consume_whitespace ();
  oldpos = pos;
  bifs[curr_bif].idname = match_identifier ();
  if (!bifs[curr_bif].idname)
    {
      (*diag) ("missing builtin id at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }

#ifdef DEBUG
  (*diag) ("ID name is '%s'.\n", bifs[curr_bif].idname);
#endif

  /* Save the ID in a lookup structure.  */
  if (!rbt_insert (&bif_rbt, bifs[curr_bif].idname))
    {
      (*diag) ("duplicate function ID '%s' at column %d.\n",
	       bifs[curr_bif].idname, oldpos + 1);
      return PC_PARSEFAIL;
    }

  /* Append a number representing the order in which this function
     was encountered to its name, and save in another lookup
     structure.  */
  char *buf;
  asprintf (&buf, "%s:%05d", bifs[curr_bif].idname, curr_bif);

  if (!rbt_insert (&bifo_rbt, buf))
    {
      (*diag) ("internal error inserting '%s' in bifo_rbt\n", buf);
      return PC_PARSEFAIL;
    }

  /* Now the pattern name.  */
  consume_whitespace ();
  bifs[curr_bif].patname = match_identifier ();
  if (!bifs[curr_bif].patname)
    {
      (*diag) ("missing pattern name at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }

#ifdef DEBUG
  (*diag) ("pattern name is '%s'.\n", bifs[curr_bif].patname);
#endif

  /* Process attributes.  */
  return parse_bif_attrs (&bifs[curr_bif].attrs);
}

/* Parse one stanza of the input BIF file.  linebuf already contains the
   first line to parse.  */
static parse_codes
parse_bif_stanza (void)
{
  /* Parse the stanza header.  */
  pos = 0;
  consume_whitespace ();

  if (linebuf[pos] != '[')
    {
      (*diag) ("ill-formed stanza header at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }
  safe_inc_pos ();

  const char *stanza_name = match_to_right_bracket ();
  if (!stanza_name)
    {
      (*diag) ("no expression found in stanza header.\n");
      return PC_PARSEFAIL;
    }

  curr_bif_stanza = stanza_name_to_stanza (stanza_name);

  if (linebuf[pos] != ']')
    {
      (*diag) ("ill-formed stanza header at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }
  safe_inc_pos ();

  consume_whitespace ();
  if (linebuf[pos] != '\n' && pos != LINELEN - 1)
    {
      (*diag) ("garbage after stanza header.\n");
      return PC_PARSEFAIL;
    }

  parse_codes result = PC_OK;

  while (result != PC_EOSTANZA)
    {
      if (!advance_line (bif_file))
	return PC_EOFILE;
      result = parse_bif_entry ();
      if (result == PC_PARSEFAIL)
	return PC_PARSEFAIL;
    }

  return PC_OK;
}

/* Parse the built-in file.  */
static parse_codes
parse_bif (void)
{
  parse_codes result;
  diag = &bif_diag;
  if (!advance_line (bif_file))
    return PC_OK;

  do
    result = parse_bif_stanza ();
  while (result == PC_OK);

  if (result == PC_EOFILE)
    return PC_OK;
  return result;
}

/* Callback function for create_bif_order.  */
void set_bif_order (char *str)
{
  int num = 0;
  char *colon = strchr (str, ':');
  sscanf (++colon, "%d", &num);
  bif_order[bif_index++] = num;
}

/* Create a mapping from function IDs in their final order to the order
   they appear in the built-in function file.  */
static void
create_bif_order (void)
{
  bif_order = (int *) malloc ((curr_bif + 1)  * sizeof (int));
  rbt_inorder_callback (&bifo_rbt, bifo_rbt.rbt_root, set_bif_order);
}

/* Parse the overload file.  */
static parse_codes
parse_ovld (void)
{
  return PC_OK;
}

/* Write everything to the header file (rs6000-builtins.h).  Return
   1 if successful, 0 otherwise.  */
static int
write_header_file (void)
{
  return 1;
}

/* Write everything to the initialization file (rs6000-builtins.c).
   Return 1 if successful, 0 otherwise.  */
static int
write_init_file (void)
{
  return 1;
}

/* Write everything to the include file (rs6000-vecdefines.h).
   Return 1 if successful, 0 otherwise.  */
static int
write_defines_file (void)
{
  return 1;
}

/* Close and delete output files after any failure, so that subsequent
   build dependencies will fail.  */
static void
delete_output_files (void)
{
  /* Depending on whence we're called, some of these may already be
     closed.  Don't check for errors.  */
  fclose (header_file);
  fclose (init_file);
  fclose (defines_file);

  remove (header_path);
  remove (init_path);
  remove (defines_path);
}

/* Main program to convert flat files into built-in initialization code.  */
int
main (int argc, const char **argv)
{
  if (argc != 6)
    {
      fprintf (stderr,
	       "Five arguments required: two input files and three output "
	       "files.\n");
      exit (1);
    }

  pgm_path = argv[0];
  bif_path = argv[1];
  ovld_path = argv[2];
  header_path = argv[3];
  init_path = argv[4];
  defines_path = argv[5];

  bif_file = fopen (bif_path, "r");
  if (!bif_file)
    {
      fprintf (stderr, "Cannot open input built-in file '%s'.\n", bif_path);
      exit (1);
    }
  ovld_file = fopen (ovld_path, "r");
  if (!ovld_file)
    {
      fprintf (stderr, "Cannot open input overload file '%s'.\n", ovld_path);
      exit (1);
    }
  header_file = fopen (header_path, "w");
  if (!header_file)
    {
      fprintf (stderr, "Cannot open header file '%s' for output.\n",
	       header_path);
      exit (1);
    }
  init_file = fopen (init_path, "w");
  if (!init_file)
    {
      fprintf (stderr, "Cannot open init file '%s' for output.\n", init_path);
      exit (1);
    }
  defines_file = fopen (defines_path, "w");
  if (!defines_file)
    {
      fprintf (stderr, "Cannot open defines file '%s' for output.\n",
	       defines_path);
      exit (1);
    }

  /* Initialize the balanced trees containing built-in function ids,
     overload function ids, and function type declaration ids.  */
  rbt_new (&bif_rbt);
  rbt_new (&ovld_rbt);
  rbt_new (&fntype_rbt);

  /* Initialize another balanced tree that contains a map from built-in
     function ids to the order in which they were encountered.  */
  rbt_new (&bifo_rbt);

  /* Parse the built-in function file.  */
  num_bifs = 0;
  line = 0;
  if (parse_bif () == PC_PARSEFAIL)
    {
      fprintf (stderr, "Parsing of '%s' failed, aborting.\n", bif_path);
      delete_output_files ();
      exit (1);
    }
  fclose (bif_file);

  /* Create a mapping from function IDs in their final order to
     the order they appear in the built-in function file.  */
  create_bif_order ();

#ifdef DEBUG
  fprintf (stderr, "\nFunction ID list:\n");
  rbt_dump (&bif_rbt, bif_rbt.rbt_root);
  fprintf (stderr, "\n");
#endif

  /* Parse the overload file.  */
  num_ovld_stanzas = 0;
  num_ovlds = 0;
  line = 0;
  if (parse_ovld () == PC_PARSEFAIL)
    {
      fprintf (stderr, "Parsing of '%s' failed, aborting.\n", ovld_path);
      delete_output_files ();
      exit (1);
    }
  fclose (ovld_file);

#ifdef DEBUG
  fprintf (stderr, "\nFunction type decl list:\n");
  rbt_dump (&fntype_rbt, fntype_rbt.rbt_root);
  fprintf (stderr, "\n");
#endif

  /* Write the header file and the file containing initialization code.  */
  if (!write_header_file ())
    {
      fprintf (stderr, "Output to '%s' failed, aborting.\n", header_path);
      delete_output_files ();
      exit (1);
    }
  if (!write_init_file ())
    {
      fprintf (stderr, "Output to '%s' failed, aborting.\n", init_path);
      delete_output_files ();
      exit (1);
    }

  /* Write the defines file to be included into altivec.h.  */
  if (!write_defines_file ())
    {
      fprintf (stderr, "Output to '%s' failed, aborting.\n", defines_path);
      delete_output_files ();
      exit (1);
    }

  fclose (header_file);
  fclose (init_file);
  fclose (defines_file);

  return 0;
}
