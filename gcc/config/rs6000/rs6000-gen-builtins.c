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
     mmaint   MMA instruction expanding to internal call at GIMPLE time
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
  bool ismmaint;
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

/* Stanzas are groupings of built-in functions and overloads by some
   common feature/attribute.  These definitions are for overload stanzas.  */
struct ovld_stanza
{
  char *stanza_id;
  char *extern_name;
  char *intern_name;
  char *ifdef;
};

#define MAXOVLDSTANZAS 512
static ovld_stanza ovld_stanzas[MAXOVLDSTANZAS];
static int num_ovld_stanzas;
static int curr_ovld_stanza;

#define MAXOVLDS 16384
struct ovlddata
{
  int stanza;
  prototype proto;
  char *bif_id_name;
  char *ovld_id_name;
  char *fndecl;
};

static ovlddata ovlds[MAXOVLDS];
static int num_ovlds;
static int curr_ovld;
static int max_ovld_args = 0;

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

/* Mapping from type tokens to type node names.  */
struct typemap
{
  const char *key;
  const char *value;
};

/* This table must be kept in alphabetical order, as we use binary
   search for table lookups in map_token_to_type_node.  The table
   maps tokens from a fntype string to a tree type.  For example,
   in "si_ftype_hi" we would map "si" to "intSI_type_node" and
   map "hi" to "intHI_type_node".  */
#define TYPE_MAP_SIZE 86
static typemap type_map[TYPE_MAP_SIZE] =
  {
    { "bi",		"bool_int" },
    { "bv16qi",		"bool_V16QI" },
    { "bv1ti",		"bool_V1TI" },
    { "bv2di",		"bool_V2DI" },
    { "bv4si",		"bool_V4SI" },
    { "bv8hi",		"bool_V8HI" },
    { "ci",		"integer" },
    { "dd",		"dfloat64" },
    { "df",		"double" },
    { "di",		"long_long_integer" },
    { "hi",		"intHI" },
    { "if",		"ibm128_float" },
    { "ld",		"long_double" },
    { "lg",		"long_integer" },
    { "pbv16qi",	"ptr_bool_V16QI" },
    { "pbv1ti",		"ptr_bool_V1TI" },
    { "pbv2di",		"ptr_bool_V2DI" },
    { "pbv4si",		"ptr_bool_V4SI" },
    { "pbv8hi",		"ptr_bool_V8HI" },
    { "pcvoid",		"pcvoid" },
    { "pdd",		"ptr_dfloat64" },
    { "pdf",		"ptr_double" },
    { "pdi",		"ptr_long_long_integer" },
    { "phi",		"ptr_intHI" },
    { "pif",		"ptr_ibm128_float" },
    { "pld",		"ptr_long_double" },
    { "plg",		"ptr_long_integer" },
    { "pqi",		"ptr_intQI" },
    { "psf",		"ptr_float" },
    { "psi",		"ptr_intSI" },
    { "ptd",		"ptr_dfloat128" },
    { "ptf",		"ptr_float128" },
    { "pti",		"ptr_intTI" },
    { "pudi",		"ptr_long_long_unsigned" },
    { "puhi",		"ptr_uintHI" },
    { "pulg",		"ptr_long_unsigned" },
    { "puqi",		"ptr_uintQI" },
    { "pusi",		"ptr_uintSI" },
    { "puti",		"ptr_uintTI" },
    { "puv16qi",	"ptr_unsigned_V16QI" },
    { "puv1ti",		"ptr_unsigned_V1TI" },
    { "puv2di",		"ptr_unsigned_V2DI" },
    { "puv4si",		"ptr_unsigned_V4SI" },
    { "puv8hi",		"ptr_unsigned_V8HI" },
    { "pv",		"ptr" },
    { "pv16qi",		"ptr_V16QI" },
    { "pv1poi",		"ptr_vector_pair" },
    { "pv1pxi",		"ptr_vector_quad" },
    { "pv1ti",		"ptr_V1TI" },
    { "pv2df",		"ptr_V2DF" },
    { "pv2di",		"ptr_V2DI" },
    { "pv4sf",		"ptr_V4SF" },
    { "pv4si",		"ptr_V4SI" },
    { "pv8hi",		"ptr_V8HI" },
    { "pvp8hi",		"ptr_pixel_V8HI" },
    { "qi",		"intQI" },
    { "sd",		"dfloat32" },
    { "sf",		"float" },
    { "si",		"intSI" },
    { "st",		"const_str" },
    { "td",		"dfloat128" },
    { "tf",		"float128" },
    { "ti",		"intTI" },
    { "udi",		"long_long_unsigned" },
    { "uhi",		"unsigned_intHI" },
    { "ulg",		"long_unsigned" },
    { "uqi",		"unsigned_intQI" },
    { "usi",		"unsigned_intSI" },
    { "uti",		"unsigned_intTI" },
    { "uv16qi",		"unsigned_V16QI" },
    { "uv1ti",		"unsigned_V1TI" },
    { "uv2di",		"unsigned_V2DI" },
    { "uv4si",		"unsigned_V4SI" },
    { "uv8hi",		"unsigned_V8HI" },
    { "v",		"void" },
    { "v16qi",		"V16QI" },
    { "v1poi",		"vector_pair" },
    { "v1pxi",		"vector_quad" },
    { "v1ti",		"V1TI" },
    { "v2df",		"V2DF" },
    { "v2di",		"V2DI" },
    { "v4sf",		"V4SF" },
    { "v4si",		"V4SI" },
    { "v8hi",		"V8HI" },
    { "vp8hi",		"pixel_V8HI" },
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

  if (pos >= LINELEN)
    {
      diag ("line length overrun at %d.\n", pos);
      exit (1);
    }

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
  if (++pos >= LINELEN)
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
  while (lastpos < LINELEN - 1
	 && (isalnum (linebuf[lastpos + 1]) || linebuf[lastpos + 1] == '_'))
    ++lastpos;

  if (lastpos >= LINELEN - 1)
    {
      diag ("line length overrun at %d.\n", lastpos);
      exit (1);
    }

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
  while (lastpos < LINELEN - 1 && isdigit (linebuf[lastpos + 1]))
    ++lastpos;

  if (lastpos >= LINELEN - 1)
    {
      diag ("line length overrun at %d.\n", lastpos);
      exit (1);
    }

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
  while (lastpos < LINELEN - 1 && linebuf[lastpos + 1] != ']')
    {
      if (linebuf[lastpos + 1] == '\n')
	{
	  (*diag) ("no ']' found before end of line.\n");
	  exit (1);
	}
      ++lastpos;
    }

  if (lastpos >= LINELEN - 1)
    {
      diag ("line length overrun at %d.\n", lastpos);
      exit (1);
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
	else if (!strcmp (attrname, "mmaint"))
	  attrptr->ismmaint = 1;
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
	   "mmaint = %d, no32bit = %d, 32bit = %d, cpu = %d, ldstmask = %d, "
	   "lxvrse = %d, lxvrze = %d, endian = %d.\n",
	   attrptr->isinit, attrptr->isset, attrptr->isextract,
	   attrptr->isnosoft, attrptr->isldvec, attrptr->isstvec,
	   attrptr->isreve, attrptr->ispred, attrptr->ishtm, attrptr->ishtmspr,
	   attrptr->ishtmcr, attrptr->ismma, attrptr->isquad, attrptr->ispair,
	   attrptr->ismmaint, attrptr->isno32bit, attrptr->is32bit,
	   attrptr->iscpu, attrptr->isldstmask, attrptr->islxvrse,
	   attrptr->islxvrze, attrptr->isendian);
#endif

  return PC_OK;
}

/* Convert a vector type into a mode string.  */
static void
complete_vector_type (typeinfo *typeptr, char *buf, int *bufi)
{
  if (typeptr->isbool)
    buf[(*bufi)++] = 'b';
  buf[(*bufi)++] = 'v';
  if (typeptr->ispixel)
    {
      memcpy (&buf[*bufi], "p8hi", 4);
      *bufi += 4;
      return;
    }
  switch (typeptr->base)
    {
    case BT_CHAR:
      memcpy (&buf[*bufi], "16qi", 4);
      *bufi += 4;
      break;
    case BT_SHORT:
      memcpy (&buf[*bufi], "8hi", 3);
      *bufi += 3;
      break;
    case BT_INT:
      memcpy (&buf[*bufi], "4si", 3);
      *bufi += 3;
      break;
    case BT_LONGLONG:
      memcpy (&buf[*bufi], "2di", 3);
      *bufi += 3;
      break;
    case BT_FLOAT:
      memcpy (&buf[*bufi], "4sf", 3);
      *bufi += 3;
      break;
    case BT_DOUBLE:
      memcpy (&buf[*bufi], "2df", 3);
      *bufi += 3;
      break;
    case BT_INT128:
      memcpy (&buf[*bufi], "1ti", 3);
      *bufi += 3;
      break;
    case BT_FLOAT128:
      memcpy (&buf[*bufi], "1tf", 3);
      *bufi += 3;
      break;
    case BT_VPAIR:
      memcpy (&buf[*bufi], "1poi", 4);
      *bufi += 4;
      break;
    case BT_VQUAD:
      memcpy (&buf[*bufi], "1pxi", 4);
      *bufi += 4;
      break;
    default:
      (*diag) ("unhandled basetype %d.\n", typeptr->base);
      exit (1);
    }
}

/* Convert a base type into a mode string.  */
static void
complete_base_type (typeinfo *typeptr, char *buf, int *bufi)
{
  switch (typeptr->base)
    {
    case BT_CHAR:
      memcpy (&buf[*bufi], "qi", 2);
      break;
    case BT_SHORT:
      memcpy (&buf[*bufi], "hi", 2);
      break;
    case BT_INT:
      memcpy (&buf[*bufi], "si", 2);
      break;
    case BT_LONG:
      memcpy (&buf[*bufi], "lg", 2);
      break;
    case BT_LONGLONG:
      memcpy (&buf[*bufi], "di", 2);
      break;
    case BT_FLOAT:
      memcpy (&buf[*bufi], "sf", 2);
      break;
    case BT_DOUBLE:
      memcpy (&buf[*bufi], "df", 2);
      break;
    case BT_LONGDOUBLE:
      memcpy (&buf[*bufi], "ld", 2);
      break;
    case BT_INT128:
      memcpy (&buf[*bufi], "ti", 2);
      break;
    case BT_FLOAT128:
      memcpy (&buf[*bufi], "tf", 2);
      break;
    case BT_BOOL:
      memcpy (&buf[*bufi], "bi", 2);
      break;
    case BT_STRING:
      memcpy (&buf[*bufi], "st", 2);
      break;
    case BT_DECIMAL32:
      memcpy (&buf[*bufi], "sd", 2);
      break;
    case BT_DECIMAL64:
      memcpy (&buf[*bufi], "dd", 2);
      break;
    case BT_DECIMAL128:
      memcpy (&buf[*bufi], "td", 2);
      break;
    case BT_IBM128:
      memcpy (&buf[*bufi], "if", 2);
      break;
    default:
      (*diag) ("unhandled basetype %d.\n", typeptr->base);
      exit (1);
    }

  *bufi += 2;
}

/* Build a function type descriptor identifier from the return type
   and argument types described by PROTOPTR, and store it if it does
   not already exist.  Return the identifier.  */
static char *
construct_fntype_id (prototype *protoptr)
{
  /* Determine the maximum space for a function type descriptor id.
     Each type requires at most 9 characters (6 for the mode*, 1 for
     the optional 'u' preceding the mode, 1 for the optional 'p'
     preceding the mode, and 1 for an underscore following the mode).
     We also need 5 characters for the string "ftype" that separates
     the return mode from the argument modes.  The last argument doesn't
     need a trailing underscore, but we count that as the one trailing
     "ftype" instead.  For the special case of zero arguments, we need 9
     for the return type and 7 for "ftype_v".  Finally, we need one
     character for the terminating null.  Thus for a function with N
     arguments, we need at most 9N+15 characters for N>0, otherwise 17.
     ----
       *Worst case is bv16qi for "vector bool char".  */
  int len = protoptr->nargs ? (protoptr->nargs + 1) * 9 + 6 : 17;
  char *buf = (char *) malloc (len);
  int bufi = 0;

  if (protoptr->rettype.ispointer)
    buf[bufi++] = 'p';

  if (protoptr->rettype.isvoid)
    buf[bufi++] = 'v';
  else
    {
      if (protoptr->rettype.isunsigned)
	buf[bufi++] = 'u';
      if (protoptr->rettype.isvector)
	complete_vector_type (&protoptr->rettype, buf, &bufi);
      else
	complete_base_type (&protoptr->rettype, buf, &bufi);
    }

  memcpy (&buf[bufi], "_ftype", 6);
  bufi += 6;

  if (!protoptr->nargs)
    {
      memcpy (&buf[bufi], "_v", 2);
      bufi += 2;
    }
  else
    {
      typelist *argptr = protoptr->args;
      for (int i = 0; i < protoptr->nargs; i++, argptr = argptr->next)
	{
	  assert (argptr);
	  buf[bufi++] = '_';
	  if (argptr->info.isconst
	      && argptr->info.base == BT_INT
	      && !argptr->info.ispointer)
	    {
	      buf[bufi++] = 'c';
	      buf[bufi++] = 'i';
	      continue;
	    }
	  if (argptr->info.ispointer)
	    {
	      if (argptr->info.isvoid)
		{
		  if (argptr->info.isconst)
		    {
		      memcpy (&buf[bufi], "pcvoid", 6);
		      bufi += 6;
		      continue;
		    }
		  else
		    {
		      buf[bufi++] = 'p';
		      buf[bufi++] = 'v';
		      continue;
		    }
		}
	      else
		buf[bufi++] = 'p';
	    }

	  if (argptr->info.isunsigned)
	    buf[bufi++] = 'u';
	  if (argptr->info.isvector)
	    complete_vector_type (&argptr->info, buf, &bufi);
	  else
	    complete_base_type (&argptr->info, buf, &bufi);
	}
      assert (!argptr);
    }

  buf[bufi] = '\0';

  /* Ignore return value, as duplicates are fine and expected here.  */
  rbt_insert (&fntype_rbt, buf);

  return buf;
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

  /* Build a function type descriptor identifier from the return type
     and argument types, and store it if it does not already exist.  */
  bifs[curr_bif].fndecl = construct_fntype_id (&bifs[curr_bif].proto);

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
  int orig_len = strlen (bifs[curr_bif].idname);
  char *buf = (char *) malloc (orig_len + 7);
  sprintf (buf, "%s:%05d", bifs[curr_bif].idname, curr_bif);

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

/* Parse one two-line entry in the overload file.  */
static parse_codes
parse_ovld_entry (void)
{
  /* Check for end of stanza.  */
  pos = 0;
  consume_whitespace ();
  if (linebuf[pos] == '[')
    return PC_EOSTANZA;

  /* Allocate an entry in the overload table.  */
  if (num_ovlds >= MAXOVLDS - 1)
    {
      (*diag) ("too many overloads.\n");
      return PC_PARSEFAIL;
    }

  curr_ovld = num_ovlds++;
  ovlds[curr_ovld].stanza = curr_ovld_stanza;

  if (parse_prototype (&ovlds[curr_ovld].proto) == PC_PARSEFAIL)
    return PC_PARSEFAIL;

  if (ovlds[curr_ovld].proto.nargs > max_ovld_args)
    max_ovld_args = ovlds[curr_ovld].proto.nargs;

  /* Build a function type descriptor identifier from the return type
     and argument types, and store it if it does not already exist.  */
  ovlds[curr_ovld].fndecl = construct_fntype_id (&ovlds[curr_ovld].proto);

  /* Now process line 2, which just contains the builtin id and an
     optional overload id.  */
  if (!advance_line (ovld_file))
    {
      (*diag) ("unexpected EOF.\n");
      return PC_EOFILE;
    }

  pos = 0;
  consume_whitespace ();
  int oldpos = pos;
  char *id = match_identifier ();
  ovlds[curr_ovld].bif_id_name = id;
  ovlds[curr_ovld].ovld_id_name = id;
  if (!id)
    {
      (*diag) ("missing overload id at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }

#ifdef DEBUG
  (*diag) ("ID name is '%s'.\n", id);
#endif

  /* The builtin id has to match one from the bif file.  */
  if (!rbt_find (&bif_rbt, id))
    {
      (*diag) ("builtin ID '%s' not found in bif file.\n", id);
      return PC_PARSEFAIL;
    }

  /* Check for an optional overload id.  Usually we use the builtin
     function id for that purpose, but sometimes we need multiple
     overload entries for the same builtin id, and it needs to be unique.  */
  consume_whitespace ();
  if (linebuf[pos] != '\n')
    {
      id = match_identifier ();
      ovlds[curr_ovld].ovld_id_name = id;
      consume_whitespace ();
    }

 /* Save the overload ID in a lookup structure.  */
  if (!rbt_insert (&ovld_rbt, id))
    {
      (*diag) ("duplicate overload ID '%s' at column %d.\n", id, oldpos + 1);
      return PC_PARSEFAIL;
    }

  if (linebuf[pos] != '\n')
    {
      (*diag) ("garbage at end of line at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }
  return PC_OK;
}

/* Parse one stanza of the input overload file.  linebuf already contains the
   first line to parse.  */
static parse_codes
parse_ovld_stanza (void)
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

  char *stanza_name = match_identifier ();
  if (!stanza_name)
    {
      (*diag) ("no identifier found in stanza header.\n");
      return PC_PARSEFAIL;
    }

  /* Add the identifier to a table and set the number to be recorded
     with subsequent overload entries.  */
  if (num_ovld_stanzas >= MAXOVLDSTANZAS)
    {
      (*diag) ("too many stanza headers.\n");
      return PC_PARSEFAIL;
    }

  curr_ovld_stanza = num_ovld_stanzas++;
  ovld_stanza *stanza = &ovld_stanzas[curr_ovld_stanza];
  stanza->stanza_id = stanza_name;

  consume_whitespace ();
  if (linebuf[pos] != ',')
    {
      (*diag) ("missing comma at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }
  safe_inc_pos ();

  consume_whitespace ();
  stanza->extern_name = match_identifier ();
  if (!stanza->extern_name)
    {
      (*diag) ("missing external name at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }

  consume_whitespace ();
  if (linebuf[pos] != ',')
    {
      (*diag) ("missing comma at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }
  safe_inc_pos ();

  consume_whitespace ();
  stanza->intern_name = match_identifier ();
  if (!stanza->intern_name)
    {
      (*diag) ("missing internal name at column %d.\n", pos + 1);
      return PC_PARSEFAIL;
    }

  consume_whitespace ();
  if (linebuf[pos] == ',')
    {
      safe_inc_pos ();
      consume_whitespace ();
      stanza->ifdef = match_identifier ();
      if (!stanza->ifdef)
	{
	  (*diag) ("missing ifdef token at column %d.\n", pos + 1);
	  return PC_PARSEFAIL;
	}
      consume_whitespace ();
    }
  else
    stanza->ifdef = 0;

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
      if (!advance_line (ovld_file))
	return PC_EOFILE;

      result = parse_ovld_entry ();
      if (result == PC_EOFILE || result == PC_PARSEFAIL)
	return result;
    }

  return PC_OK;
}

/* Parse the overload file.  */
static parse_codes
parse_ovld (void)
{
  parse_codes result = PC_OK;
  diag = &ovld_diag;

  if (!advance_line (ovld_file))
    return PC_OK;

  while (result == PC_OK)
    result = parse_ovld_stanza ();

  if (result == PC_EOFILE)
    return PC_OK;
  return result;
}

/* Write a comment at the top of FILE about how the code was generated.  */
static void
write_autogenerated_header (FILE *file)
{
  fprintf (file, "/* Automatically generated by the program '%s'\n",
	   pgm_path);
  fprintf (file, "   from the files '%s' and '%s'.  */\n\n",
	   bif_path, ovld_path);
}

/* Write declarations into the header file.  */
static void
write_decls (void)
{
  fprintf (header_file, "enum rs6000_gen_builtins\n{\n  RS6000_BIF_NONE,\n");
  for (int i = 0; i <= curr_bif; i++)
    fprintf (header_file, "  RS6000_BIF_%s,\n", bifs[bif_order[i]].idname);
  fprintf (header_file, "  RS6000_BIF_MAX,\n");
  fprintf (header_file, "  RS6000_OVLD_NONE,\n");
  for (int i = 0; i < num_ovld_stanzas; i++)
    fprintf (header_file, "  RS6000_OVLD_%s,\n", ovld_stanzas[i].stanza_id);
  fprintf (header_file, "  RS6000_OVLD_MAX\n};\n\n");

  fprintf (header_file,
	   "extern GTY(()) tree rs6000_builtin_decls_x[RS6000_OVLD_MAX];\n\n");

  fprintf (header_file,
	   "enum rs6000_ovld_instances\n{\n  RS6000_INST_NONE,\n");
  for (int i = 0; i <= curr_ovld; i++)
    fprintf (header_file, "  RS6000_INST_%s,\n", ovlds[i].ovld_id_name);
  fprintf (header_file, "  RS6000_INST_MAX\n};\n\n");

  fprintf (header_file, "#define MAX_OVLD_ARGS %d\n", max_ovld_args);

  fprintf (header_file, "enum restriction {\n");
  fprintf (header_file, "  RES_NONE,\n");
  fprintf (header_file, "  RES_BITS,\n");
  fprintf (header_file, "  RES_RANGE,\n");
  fprintf (header_file, "  RES_VAR_RANGE,\n");
  fprintf (header_file, "  RES_VALUES\n");
  fprintf (header_file, "};\n\n");

  fprintf (header_file, "enum bif_enable {\n");
  fprintf (header_file, "  ENB_ALWAYS,\n");
  fprintf (header_file, "  ENB_P5,\n");
  fprintf (header_file, "  ENB_P6,\n");
  fprintf (header_file, "  ENB_ALTIVEC,\n");
  fprintf (header_file, "  ENB_CELL,\n");
  fprintf (header_file, "  ENB_VSX,\n");
  fprintf (header_file, "  ENB_P7,\n");
  fprintf (header_file, "  ENB_P7_64,\n");
  fprintf (header_file, "  ENB_P8,\n");
  fprintf (header_file, "  ENB_P8V,\n");
  fprintf (header_file, "  ENB_P9,\n");
  fprintf (header_file, "  ENB_P9_64,\n");
  fprintf (header_file, "  ENB_P9V,\n");
  fprintf (header_file, "  ENB_IEEE128_HW,\n");
  fprintf (header_file, "  ENB_DFP,\n");
  fprintf (header_file, "  ENB_CRYPTO,\n");
  fprintf (header_file, "  ENB_HTM,\n");
  fprintf (header_file, "  ENB_P10,\n");
  fprintf (header_file, "  ENB_P10_64,\n");
  fprintf (header_file, "  ENB_MMA\n");
  fprintf (header_file, "};\n\n");

  fprintf (header_file, "#define PPC_MAXRESTROPNDS 3\n");
  fprintf (header_file, "struct GTY((user)) bifdata\n");
  fprintf (header_file, "{\n");
  fprintf (header_file, "  const char *bifname;\n");
  fprintf (header_file, "  bif_enable enable;\n");
  fprintf (header_file, "  tree fntype;\n");
  fprintf (header_file, "  insn_code icode;\n");
  fprintf (header_file, "  int  nargs;\n");
  fprintf (header_file, "  int  bifattrs;\n");
  fprintf (header_file, "  int  restr_opnd[PPC_MAXRESTROPNDS];\n");
  fprintf (header_file, "  restriction restr[PPC_MAXRESTROPNDS];\n");
  fprintf (header_file, "  int  restr_val1[PPC_MAXRESTROPNDS];\n");
  fprintf (header_file, "  int  restr_val2[PPC_MAXRESTROPNDS];\n");
  fprintf (header_file, "  const char *attr_string;\n");
  fprintf (header_file, "  rs6000_gen_builtins assoc_bif;\n");
  fprintf (header_file, "};\n\n");

  fprintf (header_file, "#define bif_init_bit\t\t(0x00000001)\n");
  fprintf (header_file, "#define bif_set_bit\t\t(0x00000002)\n");
  fprintf (header_file, "#define bif_extract_bit\t\t(0x00000004)\n");
  fprintf (header_file, "#define bif_nosoft_bit\t\t(0x00000008)\n");
  fprintf (header_file, "#define bif_ldvec_bit\t\t(0x00000010)\n");
  fprintf (header_file, "#define bif_stvec_bit\t\t(0x00000020)\n");
  fprintf (header_file, "#define bif_reve_bit\t\t(0x00000040)\n");
  fprintf (header_file, "#define bif_pred_bit\t\t(0x00000080)\n");
  fprintf (header_file, "#define bif_htm_bit\t\t(0x00000100)\n");
  fprintf (header_file, "#define bif_htmspr_bit\t\t(0x00000200)\n");
  fprintf (header_file, "#define bif_htmcr_bit\t\t(0x00000400)\n");
  fprintf (header_file, "#define bif_mma_bit\t\t(0x00000800)\n");
  fprintf (header_file, "#define bif_quad_bit\t\t(0x00001000)\n");
  fprintf (header_file, "#define bif_pair_bit\t\t(0x00002000)\n");
  fprintf (header_file, "#define bif_mmaint_bit\t\t(0x00004000)\n");
  fprintf (header_file, "#define bif_no32bit_bit\t\t(0x00008000)\n");
  fprintf (header_file, "#define bif_32bit_bit\t\t(0x00010000)\n");
  fprintf (header_file, "#define bif_cpu_bit\t\t(0x00020000)\n");
  fprintf (header_file, "#define bif_ldstmask_bit\t(0x00040000)\n");
  fprintf (header_file, "#define bif_lxvrse_bit\t\t(0x00080000)\n");
  fprintf (header_file, "#define bif_lxvrze_bit\t\t(0x00100000)\n");
  fprintf (header_file, "#define bif_endian_bit\t\t(0x00200000)\n");
  fprintf (header_file, "\n");
  fprintf (header_file,
	   "#define bif_is_init(x)\t\t((x).bifattrs & bif_init_bit)\n");
  fprintf (header_file,
	   "#define bif_is_set(x)\t\t((x).bifattrs & bif_set_bit)\n");
  fprintf (header_file,
	   "#define bif_is_extract(x)\t((x).bifattrs & bif_extract_bit)\n");
  fprintf (header_file,
	   "#define bif_is_nosoft(x)\t((x).bifattrs & bif_nosoft_bit)\n");
  fprintf (header_file,
	   "#define bif_is_ldvec(x)\t\t((x).bifattrs & bif_ldvec_bit)\n");
  fprintf (header_file,
	   "#define bif_is_stvec(x)\t\t((x).bifattrs & bif_stvec_bit)\n");
  fprintf (header_file,
	   "#define bif_is_reve(x)\t\t((x).bifattrs & bif_reve_bit)\n");
  fprintf (header_file,
	   "#define bif_is_predicate(x)\t((x).bifattrs & bif_pred_bit)\n");
  fprintf (header_file,
	   "#define bif_is_htm(x)\t\t((x).bifattrs & bif_htm_bit)\n");
  fprintf (header_file,
	   "#define bif_is_htmspr(x)\t((x).bifattrs & bif_htmspr_bit)\n");
  fprintf (header_file,
	   "#define bif_is_htmcr(x)\t\t((x).bifattrs & bif_htmcr_bit)\n");
  fprintf (header_file,
	   "#define bif_is_mma(x)\t\t((x).bifattrs & bif_mma_bit)\n");
  fprintf (header_file,
	   "#define bif_is_quad(x)\t\t((x).bifattrs & bif_quad_bit)\n");
  fprintf (header_file,
	   "#define bif_is_pair(x)\t\t((x).bifattrs & bif_pair_bit)\n");
  fprintf (header_file,
	   "#define bif_is_mmaint(x)\t\t((x).bifattrs & bif_mmaint_bit)\n");
  fprintf (header_file,
	   "#define bif_is_no32bit(x)\t((x).bifattrs & bif_no32bit_bit)\n");
  fprintf (header_file,
	   "#define bif_is_32bit(x)\t((x).bifattrs & bif_32bit_bit)\n");
  fprintf (header_file,
	   "#define bif_is_cpu(x)\t\t((x).bifattrs & bif_cpu_bit)\n");
  fprintf (header_file,
	   "#define bif_is_ldstmask(x)\t((x).bifattrs & bif_ldstmask_bit)\n");
  fprintf (header_file,
	   "#define bif_is_lxvrse(x)\t((x).bifattrs & bif_lxvrse_bit)\n");
  fprintf (header_file,
	   "#define bif_is_lxvrze(x)\t((x).bifattrs & bif_lxvrze_bit)\n");
  fprintf (header_file,
	   "#define bif_is_endian(x)\t((x).bifattrs & bif_endian_bit)\n");
  fprintf (header_file, "\n");

  /* #### Note that the _x is added for now to avoid conflict with
     the existing rs6000_builtin_info[] file while testing.  It will
     be removed as we progress.  */
  /* #### Cannot mark this as a GC root because only pointer types can
     be marked as GTY((user)) and be GC roots.  All trees in here are
     kept alive by other globals, so not a big deal.  Alternatively,
     we could change the enum fields to ints and cast them in and out
     to avoid requiring a GTY((user)) designation, but that seems
     unnecessarily gross.  */
  fprintf (header_file,
	   "extern bifdata rs6000_builtin_info_x[RS6000_BIF_MAX];\n\n");

  fprintf (header_file, "struct GTY((user)) ovlddata\n");
  fprintf (header_file, "{\n");
  fprintf (header_file, "  const char *bifname;\n");
  fprintf (header_file, "  rs6000_gen_builtins bifid;\n");
  fprintf (header_file, "  tree fntype;\n");
  fprintf (header_file, "  ovlddata *next;\n");
  fprintf (header_file, "};\n\n");

  fprintf (header_file, "struct ovldrecord\n");
  fprintf (header_file, "{\n");
  fprintf (header_file, "  const char *ovld_name;\n");
  fprintf (header_file, "  ovlddata *first_instance;\n");
  fprintf (header_file, "};\n\n");

  fprintf (header_file,
	   "/* #### Cannot mark this as a GC root because only pointer\n"
	   "   types can be marked as GTY((user)) and be GC roots.  All\n"
	   "   trees in here are kept alive by other globals, so not a big\n"
	   "   deal.  Alternatively, we could change the enum fields to ints\n"
	   "   and cast them in and out to avoid requiring a GTY((user))\n"
	   "   designation, but that seems unnecessarily gross.  */\n");
  fprintf (header_file,
	   "extern ovlddata rs6000_instance_info[RS6000_INST_MAX];\n");
  fprintf (header_file, "extern ovldrecord rs6000_overload_info[];\n\n");

  fprintf (header_file, "extern void rs6000_init_generated_builtins ();\n\n");
  fprintf (header_file,
	   "extern bool rs6000_new_builtin_is_supported "
	   "(rs6000_gen_builtins);\n");
  fprintf (header_file,
	   "extern tree rs6000_builtin_decl (unsigned, "
	   "bool ATTRIBUTE_UNUSED);\n\n");
  fprintf (header_file,
	   "extern void gt_ggc_mx (bifdata *bd);\n");
  fprintf (header_file,
	   "extern void gt_pch_nx (bifdata *bd);\n");
  fprintf (header_file,
	   "extern void gt_pch_nx (bifdata *bd, gt_pointer_operator op, "
	   "void *cookie);\n");
  fprintf (header_file,
	   "extern void gt_ggc_mx (ovlddata *od);\n");
  fprintf (header_file,
	   "extern void gt_pch_nx (ovlddata *od);\n");
  fprintf (header_file,
	   "extern void gt_pch_nx (ovlddata *od, gt_pointer_operator op, "
	   "void *cookie);\n");
}

/* Callback functions used for generating trees for function types.  */
void
write_extern_fntype (char *str)
{
  fprintf (header_file, "extern GTY(()) tree %s;\n", str);
}

void
write_fntype (char *str)
{
  fprintf (init_file, "tree %s;\n", str);
}

/* Comparator for bsearch on the type map.  */
int
typemap_cmp (const void *key, const void *entry)
{
  return strcmp ((const char *)key, ((const typemap *)entry)->key);
}

/* Write the type node corresponding to TOK.  */
static void
write_type_node (char *tok, bool indent)
{
  if (indent)
    fprintf (init_file, "  ");
  typemap *entry = (typemap *) bsearch (tok, type_map, TYPE_MAP_SIZE,
					sizeof (typemap), typemap_cmp);
  if (!entry)
    fatal ("Type map is inconsistent.");
  fprintf (init_file, "%s_type_node", entry->value);
}

/* Write an initializer for a function type identified by STR.  */
void
write_fntype_init (char *str)
{
  char *tok;

  /* Check whether we have a "tf" token in this string, representing
     a float128_type_node.  It's possible that float128_type_node is
     undefined (occurs for -maltivec -mno-vsx, for example), so we
     must guard against that.  */
  int tf_found = strstr (str, "tf") != NULL;

  /* Similarly, look for decimal float tokens.  */
  int dfp_found = (strstr (str, "dd") != NULL
		   || strstr (str, "td") != NULL
		   || strstr (str, "sd") != NULL);

  /* Avoid side effects of strtok on the original string by using a copy.  */
  char *buf = strdup (str);

  if (tf_found)
    fprintf (init_file, "  if (float128_type_node)\n  ");
  else if (dfp_found)
    fprintf (init_file, "  if (dfloat64_type_node)\n  ");

  fprintf (init_file, "  %s\n    = build_function_type_list (", buf);
  tok = strtok (buf, "_");
  write_type_node (tok, tf_found || dfp_found);
  tok = strtok (0, "_");
  assert (tok);
  assert (!strcmp (tok, "ftype"));

  tok = strtok (0, "_");
  if (tok)
    fprintf (init_file, ",\n\t\t\t\t");

  /* Note:  A function with no arguments ends with '_ftype_v'.  */
  while (tok && strcmp (tok, "v"))
    {
      write_type_node (tok, tf_found || dfp_found);
      tok = strtok (0, "_");
      fprintf (init_file, ",\n\t\t\t\t");
    }
  fprintf (init_file, "NULL_TREE);\n");
  free (buf);
}

/* Write everything to the header file (rs6000-builtins.h).  Return
   1 if successful, 0 otherwise.  */
static int
write_header_file (void)
{
  write_autogenerated_header (header_file);

  fprintf (header_file, "#ifndef _RS6000_BUILTINS_H\n");
  fprintf (header_file, "#define _RS6000_BUILTINS_H 1\n\n");
  fprintf (header_file, "extern int new_builtins_are_live;\n\n");

  write_decls ();

  /* Write function type list declarators to the header file.  */
  rbt_inorder_callback (&fntype_rbt, fntype_rbt.rbt_root, write_extern_fntype);
  fprintf (header_file, "\n");
  fprintf (header_file, "\n#endif\n");

  return 1;
}

/* Write the decl and initializer for rs6000_builtin_info_x[].  */
static void
write_bif_static_init (void)
{
  const char *res[3];
  fprintf (init_file, "bifdata rs6000_builtin_info_x[RS6000_BIF_MAX] =\n");
  fprintf (init_file, "  {\n");
  fprintf (init_file, "    { /* RS6000_BIF_NONE: */\n");
  fprintf (init_file, "      \"\", ENB_ALWAYS, 0, CODE_FOR_nothing, 0,\n");
  fprintf (init_file, "      0, {0, 0, 0}, {RES_NONE, RES_NONE, RES_NONE},\n");
  fprintf (init_file, "      {0, 0, 0}, {0, 0, 0}, \"\", RS6000_BIF_NONE\n");
  fprintf (init_file, "    },\n");
  for (int i = 0; i <= curr_bif; i++)
    {
      bifdata *bifp = &bifs[bif_order[i]];
      fprintf (init_file, "    { /* RS6000_BIF_%s: */\n", bifp->idname);
      fprintf (init_file, "      /* bifname */\t\"%s\",\n",
	       bifp->proto.bifname);
      fprintf (init_file, "      /* enable*/\t%s,\n",
	       enable_string[bifp->stanza]);
      /* Type must be instantiated at run time.  */
      fprintf (init_file, "      /* fntype */\t0,\n");
      fprintf (init_file, "      /* icode */\tCODE_FOR_%s,\n",
	       bifp->patname);
      fprintf (init_file, "      /* nargs */\t%d,\n",
	       bifp->proto.nargs);
      fprintf (init_file, "      /* bifattrs */\t0");
      if (bifp->attrs.isinit)
	fprintf (init_file, " | bif_init_bit");
      if (bifp->attrs.isset)
	fprintf (init_file, " | bif_set_bit");
      if (bifp->attrs.isextract)
	fprintf (init_file, " | bif_extract_bit");
      if (bifp->attrs.isnosoft)
	fprintf (init_file, " | bif_nosoft_bit");
      if (bifp->attrs.isldvec)
	fprintf (init_file, " | bif_ldvec_bit");
      if (bifp->attrs.isstvec)
	fprintf (init_file, " | bif_stvec_bit");
      if (bifp->attrs.isreve)
	fprintf (init_file, " | bif_reve_bit");
      if (bifp->attrs.ispred)
	fprintf (init_file, " | bif_pred_bit");
      if (bifp->attrs.ishtm)
	fprintf (init_file, " | bif_htm_bit");
      if (bifp->attrs.ishtmspr)
	fprintf (init_file, " | bif_htmspr_bit");
      if (bifp->attrs.ishtmcr)
	fprintf (init_file, " | bif_htmcr_bit");
      if (bifp->attrs.ismma)
	fprintf (init_file, " | bif_mma_bit");
      if (bifp->attrs.isquad)
	fprintf (init_file, " | bif_quad_bit");
      if (bifp->attrs.ispair)
	fprintf (init_file, " | bif_pair_bit");
      if (bifp->attrs.ismmaint)
	fprintf (init_file, " | bif_mmaint_bit");
      if (bifp->attrs.isno32bit)
	fprintf (init_file, " | bif_no32bit_bit");
      if (bifp->attrs.is32bit)
	fprintf (init_file, " | bif_32bit_bit");
      if (bifp->attrs.iscpu)
	fprintf (init_file, " | bif_cpu_bit");
      if (bifp->attrs.isldstmask)
	fprintf (init_file, " | bif_ldstmask_bit");
      if (bifp->attrs.islxvrse)
	fprintf (init_file, " | bif_lxvrse_bit");
      if (bifp->attrs.islxvrze)
	fprintf (init_file, " | bif_lxvrze_bit");
      if (bifp->attrs.isendian)
	fprintf (init_file, " | bif_endian_bit");
      fprintf (init_file, ",\n");
      fprintf (init_file, "      /* restr_opnd */\t{%d, %d, %d},\n",
	       bifp->proto.restr_opnd[0], bifp->proto.restr_opnd[1],
	       bifp->proto.restr_opnd[2]);
      for (int j = 0; j < 3; j++)
	if (!bifp->proto.restr_opnd[j])
	  res[j] = "RES_NONE";
	else if (bifp->proto.restr[j] == RES_BITS)
	  res[j] = "RES_BITS";
	else if (bifp->proto.restr[j] == RES_RANGE)
	  res[j] = "RES_RANGE";
	else if (bifp->proto.restr[j] == RES_VALUES)
	  res[j] = "RES_VALUES";
	else if (bifp->proto.restr[j] == RES_VAR_RANGE)
	  res[j] = "RES_VAR_RANGE";
	else
	  res[j] = "ERROR";
      fprintf (init_file, "      /* restr */\t{%s, %s, %s},\n",
	       res[0], res[1], res[2]);
      fprintf (init_file, "      /* restr_val1 */\t{%s, %s, %s},\n",
	       bifp->proto.restr_val1[0] ? bifp->proto.restr_val1[0] : "0",
	       bifp->proto.restr_val1[1] ? bifp->proto.restr_val1[1] : "0",
	       bifp->proto.restr_val1[2] ? bifp->proto.restr_val1[2] : "0");
      fprintf (init_file, "      /* restr_val2 */\t{%s, %s, %s},\n",
	       bifp->proto.restr_val2[0] ? bifp->proto.restr_val2[0] : "0",
	       bifp->proto.restr_val2[1] ? bifp->proto.restr_val2[1] : "0",
	       bifp->proto.restr_val2[2] ? bifp->proto.restr_val2[2] : "0");
      fprintf (init_file, "      /* attr_string */\t\"%s\",\n",
	       (bifp->kind == FNK_CONST ? "= const"
		: (bifp->kind == FNK_PURE ? "= pure"
		   : (bifp->kind == FNK_FPMATH ? "= fp, const"
		      : ""))));
      fprintf (init_file, "      /* assoc_bif */\tRS6000_BIF_%s%s\n",
	       bifp->attrs.ismmaint ? bifp->idname : "NONE",
	       bifp->attrs.ismmaint ? "_INTERNAL" : "");
      fprintf (init_file, "    },\n");
    }
  fprintf (init_file, "  };\n\n");
}

/* Write the decls and initializers for rs6000_overload_info[] and
   rs6000_instance_info[].  */
static void
write_ovld_static_init (void)
{
  fprintf (init_file,
	   "ovldrecord rs6000_overload_info[RS6000_OVLD_MAX "
	   "- RS6000_OVLD_NONE] =\n");
  fprintf (init_file, "  {\n");
  fprintf (init_file, "    { /* RS6000_OVLD_NONE: */\n");
  fprintf (init_file, "      \"\", NULL\n");
  fprintf (init_file, "    },\n");
  for (int i = 0; i <= curr_ovld_stanza; i++)
    {
      fprintf (init_file, "    { /* RS6000_OVLD_%s: */\n",
	       ovld_stanzas[i].stanza_id);
      fprintf (init_file, "      /* ovld_name */\t\"%s\",\n",
	       ovld_stanzas[i].intern_name);
      /* First-instance must currently be instantiated at run time.  */
      fprintf (init_file, "      /* first_instance */\tNULL\n");
      fprintf (init_file, "    },\n");
    }
  fprintf (init_file, "  };\n\n");

  fprintf (init_file, "ovlddata rs6000_instance_info[RS6000_INST_MAX] =\n");
  fprintf (init_file, "  {\n");
  fprintf (init_file, "    { /* RS6000_INST_NONE: */\n");
  fprintf (init_file, "      \"\", RS6000_BIF_NONE, NULL_TREE, NULL\n");
  fprintf (init_file, "    },\n");
  for (int i = 0; i <= curr_ovld; i++)
    {
      fprintf (init_file, "    { /* RS6000_INST_%s: */\n",
	       ovlds[i].ovld_id_name);
      fprintf (init_file, "      /* bifname */\t\"%s\",\n",
	       ovlds[i].proto.bifname);
      fprintf (init_file, "      /* bifid */\tRS6000_BIF_%s,\n",
	       ovlds[i].bif_id_name);
      /* Type must be instantiated at run time.  */
      fprintf (init_file, "      /* fntype */\t0,\n");
      fprintf (init_file, "      /* next */\t");
      if (i < curr_ovld
	  && !strcmp (ovlds[i+1].proto.bifname, ovlds[i].proto.bifname))
	fprintf (init_file,
		 "&rs6000_instance_info[RS6000_INST_%s]\n",
		 ovlds[i+1].ovld_id_name);
      else
	fprintf (init_file, "NULL\n");
      fprintf (init_file, "    },\n");
    }
  fprintf (init_file, "  };\n\n");
}

/* Write code to initialize the built-in function table.  */
static void
write_init_bif_table (void)
{
  for (int i = 0; i <= curr_bif; i++)
    {
      fprintf (init_file,
	       "  rs6000_builtin_info_x[RS6000_BIF_%s].fntype"
	       "\n    = %s;\n",
	       bifs[i].idname, bifs[i].fndecl);

      /* Check whether we have a "tf" token in this string, representing
	 a float128_type_node.  It's possible that float128_type_node is
	 undefined (occurs for -maltivec -mno-vsx, for example), so we
	 must guard against that.  */
      int tf_found = strstr (bifs[i].fndecl, "tf") != NULL;

      /* Similarly, look for decimal float tokens.  */
      int dfp_found = (strstr (bifs[i].fndecl, "sd") != NULL
		       || strstr (bifs[i].fndecl, "dd") != NULL
		       || strstr (bifs[i].fndecl, "td") != NULL);

      fprintf (init_file,
	       "  if (new_builtins_are_live)\n");
      fprintf (init_file, "    {\n");

      if (tf_found)
	{
	  fprintf (init_file, "      if (float128_type_node)\n");
	  fprintf (init_file, "        {\n");
	}
      else if (dfp_found)
	{
	  fprintf (init_file, "      if (dfloat64_type_node)\n");
	  fprintf (init_file, "        {\n");
	}

      fprintf (init_file,
	       "      rs6000_builtin_decls_x[(int)RS6000_BIF_%s] = t\n",
	       bifs[i].idname);
      fprintf (init_file,
	       "        = add_builtin_function (\"%s\",\n",
	       bifs[i].proto.bifname);
      fprintf (init_file,
	       "                                %s,\n",
	       bifs[i].fndecl);
      fprintf (init_file,
	       "                                (int)RS6000_BIF_%s,"
	       " BUILT_IN_MD,\n",
	       bifs[i].idname);
      fprintf (init_file,
	       "                                NULL, NULL_TREE);\n");
      if (bifs[i].kind == FNK_CONST)
	{
	  fprintf (init_file, "      TREE_READONLY (t) = 1;\n");
	  fprintf (init_file, "      TREE_NOTHROW (t) = 1;\n");
	}
      else if (bifs[i].kind == FNK_PURE)
	{
	  fprintf (init_file, "      DECL_PURE_P (t) = 1;\n");
	  fprintf (init_file, "      TREE_NOTHROW (t) = 1;\n");
	}
      else if (bifs[i].kind == FNK_FPMATH)
	{
	  fprintf (init_file, "      TREE_NOTHROW (t) = 1;\n");
	  fprintf (init_file, "      if (flag_rounding_math)\n");
	  fprintf (init_file, "        {\n");
	  fprintf (init_file, "          DECL_PURE_P (t) = 1;\n");
	  fprintf (init_file, "          DECL_IS_NOVOPS (t) = 1;\n");
	  fprintf (init_file, "        }\n");
	  fprintf (init_file, "      else\n");
	  fprintf (init_file, "        TREE_READONLY (t) = 1;\n");
	}

      if (tf_found || dfp_found)
	{
	  fprintf (init_file, "        }\n");
	  fprintf (init_file, "      else\n");
	  fprintf (init_file, "        {\n");
	  fprintf (init_file, "          rs6000_builtin_decls_x"
		   "[(int)RS6000_BIF_%s] = NULL_TREE;\n", bifs[i].idname);
	  fprintf (init_file, "        }\n");
	}
      fprintf (init_file, "    }\n\n");
    }
}

/* Write code to initialize the overload table.  */
static void
write_init_ovld_table (void)
{
  fprintf (init_file, "  int base = RS6000_OVLD_NONE;\n\n");
  fprintf (init_file,
	   "  /* The fndecl for an overload is arbitrarily the first one\n"
	   "     for the overload.  We sort out the real types when\n"
	   "     processing the overload in the gcc front end.  */\n");

  for (int i = 0; i <= curr_ovld; i++)
    {
      fprintf (init_file,
	       "  rs6000_instance_info[RS6000_INST_%s].fntype"
	       "\n    = %s;\n",
	       ovlds[i].ovld_id_name, ovlds[i].fndecl);

      if (i == 0 || ovlds[i].stanza != ovlds[i-1].stanza)
	{
	  ovld_stanza *stanza = &ovld_stanzas[ovlds[i].stanza];
	  fprintf (init_file, "\n");

	  /* Check whether we have a "tf" token in this string, representing
	     a float128_type_node.  It's possible that float128_type_node is
	     undefined (occurs for -maltivec -mno-vsx, for example), so we
	     must guard against that.  */
	  int tf_found = strstr (ovlds[i].fndecl, "tf") != NULL;

	  /* Similarly, look for decimal float tokens.  */
	  int dfp_found = (strstr (ovlds[i].fndecl, "sd") != NULL
			   || strstr (ovlds[i].fndecl, "dd") != NULL
			   || strstr (ovlds[i].fndecl, "td") != NULL);

	  fprintf (init_file,
		   "  if (new_builtins_are_live)\n");
	  fprintf (init_file, "    {\n");

	  if (tf_found)
	    {
	      fprintf (init_file, "      if (float128_type_node)\n");
	      fprintf (init_file, "        {\n");
	    }
	  else if (dfp_found)
	    {
	      fprintf (init_file, "      if (dfloat64_type_node)\n");
	      fprintf (init_file, "        {\n");
	    }

	  fprintf (init_file,
		   "      rs6000_builtin_decls_x[(int)RS6000_OVLD_%s] = t\n",
		   stanza->stanza_id);
	  fprintf (init_file,
		   "        = add_builtin_function (\"%s\",\n",
		   stanza->intern_name);
	  fprintf (init_file,
		   "                                %s,\n",
		   ovlds[i].fndecl);
	  fprintf (init_file,
		   "                                (int)RS6000_OVLD_%s,"
		   " BUILT_IN_MD,\n",
		   stanza->stanza_id);
	  fprintf (init_file,
		   "                                NULL, NULL_TREE);\n");

	  if (tf_found || dfp_found)
	    fprintf (init_file, "        }\n");

	  fprintf (init_file, "    }\n\n");

	  fprintf (init_file,
		   "  rs6000_overload_info[RS6000_OVLD_%s - base]"
		   ".first_instance\n",
		   stanza->stanza_id);
	  fprintf (init_file,
		   "    = &rs6000_instance_info[RS6000_INST_%s];\n\n",
		   ovlds[i].ovld_id_name);
	}
    }
}

/* Write everything to the initialization file (rs6000-builtins.c).
   Return 1 if successful, 0 otherwise.  */
static int
write_init_file (void)
{
  write_autogenerated_header (init_file);

  fprintf (init_file, "#include \"config.h\"\n");
  fprintf (init_file, "#include \"system.h\"\n");
  fprintf (init_file, "#include \"coretypes.h\"\n");
  fprintf (init_file, "#include \"backend.h\"\n");
  fprintf (init_file, "#include \"rtl.h\"\n");
  fprintf (init_file, "#include \"tree.h\"\n");
  fprintf (init_file, "#include \"langhooks.h\"\n");
  fprintf (init_file, "#include \"insn-codes.h\"\n");
  fprintf (init_file, "#include \"rs6000-builtins.h\"\n");
  fprintf (init_file, "\n");

  fprintf (init_file, "int new_builtins_are_live = 0;\n\n");

  fprintf (init_file, "tree rs6000_builtin_decls_x[RS6000_OVLD_MAX];\n\n");

  write_bif_static_init ();
  write_ovld_static_init ();

  rbt_inorder_callback (&fntype_rbt, fntype_rbt.rbt_root, write_fntype);
  fprintf (init_file, "\n");

  fprintf (init_file, "void\n");
  fprintf (init_file, "rs6000_init_generated_builtins ()\n");
  fprintf (init_file, "{\n");
  fprintf (init_file, "  tree t;\n");
  rbt_inorder_callback (&fntype_rbt, fntype_rbt.rbt_root, write_fntype_init);
  fprintf (init_file, "\n");

  fprintf (init_file,
	   "  rs6000_builtin_decls_x[RS6000_BIF_NONE] = NULL_TREE;\n");
  fprintf (init_file,
	   "  rs6000_builtin_decls_x[RS6000_BIF_MAX] = NULL_TREE;\n");
  fprintf (init_file,
	   "  rs6000_builtin_decls_x[RS6000_OVLD_NONE] = NULL_TREE;\n\n");

  write_init_bif_table ();
  write_init_ovld_table ();

  fprintf (init_file, "}\n\n");

  fprintf (init_file,
	   "void gt_ggc_mx (bifdata *bd)\n");
  fprintf (init_file,
	   "{\n  gt_ggc_mx (bd->fntype);\n}\n\n");
  fprintf (init_file,
	   "void gt_pch_nx (bifdata *bd)\n");
  fprintf (init_file,
	   "{\n  gt_pch_nx (bd->fntype);\n}\n\n");
  fprintf (init_file,
	   "void gt_pch_nx (bifdata *bd, gt_pointer_operator op, "
	   "void *cookie)\n");
  fprintf (init_file,
	   "{\n  op(&(bd->fntype), cookie);\n}\n\n");
  fprintf (init_file,
	   "void gt_ggc_mx (ovlddata *od)\n");
  fprintf (init_file,
	   "{\n  gt_ggc_mx (od->fntype);\n}\n\n");
  fprintf (init_file,
	   "void gt_pch_nx (ovlddata *od)\n");
  fprintf (init_file,
	   "{\n  gt_pch_nx (od->fntype);\n}\n\n");
  fprintf (init_file,
	   "void gt_pch_nx (ovlddata *od, gt_pointer_operator op, "
	   "void *cookie)\n");
  fprintf (init_file,
	   "{\n  op(&(od->fntype), cookie);\n}\n");

  return 1;
}

/* Write everything to the include file (rs6000-vecdefines.h).
   Return 1 if successful, 0 otherwise.  */
static int
write_defines_file (void)
{
  fprintf (defines_file, "#ifndef _RS6000_VECDEFINES_H\n");
  fprintf (defines_file, "#define _RS6000_VECDEFINES_H 1\n\n");
  fprintf (defines_file, "#if defined(_ARCH_PPC64) && defined (_ARCH_PWR9)\n");
  fprintf (defines_file, "  #define _ARCH_PPC64_PWR9 1\n");
  fprintf (defines_file, "#endif\n\n");
  for (int i = 0; i < num_ovld_stanzas; i++)
    if (strcmp (ovld_stanzas[i].extern_name, "SKIP"))
      {
	if (ovld_stanzas[i].ifdef)
	  fprintf (defines_file, "#ifdef %s\n", ovld_stanzas[i].ifdef);
	fprintf (defines_file, "#define %s %s\n",
		 ovld_stanzas[i].extern_name,
		 ovld_stanzas[i].intern_name);
	if (ovld_stanzas[i].ifdef)
	  fprintf (defines_file, "#endif\n");
      }
  fprintf (defines_file, "\n#endif\n");
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

  /* Always close init_file last.  This avoids race conditions in the
     build machinery.  See comments in t-rs6000.  */
  fclose (header_file);
  fclose (defines_file);
  fclose (init_file);

  return 0;
}
