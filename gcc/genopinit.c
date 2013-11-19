/* Generate code to initialize optabs from machine description.
   Copyright (C) 1993-2013 Free Software Foundation, Inc.

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


#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "errors.h"
#include "gensupport.h"


#define DEF_RTL_EXPR(V, N, X, C) #V,

static const char * const rtx_upname[] = {
#include "rtl.def"
};

#undef DEF_RTL_EXPR


/* The entries in optabs.def are categorized:
     C: A "conversion" optab, which uses two modes; has libcall data.
     N: A "normal" optab, which uses one mode; has libcall data.
     D: A "direct" optab, which uses one mode; does not have libcall data.
     V: An "oVerflow" optab.  Like N, but does not record its code in
        code_to_optab.

     CX, NX, VX: An extra pattern entry for a conversion or normal optab.

   These patterns may be present in the MD file with names that contain
   the mode(s) used and the name of the operation.  This array contains
   a list of optabs that need to be initialized.  Within each name,
   $a and $b are used to match a short mode name (the part of the mode
   name not including `mode' and converted to lower-case).

   $I means that only full integer modes should be considered for the
   next mode, and $F means that only float modes should be considered.
   $P means that both full and partial integer modes should be considered.
   $Q means that only fixed-point modes should be considered.

   The pattern may be NULL if the optab exists only for the libcalls
   that we plan to attach to it, and there are no named patterns in
   the md files.  */

#define OPTAB_CL(name, pat, c, b, l)		name,
#define OPTAB_CX(name, pat)
#define OPTAB_CD(name, pat)			name,
#define OPTAB_NL(name, pat, c, b, s, l)		name,
#define OPTAB_NC(name, pat, c)			name,
#define OPTAB_NX(name, pat)
#define OPTAB_VL(name, pat, c, b, s, l)		name,
#define OPTAB_VC(name, pat, c)			name,
#define OPTAB_VX(name, pat)
#define OPTAB_DC(name, pat, c)			name,
#define OPTAB_D(name, pat)			name,

typedef enum optab_tag {
  unknown_optab,
#include "optabs.def"
  NUM_OPTABS
} optab;

#undef OPTAB_CL
#undef OPTAB_CX
#undef OPTAB_CD
#undef OPTAB_NL
#undef OPTAB_NC
#undef OPTAB_NX
#undef OPTAB_VL
#undef OPTAB_VC
#undef OPTAB_VX
#undef OPTAB_DC
#undef OPTAB_D

#define NS "NULL"
#define ZS "'\\0'"
#define OPTAB_CL(o, p, c, b, l)    { #o, p, #b, ZS, #l, o, c, UNKNOWN, 1 },
#define OPTAB_CX(o, p) { #o, p, NULL, NULL, NULL, o, UNKNOWN, UNKNOWN, 1 },
#define OPTAB_CD(o, p) { #o, p, NS, ZS, NS, o, UNKNOWN, UNKNOWN, 2 },
#define OPTAB_NL(o, p, c, b, s, l) { #o, p, #b, #s, #l, o, c, c, 3 },
#define OPTAB_NC(o, p, c)          { #o, p, NS, ZS, NS, o, c, c, 3 },
#define OPTAB_NX(o, p) { #o, p, NULL, NULL, NULL, o, UNKNOWN, UNKNOWN, 3 },
#define OPTAB_VL(o, p, c, b, s, l) { #o, p, #b, #s, #l, o, c, UNKNOWN, 3 },
#define OPTAB_VC(o, p, c)          { #o, p, NS, ZS, NS, o, c, UNKNOWN, 3 },
#define OPTAB_VX(o, p) { #o, p, NULL, NULL, NULL, o, UNKNOWN, UNKNOWN, 3 },
#define OPTAB_DC(o, p, c)          { #o, p, NS, ZS, NS, o, c, c, 4 },
#define OPTAB_D(o, p)  { #o, p, NS, ZS, NS, o, UNKNOWN, UNKNOWN, 4 },

typedef struct optab_def_d
{
  const char *name;
  const char *pattern;
  const char *base;
  const char *suffix;
  const char *libcall;
  unsigned int op;
  enum rtx_code fcode;
  enum rtx_code rcode;
  unsigned int kind;
} optab_def;

static optab_def optabs[] = {
  { "unknown_optab", NULL, NS, ZS, NS, unknown_optab, UNKNOWN, UNKNOWN, 0 },
#include "optabs.def"
};

#undef OPTAB_CL
#undef OPTAB_CX
#undef OPTAB_CD
#undef OPTAB_NL
#undef OPTAB_NC
#undef OPTAB_NX
#undef OPTAB_VL
#undef OPTAB_VC
#undef OPTAB_VX
#undef OPTAB_DC
#undef OPTAB_D

/* Vector in which to collect insns that match.  */

typedef struct pattern_d
{
  const char *name;
  unsigned int op;
  unsigned int m1, m2;
  unsigned int sort_num;
} pattern;


static vec<pattern> patterns;

static bool
match_pattern (pattern *p, const char *name, const char *pat)
{
  bool force_float = false;
  bool force_int = false;
  bool force_partial_int = false;
  bool force_fixed = false;

  if (pat == NULL)
    return false;
  for (; ; ++pat)
    {
      if (*pat != '$')
	{
	  if (*pat != *name++)
	    return false;
	  if (*pat == '\0')
	    return true;
	  continue;
	}
      switch (*++pat)
	{
	case 'I':
	  force_int = 1;
	  break;
	case 'P':
	  force_partial_int = 1;
	  break;
	case 'F':
	  force_float = 1;
	  break;
	case 'Q':
	  force_fixed = 1;
	  break;

	case 'a':
	case 'b':
	  {
	    int i;

	    /* This loop will stop at the first prefix match, so
	       look through the modes in reverse order, in case
	       there are extra CC modes and CC is a prefix of the
	       CC modes (as it should be).  */
	    for (i = (MAX_MACHINE_MODE) - 1; i >= 0; i--)
	      {
		const char *p, *q;
		for (p = GET_MODE_NAME (i), q = name; *p; p++, q++)
		  if (TOLOWER (*p) != *q)
		    break;
		if (*p == 0
		    && (! force_int || mode_class[i] == MODE_INT
			|| mode_class[i] == MODE_VECTOR_INT)
		    && (! force_partial_int
			|| mode_class[i] == MODE_INT
			|| mode_class[i] == MODE_PARTIAL_INT
			|| mode_class[i] == MODE_VECTOR_INT)
		    && (! force_float
			|| mode_class[i] == MODE_FLOAT
			|| mode_class[i] == MODE_DECIMAL_FLOAT
			|| mode_class[i] == MODE_COMPLEX_FLOAT
			|| mode_class[i] == MODE_VECTOR_FLOAT)
		    && (! force_fixed
			|| mode_class[i] == MODE_FRACT
			|| mode_class[i] == MODE_UFRACT
			|| mode_class[i] == MODE_ACCUM
			|| mode_class[i] == MODE_UACCUM
			|| mode_class[i] == MODE_VECTOR_FRACT
			|| mode_class[i] == MODE_VECTOR_UFRACT
			|| mode_class[i] == MODE_VECTOR_ACCUM
			|| mode_class[i] == MODE_VECTOR_UACCUM))
		  break;
	      }

	    if (i < 0)
	      return false;
	    name += strlen (GET_MODE_NAME (i));
	    if (*pat == 'a')
	      p->m1 = i;
	    else
	      p->m2 = i;

	    force_int = false;
	    force_partial_int = false;
	    force_float = false;
	    force_fixed = false;
	  }
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

static void
gen_insn (rtx insn)
{
  const char *name = XSTR (insn, 0);
  pattern p;
  unsigned pindex;

  /* Don't mention "unnamed" instructions.  */
  if (*name == 0 || *name == '*')
    return;
  p.name = name;

  /* See if NAME matches one of the patterns we have for the optabs
     we know about.  */
  for (pindex = 0; pindex < ARRAY_SIZE (optabs); pindex++)
    {
      p.m1 = p.m2 = 0;
      if (match_pattern (&p, name, optabs[pindex].pattern))
	{
	  p.op = optabs[pindex].op;
	  p.sort_num = (p.op << 16) | (p.m2 << 8) | p.m1;
	  patterns.safe_push (p);
	  return;
	}
    }
}

static int
pattern_cmp (const void *va, const void *vb)
{
  const pattern *a = (const pattern *)va;
  const pattern *b = (const pattern *)vb;
  return a->sort_num - b->sort_num;
}

static int
optab_kind_cmp (const void *va, const void *vb)
{
  const optab_def *a = (const optab_def *)va;
  const optab_def *b = (const optab_def *)vb;
  int diff = a->kind - b->kind;
  if (diff == 0)
    diff = a->op - b->op;
  return diff;
}

static int
optab_rcode_cmp (const void *va, const void *vb)
{
  const optab_def *a = (const optab_def *)va;
  const optab_def *b = (const optab_def *)vb;
  return a->rcode - b->rcode;
}

static const char *header_file_name = "init-opinit.h";
static const char *source_file_name = "init-opinit.c";

static bool
handle_arg (const char *arg)
{
  switch (arg[1])
    {
    case 'h':
      header_file_name = &arg[2];
      return true;
    case 'c':
      source_file_name = &arg[2];
      return true;
    default:
      return false;
    }
}

static FILE *
open_outfile (const char *file_name)
{
  FILE *f = fopen (file_name, "w");
  if (!f)
    fatal ("cannot open file %s: %s", file_name, xstrerror (errno));
  fprintf (f,
	   "/* Generated automatically by the program `genopinit'\n"
	   "   from the machine description file `md'.  */\n\n");
  return f;
}

int
main (int argc, char **argv)
{
  FILE *h_file, *s_file;
  unsigned int i, j, n, last_kind[5];
  pattern *p;

  progname = "genopinit";

  if (NUM_OPTABS > 0xffff || MAX_MACHINE_MODE >= 0xff)
    fatal ("genopinit range assumptions invalid");

  if (!init_rtx_reader_args_cb (argc, argv, handle_arg))
    return (FATAL_EXIT_CODE);

  h_file = open_outfile (header_file_name);
  s_file = open_outfile (source_file_name);

  /* Read the machine description.  */
  while (1)
    {
      int line_no, insn_code_number = 0;
      rtx desc = read_md_rtx (&line_no, &insn_code_number);
      if (desc == NULL)
	break;
      if (GET_CODE (desc) == DEFINE_INSN || GET_CODE (desc) == DEFINE_EXPAND)
	gen_insn (desc);
    }

  /* Sort the collected patterns.  */
  qsort (patterns.address (), patterns.length (),
	 sizeof (pattern), pattern_cmp);

  /* Now that we've handled the "extra" patterns, eliminate them from
     the optabs array.  That way they don't get in the way below.  */
  n = ARRAY_SIZE (optabs);
  for (i = 0; i < n; )
    if (optabs[i].base == NULL)
      optabs[i] = optabs[--n];
    else
      ++i;

  /* Sort the (real) optabs.  Better than forcing the optabs.def file to
     remain sorted by kind.  We also scrogged any real ordering with the
     purging of the X patterns above.  */
  qsort (optabs, n, sizeof (optab_def), optab_kind_cmp);

  /* Emit the optab enumeration for the header file.  */
  fprintf (h_file, "enum optab_tag {\n");
  for (i = j = 0; i < n; ++i)
    {
      optabs[i].op = i;
      fprintf (h_file, "  %s,\n", optabs[i].name);
      if (optabs[i].kind != j)
	last_kind[j++] = i - 1;
    }
  fprintf (h_file, "  FIRST_CONV_OPTAB = %s,\n", optabs[last_kind[0]+1].name);
  fprintf (h_file, "  LAST_CONVLIB_OPTAB = %s,\n", optabs[last_kind[1]].name);
  fprintf (h_file, "  LAST_CONV_OPTAB = %s,\n", optabs[last_kind[2]].name);
  fprintf (h_file, "  FIRST_NORM_OPTAB = %s,\n", optabs[last_kind[2]+1].name);
  fprintf (h_file, "  LAST_NORMLIB_OPTAB = %s,\n", optabs[last_kind[3]].name);
  fprintf (h_file, "  LAST_NORM_OPTAB = %s\n", optabs[i-1].name);
  fprintf (h_file, "};\n\n");

  fprintf (h_file, "#define NUM_OPTABS          %u\n", n);
  fprintf (h_file, "#define NUM_CONVLIB_OPTABS  %u\n",
	   last_kind[1] - last_kind[0]);
  fprintf (h_file, "#define NUM_NORMLIB_OPTABS  %u\n",
	   last_kind[3] - last_kind[2]);
  fprintf (h_file, "#define NUM_OPTAB_PATTERNS  %u\n",
	   (unsigned) patterns.length ());

  fprintf (s_file,
	   "#include \"config.h\"\n"
	   "#include \"system.h\"\n"
	   "#include \"coretypes.h\"\n"
	   "#include \"tm.h\"\n"
	   "#include \"tree.h\"\n"
	   "#include \"varasm.h\"\n"
	   "#include \"stor-layout.h\"\n"
	   "#include \"calls.h\"\n"
	   "#include \"rtl.h\"\n"
	   "#include \"tm_p.h\"\n"
	   "#include \"flags.h\"\n"
	   "#include \"insn-config.h\"\n"
	   "#include \"expr.h\"\n"
	   "#include \"optabs.h\"\n"
	   "\n"
	   "struct optab_pat {\n"
	   "  unsigned scode;\n"
	   "  enum insn_code icode;\n"
	   "};\n\n");

  fprintf (s_file,
	   "static const struct optab_pat pats[NUM_OPTAB_PATTERNS] = {\n");
  for (i = 0; patterns.iterate (i, &p); ++i)
    fprintf (s_file, "  { %#08x, CODE_FOR_%s },\n", p->sort_num, p->name);
  fprintf (s_file, "};\n\n");

  fprintf (s_file, "void\ninit_all_optabs (struct target_optabs *optabs)\n{\n");
  fprintf (s_file, "  bool *ena = optabs->pat_enable;\n");
  for (i = 0; patterns.iterate (i, &p); ++i)
    fprintf (s_file, "  ena[%u] = HAVE_%s;\n", i, p->name);
  fprintf (s_file, "}\n\n");

  /* Perform a binary search on a pre-encoded optab+mode*2.  */
  /* ??? Perhaps even better to generate a minimal perfect hash.
     Using gperf directly is awkward since it's so geared to working
     with strings.  Plus we have no visibility into the ordering of
     the hash entries, which complicates the pat_enable array.  */
  fprintf (s_file,
	   "static int\n"
	   "lookup_handler (unsigned scode)\n"
	   "{\n"
	   "  int l = 0, h = ARRAY_SIZE (pats), m;\n"
	   "  while (h > l)\n"
	   "    {\n"
	   "      m = (h + l) / 2;\n"
	   "      if (scode == pats[m].scode)\n"
	   "        return m;\n"
	   "      else if (scode < pats[m].scode)\n"
	   "        h = m;\n"
	   "      else\n"
	   "        l = m + 1;\n"
	   "    }\n"
	   "  return -1;\n"
	   "}\n\n");

  fprintf (s_file,
	   "enum insn_code\n"
	   "raw_optab_handler (unsigned scode)\n"
	   "{\n"
	   "  int i = lookup_handler (scode);\n"
	   "  return (i >= 0 && this_fn_optabs->pat_enable[i]\n"
	   "          ? pats[i].icode : CODE_FOR_nothing);\n"
	   "}\n\n");

  fprintf (s_file,
	   "bool\n"
	   "swap_optab_enable (optab op, enum machine_mode m, bool set)\n"
	   "{\n"
	   "  unsigned scode = (op << 16) | m;\n"
	   "  int i = lookup_handler (scode);\n"
	   "  if (i >= 0)\n"
	   "    {\n"
	   "      bool ret = this_fn_optabs->pat_enable[i];\n"
	   "      this_fn_optabs->pat_enable[i] = set;\n"
	   "      return ret;\n"
	   "    }\n"
	   "  else\n"
	   "    {\n"
	   "      gcc_assert (!set);\n"
	   "      return false;\n"
	   "    }\n"
	   "}\n\n");

  /* C++ (even G++) does not support (non-trivial) designated initializers.
     To work around that, generate these arrays programatically rather than
     by our traditional multiple inclusion of def files.  */

  fprintf (s_file,
	   "const struct convert_optab_libcall_d "
	   "convlib_def[NUM_CONVLIB_OPTABS] = {\n");
  for (i = last_kind[0] + 1; i <= last_kind[1]; ++i)
    fprintf (s_file, "  { %s, %s },\n", optabs[i].base, optabs[i].libcall);
  fprintf (s_file, "};\n\n");

  fprintf (s_file,
	   "const struct optab_libcall_d "
	   "normlib_def[NUM_NORMLIB_OPTABS] = {\n");
  for (i = last_kind[2] + 1; i <= last_kind[3]; ++i)
    fprintf (s_file, "  { %s, %s, %s },\n",
	     optabs[i].suffix, optabs[i].base, optabs[i].libcall);
  fprintf (s_file, "};\n\n");

  fprintf (s_file, "enum rtx_code const optab_to_code_[NUM_OPTABS] = {\n");
  for (i = 0; i < n; ++i)
    fprintf (s_file, "  %s,\n", rtx_upname[optabs[i].fcode]);
  fprintf (s_file, "};\n\n");

  qsort (optabs, n, sizeof (optab_def), optab_rcode_cmp);

  fprintf (s_file, "const optab code_to_optab_[NUM_RTX_CODE] = {\n");
  for (j = 0; optabs[j].rcode == UNKNOWN; ++j)
    continue;
  for (i = 0; i < NON_GENERATOR_NUM_RTX_CODE; ++i)
    {
      if (j < n && optabs[j].rcode == i)
	fprintf (s_file, "  %s,\n", optabs[j++].name);
      else
	fprintf (s_file, "  unknown_optab,\n");
    }
  fprintf (s_file, "};\n\n");

  return (fclose (h_file) == 0 && fclose (s_file) == 0
	  ? SUCCESS_EXIT_CODE : FATAL_EXIT_CODE);
}
