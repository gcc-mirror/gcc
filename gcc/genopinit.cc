/* Generate code to initialize optabs from machine description.
   Copyright (C) 1993-2024 Free Software Foundation, Inc.

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

/* Vector in which to collect insns that match.  */
static vec<optab_pattern> patterns;

static void
gen_insn (md_rtx_info *info)
{
  optab_pattern p;
  if (find_optab (&p, XSTR (info->def, 0)))
    patterns.safe_push (p);
}

static int
pattern_cmp (const void *va, const void *vb)
{
  const optab_pattern *a = (const optab_pattern *)va;
  const optab_pattern *b = (const optab_pattern *)vb;
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

/* Declare the maybe_code_for_* function for ONAME, and provide
   an inline definition of the assserting code_for_* wrapper.  */

static void
handle_overloaded_code_for (FILE *file, overloaded_name *oname)
{
  fprintf (file, "\nextern insn_code maybe_code_for_%s (", oname->name);
  for (unsigned int i = 0; i < oname->arg_types.length (); ++i)
    fprintf (file, "%s%s", i == 0 ? "" : ", ", oname->arg_types[i]);
  fprintf (file, ");\n");

  fprintf (file, "inline insn_code\ncode_for_%s (", oname->name);
  for (unsigned int i = 0; i < oname->arg_types.length (); ++i)
    fprintf (file, "%s%s arg%d", i == 0 ? "" : ", ", oname->arg_types[i], i);
  fprintf (file, ")\n{\n  insn_code code = maybe_code_for_%s (", oname->name);
  for (unsigned int i = 0; i < oname->arg_types.length (); ++i)
    fprintf (file, "%sarg%d", i == 0 ? "" : ", ", i);
  fprintf (file,
	   ");\n"
	   "  gcc_assert (code != CODE_FOR_nothing);\n"
	   "  return code;\n"
	   "}\n");
}

/* Declare the maybe_gen_* function for ONAME, and provide
   an inline definition of the assserting gen_* wrapper.  */

static void
handle_overloaded_gen (FILE *file, overloaded_name *oname)
{
  unsigned HOST_WIDE_INT seen = 0;
  for (overloaded_instance *instance = oname->first_instance->next;
       instance; instance = instance->next)
    {
      pattern_stats stats;
      get_pattern_stats (&stats, XVEC (instance->insn, 1));
      unsigned HOST_WIDE_INT mask
	= HOST_WIDE_INT_1U << stats.num_generator_args;
      if (seen & mask)
	continue;

      seen |= mask;

      fprintf (file, "\nextern rtx maybe_gen_%s (", oname->name);
      for (unsigned int i = 0; i < oname->arg_types.length (); ++i)
	fprintf (file, "%s%s", i == 0 ? "" : ", ", oname->arg_types[i]);
      for (int i = 0; i < stats.num_generator_args; ++i)
	fprintf (file, ", rtx");
      fprintf (file, ");\n");

      fprintf (file, "inline rtx\ngen_%s (", oname->name);
      for (unsigned int i = 0; i < oname->arg_types.length (); ++i)
	fprintf (file, "%s%s arg%d", i == 0 ? "" : ", ",
		 oname->arg_types[i], i);
      for (int i = 0; i < stats.num_generator_args; ++i)
	fprintf (file, ", rtx x%d", i);
      fprintf (file, ")\n{\n  rtx res = maybe_gen_%s (", oname->name);
      for (unsigned int i = 0; i < oname->arg_types.length (); ++i)
	fprintf (file, "%sarg%d", i == 0 ? "" : ", ", i);
      for (int i = 0; i < stats.num_generator_args; ++i)
	fprintf (file, ", x%d", i);
      fprintf (file,
	       ");\n"
	       "  gcc_assert (res);\n"
	       "  return res;\n"
	       "}\n");
    }
}

int
main (int argc, const char **argv)
{
  FILE *h_file, *s_file;
  unsigned int i, j, n, last_kind[5];
  optab_pattern *p;

  progname = "genopinit";

  if (NUM_OPTABS > 0xfff || NUM_MACHINE_MODES > 0x3ff)
    fatal ("genopinit range assumptions invalid");

  if (!init_rtx_reader_args_cb (argc, argv, handle_arg))
    return (FATAL_EXIT_CODE);

  h_file = open_outfile (header_file_name);
  s_file = open_outfile (source_file_name);

  /* Read the machine description.  */
  md_rtx_info info;
  while (read_md_rtx (&info))
    switch (GET_CODE (info.def))
      {
      case DEFINE_INSN:
      case DEFINE_EXPAND:
	gen_insn (&info);
	break;

      default:
	break;
      }

  /* Sort the collected patterns.  */
  patterns.qsort (pattern_cmp);

  /* Now that we've handled the "extra" patterns, eliminate them from
     the optabs array.  That way they don't get in the way below.  */
  n = num_optabs;
  for (i = 0; i < n; )
    if (optabs[i].base == NULL)
      optabs[i] = optabs[--n];
    else
      ++i;

  /* Sort the (real) optabs.  Better than forcing the optabs.def file to
     remain sorted by kind.  We also scrogged any real ordering with the
     purging of the X patterns above.  */
  qsort (optabs, n, sizeof (optab_def), optab_kind_cmp);

  fprintf (h_file, "#ifndef GCC_INSN_OPINIT_H\n");
  fprintf (h_file, "#define GCC_INSN_OPINIT_H 1\n");

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

  fprintf (h_file, 
	   "typedef enum optab_tag optab;\n"
	   "typedef enum optab_tag convert_optab;\n"
	   "typedef enum optab_tag direct_optab;\n"
	   "\n"
	   "struct optab_libcall_d\n"
	   "{\n"
	   "  char libcall_suffix;\n"
	   "  const char *libcall_basename;\n"
	   "  void (*libcall_gen) (optab, const char *name,\n"
	   "		       char suffix, machine_mode);\n"
	   "};\n"
	   "\n"
	   "struct convert_optab_libcall_d\n"
	   "{\n"
	   "  const char *libcall_basename;\n"
	   "  void (*libcall_gen) (convert_optab, const char *name,\n"
	   "		       machine_mode, machine_mode);\n"
	   "};\n"
	   "\n"
	   "/* Given an enum insn_code, access the function to construct\n"
	   "   the body of that kind of insn.  */\n"
	   "#define GEN_FCN(CODE) (insn_data[CODE].genfun)\n"
	   "\n"
	   "#ifdef NUM_RTX_CODE\n"
	   "/* Contains the optab used for each rtx code, and vice-versa.  */\n"
	   "extern const optab code_to_optab_[NUM_RTX_CODE];\n"
	   "extern const enum rtx_code optab_to_code_[NUM_OPTABS];\n"
	   "\n"
	   "static inline optab\n"
	   "code_to_optab (enum rtx_code code)\n"
	   "{\n"
	   "  return code_to_optab_[code];\n"
	   "}\n"
	   "\n"
	   "static inline enum rtx_code\n"
	   "optab_to_code (optab op)\n"
	   "{\n"
	   "  return optab_to_code_[op];\n"
	   "}\n");

  for (overloaded_name *oname = rtx_reader_ptr->get_overloads ();
       oname; oname = oname->next)
    {
      handle_overloaded_code_for (h_file, oname);
      handle_overloaded_gen (h_file, oname);
    }

  fprintf (h_file,
	   "#endif\n"
	   "\n"
	   "extern const struct convert_optab_libcall_d convlib_def[NUM_CONVLIB_OPTABS];\n"
	   "extern const struct optab_libcall_d normlib_def[NUM_NORMLIB_OPTABS];\n"
	   "\n"
	   "/* Returns the active icode for the given (encoded) optab.  */\n"
	   "extern enum insn_code raw_optab_handler (unsigned);\n"
	   "extern bool swap_optab_enable (optab, machine_mode, bool);\n"
	   "\n"
	   "/* Target-dependent globals.  */\n"
	   "struct target_optabs {\n"
	   "  /* Patterns that are used by optabs that are enabled for this target.  */\n"
	   "  bool pat_enable[NUM_OPTAB_PATTERNS];\n"
	   "\n"
	   "  /* Index VOIDmode caches if the target supports vec_gather_load for any\n"
	   "     vector mode.  Every other index X caches specifically for mode X.\n"
	   "     1 means yes, -1 means no.  */\n"
	   "  signed char supports_vec_gather_load[NUM_MACHINE_MODES];\n"
	   "  signed char supports_vec_scatter_store[NUM_MACHINE_MODES];\n"
	   "};\n"
	   "extern void init_all_optabs (struct target_optabs *);\n"
	   "extern bool partial_vectors_supported_p (void);\n"
	   "\n"
	   "extern struct target_optabs default_target_optabs;\n"
	   "extern struct target_optabs *this_fn_optabs;\n"
	   "#if SWITCHABLE_TARGET\n"
	   "extern struct target_optabs *this_target_optabs;\n"
	   "#else\n"
	   "#define this_target_optabs (&default_target_optabs)\n"
	   "#endif\n");

  fprintf (s_file,
	   "#define IN_TARGET_CODE 1\n"
	   "#include \"config.h\"\n"
	   "#include \"system.h\"\n"
	   "#include \"coretypes.h\"\n"
	   "#include \"backend.h\"\n"
	   "#include \"predict.h\"\n"
	   "#include \"tree.h\"\n"
	   "#include \"rtl.h\"\n"
	   "#include \"alias.h\"\n"
	   "#include \"varasm.h\"\n"
	   "#include \"stor-layout.h\"\n"
	   "#include \"calls.h\"\n"
	   "#include \"memmodel.h\"\n"
	   "#include \"tm_p.h\"\n"
	   "#include \"flags.h\"\n"
	   "#include \"insn-config.h\"\n"
	   "#include \"expmed.h\"\n"
	   "#include \"dojump.h\"\n"
	   "#include \"explow.h\"\n"
	   "#include \"emit-rtl.h\"\n"
	   "#include \"stmt.h\"\n"
	   "#include \"expr.h\"\n"
	   "#include \"insn-codes.h\"\n"
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

  /* Some targets like riscv have a large number of patterns.  In order to
     prevent pathological situations in dataflow analysis split the init
     function into separate ones that initialize 1000 patterns each.  */

  const int patterns_per_function = 1000;

  if (patterns.length () > patterns_per_function)
    {
      unsigned num_init_functions
	= patterns.length () / patterns_per_function + 1;
      for (i = 0; i < num_init_functions; i++)
	{
	  fprintf (s_file, "static void\ninit_optabs_%02d "
		   "(struct target_optabs *optabs)\n{\n", i);
	  fprintf (s_file, "  bool *ena = optabs->pat_enable;\n");
	  unsigned start = i * patterns_per_function;
	  unsigned end = MIN (patterns.length (),
			      (i + 1) * patterns_per_function);
	  for (j = start; j < end; ++j)
	    fprintf (s_file, "  ena[%u] = HAVE_%s;\n", j, patterns[j].name);
	  fprintf (s_file, "}\n\n");
	}

      fprintf (s_file, "void\ninit_all_optabs "
	       "(struct target_optabs *optabs)\n{\n");
      for (i = 0; i < num_init_functions; ++i)
	fprintf (s_file, "  init_optabs_%02d (optabs);\n", i);
      fprintf (s_file, "}\n\n");
    }
  else
    {
      fprintf (s_file, "void\ninit_all_optabs "
	       "(struct target_optabs *optabs)\n{\n");
      fprintf (s_file, "  bool *ena = optabs->pat_enable;\n");
      for (i = 0; patterns.iterate (i, &p); ++i)
	fprintf (s_file, "  ena[%u] = HAVE_%s;\n", i, p->name);
      fprintf (s_file, "}\n\n");
    }

  fprintf (s_file,
	   "/* Returns TRUE if the target supports any of the partial vector\n"
	   "   optabs: while_ult_optab, len_load_optab, len_store_optab,\n"
	   "   mask_len_load_optab or mask_len_store_optab,\n"
	   "   for any mode.  */\n"
	   "bool\npartial_vectors_supported_p (void)\n{\n");
  bool any_match = false;
  fprintf (s_file, "\treturn");
  bool first = true;
  for (i = 0; patterns.iterate (i, &p); ++i)
    {
#define CMP_NAME(N) !strncmp (p->name, (N), strlen ((N)))
      if (CMP_NAME("while_ult") || CMP_NAME ("len_load")
	  || CMP_NAME ("len_store")|| CMP_NAME ("mask_len_load")
	  || CMP_NAME ("mask_len_store"))
	{
	  if (first)
	    fprintf (s_file, " HAVE_%s", p->name);
	  else
	    fprintf (s_file, " || HAVE_%s", p->name);
	  first = false;
	  any_match = true;
	}
    }
  if (!any_match)
    fprintf (s_file, " false");
  fprintf (s_file, ";\n}\n");


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
	   "swap_optab_enable (optab op, machine_mode m, bool set)\n"
	   "{\n"
	   "  unsigned scode = (op << 20) | m;\n"
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

  fprintf (h_file, "#endif\n");
  return (fclose (h_file) == 0 && fclose (s_file) == 0
	  ? SUCCESS_EXIT_CODE : FATAL_EXIT_CODE);
}
