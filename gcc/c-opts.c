/* C/ObjC/C++ command line option handling.
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Neil Booth.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "c-common.h"
#include "c-pragma.h"
#include "flags.h"
#include "toplev.h"
#include "langhooks.h"
#include "tree-inline.h"
#include "diagnostic.h"
#include "intl.h"

/* CPP's options.  */
static cpp_options *cpp_opts;

/* Input filename.  */
static const char *in_fname;

/* Filename and stream for preprocessed output.  */
static const char *out_fname;
static FILE *out_stream;

/* Append dependencies to deps_file.  */
static bool deps_append;

/* If dependency switches (-MF etc.) have been given.  */
static bool deps_seen;

/* Dependency output file.  */
static const char *deps_file;

/* Number of deferred options, deferred options array size.  */
static size_t deferred_count, deferred_size;

static void missing_arg PARAMS ((size_t));
static size_t find_opt PARAMS ((const char *, int));
static void set_Wimplicit PARAMS ((int));
static void complain_wrong_lang PARAMS ((size_t, int));
static void write_langs PARAMS ((char *, int));
static void print_help PARAMS ((void));
static void handle_OPT_d PARAMS ((const char *));
static void set_std_cxx98 PARAMS ((int));
static void set_std_c89 PARAMS ((int, int));
static void set_std_c99 PARAMS ((int));
static void check_deps_environment_vars PARAMS ((void));
static void preprocess_file PARAMS ((void));
static void handle_deferred_opts PARAMS ((void));
static void sanitize_cpp_opts PARAMS ((void));

#ifndef STDC_0_IN_SYSTEM_HEADERS
#define STDC_0_IN_SYSTEM_HEADERS 0
#endif

#define CL_C_ONLY	(1 << 0) /* Only C.  */
#define CL_OBJC_ONLY	(1 << 1) /* Only ObjC.  */
#define CL_CXX_ONLY	(1 << 2) /* Only C++.  */
#define CL_OBJCXX_ONLY	(1 << 3) /* Only ObjC++.  */
#define CL_JOINED	(1 << 4) /* If takes joined argument.  */
#define CL_SEPARATE	(1 << 5) /* If takes a separate argument.  */

#define CL_ARG		(CL_JOINED | CL_SEPARATE)
#define CL_C		(CL_C_ONLY | CL_OBJC_ONLY)
#define CL_OBJC		(CL_OBJC_ONLY | CL_OBJCXX_ONLY)
#define CL_CXX		(CL_CXX_ONLY | CL_OBJCXX_ONLY)
#define CL_ALL		(CL_C | CL_CXX)

/* This is the list of all command line options, with the leading "-"
   removed.  It must be sorted in ASCII collating order.  All options
   beginning with "f" or "W" are implicitly assumed to take a "no-"
   form; this form should not be listed.  The variable "on" is true if
   the positive form is given, otherwise it is false.  If you don't
   want to allow a "no-" form, your handler should reject "on" being
   false by returning zero.  See, for example, the handling of
   -ftabstop=.

   If the user gives an option to a front end that doesn't support it,
   an error is output, mentioning which front ends the option is valid
   for.  If you don't want this, you must accept it for all front
   ends, and test for the front end in the option handler.  See, for
   example, the handling of -Wno-strict-prototypes for C++.

   If you request an argument with CL_JOINED, CL_SEPARATE or their
   combination CL_ARG, it is stored in the variable "arg", which is
   guaranteed to be non-NULL and to not be an empty string.  It points
   to the argument either within the argv[] vector or within one of
   that vector's strings, and so the text is permanent and copies need
   not be made.  Be sure to add an error message in missing_arg() if
   the default is not appropriate.  */

#define COMMAND_LINE_OPTIONS						     \
  OPT("-help",                  CL_ALL,   OPT__help)			     \
  OPT("C",                      CL_ALL,   OPT_C)			     \
  OPT("CC",                     CL_ALL,   OPT_CC)			     \
  OPT("E",			CL_ALL,   OPT_E)			     \
  OPT("H",                      CL_ALL,   OPT_H)			     \
  OPT("M",                      CL_ALL,   OPT_M)			     \
  OPT("MD",                     CL_ALL | CL_SEPARATE, OPT_MD)		     \
  OPT("MF",                     CL_ALL | CL_ARG, OPT_MF)		     \
  OPT("MG",                     CL_ALL,   OPT_MG)			     \
  OPT("MM",                     CL_ALL,   OPT_MM)			     \
  OPT("MMD",                    CL_ALL | CL_SEPARATE, OPT_MMD)		     \
  OPT("MP",                     CL_ALL,   OPT_MP)			     \
  OPT("MQ",                     CL_ALL | CL_ARG, OPT_MQ)		     \
  OPT("MT",                     CL_ALL | CL_ARG, OPT_MT)		     \
  OPT("P",                      CL_ALL,   OPT_P)			     \
  OPT("Wabi",                   CL_CXX,   OPT_Wabi)                          \
  OPT("Wall",			CL_ALL,   OPT_Wall)			     \
  OPT("Wbad-function-cast",	CL_C,     OPT_Wbad_function_cast)	     \
  OPT("Wcast-qual",		CL_ALL,   OPT_Wcast_qual)		     \
  OPT("Wchar-subscripts",	CL_ALL,   OPT_Wchar_subscripts)		     \
  OPT("Wcomment",		CL_ALL,   OPT_Wcomment)			     \
  OPT("Wcomments",              CL_ALL,   OPT_Wcomments)		     \
  OPT("Wconversion",		CL_ALL,   OPT_Wconversion)		     \
  OPT("Wctor-dtor-privacy",	CL_CXX,   OPT_Wctor_dtor_privacy)	     \
  OPT("Wdeprecated",		CL_CXX,   OPT_Wdeprecated)		     \
  OPT("Wdiv-by-zero",		CL_C,     OPT_Wdiv_by_zero)		     \
  OPT("Weffc++",		CL_CXX,   OPT_Weffcxx)			     \
  OPT("Wendif-labels",		CL_ALL,   OPT_Wendif_labels)		     \
  OPT("Werror",                 CL_ALL,   OPT_Werror)			     \
  OPT("Werror-implicit-function-declaration",				     \
	     			CL_C,     OPT_Werror_implicit_function_decl) \
  OPT("Wfloat-equal",		CL_ALL,   OPT_Wfloat_equal)		     \
  OPT("Wformat",		CL_ALL,   OPT_Wformat)			     \
  OPT("Wformat-extra-args",	CL_ALL,   OPT_Wformat_extra_args)	     \
  OPT("Wformat-nonliteral",	CL_ALL,   OPT_Wformat_nonliteral)	     \
  OPT("Wformat-security",	CL_ALL,   OPT_Wformat_security)		     \
  OPT("Wformat-y2k",		CL_ALL,   OPT_Wformat_y2k)		     \
  OPT("Wformat-zero-length",	CL_C,     OPT_Wformat_zero_length)	     \
  OPT("Wformat=",		CL_ALL | CL_JOINED, OPT_Wformat_eq)	     \
  OPT("Wimplicit",		CL_ALL,   OPT_Wimplicit)		     \
  OPT("Wimplicit-function-declaration",	CL_C, OPT_Wimplicit_function_decl)   \
  OPT("Wimplicit-int",		CL_C,	  OPT_Wimplicit_int)		     \
  OPT("Wimport",                CL_ALL,   OPT_Wimport)			     \
  OPT("Wlong-long",		CL_ALL,   OPT_Wlong_long)		     \
  OPT("Wmain",			CL_C,     OPT_Wmain)			     \
  OPT("Wmissing-braces",	CL_ALL,   OPT_Wmissing_braces)		     \
  OPT("Wmissing-declarations",	CL_C,     OPT_Wmissing_declarations)	     \
  OPT("Wmissing-format-attribute",CL_ALL, OPT_Wmissing_format_attribute)     \
  OPT("Wmissing-prototypes",	CL_ALL,   OPT_Wmissing_prototypes)	     \
  OPT("Wmultichar",		CL_ALL,   OPT_Wmultichar)		     \
  OPT("Wnested-externs",	CL_C,     OPT_Wnested_externs)		     \
  OPT("Wnon-template-friend",	CL_CXX,   OPT_Wnon_template_friend)	     \
  OPT("Wnon-virtual-dtor",	CL_CXX,   OPT_Wnon_virtual_dtor)	     \
  OPT("Wnonnull",		CL_C,     OPT_Wnonnull)			     \
  OPT("Wold-style-cast",	CL_CXX,   OPT_Wold_style_cast)		     \
  OPT("Woverloaded-virtual",	CL_CXX,   OPT_Woverloaded_virtual)	     \
  OPT("Wparentheses",		CL_ALL,   OPT_Wparentheses)		     \
  OPT("Wpmf-conversions",	CL_CXX,   OPT_Wpmf_conversions)		     \
  OPT("Wpointer-arith",		CL_ALL,   OPT_Wpointer_arith)		     \
  OPT("Wprotocol",		CL_OBJC,  OPT_Wprotocol)		     \
  OPT("Wredundant-decls",	CL_ALL,   OPT_Wredundant_decls)		     \
  OPT("Wreorder",		CL_CXX,   OPT_Wreorder)			     \
  OPT("Wreturn-type",		CL_ALL,   OPT_Wreturn_type)		     \
  OPT("Wselector",		CL_OBJC,  OPT_Wselector)		     \
  OPT("Wsequence-point",	CL_C,     OPT_Wsequence_point)		     \
  OPT("Wsign-compare",		CL_ALL,   OPT_Wsign_compare)		     \
  OPT("Wsign-promo",		CL_CXX,   OPT_Wsign_promo)		     \
  OPT("Wstrict-prototypes",	CL_ALL,   OPT_Wstrict_prototypes)	     \
  OPT("Wsynth",			CL_CXX,   OPT_Wsynth)			     \
  OPT("Wsystem-headers",	CL_ALL,   OPT_Wsystem_headers)		     \
  OPT("Wtraditional",		CL_C,     OPT_Wtraditional)		     \
  OPT("Wtrigraphs",		CL_ALL,   OPT_Wtrigraphs)		     \
  OPT("Wundeclared-selector",	CL_OBJC,  OPT_Wundeclared_selector)	     \
  OPT("Wundef",			CL_ALL,   OPT_Wundef)			     \
  OPT("Wunknown-pragmas",	CL_ALL,   OPT_Wunknown_pragmas)		     \
  OPT("Wunused-macros",		CL_ALL,   OPT_Wunused_macros)		     \
  OPT("Wwrite-strings",		CL_ALL,   OPT_Wwrite_strings)		     \
  OPT("ansi",			CL_ALL,   OPT_ansi)			     \
  OPT("d",                      CL_ALL | CL_JOINED, OPT_d)		     \
  OPT("fabi-version=",          CL_CXX | CL_JOINED, OPT_fabi_version)        \
  OPT("faccess-control",	CL_CXX,   OPT_faccess_control)		     \
  OPT("fall-virtual",		CL_CXX,   OPT_fall_virtual)		     \
  OPT("falt-external-templates",CL_CXX,   OPT_falt_external_templates)	     \
  OPT("fasm",			CL_ALL,   OPT_fasm)			     \
  OPT("fbuiltin",		CL_ALL,   OPT_fbuiltin)			     \
  OPT("fbuiltin-",		CL_ALL | CL_JOINED, OPT_fbuiltin_)	     \
  OPT("fcheck-new",		CL_CXX,   OPT_fcheck_new)		     \
  OPT("fcond-mismatch",		CL_ALL,   OPT_fcond_mismatch)		     \
  OPT("fconserve-space",	CL_CXX,   OPT_fconserve_space)		     \
  OPT("fconst-strings",		CL_CXX,   OPT_fconst_strings)		     \
  OPT("fconstant-string-class=", CL_OBJC | CL_JOINED,			     \
					  OPT_fconstant_string_class)	     \
  OPT("fdefault-inline",	CL_CXX,   OPT_fdefault_inline)		     \
  OPT("fdollars-in-identifiers",CL_ALL,   OPT_fdollars_in_identifiers)	     \
  OPT("fdump-",			CL_ALL | CL_JOINED, OPT_fdump)		     \
  OPT("felide-constructors",	CL_CXX,   OPT_felide_constructors)	     \
  OPT("fenforce-eh-specs",	CL_CXX,   OPT_fenforce_eh_specs)	     \
  OPT("fenum-int-equiv",	CL_CXX,   OPT_fenum_int_equiv)		     \
  OPT("fexternal-templates",	CL_CXX,   OPT_fexternal_templates)	     \
  OPT("ffixed-form",		CL_C,	  OPT_ffixed_form)		     \
  OPT("ffixed-line-length-",	CL_C | CL_JOINED, OPT_ffixed_line_length)    \
  OPT("ffor-scope",		CL_CXX,   OPT_ffor_scope)		     \
  OPT("ffreestanding",		CL_C,     OPT_ffreestanding)		     \
  OPT("fgnu-keywords",		CL_CXX,   OPT_fgnu_keywords)		     \
  OPT("fgnu-runtime",		CL_OBJC,  OPT_fgnu_runtime)		     \
  OPT("fguiding-decls",		CL_CXX,   OPT_fguiding_decls)		     \
  OPT("fhandle-exceptions",	CL_CXX,   OPT_fhandle_exceptions)	     \
  OPT("fhonor-std",		CL_CXX,   OPT_fhonor_std)		     \
  OPT("fhosted",		CL_C,     OPT_fhosted)			     \
  OPT("fhuge-objects",		CL_CXX,   OPT_fhuge_objects)		     \
  OPT("fimplement-inlines",	CL_CXX,   OPT_fimplement_inlines)	     \
  OPT("fimplicit-inline-templates", CL_CXX, OPT_fimplicit_inline_templates)  \
  OPT("fimplicit-templates",	CL_CXX,   OPT_fimplicit_templates)	     \
  OPT("flabels-ok",		CL_CXX,   OPT_flabels_ok)		     \
  OPT("fms-extensions",		CL_ALL,   OPT_fms_extensions)		     \
  OPT("fname-mangling-version-",CL_CXX | CL_JOINED, OPT_fname_mangling)	     \
  OPT("fnew-abi",		CL_CXX,   OPT_fnew_abi)			     \
  OPT("fnext-runtime",		CL_OBJC,  OPT_fnext_runtime)		     \
  OPT("fnonansi-builtins",	CL_CXX,   OPT_fnonansi_builtins)	     \
  OPT("fnonnull-objects",	CL_CXX,   OPT_fnonnull_objects)		     \
  OPT("foperator-names",	CL_CXX,   OPT_foperator_names)		     \
  OPT("foptional-diags",	CL_CXX,   OPT_foptional_diags)		     \
  OPT("fpermissive",		CL_CXX,   OPT_fpermissive)		     \
  OPT("fpreprocessed",		CL_ALL,   OPT_fpreprocessed)		     \
  OPT("frepo",			CL_CXX,   OPT_frepo)			     \
  OPT("frtti",			CL_CXX,   OPT_frtti)			     \
  OPT("fshort-double",		CL_ALL,   OPT_fshort_double)		     \
  OPT("fshort-enums",		CL_ALL,   OPT_fshort_enums)		     \
  OPT("fshort-wchar",		CL_ALL,   OPT_fshort_wchar)		     \
  OPT("fshow-column",		CL_ALL,   OPT_fshow_column)		     \
  OPT("fsigned-bitfields",	CL_ALL,   OPT_fsigned_bitfields)	     \
  OPT("fsigned-char",		CL_ALL,   OPT_fsigned_char)		     \
  OPT("fsquangle",		CL_CXX,   OPT_fsquangle)		     \
  OPT("fstats",			CL_CXX,   OPT_fstats)			     \
  OPT("fstrict-prototype",	CL_CXX,   OPT_fstrict_prototype)	     \
  OPT("ftabstop=",              CL_ALL | CL_JOINED, OPT_ftabstop)	     \
  OPT("ftemplate-depth-",	CL_CXX | CL_JOINED, OPT_ftemplate_depth)     \
  OPT("fthis-is-variable",	CL_CXX,   OPT_fthis_is_variable)	     \
  OPT("funsigned-bitfields",	CL_ALL,   OPT_funsigned_bitfields)	     \
  OPT("funsigned-char",		CL_ALL,   OPT_funsigned_char)		     \
  OPT("fuse-cxa-atexit",	CL_CXX,   OPT_fuse_cxa_atexit)		     \
  OPT("fvtable-gc",		CL_CXX,   OPT_fvtable_gc)		     \
  OPT("fvtable-thunks",		CL_CXX,   OPT_fvtable_thunks)		     \
  OPT("fweak",			CL_CXX,   OPT_fweak)			     \
  OPT("fxref",			CL_CXX,   OPT_fxref)			     \
  OPT("gen-decls",		CL_OBJC,  OPT_gen_decls)		     \
  OPT("lang-asm",		CL_C_ONLY, OPT_lang_asm)		     \
  OPT("lang-objc",              CL_ALL,   OPT_lang_objc)		     \
  OPT("nostdinc",               CL_ALL,   OPT_nostdinc)			     \
  OPT("nostdinc++",             CL_ALL,   OPT_nostdincplusplus)		     \
  OPT("o",			CL_ALL | CL_ARG, OPT_o)                      \
  OPT("pedantic",		CL_ALL,   OPT_pedantic)			     \
  OPT("pedantic-errors",	CL_ALL,   OPT_pedantic_errors)		     \
  OPT("print-objc-runtime-info", CL_OBJC, OPT_print_objc_runtime_info)	     \
  OPT("remap",			CL_ALL,   OPT_remap)                         \
  OPT("std=c++98",		CL_CXX,	  OPT_std_cplusplus98)		     \
  OPT("std=c89",		CL_C,     OPT_std_c89)			     \
  OPT("std=c99",		CL_C,     OPT_std_c99)			     \
  OPT("std=c9x",		CL_C,     OPT_std_c9x)			     \
  OPT("std=gnu++98",		CL_CXX,	  OPT_std_gnuplusplus98)	     \
  OPT("std=gnu89",		CL_C,     OPT_std_gnu89)		     \
  OPT("std=gnu99",		CL_C,     OPT_std_gnu99)		     \
  OPT("std=gnu9x",		CL_C,     OPT_std_gnu9x)		     \
  OPT("std=iso9899:1990",	CL_C,     OPT_std_iso9899_1990)		     \
  OPT("std=iso9899:199409",	CL_C,     OPT_std_iso9899_199409)	     \
  OPT("std=iso9899:1999",	CL_C,     OPT_std_iso9899_1999)		     \
  OPT("std=iso9899:199x",	CL_C,     OPT_std_iso9899_199x)		     \
  OPT("traditional-cpp",	CL_ALL,   OPT_traditional_cpp)		     \
  OPT("trigraphs",              CL_ALL,   OPT_trigraphs)		     \
  OPT("undef",			CL_ALL,   OPT_undef)			     \
  OPT("v",                      CL_ALL,   OPT_v)			     \
  OPT("w",                      CL_ALL,   OPT_w)

#define OPT(text, flags, code) code,
enum opt_code
{
  COMMAND_LINE_OPTIONS
  N_OPTS
};
#undef OPT

struct cl_option
{
  const char *opt_text;
  unsigned char opt_len;
  unsigned char flags;
  ENUM_BITFIELD (opt_code) opt_code : 2 * CHAR_BIT;
};

#define OPT(text, flags, code) { text, sizeof(text) - 1, flags, code },
#ifdef HOST_EBCDIC
static struct cl_option cl_options[] =
#else
static const struct cl_option cl_options[] =
#endif
{
  COMMAND_LINE_OPTIONS
};
#undef OPT
#undef COMMAND_LINE_OPTIONS

/* Holds switches parsed by c_common_decode_option (), but whose
   handling is deffered to c_common_post_options ().  */
static void defer_opt PARAMS ((enum opt_code, const char *));
static struct deferred_opt
{
  enum opt_code code;
  const char *arg;
} *deferred_opts;


#ifdef HOST_EBCDIC
static int opt_comp PARAMS ((const void *, const void *));

/* Run-time sorting of options array.  */
static int
opt_comp (p1, p2)
     const void *p1, *p2;
{
  return strcmp (((struct cl_option *) p1)->opt_text,
		 ((struct cl_option *) p2)->opt_text);
}
#endif

/* Complain that switch OPT_INDEX expects an argument but none was
   provided.  */
static void
missing_arg (opt_index)
     size_t opt_index;
{
  const char *opt_text = cl_options[opt_index].opt_text;

  switch (cl_options[opt_index].opt_code)
    {
    case OPT_Wformat_eq:
    case OPT_d:
    case OPT_fabi_version:
    case OPT_fbuiltin_:
    case OPT_fdump:
    case OPT_fname_mangling:
    case OPT_ftabstop:
    case OPT_ftemplate_depth:
    default:
      error ("missing argument to \"-%s\"", opt_text);
      break;

    case OPT_fconstant_string_class:
      error ("no class name specified with \"-%s\"", opt_text);
      break;

    case OPT_MF:
    case OPT_MD:
    case OPT_MMD:
    case OPT_o:
      error ("missing filename after \"-%s\"", opt_text);
      break;

    case OPT_MQ:
    case OPT_MT:
      error ("missing target after \"-%s\"", opt_text);
      break;
    }
}

/* Perform a binary search to find which option the command-line INPUT
   matches.  Returns its index in the option array, and N_OPTS on
   failure.

   Complications arise since some options can be suffixed with an
   argument, and multiple complete matches can occur, e.g. -pedantic
   and -pedantic-errors.  Also, some options are only accepted by some
   languages.  If a switch matches for a different language and
   doesn't match any alternatives for the true front end, the index of
   the matched switch is returned anyway.  The caller should check for
   this case.  */
static size_t
find_opt (input, lang_flag)
     const char *input;
     int lang_flag;
{
  size_t md, mn, mx;
  size_t opt_len;
  size_t result = N_OPTS;
  int comp;

  mn = 0;
  mx = N_OPTS;

  while (mx > mn)
    {
      md = (mn + mx) / 2;

      opt_len = cl_options[md].opt_len;
      comp = strncmp (input, cl_options[md].opt_text, opt_len);

      if (comp < 0)
	mx = md;
      else if (comp > 0)
	mn = md + 1;
      else
	{
	  /* The switch matches.  It it an exact match?  */
	  if (input[opt_len] == '\0')
	    return md;
	  else
	    {
	      mn = md + 1;

	      /* If the switch takes no arguments this is not a proper
		 match, so we continue the search (e.g. input="stdc++"
		 match was "stdc").  */
	      if (!(cl_options[md].flags & CL_JOINED))
		continue;

	      /* Is this switch valid for this front end?  */
	      if (!(cl_options[md].flags & lang_flag))
		{
		  /* If subsequently we don't find a better match,
		     return this and let the caller report it as a bad
		     match.  */
		  result = md;
		  continue;
		}

	      /* Two scenarios remain: we have the switch's argument,
		 or we match a longer option.  This can happen with
		 -iwithprefix and -withprefixbefore.  The longest
		 possible option match succeeds.

		 Scan forwards, and return an exact match.  Otherwise
		 return the longest valid option-accepting match (mx).
		 This loops at most twice with current options.  */
	      mx = md;
	      for (md = md + 1; md < (size_t) N_OPTS; md++)
		{
		  opt_len = cl_options[md].opt_len;
		  if (strncmp (input, cl_options[md].opt_text, opt_len))
		    break;
		  if (input[opt_len] == '\0')
		    return md;
		  if (cl_options[md].flags & lang_flag
		      && cl_options[md].flags & CL_JOINED)
		    mx = md;
		}

	      return mx;
	    }
	}
    }

  return result;
}

/* Defer option CODE with argument ARG.  */
static void
defer_opt (code, arg)
     enum opt_code code;
     const char *arg;
{
  /* FIXME: this should be in c_common_init_options, which should take
     argc and argv.  */
  if (!deferred_opts)
    {
      extern int save_argc;
      deferred_size = save_argc;
      deferred_opts = (struct deferred_opt *)
	xmalloc (deferred_size * sizeof (struct deferred_opt));
    }

  if (deferred_count == deferred_size)
    abort ();

  deferred_opts[deferred_count].code = code;
  deferred_opts[deferred_count].arg = arg;
  deferred_count++;
}

/* Common initialization before parsing options.  */
void
c_common_init_options (lang)
     enum c_language_kind lang;
{
#ifdef HOST_EBCDIC
  /* For non-ASCII hosts, the cl_options array needs to be sorted at
     runtime.  */
  qsort (cl_options, N_OPTS, sizeof (struct cl_option), opt_comp);
#endif
#if ENABLE_CHECKING
 {
  size_t i;

  for (i = 1; i < N_OPTS; i++)
    if (strcmp (cl_options[i - 1].opt_text, cl_options[i].opt_text) >= 0)
      error ("options array incorrectly sorted: %s is before %s",
	     cl_options[i - 1].opt_text, cl_options[i].opt_text);
 }
#endif

  c_language = lang;
  parse_in = cpp_create_reader (lang == clk_c ? CLK_GNUC89 : CLK_GNUCXX);
  cpp_opts = cpp_get_options (parse_in);
  if (flag_objc)
    cpp_opts->objc = 1;

  flag_const_strings = (lang == clk_cplusplus);
  warn_pointer_arith = (lang == clk_cplusplus);
  if (lang == clk_c)
    warn_sign_compare = -1;
}

/* Handle one command-line option in (argc, argv).
   Can be called multiple times, to handle multiple sets of options.
   Returns number of strings consumed.  */
int
c_common_decode_option (argc, argv)
     int argc;
     char **argv;
{
  static const int lang_flags[] = {CL_C_ONLY, CL_C, CL_CXX_ONLY, CL_CXX};
  size_t opt_index;
  const char *opt, *arg = 0;
  char *dup = 0;
  bool on = true;
  int result, lang_flag;
  const struct cl_option *option;
  enum opt_code code;

  opt = argv[0];

  /* Interpret "-" or a non-switch as a file name.  */
  if (opt[0] != '-' || opt[1] == '\0')
    {
      if (!in_fname)
	in_fname = opt;
      else if (!out_fname)
	out_fname = opt;
      else
	{
	  error ("too many filenames given.  Type %s --help for usage",
		 progname);
	  return argc;
	}

      return 1;
    }

  /* Drop the "no-" from negative switches.  */
  if ((opt[1] == 'W' || opt[1] == 'f')
      && opt[2] == 'n' && opt[3] == 'o' && opt[4] == '-')
    {
      size_t len = strlen (opt) - 3;

      dup = xmalloc (len + 1);
      dup[0] = '-';
      dup[1] = opt[1];
      memcpy (dup + 2, opt + 5, len - 2 + 1);
      opt = dup;
      on = false;
    }

  result = cpp_handle_option (parse_in, argc, argv);

  /* Skip over '-'.  */
  lang_flag = lang_flags[(c_language << 1) + flag_objc];
  opt_index = find_opt (opt + 1, lang_flag);
  if (opt_index == N_OPTS)
    goto done;

  result = 1;
  option = &cl_options[opt_index];

  /* Sort out any argument the switch takes.  */
  if (option->flags & CL_ARG)
    {
      if (option->flags & CL_JOINED)
	{
	  /* Have arg point to the original switch.  This is because
	     some code, such as disable_builtin_function, expects its
	     argument to be persistent until the program exits.  */
	  arg = argv[0] + cl_options[opt_index].opt_len + 1;
	  if (!on)
	    arg += strlen ("no-");
	}

      /* If we don't have an argument, and CL_SEPARATE, try the next
	 argument in the vector.  */
      if (!arg || (*arg == '\0' && option->flags & CL_SEPARATE))
	{
	  arg = argv[1];
	  result = 2;
	}

      if (!arg || *arg == '\0')
	{
	  missing_arg (opt_index);
	  result = argc;
	  goto done;
	}
    }

  /* Complain about the wrong language after we've swallowed any
     necessary extra argument.  Eventually make this a hard error
     after the call to find_opt, and return argc.  */
  if (!(cl_options[opt_index].flags & lang_flag))
    {
      complain_wrong_lang (opt_index, on);
      goto done;
    }

  switch (code = option->opt_code)
    {
    case N_OPTS: /* Shut GCC up.  */
      break;

    case OPT__help:
      print_help ();
      break;

    case OPT_C:
      cpp_opts->discard_comments = 0;
      break;

    case OPT_CC:
      cpp_opts->discard_comments = 0;
      cpp_opts->discard_comments_in_macro_exp = 0;
      break;

    case OPT_E:
      flag_preprocess_only = 1;
      break;

    case OPT_H:
      cpp_opts->print_include_names = 1;
      break;

    case OPT_M:
    case OPT_MM:
      /* When doing dependencies with -M or -MM, suppress normal
	 preprocessed output, but still do -dM etc. as software
	 depends on this.  Preprocessed output does occur if -MD, -MMD
	 or environment var dependency generation is used.  */
      cpp_opts->deps.style = (code == OPT_M ? DEPS_SYSTEM: DEPS_USER);
      cpp_opts->no_output = 1;
      cpp_opts->inhibit_warnings = 1;
      break;

    case OPT_MD:
    case OPT_MMD:
      cpp_opts->deps.style = (code == OPT_MD ? DEPS_SYSTEM: DEPS_USER);
      deps_file = arg;
      break;

    case OPT_MF:
      deps_seen = true;
      deps_file = arg;
      break;

    case OPT_MG:
      deps_seen = true;
      cpp_opts->deps.missing_files = true;
      break;

    case OPT_MP:
      deps_seen = true;
      cpp_opts->deps.phony_targets = true;
      break;

    case OPT_MQ:
    case OPT_MT:
      deps_seen = true;
      defer_opt (code, arg);
      break;

    case OPT_P:
      cpp_opts->no_line_commands = 1;
      break;

    case OPT_Wabi:
      warn_abi = on;
      break;

    case OPT_Wall:
      set_Wunused (on);
      set_Wformat (on);
      set_Wimplicit (on);
      warn_char_subscripts = on;
      warn_missing_braces = on;
      warn_parentheses = on;
      warn_return_type = on;
      warn_sequence_point = on;	/* Was C only.  */
      if (c_language == clk_cplusplus)
	warn_sign_compare = on;
      warn_switch = on;
      warn_strict_aliasing = on;
      
      /* Only warn about unknown pragmas that are not in system
	 headers.  */                                        
      warn_unknown_pragmas = on;

      /* We save the value of warn_uninitialized, since if they put
	 -Wuninitialized on the command line, we need to generate a
	 warning about not using it without also specifying -O.  */
      if (warn_uninitialized != 1)
	warn_uninitialized = (on ? 2 : 0);

      if (c_language == clk_c)
	/* We set this to 2 here, but 1 in -Wmain, so -ffreestanding
	   can turn it off only if it's not explicit.  */
	warn_main = on * 2;
      else
	{
	  /* C++-specific warnings.  */
	  warn_ctor_dtor_privacy = on;
	  warn_nonvdtor = on;
	  warn_reorder = on;
	  warn_nontemplate_friend = on;
	}

      cpp_opts->warn_trigraphs = on;
      cpp_opts->warn_comments = on;
      cpp_opts->warn_num_sign_change = on;
      cpp_opts->warn_multichar = on;	/* Was C++ only.  */
      break;

    case OPT_Wbad_function_cast:
      warn_bad_function_cast = on;
      break;

    case OPT_Wcast_qual:
      warn_cast_qual = on;
      break;

    case OPT_Wchar_subscripts:
      warn_char_subscripts = on;
      break;

    case OPT_Wcomment:
    case OPT_Wcomments:
      cpp_opts->warn_comments = on;
      break;

    case OPT_Wconversion:
      warn_conversion = on;
      break;

    case OPT_Wctor_dtor_privacy:
      warn_ctor_dtor_privacy = on;
      break;

    case OPT_Wdeprecated:
      warn_deprecated = on;
      break;

    case OPT_Wdiv_by_zero:
      warn_div_by_zero = on;
      break;

    case OPT_Weffcxx:
      warn_ecpp = on;
      break;

    case OPT_Wendif_labels:
      cpp_opts->warn_endif_labels = on;
      break;

    case OPT_Werror:
      cpp_opts->warnings_are_errors = on;
      break;

    case OPT_Werror_implicit_function_decl:
      if (!on)
	result = 0;
      else
	mesg_implicit_function_declaration = 2;
      break;

    case OPT_Wfloat_equal:
      warn_float_equal = on;
      break;

    case OPT_Wformat:
      set_Wformat (on);
      break;

    case OPT_Wformat_eq:
      set_Wformat (atoi (arg));
      break;

    case OPT_Wformat_extra_args:
      warn_format_extra_args = on;
      break;

    case OPT_Wformat_nonliteral:
      warn_format_nonliteral = on;
      break;

    case OPT_Wformat_security:
      warn_format_security = on;
      break;

    case OPT_Wformat_y2k:
      warn_format_y2k = on;
      break;

    case OPT_Wformat_zero_length:
      warn_format_zero_length = on;
      break;

    case OPT_Wimplicit:
      set_Wimplicit (on);
      break;

    case OPT_Wimplicit_function_decl:
      mesg_implicit_function_declaration = on;
      break;

    case OPT_Wimplicit_int:
      warn_implicit_int = on;
      break;

    case OPT_Wimport:
      cpp_opts->warn_import = on;
      break;

    case OPT_Wlong_long:
      warn_long_long = on;
      break;

    case OPT_Wmain:
      if (on)
	warn_main = 1;
      else
	warn_main = -1;
      break;

    case OPT_Wmissing_braces:
      warn_missing_braces = on;
      break;

    case OPT_Wmissing_declarations:
      warn_missing_declarations = on;
      break;

    case OPT_Wmissing_format_attribute:
      warn_missing_format_attribute = on;
      break;

    case OPT_Wmissing_prototypes:
      warn_missing_prototypes = on;
      break;

    case OPT_Wmultichar:
      cpp_opts->warn_multichar = on;
      break;

    case OPT_Wnested_externs:
      warn_nested_externs = on;
      break;

    case OPT_Wnon_template_friend:
      warn_nontemplate_friend = on;
      break;

    case OPT_Wnon_virtual_dtor:
      warn_nonvdtor = on;
      break;

    case OPT_Wnonnull:
      warn_nonnull = on;
      break;

    case OPT_Wold_style_cast:
      warn_old_style_cast = on;
      break;

    case OPT_Woverloaded_virtual:
      warn_overloaded_virtual = on;
      break;

    case OPT_Wparentheses:
      warn_parentheses = on;
      break;

    case OPT_Wpmf_conversions:
      warn_pmf2ptr = on;
      break;

    case OPT_Wpointer_arith:
      warn_pointer_arith = on;
      break;

    case OPT_Wprotocol:
      warn_protocol = on;
      break;

    case OPT_Wselector:
      warn_selector = on;
      break;

    case OPT_Wredundant_decls:
      warn_redundant_decls = on;
      break;

    case OPT_Wreorder:
      warn_reorder = on;
      break;

    case OPT_Wreturn_type:
      warn_return_type = on;
      break;

    case OPT_Wsequence_point:
      warn_sequence_point = on;
      break;

    case OPT_Wsign_compare:
      warn_sign_compare = on;
      break;

    case OPT_Wsign_promo:
      warn_sign_promo = on;
      break;

    case OPT_Wstrict_prototypes:
      if (!on && c_language == clk_cplusplus)
	warning ("-Wno-strict-prototypes is not supported in C++");
      else
	warn_strict_prototypes = on;
      break;

    case OPT_Wsynth:
      warn_synth = on;
      break;

    case OPT_Wsystem_headers:
      cpp_opts->warn_system_headers = on;
      break;

    case OPT_Wtraditional:
      warn_traditional = on;
      cpp_opts->warn_traditional = on;
      break;

    case OPT_Wtrigraphs:
      cpp_opts->warn_trigraphs = on;
      break;

    case OPT_Wundeclared_selector:
      warn_undeclared_selector = on;
      break;

    case OPT_Wundef:
      cpp_opts->warn_undef = on;
      break;

    case OPT_Wunknown_pragmas:
      /* Set to greater than 1, so that even unknown pragmas in
	 system headers will be warned about.  */  
      warn_unknown_pragmas = on * 2;
      break;

    case OPT_Wunused_macros:
      cpp_opts->warn_unused_macros = on;
      break;

    case OPT_Wwrite_strings:
      if (c_language == clk_c)
	flag_const_strings = on;
      else
	warn_write_strings = on;
      break;
      
    case OPT_ansi:
      if (c_language == clk_c)
	set_std_c89 (false, true);
      else
	set_std_cxx98 (true);
      break;

    case OPT_d:
      handle_OPT_d (arg);
      break;

    case OPT_fcond_mismatch:
      if (c_language == clk_c)
	{
	  flag_cond_mismatch = on;
	  break;
	}
      /* Fall through.  */

    case OPT_fall_virtual:
    case OPT_fenum_int_equiv:
    case OPT_fguiding_decls:
    case OPT_fhonor_std:
    case OPT_fhuge_objects:
    case OPT_flabels_ok:
    case OPT_fname_mangling:
    case OPT_fnew_abi:
    case OPT_fnonnull_objects:
    case OPT_fsquangle:
    case OPT_fstrict_prototype:
    case OPT_fthis_is_variable:
    case OPT_fvtable_thunks:
    case OPT_fxref:
      warning ("switch \"%s\" is no longer supported", argv[0]);
      break;

    case OPT_fabi_version:
      flag_abi_version = read_integral_parameter (arg, argv[0], 1);
      break;

    case OPT_faccess_control:
      flag_access_control = on;
      break;

    case OPT_falt_external_templates:
      flag_alt_external_templates = on;
      if (on)
	flag_external_templates = true;
    cp_deprecated:
      warning ("switch \"%s\" is deprecated, please see documentation for details", argv[0]);
      break;

    case OPT_fasm:
      flag_no_asm = !on;
      break;

    case OPT_fbuiltin:
      flag_no_builtin = !on;
      break;

    case OPT_fbuiltin_:
      if (on)
	result = 0;
      else
	disable_builtin_function (arg);
      break;

    case OPT_fdollars_in_identifiers:
      dollars_in_ident = on;
      break;

    case OPT_fdump:
      if (!on || !dump_switch_p (argv[0] + strlen ("-f")))
	result = 0;
      break;

    case OPT_ffreestanding:
      on = !on;
      /* Fall through...  */
    case OPT_fhosted:
      flag_hosted = on;
      flag_no_builtin = !on;
      /* warn_main will be 2 if set by -Wall, 1 if set by -Wmain */
      if (!on && warn_main == 2)
	warn_main = 0;
      break;

    case OPT_fshort_double:
      flag_short_double = on;
      break;

    case OPT_fshort_enums:
      flag_short_enums = on;
      break;

    case OPT_fshort_wchar:
      flag_short_wchar = on;
      break;

    case OPT_fsigned_bitfields:
      flag_signed_bitfields = on;
      explicit_flag_signed_bitfields = 1;
      break;

    case OPT_fsigned_char:
      flag_signed_char = on;
      break;

    case OPT_funsigned_bitfields:
      flag_signed_bitfields = !on;
      explicit_flag_signed_bitfields = 1;
      break;

    case OPT_funsigned_char:
      flag_signed_char = !on;
      break;

    case OPT_fcheck_new:
      flag_check_new = on;
      break;

    case OPT_fconserve_space:
      flag_conserve_space = on;
      break;

    case OPT_fconst_strings:
      flag_const_strings = on;
      break;

    case OPT_fconstant_string_class:
      constant_string_class_name = arg;
      break;

    case OPT_fdefault_inline:
      flag_default_inline = on;
      break;

    case OPT_felide_constructors:
      flag_elide_constructors = on;
      break;

    case OPT_fenforce_eh_specs:
      flag_enforce_eh_specs = on;
      break;

    case OPT_fexternal_templates:
      flag_external_templates = on;
      goto cp_deprecated;

    case OPT_ffixed_form:
    case OPT_ffixed_line_length:
      /* Fortran front end options ignored when preprocessing only.  */
      if (flag_preprocess_only)
        result = -1;
      break;

    case OPT_ffor_scope:
      flag_new_for_scope = on;
      break;

    case OPT_fgnu_keywords:
      flag_no_gnu_keywords = !on;
      break;

    case OPT_fgnu_runtime:
      flag_next_runtime = !on;
      break;

    case OPT_fhandle_exceptions:
      warning ("-fhandle-exceptions has been renamed to -fexceptions (and is now on by default)");
      flag_exceptions = on;
      break;

    case OPT_fimplement_inlines:
      flag_implement_inlines = on;
      break;

    case OPT_fimplicit_inline_templates:
      flag_implicit_inline_templates = on;
      break;

    case OPT_fimplicit_templates:
      flag_implicit_templates = on;
      break;

    case OPT_fms_extensions:
      flag_ms_extensions = on;
      break;

    case OPT_fnext_runtime:
      flag_next_runtime = on;
      break;

    case OPT_fnonansi_builtins:
      flag_no_nonansi_builtin = !on;
      break;

    case OPT_foperator_names:
      cpp_opts->operator_names = on;
      break;

    case OPT_foptional_diags:
      flag_optional_diags = on;
      break;

    case OPT_fpermissive:
      flag_permissive = on;
      break;

    case OPT_fpreprocessed:
      cpp_opts->preprocessed = on;
      break;

    case OPT_frepo:
      flag_use_repository = on;
      if (on)
	flag_implicit_templates = 0;
      break;

    case OPT_frtti:
      flag_rtti = on;
      break;

    case OPT_fshow_column:
      cpp_opts->show_column = on;
      break;

    case OPT_fstats:
      flag_detailed_statistics = on;
      break;

    case OPT_ftabstop:
      /* Don't recognize -fno-tabstop=.  */
      if (!on)
	return 0;

      /* It is documented that we silently ignore silly values.  */
	{
	  char *endptr;
	  long tabstop = strtol (arg, &endptr, 10);
	  if (*endptr == '\0' && tabstop >= 1 && tabstop <= 100)
	    cpp_opts->tabstop = tabstop;
	}
      break;

    case OPT_ftemplate_depth:
      max_tinst_depth = read_integral_parameter (arg, argv[0], 0);
      break;

    case OPT_fvtable_gc:
      flag_vtable_gc = on;
      break;

    case OPT_fuse_cxa_atexit:
      flag_use_cxa_atexit = on;
      break;

    case OPT_fweak:
      flag_weak = on;
      break;

    case OPT_gen_decls:
      flag_gen_declaration = 1;
      break;

    case OPT_lang_asm:
      cpp_set_lang (parse_in, CLK_ASM);
      break;

    case OPT_lang_objc:
      cpp_opts->objc = 1;
      break;

    case OPT_nostdinc:
      /* No default include directories.  You must specify all
	 include-file directories with -I.  */
      cpp_opts->no_standard_includes = 1;
      break;

    case OPT_nostdincplusplus:
      /* No default C++-specific include directories.  */
      cpp_opts->no_standard_cplusplus_includes = 1;
      break;

    case OPT_o:
      if (!out_fname)
	out_fname = arg;
      else
	{
	  error ("output filename specified twice");
	  result = argc;
	}
      break;

      /* We need to handle the -pedantic switches here, rather than in
	 c_common_post_options, so that a subsequent -Wno-endif-labels
	 is not overridden.  */
    case OPT_pedantic_errors:
      cpp_opts->pedantic_errors = 1;
      /* fall through */
    case OPT_pedantic:
      cpp_opts->pedantic = 1;
      cpp_opts->warn_endif_labels = 1;
      break;

    case OPT_print_objc_runtime_info:
      print_struct_values = 1;
      break;

    case OPT_remap:
      cpp_opts->remap = 1;
      break;

    case OPT_std_cplusplus98:
    case OPT_std_gnuplusplus98:
      set_std_cxx98 (code == OPT_std_cplusplus98 /* ISO */);
      break;

    case OPT_std_c89:
    case OPT_std_iso9899_1990:
    case OPT_std_iso9899_199409:
      set_std_c89 (code == OPT_std_iso9899_199409 /* c94 */, true /* ISO */);
      break;

    case OPT_std_gnu89:
      set_std_c89 (false /* c94 */, false /* ISO */);
      break;

    case OPT_std_c99:
    case OPT_std_c9x:
    case OPT_std_iso9899_1999:
    case OPT_std_iso9899_199x:
      set_std_c99 (true /* ISO */);
      break;

    case OPT_std_gnu99:
    case OPT_std_gnu9x:
      set_std_c99 (false /* ISO */);
      break;

    case OPT_trigraphs:
      cpp_opts->trigraphs = 1;
      break;

    case OPT_traditional_cpp:
      cpp_opts->traditional = 1;
      break;

    case OPT_undef:
      flag_undef = 1;
      break;

    case OPT_w:
      cpp_opts->inhibit_warnings = 1;
      break;

    case OPT_v:
      cpp_opts->verbose = 1;
      break;
    }

 done:
  if (dup)
    free (dup);
  return result;
}

/* Post-switch processing.  */
bool
c_common_post_options ()
{
  /* Canonicalize the input and output filenames.  */
  if (in_fname == NULL || !strcmp (in_fname, "-"))
    in_fname = "";

  if (out_fname == NULL || !strcmp (out_fname, "-"))
    out_fname = "";

  if (cpp_opts->deps.style == DEPS_NONE)
    check_deps_environment_vars ();

  handle_deferred_opts ();

  sanitize_cpp_opts ();

  flag_inline_trees = 1;

  /* Use tree inlining if possible.  Function instrumentation is only
     done in the RTL level, so we disable tree inlining.  */
  if (! flag_instrument_function_entry_exit)
    {
      if (!flag_no_inline)
	flag_no_inline = 1;
      if (flag_inline_functions)
	{
	  flag_inline_trees = 2;
	  flag_inline_functions = 0;
	}
    }

  /* Special format checking options don't work without -Wformat; warn if
     they are used.  */
  if (warn_format_y2k && !warn_format)
    warning ("-Wformat-y2k ignored without -Wformat");
  if (warn_format_extra_args && !warn_format)
    warning ("-Wformat-extra-args ignored without -Wformat");
  if (warn_format_zero_length && !warn_format)
    warning ("-Wformat-zero-length ignored without -Wformat");
  if (warn_format_nonliteral && !warn_format)
    warning ("-Wformat-nonliteral ignored without -Wformat");
  if (warn_format_security && !warn_format)
    warning ("-Wformat-security ignored without -Wformat");
  if (warn_missing_format_attribute && !warn_format)
    warning ("-Wmissing-format-attribute ignored without -Wformat");

  /* If an error has occurred in cpplib, note it so we fail
     immediately.  */
  errorcount += cpp_errors (parse_in);

  return flag_preprocess_only;
}

/* Preprocess the input file to out_stream.  */
static void
preprocess_file ()
{
  /* Open the output now.  We must do so even if no_output is on,
     because there may be other output than from the actual
     preprocessing (e.g. from -dM).  */
  if (out_fname[0] == '\0')
    out_stream = stdout;
  else
    out_stream = fopen (out_fname, "w");

  if (out_stream == NULL)
    fatal_io_error ("opening output file %s", out_fname);
  else
    cpp_preprocess_file (parse_in, in_fname, out_stream);
}

/* Front end initialization common to C, ObjC and C++.  */
const char *
c_common_init (filename)
     const char *filename;
{
  /* Set up preprocessor arithmetic.  Must be done after call to
     c_common_nodes_and_builtins for type nodes to be good.  */
  cpp_opts->precision = TYPE_PRECISION (intmax_type_node);
  cpp_opts->char_precision = TYPE_PRECISION (char_type_node);
  cpp_opts->int_precision = TYPE_PRECISION (integer_type_node);
  cpp_opts->wchar_precision = TYPE_PRECISION (wchar_type_node);
  cpp_opts->unsigned_wchar = TREE_UNSIGNED (wchar_type_node);

  /* Register preprocessor built-ins before calls to
     cpp_main_file.  */
  cpp_get_callbacks (parse_in)->register_builtins = cb_register_builtins;

  /* NULL is passed up to toplev.c and we exit quickly.  */
  if (flag_preprocess_only)
    {
      preprocess_file ();
      return NULL;
    }

  /* Do this before initializing pragmas, as then cpplib's hash table
     has been set up.  NOTE: we are using our own file name here, not
     the one supplied.  */
  filename = init_c_lex (in_fname);

  init_pragma ();

  return filename;
}

/* Common finish hook for the C, ObjC and C++ front ends.  */
void
c_common_finish ()
{
  FILE *deps_stream = NULL;

  if (cpp_opts->deps.style != DEPS_NONE)
    {
      /* If -M or -MM was seen without -MF, default output to the
	 output stream.  */
      if (!deps_file)
	deps_stream = out_stream;
      else
	{
	  deps_stream = fopen (deps_file, deps_append ? "a": "w");
	  if (!deps_stream)
	    fatal_io_error ("opening dependency file %s", deps_file);
	}
    }

  /* For performance, avoid tearing down cpplib's internal structures
     with cpp_destroy ().  */
  errorcount += cpp_finish (parse_in, deps_stream);

  if (deps_stream && deps_stream != out_stream
      && (ferror (deps_stream) || fclose (deps_stream)))
    fatal_io_error ("closing dependency file %s", deps_file);

  if (out_stream && (ferror (out_stream) || fclose (out_stream)))
    fatal_io_error ("when writing output to %s", out_fname);
}

/* Either of two environment variables can specify output of
   dependencies.  Their value is either "OUTPUT_FILE" or "OUTPUT_FILE
   DEPS_TARGET", where OUTPUT_FILE is the file to write deps info to
   and DEPS_TARGET is the target to mention in the deps.  They also
   result in dependency information being appended to the output file
   rather than overwriting it, and like Sun's compiler
   SUNPRO_DEPENDENCIES suppresses the dependency on the main file.  */
static void
check_deps_environment_vars ()
{
  char *spec;

  GET_ENVIRONMENT (spec, "DEPENDENCIES_OUTPUT");
  if (spec)
    cpp_opts->deps.style = DEPS_USER;
  else
    {
      GET_ENVIRONMENT (spec, "SUNPRO_DEPENDENCIES");
      if (spec)
	{
	  cpp_opts->deps.style = DEPS_SYSTEM;
	  cpp_opts->deps.ignore_main_file = true;
	}
    }

  if (spec)
    {
      /* Find the space before the DEPS_TARGET, if there is one.  */
      char *s = strchr (spec, ' ');
      if (s)
	{
	  /* Let the caller perform MAKE quoting.  */
	  defer_opt (OPT_MT, s + 1);
	  *s = '\0';
	}

      /* Command line -MF overrides environment variables and default.  */
      if (!deps_file)
	deps_file = spec;

      deps_append = 1;
    }
}

/* Handle deferred command line switches.  */
static void
handle_deferred_opts ()
{
  size_t i;

  for (i = 0; i < deferred_count; i++)
    {
      struct deferred_opt *opt = &deferred_opts[i];

      switch (opt->code)
	{
	case OPT_MT:
	case OPT_MQ:
	  cpp_add_dependency_target (parse_in, opt->arg, opt->code == OPT_MQ);
	  break;

	default:
	  abort ();
	}
    }

  free (deferred_opts);
}

/* These settings are appropriate for GCC, but not necessarily so for
   cpplib as a library.  */
static void
sanitize_cpp_opts ()
{
  /* If we don't know what style of dependencies to output, complain
     if any other dependency switches have been given.  */
  if (deps_seen && cpp_opts->deps.style == DEPS_NONE)
    error ("to generate dependencies you must specify either -M or -MM");

  /* -dM and dependencies suppress normal output; do it here so that
     the last -d[MDN] switch overrides earlier ones.  */
  if (cpp_opts->dump_macros == dump_only)
    cpp_opts->no_output = 1;

  /* Disable -dD, -dN and -dI if normal output is suppressed.  Allow
     -dM since at least glibc relies on -M -dM to work.  */
  if (cpp_opts->no_output)
    {
      if (cpp_opts->dump_macros != dump_only)
	cpp_opts->dump_macros = dump_none;
      cpp_opts->dump_includes = 0;
    }

  cpp_opts->unsigned_char = !flag_signed_char;
  cpp_opts->stdc_0_in_system_headers = STDC_0_IN_SYSTEM_HEADERS;

  /* We want -Wno-long-long to override -pedantic -std=non-c99
     and/or -Wtraditional, whatever the ordering.  */
  cpp_opts->warn_long_long
    = warn_long_long && ((!flag_isoc99 && pedantic) || warn_traditional);
}

/* Set the C 89 standard (with 1994 amendments if C94, without GNU
   extensions if ISO).  There is no concept of gnu94.  */
static void
set_std_c89 (c94, iso)
     int c94, iso;
{
  cpp_set_lang (parse_in, c94 ? CLK_STDC94: iso ? CLK_STDC89: CLK_GNUC89);
  flag_iso = iso;
  flag_no_asm = iso;
  flag_no_gnu_keywords = iso;
  flag_no_nonansi_builtin = iso;
  flag_noniso_default_format_attributes = !iso;
  flag_isoc94 = c94;
  flag_isoc99 = 0;
  flag_writable_strings = 0;
}

/* Set the C 99 standard (without GNU extensions if ISO).  */
static void
set_std_c99 (iso)
     int iso;
{
  cpp_set_lang (parse_in, iso ? CLK_STDC99: CLK_GNUC99);
  flag_no_asm = iso;
  flag_no_nonansi_builtin = iso;
  flag_noniso_default_format_attributes = !iso;
  flag_iso = iso;
  flag_isoc99 = 1;
  flag_isoc94 = 1;
  flag_writable_strings = 0;
}

/* Set the C++ 98 standard (without GNU extensions if ISO).  */
static void
set_std_cxx98 (iso)
     int iso;
{
  cpp_set_lang (parse_in, iso ? CLK_CXX98: CLK_GNUCXX);
  flag_no_gnu_keywords = iso;
  flag_no_nonansi_builtin = iso;
  flag_noniso_default_format_attributes = !iso;
  flag_iso = iso;
}

/* Handle setting implicit to ON.  */
static void
set_Wimplicit (on)
     int on;
{
  warn_implicit = on;
  warn_implicit_int = on;
  if (on)
    {
      if (mesg_implicit_function_declaration != 2)
	mesg_implicit_function_declaration = 1;
    }
  else
    mesg_implicit_function_declaration = 0;
}

/* Args to -d specify what to dump.  Silently ignore
   unrecognized options; they may be aimed at toplev.c.  */
static void
handle_OPT_d (arg)
     const char *arg;
{
  char c;

  while ((c = *arg++) != '\0')
    switch (c)
      {
      case 'M':
	cpp_opts->dump_macros = dump_only;
	break;

      case 'N':
	cpp_opts->dump_macros = dump_names;
	break;

      case 'D':
	cpp_opts->dump_macros = dump_definitions;
	break;

      case 'I':
	cpp_opts->dump_includes = 1;
	break;
      }
}

/* Write a slash-separated list of languages in FLAGS to BUF.  */
static void
write_langs (buf, flags)
     char *buf;
     int flags;
{
  *buf = '\0';
  if (flags & CL_C_ONLY)
    strcat (buf, "C");
  if (flags & CL_OBJC_ONLY)
    {
      if (*buf)
	strcat (buf, "/");
      strcat (buf, "ObjC");
    }
  if (flags & CL_CXX_ONLY)
    {
      if (*buf)
	strcat (buf, "/");
      strcat (buf, "C++");
    }
}

/* Complain that switch OPT_INDEX does not apply to this front end.  */
static void
complain_wrong_lang (opt_index, on)
     size_t opt_index;
     int on;
{
  char ok_langs[60], bad_langs[60];
  int ok_flags = cl_options[opt_index].flags;

  write_langs (ok_langs, ok_flags);
  write_langs (bad_langs, ~ok_flags);
  warning ("\"-%c%s%s\" is valid for %s but not for %s",
	   cl_options[opt_index].opt_text[0], on ? "" : "no-",
	   cl_options[opt_index].opt_text + 1, ok_langs, bad_langs);
}

/* Handle --help output.  */
static void
print_help ()
{
  /* To keep the lines from getting too long for some compilers, limit
     to about 500 characters (6 lines) per chunk.  */
  fputs (_("\
Switches:\n\
  -include <file>           Include the contents of <file> before other files\n\
  -imacros <file>           Accept definition of macros in <file>\n\
  -iprefix <path>           Specify <path> as a prefix for next two options\n\
  -iwithprefix <dir>        Add <dir> to the end of the system include path\n\
  -iwithprefixbefore <dir>  Add <dir> to the end of the main include path\n\
  -isystem <dir>            Add <dir> to the start of the system include path\n\
"), stdout);
  fputs (_("\
  -idirafter <dir>          Add <dir> to the end of the system include path\n\
  -I <dir>                  Add <dir> to the end of the main include path\n\
  -I-                       Fine-grained include path control; see info docs\n\
  -nostdinc                 Do not search system include directories\n\
                             (dirs specified with -isystem will still be used)\n\
  -nostdinc++               Do not search system include directories for C++\n\
  -o <file>                 Put output into <file>\n\
"), stdout);
  fputs (_("\
  -trigraphs                Support ISO C trigraphs\n\
  -std=<std name>           Specify the conformance standard; one of:\n\
                            gnu89, gnu99, c89, c99, iso9899:1990,\n\
                            iso9899:199409, iso9899:1999, c++98\n\
  -w                        Inhibit warning messages\n\
  -W[no-]trigraphs          Warn if trigraphs are encountered\n\
  -W[no-]comment{s}         Warn if one comment starts inside another\n\
"), stdout);
  fputs (_("\
  -W[no-]traditional        Warn about features not present in traditional C\n\
  -W[no-]undef              Warn if an undefined macro is used by #if\n\
  -W[no-]import             Warn about the use of the #import directive\n\
"), stdout);
  fputs (_("\
  -W[no-]error              Treat all warnings as errors\n\
  -W[no-]system-headers     Do not suppress warnings from system headers\n\
  -W[no-]all                Enable most preprocessor warnings\n\
"), stdout);
  fputs (_("\
  -M                        Generate make dependencies\n\
  -MM                       As -M, but ignore system header files\n\
  -MD                       Generate make dependencies and compile\n\
  -MMD                      As -MD, but ignore system header files\n\
  -MF <file>                Write dependency output to the given file\n\
  -MG                       Treat missing header file as generated files\n\
"), stdout);
  fputs (_("\
  -MP			    Generate phony targets for all headers\n\
  -MQ <target>              Add a MAKE-quoted target\n\
  -MT <target>              Add an unquoted target\n\
"), stdout);
  fputs (_("\
  -D<macro>                 Define a <macro> with string '1' as its value\n\
  -D<macro>=<val>           Define a <macro> with <val> as its value\n\
  -A<question>=<answer>     Assert the <answer> to <question>\n\
  -A-<question>=<answer>    Disable the <answer> to <question>\n\
  -U<macro>                 Undefine <macro> \n\
  -v                        Display the version number\n\
"), stdout);
  fputs (_("\
  -H                        Print the name of header files as they are used\n\
  -C                        Do not discard comments\n\
  -dM                       Display a list of macro definitions active at end\n\
  -dD                       Preserve macro definitions in output\n\
  -dN                       As -dD except that only the names are preserved\n\
  -dI                       Include #include directives in the output\n\
"), stdout);
  fputs (_("\
  -f[no-]preprocessed       Treat the input file as already preprocessed\n\
  -ftabstop=<number>        Distance between tab stops for column reporting\n\
  -P                        Do not generate #line directives\n\
  -remap                    Remap file names when including files\n\
  --help                    Display this information\n\
"), stdout);
}
