/* C/ObjC/C++ command line option handling.
   Copyright (C) 2002-2014 Free Software Foundation, Inc.
   Contributed by Neil Booth.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "c-common.h"
#include "c-pragma.h"
#include "flags.h"
#include "toplev.h"
#include "langhooks.h"
#include "diagnostic.h"
#include "intl.h"
#include "cppdefault.h"
#include "incpath.h"
#include "debug.h"		/* For debug_hooks.  */
#include "opts.h"
#include "options.h"
#include "plugin.h"		/* For PLUGIN_INCLUDE_FILE event.  */
#include "mkdeps.h"
#include "c-target.h"
#include "tm.h"			/* For BYTES_BIG_ENDIAN,
				   DOLLARS_IN_IDENTIFIERS,
				   STDC_0_IN_SYSTEM_HEADERS,
				   TARGET_FLT_EVAL_METHOD_NON_DEFAULT and
				   TARGET_OPTF.  */
#include "tm_p.h"		/* For C_COMMON_OVERRIDE_OPTIONS.  */

#ifndef DOLLARS_IN_IDENTIFIERS
# define DOLLARS_IN_IDENTIFIERS true
#endif

#ifndef TARGET_SYSTEM_ROOT
# define TARGET_SYSTEM_ROOT NULL
#endif

#ifndef TARGET_OPTF
#define TARGET_OPTF(ARG)
#endif

/* CPP's options.  */
cpp_options *cpp_opts;

/* Input filename.  */
static const char *this_input_filename;

/* Filename and stream for preprocessed output.  */
static const char *out_fname;
static FILE *out_stream;

/* Append dependencies to deps_file.  */
static bool deps_append;

/* If dependency switches (-MF etc.) have been given.  */
static bool deps_seen;

/* If -v seen.  */
static bool verbose;

/* Dependency output file.  */
static const char *deps_file;

/* The prefix given by -iprefix, if any.  */
static const char *iprefix;

/* The multilib directory given by -imultilib, if any.  */
static const char *imultilib;

/* The system root, if any.  Overridden by -isysroot.  */
static const char *sysroot = TARGET_SYSTEM_ROOT;

/* Zero disables all standard directories for headers.  */
static bool std_inc = true;

/* Zero disables the C++-specific standard directories for headers.  */
static bool std_cxx_inc = true;

/* If the quote chain has been split by -I-.  */
static bool quote_chain_split;

/* Number of deferred options.  */
static size_t deferred_count;

/* Number of deferred options scanned for -include.  */
static size_t include_cursor;

/* Whether any standard preincluded header has been preincluded.  */
static bool done_preinclude;

static void handle_OPT_d (const char *);
static void set_std_cxx98 (int);
static void set_std_cxx11 (int);
static void set_std_cxx1y (int);
static void set_std_c89 (int, int);
static void set_std_c99 (int);
static void set_std_c11 (int);
static void check_deps_environment_vars (void);
static void handle_deferred_opts (void);
static void sanitize_cpp_opts (void);
static void add_prefixed_path (const char *, size_t);
static void push_command_line_include (void);
static void cb_file_change (cpp_reader *, const struct line_map *);
static void cb_dir_change (cpp_reader *, const char *);
static void c_finish_options (void);

#ifndef STDC_0_IN_SYSTEM_HEADERS
#define STDC_0_IN_SYSTEM_HEADERS 0
#endif

/* Holds switches parsed by c_common_handle_option (), but whose
   handling is deferred to c_common_post_options ().  */
static void defer_opt (enum opt_code, const char *);
static struct deferred_opt
{
  enum opt_code code;
  const char *arg;
} *deferred_opts;


extern const unsigned int 
c_family_lang_mask = (CL_C | CL_CXX | CL_ObjC | CL_ObjCXX);

/* Defer option CODE with argument ARG.  */
static void
defer_opt (enum opt_code code, const char *arg)
{
  deferred_opts[deferred_count].code = code;
  deferred_opts[deferred_count].arg = arg;
  deferred_count++;
}

/* Return language mask for option parsing.  */
unsigned int
c_common_option_lang_mask (void)
{
  static const unsigned int lang_flags[] = {CL_C, CL_ObjC, CL_CXX, CL_ObjCXX};

  return lang_flags[c_language];
}

/* Common diagnostics initialization.  */
void
c_common_initialize_diagnostics (diagnostic_context *context)
{
  /* This is conditionalized only because that is the way the front
     ends used to do it.  Maybe this should be unconditional?  */
  if (c_dialect_cxx ())
    {
      /* By default wrap lines at 80 characters.  Is getenv
	 ("COLUMNS") preferable?  */
      diagnostic_line_cutoff (context) = 80;
      /* By default, emit location information once for every
	 diagnostic message.  */
      diagnostic_prefixing_rule (context) = DIAGNOSTICS_SHOW_PREFIX_ONCE;
    }

  context->opt_permissive = OPT_fpermissive;
}

/* Whether options from all C-family languages should be accepted
   quietly.  */
static bool accept_all_c_family_options = false;

/* Return whether to complain about a wrong-language option.  */
bool
c_common_complain_wrong_lang_p (const struct cl_option *option)
{
  if (accept_all_c_family_options
      && (option->flags & c_family_lang_mask))
    return false;

  return true;
}

/* Initialize options structure OPTS.  */
void
c_common_init_options_struct (struct gcc_options *opts)
{
  opts->x_flag_exceptions = c_dialect_cxx ();
  opts->x_warn_pointer_arith = c_dialect_cxx ();
  opts->x_warn_write_strings = c_dialect_cxx ();
  opts->x_flag_warn_unused_result = true;

  /* By default, C99-like requirements for complex multiply and divide.  */
  opts->x_flag_complex_method = 2;
}

/* Common initialization before calling option handlers.  */
void
c_common_init_options (unsigned int decoded_options_count,
		       struct cl_decoded_option *decoded_options)
{
  unsigned int i;
  struct cpp_callbacks *cb;

  parse_in = cpp_create_reader (c_dialect_cxx () ? CLK_GNUCXX: CLK_GNUC89,
				ident_hash, line_table);
  cb = cpp_get_callbacks (parse_in);
  cb->error = c_cpp_error;

  cpp_opts = cpp_get_options (parse_in);
  cpp_opts->dollars_in_ident = DOLLARS_IN_IDENTIFIERS;
  cpp_opts->objc = c_dialect_objc ();

  /* Reset to avoid warnings on internal definitions.  We set it just
     before passing on command-line options to cpplib.  */
  cpp_opts->warn_dollars = 0;

  deferred_opts = XNEWVEC (struct deferred_opt, decoded_options_count);

  if (c_language == clk_c)
    {
      /* If preprocessing assembly language, accept any of the C-family
	 front end options since the driver may pass them through.  */
      for (i = 1; i < decoded_options_count; i++)
	if (decoded_options[i].opt_index == OPT_lang_asm)
	  {
	    accept_all_c_family_options = true;
	    break;
	  }
    }
}

/* Handle switch SCODE with argument ARG.  VALUE is true, unless no-
   form of an -f or -W option was given.  Returns false if the switch was
   invalid, true if valid.  Use HANDLERS in recursive handle_option calls.  */
bool
c_common_handle_option (size_t scode, const char *arg, int value,
			int kind, location_t loc,
			const struct cl_option_handlers *handlers)
{
  const struct cl_option *option = &cl_options[scode];
  enum opt_code code = (enum opt_code) scode;
  bool result = true;

  /* Prevent resetting the language standard to a C dialect when the driver
     has already determined that we're looking at assembler input.  */
  bool preprocessing_asm_p = (cpp_get_options (parse_in)->lang == CLK_ASM);

  switch (code)
    {
    default:
      if (cl_options[code].flags & c_family_lang_mask)
	{
	  if ((option->flags & CL_TARGET)
	      && ! targetcm.handle_c_option (scode, arg, value))
	    result = false;
	  break;
	}
      result = false;
      break;

    case OPT__output_pch_:
      pch_file = arg;
      break;

    case OPT_A:
      defer_opt (code, arg);
      break;

    case OPT_C:
      cpp_opts->discard_comments = 0;
      break;

    case OPT_CC:
      cpp_opts->discard_comments = 0;
      cpp_opts->discard_comments_in_macro_exp = 0;
      break;

    case OPT_D:
      defer_opt (code, arg);
      break;

    case OPT_H:
      cpp_opts->print_include_names = 1;
      break;

    case OPT_F:
      TARGET_OPTF (xstrdup (arg));
      break;

    case OPT_I:
      if (strcmp (arg, "-"))
	add_path (xstrdup (arg), BRACKET, 0, true);
      else
	{
	  if (quote_chain_split)
	    error ("-I- specified twice");
	  quote_chain_split = true;
	  split_quote_chain ();
	  inform (input_location, "obsolete option -I- used, please use -iquote instead");
	}
      break;

    case OPT_M:
    case OPT_MM:
      /* When doing dependencies with -M or -MM, suppress normal
	 preprocessed output, but still do -dM etc. as software
	 depends on this.  Preprocessed output does occur if -MD, -MMD
	 or environment var dependency generation is used.  */
      cpp_opts->deps.style = (code == OPT_M ? DEPS_SYSTEM: DEPS_USER);
      flag_no_output = 1;
      break;

    case OPT_MD:
    case OPT_MMD:
      cpp_opts->deps.style = (code == OPT_MD ? DEPS_SYSTEM: DEPS_USER);
      cpp_opts->deps.need_preprocessor_output = true;
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
      flag_no_line_commands = 1;
      break;

    case OPT_U:
      defer_opt (code, arg);
      break;

    case OPT_Wall:
      /* ??? Don't add new options here. Use LangEnabledBy in c.opt.  */

      cpp_opts->warn_trigraphs = value;
      cpp_opts->warn_comments = value;
      cpp_opts->warn_num_sign_change = value;
      break;

    case OPT_Wbuiltin_macro_redefined:
      cpp_opts->warn_builtin_macro_redefined = value;
      break;

    case OPT_Wcomment:
      cpp_opts->warn_comments = value;
      break;

    case OPT_Wc___compat:
      cpp_opts->warn_cxx_operator_names = value;
      break;

    case OPT_Wdeprecated:
      cpp_opts->cpp_warn_deprecated = value;
      break;

    case OPT_Wendif_labels:
      cpp_opts->warn_endif_labels = value;
      break;

    case OPT_Winvalid_pch:
      cpp_opts->warn_invalid_pch = value;
      break;

    case OPT_Wliteral_suffix:
      cpp_opts->warn_literal_suffix = value;
      break;

    case OPT_Wlong_long:
      cpp_opts->cpp_warn_long_long = value;
      break;

    case OPT_Wmissing_include_dirs:
      cpp_opts->warn_missing_include_dirs = value;
      break;

    case OPT_Wmultichar:
      cpp_opts->warn_multichar = value;
      break;

    case OPT_Wnormalized_:
      if (kind == DK_ERROR)
	{
	  gcc_assert (!arg);
	  inform (input_location, "-Werror=normalized=: set -Wnormalized=nfc");
	  cpp_opts->warn_normalize = normalized_C;
	}
      else
	{
	  if (!value || (arg && strcasecmp (arg, "none") == 0))
	    cpp_opts->warn_normalize = normalized_none;
	  else if (!arg || strcasecmp (arg, "nfkc") == 0)
	    cpp_opts->warn_normalize = normalized_KC;
	  else if (strcasecmp (arg, "id") == 0)
	    cpp_opts->warn_normalize = normalized_identifier_C;
	  else if (strcasecmp (arg, "nfc") == 0)
	    cpp_opts->warn_normalize = normalized_C;
	  else
	    error ("argument %qs to %<-Wnormalized%> not recognized", arg);
	  break;
	}

    case OPT_Wtraditional:
      cpp_opts->cpp_warn_traditional = value;
      break;

    case OPT_Wtrigraphs:
      cpp_opts->warn_trigraphs = value;
      break;

    case OPT_Wundef:
      cpp_opts->warn_undef = value;
      break;

    case OPT_Wunknown_pragmas:
      /* Set to greater than 1, so that even unknown pragmas in
	 system headers will be warned about.  */
      /* ??? There is no way to handle this automatically for now.  */
      warn_unknown_pragmas = value * 2;
      break;

    case OPT_ansi:
      if (!c_dialect_cxx ())
	set_std_c89 (false, true);
      else
	set_std_cxx98 (true);
      break;

    case OPT_d:
      handle_OPT_d (arg);
      break;

    case OPT_fcanonical_system_headers:
      cpp_opts->canonical_system_headers = value;
      break;

    case OPT_fcond_mismatch:
      if (!c_dialect_cxx ())
	{
	  flag_cond_mismatch = value;
	  break;
	}
      warning (0, "switch %qs is no longer supported", option->opt_text);
      break;

    case OPT_fbuiltin_:
      if (value)
	result = false;
      else
	disable_builtin_function (arg);
      break;

    case OPT_fdirectives_only:
      cpp_opts->directives_only = value;
      break;

    case OPT_fdollars_in_identifiers:
      cpp_opts->dollars_in_ident = value;
      break;

    case OPT_ffreestanding:
      value = !value;
      /* Fall through....  */
    case OPT_fhosted:
      flag_hosted = value;
      flag_no_builtin = !value;
      break;

    case OPT_fconstant_string_class_:
      constant_string_class_name = arg;
      break;

    case OPT_fextended_identifiers:
      cpp_opts->extended_identifiers = value;
      break;

    case OPT_foperator_names:
      cpp_opts->operator_names = value;
      break;

    case OPT_fpch_deps:
      cpp_opts->restore_pch_deps = value;
      break;

    case OPT_fpch_preprocess:
      flag_pch_preprocess = value;
      break;

    case OPT_fpermissive:
      flag_permissive = value;
      global_dc->permissive = value;
      break;

    case OPT_fpreprocessed:
      cpp_opts->preprocessed = value;
      break;

    case OPT_fdebug_cpp:
      cpp_opts->debug = 1;
      break;

    case OPT_ftrack_macro_expansion:
      if (value)
	value = 2;
      /* Fall Through.  */

    case OPT_ftrack_macro_expansion_:
      if (arg && *arg != '\0')
	cpp_opts->track_macro_expansion = value;
      else
	cpp_opts->track_macro_expansion = 2;
      break;

    case OPT_frepo:
      flag_use_repository = value;
      if (value)
	flag_implicit_templates = 0;
      break;

    case OPT_ftabstop_:
      /* It is documented that we silently ignore silly values.  */
      if (value >= 1 && value <= 100)
	cpp_opts->tabstop = value;
      break;

    case OPT_fexec_charset_:
      cpp_opts->narrow_charset = arg;
      break;

    case OPT_fwide_exec_charset_:
      cpp_opts->wide_charset = arg;
      break;

    case OPT_finput_charset_:
      cpp_opts->input_charset = arg;
      break;

    case OPT_ftemplate_depth_:
      max_tinst_depth = value;
      break;

    case OPT_fvisibility_inlines_hidden:
      visibility_options.inlines_hidden = value;
      break;

    case OPT_femit_struct_debug_baseonly:
      set_struct_debug_option (&global_options, loc, "base");
      break;

    case OPT_femit_struct_debug_reduced:
      set_struct_debug_option (&global_options, loc,
			       "dir:ord:sys,dir:gen:any,ind:base");
      break;

    case OPT_femit_struct_debug_detailed_:
      set_struct_debug_option (&global_options, loc, arg);
      break;

    case OPT_fext_numeric_literals:
      cpp_opts->ext_numeric_literals = value;
      break;

    case OPT_idirafter:
      add_path (xstrdup (arg), AFTER, 0, true);
      break;

    case OPT_imacros:
    case OPT_include:
      defer_opt (code, arg);
      break;

    case OPT_imultilib:
      imultilib = arg;
      break;

    case OPT_iprefix:
      iprefix = arg;
      break;

    case OPT_iquote:
      add_path (xstrdup (arg), QUOTE, 0, true);
      break;

    case OPT_isysroot:
      sysroot = arg;
      break;

    case OPT_isystem:
      add_path (xstrdup (arg), SYSTEM, 0, true);
      break;

    case OPT_iwithprefix:
      add_prefixed_path (arg, SYSTEM);
      break;

    case OPT_iwithprefixbefore:
      add_prefixed_path (arg, BRACKET);
      break;

    case OPT_lang_asm:
      cpp_set_lang (parse_in, CLK_ASM);
      cpp_opts->dollars_in_ident = false;
      break;

    case OPT_nostdinc:
      std_inc = false;
      break;

    case OPT_nostdinc__:
      std_cxx_inc = false;
      break;

    case OPT_o:
      if (!out_fname)
	out_fname = arg;
      else
	error ("output filename specified twice");
      break;

      /* We need to handle the -Wpedantic switch here, rather than in
	 c_common_post_options, so that a subsequent -Wno-endif-labels
	 is not overridden.  */
    case OPT_Wpedantic:
      cpp_opts->cpp_pedantic = 1;
      cpp_opts->warn_endif_labels = 1;
      break;

    case OPT_print_objc_runtime_info:
      print_struct_values = 1;
      break;

    case OPT_remap:
      cpp_opts->remap = 1;
      break;

    case OPT_std_c__98:
    case OPT_std_gnu__98:
      if (!preprocessing_asm_p)
	set_std_cxx98 (code == OPT_std_c__98 /* ISO */);
      break;

    case OPT_std_c__11:
    case OPT_std_gnu__11:
      if (!preprocessing_asm_p)
	{
	  set_std_cxx11 (code == OPT_std_c__11 /* ISO */);
	  if (code == OPT_std_c__11)
	    cpp_opts->ext_numeric_literals = 0;
	}
      break;

    case OPT_std_c__1y:
    case OPT_std_gnu__1y:
      if (!preprocessing_asm_p)
	{
	  set_std_cxx1y (code == OPT_std_c__1y /* ISO */);
	  if (code == OPT_std_c__1y)
	    cpp_opts->ext_numeric_literals = 0;
	}
      break;

    case OPT_std_c90:
    case OPT_std_iso9899_199409:
      if (!preprocessing_asm_p)
	set_std_c89 (code == OPT_std_iso9899_199409 /* c94 */, true /* ISO */);
      break;

    case OPT_std_gnu90:
      if (!preprocessing_asm_p)
	set_std_c89 (false /* c94 */, false /* ISO */);
      break;

    case OPT_std_c99:
      if (!preprocessing_asm_p)
	set_std_c99 (true /* ISO */);
      break;

    case OPT_std_gnu99:
      if (!preprocessing_asm_p)
	set_std_c99 (false /* ISO */);
      break;

    case OPT_std_c11:
      if (!preprocessing_asm_p)
	set_std_c11 (true /* ISO */);
      break;

    case OPT_std_gnu11:
      if (!preprocessing_asm_p)
	set_std_c11 (false /* ISO */);
      break;

    case OPT_trigraphs:
      cpp_opts->trigraphs = 1;
      break;

    case OPT_traditional_cpp:
      cpp_opts->traditional = 1;
      break;

    case OPT_v:
      verbose = true;
      break;

    case OPT_Wabi:
      warn_psabi = value;
      break;
    }

  switch (c_language)
    {
    case clk_c:
      C_handle_option_auto (&global_options, &global_options_set, 
                            scode, arg, value, 
                            c_family_lang_mask, kind,
                            loc, handlers, global_dc);
      break;

    case clk_objc:
      ObjC_handle_option_auto (&global_options, &global_options_set,
                               scode, arg, value, 
                               c_family_lang_mask, kind,
                               loc, handlers, global_dc);
      break;

    case clk_cxx:
      CXX_handle_option_auto (&global_options, &global_options_set,
                              scode, arg, value,
                              c_family_lang_mask, kind,
                              loc, handlers, global_dc);
      break;

    case clk_objcxx:
      ObjCXX_handle_option_auto (&global_options, &global_options_set,
                                 scode, arg, value,
                                 c_family_lang_mask, kind,
                                 loc, handlers, global_dc);
      break;

    default:
      gcc_unreachable ();
    }
  
  return result;
}

/* Default implementation of TARGET_HANDLE_C_OPTION.  */

bool
default_handle_c_option (size_t code ATTRIBUTE_UNUSED,
			 const char *arg ATTRIBUTE_UNUSED,
			 int value ATTRIBUTE_UNUSED)
{
  return false;
}

/* Post-switch processing.  */
bool
c_common_post_options (const char **pfilename)
{
  struct cpp_callbacks *cb;

  /* Canonicalize the input and output filenames.  */
  if (in_fnames == NULL)
    {
      in_fnames = XNEWVEC (const char *, 1);
      in_fnames[0] = "";
    }
  else if (strcmp (in_fnames[0], "-") == 0)
    in_fnames[0] = "";

  if (out_fname == NULL || !strcmp (out_fname, "-"))
    out_fname = "";

  if (cpp_opts->deps.style == DEPS_NONE)
    check_deps_environment_vars ();

  handle_deferred_opts ();

  sanitize_cpp_opts ();

  register_include_chains (parse_in, sysroot, iprefix, imultilib,
			   std_inc, std_cxx_inc && c_dialect_cxx (), verbose);

#ifdef C_COMMON_OVERRIDE_OPTIONS
  /* Some machines may reject certain combinations of C
     language-specific options.  */
  C_COMMON_OVERRIDE_OPTIONS;
#endif

  /* Excess precision other than "fast" requires front-end
     support.  */
  if (c_dialect_cxx ())
    {
      if (flag_excess_precision_cmdline == EXCESS_PRECISION_STANDARD
	  && TARGET_FLT_EVAL_METHOD_NON_DEFAULT)
	sorry ("-fexcess-precision=standard for C++");
      flag_excess_precision_cmdline = EXCESS_PRECISION_FAST;
    }
  else if (flag_excess_precision_cmdline == EXCESS_PRECISION_DEFAULT)
    flag_excess_precision_cmdline = (flag_iso
				     ? EXCESS_PRECISION_STANDARD
				     : EXCESS_PRECISION_FAST);

  /* ISO C restricts floating-point expression contraction to within
     source-language expressions (-ffp-contract=on, currently an alias
     for -ffp-contract=off).  */
  if (flag_iso
      && !c_dialect_cxx ()
      && (global_options_set.x_flag_fp_contract_mode
	  == (enum fp_contract_mode) 0)
      && flag_unsafe_math_optimizations == 0)
    flag_fp_contract_mode = FP_CONTRACT_OFF;

  /* By default we use C99 inline semantics in GNU99 or C99 mode.  C99
     inline semantics are not supported in GNU89 or C89 mode.  */
  if (flag_gnu89_inline == -1)
    flag_gnu89_inline = !flag_isoc99;
  else if (!flag_gnu89_inline && !flag_isoc99)
    error ("-fno-gnu89-inline is only supported in GNU99 or C99 mode");

  /* Default to ObjC sjlj exception handling if NeXT runtime.  */
  if (flag_objc_sjlj_exceptions < 0)
    flag_objc_sjlj_exceptions = flag_next_runtime;
  if (flag_objc_exceptions && !flag_objc_sjlj_exceptions)
    flag_exceptions = 1;

  /* -Woverlength-strings is off by default, but is enabled by -Wpedantic.
     It is never enabled in C++, as the minimum limit is not normative
     in that standard.  */
  if (c_dialect_cxx ())
    warn_overlength_strings = 0;

  /* Wmain is enabled by default in C++ but not in C.  */
  /* Wmain is disabled by default for -ffreestanding (!flag_hosted),
     even if -Wall or -Wpedantic was given (warn_main will be 2 if set
     by -Wall, 1 if set by -Wmain).  */
  if (warn_main == -1)
    warn_main = (c_dialect_cxx () && flag_hosted) ? 1 : 0;
  else if (warn_main == 2)
    warn_main = flag_hosted ? 1 : 0;

  /* In C, -Wall and -Wc++-compat enable -Wenum-compare; if it has not
     yet been set, it is disabled by default.  In C++, it is enabled
     by default.  */
  if (warn_enum_compare == -1)
    warn_enum_compare = c_dialect_cxx () ? 1 : 0;

  /* -Wpacked-bitfield-compat is on by default for the C languages.  The
     warning is issued in stor-layout.c which is not part of the front-end so
     we need to selectively turn it on here.  */
  if (warn_packed_bitfield_compat == -1)
    warn_packed_bitfield_compat = 1;

  /* Special format checking options don't work without -Wformat; warn if
     they are used.  */
  if (!warn_format)
    {
      warning (OPT_Wformat_y2k,
	       "-Wformat-y2k ignored without -Wformat");
      warning (OPT_Wformat_extra_args,
	       "-Wformat-extra-args ignored without -Wformat");
      warning (OPT_Wformat_zero_length,
	       "-Wformat-zero-length ignored without -Wformat");
      warning (OPT_Wformat_nonliteral,
	       "-Wformat-nonliteral ignored without -Wformat");
      warning (OPT_Wformat_contains_nul,
	       "-Wformat-contains-nul ignored without -Wformat");
      warning (OPT_Wformat_security,
	       "-Wformat-security ignored without -Wformat");
    }

  /* -Wimplicit-function-declaration is enabled by default for C99.  */
  if (warn_implicit_function_declaration == -1)
    warn_implicit_function_declaration = flag_isoc99;

  /* Declone C++ 'structors if -Os.  */
  if (flag_declone_ctor_dtor == -1)
    flag_declone_ctor_dtor = optimize_size;

  if (cxx_dialect >= cxx11)
    {
      /* If we're allowing C++0x constructs, don't warn about C++98
	 identifiers which are keywords in C++0x.  */
      warn_cxx0x_compat = 0;

      if (warn_narrowing == -1)
	warn_narrowing = 1;
    }
  else if (warn_narrowing == -1)
    warn_narrowing = 0;

  if (flag_extern_tls_init)
    {
#if !defined (ASM_OUTPUT_DEF) || !SUPPORTS_WEAK
      /* Lazy TLS initialization for a variable in another TU requires
	 alias and weak reference support. */
      if (flag_extern_tls_init > 0)
	sorry ("external TLS initialization functions not supported "
	       "on this target");
      flag_extern_tls_init = 0;
#else
      flag_extern_tls_init = 1;
#endif
    }

  if (flag_preprocess_only)
    {
      /* Open the output now.  We must do so even if flag_no_output is
	 on, because there may be other output than from the actual
	 preprocessing (e.g. from -dM).  */
      if (out_fname[0] == '\0')
	out_stream = stdout;
      else
	out_stream = fopen (out_fname, "w");

      if (out_stream == NULL)
	{
	  fatal_error ("opening output file %s: %m", out_fname);
	  return false;
	}

      if (num_in_fnames > 1)
	error ("too many filenames given.  Type %s --help for usage",
	       progname);

      init_pp_output (out_stream);
    }
  else
    {
      init_c_lex ();

      /* When writing a PCH file, avoid reading some other PCH file,
	 because the default address space slot then can't be used
	 for the output PCH file.  */
      if (pch_file)
	{
	  c_common_no_more_pch ();
	  /* Only -g0 and -gdwarf* are supported with PCH, for other
	     debug formats we warn here and refuse to load any PCH files.  */
	  if (write_symbols != NO_DEBUG && write_symbols != DWARF2_DEBUG)
	    warning (OPT_Wdeprecated,
		     "the \"%s\" debug format cannot be used with "
		     "pre-compiled headers", debug_type_names[write_symbols]);
	}
      else if (write_symbols != NO_DEBUG && write_symbols != DWARF2_DEBUG)
	c_common_no_more_pch ();

      /* Yuk.  WTF is this?  I do know ObjC relies on it somewhere.  */
      input_location = UNKNOWN_LOCATION;
    }

  cb = cpp_get_callbacks (parse_in);
  cb->file_change = cb_file_change;
  cb->dir_change = cb_dir_change;
  cpp_post_options (parse_in);

  input_location = UNKNOWN_LOCATION;

  *pfilename = this_input_filename
    = cpp_read_main_file (parse_in, in_fnames[0]);
  /* Don't do any compilation or preprocessing if there is no input file.  */
  if (this_input_filename == NULL)
    {
      errorcount++;
      return false;
    }

  if (flag_working_directory
      && flag_preprocess_only && !flag_no_line_commands)
    pp_dir_change (parse_in, get_src_pwd ());

  /* Disable LTO output when outputting a precompiled header.  */
  if (pch_file && flag_lto)
    {
      flag_lto = 0;
      flag_generate_lto = 0;
    }

  return flag_preprocess_only;
}

/* Front end initialization common to C, ObjC and C++.  */
bool
c_common_init (void)
{
  /* Set up preprocessor arithmetic.  Must be done after call to
     c_common_nodes_and_builtins for type nodes to be good.  */
  cpp_opts->precision = TYPE_PRECISION (intmax_type_node);
  cpp_opts->char_precision = TYPE_PRECISION (char_type_node);
  cpp_opts->int_precision = TYPE_PRECISION (integer_type_node);
  cpp_opts->wchar_precision = TYPE_PRECISION (wchar_type_node);
  cpp_opts->unsigned_wchar = TYPE_UNSIGNED (wchar_type_node);
  cpp_opts->bytes_big_endian = BYTES_BIG_ENDIAN;

  /* This can't happen until after wchar_precision and bytes_big_endian
     are known.  */
  cpp_init_iconv (parse_in);

  if (version_flag)
    {
      int i;
      fputs ("Compiler executable checksum: ", stderr);
      for (i = 0; i < 16; i++)
	fprintf (stderr, "%02x", executable_checksum[i]);
      putc ('\n', stderr);
    }

  /* Has to wait until now so that cpplib has its hash table.  */
  init_pragma ();

  if (flag_preprocess_only)
    {
      c_finish_options ();
      preprocess_file (parse_in);
      return false;
    }

  return true;
}

/* Initialize the integrated preprocessor after debug output has been
   initialized; loop over each input file.  */
void
c_common_parse_file (void)
{
  unsigned int i;

  i = 0;
  for (;;)
    {
      c_finish_options ();
      pch_init ();
      push_file_scope ();
      c_parse_file ();
      pop_file_scope ();
      /* And end the main input file, if the debug writer wants it  */
      if (debug_hooks->start_end_main_source_file)
	(*debug_hooks->end_source_file) (0);
      if (++i >= num_in_fnames)
	break;
      cpp_undef_all (parse_in);
      cpp_clear_file_cache (parse_in);
      this_input_filename
	= cpp_read_main_file (parse_in, in_fnames[i]);
      /* If an input file is missing, abandon further compilation.
	 cpplib has issued a diagnostic.  */
      if (!this_input_filename)
	break;
    }
}

/* Common finish hook for the C, ObjC and C++ front ends.  */
void
c_common_finish (void)
{
  FILE *deps_stream = NULL;

  /* Don't write the deps file if there are errors.  */
  if (cpp_opts->deps.style != DEPS_NONE && !seen_error ())
    {
      /* If -M or -MM was seen without -MF, default output to the
	 output stream.  */
      if (!deps_file)
	deps_stream = out_stream;
      else
	{
	  deps_stream = fopen (deps_file, deps_append ? "a": "w");
	  if (!deps_stream)
	    fatal_error ("opening dependency file %s: %m", deps_file);
	}
    }

  /* For performance, avoid tearing down cpplib's internal structures
     with cpp_destroy ().  */
  cpp_finish (parse_in, deps_stream);

  if (deps_stream && deps_stream != out_stream
      && (ferror (deps_stream) || fclose (deps_stream)))
    fatal_error ("closing dependency file %s: %m", deps_file);

  if (out_stream && (ferror (out_stream) || fclose (out_stream)))
    fatal_error ("when writing output to %s: %m", out_fname);
}

/* Either of two environment variables can specify output of
   dependencies.  Their value is either "OUTPUT_FILE" or "OUTPUT_FILE
   DEPS_TARGET", where OUTPUT_FILE is the file to write deps info to
   and DEPS_TARGET is the target to mention in the deps.  They also
   result in dependency information being appended to the output file
   rather than overwriting it, and like Sun's compiler
   SUNPRO_DEPENDENCIES suppresses the dependency on the main file.  */
static void
check_deps_environment_vars (void)
{
  char *spec;

  spec = getenv ("DEPENDENCIES_OUTPUT");
  if (spec)
    cpp_opts->deps.style = DEPS_USER;
  else
    {
      spec = getenv ("SUNPRO_DEPENDENCIES");
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
      deps_seen = true;
    }
}

/* Handle deferred command line switches.  */
static void
handle_deferred_opts (void)
{
  size_t i;
  struct deps *deps;

  /* Avoid allocating the deps buffer if we don't need it.
     (This flag may be true without there having been -MT or -MQ
     options, but we'll still need the deps buffer.)  */
  if (!deps_seen)
    return;

  deps = cpp_get_deps (parse_in);

  for (i = 0; i < deferred_count; i++)
    {
      struct deferred_opt *opt = &deferred_opts[i];

      if (opt->code == OPT_MT || opt->code == OPT_MQ)
	deps_add_target (deps, opt->arg, opt->code == OPT_MQ);
    }
}

/* These settings are appropriate for GCC, but not necessarily so for
   cpplib as a library.  */
static void
sanitize_cpp_opts (void)
{
  /* If we don't know what style of dependencies to output, complain
     if any other dependency switches have been given.  */
  if (deps_seen && cpp_opts->deps.style == DEPS_NONE)
    error ("to generate dependencies you must specify either -M or -MM");

  /* -dM and dependencies suppress normal output; do it here so that
     the last -d[MDN] switch overrides earlier ones.  */
  if (flag_dump_macros == 'M')
    flag_no_output = 1;

  /* By default, -fdirectives-only implies -dD.  This allows subsequent phases
     to perform proper macro expansion.  */
  if (cpp_opts->directives_only && !cpp_opts->preprocessed && !flag_dump_macros)
    flag_dump_macros = 'D';

  /* Disable -dD, -dN and -dI if normal output is suppressed.  Allow
     -dM since at least glibc relies on -M -dM to work.  */
  /* Also, flag_no_output implies flag_no_line_commands, always.  */
  if (flag_no_output)
    {
      if (flag_dump_macros != 'M')
	flag_dump_macros = 0;
      flag_dump_includes = 0;
      flag_no_line_commands = 1;
    }
  else if (cpp_opts->deps.missing_files)
    error ("-MG may only be used with -M or -MM");

  cpp_opts->unsigned_char = !flag_signed_char;
  cpp_opts->stdc_0_in_system_headers = STDC_0_IN_SYSTEM_HEADERS;
  cpp_opts->warn_date_time = cpp_warn_date_time;

  /* Wlong-long is disabled by default. It is enabled by:
      [-Wpedantic | -Wtraditional] -std=[gnu|c]++98 ; or
      [-Wpedantic | -Wtraditional] -std=non-c99 .

      Either -Wlong-long or -Wno-long-long override any other settings.  */
  if (warn_long_long == -1)
    warn_long_long = ((pedantic || warn_traditional)
		      && (c_dialect_cxx () ? cxx_dialect == cxx98 : !flag_isoc99));
  cpp_opts->cpp_warn_long_long = warn_long_long;

  /* Similarly with -Wno-variadic-macros.  No check for c99 here, since
     this also turns off warnings about GCCs extension.  */
  cpp_opts->warn_variadic_macros
    = cpp_warn_variadic_macros && (pedantic || warn_traditional);

  /* If we're generating preprocessor output, emit current directory
     if explicitly requested or if debugging information is enabled.
     ??? Maybe we should only do it for debugging formats that
     actually output the current directory?  */
  if (flag_working_directory == -1)
    flag_working_directory = (debug_info_level != DINFO_LEVEL_NONE);

  if (cpp_opts->directives_only)
    {
      if (cpp_warn_unused_macros)
	error ("-fdirectives-only is incompatible with -Wunused_macros");
      if (cpp_opts->traditional)
	error ("-fdirectives-only is incompatible with -traditional");
    }
}

/* Add include path with a prefix at the front of its name.  */
static void
add_prefixed_path (const char *suffix, size_t chain)
{
  char *path;
  const char *prefix;
  size_t prefix_len, suffix_len;

  suffix_len = strlen (suffix);
  prefix     = iprefix ? iprefix : cpp_GCC_INCLUDE_DIR;
  prefix_len = iprefix ? strlen (iprefix) : cpp_GCC_INCLUDE_DIR_len;

  path = (char *) xmalloc (prefix_len + suffix_len + 1);
  memcpy (path, prefix, prefix_len);
  memcpy (path + prefix_len, suffix, suffix_len);
  path[prefix_len + suffix_len] = '\0';

  add_path (path, chain, 0, false);
}

/* Handle -D, -U, -A, -imacros, and the first -include.  */
static void
c_finish_options (void)
{
  if (!cpp_opts->preprocessed)
    {
      size_t i;

      cb_file_change (parse_in,
		      linemap_add (line_table, LC_RENAME, 0,
				   _("<built-in>"), 0));
      /* Make sure all of the builtins about to be declared have
	 BUILTINS_LOCATION has their source_location.  */
      source_location builtins_loc = BUILTINS_LOCATION;
      cpp_force_token_locations (parse_in, &builtins_loc);

      cpp_init_builtins (parse_in, flag_hosted);
      c_cpp_builtins (parse_in);

      cpp_stop_forcing_token_locations (parse_in);

      /* We're about to send user input to cpplib, so make it warn for
	 things that we previously (when we sent it internal definitions)
	 told it to not warn.

	 C99 permits implementation-defined characters in identifiers.
	 The documented meaning of -std= is to turn off extensions that
	 conflict with the specified standard, and since a strictly
	 conforming program cannot contain a '$', we do not condition
	 their acceptance on the -std= setting.  */
      cpp_opts->warn_dollars = (cpp_opts->cpp_pedantic && !cpp_opts->c99);

      cb_file_change (parse_in,
		      linemap_add (line_table, LC_RENAME, 0,
				   _("<command-line>"), 0));

      for (i = 0; i < deferred_count; i++)
	{
	  struct deferred_opt *opt = &deferred_opts[i];

	  if (opt->code == OPT_D)
	    cpp_define (parse_in, opt->arg);
	  else if (opt->code == OPT_U)
	    cpp_undef (parse_in, opt->arg);
	  else if (opt->code == OPT_A)
	    {
	      if (opt->arg[0] == '-')
		cpp_unassert (parse_in, opt->arg + 1);
	      else
		cpp_assert (parse_in, opt->arg);
	    }
	}

      /* Start the main input file, if the debug writer wants it. */
      if (debug_hooks->start_end_main_source_file
	  && !flag_preprocess_only)
	(*debug_hooks->start_source_file) (0, this_input_filename);

      /* Handle -imacros after -D and -U.  */
      for (i = 0; i < deferred_count; i++)
	{
	  struct deferred_opt *opt = &deferred_opts[i];

	  if (opt->code == OPT_imacros
	      && cpp_push_include (parse_in, opt->arg))
	    {
	      /* Disable push_command_line_include callback for now.  */
	      include_cursor = deferred_count + 1;
	      cpp_scan_nooutput (parse_in);
	    }
	}
    }
  else
    {
      if (cpp_opts->directives_only)
	cpp_init_special_builtins (parse_in);

      /* Start the main input file, if the debug writer wants it. */
      if (debug_hooks->start_end_main_source_file
	  && !flag_preprocess_only)
	(*debug_hooks->start_source_file) (0, this_input_filename);
    }

  include_cursor = 0;
  push_command_line_include ();
}

/* Give CPP the next file given by -include, if any.  */
static void
push_command_line_include (void)
{
  if (!done_preinclude)
    {
      done_preinclude = true;
      if (flag_hosted && std_inc && !cpp_opts->preprocessed)
	{
	  const char *preinc = targetcm.c_preinclude ();
	  if (preinc && cpp_push_default_include (parse_in, preinc))
	    return;
	}
    }

  pch_cpp_save_state ();

  while (include_cursor < deferred_count)
    {
      struct deferred_opt *opt = &deferred_opts[include_cursor++];

      if (!cpp_opts->preprocessed && opt->code == OPT_include
	  && cpp_push_include (parse_in, opt->arg))
	return;
    }

  if (include_cursor == deferred_count)
    {
      include_cursor++;
      /* -Wunused-macros should only warn about macros defined hereafter.  */
      cpp_opts->warn_unused_macros = cpp_warn_unused_macros;
      /* Restore the line map from <command line>.  */
      if (!cpp_opts->preprocessed)
	cpp_change_file (parse_in, LC_RENAME, this_input_filename);

      /* Set this here so the client can change the option if it wishes,
	 and after stacking the main file so we don't trace the main file.  */
      line_table->trace_includes = cpp_opts->print_include_names;
    }
}

/* File change callback.  Has to handle -include files.  */
static void
cb_file_change (cpp_reader * ARG_UNUSED (pfile),
		const struct line_map *new_map)
{
  if (flag_preprocess_only)
    pp_file_change (new_map);
  else
    fe_file_change (new_map);

  if (new_map 
      && (new_map->reason == LC_ENTER || new_map->reason == LC_RENAME))
    {
      /* Signal to plugins that a file is included.  This could happen
	 several times with the same file path, e.g. because of
	 several '#include' or '#line' directives...  */
      invoke_plugin_callbacks 
	(PLUGIN_INCLUDE_FILE,
	 const_cast<char*> (ORDINARY_MAP_FILE_NAME (new_map)));
    }

  if (new_map == 0 || (new_map->reason == LC_LEAVE && MAIN_FILE_P (new_map)))
    {
      pch_cpp_save_state ();
      push_command_line_include ();
    }
}

void
cb_dir_change (cpp_reader * ARG_UNUSED (pfile), const char *dir)
{
  if (!set_src_pwd (dir))
    warning (0, "too late for # directive to set debug directory");
}

/* Set the C 89 standard (with 1994 amendments if C94, without GNU
   extensions if ISO).  There is no concept of gnu94.  */
static void
set_std_c89 (int c94, int iso)
{
  cpp_set_lang (parse_in, c94 ? CLK_STDC94: iso ? CLK_STDC89: CLK_GNUC89);
  flag_iso = iso;
  flag_no_asm = iso;
  flag_no_gnu_keywords = iso;
  flag_no_nonansi_builtin = iso;
  flag_isoc94 = c94;
  flag_isoc99 = 0;
  flag_isoc11 = 0;
}

/* Set the C 99 standard (without GNU extensions if ISO).  */
static void
set_std_c99 (int iso)
{
  cpp_set_lang (parse_in, iso ? CLK_STDC99: CLK_GNUC99);
  flag_no_asm = iso;
  flag_no_nonansi_builtin = iso;
  flag_iso = iso;
  flag_isoc11 = 0;
  flag_isoc99 = 1;
  flag_isoc94 = 1;
}

/* Set the C 11 standard (without GNU extensions if ISO).  */
static void
set_std_c11 (int iso)
{
  cpp_set_lang (parse_in, iso ? CLK_STDC11: CLK_GNUC11);
  flag_no_asm = iso;
  flag_no_nonansi_builtin = iso;
  flag_iso = iso;
  flag_isoc11 = 1;
  flag_isoc99 = 1;
  flag_isoc94 = 1;
}

/* Set the C++ 98 standard (without GNU extensions if ISO).  */
static void
set_std_cxx98 (int iso)
{
  cpp_set_lang (parse_in, iso ? CLK_CXX98: CLK_GNUCXX);
  flag_no_gnu_keywords = iso;
  flag_no_nonansi_builtin = iso;
  flag_iso = iso;
  cxx_dialect = cxx98;
}

/* Set the C++ 2011 standard (without GNU extensions if ISO).  */
static void
set_std_cxx11 (int iso)
{
  cpp_set_lang (parse_in, iso ? CLK_CXX11: CLK_GNUCXX11);
  flag_no_gnu_keywords = iso;
  flag_no_nonansi_builtin = iso;
  flag_iso = iso;
  /* C++11 includes the C99 standard library.  */
  flag_isoc94 = 1;
  flag_isoc99 = 1;
  cxx_dialect = cxx11;
}

/* Set the C++ 201y draft standard (without GNU extensions if ISO).  */
static void
set_std_cxx1y (int iso)
{
  cpp_set_lang (parse_in, iso ? CLK_CXX1Y: CLK_GNUCXX1Y);
  flag_no_gnu_keywords = iso;
  flag_no_nonansi_builtin = iso;
  flag_iso = iso;
  /* C++11 includes the C99 standard library.  */
  flag_isoc94 = 1;
  flag_isoc99 = 1;
  cxx_dialect = cxx1y;
}

/* Args to -d specify what to dump.  Silently ignore
   unrecognized options; they may be aimed at toplev.c.  */
static void
handle_OPT_d (const char *arg)
{
  char c;

  while ((c = *arg++) != '\0')
    switch (c)
      {
      case 'M':			/* Dump macros only.  */
      case 'N':			/* Dump names.  */
      case 'D':			/* Dump definitions.  */
      case 'U':			/* Dump used macros.  */
	flag_dump_macros = c;
	break;

      case 'I':
	flag_dump_includes = 1;
	break;
      }
}
