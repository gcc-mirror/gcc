/* Parse and display command line options.
   Copyright (C) 2000-2013 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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
#include "flags.h"
#include "intl.h"
#include "opts.h"
#include "toplev.h"  /* For save_decoded_options.  */
#include "options.h"
#include "params.h"
#include "tree-inline.h"
#include "gfortran.h"
#include "target.h"
#include "cpp.h"
#include "diagnostic.h"	/* For global_dc.  */
#include "tm.h"

gfc_option_t gfc_option;


/* Set flags that control warnings and errors for different
   Fortran standards to their default values.  Keep in sync with
   libgfortran/runtime/compile_options.c (init_compile_options).  */

static void
set_default_std_flags (void)
{
  gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F95_DEL
    | GFC_STD_F2003 | GFC_STD_F2008 | GFC_STD_F95 | GFC_STD_F77
    | GFC_STD_F2008_OBS | GFC_STD_F2008_TS | GFC_STD_GNU | GFC_STD_LEGACY;
  gfc_option.warn_std = GFC_STD_F95_DEL | GFC_STD_LEGACY;
}


/* Return language mask for Fortran options.  */

unsigned int
gfc_option_lang_mask (void)
{
  return CL_Fortran;
}

/* Initialize options structure OPTS.  */

void
gfc_init_options_struct (struct gcc_options *opts)
{
  opts->x_flag_errno_math = 0;
  opts->x_flag_associative_math = -1;
}

/* Get ready for options handling. Keep in sync with
   libgfortran/runtime/compile_options.c (init_compile_options). */

void
gfc_init_options (unsigned int decoded_options_count,
		  struct cl_decoded_option *decoded_options)
{
  gfc_source_file = NULL;
  gfc_option.module_dir = NULL;
  gfc_option.source_form = FORM_UNKNOWN;
  gfc_option.fixed_line_length = 72;
  gfc_option.free_line_length = 132;
  gfc_option.max_continue_fixed = 255;
  gfc_option.max_continue_free = 255;
  gfc_option.max_identifier_length = GFC_MAX_SYMBOL_LEN;
  gfc_option.max_subrecord_length = 0;
  gfc_option.flag_max_array_constructor = 65535;
  gfc_option.convert = GFC_CONVERT_NATIVE;
  gfc_option.record_marker = 0;
  gfc_option.dump_fortran_original = 0;
  gfc_option.dump_fortran_optimized = 0;

  gfc_option.warn_aliasing = 0;
  gfc_option.warn_ampersand = 0;
  gfc_option.warn_character_truncation = 0;
  gfc_option.warn_array_temp = 0;
  gfc_option.warn_c_binding_type = 0;
  gfc_option.gfc_warn_conversion = 0;
  gfc_option.warn_conversion_extra = 0;
  gfc_option.warn_function_elimination = 0;
  gfc_option.warn_implicit_interface = 0;
  gfc_option.warn_line_truncation = 0;
  gfc_option.warn_surprising = 0;
  gfc_option.warn_tabs = 1;
  gfc_option.warn_underflow = 1;
  gfc_option.warn_intrinsic_shadow = 0;
  gfc_option.warn_intrinsics_std = 0;
  gfc_option.warn_align_commons = 1;
  gfc_option.warn_real_q_constant = 0;
  gfc_option.warn_unused_dummy_argument = 0;
  gfc_option.warn_realloc_lhs = 0;
  gfc_option.warn_realloc_lhs_all = 0;
  gfc_option.warn_compare_reals = 0;
  gfc_option.warn_target_lifetime = 0;
  gfc_option.max_errors = 25;

  gfc_option.flag_all_intrinsics = 0;
  gfc_option.flag_default_double = 0;
  gfc_option.flag_default_integer = 0;
  gfc_option.flag_default_real = 0;
  gfc_option.flag_integer4_kind = 0;
  gfc_option.flag_real4_kind = 0;
  gfc_option.flag_real8_kind = 0;
  gfc_option.flag_dollar_ok = 0;
  gfc_option.flag_underscoring = 1;
  gfc_option.flag_whole_file = 1;
  gfc_option.flag_f2c = 0;
  gfc_option.flag_second_underscore = -1;
  gfc_option.flag_implicit_none = 0;

  /* Default value of flag_max_stack_var_size is set in gfc_post_options.  */
  gfc_option.flag_max_stack_var_size = -2;
  gfc_option.flag_stack_arrays = -1;

  gfc_option.flag_range_check = 1;
  gfc_option.flag_pack_derived = 0;
  gfc_option.flag_repack_arrays = 0;
  gfc_option.flag_preprocessed = 0;
  gfc_option.flag_automatic = 1;
  gfc_option.flag_backslash = 0;
  gfc_option.flag_module_private = 0;
  gfc_option.flag_backtrace = 1;
  gfc_option.flag_allow_leading_underscore = 0;
  gfc_option.flag_external_blas = 0;
  gfc_option.blas_matmul_limit = 30;
  gfc_option.flag_cray_pointer = 0;
  gfc_option.flag_d_lines = -1;
  gfc_option.gfc_flag_openmp = 0;
  gfc_option.flag_sign_zero = 1;
  gfc_option.flag_recursive = 0;
  gfc_option.flag_init_integer = GFC_INIT_INTEGER_OFF;
  gfc_option.flag_init_integer_value = 0;
  gfc_option.flag_init_real = GFC_INIT_REAL_OFF;
  gfc_option.flag_init_logical = GFC_INIT_LOGICAL_OFF;
  gfc_option.flag_init_character = GFC_INIT_CHARACTER_OFF;
  gfc_option.flag_init_character_value = (char)0;
  gfc_option.flag_align_commons = 1;
  gfc_option.flag_protect_parens = -1;
  gfc_option.flag_realloc_lhs = -1;
  gfc_option.flag_aggressive_function_elimination = 0;
  gfc_option.flag_frontend_optimize = -1;
  
  gfc_option.fpe = 0;
  gfc_option.rtcheck = 0;
  gfc_option.coarray = GFC_FCOARRAY_NONE;

  set_default_std_flags ();

  /* Initialize cpp-related options.  */
  gfc_cpp_init_options (decoded_options_count, decoded_options);
}


/* Determine the source form from the filename extension.  We assume
   case insensitivity.  */

static gfc_source_form
form_from_filename (const char *filename)
{
  static const struct
  {
    const char *extension;
    gfc_source_form form;
  }
  exttype[] =
  {
    {
    ".f90", FORM_FREE}
    ,
    {
    ".f95", FORM_FREE}
    ,
    {
    ".f03", FORM_FREE}
    ,
    {
    ".f08", FORM_FREE}
    ,
    {
    ".f", FORM_FIXED}
    ,
    {
    ".for", FORM_FIXED}
    ,
    {
    ".ftn", FORM_FIXED}
    ,
    {
    "", FORM_UNKNOWN}
  };		/* sentinel value */

  gfc_source_form f_form;
  const char *fileext;
  int i;

  /* Find end of file name.  Note, filename is either a NULL pointer or
     a NUL terminated string.  */
  i = 0;
  while (filename[i] != '\0')
    i++;

  /* Find last period.  */
  while (i >= 0 && (filename[i] != '.'))
    i--;

  /* Did we see a file extension?  */
  if (i < 0)
    return FORM_UNKNOWN; /* Nope  */

  /* Get file extension and compare it to others.  */
  fileext = &(filename[i]);

  i = -1;
  f_form = FORM_UNKNOWN;
  do
    {
      i++;
      if (strcasecmp (fileext, exttype[i].extension) == 0)
	{
	  f_form = exttype[i].form;
	  break;
	}
    }
  while (exttype[i].form != FORM_UNKNOWN);

  return f_form;
}


/* Finalize commandline options.  */

bool
gfc_post_options (const char **pfilename)
{
  const char *filename = *pfilename, *canon_source_file = NULL;
  char *source_path;
  int i;

  /* Excess precision other than "fast" requires front-end
     support.  */
  if (flag_excess_precision_cmdline == EXCESS_PRECISION_STANDARD
      && TARGET_FLT_EVAL_METHOD_NON_DEFAULT)
    sorry ("-fexcess-precision=standard for Fortran");
  flag_excess_precision_cmdline = EXCESS_PRECISION_FAST;

  /* Whole program needs whole file mode.  */
  if (flag_whole_program)
    gfc_option.flag_whole_file = 1;

  /* Enable whole-file mode if LTO is in effect.  */
  if (flag_lto)
    gfc_option.flag_whole_file = 1;

  /* Fortran allows associative math - but we cannot reassociate if
     we want traps or signed zeros. Cf. also flag_protect_parens.  */
  if (flag_associative_math == -1)
    flag_associative_math = (!flag_trapping_math && !flag_signed_zeros);

  if (gfc_option.flag_protect_parens == -1)
    gfc_option.flag_protect_parens = !optimize_fast;

  if (gfc_option.flag_stack_arrays == -1)
    gfc_option.flag_stack_arrays = optimize_fast;

  /* By default, disable (re)allocation during assignment for -std=f95,
     and enable it for F2003/F2008/GNU/Legacy. */
  if (gfc_option.flag_realloc_lhs == -1)
    {
      if (gfc_option.allow_std & GFC_STD_F2003)
	gfc_option.flag_realloc_lhs = 1;
      else
	gfc_option.flag_realloc_lhs = 0;
    }

  /* -fbounds-check is equivalent to -fcheck=bounds */
  if (flag_bounds_check)
    gfc_option.rtcheck |= GFC_RTCHECK_BOUNDS;

  if (flag_compare_debug)
    gfc_option.dump_fortran_original = 0;

  /* Make -fmax-errors visible to gfortran's diagnostic machinery.  */
  if (global_options_set.x_flag_max_errors)
    gfc_option.max_errors = flag_max_errors;

  /* Verify the input file name.  */
  if (!filename || strcmp (filename, "-") == 0)
    {
      filename = "";
    }

  if (gfc_option.flag_preprocessed)
    {
      /* For preprocessed files, if the first tokens are of the form # NUM.
	 handle the directives so we know the original file name.  */
      gfc_source_file = gfc_read_orig_filename (filename, &canon_source_file);
      if (gfc_source_file == NULL)
	gfc_source_file = filename;
      else
	*pfilename = gfc_source_file;
    }
  else
    gfc_source_file = filename;

  if (canon_source_file == NULL)
    canon_source_file = gfc_source_file;

  /* Adds the path where the source file is to the list of include files.  */

  i = strlen (canon_source_file);
  while (i > 0 && !IS_DIR_SEPARATOR (canon_source_file[i]))
    i--;

  if (i != 0)
    {
      source_path = (char *) alloca (i + 1);
      memcpy (source_path, canon_source_file, i);
      source_path[i] = 0;
      gfc_add_include_path (source_path, true, true, true);
    }
  else
    gfc_add_include_path (".", true, true, true);

  if (canon_source_file != gfc_source_file)
    free (CONST_CAST (char *, canon_source_file));

  /* Decide which form the file will be read in as.  */

  if (gfc_option.source_form != FORM_UNKNOWN)
    gfc_current_form = gfc_option.source_form;
  else
    {
      gfc_current_form = form_from_filename (filename);

      if (gfc_current_form == FORM_UNKNOWN)
	{
	  gfc_current_form = FORM_FREE;
	  gfc_warning_now ("Reading file '%s' as free form", 
			   (filename[0] == '\0') ? "<stdin>" : filename);
	}
    }

  /* If the user specified -fd-lines-as-{code|comments} verify that we're
     in fixed form.  */
  if (gfc_current_form == FORM_FREE)
    {
      if (gfc_option.flag_d_lines == 0)
	gfc_warning_now ("'-fd-lines-as-comments' has no effect "
			 "in free form");
      else if (gfc_option.flag_d_lines == 1)
	gfc_warning_now ("'-fd-lines-as-code' has no effect in free form");
    }

  /* If -pedantic, warn about the use of GNU extensions.  */
  if (pedantic && (gfc_option.allow_std & GFC_STD_GNU) != 0)
    gfc_option.warn_std |= GFC_STD_GNU;
  /* -std=legacy -pedantic is effectively -std=gnu.  */
  if (pedantic && (gfc_option.allow_std & GFC_STD_LEGACY) != 0)
    gfc_option.warn_std |= GFC_STD_F95_OBS | GFC_STD_F95_DEL | GFC_STD_LEGACY;

  /* If the user didn't explicitly specify -f(no)-second-underscore we
     use it if we're trying to be compatible with f2c, and not
     otherwise.  */
  if (gfc_option.flag_second_underscore == -1)
    gfc_option.flag_second_underscore = gfc_option.flag_f2c;

  if (!gfc_option.flag_automatic && gfc_option.flag_max_stack_var_size != -2
      && gfc_option.flag_max_stack_var_size != 0)
    gfc_warning_now ("Flag -fno-automatic overwrites -fmax-stack-var-size=%d",
		     gfc_option.flag_max_stack_var_size);
  else if (!gfc_option.flag_automatic && gfc_option.flag_recursive)
    gfc_warning_now ("Flag -fno-automatic overwrites -frecursive");
  else if (!gfc_option.flag_automatic && gfc_option.gfc_flag_openmp)
    gfc_warning_now ("Flag -fno-automatic overwrites -frecursive implied by "
		     "-fopenmp");
  else if (gfc_option.flag_max_stack_var_size != -2
	   && gfc_option.flag_recursive)
    gfc_warning_now ("Flag -frecursive overwrites -fmax-stack-var-size=%d",
		     gfc_option.flag_max_stack_var_size);
  else if (gfc_option.flag_max_stack_var_size != -2
	   && gfc_option.gfc_flag_openmp)
    gfc_warning_now ("Flag -fmax-stack-var-size=%d overwrites -frecursive "
		     "implied by -fopenmp", 
		     gfc_option.flag_max_stack_var_size);

  /* Implement -frecursive as -fmax-stack-var-size=-1.  */
  if (gfc_option.flag_recursive)
    gfc_option.flag_max_stack_var_size = -1;

  /* Implied -frecursive; implemented as -fmax-stack-var-size=-1.  */
  if (gfc_option.flag_max_stack_var_size == -2 && gfc_option.gfc_flag_openmp
      && gfc_option.flag_automatic)
    {
      gfc_option.flag_recursive = 1;
      gfc_option.flag_max_stack_var_size = -1;
    }

  /* Set default.  */
  if (gfc_option.flag_max_stack_var_size == -2)
    gfc_option.flag_max_stack_var_size = 32768;

  /* Implement -fno-automatic as -fmax-stack-var-size=0.  */
  if (!gfc_option.flag_automatic)
    gfc_option.flag_max_stack_var_size = 0;
  
  if (pedantic)
    { 
      gfc_option.warn_ampersand = 1;
      gfc_option.warn_tabs = 0;
    }

  if (pedantic && gfc_option.flag_whole_file)
    gfc_option.flag_whole_file = 2;

  /* Optimization implies front end optimization, unless the user
     specified it directly.  */

  if (gfc_option.flag_frontend_optimize == -1)
    gfc_option.flag_frontend_optimize = optimize;

  if (gfc_option.warn_realloc_lhs_all)
    gfc_option.warn_realloc_lhs = 1;

  gfc_cpp_post_options ();

/* FIXME: return gfc_cpp_preprocess_only ();

   The return value of this function indicates whether the
   backend needs to be initialized. On -E, we don't need
   the backend. However, if we return 'true' here, an
   ICE occurs. Initializing the backend doesn't hurt much,
   hence, for now we can live with it as is.  */
  return false;
}


/* Set the options for -Wall.  */

static void
set_Wall (int setting)
{
  gfc_option.warn_aliasing = setting;
  gfc_option.warn_ampersand = setting;
  gfc_option.warn_c_binding_type = setting;
  gfc_option.gfc_warn_conversion = setting;
  gfc_option.warn_line_truncation = setting;
  gfc_option.warn_surprising = setting;
  gfc_option.warn_tabs = !setting;
  gfc_option.warn_underflow = setting;
  gfc_option.warn_intrinsic_shadow = setting;
  gfc_option.warn_intrinsics_std = setting;
  gfc_option.warn_character_truncation = setting;
  gfc_option.warn_real_q_constant = setting;
  gfc_option.warn_unused_dummy_argument = setting;
  gfc_option.warn_target_lifetime = setting;

  warn_return_type = setting;
  warn_uninitialized = setting;
  warn_maybe_uninitialized = setting;
}

/* Set the options for -Wextra.  */

static void
set_Wextra (int setting)
{
  gfc_option.warn_compare_reals = setting;
}

static void
gfc_handle_module_path_options (const char *arg)
{

  if (gfc_option.module_dir != NULL)
    gfc_fatal_error ("gfortran: Only one -J option allowed");

  gfc_option.module_dir = XCNEWVEC (char, strlen (arg) + 2);
  strcpy (gfc_option.module_dir, arg);

  gfc_add_include_path (gfc_option.module_dir, true, false, true);

  strcat (gfc_option.module_dir, "/");
}


static void
gfc_handle_fpe_trap_option (const char *arg)
{
  int result, pos = 0, n;
  /* precision is a backwards compatibility alias for inexact.  */
  static const char * const exception[] = { "invalid", "denormal", "zero",
					    "overflow", "underflow",
					    "inexact", "precision", NULL };
  static const int opt_exception[] = { GFC_FPE_INVALID, GFC_FPE_DENORMAL,
				       GFC_FPE_ZERO, GFC_FPE_OVERFLOW,
				       GFC_FPE_UNDERFLOW, GFC_FPE_INEXACT,
				       GFC_FPE_INEXACT,
				       0 };
 
  while (*arg)
    {
      while (*arg == ',')
	arg++;

      while (arg[pos] && arg[pos] != ',')
	pos++;

      result = 0;
      for (n = 0; exception[n] != NULL; n++)
	{
	  if (exception[n] && strncmp (exception[n], arg, pos) == 0)
	    {
	      gfc_option.fpe |= opt_exception[n];
	      arg += pos;
	      pos = 0;
	      result = 1;
	      break;
	    }
	}
      if (!result)
	gfc_fatal_error ("Argument to -ffpe-trap is not valid: %s", arg);
    }
}


static void
gfc_handle_coarray_option (const char *arg)
{
  if (strcmp (arg, "none") == 0)
    gfc_option.coarray = GFC_FCOARRAY_NONE;
  else if (strcmp (arg, "single") == 0)
    gfc_option.coarray = GFC_FCOARRAY_SINGLE;
  else if (strcmp (arg, "lib") == 0)
    gfc_option.coarray = GFC_FCOARRAY_LIB;
  else
    gfc_fatal_error ("Argument to -fcoarray is not valid: %s", arg);
}


static void
gfc_handle_runtime_check_option (const char *arg)
{
  int result, pos = 0, n;
  static const char * const optname[] = { "all", "bounds", "array-temps",
					  "recursion", "do", "pointer",
					  "mem", NULL };
  static const int optmask[] = { GFC_RTCHECK_ALL, GFC_RTCHECK_BOUNDS,
				 GFC_RTCHECK_ARRAY_TEMPS,
				 GFC_RTCHECK_RECURSION, GFC_RTCHECK_DO,
				 GFC_RTCHECK_POINTER, GFC_RTCHECK_MEM,
				 0 };
 
  while (*arg)
    {
      while (*arg == ',')
	arg++;

      while (arg[pos] && arg[pos] != ',')
	pos++;

      result = 0;
      for (n = 0; optname[n] != NULL; n++)
	{
	  if (optname[n] && strncmp (optname[n], arg, pos) == 0)
	    {
	      gfc_option.rtcheck |= optmask[n];
	      arg += pos;
	      pos = 0;
	      result = 1;
	      break;
	    }
	}
      if (!result)
	gfc_fatal_error ("Argument to -fcheck is not valid: %s", arg);
    }
}


/* Handle command-line options.  Returns 0 if unrecognized, 1 if
   recognized and handled.  */

bool
gfc_handle_option (size_t scode, const char *arg, int value,
		   int kind ATTRIBUTE_UNUSED, location_t loc ATTRIBUTE_UNUSED,
		   const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED)
{
  bool result = true;
  enum opt_code code = (enum opt_code) scode;

  if (gfc_cpp_handle_option (scode, arg, value) == 1)
    return true;

  switch (code)
    {
    default:
      result = false;
      break;

    case OPT_Wall:
      handle_generated_option (&global_options, &global_options_set,
			       OPT_Wunused, NULL, value,
			       gfc_option_lang_mask (), kind, loc,
			       handlers, global_dc);
      set_Wall (value);
      break;

    case OPT_Waliasing:
      gfc_option.warn_aliasing = value;
      break;

    case OPT_Wampersand:
      gfc_option.warn_ampersand = value;
      break;

    case OPT_Warray_temporaries:
      gfc_option.warn_array_temp = value;
      break;

    case OPT_Wc_binding_type:
      gfc_option.warn_c_binding_type = value;
      break;

    case OPT_Wcharacter_truncation:
      gfc_option.warn_character_truncation = value;
      break;

    case OPT_Wcompare_reals:
      gfc_option.warn_compare_reals = value;
      break;

    case OPT_Wconversion:
      gfc_option.gfc_warn_conversion = value;
      break;

    case OPT_Wconversion_extra:
      gfc_option.warn_conversion_extra = value;
      break;

    case OPT_Wextra:
      handle_generated_option (&global_options, &global_options_set,
			       OPT_Wunused_parameter, NULL, value,
			       gfc_option_lang_mask (), kind, loc,
			       handlers, global_dc);
      set_Wextra (value);

      break;

    case OPT_Wfunction_elimination:
      gfc_option.warn_function_elimination = value;
      break;

    case OPT_Wimplicit_interface:
      gfc_option.warn_implicit_interface = value;
      break;

    case OPT_Wimplicit_procedure:
      gfc_option.warn_implicit_procedure = value;
      break;

    case OPT_Wline_truncation:
      gfc_option.warn_line_truncation = value;
      break;

    case OPT_Wrealloc_lhs:
      gfc_option.warn_realloc_lhs = value;
      break;

    case OPT_Wrealloc_lhs_all:
      gfc_option.warn_realloc_lhs_all = value;
      break;

    case OPT_Wreturn_type:
      warn_return_type = value;
      break;

    case OPT_Wsurprising:
      gfc_option.warn_surprising = value;
      break;

    case OPT_Wtabs:
      gfc_option.warn_tabs = value;
      break;

    case OPT_Wtarget_lifetime:
      gfc_option.warn_target_lifetime = value;
      break;

    case OPT_Wunderflow:
      gfc_option.warn_underflow = value;
      break;

    case OPT_Wintrinsic_shadow:
      gfc_option.warn_intrinsic_shadow = value;
      break;

    case OPT_Walign_commons:
      gfc_option.warn_align_commons = value;
      break;

    case OPT_Wreal_q_constant:
      gfc_option.warn_real_q_constant = value;
      break;

    case OPT_Wunused_dummy_argument:
      gfc_option.warn_unused_dummy_argument = value;
      break;

    case OPT_fall_intrinsics:
      gfc_option.flag_all_intrinsics = 1;
      break;

    case OPT_fautomatic:
      gfc_option.flag_automatic = value;
      break;

    case OPT_fallow_leading_underscore:
      gfc_option.flag_allow_leading_underscore = value;
      break;
      
    case OPT_fbackslash:
      gfc_option.flag_backslash = value;
      break;
      
    case OPT_fbacktrace:
      gfc_option.flag_backtrace = value;
      break;
      
    case OPT_fcheck_array_temporaries:
      gfc_option.rtcheck |= GFC_RTCHECK_ARRAY_TEMPS;
      break;
      
    case OPT_fcray_pointer:
      gfc_option.flag_cray_pointer = value;
      break;

    case OPT_ff2c:
      gfc_option.flag_f2c = value;
      break;

    case OPT_fdollar_ok:
      gfc_option.flag_dollar_ok = value;
      break;

    case OPT_fexternal_blas:
      gfc_option.flag_external_blas = value;
      break;

    case OPT_fblas_matmul_limit_:
      gfc_option.blas_matmul_limit = value;
      break;

    case OPT_fd_lines_as_code:
      gfc_option.flag_d_lines = 1;
      break;

    case OPT_fd_lines_as_comments:
      gfc_option.flag_d_lines = 0;
      break;

    case OPT_fdump_fortran_original:
    case OPT_fdump_parse_tree:
      gfc_option.dump_fortran_original = value;
      break;

    case OPT_fdump_fortran_optimized:
      gfc_option.dump_fortran_optimized = value;
      break;

    case OPT_ffixed_form:
      gfc_option.source_form = FORM_FIXED;
      break;

    case OPT_ffixed_line_length_none:
      gfc_option.fixed_line_length = 0;
      break;

    case OPT_ffixed_line_length_:
      if (value != 0 && value < 7)
	gfc_fatal_error ("Fixed line length must be at least seven.");
      gfc_option.fixed_line_length = value;
      break;

    case OPT_ffree_form:
      gfc_option.source_form = FORM_FREE;
      break;

    case OPT_fopenmp:
      gfc_option.gfc_flag_openmp = value;
      break;

    case OPT_ffree_line_length_none:
      gfc_option.free_line_length = 0;
      break;

    case OPT_ffree_line_length_:
      if (value != 0 && value < 4)
	gfc_fatal_error ("Free line length must be at least three.");
      gfc_option.free_line_length = value;
      break;

    case OPT_funderscoring:
      gfc_option.flag_underscoring = value;
      break;

    case OPT_fwhole_file:
      gfc_option.flag_whole_file = value;
      break;

    case OPT_fsecond_underscore:
      gfc_option.flag_second_underscore = value;
      break;

    case OPT_static_libgfortran:
#ifndef HAVE_LD_STATIC_DYNAMIC
      gfc_fatal_error ("-static-libgfortran is not supported in this "
		       "configuration");
#endif
      break;

    case OPT_fimplicit_none:
      gfc_option.flag_implicit_none = value;
      break;

    case OPT_fintrinsic_modules_path:
    case OPT_fintrinsic_modules_path_:

      /* This is needed because omp_lib.h is in a directory together
	 with intrinsic modules.  Do no warn because during testing
	 without an installed compiler, we would get lots of bogus
	 warnings for a missing include directory.  */
      gfc_add_include_path (arg, false, false, false);

      gfc_add_intrinsic_modules_path (arg);
      break;

    case OPT_fmax_array_constructor_:
      gfc_option.flag_max_array_constructor = value > 65535 ? value : 65535;
      break;

    case OPT_fmax_stack_var_size_:
      gfc_option.flag_max_stack_var_size = value;
      break;

    case OPT_fstack_arrays:
      gfc_option.flag_stack_arrays = value;
      break;

    case OPT_fmodule_private:
      gfc_option.flag_module_private = value;
      break;
      
    case OPT_frange_check:
      gfc_option.flag_range_check = value;
      break;

    case OPT_fpack_derived:
      gfc_option.flag_pack_derived = value;
      break;

    case OPT_frepack_arrays:
      gfc_option.flag_repack_arrays = value;
      break;

    case OPT_fpreprocessed:
      gfc_option.flag_preprocessed = value;
      break;

    case OPT_fmax_identifier_length_:
      if (value > GFC_MAX_SYMBOL_LEN)
	gfc_fatal_error ("Maximum supported identifier length is %d",
			 GFC_MAX_SYMBOL_LEN);
      gfc_option.max_identifier_length = value;
      break;

    case OPT_fdefault_integer_8:
      gfc_option.flag_default_integer = value;
      break;

    case OPT_fdefault_real_8:
      gfc_option.flag_default_real = value;
      break;

    case OPT_fdefault_double_8:
      gfc_option.flag_default_double = value;
      break;

    case OPT_finteger_4_integer_8:
      gfc_option.flag_integer4_kind = 8;
      break;

    case OPT_freal_4_real_8:
      gfc_option.flag_real4_kind = 8;
      break;

    case OPT_freal_4_real_10:
      gfc_option.flag_real4_kind = 10;
      break;

    case OPT_freal_4_real_16:
      gfc_option.flag_real4_kind = 16;
      break;

    case OPT_freal_8_real_4:
      gfc_option.flag_real8_kind = 4;
      break;

    case OPT_freal_8_real_10:
      gfc_option.flag_real8_kind = 10;
      break;

    case OPT_freal_8_real_16:
      gfc_option.flag_real8_kind = 16;
      break;

    case OPT_finit_local_zero:
      gfc_option.flag_init_integer = GFC_INIT_INTEGER_ON;
      gfc_option.flag_init_integer_value = 0;
      gfc_option.flag_init_real = GFC_INIT_REAL_ZERO;
      gfc_option.flag_init_logical = GFC_INIT_LOGICAL_FALSE;
      gfc_option.flag_init_character = GFC_INIT_CHARACTER_ON;
      gfc_option.flag_init_character_value = (char)0;
      break;

    case OPT_finit_logical_:
      if (!strcasecmp (arg, "false"))
	gfc_option.flag_init_logical = GFC_INIT_LOGICAL_FALSE;
      else if (!strcasecmp (arg, "true"))
	gfc_option.flag_init_logical = GFC_INIT_LOGICAL_TRUE;
      else
	gfc_fatal_error ("Unrecognized option to -finit-logical: %s",
			 arg);
      break;

    case OPT_finit_real_:
      if (!strcasecmp (arg, "zero"))
	gfc_option.flag_init_real = GFC_INIT_REAL_ZERO;
      else if (!strcasecmp (arg, "nan"))
	gfc_option.flag_init_real = GFC_INIT_REAL_NAN;
      else if (!strcasecmp (arg, "snan"))
	gfc_option.flag_init_real = GFC_INIT_REAL_SNAN;
      else if (!strcasecmp (arg, "inf"))
	gfc_option.flag_init_real = GFC_INIT_REAL_INF;
      else if (!strcasecmp (arg, "-inf"))
	gfc_option.flag_init_real = GFC_INIT_REAL_NEG_INF;
      else
	gfc_fatal_error ("Unrecognized option to -finit-real: %s",
			 arg);
      break;

    case OPT_finit_integer_:
      gfc_option.flag_init_integer = GFC_INIT_INTEGER_ON;
      gfc_option.flag_init_integer_value = atoi (arg);
      break;

    case OPT_finit_character_:
      if (value >= 0 && value <= 127)
	{
	  gfc_option.flag_init_character = GFC_INIT_CHARACTER_ON;
	  gfc_option.flag_init_character_value = (char)value;
	}
      else
	gfc_fatal_error ("The value of n in -finit-character=n must be "
			 "between 0 and 127");
      break;

    case OPT_I:
      gfc_add_include_path (arg, true, false, true);
      break;

    case OPT_J:
      gfc_handle_module_path_options (arg);
      break;

    case OPT_fsign_zero:
      gfc_option.flag_sign_zero = value;
      break;

    case OPT_ffpe_trap_:
      gfc_handle_fpe_trap_option (arg);
      break;

    case OPT_std_f95:
      gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F95 | GFC_STD_F77
			     | GFC_STD_F2008_OBS;
      gfc_option.warn_std = GFC_STD_F95_OBS;
      gfc_option.max_continue_fixed = 19;
      gfc_option.max_continue_free = 39;
      gfc_option.max_identifier_length = 31;
      gfc_option.warn_ampersand = 1;
      gfc_option.warn_tabs = 0;
      break;

    case OPT_std_f2003:
      gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F77 
	| GFC_STD_F2003 | GFC_STD_F95 | GFC_STD_F2008_OBS;
      gfc_option.warn_std = GFC_STD_F95_OBS;
      gfc_option.max_identifier_length = 63;
      gfc_option.warn_ampersand = 1;
      gfc_option.warn_tabs = 0;
      break;

    case OPT_std_f2008:
      gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F77 
	| GFC_STD_F2003 | GFC_STD_F95 | GFC_STD_F2008 | GFC_STD_F2008_OBS;
      gfc_option.warn_std = GFC_STD_F95_OBS | GFC_STD_F2008_OBS;
      gfc_option.max_identifier_length = 63;
      gfc_option.warn_ampersand = 1;
      gfc_option.warn_tabs = 0;
      break;

    case OPT_std_f2008ts:
      gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F77 
	| GFC_STD_F2003 | GFC_STD_F95 | GFC_STD_F2008 | GFC_STD_F2008_OBS
	| GFC_STD_F2008_TS;
      gfc_option.warn_std = GFC_STD_F95_OBS | GFC_STD_F2008_OBS;
      gfc_option.max_identifier_length = 63;
      gfc_option.warn_ampersand = 1;
      gfc_option.warn_tabs = 0;
      break;

    case OPT_std_gnu:
      set_default_std_flags ();
      break;

    case OPT_std_legacy:
      set_default_std_flags ();
      gfc_option.warn_std = 0;
      break;

    case OPT_Wintrinsics_std:
      gfc_option.warn_intrinsics_std = value;
      break;

    case OPT_fshort_enums:
      /* Handled in language-independent code.  */
      break;

    case OPT_fconvert_little_endian:
      gfc_option.convert = GFC_CONVERT_LITTLE;
      break;

    case OPT_fconvert_big_endian:
      gfc_option.convert = GFC_CONVERT_BIG;
      break;

    case OPT_fconvert_native:
      gfc_option.convert = GFC_CONVERT_NATIVE;
      break;

    case OPT_fconvert_swap:
      gfc_option.convert = GFC_CONVERT_SWAP;
      break;

    case OPT_frecord_marker_4:
      gfc_option.record_marker = 4;
      break;

    case OPT_frecord_marker_8:
      gfc_option.record_marker = 8;
      break;

    case OPT_fmax_subrecord_length_:
      if (value > MAX_SUBRECORD_LENGTH)
	gfc_fatal_error ("Maximum subrecord length cannot exceed %d",
			 MAX_SUBRECORD_LENGTH);

      gfc_option.max_subrecord_length = value;
      break;

    case OPT_frecursive:
      gfc_option.flag_recursive = value;
      break;

    case OPT_falign_commons:
      gfc_option.flag_align_commons = value;
      break;

    case  OPT_faggressive_function_elimination:
      gfc_option.flag_aggressive_function_elimination = value;
      break;

    case OPT_ffrontend_optimize:
      gfc_option.flag_frontend_optimize = value;
      break;

    case OPT_fprotect_parens:
      gfc_option.flag_protect_parens = value;
      break;

    case OPT_frealloc_lhs:
      gfc_option.flag_realloc_lhs = value;
      break;

    case OPT_fcheck_:
      gfc_handle_runtime_check_option (arg);
      break;

    case OPT_fcoarray_:
      gfc_handle_coarray_option (arg);
      break;
    }

  Fortran_handle_option_auto (&global_options, &global_options_set, 
                              scode, arg, value, 
                              gfc_option_lang_mask (), kind,
                              loc, handlers, global_dc);
  return result;
}


/* Return a string with the options passed to the compiler; used for
   Fortran's compiler_options() intrinsic.  */

char *
gfc_get_option_string (void)
{
  unsigned j;
  size_t len, pos;
  char *result;

  /* Determine required string length.  */

  len = 0;
  for (j = 1; j < save_decoded_options_count; j++)
    {
      switch (save_decoded_options[j].opt_index)
        {
        case OPT_o:
        case OPT_d:
        case OPT_dumpbase:
        case OPT_dumpdir:
        case OPT_auxbase:
        case OPT_quiet:
        case OPT_version:
        case OPT_fintrinsic_modules_path:
        case OPT_fintrinsic_modules_path_:
          /* Ignore these.  */
          break;
	default:
	  /* Ignore file names. */
	  if (save_decoded_options[j].orig_option_with_args_text[0] == '-')
	    len += 1
		 + strlen (save_decoded_options[j].orig_option_with_args_text);
        }
    }

  result = XCNEWVEC (char, len);

  pos = 0; 
  for (j = 1; j < save_decoded_options_count; j++)
    {
      switch (save_decoded_options[j].opt_index)
        {
        case OPT_o:
        case OPT_d:
        case OPT_dumpbase:
        case OPT_dumpdir:
        case OPT_auxbase:
        case OPT_quiet:
        case OPT_version:
        case OPT_fintrinsic_modules_path:
        case OPT_fintrinsic_modules_path_:
          /* Ignore these.  */
	  continue;

        case OPT_cpp_:
	  /* Use "-cpp" rather than "-cpp=<temporary file>".  */
	  len = 4;
	  break;

        default:
	  /* Ignore file names. */
	  if (save_decoded_options[j].orig_option_with_args_text[0] != '-')
	    continue;

	  len = strlen (save_decoded_options[j].orig_option_with_args_text);
        }

      memcpy (&result[pos], save_decoded_options[j].orig_option_with_args_text, len);
      pos += len;
      result[pos++] = ' ';
    }

  result[--pos] = '\0';
  return result;
}
