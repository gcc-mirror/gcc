/* Parse and display command line options.
   Copyright (C) 2000-2017 Free Software Foundation, Inc.
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
#include "target.h"
#include "tree.h"
#include "gfortran.h"
#include "diagnostic.h"	/* For global_dc.  */
#include "opts.h"
#include "toplev.h"  /* For save_decoded_options.  */
#include "cpp.h"
#include "langhooks.h"

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


/* Set all the DEC extension flags.  */

static void
set_dec_flags (int value)
{
  if (value)
    {
      /* Allow legacy code without warnings.  */
      gfc_option.allow_std |= GFC_STD_F95_OBS | GFC_STD_F95_DEL
        | GFC_STD_GNU | GFC_STD_LEGACY;
      gfc_option.warn_std &= ~(GFC_STD_LEGACY | GFC_STD_F95_DEL);
    }

  /* Set other DEC compatibility extensions.  */
  flag_dollar_ok |= value;
  flag_cray_pointer |= value;
  flag_dec_structure |= value;
  flag_dec_intrinsic_ints |= value;
  flag_dec_static |= value;
  flag_dec_math |= value;
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
  opts->frontend_set_flag_errno_math = true;
  opts->x_flag_associative_math = -1;
  opts->frontend_set_flag_associative_math = true;
}

/* Get ready for options handling. Keep in sync with
   libgfortran/runtime/compile_options.c (init_compile_options).  */

void
gfc_init_options (unsigned int decoded_options_count,
		  struct cl_decoded_option *decoded_options)
{
  gfc_source_file = NULL;
  gfc_option.module_dir = NULL;
  gfc_option.source_form = FORM_UNKNOWN;
  gfc_option.max_continue_fixed = 255;
  gfc_option.max_continue_free = 255;
  gfc_option.max_identifier_length = GFC_MAX_SYMBOL_LEN;
  gfc_option.max_errors = 25;

  gfc_option.flag_preprocessed = 0;
  gfc_option.flag_d_lines = -1;
  gfc_option.flag_init_integer = GFC_INIT_INTEGER_OFF;
  gfc_option.flag_init_integer_value = 0;
  gfc_option.flag_init_logical = GFC_INIT_LOGICAL_OFF;
  gfc_option.flag_init_character = GFC_INIT_CHARACTER_OFF;
  gfc_option.flag_init_character_value = (char)0;
  
  gfc_option.fpe = 0;
  /* All except GFC_FPE_INEXACT.  */
  gfc_option.fpe_summary = GFC_FPE_INVALID | GFC_FPE_DENORMAL
			   | GFC_FPE_ZERO | GFC_FPE_OVERFLOW
			   | GFC_FPE_UNDERFLOW;
  gfc_option.rtcheck = 0;

  /* ??? Wmissing-include-dirs is disabled by default in C/C++ but
     enabled by default in Fortran.  Ideally, we should express this
     in .opt, but that is not supported yet.  */
  if (!global_options_set.x_cpp_warn_missing_include_dirs)
    global_options.x_cpp_warn_missing_include_dirs = 1;

  set_dec_flags (0);

  set_default_std_flags ();

  /* Initialize cpp-related options.  */
  gfc_cpp_init_options (decoded_options_count, decoded_options);
  gfc_diagnostics_init ();
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
  if (flag_excess_precision_cmdline == EXCESS_PRECISION_STANDARD)
    sorry ("-fexcess-precision=standard for Fortran");
  flag_excess_precision_cmdline = EXCESS_PRECISION_FAST;

  /* Fortran allows associative math - but we cannot reassociate if
     we want traps or signed zeros. Cf. also flag_protect_parens.  */
  if (flag_associative_math == -1)
    flag_associative_math = (!flag_trapping_math && !flag_signed_zeros);

  if (flag_protect_parens == -1)
    flag_protect_parens = !optimize_fast;

  /* -Ofast sets implies -fstack-arrays unless an explicit size is set for
     stack arrays.  */
  if (flag_stack_arrays == -1 && flag_max_stack_var_size == -2)
    flag_stack_arrays = optimize_fast;

  /* By default, disable (re)allocation during assignment for -std=f95,
     and enable it for F2003/F2008/GNU/Legacy.  */
  if (flag_realloc_lhs == -1)
    {
      if (gfc_option.allow_std & GFC_STD_F2003)
	flag_realloc_lhs = 1;
      else
	flag_realloc_lhs = 0;
    }

  /* -fbounds-check is equivalent to -fcheck=bounds */
  if (flag_bounds_check)
    gfc_option.rtcheck |= GFC_RTCHECK_BOUNDS;

  if (flag_compare_debug)
    flag_dump_fortran_original = 0;

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
	  gfc_warning_now (0, "Reading file %qs as free form", 
			   (filename[0] == '\0') ? "<stdin>" : filename);
	}
    }

  /* If the user specified -fd-lines-as-{code|comments} verify that we're
     in fixed form.  */
  if (gfc_current_form == FORM_FREE)
    {
      if (gfc_option.flag_d_lines == 0)
	gfc_warning_now (0, "%<-fd-lines-as-comments%> has no effect "
			   "in free form");
      else if (gfc_option.flag_d_lines == 1)
	gfc_warning_now (0, "%<-fd-lines-as-code%> has no effect in free form");

      if (warn_line_truncation == -1)
	  warn_line_truncation = 1;

      /* Enable -Werror=line-truncation when -Werror and -Wno-error have
	 not been set.  */
      if (warn_line_truncation && !global_options_set.x_warnings_are_errors
	  && (global_dc->classify_diagnostic[OPT_Wline_truncation] ==
	      DK_UNSPECIFIED))
	diagnostic_classify_diagnostic (global_dc, OPT_Wline_truncation,
					DK_ERROR, UNKNOWN_LOCATION);
    }
  else
    {
      /* With -fdec, set -fd-lines-as-comments by default in fixed form.  */
      if (flag_dec && gfc_option.flag_d_lines == -1)
	gfc_option.flag_d_lines = 0;

      if (warn_line_truncation == -1)
	warn_line_truncation = 0;
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
  if (flag_second_underscore == -1)
    flag_second_underscore = flag_f2c;

  if (!flag_automatic && flag_max_stack_var_size != -2
      && flag_max_stack_var_size != 0)
    gfc_warning_now (0, "Flag %<-fno-automatic%> overwrites %<-fmax-stack-var-size=%d%>",
		     flag_max_stack_var_size);
  else if (!flag_automatic && flag_recursive)
    gfc_warning_now (0, "Flag %<-fno-automatic%> overwrites %<-frecursive%>");
  else if (!flag_automatic && flag_openmp)
    gfc_warning_now (0, "Flag %<-fno-automatic%> overwrites %<-frecursive%> implied by "
		     "%<-fopenmp%>");
  else if (flag_max_stack_var_size != -2 && flag_recursive)
    gfc_warning_now (0, "Flag %<-frecursive%> overwrites %<-fmax-stack-var-size=%d%>",
		     flag_max_stack_var_size);
  else if (flag_max_stack_var_size != -2 && flag_openmp)
    gfc_warning_now (0, "Flag %<-fmax-stack-var-size=%d%> overwrites %<-frecursive%> "
		     "implied by %<-fopenmp%>", flag_max_stack_var_size);

  /* Implement -frecursive as -fmax-stack-var-size=-1.  */
  if (flag_recursive)
    flag_max_stack_var_size = -1;

  /* Implied -frecursive; implemented as -fmax-stack-var-size=-1.  */
  if (flag_max_stack_var_size == -2 && flag_openmp && flag_automatic)
    {
      flag_recursive = 1;
      flag_max_stack_var_size = -1;
    }

  /* Set flag_stack_arrays correctly.  */
  if (flag_stack_arrays == -1)
    flag_stack_arrays = 0;

  /* Set default.  */
  if (flag_max_stack_var_size == -2)
    flag_max_stack_var_size = 32768;

  /* Implement -fno-automatic as -fmax-stack-var-size=0.  */
  if (!flag_automatic)
    flag_max_stack_var_size = 0;
  
  /* If the user did not specify an inline matmul limit, inline up to the BLAS
     limit or up to 30 if no external BLAS is specified.  */

  if (flag_inline_matmul_limit < 0)
    {
      if (flag_external_blas)
	flag_inline_matmul_limit = flag_blas_matmul_limit;
      else
	flag_inline_matmul_limit = 30;
    }

  /* Optimization implies front end optimization, unless the user
     specified it directly.  */

  if (flag_frontend_optimize == -1)
    flag_frontend_optimize = optimize;

  /* Same for front end loop interchange.  */

  if (flag_frontend_loop_interchange == -1)
    flag_frontend_loop_interchange = optimize;

  if (flag_max_array_constructor < 65535)
    flag_max_array_constructor = 65535;

  if (flag_fixed_line_length != 0 && flag_fixed_line_length < 7)
    gfc_fatal_error ("Fixed line length must be at least seven");

  if (flag_free_line_length != 0 && flag_free_line_length < 4)
    gfc_fatal_error ("Free line length must be at least three");

  if (flag_max_subrecord_length > MAX_SUBRECORD_LENGTH)
    gfc_fatal_error ("Maximum subrecord length cannot exceed %d",
		     MAX_SUBRECORD_LENGTH);

  gfc_cpp_post_options ();

  if (gfc_option.allow_std & GFC_STD_F2008)
    lang_hooks.name = "GNU Fortran2008";
  else if (gfc_option.allow_std & GFC_STD_F2003)
    lang_hooks.name = "GNU Fortran2003";

  return gfc_cpp_preprocess_only ();
}


static void
gfc_handle_module_path_options (const char *arg)
{

  if (gfc_option.module_dir != NULL)
    gfc_fatal_error ("gfortran: Only one %<-J%> option allowed");

  gfc_option.module_dir = XCNEWVEC (char, strlen (arg) + 2);
  strcpy (gfc_option.module_dir, arg);

  gfc_add_include_path (gfc_option.module_dir, true, false, true);

  strcat (gfc_option.module_dir, "/");
}


/* Handle options -ffpe-trap= and -ffpe-summary=.  */

static void
gfc_handle_fpe_option (const char *arg, bool trap)
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

  /* As the default for -ffpe-summary= is nonzero, set it to 0.  */
  if (!trap)
    gfc_option.fpe_summary = 0;

  while (*arg)
    {
      while (*arg == ',')
	arg++;

      while (arg[pos] && arg[pos] != ',')
	pos++;

      result = 0;
      if (!trap && strncmp ("none", arg, pos) == 0)
	{
	  gfc_option.fpe_summary = 0;
	  arg += pos;
	  pos = 0;
	  continue;
	}
      else if (!trap && strncmp ("all", arg, pos) == 0)
	{
	  gfc_option.fpe_summary = GFC_FPE_INVALID | GFC_FPE_DENORMAL
				   | GFC_FPE_ZERO | GFC_FPE_OVERFLOW
				   | GFC_FPE_UNDERFLOW | GFC_FPE_INEXACT;
	  arg += pos;
	  pos = 0;
	  continue;
	}
      else
	for (n = 0; exception[n] != NULL; n++)
	  {
	  if (exception[n] && strncmp (exception[n], arg, pos) == 0)
	    {
	      if (trap)
		gfc_option.fpe |= opt_exception[n];
	      else
		gfc_option.fpe_summary |= opt_exception[n];
	      arg += pos;
	      pos = 0;
	      result = 1;
	      break;
	    }
	  }
      if (!result && !trap)
	gfc_fatal_error ("Argument to %<-ffpe-trap%> is not valid: %s", arg);
      else if (!result)
	gfc_fatal_error ("Argument to %<-ffpe-summary%> is not valid: %s", arg);

    }
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
	  else if (optname[n] && pos > 3 && strncmp ("no-", arg, 3) == 0
		   && strncmp (optname[n], arg+3, pos-3) == 0)
	    {
	      gfc_option.rtcheck &= ~optmask[n];
	      arg += pos;
	      pos = 0;
	      result = 1;
	      break;
	    }
	}
      if (!result)
	gfc_fatal_error ("Argument to %<-fcheck%> is not valid: %s", arg);
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
      if (cl_options[code].flags & gfc_option_lang_mask ())
	break;
      result = false;
      break;

    case OPT_fcheck_array_temporaries:
      gfc_option.rtcheck |= GFC_RTCHECK_ARRAY_TEMPS;
      break;
      
    case OPT_fd_lines_as_code:
      gfc_option.flag_d_lines = 1;
      break;

    case OPT_fd_lines_as_comments:
      gfc_option.flag_d_lines = 0;
      break;

    case OPT_ffixed_form:
      gfc_option.source_form = FORM_FIXED;
      break;

    case OPT_ffree_form:
      gfc_option.source_form = FORM_FREE;
      break;

    case OPT_static_libgfortran:
#ifndef HAVE_LD_STATIC_DYNAMIC
      gfc_fatal_error ("%<-static-libgfortran%> is not supported in this "
		       "configuration");
#endif
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

    case OPT_fpreprocessed:
      gfc_option.flag_preprocessed = value;
      break;

    case OPT_fmax_identifier_length_:
      if (value > GFC_MAX_SYMBOL_LEN)
	gfc_fatal_error ("Maximum supported identifier length is %d",
			 GFC_MAX_SYMBOL_LEN);
      gfc_option.max_identifier_length = value;
      break;

    case OPT_finit_local_zero:
      gfc_option.flag_init_integer = GFC_INIT_INTEGER_ON;
      gfc_option.flag_init_integer_value = 0;
      flag_init_real = GFC_INIT_REAL_ZERO;
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
	gfc_fatal_error ("Unrecognized option to %<-finit-logical%>: %s",
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
	gfc_fatal_error ("The value of n in %<-finit-character=n%> must be "
			 "between 0 and 127");
      break;

    case OPT_I:
      gfc_add_include_path (arg, true, false, true);
      break;

    case OPT_J:
      gfc_handle_module_path_options (arg);
      break;

    case OPT_ffpe_trap_:
      gfc_handle_fpe_option (arg, true);
      break;

    case OPT_ffpe_summary_:
      gfc_handle_fpe_option (arg, false);
      break;

    case OPT_std_f95:
      gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F95 | GFC_STD_F77
			     | GFC_STD_F2008_OBS;
      gfc_option.warn_std = GFC_STD_F95_OBS;
      gfc_option.max_continue_fixed = 19;
      gfc_option.max_continue_free = 39;
      gfc_option.max_identifier_length = 31;
      warn_ampersand = 1;
      warn_tabs = 1;
      break;

    case OPT_std_f2003:
      gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F77 
	| GFC_STD_F2003 | GFC_STD_F95 | GFC_STD_F2008_OBS;
      gfc_option.warn_std = GFC_STD_F95_OBS;
      gfc_option.max_identifier_length = 63;
      warn_ampersand = 1;
      warn_tabs = 1;
      break;

    case OPT_std_f2008:
      gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F77 
	| GFC_STD_F2003 | GFC_STD_F95 | GFC_STD_F2008 | GFC_STD_F2008_OBS;
      gfc_option.warn_std = GFC_STD_F95_OBS | GFC_STD_F2008_OBS;
      gfc_option.max_identifier_length = 63;
      warn_ampersand = 1;
      warn_tabs = 1;
      break;

    case OPT_std_f2008ts:
      gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F77 
	| GFC_STD_F2003 | GFC_STD_F95 | GFC_STD_F2008 | GFC_STD_F2008_OBS
	| GFC_STD_F2008_TS;
      gfc_option.warn_std = GFC_STD_F95_OBS | GFC_STD_F2008_OBS;
      gfc_option.max_identifier_length = 63;
      warn_ampersand = 1;
      warn_tabs = 1;
      break;

    case OPT_std_gnu:
      set_default_std_flags ();
      break;

    case OPT_std_legacy:
      set_default_std_flags ();
      gfc_option.warn_std = 0;
      break;

    case OPT_fshort_enums:
      /* Handled in language-independent code.  */
      break;

    case OPT_fcheck_:
      gfc_handle_runtime_check_option (arg);
      break;

    case OPT_fdec:
      /* Enable all DEC extensions.  */
      set_dec_flags (1);
      break;

    case OPT_fdec_structure:
      flag_dec_structure = 1;
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

  /* Allocate and return a one-character string with '\0'.  */
  if (!save_decoded_options_count)
    return XCNEWVEC (char, 1);

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
	  /* Ignore file names.  */
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
	  /* Ignore file names.  */
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
