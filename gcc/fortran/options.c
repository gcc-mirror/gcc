/* Parse and display command line options.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation,
   Inc.
   Contributed by Andy Vaught

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
#include "coretypes.h"
#include "tree.h"
#include "flags.h"
#include "intl.h"
#include "opts.h"
#include "options.h"
#include "tree-inline.h"

#include "gfortran.h"

gfc_option_t gfc_option;


/* Get ready for options handling.  */

unsigned int
gfc_init_options (unsigned int argc ATTRIBUTE_UNUSED,
		  const char **argv ATTRIBUTE_UNUSED)
{
  gfc_option.source = NULL;
  gfc_option.module_dir = NULL;
  gfc_option.source_form = FORM_UNKNOWN;
  gfc_option.fixed_line_length = 72;
  gfc_option.max_identifier_length = GFC_MAX_SYMBOL_LEN;
  gfc_option.verbose = 0;

  gfc_option.warn_aliasing = 0;
  gfc_option.warn_conversion = 0;
  gfc_option.warn_implicit_interface = 0;
  gfc_option.warn_line_truncation = 0;
  gfc_option.warn_underflow = 1;
  gfc_option.warn_surprising = 0;
  gfc_option.warn_unused_labels = 0;

  gfc_option.flag_default_double = 0;
  gfc_option.flag_default_integer = 0;
  gfc_option.flag_default_real = 0;
  gfc_option.flag_dollar_ok = 0;
  gfc_option.flag_underscoring = 1;
  gfc_option.flag_f2c = 0;
  gfc_option.flag_second_underscore = -1;
  gfc_option.flag_implicit_none = 0;
  gfc_option.flag_max_stack_var_size = 32768;
  gfc_option.flag_module_access_private = 0;
  gfc_option.flag_no_backend = 0;
  gfc_option.flag_pack_derived = 0;
  gfc_option.flag_repack_arrays = 0;

  gfc_option.q_kind = gfc_default_double_kind;

  flag_argument_noalias = 2;
  flag_errno_math = 0;

  gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F95_DEL
    | GFC_STD_F2003 | GFC_STD_F95 | GFC_STD_F77 | GFC_STD_GNU
    | GFC_STD_LEGACY;
  gfc_option.warn_std = GFC_STD_F95_OBS | GFC_STD_F95_DEL
    | GFC_STD_F2003 | GFC_STD_LEGACY;

  gfc_option.warn_nonstd_intrinsics = 0;

  return CL_F95;
}


/* Finalize commandline options.  */

bool
gfc_post_options (const char **pfilename)
{
  const char *filename = *pfilename;

  /* Verify the input file name.  */
  if (!filename || strcmp (filename, "-") == 0)
    {
      filename = "";
    }

  gfc_option.source = filename;

  flag_inline_trees = 1;

  /* Use tree inlining.  */
  if (!flag_no_inline)
    flag_no_inline = 1;
  if (flag_inline_functions)
    flag_inline_trees = 2;

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

  return false;
}


/* Set the options for -Wall.  */

static void
set_Wall (void)
{

  gfc_option.warn_aliasing = 1;
  gfc_option.warn_line_truncation = 1;
  gfc_option.warn_underflow = 1;
  gfc_option.warn_surprising = 1;
  gfc_option.warn_unused_labels = 1;
  gfc_option.warn_nonstd_intrinsics = 1;

  set_Wunused (1);
  warn_return_type = 1;
  warn_switch = 1;

  /* We save the value of warn_uninitialized, since if they put
     -Wuninitialized on the command line, we need to generate a
     warning about not using it without also specifying -O.  */

  if (warn_uninitialized != 1)
    warn_uninitialized = 2;
}


static void
gfc_handle_module_path_options (const char *arg)
{

  if (gfc_option.module_dir != NULL)
    {
      gfc_status ("gfortran: Only one -M option allowed\n");
      exit (3);
    }

  if (arg == NULL)
    {
      gfc_status ("gfortran: Directory required after -M\n");
      exit (3);
    }

  gfc_option.module_dir = (char *) gfc_getmem (strlen (arg) + 2);
  strcpy (gfc_option.module_dir, arg);
  strcat (gfc_option.module_dir, "/");
}

/* Handle command-line options.  Returns 0 if unrecognized, 1 if
   recognized and handled.  */
int
gfc_handle_option (size_t scode, const char *arg, int value)
{
  int result = 1;
  enum opt_code code = (enum opt_code) scode;

  /* Ignore file names.  */
  if (code == N_OPTS)
    return 1;

  switch (code)
    {
    default:
      result = 0;
      break;

    case OPT_Wall:
      set_Wall ();
      break;

    case OPT_Waliasing:
      gfc_option.warn_aliasing = value;
      break;

    case OPT_Wconversion:
      gfc_option.warn_conversion = value;
      break;

    case OPT_Wimplicit_interface:
      gfc_option.warn_implicit_interface = value;
      break;

    case OPT_Wline_truncation:
      gfc_option.warn_line_truncation = value;
      break;

    case OPT_Wunderflow:
      gfc_option.warn_underflow = value;
      break;

    case OPT_Wsurprising:
      gfc_option.warn_surprising = value;
      break;

    case OPT_Wunused_labels:
      gfc_option.warn_unused_labels = value;
      break;

    case OPT_ff2c:
      gfc_option.flag_f2c = value;
      break;

    case OPT_fdollar_ok:
      gfc_option.flag_dollar_ok = value;
      break;

    case OPT_fdump_parse_tree:
      gfc_option.verbose = value;
      break;

    case OPT_ffixed_form:
      gfc_option.source_form = FORM_FIXED;
      break;

    case OPT_ffree_form:
      gfc_option.source_form = FORM_FREE;
      break;

    case OPT_funderscoring:
      gfc_option.flag_underscoring = value;
      break;

    case OPT_fsecond_underscore:
      gfc_option.flag_second_underscore = value;
      break;

    case OPT_fimplicit_none:
      gfc_option.flag_implicit_none = value;
      break;

    case OPT_fmax_stack_var_size_:
      gfc_option.flag_max_stack_var_size = value;
      break;

    case OPT_fmodule_private:
      gfc_option.flag_module_access_private = value;
      break;

    case OPT_fno_backend:
      gfc_option.flag_no_backend = value;
      break;

    case OPT_fpack_derived:
      gfc_option.flag_pack_derived = value;
      break;

    case OPT_frepack_arrays:
      gfc_option.flag_repack_arrays = value;
      break;

    case OPT_ffixed_line_length_none:
      gfc_option.fixed_line_length = 0;
      break;

    case OPT_ffixed_line_length_:
      if (value != 0 && value < 7)
	gfc_fatal_error ("Fixed line length must be at least seven.");
      gfc_option.fixed_line_length = value;
      break;

    case OPT_fmax_identifier_length_:
      if (value > GFC_MAX_SYMBOL_LEN)
	gfc_fatal_error ("Maximum supported idenitifier length is %d",
			 GFC_MAX_SYMBOL_LEN);
      gfc_option.max_identifier_length = value;
      break;

    case OPT_qkind_:
      if (gfc_validate_kind (BT_REAL, value, true) < 0)
	gfc_fatal_error ("Argument to -fqkind isn't a valid real kind");
      gfc_option.q_kind = value;
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

    case OPT_I:
      gfc_add_include_path (arg);
      break;

    case OPT_J:
    case OPT_M:
      gfc_handle_module_path_options (arg);
      break;
    
    case OPT_std_f95:
      gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F95 | GFC_STD_F77;
      gfc_option.warn_std = GFC_STD_F95_OBS;
      gfc_option.max_identifier_length = 31;
      break;

    case OPT_std_f2003:
      gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F77 
	| GFC_STD_F2003 | GFC_STD_F95;
      gfc_option.warn_std = GFC_STD_F95_OBS;
      gfc_option.max_identifier_length = 63;
      break;

    case OPT_std_gnu:
      gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F95_DEL
	| GFC_STD_F77 | GFC_STD_F95 | GFC_STD_F2003
	| GFC_STD_GNU | GFC_STD_LEGACY;
      gfc_option.warn_std = GFC_STD_F95_OBS | GFC_STD_F95_DEL
	| GFC_STD_LEGACY;
      break;

    case OPT_std_legacy:
      gfc_option.allow_std = GFC_STD_F95_OBS | GFC_STD_F95_DEL
	| GFC_STD_F77 | GFC_STD_F95 | GFC_STD_F2003
	| GFC_STD_GNU | GFC_STD_LEGACY;
      gfc_option.warn_std = 0;
      break;

    case OPT_Wnonstd_intrinsics:
      gfc_option.warn_nonstd_intrinsics = 1;
      break;
    }

  return result;
}
