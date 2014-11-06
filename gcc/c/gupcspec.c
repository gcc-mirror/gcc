/* gupcspec.c: the UPC compiler driver program
   Copyright (C) 2001-2014 Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "gcc.h"
#include "opts.h"

#include "tm.h"


/* GUPC driver - derived from fortran/gfortranspec.c. */

/* The original argument list and related info is copied here.  */
static unsigned int gupc_xargc;
static const struct cl_decoded_option *gupc_x_decoded_options;
static void append_arg (const struct cl_decoded_option *);

/* The new argument list will be built here.  */
static unsigned int gupc_newargc;
static struct cl_decoded_option *gupc_new_decoded_options;

static const char *const standard_bindir_prefix = STANDARD_BINDIR_PREFIX;
static const char *const standard_exec_prefix = STANDARD_EXEC_PREFIX;

/* By default the linker is always invoked.  */
static int invoke_linker = 1;

static int match_suffix (const char *s, const char *suffix);

#define END_ARGS ((char *) 0)

/* Return true if the strings S1 and S2 are either both NULL
 * or both the same string.  */

static bool
strings_same (const char *s1, const char *s2)
{
  return s1 == s2 || (s1 != NULL && s2 != NULL && strcmp (s1, s2) == 0);
}

/* Return whether decoded option structures OPT1 and OPT2 are the
   same.  */

static bool
options_same (const struct cl_decoded_option *opt1,
	      const struct cl_decoded_option *opt2)
{
  return (opt1->opt_index == opt2->opt_index
	  && strings_same (opt1->arg, opt2->arg)
	  && strings_same (opt1->orig_option_with_args_text,
			   opt2->orig_option_with_args_text)
	  && strings_same (opt1->canonical_option[0],
			   opt2->canonical_option[0])
	  && strings_same (opt1->canonical_option[1],
			   opt2->canonical_option[1])
	  && strings_same (opt1->canonical_option[2],
			   opt2->canonical_option[2])
	  && strings_same (opt1->canonical_option[3],
			   opt2->canonical_option[3])
	  && (opt1->canonical_option_num_elements
	      == opt2->canonical_option_num_elements)
	  && opt1->value == opt2->value && opt1->errors == opt2->errors);
}

/* Append another argument to the list being built.  As long as it is
   identical to the corresponding arg in the original list, just increment
   the new arg count.  Otherwise allocate a new list, etc.  */

static void
append_arg (const struct cl_decoded_option *arg)
{
  static unsigned int newargsize;

  if (gupc_new_decoded_options == gupc_x_decoded_options
      && gupc_newargc < gupc_xargc
      && options_same (arg, &gupc_x_decoded_options[gupc_newargc]))
    {
      ++gupc_newargc;
      return;			/* Nothing new here.  */
    }

  if (gupc_new_decoded_options == gupc_x_decoded_options)
    {				/* Make new arglist.  */
      unsigned int i;

      newargsize = (gupc_xargc << 2) + 20;	/* This should handle all.  */
      gupc_new_decoded_options =
	XNEWVEC (struct cl_decoded_option, newargsize);

      /* Copy what has been done so far.  */
      for (i = 0; i < gupc_newargc; ++i)
	gupc_new_decoded_options[i] = gupc_x_decoded_options[i];
    }

  if (gupc_newargc == newargsize)
    fatal_error ("overflowed output arg list for %qs",
		 arg->orig_option_with_args_text);

  gupc_new_decoded_options[gupc_newargc++] = *arg;
}

/* Append an option described by OPT_INDEX, ARG and VALUE to the list
   being built.  */
static void
append_option (size_t opt_index, const char *arg, int value)
{
  struct cl_decoded_option decoded;

  generate_option (opt_index, arg, value, CL_DRIVER, &decoded);
  append_arg (&decoded);
}

static int
match_suffix (const char *s, const char *suffix)
{
  int slen = strlen (s);
  int xlen = strlen (suffix);
  const char *start = (xlen <= slen) ? s + slen - xlen : 0;
  return start && !strncmp (start, suffix, xlen);
}

void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
		      unsigned int *in_decoded_options_count,
		      int *in_added_libraries ATTRIBUTE_UNUSED)
{
  struct cl_decoded_option *decoded_options = *in_decoded_options;
  unsigned int i;
  int is_x_in_effect = 0;
  int is_x_upc_in_effect = 0;
  int verbose = 0;
  int n_infiles = 0;
  int n_outfiles = 0;
  int pre_processed = 0;

  gupc_xargc = *in_decoded_options_count;
  gupc_x_decoded_options = decoded_options;
  gupc_newargc = 0;
  gupc_new_decoded_options = decoded_options;

  /* First pass through the argument list. */

  /* Check to see if any switches are asserted that inhibit linking
     and record the presence of other switches that may require
     special handling. */
  for (i = 1; i < gupc_xargc; ++i)
    {
      if (decoded_options[i].errors & CL_ERR_MISSING_ARG)
	continue;

      switch (decoded_options[i].opt_index)
	{

	case OPT_SPECIAL_input_file:
	  ++n_infiles;
	  continue;

	case OPT_nostdlib:
	case OPT_nodefaultlibs:
	case OPT_c:
	case OPT_S:
	case OPT_fsyntax_only:
	case OPT_E:
	  /* These options disable linking entirely or linking of the
	     standard libraries.  */
	  invoke_linker = 0;
	  break;

	case OPT_fpreprocessed:
	  pre_processed = 1;
	  break;

	case OPT_l:
	  ++n_infiles;
	  break;

	case OPT_o:
	  ++n_outfiles;
	  break;

	case OPT_v:
	  verbose = 1;
	  break;

	case OPT__version:
	  /* Optional GUPC version string. Let GCC handle it for now. */
	  break;

	case OPT__help:
	  /* Let gcc.c handle this, as it has a really
	     cool facility for handling --help and --verbose --help.  */
	  return;

	default:
	  break;
	}
    }

  /* Create the new argument list. */

  /* Start with the compiler itself. */
  append_arg (&decoded_options[0]);

  /* Always assert -fupc.  */
  append_option (OPT_fupc, NULL, 1);

  /* If there are no input files, no need for the library.  */
  if (n_infiles == 0)
    invoke_linker = 0;

  /* Copy in the arguments as passed to 'upc' */
  is_x_in_effect = 0;
  is_x_upc_in_effect = 0;
  for (i = 1; i < gupc_xargc; ++i)
    {
      /* Skip -fupc, we asserted it above.  */
      if (decoded_options[i].opt_index == OPT_fupc)
        continue;

      /* Check for "-x [c,upc,..]". */
      if (decoded_options[i].opt_index == OPT_x)
	{
	  /* Go to default if "none" found. */
	  if (!strcmp (decoded_options[i].arg, "none"))
	    {
	      is_x_in_effect = 0;
	      is_x_upc_in_effect = 0;
	    }
	  else
	    is_x_in_effect = 1;
	}

      /* By default, driver accepts C files as UPC files. Unless there
         is "-x" option in affect. */
      if (decoded_options[i].opt_index == OPT_SPECIAL_input_file)
	{
	  const int is_c_file = match_suffix (decoded_options[i].arg, ".c")
	    || match_suffix (decoded_options[i].arg, ".h")
	    || (pre_processed && match_suffix (decoded_options[i].arg, ".i"));
	  if (is_c_file)
	    {
              if ( !(is_x_in_effect || is_x_upc_in_effect))
		{
		  append_option (OPT_x, "upc", 1);
	   	  is_x_upc_in_effect = 1;
		}
	      append_arg (&decoded_options[i]);
	      continue;
	    }
	  else
	    {
	      if (is_x_upc_in_effect)
	        {
	          is_x_upc_in_effect = 0;
		  append_option (OPT_x, "none", 1);
	        }
	    }
	}
      append_arg (&decoded_options[i]);
    }

  if (verbose)
    {
      fprintf (stderr, "Driving:");
      for (i = 0; i < gupc_newargc; i++)
	fprintf (stderr, " %s",
		 gupc_new_decoded_options[i].orig_option_with_args_text);
      fprintf (stderr, "\n");
    }

  *in_decoded_options_count = gupc_newargc;
  *in_decoded_options = gupc_new_decoded_options;
}

/* Called before linking.  Returns 0 on success and -1 on failure.  */
int
lang_specific_pre_link (void)
{
  return 0;
}

/* Number of extra output files that lang_specific_pre_link may generate.  */
int lang_specific_extra_outfiles = 0;	/* Not used for GUPC.  */
