/* Specific flags and argument handling of the Fortran front-end.
   Copyright (C) 1997-2019 Free Software Foundation, Inc.

This file is part of GCC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This file is copied more or less verbatim from g77.  */
/* This file contains a filter for the main `gcc' driver, which is
   replicated for the `gfortran' driver by adding this filter.  The purpose
   of this filter is to be basically identical to gcc (in that
   it faithfully passes all of the original arguments to gcc) but,
   unless explicitly overridden by the user in certain ways, ensure
   that the needs of the language supported by this wrapper are met.

   For GNU Fortran 95(gfortran), we do the following to the argument list
   before passing it to `gcc':

   1.  Make sure `-lgfortran -lm' is at the end of the list.

   2.  Make sure each time `-lgfortran' or `-lm' is seen, it forms
       part of the series `-lgfortran -lm'.

   #1 and #2 are not done if `-nostdlib' or any option that disables
   the linking phase is present, or if `-xfoo' is in effect.  Note that
   a lack of source files or -l options disables linking.

   This program was originally made out of gcc/cp/g++spec.c, but the
   way it builds the new argument list was rewritten so it is much
   easier to maintain, improve the way it decides to add or not add
   extra arguments, etc.  And several improvements were made in the
   handling of arguments, primarily to make it more consistent with
   `gcc' itself.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "opt-suggestions.h"
#include "gcc.h"
#include "opts.h"

#include "tm.h"
#include "intl.h"

#ifndef MATH_LIBRARY
#define MATH_LIBRARY "m"
#endif

#ifndef FORTRAN_LIBRARY
#define FORTRAN_LIBRARY "gfortran"
#endif

/* Name of the spec file.  */
#define SPEC_FILE "libgfortran.spec"

/* The original argument list and related info is copied here.  */
static unsigned int g77_xargc;
static const struct cl_decoded_option *g77_x_decoded_options;
static void append_arg (const struct cl_decoded_option *);

/* The new argument list will be built here.  */
static unsigned int g77_newargc;
static struct cl_decoded_option *g77_new_decoded_options;

/* This will be NULL if we encounter a situation where we should not
   link in the fortran libraries.  */
static const char *library = NULL;


/* Return whether strings S1 and S2 are both NULL or both the same
   string.  */

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
	  && opt1->value == opt2->value
	  && opt1->errors == opt2->errors);
}

/* Append another argument to the list being built.  As long as it is
   identical to the corresponding arg in the original list, just increment
   the new arg count.  Otherwise allocate a new list, etc.  */

static void
append_arg (const struct cl_decoded_option *arg)
{
  static unsigned int newargsize;

  if (g77_new_decoded_options == g77_x_decoded_options
      && g77_newargc < g77_xargc
      && options_same (arg, &g77_x_decoded_options[g77_newargc]))
    {
      ++g77_newargc;
      return;			/* Nothing new here.  */
    }

  if (g77_new_decoded_options == g77_x_decoded_options)
    {				/* Make new arglist.  */
      unsigned int i;

      newargsize = (g77_xargc << 2) + 20;	/* This should handle all.  */
      g77_new_decoded_options = XNEWVEC (struct cl_decoded_option, newargsize);

      /* Copy what has been done so far.  */
      for (i = 0; i < g77_newargc; ++i)
	g77_new_decoded_options[i] = g77_x_decoded_options[i];
    }

  if (g77_newargc == newargsize)
    fatal_error (input_location, "overflowed output argument list for %qs",
		 arg->orig_option_with_args_text);

  g77_new_decoded_options[g77_newargc++] = *arg;
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

/* Append a libgfortran argument to the list being built.  If
   FORCE_STATIC, ensure the library is linked statically.  */

static void
add_arg_libgfortran (bool force_static ATTRIBUTE_UNUSED)
{
#ifdef HAVE_LD_STATIC_DYNAMIC
  if (force_static)
    append_option (OPT_Wl_, LD_STATIC_OPTION, 1);
#endif
  append_option (OPT_l, FORTRAN_LIBRARY, 1);
#ifdef HAVE_LD_STATIC_DYNAMIC
  if (force_static)
    append_option (OPT_Wl_, LD_DYNAMIC_OPTION, 1);
#endif
}

void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
		      unsigned int *in_decoded_options_count,
		      int *in_added_libraries ATTRIBUTE_UNUSED)
{
  unsigned int argc = *in_decoded_options_count;
  struct cl_decoded_option *decoded_options = *in_decoded_options;
  unsigned int i;
  int verbose = 0;

  /* 0 => -xnone in effect.
     1 => -xfoo in effect.  */
  int saw_speclang = 0;

  /* 0 => initial/reset state
     1 => last arg was -l<library>
     2 => last two args were -l<library> -lm.  */
  int saw_library = 0;

  /* By default, we throw on the math library if we have one.  */
  int need_math = (MATH_LIBRARY[0] != '\0');

  /* Whether we should link a static libgfortran.  */
  int static_lib = 0; 

  /* Whether we need to link statically.  */
  int static_linking = 0;

  /* The number of input and output files in the incoming arg list.  */
  int n_infiles = 0;
  int n_outfiles = 0;

  library = FORTRAN_LIBRARY;

#if 0
  fprintf (stderr, "Incoming:");
  for (i = 0; i < argc; i++)
    fprintf (stderr, " %s", decoded_options[i].orig_option_with_args_text);
  fprintf (stderr, "\n");
#endif

  g77_xargc = argc;
  g77_x_decoded_options = decoded_options;
  g77_newargc = 0;
  g77_new_decoded_options = decoded_options;

  /* First pass through arglist.

     If -nostdlib or a "turn-off-linking" option is anywhere in the
     command line, don't do any library-option processing (except
     relating to -x).  */

  for (i = 1; i < argc; ++i)
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
	case OPT_r:
	case OPT_S:
	case OPT_fsyntax_only:
	case OPT_E:
	  /* These options disable linking entirely or linking of the
	     standard libraries.  */
	  library = 0;
	  break;

	case OPT_static_libgfortran:
#ifdef HAVE_LD_STATIC_DYNAMIC
	  static_lib = 1;
#endif
	  break;

	case OPT_static:
#ifdef HAVE_LD_STATIC_DYNAMIC
	  static_linking = 1;
#endif
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
	  printf ("GNU Fortran %s%s\n", pkgversion_string, version_string);
	  printf ("Copyright %s 2019 Free Software Foundation, Inc.\n",
		  _("(C)"));
	  fputs (_("This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n"),
		stdout);
	  exit (0);
	  break;

	case OPT__help:
	  /* Let gcc.c handle this, as it has a really
	     cool facility for handling --help and --verbose --help.  */
	  return;

	default:
	  break;
	}
    }

  if ((n_outfiles != 0) && (n_infiles == 0))
    fatal_error (input_location,
		 "no input files; unwilling to write output files");

  /* If there are no input files, no need for the library.  */
  if (n_infiles == 0)
    library = 0;

  /* Second pass through arglist, transforming arguments as appropriate.  */

  append_arg (&decoded_options[0]); /* Start with command name, of course.  */

  for (i = 1; i < argc; ++i)
    {
      if (decoded_options[i].errors & CL_ERR_MISSING_ARG)
	{
	  append_arg (&decoded_options[i]);
	  continue;
	}

      if (decoded_options[i].opt_index == OPT_SPECIAL_input_file
	  && decoded_options[i].arg[0] == '\0')
	{
	  /* Interesting.  Just append as is.  */
	  append_arg (&decoded_options[i]);
	  continue;
	}

      if (decoded_options[i].opt_index != OPT_l
	  && (decoded_options[i].opt_index != OPT_SPECIAL_input_file
	      || strcmp (decoded_options[i].arg, "-") == 0))
	{
	  /* Not a filename or library.  */

	  if (saw_library == 1 && need_math)	/* -l<library>.  */
	    append_option (OPT_l, MATH_LIBRARY, 1);

	  saw_library = 0;

	  if (decoded_options[i].opt_index == OPT_SPECIAL_input_file)
	    {
	      append_arg (&decoded_options[i]);	/* "-" == Standard input.  */
	      continue;
	    }

	  if (decoded_options[i].opt_index == OPT_x)
	    {
	      /* Track input language.  */
	      const char *lang = decoded_options[i].arg;

	      saw_speclang = (strcmp (lang, "none") != 0);
	    }

	  append_arg (&decoded_options[i]);

	  continue;
	}

      /* A filename/library, not an option.  */

      if (saw_speclang)
	saw_library = 0;	/* -xfoo currently active.  */
      else
	{			/* -lfoo or filename.  */
	  if (decoded_options[i].opt_index == OPT_l
	      && strcmp (decoded_options[i].arg, MATH_LIBRARY) == 0)
	    {
	      if (saw_library == 1)
		saw_library = 2;	/* -l<library> -lm.  */
	      else
		add_arg_libgfortran (static_lib && !static_linking);
	    }
	  else if (decoded_options[i].opt_index == OPT_l
	      && strcmp (decoded_options[i].arg, FORTRAN_LIBRARY) == 0)
	    {
	      saw_library = 1;	/* -l<library>.  */
	      add_arg_libgfortran (static_lib && !static_linking);
	      continue;
	    }
	  else
	    {			/* Other library, or filename.  */
	      if (saw_library == 1 && need_math)
		append_option (OPT_l, MATH_LIBRARY, 1);
	      saw_library = 0;
	    }
	}
      append_arg (&decoded_options[i]);
    }

  /* Append `-lgfortran -lm' as necessary.  */

  if (library)
    {				/* Doing a link and no -nostdlib.  */
      if (saw_speclang)
	append_option (OPT_x, "none", 1);

      switch (saw_library)
	{
	case 0:
	  add_arg_libgfortran (static_lib && !static_linking);
	  /* Fall through.  */

	case 1:
	  if (need_math)
	    append_option (OPT_l, MATH_LIBRARY, 1);
	default:
	  break;
	}
    }

#ifdef ENABLE_SHARED_LIBGCC
  if (library)
    {
      unsigned int i;

      for (i = 1; i < g77_newargc; i++)
	if (g77_new_decoded_options[i].opt_index == OPT_static_libgcc
	    || g77_new_decoded_options[i].opt_index == OPT_static)
	  break;

      if (i == g77_newargc)
	append_option (OPT_shared_libgcc, NULL, 1);
    }

#endif

  if (verbose && g77_new_decoded_options != g77_x_decoded_options)
    {
      fprintf (stderr, _("Driving:"));
      for (i = 0; i < g77_newargc; i++)
	fprintf (stderr, " %s",
		 g77_new_decoded_options[i].orig_option_with_args_text);
      fprintf (stderr, "\n");
    }

  *in_decoded_options_count = g77_newargc;
  *in_decoded_options = g77_new_decoded_options;
}


/* Called before linking.  Returns 0 on success and -1 on failure.  */
int
lang_specific_pre_link (void)
{
  if (library)
    do_spec ("%:include(libgfortran.spec)");

  return 0;
}

/* Number of extra output files that lang_specific_pre_link may generate.  */
int lang_specific_extra_outfiles = 0;	/* Not used for F77.  */
