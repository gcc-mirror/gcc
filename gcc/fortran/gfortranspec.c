/* Specific flags and argument handling of the Fortran front-end.
   Copyright (C) 1997, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

This file is part of GCC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
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
#include "gcc.h"

#include "coretypes.h"
#include "tm.h"
#include "intl.h"

#ifndef MATH_LIBRARY
#define MATH_LIBRARY "-lm"
#endif

#ifndef FORTRAN_INIT
#define FORTRAN_INIT "-lgfortranbegin"
#endif

#ifndef FORTRAN_LIBRARY
#define FORTRAN_LIBRARY "-lgfortran"
#endif

/* Options this driver needs to recognize, not just know how to
   skip over.  */
typedef enum
{
  OPTION_b,			/* Aka --prefix.  */
  OPTION_B,			/* Aka --target.  */
  OPTION_c,			/* Aka --compile.  */
  OPTION_E,			/* Aka --preprocess.  */
  OPTION_help,			/* --help.  */
  OPTION_i,			/* -imacros, -include, -include-*.  */
  OPTION_l,
  OPTION_L,			/* Aka --library-directory.  */
  OPTION_nostdlib,		/* Aka --no-standard-libraries, or
				   -nodefaultlibs.  */
  OPTION_o,			/* Aka --output.  */
  OPTION_S,			/* Aka --assemble.  */
  OPTION_syntax_only,		/* -fsyntax-only.  */
  OPTION_v,			/* Aka --verbose.  */
  OPTION_version,		/* --version.  */
  OPTION_V,			/* Aka --use-version.  */
  OPTION_x,			/* Aka --language.  */
  OPTION_			/* Unrecognized or unimportant.  */
}
Option;

/* The original argument list and related info is copied here.  */
static int g77_xargc;
static const char *const *g77_xargv;
static void lookup_option (Option *, int *, const char **, const char *);
static void append_arg (const char *);

/* The new argument list will be built here.  */
static int g77_newargc;
static const char **g77_newargv;

/* --- This comes from gcc.c (2.8.1) verbatim: */

/* This defines which switch letters take arguments.  */

#ifndef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) DEFAULT_SWITCH_TAKES_ARG(CHAR)
#endif

/* This defines which multi-letter switches take arguments.  */

#ifndef WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR) DEFAULT_WORD_SWITCH_TAKES_ARG (STR)
#endif

/* --- End of verbatim.  */

/* Assumes text[0] == '-'.  Returns number of argv items that belong to
   (and follow) this one, an option id for options important to the
   caller, and a pointer to the first char of the arg, if embedded (else
   returns NULL, meaning no arg or it's the next argv).

   Note that this also assumes gcc.c's pass converting long options
   to short ones, where available, has already been run.  */

static void
lookup_option (Option *xopt, int *xskip, const char **xarg, const char *text)
{
  Option opt = OPTION_;
  int skip;
  const char *arg = NULL;

  if ((skip = SWITCH_TAKES_ARG (text[1])))
    skip -= (text[2] != '\0');	/* See gcc.c.  */

  if (text[1] == 'B')
    opt = OPTION_B, skip = (text[2] == '\0'), arg = text + 2;
  else if (text[1] == 'b')
    opt = OPTION_b, skip = (text[2] == '\0'), arg = text + 2;
  else if ((text[1] == 'c') && (text[2] == '\0'))
    opt = OPTION_c, skip = 0;
  else if ((text[1] == 'E') && (text[2] == '\0'))
    opt = OPTION_E, skip = 0;
  else if (text[1] == 'i')
    opt = OPTION_i, skip = 0;
  else if (text[1] == 'l')
    opt = OPTION_l;
  else if (text[1] == 'L')
    opt = OPTION_L, arg = text + 2;
  else if (text[1] == 'o')
    opt = OPTION_o;
  else if ((text[1] == 'S') && (text[2] == '\0'))
    opt = OPTION_S, skip = 0;
  else if (text[1] == 'V')
    opt = OPTION_V, skip = (text[2] == '\0');
  else if ((text[1] == 'v') && (text[2] == '\0'))
    opt = OPTION_v, skip = 0;
  else if (text[1] == 'x')
    opt = OPTION_x, arg = text + 2;
  else
    {
      if ((skip = WORD_SWITCH_TAKES_ARG (text + 1)) != 0)	/* See gcc.c.  */
	;
      else if (!strcmp (text, "-fhelp"))	/* Really --help!! */
	opt = OPTION_help;
      else if (!strcmp (text, "-nostdlib")
	       || !strcmp (text, "-nodefaultlibs"))
	opt = OPTION_nostdlib;
      else if (!strcmp (text, "-fsyntax-only"))
	opt = OPTION_syntax_only;
      else if (!strcmp (text, "-dumpversion"))
	opt = OPTION_version;
      else if (!strcmp (text, "-fversion"))	/* Really --version!! */
	opt = OPTION_version;
      else if (!strcmp (text, "-Xlinker") || !strcmp (text, "-specs"))
	skip = 1;
      else
	skip = 0;
    }

  if (xopt != NULL)
    *xopt = opt;
  if (xskip != NULL)
    *xskip = skip;
  if (xarg != NULL)
    {
      if ((arg != NULL) && (arg[0] == '\0'))
	*xarg = NULL;
      else
	*xarg = arg;
    }
}

/* Append another argument to the list being built.  As long as it is
   identical to the corresponding arg in the original list, just increment
   the new arg count.  Otherwise allocate a new list, etc.  */

static void
append_arg (const char *arg)
{
  static int newargsize;

#if 0
  fprintf (stderr, "`%s'\n", arg);
#endif

  if (g77_newargv == g77_xargv
      && g77_newargc < g77_xargc
      && (arg == g77_xargv[g77_newargc]
	  || !strcmp (arg, g77_xargv[g77_newargc])))
    {
      ++g77_newargc;
      return;			/* Nothing new here.  */
    }

  if (g77_newargv == g77_xargv)
    {				/* Make new arglist.  */
      int i;

      newargsize = (g77_xargc << 2) + 20;	/* This should handle all.  */
      g77_newargv = (const char **) xmalloc (newargsize * sizeof (char *));

      /* Copy what has been done so far.  */
      for (i = 0; i < g77_newargc; ++i)
	g77_newargv[i] = g77_xargv[i];
    }

  if (g77_newargc == newargsize)
    fatal ("overflowed output arg list for '%s'", arg);

  g77_newargv[g77_newargc++] = arg;
}

void
lang_specific_driver (int *in_argc, const char *const **in_argv,
		      int *in_added_libraries ATTRIBUTE_UNUSED)
{
  int argc = *in_argc;
  const char *const *argv = *in_argv;
  int i;
  int verbose = 0;
  Option opt;
  int skip;
  const char *arg;

  /* This will be NULL if we encounter a situation where we should not
     link in libf2c.  */
  const char *library = FORTRAN_LIBRARY;

  /* 0 => -xnone in effect.
     1 => -xfoo in effect.  */
  int saw_speclang = 0;

  /* 0 => initial/reset state
     1 => last arg was -l<library>
     2 => last two args were -l<library> -lm.  */
  int saw_library = 0;

  /* 0 => initial/reset state
     1 => FORTRAN_INIT linked in */
  int use_init = 0;

  /* By default, we throw on the math library if we have one.  */
  int need_math = (MATH_LIBRARY[0] != '\0');

  /* The number of input and output files in the incoming arg list.  */
  int n_infiles = 0;
  int n_outfiles = 0;

#if 0
  fprintf (stderr, "Incoming:");
  for (i = 0; i < argc; i++)
    fprintf (stderr, " %s", argv[i]);
  fprintf (stderr, "\n");
#endif

  g77_xargc = argc;
  g77_xargv = argv;
  g77_newargc = 0;
  g77_newargv = (const char **) argv;

  /* First pass through arglist.

     If -nostdlib or a "turn-off-linking" option is anywhere in the
     command line, don't do any library-option processing (except
     relating to -x).  Also, if -v is specified, but no other options
     that do anything special (allowing -V version, etc.), remember
     to add special stuff to make gcc command actually invoke all
     the different phases of the compilation process so all the version
     numbers can be seen.

     Also, here is where all problems with missing arguments to options
     are caught.  If this loop is exited normally, it means all options
     have the appropriate number of arguments as far as the rest of this
     program is concerned.  */

  for (i = 1; i < argc; ++i)
    {
      if ((argv[i][0] == '+') && (argv[i][1] == 'e'))
	{
	  continue;
	}

      if ((argv[i][0] != '-') || (argv[i][1] == '\0'))
	{
	  ++n_infiles;
	  continue;
	}

      lookup_option (&opt, &skip, NULL, argv[i]);

      switch (opt)
	{
	case OPTION_nostdlib:
	case OPTION_c:
	case OPTION_S:
	case OPTION_syntax_only:
	case OPTION_E:
	  /* These options disable linking entirely or linking of the
	     standard libraries.  */
	  library = 0;
	  break;

	case OPTION_l:
	  ++n_infiles;
	  break;

	case OPTION_o:
	  ++n_outfiles;
	  break;

	case OPTION_v:
	  verbose = 1;
	  break;

	case OPTION_b:
	case OPTION_B:
	case OPTION_L:
	case OPTION_i:
	case OPTION_V:
	  /* These options are useful in conjunction with -v to get
	     appropriate version info.  */
	  break;

	case OPTION_version:
	  printf ("GNU Fortran 95 (GCC) %s\n", version_string);
	  printf ("Copyright %s 2005 Free Software Foundation, Inc.\n\n",
	          _("(C)"));
	  printf (_("GNU Fortran comes with NO WARRANTY, to the extent permitted by law.\n\
You may redistribute copies of GNU Fortran\n\
under the terms of the GNU General Public License.\n\
For more information about these matters, see the file named COPYING\n\n"));
	  exit (0);
	  break;

	case OPTION_help:
	  /* Let gcc.c handle this, as it has a really
	     cool facility for handling --help and --verbose --help.  */
	  return;

	default:
	  break;
	}

      /* This is the one place we check for missing arguments in the
         program.  */

      if (i + skip < argc)
	i += skip;
      else
	fatal ("argument to '%s' missing", argv[i]);
    }

  if ((n_outfiles != 0) && (n_infiles == 0))
    fatal ("no input files; unwilling to write output files");

  /* If there are no input files, no need for the library.  */
  if (n_infiles == 0)
    library = 0;

  /* Second pass through arglist, transforming arguments as appropriate.  */

  append_arg (argv[0]);		/* Start with command name, of course.  */

  for (i = 1; i < argc; ++i)
    {
      if (argv[i][0] == '\0')
	{
	  append_arg (argv[i]);	/* Interesting.  Just append as is.  */
	  continue;
	}

      if ((argv[i][0] == '-') && (argv[i][1] == 'M'))
        {
          char *p;

          if (argv[i][2] == '\0')
            {
              p = xmalloc (strlen (argv[i + 1]) + 2);
              p[0] = '-';
              p[1] = 'J';
              strcpy (&p[2], argv[i + 1]);
              i++;
            }
          else
            {
              p = xmalloc (strlen (argv[i]) + 1);
              strcpy (p, argv[i]);
            }
          append_arg (p);
          continue;
        }

      if ((argv[i][0] == '-') && (argv[i][1] != 'l'))
	{
	  /* Not a filename or library.  */

	  if (saw_library == 1 && need_math)	/* -l<library>.  */
	    append_arg (MATH_LIBRARY);

	  saw_library = 0;

	  lookup_option (&opt, &skip, &arg, argv[i]);

	  if (argv[i][1] == '\0')
	    {
	      append_arg (argv[i]);	/* "-" == Standard input.  */
	      continue;
	    }

	  if (opt == OPTION_x)
	    {
	      /* Track input language.  */
	      const char *lang;

	      if (arg == NULL)
		lang = argv[i + 1];
	      else
		lang = arg;

	      saw_speclang = (strcmp (lang, "none") != 0);
	    }

	  append_arg (argv[i]);

	  for (; skip != 0; --skip)
	    append_arg (argv[++i]);

	  continue;
	}

      /* A filename/library, not an option.  */

      if (saw_speclang)
	saw_library = 0;	/* -xfoo currently active.  */
      else
	{			/* -lfoo or filename.  */
	  if (strcmp (argv[i], MATH_LIBRARY) == 0)
	    {
	      if (saw_library == 1)
		saw_library = 2;	/* -l<library> -lm.  */
	      else
		{
		  if (0 == use_init)
		    {
		      append_arg (FORTRAN_INIT);
		      use_init = 1;
		    }
		  append_arg (FORTRAN_LIBRARY);
		}
	    }
	  else if (strcmp (argv[i], FORTRAN_LIBRARY) == 0)
	    saw_library = 1;	/* -l<library>.  */
	  else
	    {			/* Other library, or filename.  */
	      if (saw_library == 1 && need_math)
		append_arg (MATH_LIBRARY);
	      saw_library = 0;
	    }
	}
      append_arg (argv[i]);
    }

  /* Append `-lg2c -lm' as necessary.  */

  if (library)
    {				/* Doing a link and no -nostdlib.  */
      if (saw_speclang)
	append_arg ("-xnone");

      switch (saw_library)
	{
	case 0:
	  if (0 == use_init)
	    {
	      append_arg (FORTRAN_INIT);
	      use_init = 1;
	    }
	  append_arg (library);
	case 1:
	  if (need_math)
	    append_arg (MATH_LIBRARY);
	default:
	  break;
	}
    }

#ifdef ENABLE_SHARED_LIBGCC
  if (library)
    {
      int i;

      for (i = 1; i < g77_newargc; i++)
	if (g77_newargv[i][0] == '-')
	  if (strcmp (g77_newargv[i], "-static-libgcc") == 0
	      || strcmp (g77_newargv[i], "-static") == 0)
	    break;

      if (i == g77_newargc)
	append_arg ("-shared-libgcc");
    }

#endif

  if (verbose && g77_newargv != g77_xargv)
    {
      fprintf (stderr, _("Driving:"));
      for (i = 0; i < g77_newargc; i++)
	fprintf (stderr, " %s", g77_newargv[i]);
      fprintf (stderr, "\n");
    }

  *in_argc = g77_newargc;
  *in_argv = g77_newargv;
}

/* Called before linking.  Returns 0 on success and -1 on failure.  */
int
lang_specific_pre_link (void)	/* Not used for F77.  */
{
  return 0;
}

/* Number of extra output files that lang_specific_pre_link may generate.  */
int lang_specific_extra_outfiles = 0;	/* Not used for F77.  */
