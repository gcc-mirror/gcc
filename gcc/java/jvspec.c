/* Specific flags and argument handling of the front-end of the 
   GNU compiler for the Java(TM) language.
   Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.

This file is part of GNU CC.

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. 

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

#include "config.h"

#include "system.h"

#include "gansidecl.h"

#if defined (WITH_THREAD_posix) || defined (WITH_THREAD_pthreads)
#define THREAD_NAME "-lpthread"
#elif defined (WITH_THREAD_qt)
#define THREAD_NAME "-lqthreads"
#endif

/* This bit is set if we saw a `-xfoo' language specification.  */
#define LANGSPEC	(1<<1)
/* This bit is set if they did `-lm' or `-lmath'.  */
#define MATHLIB		(1<<2)
/* This bit is set if they did `-lc'.  */
#define WITHLIBC	(1<<3)
/* This bit is set if they did `-lgc'.  */
#define GCLIB		(1<<4)
/* This bit is set if they did `-lpthread' (or added some other thread
   library).  */
#define THREADLIB	(1<<5)

#ifndef MATH_LIBRARY
#define MATH_LIBRARY "-lm"
#endif

extern char *xmalloc PROTO((size_t));
extern int do_spec		PROTO((char *));
extern char *input_filename;
extern size_t input_filename_length;

char *main_class_name = NULL;
int lang_specific_extra_outfiles = 0;

char jvgenmain_spec[] =
  "jvgenmain %i %{!pipe:%g.i} |\n\
   cc1 %{!pipe:%g.i} %1 \
		   %{!Q:-quiet} -dumpbase %b.c %{d*} %{m*} %{a*}\
		   %{g*} %{O*} \
		   %{v:-version} %{pg:-p} %{p} %{f*}\
		   %{aux-info*}\
		   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
		   %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
              %{!S:as %a %Y -o %w%b%O %{!pipe:%g.s} %A\n }";

void
lang_specific_driver (fn, in_argc, in_argv, in_added_libraries)
     void (*fn)();
     int *in_argc;
     char ***in_argv;
     int *in_added_libraries;
{
  int i, j;

  /* If non-zero, the user gave us the `-v' flag.  */ 
  int saw_verbose_flag = 0;

  /* This will be 0 if we encounter a situation where we should not
     link in libjava.  */
  int library = 1;

  /* The number of arguments being added to what's in argv, other than
     libraries.  We use this to track the number of times we've inserted
     -xc++/-xnone.  */
  int added = 2;

  /* Used to track options that take arguments, so we don't go wrapping
     those with -xc++/-xnone.  */
  char *quote = NULL;

  /* The new argument list will be contained in this.  */
  char **arglist;

  /* Non-zero if we saw a `-xfoo' language specification on the
     command line.  Used to avoid adding our own -xc++ if the user
     already gave a language for the file.  */
  int saw_speclang = 0;

  /* "-lm" or "-lmath" if it appears on the command line.  */
  char *saw_math = 0;

  /* "-lc" if it appears on the command line.  */
  char *saw_libc = 0;

  /* "-lgc" if it appears on the command line.  */
  char *saw_gc = 0;

  /* Saw `-l' option for the thread library.  */
  char *saw_threadlib = 0;

  /* Saw `-ljava' on command line.  */
  int saw_libjava = 0;

  /* An array used to flag each argument that needs a bit set for
     LANGSPEC, MATHLIB, WITHLIBC, or GCLIB.  */
  int *args;

  /* By default, we throw on the math library.  */
  int need_math = 1;

  /* By default, we throw in the thread library (if one is required).
   */
  int need_thread = 1;

  /* The total number of arguments with the new stuff.  */
  int argc;

  /* The argument list.  */
  char **argv;

  /* The number of libraries added in.  */
  int added_libraries;

  /* The total number of arguments with the new stuff.  */
  int num_args = 1;

  argc = *in_argc;
  argv = *in_argv;
  added_libraries = *in_added_libraries;

  args = (int *) xmalloc (argc * sizeof (int));
  bzero ((char *) args, argc * sizeof (int));

  for (i = 1; i < argc; i++)
    {
      /* If the previous option took an argument, we swallow it here.  */
      if (quote)
	{
	  quote = NULL;
	  continue;
	}

      /* We don't do this anymore, since we don't get them with minus
	 signs on them.  */
      if (argv[i][0] == '\0' || argv[i][1] == '\0')
	continue;

      if (argv[i][0] == '-')
	{
	  if (library != 0 && (strcmp (argv[i], "-nostdlib") == 0
			       || strcmp (argv[i], "-nodefaultlibs") == 0))
	    {
	      library = 0;
	    }
	  else if (strcmp (argv[i], "-lm") == 0
		   || strcmp (argv[i], "-lmath") == 0
#ifdef ALT_LIBM
		   || strcmp (argv[i], ALT_LIBM) == 0
#endif
		  )
	    {
	      args[i] |= MATHLIB;
	      need_math = 0;
	    }
	  else if (strncmp (argv[i], "-fmain=", 7) == 0)
	    main_class_name = argv[i] + 7;
	  else if (strcmp (argv[i], "-ljava") == 0)
	    saw_libjava = 1;
	  else if (strcmp (argv[i], "-lc") == 0)
	    args[i] |= WITHLIBC;
	  else if (strcmp (argv[i], "-lgc") == 0)
	    args[i] |= GCLIB;
#ifdef THREAD_NAME
	  else if (strcmp (argv[i], THREAD_NAME) == 0)
	    {
	      args[i] |= THREADLIB;
	      need_thread = 0;
	    }
#endif
	  else if (strcmp (argv[i], "-v") == 0)
	    {
	      saw_verbose_flag = 1;
	      if (argc == 2)
		{
		  /* If they only gave us `-v', don't try to link
		     in libjava.  */ 
		  library = 0;
		}
	    }
	  else if (strncmp (argv[i], "-x", 2) == 0)
	    saw_speclang = 1;
	  else if (((argv[i][2] == '\0'
		     && (char *)strchr ("bBVDUoeTuIYmLiA", argv[i][1]) != NULL)
		    || strcmp (argv[i], "-Tdata") == 0))
	    quote = argv[i];
	  else if (library != 0 && ((argv[i][2] == '\0'
		     && (char *) strchr ("cSEM", argv[i][1]) != NULL)
		    || strcmp (argv[i], "-MM") == 0))
	    {
	      /* Don't specify libraries if we won't link, since that would
		 cause a warning.  */
	      library = 0;
	      added -= 2;
	    }
	  else
	    /* Pass other options through.  */
	    continue;
	}
      else
	{
	  int len; 

	  if (saw_speclang)
	    {
	      saw_speclang = 0;
	      continue;
	    }

	  /* If the filename ends in .c or .i, put options around it.
	     But not if a specified -x option is currently active.  */
	  len = strlen (argv[i]);
	  if (len > 2
	      && (argv[i][len - 1] == 'c' || argv[i][len - 1] == 'i')
	      && argv[i][len - 2] == '.')
	    {
	      args[i] |= LANGSPEC;
	      added += 2;
	    }
	}
    }

  if (quote)
    (*fn) ("argument to `%s' missing\n", quote);

  /* If we know we don't have to do anything, bail now.  */
  if (! added && ! library && main_class_name == NULL)
    {
      free (args);
      return;
    }

  num_args = argc + added + need_math + need_thread;
  if (main_class_name)
    {
      lang_specific_extra_outfiles++;
    }
  arglist = (char **) xmalloc (num_args * sizeof (char *));

  /* NOTE: We start at 1 now, not 0.  */
  for (i = 0, j = 0; i < argc; i++, j++)
    {
      arglist[j] = argv[i];

      if (strncmp (argv[i], "-fmain=", 7) == 0)
	{
	  --j;
	  continue;
	}

      /* Make sure -ljava is before the math library, since libjava
	 itself uses those math routines.  */
      if (!saw_math && (args[i] & MATHLIB) && library)
	{
	  --j;
	  saw_math = argv[i];
	}

      /* Likewise -ljava must come before -lc.  */
      if (!saw_libc && (args[i] & WITHLIBC) && library)
	{
	  --j;
	  saw_libc = argv[i];
	}

      /* And -ljava must come before -lgc.  */
      if (!saw_gc && (args[i] & GCLIB) && library)
	{
	  --j;
	  saw_gc = argv[i];
	}

      /* And -ljava must come before thread library.  */
      if (!saw_threadlib && (args[i] & THREADLIB) && library)
	{
	  --j;
	  saw_threadlib = argv[i];
	}
  }

  /* Add `-ljava' if we haven't already done so.  */
  if (library && ! saw_libjava)
    {
      arglist[j++] = "-ljava";
      added_libraries++;
    }

  if (saw_math)
    arglist[j++] = saw_math;
  else if (library)
    {
      arglist[j++] = MATH_LIBRARY;
      added_libraries++;
    }

  /* FIXME: we need a way to know when the GC library should be
     added.  Then we can add it if the user hasn't already.  */
  if (saw_gc)
    arglist[j++] = saw_gc;

  /* Thread library must come after GC library as well as after
     -ljava.  */
  if (saw_threadlib)
    arglist[j++] = saw_threadlib;
#ifdef THREAD_NAME
  else if (library)
    {
      arglist[j++] = THREAD_NAME;
      added_libraries++;
    }
#endif

  if (saw_libc)
    arglist[j++] = saw_libc;

  arglist[j] = NULL;

  *in_argc = j;
  *in_argv = arglist;
  *in_added_libraries = added_libraries;
}

int
lang_specific_pre_link ()
{
  if (main_class_name == NULL)
    return 0;
  input_filename = main_class_name;
  input_filename_length = strlen (main_class_name);
  return do_spec (jvgenmain_spec);
}
