/* Specific flags and argument handling of the C++ front end.
   Copyright (C) 1996-2013 Free Software Foundation, Inc.

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
#include "tm.h"
#include "gcc.h"
#include "opts.h"

/* This bit is set if we saw a `-xfoo' language specification.  */
#define LANGSPEC	(1<<1)
/* This bit is set if they did `-lm' or `-lmath'.  */
#define MATHLIB		(1<<2)
/* This bit is set if they did `-lrt' or equivalent.  */
#define TIMELIB		(1<<3)
/* This bit is set if they did `-lc'.  */
#define WITHLIBC	(1<<4)
/* Skip this option.  */
#define SKIPOPT		(1<<5)

#ifndef MATH_LIBRARY
#define MATH_LIBRARY "m"
#endif
#ifndef MATH_LIBRARY_PROFILE
#define MATH_LIBRARY_PROFILE MATH_LIBRARY
#endif

#ifndef TIME_LIBRARY
#define TIME_LIBRARY ""
#endif

#ifndef LIBSTDCXX
#define LIBSTDCXX "stdc++"
#endif
#ifndef LIBSTDCXX_PROFILE
#define LIBSTDCXX_PROFILE LIBSTDCXX
#endif
#ifndef LIBSTDCXX_STATIC
#define LIBSTDCXX_STATIC NULL
#endif

void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
		      unsigned int *in_decoded_options_count,
		      int *in_added_libraries)
{
  unsigned int i, j;

  /* If nonzero, the user gave us the `-p' or `-pg' flag.  */
  int saw_profile_flag = 0;

  /* What do with libstdc++:
     -1 means we should not link in libstdc++
     0  means we should link in libstdc++ if it is needed
     1  means libstdc++ is needed and should be linked in.
     2  means libstdc++ is needed and should be linked statically.  */
  int library = 0;

  /* The number of arguments being added to what's in argv, other than
     libraries.  We use this to track the number of times we've inserted
     -xc++/-xnone.  */
  int added = 0;

  /* The new argument list will be contained in this.  */
  struct cl_decoded_option *new_decoded_options;

  /* Nonzero if we saw a `-xfoo' language specification on the
     command line.  Used to avoid adding our own -xc++ if the user
     already gave a language for the file.  */
  int saw_speclang = 0;

  /* "-lm" or "-lmath" if it appears on the command line.  */
  const struct cl_decoded_option *saw_math = NULL;

  /* "-lrt" or eqivalent if it appears on the command line.  */
  const struct cl_decoded_option *saw_time = NULL;

  /* "-lc" if it appears on the command line.  */
  const struct cl_decoded_option *saw_libc = NULL;

  /* An array used to flag each argument that needs a bit set for
     LANGSPEC, MATHLIB, TIMELIB, or WITHLIBC.  */
  int *args;

  /* By default, we throw on the math library if we have one.  */
  int need_math = (MATH_LIBRARY[0] != '\0');

  /* By default, we throw on the time library if we have one.  */
  int need_time = (TIME_LIBRARY[0] != '\0');

  /* True if we saw -static.  */
  int static_link = 0;

  /* True if we should add -shared-libgcc to the command-line.  */
  int shared_libgcc = 1;

  /* The total number of arguments with the new stuff.  */
  unsigned int argc;

  /* The argument list.  */
  struct cl_decoded_option *decoded_options;

  /* The number of libraries added in.  */
  int added_libraries;

  /* The total number of arguments with the new stuff.  */
  unsigned int num_args = 1;

  argc = *in_decoded_options_count;
  decoded_options = *in_decoded_options;
  added_libraries = *in_added_libraries;

  args = XCNEWVEC (int, argc);

  for (i = 1; i < argc; i++)
    {
      const char *arg = decoded_options[i].arg;
      if (decoded_options[i].errors & CL_ERR_MISSING_ARG)
	continue; /* Avoid examining arguments of options missing them.  */

      switch (decoded_options[i].opt_index)
	{
	case OPT_nostdlib:
	case OPT_nodefaultlibs:
	  library = -1;
	  break;

	case OPT_l:
	  if (strcmp (arg, MATH_LIBRARY) == 0)
	    {
	      args[i] |= MATHLIB;
	      need_math = 0;
	    }
	  else if (strcmp (arg, TIME_LIBRARY) == 0)
	    {
	      args[i] |= TIMELIB;
	      need_time = 0;
	    }
	  else if (strcmp (arg, "c") == 0)
	    args[i] |= WITHLIBC;
	  else
	    /* Unrecognized libraries (e.g. -lfoo) may require libstdc++.  */
	    library = (library == 0) ? 1 : library;
	  break;

	case OPT_pg:
	case OPT_p:
	  saw_profile_flag++;
	  break;

	case OPT_x:
	  if (library == 0
	      && (strcmp (arg, "c++") == 0
		  || strcmp (arg, "c++-cpp-output") == 0
		  || strcmp (arg, "objective-c++") == 0
		  || strcmp (arg, "objective-c++-cpp-output") == 0))
	    library = 1;
		
	  saw_speclang = 1;
	  break;

	case OPT_Xlinker:
	case OPT_Wl_:
	  /* Arguments that go directly to the linker might be .o files,
	     or something, and so might cause libstdc++ to be needed.  */
	  if (library == 0)
	    library = 1;
	  break;

	case OPT_c:
	case OPT_S:
	case OPT_E:
	case OPT_M:
	case OPT_MM:
	case OPT_fsyntax_only:
	  /* Don't specify libraries if we won't link, since that would
	     cause a warning.  */
	  library = -1;
	  break;

	case OPT_static:
	  static_link = 1;
	  break;

	case OPT_static_libgcc:
	  shared_libgcc = 0;
	  break;

	case OPT_static_libstdc__:
	  library = library >= 0 ? 2 : library;
	  args[i] |= SKIPOPT;
	  break;

	case OPT_SPECIAL_input_file:
	  {
	    int len;

	    /* We don't do this anymore, since we don't get them with minus
	       signs on them.  */
	    if (arg[0] == '\0' || arg[1] == '\0')
	      continue;

	    if (saw_speclang)
	      {
		saw_speclang = 0;
		continue;
	      }

	    /* If the filename ends in .[chi], put options around it.
	       But not if a specified -x option is currently active.  */
	    len = strlen (arg);
	    if (len > 2
		&& (arg[len - 1] == 'c'
		    || arg[len - 1] == 'i'
		    || arg[len - 1] == 'h')
		&& arg[len - 2] == '.')
	      {
		args[i] |= LANGSPEC;
		added += 2;
	      }

	    /* If we don't know that this is a header file, we might
	       need to be linking in the libraries.  */
	    if (library == 0)
	      {
		if ((len <= 2 || strcmp (arg + (len - 2), ".H") != 0)
		    && (len <= 2 || strcmp (arg + (len - 2), ".h") != 0)
		    && (len <= 4 || strcmp (arg + (len - 4), ".hpp") != 0)
		    && (len <= 3 || strcmp (arg + (len - 3), ".hp") != 0)
		    && (len <= 4 || strcmp (arg + (len - 4), ".hxx") != 0)
		    && (len <= 4 || strcmp (arg + (len - 4), ".h++") != 0)
		    && (len <= 4 || strcmp (arg + (len - 4), ".HPP") != 0)
		    && (len <= 4 || strcmp (arg + (len - 4), ".tcc") != 0)
		    && (len <= 3 || strcmp (arg + (len - 3), ".hh") != 0))
		  library = 1;
	      }
	  }
	  break;
	}
    }

  /* There's no point adding -shared-libgcc if we don't have a shared
     libgcc.  */
#ifndef ENABLE_SHARED_LIBGCC
  shared_libgcc = 0;
#endif

  /* Add one for shared_libgcc or extra static library.  */
  num_args = argc + added + need_math + (library > 0) * 4 + 1;
  new_decoded_options = XNEWVEC (struct cl_decoded_option, num_args);

  i = 0;
  j = 0;

  /* Copy the 0th argument, i.e., the name of the program itself.  */
  new_decoded_options[j++] = decoded_options[i++];

  /* NOTE: We start at 1 now, not 0.  */
  while (i < argc)
    {
      new_decoded_options[j] = decoded_options[i];

      /* Make sure -lstdc++ is before the math library, since libstdc++
	 itself uses those math routines.  */
      if (!saw_math && (args[i] & MATHLIB) && library > 0)
	{
	  --j;
	  saw_math = &decoded_options[i];
	}

      if (!saw_time && (args[i] & TIMELIB) && library > 0)
	{
	  --j;
	  saw_time = &decoded_options[i];
	}

      if (!saw_libc && (args[i] & WITHLIBC) && library > 0)
	{
	  --j;
	  saw_libc = &decoded_options[i];
	}

      /* Wrap foo.[chi] files in a language specification to
	 force the gcc compiler driver to run cc1plus on them.  */
      if (args[i] & LANGSPEC)
	{
	  const char *arg = decoded_options[i].arg;
	  int len = strlen (arg);
	  switch (arg[len - 1])
	    {
	    case 'c':
	      generate_option (OPT_x, "c++", 1, CL_DRIVER,
			       &new_decoded_options[j++]);
	      break;
	    case 'i':
	      generate_option (OPT_x, "c++-cpp-output", 1, CL_DRIVER,
			       &new_decoded_options[j++]);
	      break;
	    case 'h':
	      generate_option (OPT_x, "c++-header", 1, CL_DRIVER,
			       &new_decoded_options[j++]);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  new_decoded_options[j++] = decoded_options[i];
	  generate_option (OPT_x, "none", 1, CL_DRIVER,
			   &new_decoded_options[j]);
	}

      if ((args[i] & SKIPOPT) != 0)
	--j;

      i++;
      j++;
    }

  /* Add `-lstdc++' if we haven't already done so.  */
  if (library > 0)
    {
#ifdef HAVE_LD_STATIC_DYNAMIC
      if (library > 1 && !static_link)
	{
	  generate_option (OPT_Wl_, LD_STATIC_OPTION, 1, CL_DRIVER,
			   &new_decoded_options[j]);
	  j++;
	}
#endif
      generate_option (OPT_l,
		       saw_profile_flag ? LIBSTDCXX_PROFILE : LIBSTDCXX, 1,
		       CL_DRIVER, &new_decoded_options[j]);
      added_libraries++;
      j++;
      /* Add target-dependent static library, if necessary.  */
      if ((static_link || library > 1) && LIBSTDCXX_STATIC != NULL)
	{
	  generate_option (OPT_l, LIBSTDCXX_STATIC, 1,
			   CL_DRIVER, &new_decoded_options[j]);
	  added_libraries++;
	  j++;
	}
#ifdef HAVE_LD_STATIC_DYNAMIC
      if (library > 1 && !static_link)
	{
	  generate_option (OPT_Wl_, LD_DYNAMIC_OPTION, 1, CL_DRIVER,
			   &new_decoded_options[j]);
	  j++;
	}
#endif
    }
  if (saw_math)
    new_decoded_options[j++] = *saw_math;
  else if (library > 0 && need_math)
    {
      generate_option (OPT_l,
		       saw_profile_flag ? MATH_LIBRARY_PROFILE : MATH_LIBRARY,
		       1, CL_DRIVER, &new_decoded_options[j]);
      added_libraries++;
      j++;
    }
  if (saw_time)
    new_decoded_options[j++] = *saw_time;
  else if (library > 0 && need_time)
    {
      generate_option (OPT_l, TIME_LIBRARY, 1, CL_DRIVER,
		       &new_decoded_options[j]);
      added_libraries++;
      j++;
    }
  if (saw_libc)
    new_decoded_options[j++] = *saw_libc;
  if (shared_libgcc && !static_link)
    generate_option (OPT_shared_libgcc, NULL, 1, CL_DRIVER,
		     &new_decoded_options[j++]);

  *in_decoded_options_count = j;
  *in_decoded_options = new_decoded_options;
  *in_added_libraries = added_libraries;
}

/* Called before linking.  Returns 0 on success and -1 on failure.  */
int lang_specific_pre_link (void)  /* Not used for C++.  */
{
  return 0;
}

/* Number of extra output files that lang_specific_pre_link may generate.  */
int lang_specific_extra_outfiles = 0;  /* Not used for C++.  */
