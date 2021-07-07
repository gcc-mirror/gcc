/* gospec.c -- Specific flags and argument handling of the gcc Go front end.
   Copyright (C) 2009-2021 Free Software Foundation, Inc.

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
#include "tm.h"
#include "opts.h"

/* This bit is set if we saw a `-xfoo' language specification.  */
#define LANGSPEC	(1<<1)
/* This bit is set if they did `-lm' or `-lmath'.  */
#define MATHLIB		(1<<2)
/* This bit is set if they did `-lpthread'.  */
#define THREADLIB	(1<<3)
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

#define THREAD_LIBRARY "pthread"
#define THREAD_LIBRARY_PROFILE THREAD_LIBRARY

#define LIBGO "go"
#define LIBGO_PROFILE LIBGO
#define LIBGOBEGIN "gobegin"

void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
		      unsigned int *in_decoded_options_count,
		      int *in_added_libraries)
{
  unsigned int i, j;

  /* If true, the user gave us the `-p' or `-pg' flag.  */
  bool saw_profile_flag = false;

  /* This is a tristate:
     -1 means we should not link in libgo
     0  means we should link in libgo if it is needed
     1  means libgo is needed and should be linked in.
     2  means libgo is needed and should be linked statically.  */
  int library = 0;

  /* The new argument list will be contained in this.  */
  struct cl_decoded_option *new_decoded_options;

  /* "-lm" or "-lmath" if it appears on the command line.  */
  const struct cl_decoded_option *saw_math = 0;

  /* "-lpthread" if it appears on the command line.  */
  const struct cl_decoded_option *saw_thread = 0;

  /* "-lc" if it appears on the command line.  */
  const struct cl_decoded_option *saw_libc = 0;

  /* An array used to flag each argument that needs a bit set for
     LANGSPEC, MATHLIB, or WITHLIBC.  */
  int *args;

  /* Whether we need the thread library.  */
  int need_thread = 0;

  /* By default, we throw on the math library if we have one.  */
  int need_math = (MATH_LIBRARY[0] != '\0');

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
  int num_args = 1;

  /* Supports split stack */
  int supports_split_stack = 0;

  /* Whether the -o option was used.  */
  bool saw_opt_o = false;

  /* Whether the -c option was used.  Also used for -E, -fsyntax-only,
     in general anything which implies only compilation and not
     linking.  */
  bool saw_opt_c = false;

  /* Whether the -S option was used.  */
  bool saw_opt_S = false;

#ifdef TARGET_CAN_SPLIT_STACK_64BIT
  /* Whether the -m64 option is in force. */
  bool is_m64 = TARGET_CAN_SPLIT_STACK_64BIT;
#endif

  /* The first input file with an extension of .go.  */
  const char *first_go_file = NULL;  

  /* Whether we saw any -g option.  */
  bool saw_opt_g = false;

  argc = *in_decoded_options_count;
  decoded_options = *in_decoded_options;
  added_libraries = *in_added_libraries;

  args = XCNEWVEC (int, argc);

  for (i = 1; i < argc; i++)
    {
      const char *arg = decoded_options[i].arg;

      switch (decoded_options[i].opt_index)
	{
	case OPT_r:
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
	  else if (strcmp (arg, THREAD_LIBRARY) == 0)
	    args[i] |= THREADLIB;
	  else if (strcmp (arg, "c") == 0)
	    args[i] |= WITHLIBC;
	  else
	    /* Unrecognized libraries (e.g. -lfoo) may require libgo.  */
	    library = (library == 0) ? 1 : library;
	  break;

#ifdef TARGET_CAN_SPLIT_STACK_64BIT
	case OPT_m32:
	  is_m64 = false;
	  break;

	case OPT_m64:
	  is_m64 = true;
	  break;
#endif

	case OPT_pg:
	case OPT_p:
	  saw_profile_flag = true;
	  break;

	case OPT_x:
	  if (library == 0 && strcmp (arg, "go") == 0)
	    library = 1;
	  break;

	case OPT_Xlinker:
	case OPT_Wl_:
	  /* Arguments that go directly to the linker might be .o files,
	     or something, and so might cause libgo to be needed.  */
	  if (library == 0)
	    library = 1;
	  break;

	case OPT_c:
	case OPT_E:
	case OPT_M:
	case OPT_MM:
	case OPT_fsyntax_only:
	  /* Don't specify libraries if we won't link, since that would
	     cause a warning.  */
	  saw_opt_c = true;
	  library = -1;
	  break;

	case OPT_S:
	  saw_opt_S = true;
	  library = -1;
	  break;

	case OPT_o:
	  saw_opt_o = true;
	  break;

	case OPT_g:
	case OPT_gdwarf:
	case OPT_gdwarf_:
	case OPT_ggdb:
	case OPT_gstabs:
	case OPT_gstabs_:
	case OPT_gvms:
	case OPT_gxcoff:
	case OPT_gxcoff_:
	  saw_opt_g = true;
	  break;

	case OPT_static:
	  static_link = 1;
	  break;

	case OPT_static_libgcc:
	  shared_libgcc = 0;
	  break;

	case OPT_static_libgo:
	  library = library >= 0 ? 2 : library;
	  args[i] |= SKIPOPT;
	  break;

	case OPT_SPECIAL_input_file:
	  if (library == 0)
	    library = 1;

	  if (first_go_file == NULL)
	    {
	      int len;

	      len = strlen (arg);
	      if (len > 3 && strcmp (arg + len - 3, ".go") == 0)
		first_go_file = arg;
	    }

	  break;
	}
    }

  /* There's no point adding -shared-libgcc if we don't have a shared
     libgcc.  */
#ifndef ENABLE_SHARED_LIBGCC
  shared_libgcc = 0;
#endif

  /* Make sure to have room for the trailing NULL argument.  */
  num_args = argc + need_math + shared_libgcc + (library > 0) * 5 + 10;
  new_decoded_options = XNEWVEC (struct cl_decoded_option, num_args);

  i = 0;
  j = 0;

  /* Copy the 0th argument, i.e., the name of the program itself.  */
  new_decoded_options[j++] = decoded_options[i++];

#ifdef TARGET_CAN_SPLIT_STACK
  supports_split_stack = 1;
#endif

#ifdef TARGET_CAN_SPLIT_STACK_64BIT
  if (is_m64)
    supports_split_stack = 1;
#endif

  /* If we are linking, pass -fsplit-stack if it is supported.  */
  if ((library >= 0) && supports_split_stack)
    {
      generate_option (OPT_fsplit_stack, NULL, 1, CL_DRIVER,
		       &new_decoded_options[j]);
      j++;
    }

  /* The go1 compiler is going to enable debug info by default.  If we
     don't see any -g options, force -g, so that we invoke the
     assembler with the right debug option.  */
  if (!saw_opt_g)
    {
      generate_option (OPT_g, "1", 0, CL_DRIVER, &new_decoded_options[j]);
      j++;
    }

  /* NOTE: We start at 1 now, not 0.  */
  while (i < argc)
    {
      new_decoded_options[j] = decoded_options[i];

      /* Make sure -lgo is before the math library, since libgo itself
	 uses those math routines.  */
      if (!saw_math && (args[i] & MATHLIB) && library > 0)
	{
	  --j;
	  saw_math = &decoded_options[i];
	}

      if (!saw_thread && (args[i] & THREADLIB) && library > 0)
	{
	  --j;
	  saw_thread = &decoded_options[i];
	}

      if (!saw_libc && (args[i] & WITHLIBC) && library > 0)
	{
	  --j;
	  saw_libc = &decoded_options[i];
	}

      if ((args[i] & SKIPOPT) != 0)
	--j;

      i++;
      j++;
    }

  /* If we didn't see a -o option, add one.  This is because we need
     the driver to pass all .go files to go1.  Without a -o option the
     driver will invoke go1 separately for each input file.  FIXME:
     This should probably use some other interface to force the driver
     to set combine_inputs.  */
  if (first_go_file != NULL && !saw_opt_o)
    {
      if (saw_opt_c || saw_opt_S)
	{
	  const char *base;
	  int baselen;
	  int alen;
	  char *out;

	  base = lbasename (first_go_file);
	  baselen = strlen (base) - 3;
	  alen = baselen + 3;
	  out = XNEWVEC (char, alen);
	  memcpy (out, base, baselen);
	  /* The driver will convert .o to some other suffix (e.g.,
	     .obj) if appropriate.  */
	  out[baselen] = '.';
	  if (saw_opt_S)
	    out[baselen + 1] = 's';
	  else
	    out[baselen + 1] = 'o';
	  out[baselen + 2] = '\0';
	  generate_option (OPT_o, out, 1, CL_DRIVER,
			   &new_decoded_options[j]);
	}
      else
	generate_option (OPT_o, "a.out", 1, CL_DRIVER,
			 &new_decoded_options[j]);
      j++;
    }

  /* Add `-lgo' if we haven't already done so.  */
  if (library > 0)
    {
      generate_option (OPT_l, LIBGOBEGIN, 1, CL_DRIVER,
		       &new_decoded_options[j]);
      added_libraries++;
      j++;

#ifdef HAVE_LD_STATIC_DYNAMIC
      if (library > 1 && !static_link)
	{
	  generate_option (OPT_Wl_, LD_STATIC_OPTION, 1, CL_DRIVER,
			   &new_decoded_options[j]);
	  j++;
	}
#endif

      generate_option (OPT_l, saw_profile_flag ? LIBGO_PROFILE : LIBGO, 1,
		       CL_DRIVER, &new_decoded_options[j]);
      added_libraries++;
      j++;

#ifdef HAVE_LD_STATIC_DYNAMIC
      if (library > 1 && !static_link)
	{
	  generate_option (OPT_Wl_, LD_DYNAMIC_OPTION, 1, CL_DRIVER,
			   &new_decoded_options[j]);
	  j++;
	}
#endif

      /* When linking libgo statically we also need to link with the
	 pthread library.  */
      if (library > 1 || static_link)
	need_thread = 1;
    }

  if (saw_thread)
    new_decoded_options[j++] = *saw_thread;
  else if (library > 0 && need_thread)
    {
      generate_option (OPT_l,
		       (saw_profile_flag
			? THREAD_LIBRARY_PROFILE
			: THREAD_LIBRARY),
		       1, CL_DRIVER, &new_decoded_options[j]);
      added_libraries++;
      j++;
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

  if (saw_libc)
    new_decoded_options[j++] = *saw_libc;
  if (shared_libgcc && !static_link)
    generate_option (OPT_shared_libgcc, NULL, 1, CL_DRIVER,
		     &new_decoded_options[j++]);

  /* libgcc wraps pthread_create to support split stack, however, due to
     relative ordering of -lpthread and -lgcc, we can't just mark
     __real_pthread_create in libgcc as non-weak.  But we need to link in
     pthread_create from pthread if we are statically linking, so we work-
     around by passing -u pthread_create to the linker. */
  if (static_link && supports_split_stack)
    {
      generate_option (OPT_Wl_, "-u,pthread_create", 1, CL_DRIVER,
		       &new_decoded_options[j]);
      j++;
    }

#if defined(TARGET_SOLARIS) && !defined(USE_GLD)
  /* We use a common symbol for go$zerovalue.  On Solaris, when not
     using the GNU linker, the Solaris linker needs an option to not
     warn about this.  Everything works without this option, but you
     get unsightly warnings at link time.  */
  generate_option (OPT_Wl_, "-t", 1, CL_DRIVER, &new_decoded_options[j]);
  j++;
#endif

  *in_decoded_options_count = j;
  *in_decoded_options = new_decoded_options;
  *in_added_libraries = added_libraries;
}

/* Called before linking.  Returns 0 on success and -1 on failure.  */
int lang_specific_pre_link (void)  /* Not used for Go.  */
{
  return 0;
}

/* Number of extra output files that lang_specific_pre_link may generate.  */
int lang_specific_extra_outfiles = 0;  /* Not used for Go.  */

/* lang_register_spec_functions.  Not used for Go.  */
void
lang_register_spec_functions (void)
{
}
