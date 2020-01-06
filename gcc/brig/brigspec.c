/* brigspec.c -- Specific flags and argument handling of the gcc BRIG front end.
   Copyright (C) 2016-2020 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

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
#include "opt-suggestions.h"
#include "gcc.h"
#include "opts.h"

/* This bit is set if we saw a `-xfoo' language specification.  */
#define LANGSPEC (1 << 1)
/* This bit is set if they did `-lm' or `-lmath'.  */
#define MATHLIB (1 << 2)
/* This bit is set if they did `-lpthread'.  */
#define THREADLIB (1 << 3)
/* This bit is set if they did `-lc'.  */
#define WITHLIBC (1 << 4)
/* Skip this option.  */
#define SKIPOPT (1 << 5)

#ifndef MATH_LIBRARY
#define MATH_LIBRARY "m"
#endif
#ifndef MATH_LIBRARY_PROFILE
#define MATH_LIBRARY_PROFILE MATH_LIBRARY
#endif

#define LIBHSAIL "hsail-rt"

void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
		      unsigned int *in_decoded_options_count,
		      int *in_added_libraries)
{
  unsigned int i, j;

  /* The new argument list will be contained in this.  */
  struct cl_decoded_option *new_decoded_options;

  /* An array used to flag each argument that needs a bit set for
     LANGSPEC, MATHLIB, or WITHLIBC.  */
  int *args;

  /* By default, we throw on the math library if we have one.  */
  int need_math = (MATH_LIBRARY[0] != '\0');

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

  argc = *in_decoded_options_count;
  decoded_options = *in_decoded_options;
  added_libraries = *in_added_libraries;

  args = XCNEWVEC (int, argc);

  for (i = 1; i < argc; i++)
    {
      switch (decoded_options[i].opt_index)
	{
	case OPT_o:
	  break;

	case OPT_SPECIAL_input_file:
	  break;
	}
    }

  /* Make sure to have room for the trailing NULL argument.  */
  num_args = argc + need_math + shared_libgcc + 10;
  new_decoded_options = XNEWVEC (struct cl_decoded_option, num_args);

  i = 0;
  j = 0;

  /* Copy the 0th argument, i.e., the name of the program itself.  */
  new_decoded_options[j++] = decoded_options[i++];

  /* NOTE: We start at 1 now, not 0.  */
  while (i < argc)
    {
      new_decoded_options[j] = decoded_options[i];

      if ((args[i] & SKIPOPT) != 0)
	--j;

      i++;
      j++;
    }

  generate_option (OPT_l, LIBHSAIL, 1, CL_DRIVER, &new_decoded_options[j]);
  j++;

  *in_decoded_options_count = j;
  *in_decoded_options = new_decoded_options;
  *in_added_libraries = added_libraries;
}

/* Called before linking.  Returns 0 on success and -1 on failure.  */

int lang_specific_pre_link (void) /* Not used for Brig.  */ { return 0; }

/* Number of extra output files that lang_specific_pre_link may generate.  */

int lang_specific_extra_outfiles = 0; /* Not used for Brig.  */
