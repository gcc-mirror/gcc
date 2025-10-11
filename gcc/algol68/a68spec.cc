/* a68spec.c -- Specific flags and argument handling of the Algol 68 front end.
   Copyright (C) 2025 Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 3, or (at your option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   GCC; see the file COPYING3.  If not see <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "opt-suggestions.h"
#include "gcc.h"
#include "tm.h"
#include "opts.h"

/* satisfy intellisense  */
#include "options.h"

/* How to link with libga68.  */
enum libga68_link_mode
{
  LIBGA68_NOLINK,
  LIBGA68_STATIC,
  LIBGA68_DYNAMIC
};

static enum libga68_link_mode libga68_link = LIBGA68_STATIC;

/* This bit is set if we saw a `-xfoo' language specification.  */
#define LANGSPEC (1 << 1)
/* This bit is set if they did `-lc'.  */
#define WITHLIBC (1 << 2)
/* Skip this option.  */
#define SKIPOPT (1 << 3)

void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
		      unsigned int *in_decoded_options_count,
		      int *in_added_libraries)
{
  unsigned int i, j;

  /* The new argument list will be contained in this.  */
  struct cl_decoded_option *new_decoded_options;

  /* "-lc" if it appears on the command line.  */
  const struct cl_decoded_option *saw_libc = 0;

  /* An array used to flag each argument that needs a bit set for
     LANGSPEC or WITHLIBC.  */
  int *args;

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

  /* Whether the -o option was used.  */
  //  bool saw_opt_o = false;

  argc = *in_decoded_options_count;
  decoded_options = *in_decoded_options;
  added_libraries = *in_added_libraries;

  args = XCNEWVEC (int, argc);

  for (i = 1; i < argc; i++)
    {
      const char *arg = decoded_options[i].arg;

      switch (decoded_options[i].opt_index)
	{
	case OPT__help:
	case OPT__help_:
	  /* Let gcc.cc handle this.  */
	  *in_added_libraries = 0;
	  return;
	case OPT_c:
	case OPT_E:
	case OPT_M:
	case OPT_MM:
	case OPT_fsyntax_only:
	case OPT_S:
	  libga68_link = LIBGA68_NOLINK;
	  break;

	case OPT_l:
	  if (strcmp (arg, "c") == 0)
	    args[i] |= WITHLIBC;
	  break;

	case OPT_o:
	  //saw_opt_o = true;
	  break;

	case OPT_static:
	  static_link = 1;
	  break;

	case OPT_static_libgcc:
	  shared_libgcc = 0;
	  break;

	case OPT_static_libga68:
	  libga68_link = LIBGA68_STATIC;
#ifdef HAVE_LD_STATIC_DYNAMIC
	  /* Remove -static-libga68 from the command only if target supports
	     LD_STATIC_DYNAMIC.  When not supported, it is left in so that a
	     back-end target can use outfile substitution.  */
	  args[i] |= SKIPOPT;
#endif
	  break;

	case OPT_shared_libga68:
	  libga68_link = LIBGA68_DYNAMIC;
	  args[i] |= SKIPOPT;
	  break;

	case OPT_SPECIAL_input_file:
	  break;
	}
    }

    /* There's no point adding -shared-libgcc if we don't have a shared
       libgcc.  */
#ifndef ENABLE_SHARED_LIBGCC
  shared_libgcc = 0;
#endif

  /* Make sure to have room for the trailing NULL argument.
     - libga68 adds `-Bstatic -lga68 -Bdynamic' */
  num_args = argc + shared_libgcc + 1 * 5 + 10;
  new_decoded_options = XNEWVEC (struct cl_decoded_option, num_args);

  i = 0;
  j = 0;

  /* Copy the 0th argument, i.e., the name of the program itself.  */
  new_decoded_options[j++] = decoded_options[i++];

  /* NOTE: We start at 1 now, not 0.  */
  while (i < argc)
    {
      new_decoded_options[j] = decoded_options[i];

      if (!saw_libc && (args[i] & WITHLIBC))
	{
	  --j;
	  saw_libc = &decoded_options[i];
	}

      if ((args[i] & SKIPOPT) != 0)
	--j;

      i++;
      j++;
    }

  if (saw_libc)
    new_decoded_options[j++] = *saw_libc;
  if (shared_libgcc && !static_link)
    generate_option (OPT_shared_libgcc, NULL, 1, CL_DRIVER,
		     &new_decoded_options[j++]);

  /* Add `-lga68 -lm' if we haven't already done so.  */
#ifdef HAVE_LD_STATIC_DYNAMIC
  if (libga68_link == LIBGA68_STATIC && !static_link)
    {
      generate_option (OPT_Wl_, LD_STATIC_OPTION, 1, CL_DRIVER,
		       &new_decoded_options[j++]);
      added_libraries++; /* The driver calls add_infile while handling -Wl */
    }
#endif
  generate_option (OPT_l,
		   "ga68", 1,
		   CL_DRIVER, &new_decoded_options[j++]);
  added_libraries++;
#ifdef HAVE_LD_STATIC_DYNAMIC
  if (libga68_link == LIBGA68_STATIC && !static_link)
    {
      generate_option (OPT_Wl_, LD_DYNAMIC_OPTION, 1, CL_DRIVER,
		       &new_decoded_options[j++]);
      added_libraries++; /* The driver calls add_infile while handling -Wl */
    }
#endif
  *in_decoded_options_count = j;
  *in_decoded_options = new_decoded_options;
  *in_added_libraries = added_libraries;
}

/* Called before linking.  Returns 0 on success and -1 on failure.  */
int
lang_specific_pre_link (void)
{
  if (libga68_link != LIBGA68_NOLINK)
    do_spec ("%:include(libga68.spec)");
  return 0;
}

/* Number of extra output files that lang_specific_pre_link may generate.  */
int lang_specific_extra_outfiles = 0; /* Not used for Algol68.  */
