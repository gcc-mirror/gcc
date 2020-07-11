/* d-spec.c -- Specific flags and argument handling of the D front end.
   Copyright (C) 2006-2020 Free Software Foundation, Inc.

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
#include "opt-suggestions.h"
#include "gcc.h"
#include "tm.h"
#include "opts.h"

/* This bit is set if the arguments is a D source file.  */
#define DSOURCE		(1<<1)
/* This bit is set if they did `-lstdc++'.  */
#define WITHLIBCXX	(1<<2)
/* Skip this option.  */
#define SKIPOPT		(1<<3)

#ifndef LIBSTDCXX
#define LIBSTDCXX "stdc++"
#endif
#ifndef LIBSTDCXX_PROFILE
#define LIBSTDCXX_PROFILE LIBSTDCXX
#endif

#ifndef LIBPHOBOS
#define LIBPHOBOS "gphobos"
#endif
#ifndef LIBPHOBOS_PROFILE
#define LIBPHOBOS_PROFILE LIBPHOBOS
#endif

/* What do with libgphobos.  */
enum phobos_action
{
  /* libgphobos should not be linked in.  */
  PHOBOS_NOLINK = -1,
  /* libgphobos should be linked in if it is needed.  */
  PHOBOS_DEFAULT = 0,
  /* libgphobos is needed and should be linked in.  */
  PHOBOS_LINK,
  /* libgphobos is needed and should be linked statically.  */
  PHOBOS_STATIC,
  /* libgphobos is needed and should be linked dynamically.  */
  PHOBOS_DYNAMIC
};

static phobos_action phobos_library = PHOBOS_DEFAULT;

/* If true, do load libgphobos.spec even if not needed otherwise.  */
static bool need_spec = false;

void
lang_specific_driver (cl_decoded_option **in_decoded_options,
		      unsigned int *in_decoded_options_count,
		      int *in_added_libraries)
{
  unsigned int i, j;

  /* If nonzero, the user gave us the `-p' or `-pg' flag.  */
  int saw_profile_flag = 0;

  /* If true, the user gave `-g'.  Used by -debuglib.  */
  bool saw_debug_flag = false;

  /* The new argument list will be contained in this.  */
  cl_decoded_option *new_decoded_options;

  /* "-lstdc++" if it appears on the command line.  */
  const cl_decoded_option *saw_libcxx = 0;

  /* Whether we need the C++ STD library.  */
  bool need_stdcxx = false;

  /* True if we saw -static.  */
  bool static_link = false;

  /* True if we should add -shared-libgcc to the command-line.  */
  bool shared_libgcc = true;

  /* What default library to use instead of phobos.  */
  const char *defaultlib = NULL;

  /* What debug library to use instead of phobos.  */
  const char *debuglib = NULL;

  /* The total number of arguments with the new stuff.  */
  unsigned int num_args = 1;

  /* "-fonly" if it appears on the command line.  */
  const char *only_source_option = 0;

  /* Whether the -o option was used.  */
  bool saw_opt_o = false;

  /* Whether the -c option was used.  Also used for -E, -fsyntax-only,
     in general anything which implies only compilation and not linking.  */
  bool saw_opt_c = false;

  /* Whether the -S option was used.  */
  bool saw_opt_S = false;

  /* The first input file with an extension of .d.  */
  const char *first_d_file = NULL;

  /* The total number of arguments with the new stuff.  */
  unsigned int argc = *in_decoded_options_count;

  /* The argument list.  */
  cl_decoded_option *decoded_options = *in_decoded_options;

  /* The number of libraries added in.  */
  int added_libraries = *in_added_libraries;

  /* An array used to flag each argument that needs a bit set for
     DSOURCE, MATHLIB, WITHTHREAD, WITHLIBC or WITHLIBCXX.  */
  int *args = XCNEWVEC (int, argc);

  for (i = 1; i < argc; i++)
    {
      const char *arg = decoded_options[i].arg;
      const int value = decoded_options[i].value;

      switch (decoded_options[i].opt_index)
	{
	case OPT_dstartfiles:
	  need_spec = true;
	  break;

	case OPT_nostdlib:
	case OPT_nodefaultlibs:
	  phobos_library = PHOBOS_NOLINK;
	  break;

	case OPT_nophoboslib:
	  phobos_library = PHOBOS_NOLINK;
	  args[i] |= SKIPOPT;
	  break;

	case OPT_fdruntime:
	  if (!value)
	    phobos_library = PHOBOS_NOLINK;
	  else
	    phobos_library = PHOBOS_LINK;
	  break;

	case OPT_defaultlib_:
	  if (defaultlib != NULL)
	    free (CONST_CAST (char *, defaultlib));
	  if (arg != NULL)
	    {
	      args[i] |= SKIPOPT;
	      defaultlib = XNEWVEC (char, strlen (arg));
	      strcpy (CONST_CAST (char *, defaultlib), arg);
	    }
	  break;

	case OPT_debuglib_:
	  if (debuglib != NULL)
	    free (CONST_CAST (char *, debuglib));
	  if (arg != NULL)
	    {
	      args[i] |= SKIPOPT;
	      debuglib = XNEWVEC (char, strlen (arg));
	      strcpy (CONST_CAST (char *, debuglib), arg);
	    }
	  break;

	case OPT_l:
	  if ((strcmp (arg, LIBSTDCXX) == 0)
	      || (strcmp (arg, LIBSTDCXX_PROFILE) == 0))
	    {
	      args[i] |= WITHLIBCXX;
	      need_stdcxx = false;
	    }
	  /* Unrecognized libraries (e.g. -ltango) may require libphobos.  */
	  else if (phobos_library == PHOBOS_DEFAULT)
	    phobos_library = PHOBOS_LINK;
	  break;

	case OPT_pg:
	case OPT_p:
	  saw_profile_flag++;
	  break;

	case OPT_g:
	  saw_debug_flag = true;
	  break;

	case OPT_v:
	  /* If they only gave us `-v', don't try to link in libphobos.  */
	  if (argc == 2)
	    phobos_library = PHOBOS_NOLINK;
	  break;

	case OPT_x:
	  if (phobos_library == PHOBOS_DEFAULT && (strcmp (arg, "d") == 0))
	    phobos_library = PHOBOS_LINK;
	  break;

	case OPT_Xlinker:
	case OPT_Wl_:
	  /* Arguments that go directly to the linker might be .o files
	     or something, and so might cause libphobos to be needed.  */
	  if (phobos_library == PHOBOS_DEFAULT)
	    phobos_library = PHOBOS_LINK;
	  break;

	case OPT_c:
	case OPT_E:
	case OPT_M:
	case OPT_MM:
	case OPT_fsyntax_only:
	  /* Don't specify libaries if we won't link, since that would
	     cause a warning.  */
	  saw_opt_c = true;
	  phobos_library = PHOBOS_NOLINK;
	  break;

	case OPT_S:
	  saw_opt_S = true;
	  phobos_library = PHOBOS_NOLINK;
	  break;

	case OPT_o:
	  saw_opt_o = true;
	  break;

	case OPT_static:
	  static_link = true;
	  break;

	case OPT_static_libgcc:
	  shared_libgcc = false;
	  break;

	case OPT_static_libphobos:
	  if (phobos_library != PHOBOS_NOLINK)
	    phobos_library = PHOBOS_STATIC;
	  args[i] |= SKIPOPT;
	  break;

	case OPT_shared_libphobos:
	  if (phobos_library != PHOBOS_NOLINK)
	    phobos_library = PHOBOS_DYNAMIC;
	  args[i] |= SKIPOPT;
	  break;

	case OPT_fonly_:
	  args[i] |= SKIPOPT;
	  only_source_option = decoded_options[i].orig_option_with_args_text;

	  if (arg != NULL)
	    {
	      const char *suffix = strrchr (only_source_option, '.');
	      if (suffix == NULL || strcmp (suffix, ".d") != 0)
		only_source_option = concat (only_source_option, ".d", NULL);
	    }
	  break;

	case OPT_SPECIAL_input_file:
	  {
	    if (arg[0] == '\0' || arg[1] == '\0')
	      continue;

	    if (phobos_library == PHOBOS_DEFAULT)
	      phobos_library = PHOBOS_LINK;

	    /* Record that this is a D source file.  */
	    const char *suffix = strrchr (arg, '.');
	    if (suffix != NULL && strcmp (suffix, ".d") == 0)
	      {
		if (first_d_file == NULL)
		  first_d_file = arg;

		args[i] |= DSOURCE;
	      }

	    /* If this is a C++ source file, we'll need to link
	       against libstdc++ library.  */
	    if (suffix != NULL
		&& (strcmp (suffix, ".cc") == 0
		    || (strcmp (suffix, ".cpp") == 0)
		    || (strcmp (suffix, ".c++") == 0)))
	      need_stdcxx = true;

	    break;
	  }
	}
    }

  /* There's no point adding -shared-libgcc if we don't have a shared
     libgcc.  */
#ifndef ENABLE_SHARED_LIBGCC
  shared_libgcc = false;
#endif

  /* Make sure to have room for the trailing NULL argument.
     - need_stdcxx might add `-lstdcxx'
     - libphobos adds `-Bstatic -lphobos -Bdynamic'
     - only_source adds 1 more arg, also maybe add `-o'.  */
  num_args = argc + need_stdcxx + shared_libgcc
    + (phobos_library != PHOBOS_NOLINK) * 4 + 2;
  new_decoded_options = XNEWVEC (cl_decoded_option, num_args);

  i = 0;
  j = 0;

  /* Copy the 0th argument, i.e., the name of the program itself.  */
  new_decoded_options[j++] = decoded_options[i++];

  /* NOTE: We start at 1 now, not 0.  */
  while (i < argc)
    {
      if (args[i] & SKIPOPT)
	{
	  ++i;
	  continue;
	}

      new_decoded_options[j] = decoded_options[i];

      if (!saw_libcxx && (args[i] & WITHLIBCXX))
	{
	  --j;
	  saw_libcxx = &decoded_options[i];
	}

      if (args[i] & DSOURCE)
	{
	  if (only_source_option)
	    --j;
	}

      i++;
      j++;
    }

  if (only_source_option)
    {
      const char *only_source_arg = only_source_option + 7;
      generate_option (OPT_fonly_, only_source_arg, 1, CL_DRIVER,
		       &new_decoded_options[j]);
      j++;

      generate_option_input_file (only_source_arg,
				  &new_decoded_options[j++]);
    }

  /* If no reason to link against libphobos library, then don't add it.  */
  if (phobos_library == PHOBOS_DEFAULT)
    phobos_library = PHOBOS_NOLINK;

  /* If we didn't see a -o option, add one.  This is because we need the
     driver to pass all .d files to the D compiler.  Without a -o option
     the driver will invoke the compiler separately for each input file.  */
  if (first_d_file != NULL && !saw_opt_o)
    {
      if (saw_opt_c || saw_opt_S)
	{
	  const char *base = lbasename (first_d_file);
	  int baselen = strlen (base) - 2;
	  char *out = XNEWVEC (char, baselen + 3);

	  memcpy (out, base, baselen);
	  /* The driver will convert .o to some other suffix if appropriate.  */
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
	{
	  /* Wouldn't be necessary if the driver converted .out also.  */
	  const char *out = NULL;

#ifdef TARGET_EXECUTABLE_SUFFIX
	  if (TARGET_EXECUTABLE_SUFFIX[0] != 0)
	    out = "a" TARGET_EXECUTABLE_SUFFIX;
#endif
	  if (out == NULL)
	    out = "a.out";

	  generate_option (OPT_o, out, 1, CL_DRIVER,
			   &new_decoded_options[j]);
	}
      j++;
    }

  /* Add `-lgphobos' if we haven't already done so.  */
  if (phobos_library != PHOBOS_NOLINK)
    {
      /* Default to static linking.  */
      if (phobos_library != PHOBOS_DYNAMIC)
	phobos_library = PHOBOS_STATIC;

#ifdef HAVE_LD_STATIC_DYNAMIC
      if (phobos_library == PHOBOS_STATIC && !static_link)
	{
	  generate_option (OPT_Wl_, LD_STATIC_OPTION, 1, CL_DRIVER,
			   &new_decoded_options[j++]);
	}
#endif
      /* Order of precedence in determining what library to link against is:
	 - `-l<lib>' from `-debuglib=<lib>' if `-g' was also seen.
	 - `-l<lib>' from `-defaultlib=<lib>'.
	 - `-lgphobos' unless `-nophoboslib' or `-fno-druntime' was seen.  */
      if (debuglib && saw_debug_flag)
	{
	  generate_option (OPT_l, debuglib, 1, CL_DRIVER,
			   &new_decoded_options[j++]);
	  added_libraries++;
	}
      else if (defaultlib)
	{
	  generate_option (OPT_l, defaultlib, 1, CL_DRIVER,
			   &new_decoded_options[j++]);
	  added_libraries++;
	}
      else
	{
	  generate_option (OPT_l,
			   saw_profile_flag ? LIBPHOBOS_PROFILE : LIBPHOBOS, 1,
			   CL_DRIVER, &new_decoded_options[j++]);
	  added_libraries++;
	}

#ifdef HAVE_LD_STATIC_DYNAMIC
      if (phobos_library == PHOBOS_STATIC && !static_link)
	{
	  generate_option (OPT_Wl_, LD_DYNAMIC_OPTION, 1, CL_DRIVER,
			   &new_decoded_options[j++]);
	}
#endif
    }

  if (saw_libcxx)
    new_decoded_options[j++] = *saw_libcxx;
  else if (need_stdcxx)
    {
      generate_option (OPT_l,
		       (saw_profile_flag
			? LIBSTDCXX_PROFILE
			: LIBSTDCXX),
		       1, CL_DRIVER, &new_decoded_options[j++]);
      added_libraries++;
    }

  if (shared_libgcc && !static_link)
    {
      generate_option (OPT_shared_libgcc, NULL, 1, CL_DRIVER,
		       &new_decoded_options[j++]);
    }

  *in_decoded_options_count = j;
  *in_decoded_options = new_decoded_options;
  *in_added_libraries = added_libraries;
}

/* Called before linking.  Returns 0 on success and -1 on failure.  */

int
lang_specific_pre_link (void)
{
  if ((phobos_library != PHOBOS_NOLINK) || need_spec)
    do_spec ("%:include(libgphobos.spec)");

  return 0;
}

/* Number of extra output files that lang_specific_pre_link may generate.  */

int lang_specific_extra_outfiles = 0;  /* Not used for D.  */

