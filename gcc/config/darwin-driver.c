/* Additional functions for the GCC driver on Darwin native.
   Copyright (C) 2006-2013 Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

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

#ifndef CROSS_DIRECTORY_STRUCTURE
#include <sys/sysctl.h>
#include "xregex.h"

static bool
darwin_find_version_from_kernel (char *new_flag)
{
  char osversion[32];
  size_t osversion_len = sizeof (osversion) - 1;
  static int osversion_name[2] = { CTL_KERN, KERN_OSRELEASE };
  int major_vers;
  char minor_vers[6];
  char * version_p;
  char * version_pend;

  /* Determine the version of the running OS.  If we can't, warn user,
     and do nothing.  */
  if (sysctl (osversion_name, ARRAY_SIZE (osversion_name), osversion,
	      &osversion_len, NULL, 0) == -1)
    {
      warning (0, "sysctl for kern.osversion failed: %m");
      return false;
    }

  /* Try to parse the first two parts of the OS version number.  Warn
     user and return if it doesn't make sense.  */
  if (! ISDIGIT (osversion[0]))
    goto parse_failed;
  major_vers = osversion[0] - '0';
  version_p = osversion + 1;
  if (ISDIGIT (*version_p))
    major_vers = major_vers * 10 + (*version_p++ - '0');
  if (major_vers > 4 + 9)
    goto parse_failed;
  if (*version_p++ != '.')
    goto parse_failed;
  version_pend = strchr(version_p, '.');
  if (!version_pend)
    goto parse_failed;
  if (! ISDIGIT (*version_p))
    goto parse_failed;
  strncpy(minor_vers, version_p, version_pend - version_p);
  minor_vers[version_pend - version_p] = '\0';
  
  /* The major kernel version number is 4 plus the second OS version
     component.  */
  if (major_vers - 4 <= 4)
    /* On 10.4 and earlier, the old linker is used which does not
       support three-component system versions.  */
    sprintf (new_flag, "10.%d", major_vers - 4);
  else
    sprintf (new_flag, "10.%d.%s", major_vers - 4,
	     minor_vers);

  return true;

 parse_failed:
  warning (0, "couldn%'t understand kern.osversion %q.*s",
	   (int) osversion_len, osversion);
  return false;
}

#endif

/* When running on a Darwin system and using that system's headers and
   libraries, default the -mmacosx-version-min flag to be the version
   of the system on which the compiler is running.  
   
   When building cross or native cross compilers, default to the OSX
   version of the target (as provided by the most specific target header
   included in tm.h).  This may be overidden by setting the flag explicitly
   (or by the MACOSX_DEPLOYMENT_TARGET environment).  */

static void
darwin_default_min_version (unsigned int *decoded_options_count,
			    struct cl_decoded_option **decoded_options)
{
  const unsigned int argc = *decoded_options_count;
  struct cl_decoded_option *const argv = *decoded_options;
  unsigned int i;
  static char new_flag[sizeof ("10.0.0") + 6];

  /* If the command-line is empty, just return.  */
  if (argc <= 1)
    return;
  
  /* Don't do this if the user specified -mmacosx-version-min= or
     -mno-macosx-version-min.  */
  for (i = 1; i < argc; i++)
    if (argv[i].opt_index == OPT_mmacosx_version_min_)
      return;

  /* Retrieve the deployment target from the environment and insert
     it as a flag.  */
  {
    const char * macosx_deployment_target;
    macosx_deployment_target = getenv ("MACOSX_DEPLOYMENT_TARGET");
    if (macosx_deployment_target
	/* Apparently, an empty string for MACOSX_DEPLOYMENT_TARGET means
	   "use the default".  Or, possibly "use 10.1".  We choose
	   to ignore the environment variable, as if it was never set.  */
	&& macosx_deployment_target[0])
      {
	++*decoded_options_count;
	*decoded_options = XNEWVEC (struct cl_decoded_option,
				    *decoded_options_count);
	(*decoded_options)[0] = argv[0];
	generate_option (OPT_mmacosx_version_min_, macosx_deployment_target,
			 1, CL_DRIVER, &(*decoded_options)[1]);
	memcpy (*decoded_options + 2, argv + 1,
		(argc - 1) * sizeof (struct cl_decoded_option));
	return;
      }
  }

#ifndef CROSS_DIRECTORY_STRUCTURE

 /* Try to find the version from the kernel, if we fail - we print a message 
    and give up.  */
 if (!darwin_find_version_from_kernel (new_flag))
   return;

#else

 /* For cross-compilers, default to the target OS version. */

 strncpy (new_flag, DEF_MIN_OSX_VERSION, sizeof (new_flag));

#endif /* CROSS_DIRECTORY_STRUCTURE */

  /* Add the new flag.  */
  ++*decoded_options_count;
  *decoded_options = XNEWVEC (struct cl_decoded_option,
			      *decoded_options_count);
  (*decoded_options)[0] = argv[0];
  generate_option (OPT_mmacosx_version_min_, new_flag,
		   1, CL_DRIVER, &(*decoded_options)[1]);
  memcpy (*decoded_options + 2, argv + 1,
	  (argc - 1) * sizeof (struct cl_decoded_option));
  return;
  
}

/* Translate -filelist and -framework options in *DECODED_OPTIONS
   (size *DECODED_OPTIONS_COUNT) to use -Xlinker so that they are
   considered to be linker inputs in the case that no other inputs are
   specified.  Handling these options in DRIVER_SELF_SPECS does not
   suffice because specs are too late to add linker inputs, and
   handling them in LINK_SPEC does not suffice because the linker will
   not be called if there are no other inputs.  When native, also
   default the -mmacosx-version-min flag.  */

void
darwin_driver_init (unsigned int *decoded_options_count,
		    struct cl_decoded_option **decoded_options)
{
  unsigned int i;

  for (i = 1; i < *decoded_options_count; i++)
    {
      if ((*decoded_options)[i].errors & CL_ERR_MISSING_ARG)
	continue;
      switch ((*decoded_options)[i].opt_index)
	{
#if DARWIN_X86
	case OPT_arch:
	  if (!strcmp ((*decoded_options)[i].arg, "i386"))
	    generate_option (OPT_m32, NULL, 1, CL_DRIVER, &(*decoded_options)[i]);
	  else if (!strcmp ((*decoded_options)[i].arg, "x86_64"))
	    generate_option (OPT_m64, NULL, 1, CL_DRIVER, &(*decoded_options)[i]);
	  break;
#endif

	case OPT_filelist:
	case OPT_framework:
	  ++*decoded_options_count;
	  *decoded_options = XRESIZEVEC (struct cl_decoded_option,
					 *decoded_options,
					 *decoded_options_count);
	  memmove (*decoded_options + i + 2,
		   *decoded_options + i + 1,
		   ((*decoded_options_count - i - 2)
		    * sizeof (struct cl_decoded_option)));
	  generate_option (OPT_Xlinker, (*decoded_options)[i].arg, 1,
			   CL_DRIVER, &(*decoded_options)[i + 1]);
	  generate_option (OPT_Xlinker,
			   (*decoded_options)[i].canonical_option[0], 1,
			   CL_DRIVER, &(*decoded_options)[i]);
	  break;

	default:
	  break;
	}
    }

  darwin_default_min_version (decoded_options_count, decoded_options);
}
