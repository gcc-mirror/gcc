/* Additional functions for the GCC driver on Darwin native.
   Copyright (C) 2006-2016 Free Software Foundation, Inc.
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
#include "libiberty.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "opts.h"
#include "diagnostic-core.h"

#ifndef CROSS_DIRECTORY_STRUCTURE
#include <sys/sysctl.h>
#include "xregex.h"

static char *
darwin_find_version_from_kernel (void)
{
  char osversion[32];
  size_t osversion_len = sizeof (osversion) - 1;
  static int osversion_name[2] = { CTL_KERN, KERN_OSRELEASE };
  int major_vers;
  char minor_vers[6];
  char * version_p;
  char * version_pend;
  char * new_flag;

  /* Determine the version of the running OS.  If we can't, warn user,
     and do nothing.  */
  if (sysctl (osversion_name, ARRAY_SIZE (osversion_name), osversion,
	      &osversion_len, NULL, 0) == -1)
    {
      warning (0, "sysctl for kern.osversion failed: %m");
      return NULL;
    }

  /* Try to parse the first two parts of the OS version number.  Warn
     user and return if it doesn't make sense.  */
  if (! ISDIGIT (osversion[0]))
    goto parse_failed;
  major_vers = osversion[0] - '0';
  version_p = osversion + 1;
  if (ISDIGIT (*version_p))
    major_vers = major_vers * 10 + (*version_p++ - '0');
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
       support three-component system versions.
       FIXME: we should not assume this - a newer linker could be used.  */
    asprintf (&new_flag, "10.%d", major_vers - 4);
  else
    asprintf (&new_flag, "10.%d.%s", major_vers - 4, minor_vers);

  return new_flag;

 parse_failed:
  warning (0, "couldn%'t understand kern.osversion %q.*s",
	   (int) osversion_len, osversion);
  return NULL;
}

#endif

/* When running on a Darwin system and using that system's headers and
   libraries, default the -mmacosx-version-min flag to be the version
   of the system on which the compiler is running.  
   
   When building cross or native cross compilers, default to the OSX
   version of the target (as provided by the most specific target header
   included in tm.h).  This may be overidden by setting the flag explicitly
   (or by the MACOSX_DEPLOYMENT_TARGET environment).  */

static const char *
darwin_default_min_version (void)
{
  /* Try to retrieve the deployment target from the environment.  */
  const char *new_flag = getenv ("MACOSX_DEPLOYMENT_TARGET");

  /* Apparently, an empty string for MACOSX_DEPLOYMENT_TARGET means
     "use the default".  Or, possibly "use 10.1".  We choose
     to ignore the environment variable, as if it was never set.  */
  if (new_flag == NULL || new_flag[0] == 0)
#ifndef CROSS_DIRECTORY_STRUCTURE
    /* Try to find the version from the kernel, if we fail - we print a
       message and give up.  */
    new_flag = darwin_find_version_from_kernel ();
#else
    /* For cross-compilers, default to a minimum version determined by
       the configuration. */
    new_flag = DEF_MIN_OSX_VERSION;
#endif /* CROSS_DIRECTORY_STRUCTURE */

  if (new_flag != NULL)
    {
      size_t len = strlen (new_flag);
      if (len > 128) { /* Arbitrary limit, number should be like xx.yy.zz */
	warning (0, "couldn%'t understand version %s\n", new_flag);
	return NULL;
      }
      new_flag = xstrndup (new_flag, len);
    }
  return new_flag;
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
  bool seenX86 = false;
  bool seenX86_64 = false;
  bool seenPPC = false;
  bool seenPPC64 = false;
  bool seenM32 = false;
  bool seenM64 = false;
  bool appendM32 = false;
  bool appendM64 = false;
  const char *vers_string = NULL;
  bool seen_version_min = false;

  for (i = 1; i < *decoded_options_count; i++)
    {
      if ((*decoded_options)[i].errors & CL_ERR_MISSING_ARG)
	continue;

      switch ((*decoded_options)[i].opt_index)
	{
	case OPT_arch:
	  /* Support provision of a single -arch xxxx flag as a means of
	     specifying the sub-target/multi-lib.  Translate this into -m32/64
	     as appropriate.  */  
	  if (!strcmp ((*decoded_options)[i].arg, "i386"))
	    seenX86 = true;
	  else if (!strcmp ((*decoded_options)[i].arg, "x86_64"))
	    seenX86_64 = true;
	  else if (!strcmp ((*decoded_options)[i].arg, "ppc"))
	    seenPPC = true;
	  else if (!strcmp ((*decoded_options)[i].arg, "ppc64"))
	    seenPPC64 = true;
	  else
	    error ("this compiler does not support %s",
		   (*decoded_options)[i].arg);
	  /* Now we've examined it, drop the -arch arg.  */
	  if (*decoded_options_count > i) {
	    memmove (*decoded_options + i,
		     *decoded_options + i + 1,
		     ((*decoded_options_count - i)
		      * sizeof (struct cl_decoded_option)));
	  }
	  --i;
	  --*decoded_options_count; 
	  break;

	case OPT_m32:
	  seenM32 = true;
	  break;

	case OPT_m64:
	  seenM64 = true;
	  break;

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

	case OPT_mmacosx_version_min_:
	  seen_version_min = true;
	  vers_string = xstrndup ((*decoded_options)[i].arg, 32);

	default:
	  break;
	}
    }

  /* Turn -arch xxxx into the appropriate -m32/-m64 flag.
     If the User tried to specify multiple arch flags (which is possible with
     some Darwin compilers) warn that this mode is not supported by this
     compiler (and ignore the arch flags, which means that the default multi-
     lib will be generated).  */
  /* TODO: determine if these warnings would better be errors.  */
#if DARWIN_X86
  if (seenPPC || seenPPC64)
    warning (0, "this compiler does not support PowerPC (arch flags ignored)");
  if (seenX86)
    {
      if (seenX86_64 || seenM64)
	warning (0, "%s conflicts with i386 (arch flags ignored)",
	        (seenX86_64? "x86_64": "m64"));
      else if (! seenM32) /* Add -m32 if the User didn't. */
	appendM32 = true;
    }
  else if (seenX86_64)
    {
      if (seenX86 || seenM32)
	warning (0, "%s conflicts with x86_64 (arch flags ignored)",
		 (seenX86? "i386": "m32"));
      else if (! seenM64) /* Add -m64 if the User didn't. */
	appendM64 = true;
    }  
#elif DARWIN_PPC
  if (seenX86 || seenX86_64)
    warning (0, "this compiler does not support X86 (arch flags ignored)");
  if (seenPPC)
    {
      if (seenPPC64 || seenM64)
	warning (0, "%s conflicts with ppc (arch flags ignored)",
		 (seenPPC64? "ppc64": "m64"));
      else if (! seenM32) /* Add -m32 if the User didn't. */
	appendM32 = true;
    }
  else if (seenPPC64)
    {
      if (seenPPC || seenM32)
	warning (0, "%s conflicts with ppc64 (arch flags ignored)",
		 (seenPPC? "ppc": "m32"));
      else if (! seenM64) /* Add -m64 if the User didn't. */
	appendM64 = true;
    }
#endif

  if (appendM32 || appendM64)
    {
      ++*decoded_options_count;
      *decoded_options = XRESIZEVEC (struct cl_decoded_option,
				     *decoded_options,
				     *decoded_options_count);
      generate_option (appendM32 ? OPT_m32 : OPT_m64, NULL, 1, CL_DRIVER,
		       &(*decoded_options)[*decoded_options_count - 1]);
    }

  /* We will need to know the OS X version we're trying to build for here
     so that we can figure out the mechanism and source for the sysroot to
     be used.  */
  if (! seen_version_min && *decoded_options_count > 1)
    {
      /* Not set by the User, try to figure it out.  */
      vers_string = darwin_default_min_version ();
      if (vers_string != NULL)
	{
	  ++*decoded_options_count;
	  *decoded_options = XRESIZEVEC (struct cl_decoded_option,
					 *decoded_options,
					 *decoded_options_count);
	  generate_option (OPT_mmacosx_version_min_, vers_string, 1, CL_DRIVER,
			  &(*decoded_options)[*decoded_options_count - 1]);
	}
    }
  /* Create and push the major version for assemblers that need it.  */
  if (vers_string != NULL)
    {
      char *asm_major = NULL;
      const char *first_period = strchr(vers_string, '.');
      if (first_period != NULL)
	{
	  const char *second_period = strchr(first_period+1, '.');
	  if (second_period  != NULL)
	    asm_major = xstrndup (vers_string, second_period-vers_string);
	  else
	    asm_major = xstrdup (vers_string);
        }
      /* Else we appear to have a weird macosx version with no major number.
         Punt on this for now.  */
      if (asm_major != NULL)
        {
	  ++*decoded_options_count;
	  *decoded_options = XRESIZEVEC (struct cl_decoded_option,
					 *decoded_options,
					 *decoded_options_count);
	  generate_option (OPT_asm_macosx_version_min_, asm_major, 1, CL_DRIVER,
			  &(*decoded_options)[*decoded_options_count - 1]);
        }
    }
}
