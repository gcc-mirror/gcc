/* Additional functions for the GCC driver on Darwin native.
   Copyright (C) 2006-2023 Free Software Foundation, Inc.
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

/* Validate a version string (either given on the command line or, perhaps
   as MACOSX_DEPLOYMENT_TARGET).

   The specs %version-compare() function doesn't accept leading '0' on
   numbers so strip them out.  Do sanity checking here too.

   Return:
     * original string means it was OK and we didn't want to change it.
     * new string means it was OK but we rewrote it to avoid possible format
     problems.
     * NULL means we didn't like what we saw.
*/

static const char *
validate_macosx_version_min (const char *version_str)
{
  size_t version_len;
  unsigned long major, minor = 0, tiny = 0;
  char *end;
  const char *old_version = version_str;
  bool need_rewrite = false;

  version_len = strlen (version_str);
  if (version_len < 2) /* The minimum would be 11  */
    return NULL;

  /* Version string must consist of digits and periods only.  */
  if (strspn (version_str, "0123456789.") != version_len)
    return NULL;

  if (!ISDIGIT (version_str[0]) || !ISDIGIT (version_str[version_len - 1]))
    return NULL;

  if (version_str[0] == '0')
    need_rewrite = true;

  major = strtoul (version_str, &end, 10);

  /* macOS 10, 11, and 12 are known. clang accepts up to 99.  */
  if (major < 10 || major > 99)
    return NULL;

  /* Skip a separating period, if there's one.  */
  version_str = end + ((*end == '.') ? 1 : 0);

  if (major > 10 && *end != '\0' && !ISDIGIT (version_str[0]))
     /* For macOS 11+, we allow just the major number, but if the minor is
	there it must be numeric.  */
    return NULL;
  else if (major > 10 && *end == '\0')
    /* We will rewrite 11 =>  11.0.0.  */
    need_rewrite = true;
  else if (major == 10 && (*end == '\0' || !ISDIGIT (version_str[0])))
    /* Otherwise, minor version components must be present and numeric.  */
    return NULL;

  /* If we have one or more leading zeros on a component, then rewrite the
     version string.  */
  if (*end != '\0' && version_str[0] == '0' && version_str[1] != '\0'
      && version_str[1] != '.')
    need_rewrite = true;

  minor = strtoul (version_str, &end, 10);
  version_str = end + ((*end == '.') ? 1 : 0);
  if (minor > 99)
    return NULL;

  /* If 'tiny' is present it must be numeric.  */
  if (*end != '\0' && !ISDIGIT (version_str[0]))
    return NULL;

  /* If we have one or more leading zeros on a component, then rewrite the
     version string.  */
  if (*end != '\0' && version_str[0] == '0'
      && version_str[1] != '\0')
    need_rewrite = true;

  tiny = strtoul (version_str, &end, 10);
  if (tiny > 99)
    return NULL;

  /* Version string must contain no more than three tokens.  */
  if (*end != '\0')
    return NULL;

  if (need_rewrite)
    {
      char *new_version;
      asprintf (&new_version, "%2lu.%lu.%lu", major, minor, tiny);
      return new_version;
    }

  return old_version;
}

#ifndef CROSS_DIRECTORY_STRUCTURE
#include <sys/sysctl.h>
#include "xregex.h"

/* Determine the version of the running OS.
   We only look at the first two components (ignoring the patch one) and
   report NN.MM.0 where NN is currently either 10 or 11 and MM is the OS
   minor release number.
   If we can't parse what the kernel gives us, warn the user, and do nothing.  */

static char *
darwin_find_version_from_kernel (void)
{
  char osversion[32];
  size_t osversion_len = sizeof (osversion) - 1;
  static int osversion_name[2] = { CTL_KERN, KERN_OSRELEASE };
  int major_vers;
  char * version_p;
  char * new_flag;

  if (sysctl (osversion_name, ARRAY_SIZE (osversion_name), osversion,
	      &osversion_len, NULL, 0) == -1)
    {
      warning (0, "%<sysctl%> for %<kern.osversion%> failed: %m");
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

  /* Darwin20 sees a transition to macOS 11.  In this, it seems that the
     mapping to macOS minor version and patch level is now always 0, 0
     (at least for macOS 11 and 12).  */
  if (major_vers >= 20)
    {
      /* Apple clang doesn't include the minor version or the patch level
	 in the object file, nor does it pass it to ld  */
      asprintf (&new_flag, "%d.00.00", major_vers - 9);
    }
  else if (major_vers - 4 <= 4)
    /* On 10.4 and earlier, the old linker is used which does not
       support three-component system versions.
       FIXME: we should not assume this - a newer linker could be used.  */
    asprintf (&new_flag, "10.%d", major_vers - 4);
  else
    /* Although the newer linker supports three-component system
       versions, there's no guarantee that the minor version component
       of the kernel and the system are the same. Apple's clang always
       uses 0 as the minor version: do the same.  */
    asprintf (&new_flag, "10.%d.0", major_vers - 4);

  return new_flag;

 parse_failed:
  warning (0, "could not understand %<kern.osversion%> %q.*s",
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
      const char *checked = validate_macosx_version_min (new_flag);
      if (checked == NULL)
	{
	  warning (0, "could not understand version %qs", new_flag);
	  return NULL;
	}
      new_flag = xstrndup (checked, strlen (checked));
    }
  return new_flag;
}

/* See if we can find the sysroot from the SDKROOT environment variable.  */

static const char *
maybe_get_sysroot_from_sdkroot ()
{
  const char *maybe_sysroot = getenv ("SDKROOT");

  /* We'll use the same rules as the clang driver, for compatibility.
     1) The path must be absolute
     2) Ignore "/", that is the default anyway and we do not want the
	sysroot semantics to be applied to it.
     3) It must exist (actually, we'll check it's readable too).  */

   if (maybe_sysroot  == NULL
       || *maybe_sysroot != '/'
       || strlen (maybe_sysroot) == 1
       || access (maybe_sysroot, R_OK) == -1)
    return NULL;

  return xstrndup (maybe_sysroot, strlen (maybe_sysroot));
}

/* Handle the deduction of m32/m64 from -arch flags and the interactions
   between them (i.e. try to warn a user who thinks that they have a driver
   that can produce multi-slice "FAT" outputs with more than one arch).
   Default the -mmacosx-version-min flag, which requires a system call on
   native hosts.  */

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
  bool seen_sysroot_p = false;
  bool noexport_p = true;

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
	    error ("this compiler does not support %qs",
		   (*decoded_options)[i].arg);
	  /* Now we've examined it, drop the -arch arg.  */
	  if (*decoded_options_count > i) {
	    memmove (*decoded_options + i,
		     *decoded_options + i + 1,
		     ((*decoded_options_count - i - 1)
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

	case OPT_mmacosx_version_min_:
	  seen_version_min = true;
	  vers_string =
	    validate_macosx_version_min ((*decoded_options)[i].arg);
	  if (vers_string == NULL)
	    warning (0, "%qs is not valid for %<-mmacosx-version-min%>",
		     (*decoded_options)[i].arg);
	  else if (vers_string == (*decoded_options)[i].arg)
	    vers_string = xstrndup ((*decoded_options)[i].arg, 32);
	  /* Now we've examined it, and verified/re-written, put it to
	     one side and append later.  */
	  if (*decoded_options_count > i) {
	    memmove (*decoded_options + i,
		     *decoded_options + i + 1,
		     ((*decoded_options_count - i - 1)
		      * sizeof (struct cl_decoded_option)));
	  }
	  --i;
	  --*decoded_options_count;
	  break;

	case OPT__sysroot_:
	case OPT_isysroot:
	  seen_sysroot_p = true;
	  break;

	case OPT_Xlinker:
	case OPT_Wl_:
	  gcc_checking_assert ((*decoded_options)[i].arg);
	  if (startswith ((*decoded_options)[i].arg, "-exported_symbol"))
	    noexport_p = false;
	  break;

	default:
	  break;
	}
    }

  /* Turn -arch xxxx into the appropriate -m32/-m64 flag.
     If the User tried to specify multiple arch flags (which is possible with
     some Darwin compilers) warn that this mode is not supported by this
     compiler.  We take arch specifiers that agree with the default multilib
     as the first choice and reject others.  */
  /* TODO: determine if these warnings would better be errors.  */
#if DARWIN_X86
  if (seenPPC || seenPPC64)
    warning (0, "this compiler does not support PowerPC"
		" (%<-arch%> option ignored)");
  if (seenX86)
    {
      if (seenX86_64 || seenM64)
	{
	  const char *op = (seenX86_64? "-arch x86_64": "-m64");
	  warning (0, "%qs conflicts with %<-arch i386%> (%qs ignored)",
		   op, op);
	}
      if (! seenM32) /* Add -m32 if the User didn't. */
	appendM32 = true;
    }
  else if (seenX86_64)
    {
      if (seenM32)
	warning (0, "%<-m32%> conflicts with %<-arch x86_64%>"
		    " (%<-m32%> ignored)");
      if (! seenM64) /* Add -m64 if the User didn't. */
	appendM64 = true;
    }  
#elif DARWIN_PPC
  if (seenX86 || seenX86_64)
    warning (0, "this compiler does not support x86"
		" (%<-arch%> option ignored)");
  if (seenPPC)
    {
      if (seenPPC64 || seenM64)
	{
	  const char *op = (seenPPC64? "-arch ppc64": "-m64");
	  warning (0, "%qs conflicts with %<-arch ppc%> (%qs ignored)",
		   op, op);
	}
      if (! seenM32) /* Add -m32 if the User didn't. */
	appendM32 = true;
    }
  else if (seenPPC64)
    {
      if (seenM32)
	warning (0, "%<-m32%> conflicts with %<-arch ppc64%>"
		    " (%<-m32%> ignored)");
      if (! seenM64) /* Add -m64 if the User didn't. */
	appendM64 = true;
    }
#endif

  /* If there is nothing else on the command line, do not add sysroot etc.  */
  if (*decoded_options_count <= 1)
    return;

  if (appendM32 || appendM64)
    {
      ++*decoded_options_count;
      *decoded_options = XRESIZEVEC (struct cl_decoded_option,
				     *decoded_options,
				     *decoded_options_count);
      generate_option (appendM32 ? OPT_m32 : OPT_m64, NULL, 1, CL_DRIVER,
		       &(*decoded_options)[*decoded_options_count - 1]);
    }

  if (!seen_sysroot_p)
    {
      /* We will pick up an SDKROOT if we didn't specify a sysroot and treat
	 it as overriding any configure-time --with-sysroot.  */
       const char *sdkroot = maybe_get_sysroot_from_sdkroot ();
       if (sdkroot)
	{
	  ++*decoded_options_count;
	  *decoded_options = XRESIZEVEC (struct cl_decoded_option,
					 *decoded_options,
					 *decoded_options_count);
	  generate_option (OPT__sysroot_, sdkroot, 1, CL_DRIVER,
			   &(*decoded_options)[*decoded_options_count - 1]);
	}
    }

  /* We will need to know the OS X version we're trying to build for here
     so that we can figure out the mechanism and source for the sysroot to
     be used.  */
  if (!seen_version_min)
    /* Not set by the User, try to figure it out.  */
    vers_string = darwin_default_min_version ();

  /* Create and push a cleaned up version, plus the major version for
     assemblers and other cases that need it.  */
  if (vers_string != NULL)
    {
       ++*decoded_options_count;
       *decoded_options = XRESIZEVEC (struct cl_decoded_option,
				      *decoded_options,
				      *decoded_options_count);
      generate_option (OPT_mmacosx_version_min_, vers_string, 1, CL_DRIVER,
		       &(*decoded_options)[*decoded_options_count - 1]);

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

  if (noexport_p)
    {
      ++*decoded_options_count;
      *decoded_options = XRESIZEVEC (struct cl_decoded_option,
				     *decoded_options,
				     *decoded_options_count);
      generate_option (OPT_nodefaultexport, NULL, 1, CL_DRIVER,
		       &(*decoded_options)[*decoded_options_count - 1]);
    }
}
