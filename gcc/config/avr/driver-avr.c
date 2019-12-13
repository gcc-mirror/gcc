/* Subroutines for the gcc driver.
   Copyright (C) 2009-2019 Free Software Foundation, Inc.
   Contributed by Georg-Johann Lay <avr@gjlay.de>

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "tm.h"

// Remove -nodevicelib from the command line if not needed
#define X_NODEVLIB "%<nodevicelib"

static const char dir_separator_str[] = { DIR_SEPARATOR, 0 };


/* Implement spec function `device-specs-file´.

   Validate mcu name given with -mmcu option. Compose
   -specs=<specs-file-name>%s. If everything went well then argv[0] is the
   inflated (absolute) first device-specs directory and argv[1] is a device
   or core name as supplied by -mmcu=*. When building GCC the path might be
   relative.  */

const char*
avr_devicespecs_file (int argc, const char **argv)
{
  const char *mmcu = NULL;

#ifdef DEBUG_SPECS
  if (verbose_flag)
    fnotice (stderr, "Running spec function '%s' with %d args\n\n",
             __FUNCTION__, argc);
#endif

  switch (argc)
    {
    case 0:
      fatal_error (input_location,
                   "bad usage of spec function %qs", "device-specs-file");
      return X_NODEVLIB;

    case 1:
      if (strcmp ("device-specs", argv[0]) == 0)
        {
          /* FIXME:  This means "device-specs%s" from avr.h:DRIVER_SELF_SPECS
             has not been resolved to a path.  That case can occur when the
             c++ testsuite is run from the build directory.  DejaGNU's
             libgloss.exp:get_multilibs runs $compiler without -B, i.e.runs
             xgcc without specifying a prefix.  Without any prefix, there is
             no means to find out where the specs files might be located.
             get_multilibs runs xgcc --print-multi-lib, hence we don't actually
             need information form a specs file and may skip it here.  */
          return X_NODEVLIB;
        }

      mmcu = AVR_MMCU_DEFAULT;
      break;

    default:
      mmcu = argv[1];

      // Allow specifying the same MCU more than once.

      for (int i = 2; i < argc; i++)
	if (strcmp (mmcu, argv[i]) != 0)
          {
            error ("specified option %qs more than once", "-mmcu");
            return X_NODEVLIB;
          }

      break;
    }

  // Filter out silly -mmcu= arguments like "foo bar".

  for (const char *s = mmcu; *s; s++)
    if (!ISALNUM (*s)
        && '-' != *s
        && '_' != *s)
      {
        error ("strange device name %qs after %qs: bad character %qc",
               mmcu, "-mmcu=", *s);
        return X_NODEVLIB;
      }

  return concat ("-specs=device-specs", dir_separator_str, "specs-",
                 mmcu, "%s"
#if defined (WITH_AVRLIBC)
                 " %{mmcu=avr*:" X_NODEVLIB "} %{!mmcu=*:" X_NODEVLIB "}",
#else
                 " " X_NODEVLIB,
#endif
                 NULL);
}


/* Re-build the -mdouble= and -mlong-double= options.  This is needed
   because these options are not independent of each other.  */

const char*
avr_double_lib (int argc, const char **argv)
{
#if defined (WITH_DOUBLE64)
  int dbl = 64;
#elif defined (WITH_DOUBLE32)
  int dbl = 32;
#else
#error "align this with config.gcc"
#endif

#if defined (WITH_LONG_DOUBLE64)
  int ldb = 64;
#elif defined (WITH_LONG_DOUBLE32)
  int ldb = 32;
#else
#error "align this with config.gcc"
#endif

  for (int i = 0; i < argc; i++)
    {
      if (strcmp (argv[i], "mdouble=32") == 0)
        {
          dbl = 32;
#ifdef HAVE_LONG_DOUBLE_IS_DOUBLE
          ldb = dbl;
#endif
        }
      else if (strcmp (argv[i], "mdouble=64") == 0)
        {
          ldb = dbl = 64;
        }
      else if (strcmp (argv[i], "mlong-double=32") == 0)
        {
          ldb = dbl = 32;
        }
      else if (strcmp (argv[i], "mlong-double=64") == 0)
        {
          ldb = 64;
#ifdef HAVE_LONG_DOUBLE_IS_DOUBLE
          dbl = ldb;
#endif
        }
    }

  return concat (" %<mdouble=* -mdouble=", dbl == 32 ? "32" : "64",
                 " %<mlong-double=* -mlong-double=", ldb == 32 ? "32" : "64",
                 NULL);
}
