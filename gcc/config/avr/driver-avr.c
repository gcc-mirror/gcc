/* Subroutines for the gcc driver.
   Copyright (C) 2009-2016 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "tm.h"

// Remove -nodevicelib from the command line if not needed
#define X_NODEVLIB "%<nodevicelib"

static const char dir_separator_str[] = { DIR_SEPARATOR, 0 };

static const char specfiles_doc_url[] =
  "http://gcc.gnu.org/onlinedocs/gcc/Spec-Files.html";


static const char*
avr_diagnose_devicespecs_error (const char *mcu, const char *filename)
{
  error ("cannot access device-specs for %qs expected at %qs",
         mcu, filename);

  // Inform about natively supported devices and cores.

  if (strncmp (mcu, "avr", strlen ("avr")))
    avr_inform_devices ();

  avr_inform_core_architectures ();

  inform (input_location, "you can provide your own specs files, "
          "see <%s> for details", specfiles_doc_url);

  return X_NODEVLIB;
}


/* Implement spec function `device-specs-file´.

   Compose -specs=<specs-file-name>%s.  If everything went well then argv[0]
   is the inflated (absolute) specs directory and argv[1] is a device or
   core name as supplied by -mmcu=*.  When building GCC the path might
   be relative.  */

const char*
avr_devicespecs_file (int argc, const char **argv)
{
  char *specfile_name;
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
      if (0 == strcmp ("device-specs", argv[0]))
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
        if (0 != strcmp (mmcu, argv[i]))
          {
            error ("specified option %qs more than once", "-mmcu");
            return X_NODEVLIB;
          }

      break;
    }

  specfile_name = concat (argv[0], dir_separator_str, "specs-", mmcu, NULL);

#ifdef DEBUG_SPECS
  if (verbose_flag)
    fnotice (stderr, "'%s': mmcu='%s'\n'%s': specfile='%s'\n\n",
             __FUNCTION__, mmcu, __FUNCTION__, specfile_name);
#endif

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

  if (/* When building / configuring the compiler we might get a relative path
         as supplied by "-B.".  Assume that the specs file exists and MCU is
         a core, not a proper device then, i.e. we have "-mmcu=avr*".  */
      (0 == strncmp (mmcu, "avr", strlen ("avr"))
       && specfile_name[0] == '.')
      /* vanilla */
      || (IS_ABSOLUTE_PATH (specfile_name)
          && !access (specfile_name, R_OK)))
    {
      return concat ("-specs=device-specs", dir_separator_str, "specs-", mmcu,
                     // Use '%s' instead of the expanded specfile_name.  This
                     // is the easiest way to handle pathes containing spaces.
                     "%s",
#if defined (WITH_AVRLIBC)
                     " %{mmcu=avr*:" X_NODEVLIB "} %{!mmcu=*:" X_NODEVLIB "}",
#else
                     " " X_NODEVLIB,
#endif
                     NULL);
    }

  return avr_diagnose_devicespecs_error (mmcu, specfile_name);
}
