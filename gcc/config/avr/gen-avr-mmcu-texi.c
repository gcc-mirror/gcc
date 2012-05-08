/* Copyright (C) 2012
   Free Software Foundation, Inc.
   Contributed by Georg-Johann Lay (avr@gjlay.de)

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

#include "avr-devices.c"

int main (void)
{
  enum avr_arch arch = 0;
  unsigned i, first = 1;
  const struct mcu_type_s *mcu;

  printf ("@c Copyright (C) 2012 Free Software Foundation, Inc.\n");
  printf ("@c This is part of the GCC manual.\n");
  printf ("@c For copying conditions, see the file "
          "gcc/doc/include/fdl.texi.\n\n");

  printf ("@c This file is generated automatically using\n");
  printf ("@c gcc/config/avr/gen-avr-mmcu-texi.c from:\n");
  printf ("@c    gcc/config/avr/avr-devices.c\n");
  printf ("@c    gcc/config/avr/avr-mcus.def\n\n");

  printf ("@c Please do not edit manually.\n\n");

  printf ("@table @code\n\n");

  for (mcu = avr_mcu_types; mcu->name; mcu++)
    {
      if (mcu->macro == NULL)
        {
          arch = mcu->arch;

          for (i = 0; i < sizeof (avr_texinfo) / sizeof (*avr_texinfo); i++)
            {
              if (arch == avr_texinfo[i].arch)
                {
                  if (mcu != avr_mcu_types)
                    printf (".\n\n");
                  printf ("@item %s\n%s\n", mcu->name, avr_texinfo[i].texinfo);
                  printf ("@*@var{mcu}@tie{}=");
                  first = 1;
                  break;
                }
            }
        }
      else if (arch == (enum avr_arch) mcu->arch)
        {
          printf ("%s @code{%s}", first ? "" : ",", mcu->name);
          first = 0;
        }
    }

  printf (".\n\n");
  printf ("@end table\n");

  return EXIT_SUCCESS;
}
