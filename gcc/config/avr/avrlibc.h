/* Definitions of target machine for the GNU compiler collection
   for Atmel AVR micro controller if configured for AVR-Libc.
   Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

/* AVR-Libc implements functions from libgcc.a in libm.a, see PR54461.  */

#undef  LIBGCC_SPEC
#define LIBGCC_SPEC                                                     \
  "%{!mmcu=at90s1*:%{!mmcu=attiny11:%{!mmcu=attiny12:%{!mmcu=attiny15:%{!mmcu=attiny28: -lgcc -lm }}}}}"

#undef  LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC \
  "--start-group %G %L --end-group"
