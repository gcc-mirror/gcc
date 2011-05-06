/* Definitions for option handling for Motorola 680x0/ColdFire.
   Copyright (C) 1987, 1988, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

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

#ifndef M68K_OPTS_H
#define M68K_OPTS_H

/* Values used in the MICROARCH argument to M68K_DEVICE.  */
enum uarch_type
{
#define M68K_MICROARCH(NAME,DEVICE,MICROARCH,ISA,FLAGS) \
  u##MICROARCH,
#include "m68k-microarchs.def"
#undef M68K_MICROARCH
  ucfv5,
  unk_arch
};

/* An enumeration of all supported target devices.  */
enum target_device
{
#define M68K_DEVICE(NAME,ENUM_VALUE,FAMILY,MULTILIB,MICROARCH,ISA,FLAGS) \
  ENUM_VALUE,
#include "m68k-devices.def"
#undef M68K_DEVICE
  unk_device
};

#endif
