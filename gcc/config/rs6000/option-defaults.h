/* Definitions of default options for config/rs6000 configurations.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This header needs to be included after any other headers affecting
   TARGET_DEFAULT.  */

#if TARGET_AIX
#define OPT_64 "maix64"
#define OPT_32 "maix32"
#else
#define OPT_64 "m64"
#define OPT_32 "m32"
#endif

#ifndef MASK_64BIT
#define MASK_64BIT 0
#endif

#if TARGET_DEFAULT & MASK_64BIT
#define OPT_ARCH64 "!"OPT_32
#define OPT_ARCH32 OPT_32
#else
#define OPT_ARCH64 OPT_64
#define OPT_ARCH32 "!"OPT_64
#endif

/* Support for a compile-time default CPU, et cetera.  The rules are:
   --with-cpu is ignored if -mcpu is specified; likewise --with-cpu-32
     and --with-cpu-64.
   --with-tune is ignored if -mtune or -mcpu is specified; likewise
     --with-tune-32 and --with-tune-64.
   --with-float is ignored if -mhard-float or -msoft-float are
     specified.  */
#define OPTION_DEFAULT_SPECS \
  {"tune", "%{!mtune=*:%{!mcpu=*:-mtune=%(VALUE)}}" }, \
  {"tune_32", "%{" OPT_ARCH32 ":%{!mtune=*:%{!mcpu=*:-mtune=%(VALUE)}}}" }, \
  {"tune_64", "%{" OPT_ARCH64 ":%{!mtune=*:%{!mcpu=*:-mtune=%(VALUE)}}}" }, \
  {"cpu", "%{!mcpu=*:-mcpu=%(VALUE)}" }, \
  {"cpu_32", "%{" OPT_ARCH32 ":%{!mcpu=*:-mcpu=%(VALUE)}}" }, \
  {"cpu_64", "%{" OPT_ARCH64 ":%{!mcpu=*:-mcpu=%(VALUE)}}" }, \
  {"float", "%{!msoft-float:%{!mhard-float:-m%(VALUE)-float}}" }
