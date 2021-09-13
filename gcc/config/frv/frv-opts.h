/* Frv option-handling defitions.
   Copyright (C) 1999-2021 Free Software Foundation, Inc.

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

#ifndef FRV_OPTS_H
#define FRV_OPTS_H

/* CPU type.  This must be identical to the cpu enumeration in frv.md.  */
typedef enum frv_cpu
{
  FRV_CPU_GENERIC,
  FRV_CPU_FR550,
  FRV_CPU_FR500,
  FRV_CPU_FR450,
  FRV_CPU_FR405,
  FRV_CPU_FR400,
  FRV_CPU_FR300,
  FRV_CPU_SIMPLE,
  FRV_CPU_TOMCAT
} frv_cpu_t;

#endif
