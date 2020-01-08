/* Definitions for option handling for TI C6X.
   Copyright (C) 2011-2020 Free Software Foundation, Inc.

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

#ifndef C6X_OPTS_H
#define C6X_OPTS_H

/* An enumeration of all supported target devices.  */
typedef enum c6x_cpu_type
{
#define C6X_ISA(NAME,ENUM_VALUE,FLAGS)		\
  ENUM_VALUE,
#include "c6x-isas.def"
#undef C6X_ISA
  unk_isa
} c6x_cpu_t;

enum c6x_sdata { C6X_SDATA_NONE, C6X_SDATA_DEFAULT, C6X_SDATA_ALL };

#endif
