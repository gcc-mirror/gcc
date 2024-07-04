/* Definitions 128-bit floating point precisions used by PowerPC.
   Copyright (C) 2018-2024 Free Software Foundation, Inc.
   Contributed by Michael Meissner (meissner@linux.ibm.com)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* We order the 3 128-bit floating point types so that IFmode (IBM 128-bit
   floating point) is the 128-bit floating point type with the highest
   precision (128 bits).  This so that machine independent parts of the
   compiler do not try to widen IFmode to TFmode on ISA 3.0 (power9) that has
   hardware support for IEEE 128-bit.  We set TFmode (long double mode) in
   between, and KFmode (explicit __float128) below it.

   We won't encounter conversion from IEEE 128-bit to IBM 128-bit because we
   don't have insns to support the IBM 128-bit aritmetic operations.  */

#ifndef RS6000_MODES_H
#define RS6000_MODES_H		1
#define FLOAT_PRECISION_IFmode	128
#define FLOAT_PRECISION_TFmode	127
#define FLOAT_PRECISION_KFmode	126
#endif	/* RS6000_MODES_H */
