/* GCC option-handling definitions for the TI MSP430
   Copyright (C) 2014-2017 Free Software Foundation, Inc.

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

#ifndef MSP430_OPTS_H
#define MSP430_OPTS_H

enum msp430_hwmult_types
{
  MSP430_HWMULT_NONE,
  MSP430_HWMULT_AUTO,
  MSP430_HWMULT_SMALL,
  MSP430_HWMULT_LARGE,
  MSP430_HWMULT_F5SERIES
};

enum msp430_regions
{
  MSP430_REGION_ANY,
  MSP430_REGION_EITHER,
  MSP430_REGION_LOWER,
  MSP430_REGION_UPPER
};

#endif
