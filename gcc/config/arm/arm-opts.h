/* Definitions for option handling for ARM.
   Copyright (C) 1991-2018 Free Software Foundation, Inc.

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

#ifndef ARM_OPTS_H
#define ARM_OPTS_H

#include "arm-flags.h"
#include "arm-isa.h"
#include "arm-cpu.h"

/* Which __fp16 format to use.
   The enumeration values correspond to the numbering for the
   Tag_ABI_FP_16bit_format attribute.
 */
enum arm_fp16_format_type
{
  ARM_FP16_FORMAT_NONE = 0,
  ARM_FP16_FORMAT_IEEE = 1,
  ARM_FP16_FORMAT_ALTERNATIVE = 2
};

/* Which ABI to use.  */
enum arm_abi_type
{
  ARM_ABI_APCS,
  ARM_ABI_ATPCS,
  ARM_ABI_AAPCS,
  ARM_ABI_IWMMXT,
  ARM_ABI_AAPCS_LINUX
};

enum float_abi_type
{
  ARM_FLOAT_ABI_SOFT,
  ARM_FLOAT_ABI_SOFTFP,
  ARM_FLOAT_ABI_HARD
};

/* Which thread pointer access sequence to use.  */
enum arm_tp_type {
  TP_AUTO,
  TP_SOFT,
  TP_CP15
};

/* Which TLS scheme to use.  */
enum arm_tls_type {
  TLS_GNU,
  TLS_GNU2
};
#endif
