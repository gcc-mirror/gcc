/* Definitions for option handling for ARM.
   Copyright (C) 1991-2013 Free Software Foundation, Inc.

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

#ifndef ARM_OPTS_H
#define ARM_OPTS_H

/* The various ARM cores.  */
enum processor_type
{
#undef ARM_CORE
#define ARM_CORE(NAME, INTERNAL_IDENT, IDENT, ARCH, FLAGS, COSTS) \
  INTERNAL_IDENT,
#include "arm-cores.def"
#undef ARM_CORE
  /* Used to indicate that no processor has been specified.  */
  arm_none
};

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
