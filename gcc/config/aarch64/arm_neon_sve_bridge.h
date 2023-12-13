/* AArch64 NEON-SVE Bridge intrinsics include file.
   Copyright (C) 2023 Free Software Foundation, Inc.

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

#ifndef _ARM_NEON_SVE_BRIDGE_H_
#define _ARM_NEON_SVE_BRIDGE_H_

#include <arm_neon.h>
#include <arm_sve.h>

/* NOTE: This implementation of arm_neon_sve_bridge.h is intentionally short.  It does
   not define the types and intrinsic functions directly in C and C++
   code, but instead uses the following pragma to tell GCC to insert the
   necessary type and function definitions itself.  The net effect is the
   same, and the file is a complete implementation of arm_neon_sve_bridge.h.  */
#pragma GCC aarch64 "arm_neon_sve_bridge.h"

#endif