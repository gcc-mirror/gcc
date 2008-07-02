/*
 * Special support for e500 eabi and SVR4
 *
 *   Copyright (C) 2008 Free Software Foundation, Inc.
 *   Written by Nathan Froyd
 * 
 * This file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 * 
 * In addition to the permissions in the GNU General Public License, the
 * Free Software Foundation gives you unlimited permission to link the
 * compiled version of this file with other programs, and to distribute
 * those programs without any restriction coming from the use of this
 * file.  (The General Public License restrictions do apply in other
 * respects; for example, they cover modification of the file, and
 * distribution when not linked into another program.)
 * 
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 * 
 *    As a special exception, if you link this library with files
 *    compiled with GCC to produce an executable, this does not cause
 *    the resulting executable to be covered by the GNU General Public License.
 *    This exception does not however invalidate any other reasons why
 *    the executable file might be covered by the GNU General Public License.
 */ 

	.section ".text"
	#include "ppc-asm.h"

#ifdef __SPE__

/* Routines for restoring 32-bit integer registers, called by the compiler.  */
/* "Tail" versions that perform a tail call.  */

FUNC_START(_rest32gpr_14_t)	lwz 14,-72(11)
FUNC_START(_rest32gpr_15_t)	lwz 15,-68(11)
FUNC_START(_rest32gpr_16_t)	lwz 16,-64(11)
FUNC_START(_rest32gpr_17_t)	lwz 17,-60(11)
FUNC_START(_rest32gpr_18_t)	lwz 18,-56(11)
FUNC_START(_rest32gpr_19_t)	lwz 19,-52(11)
FUNC_START(_rest32gpr_20_t)	lwz 20,-48(11)
FUNC_START(_rest32gpr_21_t)	lwz 21,-44(11)
FUNC_START(_rest32gpr_22_t)	lwz 22,-40(11)
FUNC_START(_rest32gpr_23_t)	lwz 23,-36(11)
FUNC_START(_rest32gpr_24_t)	lwz 24,-32(11)
FUNC_START(_rest32gpr_25_t)	lwz 25,-28(11)
FUNC_START(_rest32gpr_26_t)	lwz 26,-24(11)
FUNC_START(_rest32gpr_27_t)	lwz 27,-20(11)
FUNC_START(_rest32gpr_28_t)	lwz 28,-16(11)
FUNC_START(_rest32gpr_29_t)	lwz 29,-12(11)
FUNC_START(_rest32gpr_30_t)	lwz 30,-8(11)
FUNC_START(_rest32gpr_31_t)	lwz 31,-4(11)
				lwz 0,4(11)
				mr 1,11
				blr
FUNC_END(_rest32gpr_31_t)
FUNC_END(_rest32gpr_30_t)
FUNC_END(_rest32gpr_29_t)
FUNC_END(_rest32gpr_28_t)
FUNC_END(_rest32gpr_27_t)
FUNC_END(_rest32gpr_26_t)
FUNC_END(_rest32gpr_25_t)
FUNC_END(_rest32gpr_24_t)
FUNC_END(_rest32gpr_23_t)
FUNC_END(_rest32gpr_22_t)
FUNC_END(_rest32gpr_21_t)
FUNC_END(_rest32gpr_20_t)
FUNC_END(_rest32gpr_19_t)
FUNC_END(_rest32gpr_18_t)
FUNC_END(_rest32gpr_17_t)
FUNC_END(_rest32gpr_16_t)
FUNC_END(_rest32gpr_15_t)
FUNC_END(_rest32gpr_14_t)

#endif
