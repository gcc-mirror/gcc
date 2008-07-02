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
/* "Exit" versions that return to the caller's caller.  */

FUNC_START(_rest32gpr_14_x)	lwz 14,-72(11)
FUNC_START(_rest32gpr_15_x)	lwz 15,-68(11)
FUNC_START(_rest32gpr_16_x)	lwz 16,-64(11)
FUNC_START(_rest32gpr_17_x)	lwz 17,-60(11)
FUNC_START(_rest32gpr_18_x)	lwz 18,-56(11)
FUNC_START(_rest32gpr_19_x)	lwz 19,-52(11)
FUNC_START(_rest32gpr_20_x)	lwz 20,-48(11)
FUNC_START(_rest32gpr_21_x)	lwz 21,-44(11)
FUNC_START(_rest32gpr_22_x)	lwz 22,-40(11)
FUNC_START(_rest32gpr_23_x)	lwz 23,-36(11)
FUNC_START(_rest32gpr_24_x)	lwz 24,-32(11)
FUNC_START(_rest32gpr_25_x)	lwz 25,-28(11)
FUNC_START(_rest32gpr_26_x)	lwz 26,-24(11)
FUNC_START(_rest32gpr_27_x)	lwz 27,-20(11)
FUNC_START(_rest32gpr_28_x)	lwz 28,-16(11)
FUNC_START(_rest32gpr_29_x)	lwz 29,-12(11)
FUNC_START(_rest32gpr_30_x)	lwz 30,-8(11)
FUNC_START(_rest32gpr_31_x)	lwz 0,4(11)
				lwz 31,-4(11)
				mr 1,11
				mtlr 0
				blr
FUNC_END(_rest32gpr_31_x)
FUNC_END(_rest32gpr_30_x)
FUNC_END(_rest32gpr_29_x)
FUNC_END(_rest32gpr_28_x)
FUNC_END(_rest32gpr_27_x)
FUNC_END(_rest32gpr_26_x)
FUNC_END(_rest32gpr_25_x)
FUNC_END(_rest32gpr_24_x)
FUNC_END(_rest32gpr_23_x)
FUNC_END(_rest32gpr_22_x)
FUNC_END(_rest32gpr_21_x)
FUNC_END(_rest32gpr_20_x)
FUNC_END(_rest32gpr_19_x)
FUNC_END(_rest32gpr_18_x)
FUNC_END(_rest32gpr_17_x)
FUNC_END(_rest32gpr_16_x)
FUNC_END(_rest32gpr_15_x)
FUNC_END(_rest32gpr_14_x)

#endif
