/*
 * Special support for eabi and SVR4
 *
 *   Copyright (C) 1995, 1996, 1998, 2000, 2001, 2008
 *   Free Software Foundation, Inc.
 *   Written By Michael Meissner
 *   64-bit support written by David Edelsohn
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

/* Do any initializations needed for the eabi environment */

	.section ".text"
	#include "ppc-asm.h"

/* On PowerPC64 Linux, these functions are provided by the linker.  */
#ifndef __powerpc64__

/* Routines for restoring integer registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the integer restore area.  */

FUNC_START(_restgpr_14_x)	lwz	14,-72(11)	/* restore gp registers */
FUNC_START(_restgpr_15_x)	lwz	15,-68(11)
FUNC_START(_restgpr_16_x)	lwz	16,-64(11)
FUNC_START(_restgpr_17_x)	lwz	17,-60(11)
FUNC_START(_restgpr_18_x)	lwz	18,-56(11)
FUNC_START(_restgpr_19_x)	lwz	19,-52(11)
FUNC_START(_restgpr_20_x)	lwz	20,-48(11)
FUNC_START(_restgpr_21_x)	lwz	21,-44(11)
FUNC_START(_restgpr_22_x)	lwz	22,-40(11)
FUNC_START(_restgpr_23_x)	lwz	23,-36(11)
FUNC_START(_restgpr_24_x)	lwz	24,-32(11)
FUNC_START(_restgpr_25_x)	lwz	25,-28(11)
FUNC_START(_restgpr_26_x)	lwz	26,-24(11)
FUNC_START(_restgpr_27_x)	lwz	27,-20(11)
FUNC_START(_restgpr_28_x)	lwz	28,-16(11)
FUNC_START(_restgpr_29_x)	lwz	29,-12(11)
FUNC_START(_restgpr_30_x)	lwz	30,-8(11)
FUNC_START(_restgpr_31_x)	lwz	0,4(11)
				lwz	31,-4(11)
				mtlr	0
				mr	1,11
				blr
FUNC_END(_restgpr_31_x)
FUNC_END(_restgpr_30_x)
FUNC_END(_restgpr_29_x)
FUNC_END(_restgpr_28_x)
FUNC_END(_restgpr_27_x)
FUNC_END(_restgpr_26_x)
FUNC_END(_restgpr_25_x)
FUNC_END(_restgpr_24_x)
FUNC_END(_restgpr_23_x)
FUNC_END(_restgpr_22_x)
FUNC_END(_restgpr_21_x)
FUNC_END(_restgpr_20_x)
FUNC_END(_restgpr_19_x)
FUNC_END(_restgpr_18_x)
FUNC_END(_restgpr_17_x)
FUNC_END(_restgpr_16_x)
FUNC_END(_restgpr_15_x)
FUNC_END(_restgpr_14_x)

#endif
