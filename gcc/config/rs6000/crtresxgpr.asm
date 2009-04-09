/*
 * Special support for eabi and SVR4
 *
 *   Copyright (C) 1995, 1996, 1998, 2000, 2001, 2008, 2009
 *   Free Software Foundation, Inc.
 *   Written By Michael Meissner
 *   64-bit support written by David Edelsohn
 * 
 * This file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * 
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * Under Section 7 of GPL version 3, you are granted additional
 * permissions described in the GCC Runtime Library Exception, version
 * 3.1, as published by the Free Software Foundation.
 *
 * You should have received a copy of the GNU General Public License and
 * a copy of the GCC Runtime Library Exception along with this program;
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 * <http://www.gnu.org/licenses/>.
 */ 

/* Do any initializations needed for the eabi environment */

	.section ".text"
	#include "ppc-asm.h"

/* On PowerPC64 Linux, these functions are provided by the linker.  */
#ifndef __powerpc64__

/* Routines for restoring integer registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the integer restore area.  */

HIDDEN_FUNC(_restgpr_14_x)	lwz	14,-72(11)	/* restore gp registers */
HIDDEN_FUNC(_restgpr_15_x)	lwz	15,-68(11)
HIDDEN_FUNC(_restgpr_16_x)	lwz	16,-64(11)
HIDDEN_FUNC(_restgpr_17_x)	lwz	17,-60(11)
HIDDEN_FUNC(_restgpr_18_x)	lwz	18,-56(11)
HIDDEN_FUNC(_restgpr_19_x)	lwz	19,-52(11)
HIDDEN_FUNC(_restgpr_20_x)	lwz	20,-48(11)
HIDDEN_FUNC(_restgpr_21_x)	lwz	21,-44(11)
HIDDEN_FUNC(_restgpr_22_x)	lwz	22,-40(11)
HIDDEN_FUNC(_restgpr_23_x)	lwz	23,-36(11)
HIDDEN_FUNC(_restgpr_24_x)	lwz	24,-32(11)
HIDDEN_FUNC(_restgpr_25_x)	lwz	25,-28(11)
HIDDEN_FUNC(_restgpr_26_x)	lwz	26,-24(11)
HIDDEN_FUNC(_restgpr_27_x)	lwz	27,-20(11)
HIDDEN_FUNC(_restgpr_28_x)	lwz	28,-16(11)
HIDDEN_FUNC(_restgpr_29_x)	lwz	29,-12(11)
HIDDEN_FUNC(_restgpr_30_x)	lwz	30,-8(11)
HIDDEN_FUNC(_restgpr_31_x)	lwz	0,4(11)
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
