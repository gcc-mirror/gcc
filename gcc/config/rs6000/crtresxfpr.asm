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

/* Routines for restoring floating point registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the floating point save area.  */
/* In addition to restoring the fp registers, it will return to the caller's */
/* caller */

FUNC_START(_restfpr_14_x)	lfd	14,-144(11)	/* restore fp registers */
FUNC_START(_restfpr_15_x)	lfd	15,-136(11)
FUNC_START(_restfpr_16_x)	lfd	16,-128(11)
FUNC_START(_restfpr_17_x)	lfd	17,-120(11)
FUNC_START(_restfpr_18_x)	lfd	18,-112(11)
FUNC_START(_restfpr_19_x)	lfd	19,-104(11)
FUNC_START(_restfpr_20_x)	lfd	20,-96(11)
FUNC_START(_restfpr_21_x)	lfd	21,-88(11)
FUNC_START(_restfpr_22_x)	lfd	22,-80(11)
FUNC_START(_restfpr_23_x)	lfd	23,-72(11)
FUNC_START(_restfpr_24_x)	lfd	24,-64(11)
FUNC_START(_restfpr_25_x)	lfd	25,-56(11)
FUNC_START(_restfpr_26_x)	lfd	26,-48(11)
FUNC_START(_restfpr_27_x)	lfd	27,-40(11)
FUNC_START(_restfpr_28_x)	lfd	28,-32(11)
FUNC_START(_restfpr_29_x)	lfd	29,-24(11)
FUNC_START(_restfpr_30_x)	lfd	30,-16(11)
FUNC_START(_restfpr_31_x)	lwz	0,4(11)
				lfd	31,-8(11)
				mtlr	0
				mr	1,11
				blr
FUNC_END(_restfpr_31_x)
FUNC_END(_restfpr_30_x)
FUNC_END(_restfpr_29_x)
FUNC_END(_restfpr_28_x)
FUNC_END(_restfpr_27_x)
FUNC_END(_restfpr_26_x)
FUNC_END(_restfpr_25_x)
FUNC_END(_restfpr_24_x)
FUNC_END(_restfpr_23_x)
FUNC_END(_restfpr_22_x)
FUNC_END(_restfpr_21_x)
FUNC_END(_restfpr_20_x)
FUNC_END(_restfpr_19_x)
FUNC_END(_restfpr_18_x)
FUNC_END(_restfpr_17_x)
FUNC_END(_restfpr_16_x)
FUNC_END(_restfpr_15_x)
FUNC_END(_restfpr_14_x)

#endif
