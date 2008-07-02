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

/* "Exit" versions that return to their caller's caller.  */

FUNC_START(_rest64gpr_14_x)	evldd 14,0(11)
FUNC_START(_rest64gpr_15_x)	evldd 15,8(11)
FUNC_START(_rest64gpr_16_x)	evldd 16,16(11)
FUNC_START(_rest64gpr_17_x)	evldd 17,24(11)
FUNC_START(_rest64gpr_18_x)	evldd 18,32(11)
FUNC_START(_rest64gpr_19_x)	evldd 19,40(11)
FUNC_START(_rest64gpr_20_x)	evldd 20,48(11)
FUNC_START(_rest64gpr_21_x)	evldd 21,56(11)
FUNC_START(_rest64gpr_22_x)	evldd 22,64(11)
FUNC_START(_rest64gpr_23_x)	evldd 23,72(11)
FUNC_START(_rest64gpr_24_x)	evldd 24,80(11)
FUNC_START(_rest64gpr_25_x)	evldd 25,88(11)
FUNC_START(_rest64gpr_26_x)	evldd 26,96(11)
FUNC_START(_rest64gpr_27_x)	evldd 27,104(11)
FUNC_START(_rest64gpr_28_x)	evldd 28,112(11)
FUNC_START(_rest64gpr_29_x)	evldd 29,120(11)
FUNC_START(_rest64gpr_30_x)	evldd 30,128(11)
FUNC_START(_rest64gpr_31_x)	lwz 0,148(11)
				evldd 31,136(11)
				addi 1,11,144
				mtlr 0
				blr
FUNC_END(_rest64gpr_31_x)
FUNC_END(_rest64gpr_30_x)
FUNC_END(_rest64gpr_29_x)
FUNC_END(_rest64gpr_28_x)
FUNC_END(_rest64gpr_27_x)
FUNC_END(_rest64gpr_26_x)
FUNC_END(_rest64gpr_25_x)
FUNC_END(_rest64gpr_24_x)
FUNC_END(_rest64gpr_23_x)
FUNC_END(_rest64gpr_22_x)
FUNC_END(_rest64gpr_21_x)
FUNC_END(_rest64gpr_20_x)
FUNC_END(_rest64gpr_19_x)
FUNC_END(_rest64gpr_18_x)
FUNC_END(_rest64gpr_17_x)
FUNC_END(_rest64gpr_16_x)
FUNC_END(_rest64gpr_15_x)
FUNC_END(_rest64gpr_14_x)

#endif
