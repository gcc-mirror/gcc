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

/* Routines for restoring 64-bit integer registers, called by the compiler.  */
/* "Bare" versions that return to their caller.  */

FUNC_START(_rest64gpr_14)	evldd 14,0(11)
FUNC_START(_rest64gpr_15)	evldd 15,8(11)
FUNC_START(_rest64gpr_16)	evldd 16,16(11)
FUNC_START(_rest64gpr_17)	evldd 17,24(11)
FUNC_START(_rest64gpr_18)	evldd 18,32(11)
FUNC_START(_rest64gpr_19)	evldd 19,40(11)
FUNC_START(_rest64gpr_20)	evldd 20,48(11)
FUNC_START(_rest64gpr_21)	evldd 21,56(11)
FUNC_START(_rest64gpr_22)	evldd 22,64(11)
FUNC_START(_rest64gpr_23)	evldd 23,72(11)
FUNC_START(_rest64gpr_24)	evldd 24,80(11)
FUNC_START(_rest64gpr_25)	evldd 25,88(11)
FUNC_START(_rest64gpr_26)	evldd 26,96(11)
FUNC_START(_rest64gpr_27)	evldd 27,104(11)
FUNC_START(_rest64gpr_28)	evldd 28,112(11)
FUNC_START(_rest64gpr_29)	evldd 29,120(11)
FUNC_START(_rest64gpr_30)	evldd 30,128(11)
FUNC_START(_rest64gpr_31)	evldd 31,136(11)
				blr
FUNC_END(_rest64gpr_31)
FUNC_END(_rest64gpr_30)
FUNC_END(_rest64gpr_29)
FUNC_END(_rest64gpr_28)
FUNC_END(_rest64gpr_27)
FUNC_END(_rest64gpr_26)
FUNC_END(_rest64gpr_25)
FUNC_END(_rest64gpr_24)
FUNC_END(_rest64gpr_23)
FUNC_END(_rest64gpr_22)
FUNC_END(_rest64gpr_21)
FUNC_END(_rest64gpr_20)
FUNC_END(_rest64gpr_19)
FUNC_END(_rest64gpr_18)
FUNC_END(_rest64gpr_17)
FUNC_END(_rest64gpr_16)
FUNC_END(_rest64gpr_15)
FUNC_END(_rest64gpr_14)

#endif
