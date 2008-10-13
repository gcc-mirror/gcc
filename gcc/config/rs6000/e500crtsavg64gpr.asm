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

/* Routines for saving 64-bit integer registers, called by the compiler.  */
/* "GOT" versions that load the address of the GOT into lr before returning.  */

HIDDEN_FUNC(_save64gpr_14_g)	evstdd 14,0(11)
HIDDEN_FUNC(_save64gpr_15_g)	evstdd 15,8(11)
HIDDEN_FUNC(_save64gpr_16_g)	evstdd 16,16(11)
HIDDEN_FUNC(_save64gpr_17_g)	evstdd 17,24(11)
HIDDEN_FUNC(_save64gpr_18_g)	evstdd 18,32(11)
HIDDEN_FUNC(_save64gpr_19_g)	evstdd 19,40(11)
HIDDEN_FUNC(_save64gpr_20_g)	evstdd 20,48(11)
HIDDEN_FUNC(_save64gpr_21_g)	evstdd 21,56(11)
HIDDEN_FUNC(_save64gpr_22_g)	evstdd 22,64(11)
HIDDEN_FUNC(_save64gpr_23_g)	evstdd 23,72(11)
HIDDEN_FUNC(_save64gpr_24_g)	evstdd 24,80(11)
HIDDEN_FUNC(_save64gpr_25_g)	evstdd 25,88(11)
HIDDEN_FUNC(_save64gpr_26_g)	evstdd 26,96(11)
HIDDEN_FUNC(_save64gpr_27_g)	evstdd 27,104(11)
HIDDEN_FUNC(_save64gpr_28_g)	evstdd 28,112(11)
HIDDEN_FUNC(_save64gpr_29_g)	evstdd 29,120(11)
HIDDEN_FUNC(_save64gpr_30_g)	evstdd 30,128(11)
HIDDEN_FUNC(_save64gpr_31_g)	evstdd 31,136(11)
				b _GLOBAL_OFFSET_TABLE_-4
FUNC_END(_save64gpr_31_g)
FUNC_END(_save64gpr_30_g)
FUNC_END(_save64gpr_29_g)
FUNC_END(_save64gpr_28_g)
FUNC_END(_save64gpr_27_g)
FUNC_END(_save64gpr_26_g)
FUNC_END(_save64gpr_25_g)
FUNC_END(_save64gpr_24_g)
FUNC_END(_save64gpr_23_g)
FUNC_END(_save64gpr_22_g)
FUNC_END(_save64gpr_21_g)
FUNC_END(_save64gpr_20_g)
FUNC_END(_save64gpr_19_g)
FUNC_END(_save64gpr_18_g)
FUNC_END(_save64gpr_17_g)
FUNC_END(_save64gpr_16_g)
FUNC_END(_save64gpr_15_g)
FUNC_END(_save64gpr_14_g)

#endif
