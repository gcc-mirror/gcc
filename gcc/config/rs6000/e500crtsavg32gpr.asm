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

/* Routines for saving 32-bit integer registers, called by the compiler.  */
/* "GOT" versions that load the address of the GOT into lr before returning.  */

HIDDEN_FUNC(_save32gpr_14_g)	stw 14,-72(11)
HIDDEN_FUNC(_save32gpr_15_g)	stw 15,-68(11)
HIDDEN_FUNC(_save32gpr_16_g)	stw 16,-64(11)
HIDDEN_FUNC(_save32gpr_17_g)	stw 17,-60(11)
HIDDEN_FUNC(_save32gpr_18_g)	stw 18,-56(11)
HIDDEN_FUNC(_save32gpr_19_g)	stw 19,-52(11)
HIDDEN_FUNC(_save32gpr_20_g)	stw 20,-48(11)
HIDDEN_FUNC(_save32gpr_21_g)	stw 21,-44(11)
HIDDEN_FUNC(_save32gpr_22_g)	stw 22,-40(11)
HIDDEN_FUNC(_save32gpr_23_g)	stw 23,-36(11)
HIDDEN_FUNC(_save32gpr_24_g)	stw 24,-32(11)
HIDDEN_FUNC(_save32gpr_25_g)	stw 25,-28(11)
HIDDEN_FUNC(_save32gpr_26_g)	stw 26,-24(11)
HIDDEN_FUNC(_save32gpr_27_g)	stw 27,-20(11)
HIDDEN_FUNC(_save32gpr_28_g)	stw 28,-16(11)
HIDDEN_FUNC(_save32gpr_29_g)	stw 29,-12(11)
HIDDEN_FUNC(_save32gpr_30_g)	stw 30,-8(11)
HIDDEN_FUNC(_save32gpr_31_g)	stw 31,-4(11)
				b _GLOBAL_OFFSET_TABLE_-4
FUNC_END(_save32gpr_31_g)
FUNC_END(_save32gpr_30_g)
FUNC_END(_save32gpr_29_g)
FUNC_END(_save32gpr_28_g)
FUNC_END(_save32gpr_27_g)
FUNC_END(_save32gpr_26_g)
FUNC_END(_save32gpr_25_g)
FUNC_END(_save32gpr_24_g)
FUNC_END(_save32gpr_23_g)
FUNC_END(_save32gpr_22_g)
FUNC_END(_save32gpr_21_g)
FUNC_END(_save32gpr_20_g)
FUNC_END(_save32gpr_19_g)
FUNC_END(_save32gpr_18_g)
FUNC_END(_save32gpr_17_g)
FUNC_END(_save32gpr_16_g)
FUNC_END(_save32gpr_15_g)
FUNC_END(_save32gpr_14_g)

#endif
