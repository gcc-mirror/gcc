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
/* "Bare" versions that simply return to their caller.  */

FUNC_START(_save32gpr_14)	stw 14,-72(11)
FUNC_START(_save32gpr_15)	stw 15,-68(11)
FUNC_START(_save32gpr_16)	stw 16,-64(11)
FUNC_START(_save32gpr_17)	stw 17,-60(11)
FUNC_START(_save32gpr_18)	stw 18,-56(11)
FUNC_START(_save32gpr_19)	stw 19,-52(11)
FUNC_START(_save32gpr_20)	stw 20,-48(11)
FUNC_START(_save32gpr_21)	stw 21,-44(11)
FUNC_START(_save32gpr_22)	stw 22,-40(11)
FUNC_START(_save32gpr_23)	stw 23,-36(11)
FUNC_START(_save32gpr_24)	stw 24,-32(11)
FUNC_START(_save32gpr_25)	stw 25,-28(11)
FUNC_START(_save32gpr_26)	stw 26,-24(11)
FUNC_START(_save32gpr_27)	stw 27,-20(11)
FUNC_START(_save32gpr_28)	stw 28,-16(11)
FUNC_START(_save32gpr_29)	stw 29,-12(11)
FUNC_START(_save32gpr_30)	stw 30,-8(11)
FUNC_START(_save32gpr_31)	stw 31,-4(11)
				blr
FUNC_END(_save32gpr_31)
FUNC_END(_save32gpr_30)
FUNC_END(_save32gpr_29)
FUNC_END(_save32gpr_28)
FUNC_END(_save32gpr_27)
FUNC_END(_save32gpr_26)
FUNC_END(_save32gpr_25)
FUNC_END(_save32gpr_24)
FUNC_END(_save32gpr_23)
FUNC_END(_save32gpr_22)
FUNC_END(_save32gpr_21)
FUNC_END(_save32gpr_20)
FUNC_END(_save32gpr_19)
FUNC_END(_save32gpr_18)
FUNC_END(_save32gpr_17)
FUNC_END(_save32gpr_16)
FUNC_END(_save32gpr_15)
FUNC_END(_save32gpr_14)

#endif
