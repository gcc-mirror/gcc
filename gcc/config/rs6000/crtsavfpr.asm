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

/* Routines for saving floating point registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the floating point save area.  */

FUNC_START(_savefpr_14)	stfd	14,-144(11)	/* save fp registers */
FUNC_START(_savefpr_15)	stfd	15,-136(11)
FUNC_START(_savefpr_16)	stfd	16,-128(11)
FUNC_START(_savefpr_17)	stfd	17,-120(11)
FUNC_START(_savefpr_18)	stfd	18,-112(11)
FUNC_START(_savefpr_19)	stfd	19,-104(11)
FUNC_START(_savefpr_20)	stfd	20,-96(11)
FUNC_START(_savefpr_21)	stfd	21,-88(11)
FUNC_START(_savefpr_22)	stfd	22,-80(11)
FUNC_START(_savefpr_23)	stfd	23,-72(11)
FUNC_START(_savefpr_24)	stfd	24,-64(11)
FUNC_START(_savefpr_25)	stfd	25,-56(11)
FUNC_START(_savefpr_26)	stfd	26,-48(11)
FUNC_START(_savefpr_27)	stfd	27,-40(11)
FUNC_START(_savefpr_28)	stfd	28,-32(11)
FUNC_START(_savefpr_29)	stfd	29,-24(11)
FUNC_START(_savefpr_30)	stfd	30,-16(11)
FUNC_START(_savefpr_31)	stfd	31,-8(11)
			blr
FUNC_END(_savefpr_31)
FUNC_END(_savefpr_30)
FUNC_END(_savefpr_29)
FUNC_END(_savefpr_28)
FUNC_END(_savefpr_27)
FUNC_END(_savefpr_26)
FUNC_END(_savefpr_25)
FUNC_END(_savefpr_24)
FUNC_END(_savefpr_23)
FUNC_END(_savefpr_22)
FUNC_END(_savefpr_21)
FUNC_END(_savefpr_20)
FUNC_END(_savefpr_19)
FUNC_END(_savefpr_18)
FUNC_END(_savefpr_17)
FUNC_END(_savefpr_16)
FUNC_END(_savefpr_15)
FUNC_END(_savefpr_14)

#endif
