#   Copyright (C) 2001, 2004, 2008, 2009, 2010 Free Software Foundation, Inc.
#   Written By Nick Clifton
# 
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
# 
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
# 
# You should have received a copy of the GNU General Public License and
# a copy of the GCC Runtime Library Exception along with this program;
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
# <http://www.gnu.org/licenses/>.

/* An executable stack is *not* required for these functions.  */
#if defined(__ELF__) && defined(__linux__)
.section .note.GNU-stack,"",%progbits
.previous
#endif

#ifdef __ARM_EABI__
/* Some attributes that are common to all routines in this file.  */
	/* Tag_ABI_align_needed: This code does not require 8-byte
	   alignment from the caller.  */
	/* .eabi_attribute 24, 0  -- default setting.  */
	/* Tag_ABI_align_preserved: This code preserves 8-byte
	   alignment in any callee.  */
	.eabi_attribute 25, 1
#endif /* __ARM_EABI__ */

# This file just makes sure that the .fini and .init sections do in
# fact return.  Users may put any desired instructions in those sections.
# This file is the last thing linked into any executable.

	# Note - this macro is complemented by the FUNC_START macro
	# in crti.asm.  If you change this macro you must also change
	# that macro match.
	#
	# Note - we do not try any fancy optimizations of the return
	# sequences here, it is just not worth it.  Instead keep things
	# simple.  Restore all the save resgisters, including the link
	# register and then perform the correct function return instruction.
	# We also save/restore r3 to ensure stack alignment.
.macro FUNC_END
#ifdef __thumb__
	.thumb
	
	pop	{r3, r4, r5, r6, r7}
	pop	{r3}
	mov	lr, r3
#else
	.arm
	
	sub	sp, fp, #40
	ldmfd	sp, {r4, r5, r6, r7, r8, r9, sl, fp, sp, lr}
#endif
	
#if defined __THUMB_INTERWORK__ || defined __thumb__
	bx	lr
#else
	mov	pc, lr
#endif
.endm
		
	
	.section	".init"
	;;
	FUNC_END
	
	.section	".fini"
	;;
	FUNC_END
	
# end of crtn.asm
