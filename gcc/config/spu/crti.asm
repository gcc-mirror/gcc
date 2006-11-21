#  Copyright (C) 2006 Free Software Foundation, Inc.
#
#  This file is free software; you can redistribute it and/or modify it under
#  the terms of the GNU General Public License as published by the Free
#  Software Foundation; either version 2 of the License, or (at your option) 
#  any later version.
#
#  This file is distributed in the hope that it will be useful, but WITHOUT
#  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#  for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this file; see the file COPYING.  If not, write to the Free
#  Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
#  02110-1301, USA.  */
# 
#    As a special exception, if you link this library with files
#    compiled with GCC to produce an executable, this does not cause
#    the resulting executable to be covered by the GNU General Public License.
#    This exception does not however invalidate any other reasons why
#    the executable file might be covered by the GNU General Public License.
# 

# This file just make a stack frame for the contents of the .fini and
# .init sections.  Users may put any desired instructions in those
# sections.

	# Note - this macro is complimented by the FUNC_END macro
	# in crtn.asm.  If you change this macro you must also change
	# that macro match.
.macro FUNC_START
	#  Create a stack frame and save any call-preserved registers
	ai	$sp, $sp, -16
	stqd	$lr, 0($sp)
.endm
		
	.file		"crti.asm"

	.section	".init"
	.align 2
	.global	_init
_init:
	FUNC_START
	
		
	.section	".fini"
	.align	2
	.global	_fini
_fini:
	FUNC_START
	
# end of crti.asm
