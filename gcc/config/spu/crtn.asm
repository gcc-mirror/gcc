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

# This file just makes sure that the .fini and .init sections do in
# fact return.  Users may put any desired instructions in those sections.
# This file is the last thing linked into any executable.

	# Note - this macro is complimented by the FUNC_START macro
	# in crti.asm.  If you change this macro you must also change
	# that macro match.
	#
	# Note - we do not try any fancy optimisations of the return
	# sequences here, it is just not worth it.  Instead keep things
	# simple.  Restore all the save resgisters, including the link
	# register and then perform the correct function return instruction.
.macro FUNC_END
	lqd	$lr, 0($sp)
	ai	$sp, $sp, 16
	bi	$lr
.endm
		
	
	.file		"crtn.asm"

	.section	".init"
	;;
	FUNC_END
	
	.section	".fini"
	;;
	FUNC_END
	
# end of crtn.asm
