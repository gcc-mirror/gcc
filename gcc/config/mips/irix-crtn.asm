/* Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

	.abicalls
	.set	noreorder
	.set	nomacro

	.section .gcc_init,"ax",@progbits
#if _MIPS_SIM == _ABIO32
	lw	$31,0($sp)
	jr	$31
	addiu	$sp,$sp,16
#else
	ld	$31,0($sp)
	ld	$28,8($sp)
	jr	$31
	daddiu	$sp,$sp,16
#endif

	.section .gcc_fini,"ax",@progbits
#if _MIPS_SIM == _ABIO32
	lw	$31,0($sp)
	jr	$31
	addiu	$sp,$sp,16
#else
	ld	$31,0($sp)
	ld	$28,8($sp)
	jr	$31
	daddiu	$sp,$sp,16
#endif
