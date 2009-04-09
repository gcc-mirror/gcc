/* VMS trampoline for nested functions
   Copyright (C) 2001, 2008, 2009 Free Software Foundation, Inc.
   Contributed by Douglas B. Rupp (rupp@gnat.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

;# Alpha OpenVMS trampoline
;#
	.set noreorder
	.set volatile
	.set noat
.text
	.align 3
	.globl __tramp
	.ent __tramp
__tramp..en:

.link
	.align 3
__tramp:
	.pdesc __tramp..en,null
.text
	ldq $1,24($27)
	ldq $27,16($27)
	ldq $28,8($27)
	jmp $31,($28),0
	.end __tramp
