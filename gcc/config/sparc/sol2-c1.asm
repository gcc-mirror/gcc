! crt1.s for solaris 2.0.

!   Copyright (C) 1992 Free Software Foundation, Inc.
!   Written By David Vinayak Henkel-Wallace, June 1992
! 
! This file is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by the
! Free Software Foundation; either version 2, or (at your option) any
! later version.
! 
! In addition to the permissions in the GNU General Public License, the
! Free Software Foundation gives you unlimited permission to link the
! compiled version of this file with other programs, and to distribute
! those programs without any restriction coming from the use of this
! file.  (The General Public License restrictions do apply in other
! respects; for example, they cover modification of the file, and
! distribution when not linked into another program.)
! 
! This file is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; see the file COPYING.  If not, write to
! the Free Software Foundation, 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
! 
!    As a special exception, if you link this library with files
!    compiled with GCC to produce an executable, this does not cause
!    the resulting executable to be covered by the GNU General Public License.
!    This exception does not however invalidate any other reasons why
!    the executable file might be covered by the GNU General Public License.
! 

! This file takes control of the process from the kernel, as specified
! in section 3 of the SVr4 ABI.
! This file is the first thing linked into any executable.

	.section	".text"
	.proc	022
	.global	_start

_start:
	mov	0, %fp		! Mark bottom frame pointer
	ld	[%sp + 64], %l0	! argc
	add	%sp, 68, %l1	! argv

	! Leave some room for a call.  Sun leaves 32 octets (to sit on
	! a cache line?) so we do too.
	sub	%sp, 32, %sp

	! %g1 may contain a function to be registered w/atexit
	orcc	%g0, %g1, %g0
	be	.nope
	mov	%g1, %o0
	call	atexit
	nop   
.nope:
	! Now make sure constructors and destructors are handled.
	set	_fini, %o0
	call	atexit, 1
	nop
	call	_init, 0
	nop

	! We ignore the auxiliary vector; there's no defined way to
	! access those data anyway.  Instead, go straight to main:
	mov	%l0, %o0	! argc
	mov	%l1, %o1	! argv
	! Skip argc words past argv, to env:
	sll	%l0, 2, %o2
	add	%o2, 4, %o2
	add	%l1, %o2, %o2	! env
	set	_environ, %o3
	st	%o2, [%o3]	! *_environ
	call	main, 4
	nop   
	call	exit, 0
	nop   
	call	_exit, 0
	nop   
	! We should never get here.

	.type	_start,#function
	.size	_start,.-_start
