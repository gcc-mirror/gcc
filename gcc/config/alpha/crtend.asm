 # Copyright (C) 1996 Free Software Foundation, Inc.
 #  Contributed by Richard Henderson (rth@tamu.edu)
 #
 # This file is free software; you can redistribute it and/or modify it
 # under the terms of the GNU General Public License as published by the
 # Free Software Foundation; either version 2, or (at your option) any
 # later version.
 # 
 # In addition to the permissions in the GNU General Public License, the
 # Free Software Foundation gives you unlimited permission to link the
 # compiled version of this file with other programs, and to distribute
 # those programs without any restriction coming from the use of this
 # file.  (The General Public License restrictions do apply in other
 # respects; for example, they cover modification of the file, and
 # distribution when not linked into another program.)
 # 
 # This file is distributed in the hope that it will be useful, but
 # WITHOUT ANY WARRANTY; without even the implied warranty of
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 # General Public License for more details.
 # 
 # You should have received a copy of the GNU General Public License
 # along with this program; see the file COPYING.  If not, write to
 # the Free Software Foundation, 59 Temple Place - Suite 330,
 # Boston, MA 02111-1307, USA.
 # 
 #    As a special exception, if you link this library with files
 #    compiled with GCC to produce an executable, this does not cause
 #    the resulting executable to be covered by the GNU General Public License.
 #    This exception does not however invalidate any other reasons why
 #    the executable file might be covered by the GNU General Public License.

 #
 # Tails of the constructor/destructor lists.
 #

 # The __*TOR_END__ symbols are not global because when this file is used
 # in a shared library, we do not want the symbol to fall over to the
 # application's lists.

.section .ctors,"aw"

	.align 3
__CTOR_END__:
	.quad   0

.section .dtors,"aw"

	.align 3
__DTOR_END__:
	.quad   0

.section .eh_frame,"aw"
__FRAME_END__:
	.quad	0

 #
 # Fragment of the ELF _init routine that invokes our ctor startup
 #

.section .init,"ax"

	# Since the bits of the _init function are spread across many
	# object files, each potentially with its own GP, we must
	# assume we need to load ours.  Further, our .init section
	# can easily be more than 4MB away from our .text bits so we
	# can't use bsr.

	br      $29,1f
1:	ldgp    $29,0($29)
	jsr     $26,__do_global_ctors_aux

	# Must match the alignment we got from crti.o else we get
	# zero-filled holes in our _init function and thense SIGILL.
	.align 3

 #
 # Invoke our destructors in order.
 #

.text

	.align 3
	.ent __do_global_ctors_aux

__do_global_ctors_aux:
	ldgp	$29,0($27)
	lda     $30,-16($30)
	.frame  $30,16,$26,0
	stq     $9,8($30)
	stq     $26,0($30)
	.mask   0x4000200,-16
	.prologue 1

	lda     $9,__CTOR_END__-8
	br      1f
0:	jsr     $26,($27)
1:	ldq     $27,0($9)
	subq    $9,8,$9
	not     $27,$0
	bne     $0,0b

	ldq     $26,0($30)
	ldq     $9,8($30)
	lda     $30,16($30)
	ret

	.end __do_global_ctors_aux
