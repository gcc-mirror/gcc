 # Copyright (C) 1996, 1998 Free Software Foundation, Inc.
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
 # Heads of the constructor/destructor lists.
 #

 # The __*TOR_LIST__ symbols are not global because when this file is used
 # in a shared library, we do not want the symbol to fall over to the
 # application's lists.

.section .ctors,"aw"

	.align 3
__CTOR_LIST__:
	.quad -1

.section .dtors,"aw"

	.align 3
__DTOR_LIST__:
	.quad -1

.section .eh_frame,"aw"
__EH_FRAME_BEGIN__:

 #
 # Fragment of the ELF _fini routine that invokes our dtor cleanup.
 #

.section .fini,"ax"

	# Since the bits of the _fini function are spread across many
	# object files, each potentially with its own GP, we must
	# assume we need to load ours.  Further, our .fini section
	# can easily be more than 4MB away from our .text bits so we
	# can't use bsr.

	br      $29,1f
1:	ldgp    $29,0($29)
	jsr     $26,__do_global_dtors_aux

	# Ideally this call would go in crtend.o, except that we can't
	# get hold of __EH_FRAME_BEGIN__ there.

	jsr	$26,__do_frame_takedown

	# Must match the alignment we got from crti.o else we get
	# zero-filled holes in our _fini function and then SIGILL.
	.align 3

 #
 # Fragment of the ELF _init routine that sets up the frame info.
 #

.section .init,"ax"
       br      $29,1f
1:     ldgp    $29,0($29)
       jsr     $26,__do_frame_setup
       .align 3

 #
 # Invoke our destructors in order.
 #

.data

 # Support recursive calls to exit.
$ptr:	.quad	__DTOR_LIST__

.text

	.align 3
	.ent __do_global_dtors_aux

__do_global_dtors_aux:
	lda     $30,-16($30)
	.frame  $30,16,$26,0
	stq	$9,8($30)
	stq     $26,0($30)
	.mask   0x4000200,-16
	.prologue 0

	lda     $9,$ptr
	br      1f
0:	stq	$1,0($9)
	jsr     $26,($27)
1:	ldq	$1,0($9)
	ldq     $27,8($1)
	addq    $1,8,$1
	bne     $27,0b

	ldq     $26,0($30)
	ldq	$9,8($30)
	lda     $30,16($30)
	ret

	.end __do_global_dtors_aux

 #
 # Install our frame info.
 #

 # ??? How can we rationally keep this size correct?

.section .bss
	.type $object,@object
	.align 3
$object:
	.zero 48
	.size $object, 48

.text 

	.align 3
	.ent __do_frame_setup

__do_frame_setup:
	ldgp	$29,0($27)
	lda     $30,-16($30)
	.frame  $30,16,$26,0
	stq     $26,0($30)
	.mask   0x4000000,-16
	.prologue 1

	lda	$1,__register_frame_info
	beq	$1,0f
	lda	$16,__EH_FRAME_BEGIN__
	lda	$17,$object
	jsr	$26,__register_frame_info
	ldq     $26,0($30)
0:	lda     $30,16($30)
	ret

	.end __do_frame_setup

 #
 # Remove our frame info.
 #

	.align 3
	.ent __do_frame_takedown

__do_frame_takedown:
	ldgp	$29,0($27)
	lda     $30,-16($30)
	.frame  $30,16,$26,0
	stq     $26,0($30)
	.mask   0x4000000,-16
	.prologue 1

	lda	$1,__deregister_frame_info
	beq	$1,0f
	lda	$16,__EH_FRAME_BEGIN__
	jsr	$26,__deregister_frame_info
	ldq     $26,0($30)
0:	lda     $30,16($30)
	ret

	.end __do_frame_takedown

.weak __register_frame_info
.weak __deregister_frame_info
