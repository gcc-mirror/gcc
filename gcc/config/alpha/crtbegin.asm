/*
 * Copyright (C) 1996, 1998, 2000 Free Software Foundation, Inc.
 *  Contributed by Richard Henderson (rth@tamu.edu)
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
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 * 
 *    As a special exception, if you link this library with files
 *    compiled with GCC to produce an executable, this does not cause
 *    the resulting executable to be covered by the GNU General Public License.
 *    This exception does not however invalidate any other reasons why
 *    the executable file might be covered by the GNU General Public License.
 */

#include "auto-host.h"


/*
 * Heads of the constructor/destructor lists.
 */

/* The __*TOR_LIST__ symbols are not global because when this file is used
   in a shared library, we do not want the symbol to fall over to the
   application's lists.  */

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

/*
 * Fragment of the ELF _fini routine that invokes our dtor cleanup.
 */

.section .fini,"ax"

	/* Since the bits of the _fini function are spread across many
	   object files, each potentially with its own GP, we must
	   assume we need to load ours.  Further, our .fini section
	   can easily be more than 4MB away from our .text bits so we
	   can't use bsr.  */

	br      $29,1f
1:	ldgp    $29,0($29)
	jsr     $26,__do_global_dtors_aux

	/* Must match the alignment we got from crti.o else we get
	   zero-filled holes in our _fini function and then SIGILL.  */
	.align 3

/*
 * Fragment of the ELF _init routine that sets up the frame info.
 */

.section .init,"ax"
       br      $29,1f
1:     ldgp    $29,0($29)
       jsr     $26,__do_frame_setup
       .align 3

/*
 * Invoke our destructors in order.
 */

.section .sdata

/* Support recursive calls to exit.  */
	.type dtor_ptr,@object
	.size dtor_ptr,8
dtor_ptr:
	.quad	__DTOR_LIST__ + 8

/* A globally unique widget for c++ local destructors to hang off.

   This has a unique value in every dso; in the main program its
   value is zero.  The object should be protected.  This means the
   instance in any dso or the main program is not used in any other
   dso.  The dynamic linker takes care of this.  */
 
	.global __dso_handle
	.type __dso_handle,@object
	.size __dso_handle,8
#ifdef SHARED
.section .data
	.align 3
__dso_handle:
	.quad	__dso_handle
#else
.section .bss
	.align 3
__dso_handle:
	.zero 8
#endif
#ifdef HAVE_GAS_HIDDEN
	.hidden	__dso_handle
#endif

.text

	.align 3
	.ent __do_global_dtors_aux

__do_global_dtors_aux:
	ldgp	$29,0($27)
	lda     $30,-16($30)
	.frame  $30,16,$26,0
	stq	$9,8($30)
	stq     $26,0($30)
	.mask   0x4000200,-16
	.prologue 1

#ifdef SHARED
	/* Do c++ local destructors.  */
	lda	$1,__cxa_finalize
	beq	$1,0f
	lda	$16,__dso_handle
	jsr	$26,__cxa_finalize
	ldgp	$29,0($26)
#endif

0:	lda     $9,dtor_ptr
	br      2f
1:	stq	$1,0($9)
	jsr     $26,($27)
	ldgp	$29,0($26)
2:	ldq	$1,0($9)
	ldq     $27,0($1)
	addq    $1,8,$1
	bne     $27,1b

	/* Remove our frame info.  */
	lda	$1,__deregister_frame_info
	beq	$1,3f
	lda	$16,__EH_FRAME_BEGIN__
	jsr	$26,__deregister_frame_info
	ldgp	$29,0($26)

3:	ldq     $26,0($30)
	ldq	$9,8($30)
	lda     $30,16($30)
	ret

	.end __do_global_dtors_aux

/*
 * Install our frame info.
 */

/* ??? How can we rationally keep this size correct?  */

.section .bss
	.type frame_object,@object
	.size frame_object, 48
	.align 3
frame_object:
	.zero 48

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
	lda	$17,frame_object
	jsr	$26,__register_frame_info
	ldgp	$29,0($26)

	ldq     $26,0($30)
0:	lda     $30,16($30)
	ret

	.end __do_frame_setup

.weak __register_frame_info
.weak __deregister_frame_info
#ifdef SHARED
.weak __cxa_finalize
#endif
