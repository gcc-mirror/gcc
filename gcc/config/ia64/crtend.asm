/* Copyright (C) 2000, 2001, 2003 Free Software Foundation, Inc.
   Contributed by Jes Sorensen, <Jes.Sorensen@cern.ch>

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "auto-host.h"

.section .ctors,"aw","progbits"
	.align	8
__CTOR_END__:
	data8	0

.section .dtors,"aw","progbits"
	.align 8
__DTOR_END__:
	data8	0

.section .jcr,"aw","progbits"
	.align 8
__JCR_END__:
	data8	0

#ifndef HAVE_INITFINI_ARRAY
/*
 * Fragment of the ELF _init routine that invokes our dtor cleanup.
 *
 * We make the call by indirection, because in large programs the 
 * .fini and .init sections are not in range of the destination, and
 * we cannot allow the linker to insert a stub at the end of this
 * fragment of the _fini function.  Further, Itanium does not implement
 * the long branch instructions, and we do not wish every program to
 * trap to the kernel for emulation.
 *
 * Note that we require __do_global_ctors_aux to preserve the GP,
 * so that the next fragment in .fini gets the right value.
 */
.section .init,"ax","progbits"
	{ .mlx
	  movl r2 = @pcrel(__do_global_ctors_aux# - 16)
	}
	{ .mii
	  mov r3 = ip
	  ;;
	  add r2 = r2, r3
	  ;;
	}
	{ .mib
	  mov b6 = r2
	  br.call.sptk.many b0 = b6
	  ;;
	}
#endif /* !HAVE_INITFINI_ARRAY */

.text
	.align 16
#ifdef HAVE_INITFINI_ARRAY
	/* This is referenced from crtbegin.o.  */
	.globl __do_global_ctors_aux#
	.type __do_global_ctors_aux#,@function
	.hidden __do_global_ctors_aux#
#endif
	.proc __do_global_ctors_aux#
__do_global_ctors_aux:
	/*
		for (loc0 = __CTOR_END__-1; *p != -1; --p)
		  (*p) ();
	*/
	{ .mlx
	  alloc loc4 = ar.pfs, 0, 5, 0, 0
	  movl loc0 = @gprel(__CTOR_END__# - 8)
	  ;;
	}
	{ .mmi
	  add loc0 = loc0, gp
	  mov loc1 = b0
	  ;;
	}
	{
	  .mmi
	  ld8 loc3 = [loc0], -8
	  mov loc2 = gp
	  ;;
	}
	{ .mfb
	  cmp.eq p6, p0 = -1, loc3
(p6)	  br.cond.spnt.few 2f
	}
0:
	{ .mmi
	  ld8 r15 = [loc3], 8
	  ;;
	  ld8 gp = [loc3]
	  mov b6 = r15
	}
	{ .mfb
	  ld8 loc3 = [loc0], -8
	  br.call.sptk.many b0 = b6
	  ;;
	}
	{ .mfb
	  cmp.ne p6, p0 = -1, loc3
(p6)	  br.cond.sptk.few 0b
	}
2:
	{ .mii
	  mov gp = loc2
	  mov b0 = loc1
	  mov ar.pfs = loc4
	}
	{ .bbb
	  br.ret.sptk.many b0
	  ;;
	}
	.endp __do_global_ctors_aux#
