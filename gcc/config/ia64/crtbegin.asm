/* Copyright (C) 2000 Free Software Foundation, Inc.
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

.section .ctors,"aw","progbits"
	.align	8
__CTOR_LIST__:
	data8	-1

.section .dtors,"aw","progbits"
	.align	8
__DTOR_LIST__:
	data8	-1

.section .sdata
	.type dtor_ptr#,@object
	.size dtor_ptr#,8
dtor_ptr:
	data8	__DTOR_LIST__# + 8

	.global __dso_handle#
	.type __dso_handle#,@object
	.size __dso_handle#,8
#ifdef SHARED
.section .data
__dso_handle:
	data8	__dso_handle#
#else
.section .bss
__dso_handle:
	data8	0
#endif

/*
 * Fragment of the ELF _fini routine that invokes our dtor cleanup.
 *
 * The code going into .fini is spread all over the place, thus we need
 * to save gp in order to make sure that other bits don't get into any
 * nasty surprises by expecting a gp that has suddenly changed.
 */
.section .fini,"ax","progbits"
	{ .mfb
	  st8 [r12] = gp, -16
	  br.call.sptk.many b0 = __do_global_dtors_aux#
	  ;;
	}
	{ .mmi
	  adds r12 = 16, r12
	  ;;
	  ld8 gp = [r12]
	  ;;
	}

.text

	.align	16
	.proc	__do_global_dtors_aux#

__do_global_dtors_aux:
#ifndef SHARED
	{ .mii
	  alloc loc2 = ar.pfs, 0, 3, 0, 0
	  addl loc0 = @gprel(dtor_ptr#), gp
	  mov loc1 = b0
	}
#else
	/*
		if (__cxa_finalize)
		  __cxa_finalize(__dso_handle)
	*/
	{ .mii
	  alloc loc2 = ar.pfs, 1, 3, 0, 0
	  addl loc0 = @gprel(dtor_ptr#), gp
	  addl r16 = @ltoff(@fptr(__cxa_finalize#)), gp
	  ;;
	}
	{ .mmi
	  ld8 r16 = [r16]
	  ;;
	  addl r32 = @ltoff(__dso_handle#), gp
	  cmp.ne p7, p0 = r0, r16
	  ;;
	}
	{ .mmi
	  ld8 r32 = [r32]
(p7)	  ld8 r18 = [r16], 8
	  mov loc1 = b0
	  ;;
	}
	{ .mib
(p7)	  ld8 gp = [r16]
(p7)	  mov b6 = r18
(p7)	  br.call.sptk.many b0 = b6
	}
#endif
	/*
		do {
		  dtor_ptr++;
		  (*(dtor_ptr-1)) ();
		} while (dtor_ptr);
	*/
	{ .bbb
	  br.sptk.few 1f
	  ;;
	}
0:
	{ .mmi
	  st8 [loc0] = r15
	  ld8 r17 = [r16], 8
	  ;;
	}
	{ .mib
	  ld8 gp = [r16]
	  mov b6 = r17
	  br.call.sptk.many b0 = b6
	}
1:
	{ .mmi
	  ld8 r15 = [loc0]
	  ;;
	  ld8 r16 = [r15], 8
	  ;;
	}
	{ .mfb
	  cmp.ne p6, p0 = r0, r16
(p6)	  br.cond.sptk.few 0b
	}
	{ .mii
	  mov b0 = loc1
	  mov ar.pfs = loc2
	}
	{ .bbb
	  br.ret.sptk.many b0
	  ;;
	}
	.endp	__do_global_dtors_aux#

#ifdef SHARED
.weak __cxa_finalize#
#endif
