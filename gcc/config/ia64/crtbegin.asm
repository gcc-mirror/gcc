/* Copyright (C) 2000, 2001 Free Software Foundation, Inc.
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

.section .jcr,"aw","progbits"
	.align	8
__JCR_LIST__:

.section .sdata
	.type dtor_ptr#,@object
	.size dtor_ptr#,8
dtor_ptr:
	data8	@gprel(__DTOR_LIST__# + 8)

	/* A handle for __cxa_finalize to manage c++ local destructors.  */
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
	.hidden __dso_handle#


/*
 * Fragment of the ELF _fini routine that invokes our dtor cleanup.
 *
 * We make the call by indirection, because in large programs the 
 * .fini and .init sections are not in range of the destination, and
 * we cannot allow the linker to insert a stub at the end of this
 * fragment of the _fini function.  Further, Itanium does not implement
 * the long branch instructions, and we do not wish every program to
 * trap to the kernel for emulation.
 *
 * Note that we require __do_global_dtors_aux to preserve the GP,
 * so that the next fragment in .fini gets the right value.
 */
.section .fini,"ax","progbits"
	{ .mlx
	  movl r2 = @pcrel(__do_global_dtors_aux# - 16)
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

/* Likewise for _init.  */

.section .init,"ax","progbits"
	{ .mlx
	  movl r2 = @pcrel(__do_jv_register_classes# - 16)
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

.section .text
	.align	16
	.proc	__do_global_dtors_aux#
__do_global_dtors_aux:
#ifndef SHARED
	{ .mii
	  alloc loc3 = ar.pfs, 0, 4, 1, 0
	  addl loc0 = @gprel(dtor_ptr#), gp
	  mov loc1 = b0
	}
	{ .mib
	  mov loc2 = gp
	  br.sptk.few 1f
	  ;;
	}
#else
	/*
		if (__cxa_finalize)
		  __cxa_finalize(__dso_handle)
	*/
	{ .mii
	  alloc loc3 = ar.pfs, 0, 4, 1, 0
	  addl loc0 = @gprel(dtor_ptr#), gp
	  addl r16 = @ltoff(@fptr(__cxa_finalize#)), gp
	  ;;
	}
	{ .mmi
	  ld8 r16 = [r16]
	  ;;
	  addl out0 = @ltoff(__dso_handle#), gp
	  cmp.ne p7, p0 = r0, r16
	  ;;
	}
	{ .mmi
	  ld8 out0 = [out0]
(p7)	  ld8 r18 = [r16], 8
	  mov loc1 = b0
	  ;;
	}
	{ .mfi
	  mov loc2 = gp
(p7)	  mov b6 = r18
	}
	{
	  .mfb
(p7)	  ld8 gp = [r16]
(p7)	  br.call.sptk.many b0 = b6
	}
	{ .mfb
	  br.sptk.few 1f
	}
#endif
	/*
		do {
		  dtor_ptr++;
		  (*(dtor_ptr-1)) ();
		} while (dtor_ptr);
	*/
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
	  add r16 = r15, loc2
	  adds r15 = 8, r15
	  ;;
	}
	{ .mmi
	  ld8 r16 = [r16]
	  mov gp = loc2
	  mov b0 = loc1
	  ;;
	}
	{ .mib
	  cmp.ne p6, p0 = r0, r16
	  mov ar.pfs = loc3
(p6)	  br.cond.sptk.few 0b
	}
	{ .bbb
	  br.ret.sptk.many b0
	  ;;
	}
	.endp	__do_global_dtors_aux#

	.align	16
	.proc	__do_jv_register_classes#
__do_jv_register_classes:
	{ .mlx
	  alloc loc2 = ar.pfs, 0, 3, 1, 0
	  movl out0 = @gprel(__JCR_LIST__)
	  ;;
	}
	{ .mmi
	  addl r14 = @ltoff(@fptr(_Jv_RegisterClasses)), gp
	  add out0 = out0, gp
	  ;;
	}
	{ .mmi
	  ld8 r14 = [r14]
	  ld8 r15 = [out0]
	  cmp.ne p6, p0 = r0, r0
	  ;;
	}
	{ .mib
	  cmp.eq.or p6, p0 = r0, r14
	  cmp.eq.or p6, p0 = r0, r15
(p6)	  br.ret.sptk.many b0
	}
	{ .mii
	  ld8 r15 = [r14], 8
	  mov loc0 = b0
	  mov loc1 = gp
	  ;;
	}
	{ .mib
	  ld8 gp = [r14]
	  mov b6 = r15
	  br.call.sptk.many b0 = b6
	  ;;
	}
	{ .mii
	  mov gp = loc1
	  mov b0 = loc0
	  mov ar.pfs = loc2
	}
	{ .bbb
	  br.ret.sptk.many b0
	  ;;
	}
	.endp	__do_jv_register_classes#

#ifdef SHARED
.weak __cxa_finalize#
#endif
.weak _Jv_RegisterClasses
