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

.section .IA_64.unwind
__EH_FRAME_BEGIN__:

.section .sdata
	.type dtor_ptr#,@object
	.size dtor_ptr#,8
dtor_ptr:
	data8	__DTOR_LIST__# + 8

	.type segrel_ofs#,@object
	.size segrel_ofs#,8
segrel_ofs:
	data8	@segrel(.Lsegrel_ref#)

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
#ifdef HAVE_GAS_HIDDEN
	.hidden __dso_handle#
#endif

	/* The frame object.  */
	/* ??? How can we rationally keep this size correct?  */
.section .bss
	.type frame_object#,@object
	.size frame_object#,64
	.align 8
frame_object:
	.zero 64

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
	  movl r2 = @gprel(__do_global_dtors_aux#)
	  ;;
	}
	{ .mii
	  nop.m 0
	  add r2 = r2, gp
	  ;;
	  mov b6 = r2
	}
	{ .bbb
	  br.call.sptk.many b0 = b6
	  ;;
	}

/*
 * Fragment of the ELF _init routine that sets up the frame info.
 */

.section .init,"ax","progbits"
	{ .mlx
	  movl r2 = @gprel(__do_frame_setup#)
	  ;;
	}
	{ .mii
	  nop.m 0
	  add r2 = r2, gp
	  ;;
	  mov b6 = r2
	}
	{ .bbb
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
	  ld8 r16 = [r15], 8
	  ;;
	}
	{ .mfb
	  cmp.ne p6, p0 = r0, r16
(p6)	  br.cond.sptk.few 0b
	}
	/*
		if (__deregister_frame_info)
		  __deregister_frame_info(__EH_FRAME_BEGIN__)
	*/
	{ .mmi
	  mov gp = loc2
	  ;;
	  addl r16 = @ltoff(@fptr(__deregister_frame_info#)), gp
	  addl out0 = @ltoff(__EH_FRAME_BEGIN__#), gp
	  ;;
	}
	{ .mmi
	  ld8 r16 = [r16]
	  ld8 out0 = [out0]
	  ;;
	}
	{ .mmi
	  cmp.ne p7, p0 = r0, r16
	  ;;
(p7)	  ld8 r18 = [r16], 8
	  ;;
	}
	{ .mib
(p7)	  ld8 gp = [r16]
(p7)	  mov b6 = r18
(p7)	  br.call.sptk.many b0 = b6
	}
	{ .mii
	  mov gp = loc2
	  mov b0 = loc1
	  mov ar.pfs = loc3
	}
	{ .bbb
	  br.ret.sptk.many b0
	  ;;
	}
	.endp	__do_global_dtors_aux#

	.proc	__do_frame_setup#
__do_frame_setup:
	/*
		if (__register_frame_info)
		  __register_frame_info(__EH_FRAME_BEGIN__)
	*/
	{ .mii
	  alloc loc2 = ar.pfs, 0, 3, 2, 0
	  addl r16 = @ltoff(@fptr(__register_frame_info#)), gp
	  addl out0 = @ltoff(__EH_FRAME_BEGIN__#), gp
	}
	/* frame_object.pc_base = segment_base_offset;
	   pc_base is at offset 0 within frame_object.  */
.Lsegrel_ref:
	{ .mmi
	  addl out1 = @ltoff(frame_object#), gp
	  ;;
	  addl r2 = @gprel(segrel_ofs#), gp
	  mov r3 = ip
	  ;;
	}
	{ .mmi
	  ld8 r2 = [r2]
	  ld8 r16 = [r16]
	  mov loc0 = b0
	  ;;
	}
	{ .mii
	  ld8 out1 = [out1]
	  cmp.ne p7, p0 = r0, r16
	  sub r3 = r3, r2
	  ;;
	}
	{ .mmi
	  st8 [out1] = r3 
(p7)	  ld8 r18 = [r16], 8
	  mov loc1 = gp
	  ;;
	}
	{ .mfb
	  ld8 out0 = [out0]  
	}
	{ .mib
(p7)	  ld8 gp = [r16]
(p7)	  mov b6 = r18
(p7)	  br.call.sptk.many b0 = b6
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
	.endp	__do_frame_setup#

#ifdef SHARED
.weak __cxa_finalize#
#endif
.weak __deregister_frame_info#
.weak __register_frame_info#

	.text
	.align 16
	.global	__do_frame_setup_aux#
	.proc	__do_frame_setup_aux#
__do_frame_setup_aux:
	/*
		if (__register_frame_info_aux)
		  __register_frame_info_aux(__EH_FRAME_END__)
	*/
        alloc loc0 = ar.pfs, 0, 3, 1, 0
        addl r14 = @ltoff(@fptr(__register_frame_info_aux#)), gp
        mov loc1 = b0
        ;;
	// r16 contains the address of a pointer to __EH_FRAME_END__.
        ld8 out0 = [r16]
        ld8 r15 = [r14]
	mov loc2 = gp
        ;;
        cmp.eq p6, p7 = 0, r15
        (p6) br.cond.dptk 1f
        ld8 r8 = [r15], 8
        ;;
        ld8 gp = [r15]
        mov b6 = r8
        ;;
        br.call.sptk.many b0 = b6
	;;
1:
	mov gp = loc2
        mov ar.pfs = loc0
        mov b0 = loc1
        br.ret.sptk.many b0
	.endp	__do_frame_setup#
.weak __register_frame_info_aux#
