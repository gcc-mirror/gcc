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
5:	data8	@segrel(6f)
	.type dtor_ptr#,@object
	.size dtor_ptr#,8
dtor_ptr:
	data8	__DTOR_LIST__# + 8

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

/* The frame object.  */
/* ??? How can we rationally keep this size correct?  */

.section .bss
	.type frame_object#,@object
	.size frame_object#,56
	.align 8
frame_object:
	.zero 56

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

/*
 * Fragment of the ELF _init routine that sets up the frame info.
 */

.section .init,"ax","progbits"
	{ .mfb
	  st8 [r12] = gp, -16
	  br.call.sptk.many b0 = __do_frame_setup#
	  ;;
	}
	{ .mmi
	  adds r12 = 16, r12
	  ;;
	  ld8 gp = [r12]
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
	mov loc2 = gp
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
	mov loc2 = gp
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
	mov gp = loc2
	;;
	/*
		if (__deregister_frame_info)
		  __deregister_frame_info(__EH_FRAME_BEGIN__)
	*/
	{ .mii
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
	  alloc loc3 = ar.pfs, 0, 4, 2, 0
	  addl r16 = @ltoff(@fptr(__register_frame_info#)), gp
	  addl out0 = @ltoff(__EH_FRAME_BEGIN__#), gp
	  ;;
	}
	addl out1 = @ltoff(frame_object#), gp
	;;
	/* frame_object.pc_base = segment_base_offset;
	      pc_base is at offset 0 within frame_object.  */
6:
	mov loc0 = ip
	addl loc1 = @gprel(5b), gp
	;;
	ld8 loc1 = [loc1]
	ld8 out1 = [out1]
	;;
	sub loc2 = loc0, loc1
	;;
	st8 [out1] = loc2
	{ .mmi
	  ld8 r16 = [r16]
	  ld8 out0 = [out0]  
	  mov loc0 = b0
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
	  mov b0 = loc0
	  mov ar.pfs = loc3
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
