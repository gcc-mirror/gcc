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
__CTOR_END__:
	data8	0

.section .dtors,"aw","progbits"
	.align 8
__DTOR_END__:
	data8	0

.section .IA_64.unwind
__EH_FRAME_END__:
	data8	-1

/*
 * Fragment of the ELF _init routine that invokes our dtor cleanup.
 *
 * The code going into .init is spread all over the place, thus we need
 * to save gp in order to make sure that other bits don't get into any
 * nasty surprises by expecting a gp that has suddenly changed.
 */
.section .init,"ax","progbits"
	{ .mfb
	  st8 [r12] = gp, -16
	  br.call.sptk.many b0 = __do_global_ctors_aux
	  ;;
	}
	{ .mmi
	  adds r12 = 16, r12
	  ;;
	  ld8 gp = [r12]
	  ;;
	}

.text
	.align 16
	.proc __do_global_ctors_aux#
__do_global_ctors_aux:
	/*
		for (loc0 = __CTOR_END__-1; *p != -1; --p)
		  (*p) ();
	*/
	{ .mii
	  alloc loc2 = ar.pfs, 0, 4, 0, 0
	  addl loc0 = @ltoff(__CTOR_END__# - 8), gp
	  cmp.ne p6, p0 = r0, r0
	  ;;
	}
	{ .mfi
	  ld8 loc0 = [loc0]
	  mov loc1 = b0
	}
0:
	{ .mmi
(p6)	  ld8 r15 = [loc3], 8
	  ;;
(p6)	  ld8 gp = [loc3]
(p6)	  mov b6 = r15
	}
	{ .mfb
	  ld8 loc3 = [loc0], -8
(p6)	  br.call.sptk.many b0 = b6
	  ;;
	}
	{ .mfb
	  cmp.ne p6, p0 = -1, loc3
(p6)	  br.cond.sptk.few 0b
	}
	{ .mii
	  mov ar.pfs = loc2
	  mov b0 = loc1
	}
	{ .bbb
	  br.ret.sptk.many b0
	  ;;
	}
	.endp __do_global_ctors_aux#
