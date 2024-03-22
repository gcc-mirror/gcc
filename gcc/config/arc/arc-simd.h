/* Synopsys DesignWare ARC SIMD include file.
   Copyright (C) 2007-2024 Free Software Foundation, Inc.
   Written by Saurabh Verma (saurabh.verma@celunite.com) on behalf os Synopsys
   Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */

#ifndef _ARC_SIMD_H
#define _ARC_SIMD_H 1

#ifndef __ARC_SIMD__
#error Use the "-msimd" flag to enable ARC SIMD support
#endif

/* I0-I7 registers.  */
#define _IREG_I0  0
#define _IREG_I1  1
#define _IREG_I2  2
#define _IREG_I3  3
#define _IREG_I4  4
#define _IREG_I5  5
#define _IREG_I6  6
#define _IREG_I7  7

/* DMA configuration registers.  */
#define _DMA_REG_DR0		0
#define _DMA_SDM_SRC_ADR_REG	_DMA_REG_DR0
#define _DMA_SDM_DEST_ADR_REG	_DMA_REG_DR0

#define _DMA_REG_DR1		1
#define _DMA_SDM_STRIDE_REG	_DMA_REG_DR1

#define _DMA_REG_DR2		2
#define _DMA_BLK_REG		_DMA_REG_DR2

#define _DMA_REG_DR3		3
#define _DMA_LOC_REG		_DMA_REG_DR3

#define _DMA_REG_DR4		4
#define _DMA_SYS_SRC_ADR_REG	_DMA_REG_DR4
#define _DMA_SYS_DEST_ADR_REG	_DMA_REG_DR4

#define _DMA_REG_DR5		5
#define _DMA_SYS_STRIDE_REG	_DMA_REG_DR5

#define _DMA_REG_DR6		6
#define _DMA_CFG_REG		_DMA_REG_DR6

#define _DMA_REG_DR7		7
#define _DMA_FT_BASE_ADR_REG	_DMA_REG_DR7

/* Predefined types used in vector instructions.  */
typedef int   __v4si  __attribute__((vector_size(16)));
typedef short __v8hi  __attribute__((vector_size(16)));

/* Synonyms */
#define _vaddaw    __builtin_arc_vaddaw
#define _vaddw     __builtin_arc_vaddw
#define _vavb      __builtin_arc_vavb
#define _vavrb     __builtin_arc_vavrb
#define _vdifaw    __builtin_arc_vdifaw
#define _vdifw     __builtin_arc_vdifw
#define _vmaxaw    __builtin_arc_vmaxaw
#define _vmaxw     __builtin_arc_vmaxw
#define _vminaw    __builtin_arc_vminaw
#define _vminw     __builtin_arc_vminw
#define _vmulaw    __builtin_arc_vmulaw
#define _vmulfaw   __builtin_arc_vmulfaw
#define _vmulfw    __builtin_arc_vmulfw
#define _vmulw     __builtin_arc_vmulw
#define _vsubaw    __builtin_arc_vsubaw
#define _vsubw     __builtin_arc_vsubw
#define _vsummw    __builtin_arc_vsummw
#define _vand      __builtin_arc_vand
#define _vandaw    __builtin_arc_vandaw
#define _vbic      __builtin_arc_vbic
#define _vbicaw    __builtin_arc_vbicaw
#define _vor       __builtin_arc_vor
#define _vxor      __builtin_arc_vxor
#define _vxoraw    __builtin_arc_vxoraw
#define _veqw      __builtin_arc_veqw
#define _vlew      __builtin_arc_vlew
#define _vltw      __builtin_arc_vltw
#define _vnew      __builtin_arc_vnew
#define _vmr1aw    __builtin_arc_vmr1aw
#define _vmr1w     __builtin_arc_vmr1w
#define _vmr2aw    __builtin_arc_vmr2aw
#define _vmr2w     __builtin_arc_vmr2w
#define _vmr3aw    __builtin_arc_vmr3aw
#define _vmr3w     __builtin_arc_vmr3w
#define _vmr4aw    __builtin_arc_vmr4aw
#define _vmr4w     __builtin_arc_vmr4w
#define _vmr5aw    __builtin_arc_vmr5aw
#define _vmr5w     __builtin_arc_vmr5w
#define _vmr6aw    __builtin_arc_vmr6aw
#define _vmr6w     __builtin_arc_vmr6w
#define _vmr7aw    __builtin_arc_vmr7aw
#define _vmr7w     __builtin_arc_vmr7w
#define _vmrb      __builtin_arc_vmrb
#define _vh264f    __builtin_arc_vh264f
#define _vh264ft   __builtin_arc_vh264ft
#define _vh264fw   __builtin_arc_vh264fw
#define _vvc1f     __builtin_arc_vvc1f
#define _vvc1ft    __builtin_arc_vvc1ft
#define _vbaddw    __builtin_arc_vbaddw
#define _vbmaxw    __builtin_arc_vbmaxw
#define _vbminw    __builtin_arc_vbminw
#define _vbmulaw   __builtin_arc_vbmulaw
#define _vbmulfw   __builtin_arc_vbmulfw
#define _vbmulw    __builtin_arc_vbmulw
#define _vbrsubw   __builtin_arc_vbrsubw
#define _vbsubw    __builtin_arc_vbsubw
#define _vasrw     __builtin_arc_vasrw
#define _vsr8      __builtin_arc_vsr8
#define _vsr8aw    __builtin_arc_vsr8aw
#define _vasrrwi   __builtin_arc_vasrrwi
#define _vasrsrwi  __builtin_arc_vasrsrwi
#define _vasrwi    __builtin_arc_vasrwi
#define _vasrpwbi  __builtin_arc_vasrpwbi
#define _vasrrpwbi __builtin_arc_vasrrpwbi
#define _vsr8awi   __builtin_arc_vsr8awi
#define _vsr8i     __builtin_arc_vsr8i
#define _vmvaw     __builtin_arc_vmvaw
#define _vmvw      __builtin_arc_vmvw
#define _vmvzw     __builtin_arc_vmvzw
#define _vd6tapf   __builtin_arc_vd6tapf
#define _vmovaw    __builtin_arc_vmovaw
#define _vmovw     __builtin_arc_vmovw
#define _vmovzw    __builtin_arc_vmovzw
#define _vabsaw    __builtin_arc_vabsaw
#define _vabsw     __builtin_arc_vabsw
#define _vaddsuw   __builtin_arc_vaddsuw
#define _vsignw    __builtin_arc_vsignw
#define _vexch1    __builtin_arc_vexch1
#define _vexch2    __builtin_arc_vexch2
#define _vexch4    __builtin_arc_vexch4
#define _vupbaw    __builtin_arc_vupbaw
#define _vupbw     __builtin_arc_vupbw
#define _vupsbaw   __builtin_arc_vupsbaw
#define _vupsbw    __builtin_arc_vupsbw
#define _vdirun    __builtin_arc_vdirun
#define _vdorun    __builtin_arc_vdorun
#define _vdiwr     __builtin_arc_vdiwr
#define _vdowr     __builtin_arc_vdowr
#define _vrec      __builtin_arc_vrec
#define _vrun      __builtin_arc_vrun
#define _vrecrun   __builtin_arc_vrecrun
#define _vendrec   __builtin_arc_vendrec
#define _vld32wh   __builtin_arc_vld32wh
#define _vld32wl   __builtin_arc_vld32wl
#define _vld64     __builtin_arc_vld64
#define _vld32     __builtin_arc_vld32
#define _vld64w    __builtin_arc_vld64w
#define _vld128    __builtin_arc_vld128
#define _vst128    __builtin_arc_vst128
#define _vst64     __builtin_arc_vst64
#define _vst16_n   __builtin_arc_vst16_n
#define _vst32_n   __builtin_arc_vst32_n
#define _vinti     __builtin_arc_vinti

/* Additional synonyms to ease programming.  */
#define _setup_dma_in_channel_reg  _vdiwr
#define _setup_dma_out_channel_reg _vdowr

#endif /* _ARC_SIMD_H */
