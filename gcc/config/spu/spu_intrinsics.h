/* Definitions of Synergistic Processing Unit (SPU). */
/* Copyright (C) 2006-2016 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef  _SPU_INTRINSICS_H
#define _SPU_INTRINSICS_H 
 
#define vec_uchar16             __vector unsigned char
#define vec_char16              __vector   signed char
#define vec_ushort8             __vector unsigned short
#define vec_short8              __vector   signed short
#define vec_uint4               __vector unsigned int
#define vec_int4                __vector   signed int
#define vec_ullong2             __vector unsigned long long
#define vec_llong2              __vector   signed long long
#define vec_float4              __vector          float
#define vec_double2             __vector          double

/* SPU Channel Defines 
 */
#define SPU_RdEventStat		 0
#define SPU_WrEventMask		 1
#define SPU_WrEventAck		 2
#define SPU_RdSigNotify1	 3
#define SPU_RdSigNotify2	 4
#define SPU_WrDec		 7
#define SPU_RdDec		 8
#define SPU_RdEventMask		11
#define SPU_RdMachStat		13
#define SPU_WrSRR0		14
#define SPU_RdSRR0		15
#define SPU_WrOutMbox		28 
#define SPU_RdInMbox		29 
#define SPU_WrOutIntrMbox	30 

/* MFC Channel Defines. 
 */
#define MFC_WrMSSyncReq		 9
#define MFC_RdTagMask		12
#define MFC_LSA			16 
#define MFC_EAH			17 
#define MFC_EAL			18 
#define MFC_Size		19 
#define MFC_TagID		20 
#define MFC_Cmd			21 
#define MFC_WrTagMask		22 
#define MFC_WrTagUpdate		23 
#define MFC_RdTagStat		24 
#define MFC_RdListStallStat	25 
#define MFC_WrListStallAck	26 
#define MFC_RdAtomicStat	27 

/* Bit flag mnemonics for test special value.
 */
#define SPU_SV_NEG_DENORM       0x01    /* negative denormalized number  */
#define SPU_SV_POS_DENORM       0x02    /* positive denormalized number  */
#define SPU_SV_NEG_ZERO         0x04    /* negative zero                 */
#define SPU_SV_POS_ZERO         0x08    /* positive zero                 */
#define SPU_SV_NEG_INFINITY     0x10    /* negative infinity             */
#define SPU_SV_POS_INFINITY     0x20    /* positive infinity             */
#define SPU_SV_NAN              0x40    /* not a number                  */

#include <spu_internals.h>

#endif /* _SPU_INTRINSICS_H */
