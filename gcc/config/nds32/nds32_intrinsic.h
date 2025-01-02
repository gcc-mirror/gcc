/* Intrinsic definitions of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2025 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _NDS32_INTRINSIC_H
#define _NDS32_INTRINSIC_H

typedef signed char int8x4_t __attribute ((vector_size(4)));
typedef short int16x2_t __attribute ((vector_size(4)));
typedef int int32x2_t __attribute__((vector_size(8)));
typedef unsigned char uint8x4_t __attribute__ ((vector_size (4)));
typedef unsigned short uint16x2_t __attribute__ ((vector_size (4)));
typedef unsigned int uint32x2_t __attribute__((vector_size(8)));

/* General instrinsic register names.  */
enum nds32_intrinsic_registers
{
  __NDS32_REG_CPU_VER__ = 1024,
  __NDS32_REG_ICM_CFG__,
  __NDS32_REG_DCM_CFG__,
  __NDS32_REG_MMU_CFG__,
  __NDS32_REG_MSC_CFG__,
  __NDS32_REG_MSC_CFG2__,
  __NDS32_REG_CORE_ID__,
  __NDS32_REG_FUCOP_EXIST__,

  __NDS32_REG_PSW__,
  __NDS32_REG_IPSW__,
  __NDS32_REG_P_IPSW__,
  __NDS32_REG_IVB__,
  __NDS32_REG_EVA__,
  __NDS32_REG_P_EVA__,
  __NDS32_REG_ITYPE__,
  __NDS32_REG_P_ITYPE__,

  __NDS32_REG_MERR__,
  __NDS32_REG_IPC__,
  __NDS32_REG_P_IPC__,
  __NDS32_REG_OIPC__,
  __NDS32_REG_P_P0__,
  __NDS32_REG_P_P1__,

  __NDS32_REG_INT_MASK__,
  __NDS32_REG_INT_MASK2__,
  __NDS32_REG_INT_MASK3__,
  __NDS32_REG_INT_PEND__,
  __NDS32_REG_INT_PEND2__,
  __NDS32_REG_INT_PEND3__,
  __NDS32_REG_SP_USR__,
  __NDS32_REG_SP_PRIV__,
  __NDS32_REG_INT_PRI__,
  __NDS32_REG_INT_PRI2__,
  __NDS32_REG_INT_PRI3__,
  __NDS32_REG_INT_PRI4__,
  __NDS32_REG_INT_CTRL__,
  __NDS32_REG_INT_TRIGGER__,
  __NDS32_REG_INT_TRIGGER2__,
  __NDS32_REG_INT_GPR_PUSH_DIS__,

  __NDS32_REG_MMU_CTL__,
  __NDS32_REG_L1_PPTB__,
  __NDS32_REG_TLB_VPN__,
  __NDS32_REG_TLB_DATA__,
  __NDS32_REG_TLB_MISC__,
  __NDS32_REG_VLPT_IDX__,
  __NDS32_REG_ILMB__,
  __NDS32_REG_DLMB__,

  __NDS32_REG_CACHE_CTL__,
  __NDS32_REG_HSMP_SADDR__,
  __NDS32_REG_HSMP_EADDR__,
  __NDS32_REG_SDZ_CTL__,
  __NDS32_REG_N12MISC_CTL__,
  __NDS32_REG_MISC_CTL__,
  __NDS32_REG_ECC_MISC__,

  __NDS32_REG_BPC0__,
  __NDS32_REG_BPC1__,
  __NDS32_REG_BPC2__,
  __NDS32_REG_BPC3__,
  __NDS32_REG_BPC4__,
  __NDS32_REG_BPC5__,
  __NDS32_REG_BPC6__,
  __NDS32_REG_BPC7__,

  __NDS32_REG_BPA0__,
  __NDS32_REG_BPA1__,
  __NDS32_REG_BPA2__,
  __NDS32_REG_BPA3__,
  __NDS32_REG_BPA4__,
  __NDS32_REG_BPA5__,
  __NDS32_REG_BPA6__,
  __NDS32_REG_BPA7__,

  __NDS32_REG_BPAM0__,
  __NDS32_REG_BPAM1__,
  __NDS32_REG_BPAM2__,
  __NDS32_REG_BPAM3__,
  __NDS32_REG_BPAM4__,
  __NDS32_REG_BPAM5__,
  __NDS32_REG_BPAM6__,
  __NDS32_REG_BPAM7__,

  __NDS32_REG_BPV0__,
  __NDS32_REG_BPV1__,
  __NDS32_REG_BPV2__,
  __NDS32_REG_BPV3__,
  __NDS32_REG_BPV4__,
  __NDS32_REG_BPV5__,
  __NDS32_REG_BPV6__,
  __NDS32_REG_BPV7__,

  __NDS32_REG_BPCID0__,
  __NDS32_REG_BPCID1__,
  __NDS32_REG_BPCID2__,
  __NDS32_REG_BPCID3__,
  __NDS32_REG_BPCID4__,
  __NDS32_REG_BPCID5__,
  __NDS32_REG_BPCID6__,
  __NDS32_REG_BPCID7__,

  __NDS32_REG_EDM_CFG__,
  __NDS32_REG_EDMSW__,
  __NDS32_REG_EDM_CTL__,
  __NDS32_REG_EDM_DTR__,
  __NDS32_REG_BPMTC__,
  __NDS32_REG_DIMBR__,

  __NDS32_REG_TECR0__,
  __NDS32_REG_TECR1__,
  __NDS32_REG_PFMC0__,
  __NDS32_REG_PFMC1__,
  __NDS32_REG_PFMC2__,
  __NDS32_REG_PFM_CTL__,
  __NDS32_REG_PFT_CTL__,
  __NDS32_REG_HSP_CTL__,
  __NDS32_REG_SP_BOUND__,
  __NDS32_REG_SP_BOUND_PRIV__,
  __NDS32_REG_SP_BASE__,
  __NDS32_REG_SP_BASE_PRIV__,
  __NDS32_REG_FUCOP_CTL__,
  __NDS32_REG_PRUSR_ACC_CTL__,

  __NDS32_REG_DMA_CFG__,
  __NDS32_REG_DMA_GCSW__,
  __NDS32_REG_DMA_CHNSEL__,
  __NDS32_REG_DMA_ACT__,
  __NDS32_REG_DMA_SETUP__,
  __NDS32_REG_DMA_ISADDR__,
  __NDS32_REG_DMA_ESADDR__,
  __NDS32_REG_DMA_TCNT__,
  __NDS32_REG_DMA_STATUS__,
  __NDS32_REG_DMA_2DSET__,
  __NDS32_REG_DMA_2DSCTL__,
  __NDS32_REG_DMA_RCNT__,
  __NDS32_REG_DMA_HSTATUS__,

  __NDS32_REG_PC__,
  __NDS32_REG_SP_USR1__,
  __NDS32_REG_SP_USR2__,
  __NDS32_REG_SP_USR3__,
  __NDS32_REG_SP_PRIV1__,
  __NDS32_REG_SP_PRIV2__,
  __NDS32_REG_SP_PRIV3__,
  __NDS32_REG_BG_REGION__,
  __NDS32_REG_SFCR__,
  __NDS32_REG_SIGN__,
  __NDS32_REG_ISIGN__,
  __NDS32_REG_P_ISIGN__,
  __NDS32_REG_IFC_LP__,
  __NDS32_REG_ITB__
};

/* The cctl subtype for intrinsic.  */
enum nds32_cctl_valck
{
  __NDS32_CCTL_L1D_VA_FILLCK__,
  __NDS32_CCTL_L1D_VA_ULCK__,
  __NDS32_CCTL_L1I_VA_FILLCK__,
  __NDS32_CCTL_L1I_VA_ULCK__
};

enum nds32_cctl_idxwbinv
{
  __NDS32_CCTL_L1D_IX_WBINVAL__,
  __NDS32_CCTL_L1D_IX_INVAL__,
  __NDS32_CCTL_L1D_IX_WB__,
  __NDS32_CCTL_L1I_IX_INVAL__
};

enum nds32_cctl_vawbinv
{
  __NDS32_CCTL_L1D_VA_INVAL__,
  __NDS32_CCTL_L1D_VA_WB__,
  __NDS32_CCTL_L1D_VA_WBINVAL__,
  __NDS32_CCTL_L1I_VA_INVAL__
};

enum nds32_cctl_idxread
{
  __NDS32_CCTL_L1D_IX_RTAG__,
  __NDS32_CCTL_L1D_IX_RWD__,
  __NDS32_CCTL_L1I_IX_RTAG__,
  __NDS32_CCTL_L1I_IX_RWD__
};

enum nds32_cctl_idxwrite
{
  __NDS32_CCTL_L1D_IX_WTAG__,
  __NDS32_CCTL_L1D_IX_WWD__,
  __NDS32_CCTL_L1I_IX_WTAG__,
  __NDS32_CCTL_L1I_IX_WWD__
};

enum nds32_dpref
{
  __NDS32_DPREF_SRD__,
  __NDS32_DPREF_MRD__,
  __NDS32_DPREF_SWR__,
  __NDS32_DPREF_MWR__,
  __NDS32_DPREF_PTE__,
  __NDS32_DPREF_CLWR__
};

/* ------------------------------------------------------------------------ */

/* Define interrupt number for intrinsic function.  */
#define NDS32_INT_H0 0
#define NDS32_INT_H1 1
#define NDS32_INT_H2 2
#define NDS32_INT_H3 3
#define NDS32_INT_H4 4
#define NDS32_INT_H5 5
#define NDS32_INT_H6 6
#define NDS32_INT_H7 7
#define NDS32_INT_H8 8
#define NDS32_INT_H9 9
#define NDS32_INT_H10 10
#define NDS32_INT_H11 11
#define NDS32_INT_H12 12
#define NDS32_INT_H13 13
#define NDS32_INT_H14 14
#define NDS32_INT_H15 15
#define NDS32_INT_H16 16
#define NDS32_INT_H17 17
#define NDS32_INT_H18 18
#define NDS32_INT_H19 19
#define NDS32_INT_H20 20
#define NDS32_INT_H21 21
#define NDS32_INT_H22 22
#define NDS32_INT_H23 23
#define NDS32_INT_H24 24
#define NDS32_INT_H25 25
#define NDS32_INT_H26 26
#define NDS32_INT_H27 27
#define NDS32_INT_H28 28
#define NDS32_INT_H29 29
#define NDS32_INT_H30 30
#define NDS32_INT_H31 31
#define NDS32_INT_H32 32
#define NDS32_INT_H33 33
#define NDS32_INT_H34 34
#define NDS32_INT_H35 35
#define NDS32_INT_H36 36
#define NDS32_INT_H37 37
#define NDS32_INT_H38 38
#define NDS32_INT_H39 39
#define NDS32_INT_H40 40
#define NDS32_INT_H41 41
#define NDS32_INT_H42 42
#define NDS32_INT_H43 43
#define NDS32_INT_H44 44
#define NDS32_INT_H45 45
#define NDS32_INT_H46 46
#define NDS32_INT_H47 47
#define NDS32_INT_H48 48
#define NDS32_INT_H49 49
#define NDS32_INT_H50 50
#define NDS32_INT_H51 51
#define NDS32_INT_H52 52
#define NDS32_INT_H53 53
#define NDS32_INT_H54 54
#define NDS32_INT_H55 55
#define NDS32_INT_H56 56
#define NDS32_INT_H57 57
#define NDS32_INT_H58 58
#define NDS32_INT_H59 59
#define NDS32_INT_H60 60
#define NDS32_INT_H61 61
#define NDS32_INT_H62 62
#define NDS32_INT_H63 63
#define NDS32_INT_SWI 64
#define NDS32_INT_ALZ 65
#define NDS32_INT_IDIVZE 66
#define NDS32_INT_DSSIM 67

/* ------------------------------------------------------------------------ */

/* Define intrinsic register name macro for compatibility.  */
#define NDS32_SR_CPU_VER               __NDS32_REG_CPU_VER__
#define NDS32_SR_ICM_CFG               __NDS32_REG_ICM_CFG__
#define NDS32_SR_DCM_CFG               __NDS32_REG_DCM_CFG__
#define NDS32_SR_MMU_CFG               __NDS32_REG_MMU_CFG__
#define NDS32_SR_MSC_CFG               __NDS32_REG_MSC_CFG__
#define NDS32_SR_MSC_CFG2              __NDS32_REG_MSC_CFG2__
#define NDS32_SR_CORE_ID               __NDS32_REG_CORE_ID__
#define NDS32_SR_FUCOP_EXIST           __NDS32_REG_FUCOP_EXIST__
#define NDS32_SR_PSW                   __NDS32_REG_PSW__
#define NDS32_SR_IPSW                  __NDS32_REG_IPSW__
#define NDS32_SR_P_IPSW                __NDS32_REG_P_IPSW__
#define NDS32_SR_IVB                   __NDS32_REG_IVB__
#define NDS32_SR_EVA                   __NDS32_REG_EVA__
#define NDS32_SR_P_EVA                 __NDS32_REG_P_EVA__
#define NDS32_SR_ITYPE                 __NDS32_REG_ITYPE__
#define NDS32_SR_P_ITYPE               __NDS32_REG_P_ITYPE__
#define NDS32_SR_MERR                  __NDS32_REG_MERR__
#define NDS32_SR_IPC                   __NDS32_REG_IPC__
#define NDS32_SR_P_IPC                 __NDS32_REG_P_IPC__
#define NDS32_SR_OIPC                  __NDS32_REG_OIPC__
#define NDS32_SR_P_P0                  __NDS32_REG_P_P0__
#define NDS32_SR_P_P1                  __NDS32_REG_P_P1__
#define NDS32_SR_INT_MASK              __NDS32_REG_INT_MASK__
#define NDS32_SR_INT_MASK2             __NDS32_REG_INT_MASK2__
#define NDS32_SR_INT_MASK3             __NDS32_REG_INT_MASK3__
#define NDS32_SR_INT_PEND              __NDS32_REG_INT_PEND__
#define NDS32_SR_INT_PEND2             __NDS32_REG_INT_PEND2__
#define NDS32_SR_INT_PEND3             __NDS32_REG_INT_PEND3__
#define NDS32_SR_SP_USR                __NDS32_REG_SP_USR__
#define NDS32_SR_SP_PRIV               __NDS32_REG_SP_PRIV__
#define NDS32_SR_INT_PRI               __NDS32_REG_INT_PRI__
#define NDS32_SR_INT_PRI2              __NDS32_REG_INT_PRI2__
#define NDS32_SR_INT_PRI3              __NDS32_REG_INT_PRI3__
#define NDS32_SR_INT_PRI4              __NDS32_REG_INT_PRI4__
#define NDS32_SR_INT_CTRL              __NDS32_REG_INT_CTRL__
#define NDS32_SR_INT_TRIGGER           __NDS32_REG_INT_TRIGGER__
#define NDS32_SR_INT_TRIGGER2          __NDS32_REG_INT_TRIGGER2__
#define NDS32_SR_INT_GPR_PUSH_DIS      __NDS32_REG_INT_GPR_PUSH_DIS__
#define NDS32_SR_MMU_CTL               __NDS32_REG_MMU_CTL__
#define NDS32_SR_L1_PPTB               __NDS32_REG_L1_PPTB__
#define NDS32_SR_TLB_VPN               __NDS32_REG_TLB_VPN__
#define NDS32_SR_TLB_DATA              __NDS32_REG_TLB_DATA__
#define NDS32_SR_TLB_MISC              __NDS32_REG_TLB_MISC__
#define NDS32_SR_VLPT_IDX              __NDS32_REG_VLPT_IDX__
#define NDS32_SR_ILMB                  __NDS32_REG_ILMB__
#define NDS32_SR_DLMB                  __NDS32_REG_DLMB__
#define NDS32_SR_CACHE_CTL             __NDS32_REG_CACHE_CTL__
#define NDS32_SR_HSMP_SADDR            __NDS32_REG_HSMP_SADDR__
#define NDS32_SR_HSMP_EADDR            __NDS32_REG_HSMP_EADDR__
#define NDS32_SR_SDZ_CTL               __NDS32_REG_SDZ_CTL__
#define NDS32_SR_N12MISC_CTL           __NDS32_REG_N12MISC_CTL__
#define NDS32_SR_MISC_CTL              __NDS32_REG_MISC_CTL__
#define NDS32_SR_ECC_MISC              __NDS32_REG_ECC_MISC__
#define NDS32_SR_BPC0                  __NDS32_REG_BPC0__
#define NDS32_SR_BPC1                  __NDS32_REG_BPC1__
#define NDS32_SR_BPC2                  __NDS32_REG_BPC2__
#define NDS32_SR_BPC3                  __NDS32_REG_BPC3__
#define NDS32_SR_BPC4                  __NDS32_REG_BPC4__
#define NDS32_SR_BPC5                  __NDS32_REG_BPC5__
#define NDS32_SR_BPC6                  __NDS32_REG_BPC6__
#define NDS32_SR_BPC7                  __NDS32_REG_BPC7__
#define NDS32_SR_BPA0                  __NDS32_REG_BPA0__
#define NDS32_SR_BPA1                  __NDS32_REG_BPA1__
#define NDS32_SR_BPA2                  __NDS32_REG_BPA2__
#define NDS32_SR_BPA3                  __NDS32_REG_BPA3__
#define NDS32_SR_BPA4                  __NDS32_REG_BPA4__
#define NDS32_SR_BPA5                  __NDS32_REG_BPA5__
#define NDS32_SR_BPA6                  __NDS32_REG_BPA6__
#define NDS32_SR_BPA7                  __NDS32_REG_BPA7__
#define NDS32_SR_BPAM0                 __NDS32_REG_BPAM0__
#define NDS32_SR_BPAM1                 __NDS32_REG_BPAM1__
#define NDS32_SR_BPAM2                 __NDS32_REG_BPAM2__
#define NDS32_SR_BPAM3                 __NDS32_REG_BPAM3__
#define NDS32_SR_BPAM4                 __NDS32_REG_BPAM4__
#define NDS32_SR_BPAM5                 __NDS32_REG_BPAM5__
#define NDS32_SR_BPAM6                 __NDS32_REG_BPAM6__
#define NDS32_SR_BPAM7                 __NDS32_REG_BPAM7__
#define NDS32_SR_BPV0                  __NDS32_REG_BPV0__
#define NDS32_SR_BPV1                  __NDS32_REG_BPV1__
#define NDS32_SR_BPV2                  __NDS32_REG_BPV2__
#define NDS32_SR_BPV3                  __NDS32_REG_BPV3__
#define NDS32_SR_BPV4                  __NDS32_REG_BPV4__
#define NDS32_SR_BPV5                  __NDS32_REG_BPV5__
#define NDS32_SR_BPV6                  __NDS32_REG_BPV6__
#define NDS32_SR_BPV7                  __NDS32_REG_BPV7__
#define NDS32_SR_BPCID0                __NDS32_REG_BPCID0__
#define NDS32_SR_BPCID1                __NDS32_REG_BPCID1__
#define NDS32_SR_BPCID2                __NDS32_REG_BPCID2__
#define NDS32_SR_BPCID3                __NDS32_REG_BPCID3__
#define NDS32_SR_BPCID4                __NDS32_REG_BPCID4__
#define NDS32_SR_BPCID5                __NDS32_REG_BPCID5__
#define NDS32_SR_BPCID6                __NDS32_REG_BPCID6__
#define NDS32_SR_BPCID7                __NDS32_REG_BPCID7__
#define NDS32_SR_EDM_CFG               __NDS32_REG_EDM_CFG__
#define NDS32_SR_EDMSW                 __NDS32_REG_EDMSW__
#define NDS32_SR_EDM_CTL               __NDS32_REG_EDM_CTL__
#define NDS32_SR_EDM_DTR               __NDS32_REG_EDM_DTR__
#define NDS32_SR_BPMTC                 __NDS32_REG_BPMTC__
#define NDS32_SR_DIMBR                 __NDS32_REG_DIMBR__
#define NDS32_SR_TECR0                 __NDS32_REG_TECR0__
#define NDS32_SR_TECR1                 __NDS32_REG_TECR1__
#define NDS32_SR_PFMC0                 __NDS32_REG_PFMC0__
#define NDS32_SR_PFMC1                 __NDS32_REG_PFMC1__
#define NDS32_SR_PFMC2                 __NDS32_REG_PFMC2__
#define NDS32_SR_PFM_CTL               __NDS32_REG_PFM_CTL__
#define NDS32_SR_HSP_CTL               __NDS32_REG_HSP_CTL__
#define NDS32_SR_SP_BOUND              __NDS32_REG_SP_BOUND__
#define NDS32_SR_SP_BOUND_PRIV         __NDS32_REG_SP_BOUND_PRIV__
#define NDS32_SR_SP_BASE               __NDS32_REG_SP_BASE__
#define NDS32_SR_SP_BASE_PRIV          __NDS32_REG_SP_BASE_PRIV__
#define NDS32_SR_FUCOP_CTL             __NDS32_REG_FUCOP_CTL__
#define NDS32_SR_PRUSR_ACC_CTL         __NDS32_REG_PRUSR_ACC_CTL__
#define NDS32_SR_DMA_CFG               __NDS32_REG_DMA_CFG__
#define NDS32_SR_DMA_GCSW              __NDS32_REG_DMA_GCSW__
#define NDS32_SR_DMA_CHNSEL            __NDS32_REG_DMA_CHNSEL__
#define NDS32_SR_DMA_ACT               __NDS32_REG_DMA_ACT__
#define NDS32_SR_DMA_SETUP             __NDS32_REG_DMA_SETUP__
#define NDS32_SR_DMA_ISADDR            __NDS32_REG_DMA_ISADDR__
#define NDS32_SR_DMA_ESADDR            __NDS32_REG_DMA_ESADDR__
#define NDS32_SR_DMA_TCNT              __NDS32_REG_DMA_TCNT__
#define NDS32_SR_DMA_STATUS            __NDS32_REG_DMA_STATUS__
#define NDS32_SR_DMA_2DSET             __NDS32_REG_DMA_2DSET__
#define NDS32_SR_DMA_2DSCTL            __NDS32_REG_DMA_2DSCTL__
#define NDS32_SR_DMA_RCNT              __NDS32_REG_DMA_RCNT__
#define NDS32_SR_DMA_HSTATUS           __NDS32_REG_DMA_HSTATUS__
#define NDS32_SR_SP_USR1               __NDS32_REG_SP_USR1__
#define NDS32_SR_SP_USR2               __NDS32_REG_SP_USR2__
#define NDS32_SR_SP_USR3               __NDS32_REG_SP_USR3__
#define NDS32_SR_SP_PRIV1              __NDS32_REG_SP_PRIV1__
#define NDS32_SR_SP_PRIV2              __NDS32_REG_SP_PRIV2__
#define NDS32_SR_SP_PRIV3              __NDS32_REG_SP_PRIV3__
#define NDS32_SR_BG_REGION             __NDS32_REG_BG_REGION__
#define NDS32_SR_SFCR                  __NDS32_REG_SFCR__
#define NDS32_SR_SIGN                  __NDS32_REG_SIGN__
#define NDS32_SR_ISIGN                 __NDS32_REG_ISIGN__
#define NDS32_SR_P_ISIGN               __NDS32_REG_P_ISIGN__

#define NDS32_USR_PC                    __NDS32_REG_PC__
#define NDS32_USR_DMA_CFG               __NDS32_REG_DMA_CFG__
#define NDS32_USR_DMA_GCSW              __NDS32_REG_DMA_GCSW__
#define NDS32_USR_DMA_CHNSEL            __NDS32_REG_DMA_CHNSEL__
#define NDS32_USR_DMA_ACT               __NDS32_REG_DMA_ACT__
#define NDS32_USR_DMA_SETUP             __NDS32_REG_DMA_SETUP__
#define NDS32_USR_DMA_ISADDR            __NDS32_REG_DMA_ISADDR__
#define NDS32_USR_DMA_ESADDR            __NDS32_REG_DMA_ESADDR__
#define NDS32_USR_DMA_TCNT              __NDS32_REG_DMA_TCNT__
#define NDS32_USR_DMA_STATUS            __NDS32_REG_DMA_STATUS__
#define NDS32_USR_DMA_2DSET             __NDS32_REG_DMA_2DSET__
#define NDS32_USR_DMA_2DSCTL            __NDS32_REG_DMA_2DSCTL__
#define NDS32_USR_PFMC0                 __NDS32_REG_PFMC0__
#define NDS32_USR_PFMC1                 __NDS32_REG_PFMC1__
#define NDS32_USR_PFMC2                 __NDS32_REG_PFMC2__
#define NDS32_USR_PFM_CTL               __NDS32_REG_PFM_CTL__
#define NDS32_USR_IFC_LP                __NDS32_REG_IFC_LP__
#define NDS32_USR_ITB                   __NDS32_REG_ITB__

#define NDS32_CCTL_L1D_VA_FILLCK        __NDS32_CCTL_L1D_VA_FILLCK__
#define NDS32_CCTL_L1D_VA_ULCK          __NDS32_CCTL_L1D_VA_ULCK__
#define NDS32_CCTL_L1I_VA_FILLCK        __NDS32_CCTL_L1I_VA_FILLCK__
#define NDS32_CCTL_L1I_VA_ULCK          __NDS32_CCTL_L1I_VA_ULCK__

#define NDS32_CCTL_L1D_IX_WBINVAL       __NDS32_CCTL_L1D_IX_WBINVAL__
#define NDS32_CCTL_L1D_IX_INVAL         __NDS32_CCTL_L1D_IX_INVAL__
#define NDS32_CCTL_L1D_IX_WB            __NDS32_CCTL_L1D_IX_WB__
#define NDS32_CCTL_L1I_IX_INVAL         __NDS32_CCTL_L1I_IX_INVAL__

#define NDS32_CCTL_L1D_VA_INVAL         __NDS32_CCTL_L1D_VA_INVAL__
#define NDS32_CCTL_L1D_VA_WB            __NDS32_CCTL_L1D_VA_WB__
#define NDS32_CCTL_L1D_VA_WBINVAL       __NDS32_CCTL_L1D_VA_WBINVAL__
#define NDS32_CCTL_L1I_VA_INVAL         __NDS32_CCTL_L1I_VA_INVAL__

#define NDS32_CCTL_L1D_IX_RTAG          __NDS32_CCTL_L1D_IX_RTAG__
#define NDS32_CCTL_L1D_IX_RWD           __NDS32_CCTL_L1D_IX_RWD__
#define NDS32_CCTL_L1I_IX_RTAG          __NDS32_CCTL_L1I_IX_RTAG__
#define NDS32_CCTL_L1I_IX_RWD           __NDS32_CCTL_L1I_IX_RWD__

#define NDS32_CCTL_L1D_IX_WTAG          __NDS32_CCTL_L1D_IX_WTAG__
#define NDS32_CCTL_L1D_IX_WWD           __NDS32_CCTL_L1D_IX_WWD__
#define NDS32_CCTL_L1I_IX_WTAG          __NDS32_CCTL_L1I_IX_WTAG__
#define NDS32_CCTL_L1I_IX_WWD           __NDS32_CCTL_L1I_IX_WWD__

#define NDS32_DPREF_SRD                 __NDS32_DPREF_SRD__
#define NDS32_DPREF_MRD                 __NDS32_DPREF_MRD__
#define NDS32_DPREF_SWR                 __NDS32_DPREF_SWR__
#define NDS32_DPREF_MWR                 __NDS32_DPREF_MWR__
#define NDS32_DPREF_PTE                 __NDS32_DPREF_PTE__
#define NDS32_DPREF_CLWR                __NDS32_DPREF_CLWR__

/* ------------------------------------------------------------------------ */


/* Map __nds32__xxx() to __builtin_xxx() functions for compatibility.  */
#define __nds32__llw(a) \
  (__builtin_nds32_llw ((a)))
#define __nds32__lwup(a) \
  (__builtin_nds32_lwup ((a)))
#define __nds32__lbup(a) \
  (__builtin_nds32_lbup ((a)))
#define __nds32__scw(a, b) \
  (__builtin_nds32_scw ((a), (b)))
#define __nds32__swup(a, b) \
  (__builtin_nds32_swup ((a), (b)))
#define __nds32__sbup(a, b) \
  (__builtin_nds32_sbup ((a), (b)))

#define __nds32__mfsr(srname) \
  (__builtin_nds32_mfsr ((srname)))
#define __nds32__mfusr(usrname) \
  (__builtin_nds32_mfusr ((usrname)))
#define __nds32__mtsr(val, srname) \
  (__builtin_nds32_mtsr ((val), (srname)))
#define __nds32__mtsr_isb(val, srname) \
  (__builtin_nds32_mtsr_isb ((val), (srname)))
#define __nds32__mtsr_dsb(val, srname) \
  (__builtin_nds32_mtsr_dsb ((val), (srname)))
#define __nds32__mtusr(val, usrname) \
  (__builtin_nds32_mtusr ((val), (usrname)))

#define __nds32__break(swid) \
  (__builtin_nds32_break(swid))
#define __nds32__cctlva_lck(subtype, va) \
  (__builtin_nds32_cctl_va_lck ((subtype), (va)))
#define __nds32__cctlidx_wbinval(subtype, idx) \
  (__builtin_nds32_cctl_idx_wbinval ((subtype), (idx)))
#define __nds32__cctlva_wbinval_alvl(subtype, va) \
  (__builtin_nds32_cctl_va_wbinval_la ((subtype), (va)))
#define __nds32__cctlva_wbinval_one_lvl(subtype, va) \
  (__builtin_nds32_cctl_va_wbinval_l1 ((subtype), (va)))
#define __nds32__cctlidx_read(subtype, idx) \
  (__builtin_nds32_cctl_idx_read ((subtype), (idx)))
#define __nds32__cctlidx_write(subtype, b, idxw) \
  (__builtin_nds32_cctl_idx_write ((subtype), (b), (idxw)))
#define __nds32__cctl_l1d_invalall()  \
  (__builtin_nds32_cctl_l1d_invalall())
#define __nds32__cctl_l1d_wball_alvl() \
  (__builtin_nds32_cctl_l1d_wball_alvl())
#define __nds32__cctl_l1d_wball_one_lvl() \
  (__builtin_nds32_cctl_l1d_wball_one_lvl())

#define __nds32__dsb() \
  (__builtin_nds32_dsb())
#define __nds32__isb() \
  (__builtin_nds32_isb())
#define __nds32__msync_store() \
  (__builtin_nds32_msync_store())
#define __nds32__msync_all() \
  (__builtin_nds32_msync_all())
#define __nds32__nop() \
  (__builtin_nds32_nop())

#define __nds32__standby_wait_done() \
  (__builtin_nds32_standby_wait_done())
#define __nds32__standby_no_wake_grant() \
  (__builtin_nds32_standby_no_wake_grant())
#define __nds32__standby_wake_grant() \
  (__builtin_nds32_standby_wake_grant())
#define __nds32__schedule_barrier() \
  (__builtin_nds32_schedule_barrier())
#define __nds32__setend_big() \
  (__builtin_nds32_setend_big())
#define __nds32__setend_little() \
  (__builtin_nds32_setend_little())
#define __nds32__setgie_en() \
  (__builtin_nds32_setgie_en())
#define __nds32__setgie_dis() \
  (__builtin_nds32_setgie_dis())

#define __nds32__jr_itoff(a) \
  (__builtin_nds32_jr_itoff ((a)))
#define __nds32__jr_toff(a) \
  (__builtin_nds32_jr_toff ((a)))
#define __nds32__jral_iton(a) \
  (__builtin_nds32_jral_iton ((a)))
#define __nds32__jral_ton(a) \
  (__builtin_nds32_jral_ton ((a)))
#define __nds32__ret_itoff(a) \
  (__builtin_nds32_ret_itoff ((a)))
#define __nds32__ret_toff(a) \
  (__builtin_nds32_ret_toff ((a)))
#define __nds32__svs(a, b) \
  (__builtin_nds32_svs ((a), (b)))
#define __nds32__sva(a, b) \
  (__builtin_nds32_sva ((a), (b)))
#define __nds32__dpref_qw(a, b, subtype) \
  (__builtin_nds32_dpref_qw ((a), (b), (subtype)))
#define __nds32__dpref_hw(a, b, subtype) \
  (__builtin_nds32_dpref_hw ((a), (b), (subtype)))
#define __nds32__dpref_w(a, b, subtype) \
  (__builtin_nds32_dpref_w ((a), (b), (subtype)))
#define __nds32__dpref_dw(a, b, subtype) \
  (__builtin_nds32_dpref_dw ((a), (b), (subtype)))

#define __nds32__teqz(a, swid) \
  (__builtin_nds32_teqz ((a), (swid)))
#define __nds32__tnez(a, swid) \
  ( __builtin_nds32_tnez ((a), (swid)))
#define __nds32__trap(swid) \
  (__builtin_nds32_trap ((swid)))
#define __nds32__isync(a) \
  (__builtin_nds32_isync ((a)))
#define __nds32__rotr(val, ror) \
  (__builtin_nds32_rotr ((val), (ror)))
#define __nds32__wsbh(a) \
  (__builtin_nds32_wsbh ((a)))
#define __nds32__syscall(a) \
  (__builtin_nds32_syscall ((a)))
#define __nds32__return_address() \
  (__builtin_nds32_return_address())
#define __nds32__get_current_sp() \
  (__builtin_nds32_get_current_sp())
#define __nds32__set_current_sp(a) \
  (__builtin_nds32_set_current_sp ((a)))
#define __nds32__abs(a) \
  (__builtin_nds32_pe_abs ((a)))
#define __nds32__ave(a, b) \
  (__builtin_nds32_pe_ave ((a), (b)))
#define __nds32__bclr(a, pos) \
  (__builtin_nds32_pe_bclr ((a), (pos)))
#define __nds32__bset(a, pos) \
  (__builtin_nds32_pe_bset ((a), (pos)))
#define __nds32__btgl(a, pos) \
  (__builtin_nds32_pe_btgl ((a), (pos)))
#define __nds32__btst(a, pos) \
  (__builtin_nds32_pe_btst ((a), (pos)))

#define __nds32__clip(a, imm) \
  (__builtin_nds32_pe_clip ((a), (imm)))
#define __nds32__clips(a, imm) \
  (__builtin_nds32_pe_clips ((a), (imm)))
#define __nds32__clz(a) \
  (__builtin_nds32_pe_clz ((a)))
#define __nds32__clo(a) \
  (__builtin_nds32_pe_clo ((a)))
#define __nds32__bse(r, a, b) \
  (__builtin_nds32_pe2_bse ((r), (a), (b)))
#define __nds32__bsp(r, a, b) \
  (__builtin_nds32_pe2_bsp ((r), (a), (b)))
#define __nds32__pbsad(a, b) \
  (__builtin_nds32_pe2_pbsad ((a), (b)))
#define __nds32__pbsada(acc, a, b) \
  (__builtin_nds32_pe2_pbsada ((acc), (a), (b)))

#define __nds32__ffb(a, b) \
  (__builtin_nds32_se_ffb ((a), (b)))
#define __nds32__ffmism(a, b) \
  (__builtin_nds32_se_ffmism ((a), (b)))
#define __nds32__flmism(a, b) \
  (__builtin_nds32_se_flmism ((a), (b)))
#define __nds32__fcpynsd(a, b) \
  (__builtin_nds32_fcpynsd ((a), (b)))
#define __nds32__fcpynss(a, b) \
  (__builtin_nds32_fcpynss ((a), (b)))
#define __nds32__fcpysd(a, b) \
  (__builtin_nds32_fcpysd ((a), (b)))
#define __nds32__fcpyss(a, b) \
  (__builtin_nds32_fcpyss ((a), (b)))
#define __nds32__fmfcsr() \
  (__builtin_nds32_fmfcsr())
#define __nds32__fmtcsr(fpcsr) \
  (__builtin_nds32_fmtcsr ((fpcsr)))
#define __nds32__fmfcfg() \
  (__builtin_nds32_fmfcfg())

#define __nds32__tlbop_trd(a) \
  (__builtin_nds32_tlbop_trd ((a)))
#define __nds32__tlbop_twr(a) \
  (__builtin_nds32_tlbop_twr ((a)))
#define __nds32__tlbop_rwr(a) \
  (__builtin_nds32_tlbop_rwr ((a)))
#define __nds32__tlbop_rwlk(a) \
  (__builtin_nds32_tlbop_rwlk ((a)))
#define __nds32__tlbop_unlk(a) \
  (__builtin_nds32_tlbop_unlk ((a)))
#define __nds32__tlbop_pb(a) \
  (__builtin_nds32_tlbop_pb ((a)))
#define __nds32__tlbop_inv(a) \
  (__builtin_nds32_tlbop_inv ((a)))
#define __nds32__tlbop_flua() \
(__builtin_nds32_tlbop_flua())

#define __nds32__kaddw(a, b) \
  (__builtin_nds32_kaddw ((a), (b)))
#define __nds32__kaddh(a, b) \
  (__builtin_nds32_kaddh ((a), (b)))
#define __nds32__ksubw(a, b) \
  (__builtin_nds32_ksubw ((a), (b)))
#define __nds32__ksubh(a, b) \
  (__builtin_nds32_ksubh ((a), (b)))
#define __nds32__kdmbb(a, b) \
  (__builtin_nds32_kdmbb ((a), (b)))
#define __nds32__v_kdmbb(a, b) \
  (__builtin_nds32_v_kdmbb ((a), (b)))
#define __nds32__kdmbt(a, b) \
  (__builtin_nds32_kdmbt ((a), (b)))
#define __nds32__v_kdmbt(a, b) \
  (__builtin_nds32_v_kdmbt ((a), (b)))
#define __nds32__kdmtb(a, b) \
  (__builtin_nds32_kdmtb ((a), (b)))
#define __nds32__v_kdmtb(a, b) \
  (__builtin_nds32_v_kdmtb ((a), (b)))
#define __nds32__kdmtt(a, b) \
  (__builtin_nds32_kdmtt ((a), (b)))
#define __nds32__v_kdmtt(a, b) \
  (__builtin_nds32_v_kdmtt ((a), (b)))
#define __nds32__khmbb(a, b) \
  (__builtin_nds32_khmbb ((a), (b)))
#define __nds32__v_khmbb(a, b) \
  (__builtin_nds32_v_khmbb ((a), (b)))
#define __nds32__khmbt(a, b) \
  (__builtin_nds32_khmbt ((a), (b)))
#define __nds32__v_khmbt(a, b) \
  (__builtin_nds32_v_khmbt ((a), (b)))
#define __nds32__khmtb(a, b) \
  (__builtin_nds32_khmtb ((a), (b)))
#define __nds32__v_khmtb(a, b) \
  (__builtin_nds32_v_khmtb ((a), (b)))
#define __nds32__khmtt(a, b) \
  (__builtin_nds32_khmtt ((a), (b)))
#define __nds32__v_khmtt(a, b) \
  (__builtin_nds32_v_khmtt ((a), (b)))
#define __nds32__kslraw(a, b) \
  (__builtin_nds32_kslraw ((a), (b)))
#define __nds32__kslraw_u(a, b) \
  (__builtin_nds32_kslraw_u ((a), (b)))

#define __nds32__rdov() \
  (__builtin_nds32_rdov())
#define __nds32__clrov() \
  (__builtin_nds32_clrov())
#define __nds32__gie_dis() \
  (__builtin_nds32_gie_dis())
#define __nds32__gie_en() \
  (__builtin_nds32_gie_en())
#define __nds32__enable_int(a) \
  (__builtin_nds32_enable_int ((a)))
#define __nds32__disable_int(a) \
  (__builtin_nds32_disable_int ((a)))
#define __nds32__set_pending_swint() \
  (__builtin_nds32_set_pending_swint())
#define __nds32__clr_pending_swint() \
  (__builtin_nds32_clr_pending_swint())
#define __nds32__clr_pending_hwint(a) \
  (__builtin_nds32_clr_pending_hwint(a))
#define __nds32__get_all_pending_int() \
  (__builtin_nds32_get_all_pending_int())
#define __nds32__get_pending_int(a) \
  (__builtin_nds32_get_pending_int ((a)))
#define __nds32__set_int_priority(a, b) \
  (__builtin_nds32_set_int_priority ((a), (b)))
#define __nds32__get_int_priority(a) \
  (__builtin_nds32_get_int_priority ((a)))
#define __nds32__set_trig_type_level(a) \
  (__builtin_nds32_set_trig_level(a))
#define __nds32__set_trig_type_edge(a) \
  (__builtin_nds32_set_trig_edge(a))
#define __nds32__get_trig_type(a) \
  (__builtin_nds32_get_trig_type ((a)))

#define __nds32__get_unaligned_hw(a) \
  (__builtin_nds32_unaligned_load_hw ((a)))
#define __nds32__get_unaligned_w(a) \
  (__builtin_nds32_unaligned_load_w ((a)))
#define __nds32__get_unaligned_dw(a) \
  (__builtin_nds32_unaligned_load_dw ((a)))
#define __nds32__put_unaligned_hw(a, data) \
  (__builtin_nds32_unaligned_store_hw ((a), (data)))
#define __nds32__put_unaligned_w(a, data) \
  (__builtin_nds32_unaligned_store_w ((a), (data)))
#define __nds32__put_unaligned_dw(a, data) \
  (__builtin_nds32_unaligned_store_dw ((a), (data)))

#define __nds32__add16(a, b) \
  (__builtin_nds32_add16 ((a), (b)))
#define __nds32__v_uadd16(a, b) \
  (__builtin_nds32_v_uadd16 ((a), (b)))
#define __nds32__v_sadd16(a, b) \
  (__builtin_nds32_v_sadd16 ((a), (b)))
#define __nds32__radd16(a, b) \
  (__builtin_nds32_radd16 ((a), (b)))
#define __nds32__v_radd16(a, b) \
  (__builtin_nds32_v_radd16 ((a), (b)))
#define __nds32__uradd16(a, b) \
  (__builtin_nds32_uradd16 ((a), (b)))
#define __nds32__v_uradd16(a, b) \
  (__builtin_nds32_v_uradd16 ((a), (b)))
#define __nds32__kadd16(a, b) \
  (__builtin_nds32_kadd16 ((a), (b)))
#define __nds32__v_kadd16(a, b) \
  (__builtin_nds32_v_kadd16 ((a), (b)))
#define __nds32__ukadd16(a, b) \
  (__builtin_nds32_ukadd16 ((a), (b)))
#define __nds32__v_ukadd16(a, b) \
  (__builtin_nds32_v_ukadd16 ((a), (b)))
#define __nds32__sub16(a, b) \
  (__builtin_nds32_sub16 ((a), (b)))
#define __nds32__v_usub16(a, b) \
  (__builtin_nds32_v_usub16 ((a), (b)))
#define __nds32__v_ssub16(a, b) \
  (__builtin_nds32_v_ssub16 ((a), (b)))
#define __nds32__rsub16(a, b) \
  (__builtin_nds32_rsub16 ((a), (b)))
#define __nds32__v_rsub16(a, b) \
  (__builtin_nds32_v_rsub16 ((a), (b)))
#define __nds32__ursub16(a, b) \
  (__builtin_nds32_ursub16 ((a), (b)))
#define __nds32__v_ursub16(a, b) \
  (__builtin_nds32_v_ursub16 ((a), (b)))
#define __nds32__ksub16(a, b) \
  (__builtin_nds32_ksub16 ((a), (b)))
#define __nds32__v_ksub16(a, b) \
  (__builtin_nds32_v_ksub16 ((a), (b)))
#define __nds32__uksub16(a, b) \
  (__builtin_nds32_uksub16 ((a), (b)))
#define __nds32__v_uksub16(a, b) \
  (__builtin_nds32_v_uksub16 ((a), (b)))
#define __nds32__cras16(a, b) \
  (__builtin_nds32_cras16 ((a), (b)))
#define __nds32__v_ucras16(a, b) \
  (__builtin_nds32_v_ucras16 ((a), (b)))
#define __nds32__v_scras16(a, b) \
  (__builtin_nds32_v_scras16 ((a), (b)))
#define __nds32__rcras16(a, b) \
  (__builtin_nds32_rcras16 ((a), (b)))
#define __nds32__v_rcras16(a, b) \
  (__builtin_nds32_v_rcras16 ((a), (b)))
#define __nds32__urcras16(a, b) \
  (__builtin_nds32_urcras16 ((a), (b)))
#define __nds32__v_urcras16(a, b) \
  (__builtin_nds32_v_urcras16 ((a), (b)))
#define __nds32__kcras16(a, b) \
  (__builtin_nds32_kcras16 ((a), (b)))
#define __nds32__v_kcras16(a, b) \
  (__builtin_nds32_v_kcras16 ((a), (b)))
#define __nds32__ukcras16(a, b) \
  (__builtin_nds32_ukcras16 ((a), (b)))
#define __nds32__v_ukcras16(a, b) \
  (__builtin_nds32_v_ukcras16 ((a), (b)))
#define __nds32__crsa16(a, b) \
  (__builtin_nds32_crsa16 ((a), (b)))
#define __nds32__v_ucrsa16(a, b) \
  (__builtin_nds32_v_ucrsa16 ((a), (b)))
#define __nds32__v_scrsa16(a, b) \
  (__builtin_nds32_v_scrsa16 ((a), (b)))
#define __nds32__rcrsa16(a, b) \
  (__builtin_nds32_rcrsa16 ((a), (b)))
#define __nds32__v_rcrsa16(a, b) \
  (__builtin_nds32_v_rcrsa16 ((a), (b)))
#define __nds32__urcrsa16(a, b) \
  (__builtin_nds32_urcrsa16 ((a), (b)))
#define __nds32__v_urcrsa16(a, b) \
  (__builtin_nds32_v_urcrsa16 ((a), (b)))
#define __nds32__kcrsa16(a, b) \
  (__builtin_nds32_kcrsa16 ((a), (b)))
#define __nds32__v_kcrsa16(a, b) \
  (__builtin_nds32_v_kcrsa16 ((a), (b)))
#define __nds32__ukcrsa16(a, b) \
  (__builtin_nds32_ukcrsa16 ((a), (b)))
#define __nds32__v_ukcrsa16(a, b) \
  (__builtin_nds32_v_ukcrsa16 ((a), (b)))

#define __nds32__add8(a, b) \
  (__builtin_nds32_add8 ((a), (b)))
#define __nds32__v_uadd8(a, b) \
  (__builtin_nds32_v_uadd8 ((a), (b)))
#define __nds32__v_sadd8(a, b) \
  (__builtin_nds32_v_sadd8 ((a), (b)))
#define __nds32__radd8(a, b) \
  (__builtin_nds32_radd8 ((a), (b)))
#define __nds32__v_radd8(a, b) \
  (__builtin_nds32_v_radd8 ((a), (b)))
#define __nds32__uradd8(a, b) \
  (__builtin_nds32_uradd8 ((a), (b)))
#define __nds32__v_uradd8(a, b) \
  (__builtin_nds32_v_uradd8 ((a), (b)))
#define __nds32__kadd8(a, b) \
  (__builtin_nds32_kadd8 ((a), (b)))
#define __nds32__v_kadd8(a, b) \
  (__builtin_nds32_v_kadd8 ((a), (b)))
#define __nds32__ukadd8(a, b) \
  (__builtin_nds32_ukadd8 ((a), (b)))
#define __nds32__v_ukadd8(a, b) \
  (__builtin_nds32_v_ukadd8 ((a), (b)))
#define __nds32__sub8(a, b) \
  (__builtin_nds32_sub8 ((a), (b)))
#define __nds32__v_usub8(a, b) \
  (__builtin_nds32_v_usub8 ((a), (b)))
#define __nds32__v_ssub8(a, b) \
  (__builtin_nds32_v_ssub8 ((a), (b)))
#define __nds32__rsub8(a, b) \
  (__builtin_nds32_rsub8 ((a), (b)))
#define __nds32__v_rsub8(a, b) \
  (__builtin_nds32_v_rsub8 ((a), (b)))
#define __nds32__ursub8(a, b) \
  (__builtin_nds32_ursub8 ((a), (b)))
#define __nds32__v_ursub8(a, b) \
  (__builtin_nds32_v_ursub8 ((a), (b)))
#define __nds32__ksub8(a, b) \
  (__builtin_nds32_ksub8 ((a), (b)))
#define __nds32__v_ksub8(a, b) \
  (__builtin_nds32_v_ksub8 ((a), (b)))
#define __nds32__uksub8(a, b) \
  (__builtin_nds32_uksub8 ((a), (b)))
#define __nds32__v_uksub8(a, b) \
  (__builtin_nds32_v_uksub8 ((a), (b)))

#define __nds32__sra16(a, b) \
  (__builtin_nds32_sra16 ((a), (b)))
#define __nds32__v_sra16(a, b) \
  (__builtin_nds32_v_sra16 ((a), (b)))
#define __nds32__sra16_u(a, b) \
  (__builtin_nds32_sra16_u ((a), (b)))
#define __nds32__v_sra16_u(a, b) \
  (__builtin_nds32_v_sra16_u ((a), (b)))
#define __nds32__srl16(a, b) \
  (__builtin_nds32_srl16 ((a), (b)))
#define __nds32__v_srl16(a, b) \
  (__builtin_nds32_v_srl16 ((a), (b)))
#define __nds32__srl16_u(a, b) \
  (__builtin_nds32_srl16_u ((a), (b)))
#define __nds32__v_srl16_u(a, b) \
  (__builtin_nds32_v_srl16_u ((a), (b)))
#define __nds32__sll16(a, b) \
  (__builtin_nds32_sll16 ((a), (b)))
#define __nds32__v_sll16(a, b) \
  (__builtin_nds32_v_sll16 ((a), (b)))
#define __nds32__ksll16(a, b) \
  (__builtin_nds32_ksll16 ((a), (b)))
#define __nds32__v_ksll16(a, b) \
  (__builtin_nds32_v_ksll16 ((a), (b)))
#define __nds32__kslra16(a, b) \
  (__builtin_nds32_kslra16 ((a), (b)))
#define __nds32__v_kslra16(a, b) \
  (__builtin_nds32_v_kslra16 ((a), (b)))
#define __nds32__kslra16_u(a, b) \
  (__builtin_nds32_kslra16_u ((a), (b)))
#define __nds32__v_kslra16_u(a, b) \
  (__builtin_nds32_v_kslra16_u ((a), (b)))

#define __nds32__cmpeq16(a, b) \
  (__builtin_nds32_cmpeq16 ((a), (b)))
#define __nds32__v_scmpeq16(a, b) \
  (__builtin_nds32_v_scmpeq16 ((a), (b)))
#define __nds32__v_ucmpeq16(a, b) \
  (__builtin_nds32_v_ucmpeq16 ((a), (b)))
#define __nds32__scmplt16(a, b) \
  (__builtin_nds32_scmplt16 ((a), (b)))
#define __nds32__v_scmplt16(a, b) \
  (__builtin_nds32_v_scmplt16 ((a), (b)))
#define __nds32__scmple16(a, b) \
  (__builtin_nds32_scmple16 ((a), (b)))
#define __nds32__v_scmple16(a, b) \
  (__builtin_nds32_v_scmple16 ((a), (b)))
#define __nds32__ucmplt16(a, b) \
  (__builtin_nds32_ucmplt16 ((a), (b)))
#define __nds32__v_ucmplt16(a, b) \
  (__builtin_nds32_v_ucmplt16 ((a), (b)))
#define __nds32__ucmple16(a, b) \
  (__builtin_nds32_ucmple16 ((a), (b)))
#define __nds32__v_ucmple16(a, b) \
  (__builtin_nds32_v_ucmple16 ((a), (b)))

#define __nds32__cmpeq8(a, b) \
  (__builtin_nds32_cmpeq8 ((a), (b)))
#define __nds32__v_scmpeq8(a, b) \
  (__builtin_nds32_v_scmpeq8 ((a), (b)))
#define __nds32__v_ucmpeq8(a, b) \
  (__builtin_nds32_v_ucmpeq8 ((a), (b)))
#define __nds32__scmplt8(a, b) \
  (__builtin_nds32_scmplt8 ((a), (b)))
#define __nds32__v_scmplt8(a, b) \
  (__builtin_nds32_v_scmplt8 ((a), (b)))
#define __nds32__scmple8(a, b) \
  (__builtin_nds32_scmple8 ((a), (b)))
#define __nds32__v_scmple8(a, b) \
  (__builtin_nds32_v_scmple8 ((a), (b)))
#define __nds32__ucmplt8(a, b) \
  (__builtin_nds32_ucmplt8 ((a), (b)))
#define __nds32__v_ucmplt8(a, b) \
  (__builtin_nds32_v_ucmplt8 ((a), (b)))
#define __nds32__ucmple8(a, b) \
  (__builtin_nds32_ucmple8 ((a), (b)))
#define __nds32__v_ucmple8(a, b) \
  (__builtin_nds32_v_ucmple8 ((a), (b)))

#define __nds32__smin16(a, b) \
  (__builtin_nds32_smin16 ((a), (b)))
#define __nds32__v_smin16(a, b) \
  (__builtin_nds32_v_smin16 ((a), (b)))
#define __nds32__umin16(a, b) \
  (__builtin_nds32_umin16 ((a), (b)))
#define __nds32__v_umin16(a, b) \
  (__builtin_nds32_v_umin16 ((a), (b)))
#define __nds32__smax16(a, b) \
  (__builtin_nds32_smax16 ((a), (b)))
#define __nds32__v_smax16(a, b) \
  (__builtin_nds32_v_smax16 ((a), (b)))
#define __nds32__umax16(a, b) \
  (__builtin_nds32_umax16 ((a), (b)))
#define __nds32__v_umax16(a, b) \
  (__builtin_nds32_v_umax16 ((a), (b)))
#define __nds32__sclip16(a, b) \
  (__builtin_nds32_sclip16 ((a), (b)))
#define __nds32__v_sclip16(a, b) \
  (__builtin_nds32_v_sclip16 ((a), (b)))
#define __nds32__uclip16(a, b) \
  (__builtin_nds32_uclip16 ((a), (b)))
#define __nds32__v_uclip16(a, b) \
  (__builtin_nds32_v_uclip16 ((a), (b)))
#define __nds32__khm16(a, b) \
  (__builtin_nds32_khm16 ((a), (b)))
#define __nds32__v_khm16(a, b) \
  (__builtin_nds32_v_khm16 ((a), (b)))
#define __nds32__khmx16(a, b) \
  (__builtin_nds32_khmx16 ((a), (b)))
#define __nds32__v_khmx16(a, b) \
  (__builtin_nds32_v_khmx16 ((a), (b)))
#define __nds32__kabs16(a) \
  (__builtin_nds32_kabs16 ((a)))
#define __nds32__v_kabs16(a) \
  (__builtin_nds32_v_kabs16 ((a)))

#define __nds32__smin8(a, b) \
  (__builtin_nds32_smin8 ((a), (b)))
#define __nds32__v_smin8(a, b) \
  (__builtin_nds32_v_smin8 ((a), (b)))
#define __nds32__umin8(a, b) \
  (__builtin_nds32_umin8 ((a), (b)))
#define __nds32__v_umin8(a, b) \
  (__builtin_nds32_v_umin8 ((a), (b)))
#define __nds32__smax8(a, b) \
  (__builtin_nds32_smax8 ((a), (b)))
#define __nds32__v_smax8(a, b) \
  (__builtin_nds32_v_smax8 ((a), (b)))
#define __nds32__umax8(a, b) \
  (__builtin_nds32_umax8 ((a), (b)))
#define __nds32__v_umax8(a, b) \
  (__builtin_nds32_v_umax8 ((a), (b)))
#define __nds32__kabs8(a) \
  (__builtin_nds32_kabs8 ((a)))
#define __nds32__v_kabs8(a) \
  (__builtin_nds32_v_kabs8 ((a)))

#define __nds32__sunpkd810(a) \
  (__builtin_nds32_sunpkd810 ((a)))
#define __nds32__v_sunpkd810(a) \
  (__builtin_nds32_v_sunpkd810 ((a)))
#define __nds32__sunpkd820(a) \
  (__builtin_nds32_sunpkd820 ((a)))
#define __nds32__v_sunpkd820(a) \
  (__builtin_nds32_v_sunpkd820 ((a)))
#define __nds32__sunpkd830(a) \
  (__builtin_nds32_sunpkd830 ((a)))
#define __nds32__v_sunpkd830(a) \
  (__builtin_nds32_v_sunpkd830 ((a)))
#define __nds32__sunpkd831(a) \
  (__builtin_nds32_sunpkd831 ((a)))
#define __nds32__v_sunpkd831(a) \
  (__builtin_nds32_v_sunpkd831 ((a)))
#define __nds32__zunpkd810(a) \
  (__builtin_nds32_zunpkd810 ((a)))
#define __nds32__v_zunpkd810(a) \
  (__builtin_nds32_v_zunpkd810 ((a)))
#define __nds32__zunpkd820(a) \
  (__builtin_nds32_zunpkd820 ((a)))
#define __nds32__v_zunpkd820(a) \
  (__builtin_nds32_v_zunpkd820 ((a)))
#define __nds32__zunpkd830(a) \
  (__builtin_nds32_zunpkd830 ((a)))
#define __nds32__v_zunpkd830(a) \
  (__builtin_nds32_v_zunpkd830 ((a)))
#define __nds32__zunpkd831(a) \
  (__builtin_nds32_zunpkd831 ((a)))
#define __nds32__v_zunpkd831(a) \
  (__builtin_nds32_v_zunpkd831 ((a)))

#define __nds32__raddw(a, b) \
  (__builtin_nds32_raddw ((a), (b)))
#define __nds32__uraddw(a, b) \
  (__builtin_nds32_uraddw ((a), (b)))
#define __nds32__rsubw(a, b) \
  (__builtin_nds32_rsubw ((a), (b)))
#define __nds32__ursubw(a, b) \
  (__builtin_nds32_ursubw ((a), (b)))

#define __nds32__sra_u(a, b) \
  (__builtin_nds32_sra_u ((a), (b)))
#define __nds32__ksll(a, b) \
  (__builtin_nds32_ksll ((a), (b)))
#define __nds32__pkbb16(a, b) \
  (__builtin_nds32_pkbb16 ((a), (b)))
#define __nds32__v_pkbb16(a, b) \
  (__builtin_nds32_v_pkbb16 ((a), (b)))
#define __nds32__pkbt16(a, b) \
  (__builtin_nds32_pkbt16 ((a), (b)))
#define __nds32__v_pkbt16(a, b) \
  (__builtin_nds32_v_pkbt16 ((a), (b)))
#define __nds32__pktb16(a, b) \
  (__builtin_nds32_pktb16 ((a), (b)))
#define __nds32__v_pktb16(a, b) \
  (__builtin_nds32_v_pktb16 ((a), (b)))
#define __nds32__pktt16(a, b) \
  (__builtin_nds32_pktt16 ((a), (b)))
#define __nds32__v_pktt16(a, b) \
  (__builtin_nds32_v_pktt16 ((a), (b)))

#define __nds32__smmul(a, b) \
  (__builtin_nds32_smmul ((a), (b)))
#define __nds32__smmul_u(a, b) \
  (__builtin_nds32_smmul_u ((a), (b)))
#define __nds32__kmmac(r, a, b) \
  (__builtin_nds32_kmmac ((r), (a), (b)))
#define __nds32__kmmac_u(r, a, b) \
  (__builtin_nds32_kmmac_u ((r), (a), (b)))
#define __nds32__kmmsb(r, a, b) \
  (__builtin_nds32_kmmsb ((r), (a), (b)))
#define __nds32__kmmsb_u(r, a, b) \
  (__builtin_nds32_kmmsb_u ((r), (a), (b)))
#define __nds32__kwmmul(a, b) \
  (__builtin_nds32_kwmmul ((a), (b)))
#define __nds32__kwmmul_u(a, b) \
  (__builtin_nds32_kwmmul_u ((a), (b)))

#define __nds32__smmwb(a, b) \
  (__builtin_nds32_smmwb ((a), (b)))
#define __nds32__v_smmwb(a, b) \
  (__builtin_nds32_v_smmwb ((a), (b)))
#define __nds32__smmwb_u(a, b) \
  (__builtin_nds32_smmwb_u ((a), (b)))
#define __nds32__v_smmwb_u(a, b) \
  (__builtin_nds32_v_smmwb_u ((a), (b)))
#define __nds32__smmwt(a, b) \
  (__builtin_nds32_smmwt ((a), (b)))
#define __nds32__v_smmwt(a, b) \
  (__builtin_nds32_v_smmwt ((a), (b)))
#define __nds32__smmwt_u(a, b) \
  (__builtin_nds32_smmwt_u ((a), (b)))
#define __nds32__v_smmwt_u(a, b) \
  (__builtin_nds32_v_smmwt_u ((a), (b)))
#define __nds32__kmmawb(r, a, b) \
  (__builtin_nds32_kmmawb ((r), (a), (b)))
#define __nds32__v_kmmawb(r, a, b) \
  (__builtin_nds32_v_kmmawb ((r), (a), (b)))
#define __nds32__kmmawb_u(r, a, b) \
  (__builtin_nds32_kmmawb_u ((r), (a), (b)))
#define __nds32__v_kmmawb_u(r, a, b) \
  (__builtin_nds32_v_kmmawb_u ((r), (a), (b)))
#define __nds32__kmmawt(r, a, b) \
  (__builtin_nds32_kmmawt ((r), (a), (b)))
#define __nds32__v_kmmawt(r, a, b) \
  (__builtin_nds32_v_kmmawt ((r), (a), (b)))
#define __nds32__kmmawt_u(r, a, b) \
  (__builtin_nds32_kmmawt_u ((r), (a), (b)))
#define __nds32__v_kmmawt_u(r, a, b) \
  (__builtin_nds32_v_kmmawt_u ((r), (a), (b)))

#define __nds32__smbb(a, b) \
  (__builtin_nds32_smbb ((a), (b)))
#define __nds32__v_smbb(a, b) \
  (__builtin_nds32_v_smbb ((a), (b)))
#define __nds32__smbt(a, b) \
  (__builtin_nds32_smbt ((a), (b)))
#define __nds32__v_smbt(a, b) \
  (__builtin_nds32_v_smbt ((a), (b)))
#define __nds32__smtt(a, b) \
  (__builtin_nds32_smtt ((a), (b)))
#define __nds32__v_smtt(a, b) \
  (__builtin_nds32_v_smtt ((a), (b)))
#define __nds32__kmda(a, b) \
  (__builtin_nds32_kmda ((a), (b)))
#define __nds32__v_kmda(a, b) \
  (__builtin_nds32_v_kmda ((a), (b)))
#define __nds32__kmxda(a, b) \
  (__builtin_nds32_kmxda ((a), (b)))
#define __nds32__v_kmxda(a, b) \
  (__builtin_nds32_v_kmxda ((a), (b)))
#define __nds32__smds(a, b) \
  (__builtin_nds32_smds ((a), (b)))
#define __nds32__v_smds(a, b) \
  (__builtin_nds32_v_smds ((a), (b)))
#define __nds32__smdrs(a, b) \
  (__builtin_nds32_smdrs ((a), (b)))
#define __nds32__v_smdrs(a, b) \
  (__builtin_nds32_v_smdrs ((a), (b)))
#define __nds32__smxds(a, b) \
  (__builtin_nds32_smxds ((a), (b)))
#define __nds32__v_smxds(a, b) \
  (__builtin_nds32_v_smxds ((a), (b)))
#define __nds32__kmabb(r, a, b) \
  (__builtin_nds32_kmabb ((r), (a), (b)))
#define __nds32__v_kmabb(r, a, b) \
  (__builtin_nds32_v_kmabb ((r), (a), (b)))
#define __nds32__kmabt(r, a, b) \
  (__builtin_nds32_kmabt ((r), (a), (b)))
#define __nds32__v_kmabt(r, a, b) \
  (__builtin_nds32_v_kmabt ((r), (a), (b)))
#define __nds32__kmatt(r, a, b) \
  (__builtin_nds32_kmatt ((r), (a), (b)))
#define __nds32__v_kmatt(r, a, b) \
  (__builtin_nds32_v_kmatt ((r), (a), (b)))
#define __nds32__kmada(r, a, b) \
  (__builtin_nds32_kmada ((r), (a), (b)))
#define __nds32__v_kmada(r, a, b) \
  (__builtin_nds32_v_kmada ((r), (a), (b)))
#define __nds32__kmaxda(r, a, b) \
  (__builtin_nds32_kmaxda ((r), (a), (b)))
#define __nds32__v_kmaxda(r, a, b) \
  (__builtin_nds32_v_kmaxda ((r), (a), (b)))
#define __nds32__kmads(r, a, b) \
  (__builtin_nds32_kmads ((r), (a), (b)))
#define __nds32__v_kmads(r, a, b) \
  (__builtin_nds32_v_kmads ((r), (a), (b)))
#define __nds32__kmadrs(r, a, b) \
  (__builtin_nds32_kmadrs ((r), (a), (b)))
#define __nds32__v_kmadrs(r, a, b) \
  (__builtin_nds32_v_kmadrs ((r), (a), (b)))
#define __nds32__kmaxds(r, a, b) \
  (__builtin_nds32_kmaxds ((r), (a), (b)))
#define __nds32__v_kmaxds(r, a, b) \
  (__builtin_nds32_v_kmaxds ((r), (a), (b)))
#define __nds32__kmsda(r, a, b) \
  (__builtin_nds32_kmsda ((r), (a), (b)))
#define __nds32__v_kmsda(r, a, b) \
  (__builtin_nds32_v_kmsda ((r), (a), (b)))
#define __nds32__kmsxda(r, a, b) \
  (__builtin_nds32_kmsxda ((r), (a), (b)))
#define __nds32__v_kmsxda(r, a, b) \
  (__builtin_nds32_v_kmsxda ((r), (a), (b)))

#define __nds32__smal(a, b) \
  (__builtin_nds32_smal ((a), (b)))
#define __nds32__v_smal(a, b) \
  (__builtin_nds32_v_smal ((a), (b)))

#define __nds32__bitrev(a, b) \
  (__builtin_nds32_bitrev ((a), (b)))
#define __nds32__wext(a, b) \
  (__builtin_nds32_wext ((a), (b)))
#define __nds32__bpick(r, a, b) \
  (__builtin_nds32_bpick ((r), (a), (b)))
#define __nds32__insb(r, a, b) \
  (__builtin_nds32_insb ((r), (a), (b)))

#define __nds32__sadd64(a, b) \
  (__builtin_nds32_sadd64 ((a), (b)))
#define __nds32__uadd64(a, b) \
  (__builtin_nds32_uadd64 ((a), (b)))
#define __nds32__radd64(a, b) \
  (__builtin_nds32_radd64 ((a), (b)))
#define __nds32__uradd64(a, b) \
  (__builtin_nds32_uradd64 ((a), (b)))
#define __nds32__kadd64(a, b) \
  (__builtin_nds32_kadd64 ((a), (b)))
#define __nds32__ukadd64(a, b) \
  (__builtin_nds32_ukadd64 ((a), (b)))
#define __nds32__ssub64(a, b) \
  (__builtin_nds32_ssub64 ((a), (b)))
#define __nds32__usub64(a, b) \
  (__builtin_nds32_usub64 ((a), (b)))
#define __nds32__rsub64(a, b) \
  (__builtin_nds32_rsub64 ((a), (b)))
#define __nds32__ursub64(a, b) \
  (__builtin_nds32_ursub64 ((a), (b)))
#define __nds32__ksub64(a, b) \
  (__builtin_nds32_ksub64 ((a), (b)))
#define __nds32__uksub64(a, b) \
  (__builtin_nds32_uksub64 ((a), (b)))

#define __nds32__smar64(r, a, b) \
  (__builtin_nds32_smar64 ((r), (a), (b)))
#define __nds32__smsr64(r, a, b) \
  (__builtin_nds32_smsr64 ((r), (a), (b)))
#define __nds32__umar64(r, a, b) \
  (__builtin_nds32_umar64 ((r), (a), (b)))
#define __nds32__umsr64(r, a, b) \
  (__builtin_nds32_umsr64 ((r), (a), (b)))
#define __nds32__kmar64(r, a, b) \
  (__builtin_nds32_kmar64 ((r), (a), (b)))
#define __nds32__kmsr64(r, a, b) \
  (__builtin_nds32_kmsr64 ((r), (a), (b)))
#define __nds32__ukmar64(r, a, b) \
  (__builtin_nds32_ukmar64 ((r), (a), (b)))
#define __nds32__ukmsr64(r, a, b) \
  (__builtin_nds32_ukmsr64 ((r), (a), (b)))

#define __nds32__smalbb(r, a, b) \
  (__builtin_nds32_smalbb ((r), (a), (b)))
#define __nds32__v_smalbb(r, a, b) \
  (__builtin_nds32_v_smalbb ((r), (a), (b)))
#define __nds32__smalbt(r, a, b) \
  (__builtin_nds32_smalbt ((r), (a), (b)))
#define __nds32__v_smalbt(r, a, b) \
  (__builtin_nds32_v_smalbt ((r), (a), (b)))
#define __nds32__smaltt(r, a, b) \
  (__builtin_nds32_smaltt ((r), (a), (b)))
#define __nds32__v_smaltt(r, a, b) \
  (__builtin_nds32_v_smaltt ((r), (a), (b)))
#define __nds32__smalda(r, a, b) \
  (__builtin_nds32_smalda ((r), (a), (b)))
#define __nds32__v_smalda(r, a, b) \
  (__builtin_nds32_v_smalda ((r), (a), (b)))
#define __nds32__smalxda(r, a, b) \
  (__builtin_nds32_smalxda ((r), (a), (b)))
#define __nds32__v_smalxda(r, a, b) \
  (__builtin_nds32_v_smalxda ((r), (a), (b)))
#define __nds32__smalds(r, a, b) \
  (__builtin_nds32_smalds ((r), (a), (b)))
#define __nds32__v_smalds(r, a, b) \
  (__builtin_nds32_v_smalds ((r), (a), (b)))
#define __nds32__smaldrs(r, a, b) \
  (__builtin_nds32_smaldrs ((r), (a), (b)))
#define __nds32__v_smaldrs(r, a, b) \
  (__builtin_nds32_v_smaldrs ((r), (a), (b)))
#define __nds32__smalxds(r, a, b) \
  (__builtin_nds32_smalxds ((r), (a), (b)))
#define __nds32__v_smalxds(r, a, b) \
  (__builtin_nds32_v_smalxds ((r), (a), (b)))
#define __nds32__smslda(r, a, b) \
  (__builtin_nds32_smslda ((r), (a), (b)))
#define __nds32__v_smslda(r, a, b) \
  (__builtin_nds32_v_smslda ((r), (a), (b)))
#define __nds32__smslxda(r, a, b) \
  (__builtin_nds32_smslxda ((r), (a), (b)))
#define __nds32__v_smslxda(r, a, b) \
  (__builtin_nds32_v_smslxda ((r), (a), (b)))

#define __nds32__smul16(a, b) \
  (__builtin_nds32_smul16 ((a), (b)))
#define __nds32__v_smul16(a, b) \
  (__builtin_nds32_v_smul16 ((a), (b)))
#define __nds32__smulx16(a, b) \
  (__builtin_nds32_smulx16 ((a), (b)))
#define __nds32__v_smulx16(a, b) \
  (__builtin_nds32_v_smulx16 ((a), (b)))
#define __nds32__umul16(a, b) \
  (__builtin_nds32_umul16 ((a), (b)))
#define __nds32__v_umul16(a, b) \
  (__builtin_nds32_v_umul16 ((a), (b)))
#define __nds32__umulx16(a, b) \
  (__builtin_nds32_umulx16 ((a), (b)))
#define __nds32__v_umulx16(a, b) \
  (__builtin_nds32_v_umulx16 ((a), (b)))

#define __nds32__uclip32(a, imm) \
  (__builtin_nds32_uclip32 ((a), (imm)))
#define __nds32__sclip32(a, imm) \
  (__builtin_nds32_sclip32 ((a), (imm)))
#define __nds32__kabs(a) \
  (__builtin_nds32_kabs ((a)))

#define __nds32__unaligned_feature() \
  (__builtin_nds32_unaligned_feature())
#define __nds32__enable_unaligned() \
  (__builtin_nds32_enable_unaligned())
#define __nds32__disable_unaligned() \
  (__builtin_nds32_disable_unaligned())

#define __nds32__get_unaligned_u16x2(a) \
  (__builtin_nds32_get_unaligned_u16x2 ((a)))
#define __nds32__get_unaligned_s16x2(a) \
  (__builtin_nds32_get_unaligned_s16x2 ((a)))
#define __nds32__get_unaligned_u8x4(a) \
  (__builtin_nds32_get_unaligned_u8x4 ((a)))
#define __nds32__get_unaligned_s8x4(a) \
  (__builtin_nds32_get_unaligned_s8x4 ((a)))

#define __nds32__put_unaligned_u16x2(a, data) \
  (__builtin_nds32_put_unaligned_u16x2 ((a), (data)))
#define __nds32__put_unaligned_s16x2(a, data) \
  (__builtin_nds32_put_unaligned_s16x2 ((a), (data)))
#define __nds32__put_unaligned_u8x4(a, data) \
  (__builtin_nds32_put_unaligned_u8x4 ((a), (data)))
#define __nds32__put_unaligned_s8x4(a, data) \
  (__builtin_nds32_put_unaligned_s8x4 ((a), (data)))

#define NDS32ATTR_SIGNATURE              __attribute__((signature))

#endif /* nds32_intrinsic.h */
