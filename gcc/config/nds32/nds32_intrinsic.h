/* Intrinsic definitions of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2018 Free Software Foundation, Inc.
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

#define __nds32__unaligned_feature() \
  (__builtin_nds32_unaligned_feature())
#define __nds32__enable_unaligned() \
  (__builtin_nds32_enable_unaligned())
#define __nds32__disable_unaligned() \
  (__builtin_nds32_disable_unaligned())
#endif /* nds32_intrinsic.h */
