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

#endif /* nds32_intrinsic.h */
