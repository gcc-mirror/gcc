/* Copyright (C) 2000, 2001 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */

/* ushmedia.h: Intrinsics corresponding to SHmedia instructions that
   may be executed in both user and privileged mode.  */

#ifndef _USHMEDIA_H
#define _USHMEDIA_H

#if __SHMEDIA__
#if ! __SH4_NO_FPU
typedef float __GCC_FV __attribute__ ((mode (V4SF)));
typedef float __GCC_MTRX __attribute__ ((mode (V16SF)));
#endif

__inline__ static
unsigned long long
sh_media_MABS_L (unsigned long long mm)
{
  unsigned long long res;
  __asm__ ("mabs.l	%1, %0" : "=r" (res) : "r" (mm));
  return res;
}

__inline__ static
unsigned long long
sh_media_MABS_W (unsigned long long mm)
{
  unsigned long long res;
  __asm__ ("mabs.w	%1, %0" : "=r" (res) : "r" (mm));
  return res;
}

__inline__ static
unsigned long long
sh_media_MADD_L (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("madd.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MADD_W (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("madd.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MADDS_L (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("madds.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MADDS_UB (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("madds.ub	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MADDS_W (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("madds.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MCMPEQ_B (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mcmpeq.b	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MCMPEQ_L (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mcmpeq.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MCMPEQ_W (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mcmpeq.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MCMPGT_L (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mcmpgt.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MCMPGT_UB (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mcmpgt.ub	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MCMPGT_W (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mcmpgt.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MCMV (unsigned long long mm, unsigned long long mn, unsigned long long mw)
{
  unsigned long long res;
  __asm__ ("mcmv	%1, %2, %0" : "=r" (res)
	   : "r" (mm), "r" (mn), "0" (mw));
  return res;
}

__inline__ static
unsigned long long
sh_media_MCNVS_LW (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mcnvs.lw	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MCNVS_WB (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mcnvs.wb	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MCNVS_WUB (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mcnvs.wub	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MEXTR1 (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mextr1	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MEXTR2 (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mextr2	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MEXTR3 (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mextr3	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MEXTR4 (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mextr4	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MEXTR5 (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mextr5	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MEXTR6 (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mextr6	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MEXTR7 (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mextr7	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MMACFX_WL (unsigned long long mm, unsigned long long mn, unsigned long long mw)
{
  unsigned long long res;
  __asm__ ("mmacfx.wl	%1, %2, %0" : "=r" (res)
	   : "r" (mm), "r" (mn), "0" (mw));
  return res;
}

__inline__ static
unsigned long long
sh_media_MMACNFX_WL (unsigned long long mm, unsigned long long mn, unsigned long long mw)
{
  unsigned long long res;
  __asm__ ("mmacnfx.wl	%1, %2, %0" : "=r" (res)
	   : "r" (mm), "r" (mn), "0" (mw));
  return res;
}

__inline__ static
unsigned long long
sh_media_MMUL_L (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mmul.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MMUL_W (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mmul.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MMULFX_L (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mmulfx.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MMULFX_W (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mmulfx.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MMULFXRP_W (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mmulfxrp.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MMULHI_WL (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mmulhi.wl	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MMULLO_WL (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mmullo.wl	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MMULSUM_WQ (unsigned long long mm, unsigned long long mn, unsigned long long mw)
{
  unsigned long long res;
  __asm__ ("mmulsum.wq	%1, %2, %0" : "=r" (res)
	   : "r" (mm), "r" (mn), "0" (mw));
  return res;
}

__inline__ static
unsigned long long
sh_media_MPERM_W (unsigned long long mm, unsigned int mn)
{
  unsigned long long res;
  __asm__ ("mperm.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSAD_UBQ (unsigned long long mm, unsigned long long mn, unsigned long long mw)
{
  unsigned long long res;
  __asm__ ("msad.ubq	%1, %2, %0" : "=r" (res)
	   : "r" (mm), "r" (mn), "0" (mw));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHALDS_L (unsigned long long mm, unsigned int mn)
{
  unsigned long long res;
  __asm__ ("mshalds.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHALDS_W (unsigned long long mm, unsigned int mn)
{
  unsigned long long res;
  __asm__ ("mshalds.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHARD_L (unsigned long long mm, unsigned int mn)
{
  unsigned long long res;
  __asm__ ("mshard.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHARD_W (unsigned long long mm, unsigned int mn)
{
  unsigned long long res;
  __asm__ ("mshard.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
short
sh_media_MSHARDS_Q (long long mm, unsigned int mn)
{
  short res;
  __asm__ ("mshards.q	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHFHI_B (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mshfhi.b	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHFHI_L (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mshfhi.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHFHI_W (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mshfhi.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHFLO_B (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mshflo.b	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHFLO_L (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mshflo.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHFLO_W (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("mshflo.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHLLD_L (unsigned long long mm, unsigned int mn)
{
  unsigned long long res;
  __asm__ ("mshlld.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHLLD_W (unsigned long long mm, unsigned int mn)
{
  unsigned long long res;
  __asm__ ("mshlld.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHLRD_L (unsigned long long mm, unsigned int mn)
{
  unsigned long long res;
  __asm__ ("mshlrd.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSHLRD_W (unsigned long long mm, unsigned int mn)
{
  unsigned long long res;
  __asm__ ("mshlrd.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSUB_L (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("msub.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSUB_W (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("msub.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSUBS_L (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("msubs.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSUBS_UB (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("msubs.ub	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
unsigned long long
sh_media_MSUBS_W (unsigned long long mm, unsigned long long mn)
{
  unsigned long long res;
  __asm__ ("msubs.w	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

#if ! __SH4_NOFPU__
__inline__ static
double
sh_media_FABS_D (double dg)
{
  double res;
  __asm__ ("fabs.d	%1, %0" : "=f" (res) : "f" (dg));
  return res;
}

__inline__ static
float 
sh_media_FABS_S (float fg)
{
  float res;
  __asm__ ("fabs.s	%1, %0" : "=f" (res) : "f" (fg));
  return res;
}

__inline__ static
int   
sh_media_FCMPUN_D (double dg, double dh)
{
  int res;
  __asm__ ("fcmpun.d	%1, %2, %0" : "=f" (res) : "f" (dg), "f" (dh));
  return res;
}

__inline__ static
int   
sh_media_FCMPUN_S (float fg, float fh)
{
  int res;
  __asm__ ("fcmpun.s	%1, %2, %0" : "=f" (res) : "f" (fg), "f" (fh));
  return res;
}

__inline__ static
float 
sh_media_FGETSCR (void)
{
  float res;
  __asm__ ("fgetscr	%0" : "=f" (res));
  return res;
}

__inline__ static
float 
sh_media_FIPR_S (const void *fvg, const void *fvh)
{
  float res;
  __asm__ ("fipr.s	%1, %2, %0" : "=f" (res)
	   : "f" (*(const __GCC_FV *)fvg), "f" (*(const __GCC_FV *)fvh));
  return res;
}

__inline__ static
float 
sh_media_FMAC_S (float fg, float fh, float fq)
{
  float res;
  __asm__ ("fmac.s	%1, %2, %0" : "=f" (res)
	   : "f" (fg), "f" (fh), "0" (fq));
  return res;
}

__inline__ static
long long
sh_media_FMOV_DQ (double dg)
{
  long long res;
  __asm__ ("fmov.dq	%1, %0" : "=r" (res) : "f" (dg));
  return res;
}

__inline__ static
float
sh_media_FMOV_LS (int mm)
{
  float res;
  __asm__ ("fmov.ls	%1, %0" : "=f" (res) : "r" (mm));
  return res;
}

__inline__ static
double
sh_media_FMOV_QD (long long mm)
{
  double res;
  __asm__ ("fmov.qd	%1, %0" : "=f" (res) : "r" (mm));
  return res;
}

__inline__ static
int
sh_media_FMOV_SL (float fg)
{
  int res;
  __asm__ ("fmov.sl	%1, %0" : "=r" (res) : "f" (fg));
  return res;
}

__inline__ static
void  
sh_media_FPUTSCR (float fg)
{
  __asm__ ("fputscr	%0" : : "f" (fg));
}

__inline__ static
double
sh_media_FSQRT_D (double dg)
{
  double res;
  __asm__ ("fsqrt.d	%1, %0" : "=f" (res) : "f" (dg));
  return res;
}

__inline__ static
float 
sh_media_FSQRT_S (float fg)
{
  float res;
  __asm__ ("fsqrt.s	%1, %0" : "=f" (res) : "f" (fg));
  return res;
}

__inline__ static
void  
sh_media_FTRV_S (const void *mtrxg, const void *fvh, void *fvf)
{
  __asm__ ("ftrv.s	%2, %1, %0" : "=f" (*(__GCC_FV *)fvf)
	   : "f" (*(const __GCC_FV *)fvh), "f" (*(const __GCC_MTRX *)mtrxg));
}
#endif /* ! __SH4_NOFPU__ */

__inline__ static
unsigned long long
sh_media_LDHI_L (void *p, int s)
{
  unsigned long long res;
  __asm__ ("ldhi.l	%m1, %0" : "=r" (res) : "o" (((char*)p)[s]));
  return res;
}

__inline__ static
unsigned long long
sh_media_LDHI_Q (void *p, int s)
{
  unsigned long long res;
  __asm__ ("ldhi.q	%m1, %0" : "=r" (res) : "o" (((char*)p)[s]));
  return res;
}

__inline__ static
unsigned long long
sh_media_LDLO_L (void *p, int s)
{
  unsigned long long res;
  __asm__ ("ldlo.l	%m1, %0" : "=r" (res) : "o" (((char*)p)[s]));
  return res;
}

__inline__ static
unsigned long long
sh_media_LDLO_Q (void *p, int s)
{
  unsigned long long res;
  __asm__ ("ldlo.q	%m1, %0" : "=r" (res) : "o" (((char*)p)[s]));
  return res;
}

__inline__ static
void     
sh_media_STHI_L (void *p, int s, unsigned int mw)
{
  __asm__ ("sthi.l %m0, %1" : "+o" (((char*)p)[s]) : "r" (mw));
}

__inline__ static
void     
sh_media_STHI_Q (void *p, int s, unsigned long long mw)
{
  __asm__ ("sthi.q %m0, %1" : "+o" (((char*)p)[s]) : "r" (mw));
}

__inline__ static
void     
sh_media_STLO_L (void *p, int s, unsigned int mw)
{
  __asm__ ("stlo.l %m0, %1" : "+o" (((char*)p)[s]) : "r" (mw));
}

__inline__ static
void     
sh_media_STLO_Q (void *p, int s, unsigned long long mw)
{
  __asm__ ("stlo.q %m0, %1" : "+o" (((char*)p)[s]) : "r" (mw));
}

__inline__ static
unsigned char
sh_media_NSB (long long mm)
{
  unsigned char res;
  __asm__ ("nsb	%1, %0" : "=r" (res) : "r" (mm));
  return res;
}

__inline__ static
unsigned long long
sh_media_BYTEREV (unsigned long long mm)
{
  unsigned long long res;
  __asm__ ("byterev	%1, %0" : "=r" (res) : "r" (mm));
  return res;
}

__inline__ static
unsigned long long
sh_media_CMVEQ (unsigned long long mm, unsigned long long mn, unsigned long long mw)
{
  unsigned long long res;
  __asm__ ("cmveq	%1, %2, %0" : "=r" (res)
	   : "r" (mm), "r" (mn), "0" (mw));
  return res;
}

__inline__ static
unsigned long long
sh_media_CMVNE (unsigned long long mm, unsigned long long mn, unsigned long long mw)
{
  unsigned long long res;
  __asm__ ("cmveq	%1, %2, %0" : "=r" (res)
	   : "r" (mm), "r" (mn), "0" (mw));
  return res;
}

__inline__ static
unsigned long long
sh_media_ADDZ_L (unsigned int mm, unsigned int mn)
{
  unsigned long long res;
  __asm__ ("addz.l	%1, %2, %0" : "=r" (res) : "r" (mm), "r" (mn));
  return res;
}

__inline__ static
void
sh_media_NOP (void)
{
  __asm__ __volatile__ ("nop" : :);
}

__inline__ static
unsigned long long
sh_media_SWAP_Q (void *mm, long long mn, unsigned long long mw)
{
  unsigned long long res;
  unsigned long long *addr = (unsigned long long *)((char *)mm + mn);
  __asm__ ("swap.q	%m1, %0" : "=r" (res), "+o" (*addr) : "0" (mw));
  return res;
}

__inline__ static
void     
sh_media_SYNCI (void)
{
  __asm__ __volatile__ ("synci");
}

__inline__ static
void     
sh_media_SYNCO (void)
{
  __asm__ __volatile__ ("synco");
}

__inline__ static
void
sh_media_ALLOCO (void *mm, int s)
{
  __asm__ __volatile__ ("alloco	%m0" : : "o" (((char*)mm)[s]));
}

__inline__ static
void
sh_media_ICBI (void *mm, int s)
{
  __asm__ __volatile__ ("icbi	%m0" : : "o" (((char*)mm)[s]));
}

__inline__ static
void
sh_media_OCBI (void *mm, int s)
{
  __asm__ __volatile__ ("ocbi	%m0" : : "o" (((char*)mm)[s]));
}

__inline__ static
void
sh_media_OCBP (void *mm, int s)
{
  __asm__ __volatile__ ("ocbp	%m0" : : "o" (((char*)mm)[s]));
}

__inline__ static
void
sh_media_OCBWB (void *mm, int s)
{
  __asm__ __volatile__ ("ocbwb	%m0" : : "o" (((char*)mm)[s]));
}

__inline__ static
void
sh_media_PREFI (void *mm, int s)
{
  __asm__ __volatile__ ("prefi	%m0" : : "o" (((char*)mm)[s]));
}

__inline__ static
void
sh_media_PREFO (void *mm, int s)
{
  __asm__ __volatile__ ("ld.b	%m0, r63" : : "o" (((char*)mm)[s]));
}

__inline__ static
void
sh_media_BRK (void)
{
  __asm__ __volatile__ ("brk");
}

__inline__ static
void
sh_media_TRAPA (unsigned long long mm)
{
  __asm__ __volatile__ ("trapa	%%0" : : "r" (mm));
}

__inline__ static
short         
sh_media_unaligned_LD_W (void *p)
{
#if __LITTLE_ENDIAN__
  return (((unsigned char *)p)[0]
	  | (((short)((__signed__ char *)p)[1]) << 8));
#else
  return ((((short)((__signed__ char *)p)[0]) << 8)
	  | ((unsigned char *)p)[1]);
#endif
}

__inline__ static
unsigned short
sh_media_unaligned_LD_UW (void *p)
{
  unsigned char *addr = p;
#if __LITTLE_ENDIAN__
  return sh_media_MSHFLO_B (addr[0], addr[1]);
#else
  return sh_media_MSHFLO_B (addr[1], addr[0]);
#endif
}

__inline__ static
int           
sh_media_unaligned_LD_L (void *p)
{
#if __LITTLE_ENDIAN__
  return sh_media_LDHI_L (p, 3) | sh_media_LDLO_L (p, 0);
#else
  return sh_media_LDLO_L (p, 3) | sh_media_LDHI_L (p, 0);
#endif
}

__inline__ static
long long     
sh_media_unaligned_LD_Q (void *p)
{
#if __LITTLE_ENDIAN__
  return sh_media_LDHI_Q (p, 7) | sh_media_LDLO_Q (p, 0);
#else
  return sh_media_LDLO_Q (p, 7) | sh_media_LDHI_Q (p, 0);
#endif
}

__inline__ static
void
sh_media_unaligned_ST_W (void *p, unsigned int k)
{
  char *addr = p;
#if __LITTLE_ENDIAN__
  addr[0] = k;
  addr[1] = k >> 8;
#else
  addr[1] = k;
  addr[0] = k >> 8;
#endif
}

__inline__ static
void
sh_media_unaligned_ST_L (void *p, unsigned int k)
{
#if __LITTLE_ENDIAN__
  sh_media_STHI_L (p, 3, k);
  sh_media_STLO_L (p, 0, k);
#else
  sh_media_STLO_L (p, 3, k);
  sh_media_STHI_L (p, 0, k);
#endif
}

__inline__ static
void
sh_media_unaligned_ST_Q (void *p, unsigned long long k)
{
#if __LITTLE_ENDIAN__
  sh_media_STHI_Q (p, 7, k);
  sh_media_STLO_Q (p, 0, k);
#else
  sh_media_STLO_Q (p, 7, k);
  sh_media_STHI_Q (p, 0, k);
#endif
}

#if ! __SH4_NOFPU__
__inline__ static
void
sh_media_FVCOPY_S (const void *fvg, void *fvf)
{
  const __GCC_FV *g = fvg;
  __GCC_FV *f = fvf;
  *f = *g;
}

__inline__ static
void
sh_media_FVADD_S (const void *fvg, const void *fvh, void *fvf)
{
  const float *g = fvg, *h = fvh;
  float *f = fvf;
#if 1
  int i;

  for (i = 0; i < 4; i++)
    f[i] = g[i] + h[i];
#else
  f[0] = g[0] + h[0];
  f[1] = g[1] + h[1];
  f[2] = g[2] + h[2];
  f[3] = g[3] + h[3];
#endif
}

__inline__ static
void
sh_media_FVSUB_S (const void *fvg, const void *fvh, void *fvf)
{
  const float *g = fvg, *h = fvh;
  float *f = fvf;
#if 1
  int i;

  for (i = 0; i < 4; i++)
    f[i] = g[i] - h[i];
#else
  f[0] = g[0] - h[0];
  f[1] = g[1] - h[1];
  f[2] = g[2] - h[2];
  f[3] = g[3] - h[3];
#endif
}

__inline__ static
void
sh_media_FMTRXCOPY_S (const void *mtrxg, void *mtrxf)
{
  const __GCC_MTRX *g = mtrxg;
  __GCC_MTRX *f = mtrxf;
  *f = *g;
}

__inline__ static
void
sh_media_FMTRXADD_S (const void *mtrxg, const void *mtrxh, void *mtrxf)
{
  const __GCC_FV *g = mtrxg, *h = mtrxh;
  __GCC_FV *f = mtrxf;
#if 1
  int i;

  for (i = 0; i < 4; i++)
    sh_media_FVADD_S (&g[i], &h[i], &f[i]);
#else
  sh_media_FVADD_S (&g[0], &h[0], &f[0]);
  sh_media_FVADD_S (&g[1], &h[1], &f[1]);
  sh_media_FVADD_S (&g[2], &h[2], &f[2]);
  sh_media_FVADD_S (&g[3], &h[3], &f[3]);
#endif
}

__inline__ static
void
sh_media_FMTRXSUB_S (const void *mtrxg, const void *mtrxh, void *mtrxf)
{
  const __GCC_FV *g = mtrxg, *h = mtrxh;
  __GCC_FV *f = mtrxf;
#if 1
  int i;

  for (i = 0; i < 4; i++)
    sh_media_FVSUB_S (&g[i], &h[i], &f[i]);
#else
  sh_media_FVSUB_S (&g[0], &h[0], &f[0]);
  sh_media_FVSUB_S (&g[1], &h[1], &f[1]);
  sh_media_FVSUB_S (&g[2], &h[2], &f[2]);
  sh_media_FVSUB_S (&g[3], &h[3], &f[3]);
#endif
}

__inline__ static
void
sh_media_FTRVADD_S (const void *mtrxg, const void *fvh, const void *fvi, void *fvf)
{
  sh_media_FTRV_S (mtrxg, fvh, fvf);
  sh_media_FVADD_S (fvf, fvi, fvf);
}

__inline__ static
void
sh_media_FTRVSUB_S (const void *mtrxg, const void *fvh, const void *fvi, void *fvf)
{
  sh_media_FTRV_S (mtrxg, fvh, fvf);
  sh_media_FVSUB_S (fvf, fvi, fvf);
}

__inline__ static
void
sh_media_FMTRXMUL_S (const void *mtrxg, const void *mtrxh, void *mtrxf)
{
  const __GCC_FV *g = mtrxg;
  __GCC_FV *f = mtrxf;
#if 1
  int j;

  for (j = 0; j < 4; j++)
    sh_media_FTRV_S (mtrxh, &g[j], &f[j]);
#else
  sh_media_FTRV_S (mtrxh, &g[0], &f[0]);
  sh_media_FTRV_S (mtrxh, &g[1], &f[1]);
  sh_media_FTRV_S (mtrxh, &g[2], &f[2]);
  sh_media_FTRV_S (mtrxh, &g[3], &f[3]);
#endif
}

__inline__ static
void
sh_media_FMTRXMULADD_S (const void *mtrxg, const void *mtrxh, const void *mtrxi, void *mtrxf)
{
  const __GCC_FV *g = mtrxg, *i = mtrxi;
  __GCC_FV *f = mtrxf;
#if 1
  int j;

  for (j = 0; j < 4; j++)
    sh_media_FTRVADD_S (mtrxh, &g[j], &i[j], &f[j]);
#else
  sh_media_FTRVADD_S (mtrxh, &g[0], &i[0], &f[0]);
  sh_media_FTRVADD_S (mtrxh, &g[1], &i[1], &f[1]);
  sh_media_FTRVADD_S (mtrxh, &g[2], &i[2], &f[2]);
  sh_media_FTRVADD_S (mtrxh, &g[3], &i[3], &f[3]);
#endif
}

__inline__ static
void
sh_media_FMTRXMULSUB_S (const void *mtrxg, const void *mtrxh, const void *mtrxi, void *mtrxf)
{
  const __GCC_FV *g = mtrxg, *i = mtrxi;
  __GCC_FV *f = mtrxf;
#if 1
  int j;

  for (j = 0; j < 4; j++)
    sh_media_FTRVSUB_S (mtrxh, &g[j], &i[j], &f[j]);
#else
  sh_media_FTRVSUB_S (mtrxh, &g[0], &i[0], &f[0]);
  sh_media_FTRVSUB_S (mtrxh, &g[1], &i[1], &f[1]);
  sh_media_FTRVSUB_S (mtrxh, &g[2], &i[2], &f[2]);
  sh_media_FTRVSUB_S (mtrxh, &g[3], &i[3], &f[3]);
#endif
}
#endif /* ! __SH4_NOFPU__ */

#endif /* __SHMEDIA__ */

#endif /* _USHMEDIA_H */
