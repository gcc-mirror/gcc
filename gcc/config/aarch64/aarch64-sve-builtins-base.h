/* ACLE support for AArch64 SVE (__ARM_FEATURE_SVE intrinsics)
   Copyright (C) 2018-2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_SVE_BUILTINS_BASE_H
#define GCC_AARCH64_SVE_BUILTINS_BASE_H

namespace aarch64_sve
{
  namespace functions
  {
    extern const function_base *const svabd;
    extern const function_base *const svabs;
    extern const function_base *const svacge;
    extern const function_base *const svacgt;
    extern const function_base *const svacle;
    extern const function_base *const svaclt;
    extern const function_base *const svadd;
    extern const function_base *const svadda;
    extern const function_base *const svaddv;
    extern const function_base *const svadrb;
    extern const function_base *const svadrd;
    extern const function_base *const svadrh;
    extern const function_base *const svadrw;
    extern const function_base *const svamax;
    extern const function_base *const svamin;
    extern const function_base *const svand;
    extern const function_base *const svandv;
    extern const function_base *const svasr;
    extern const function_base *const svasr_wide;
    extern const function_base *const svasrd;
    extern const function_base *const svbfdot;
    extern const function_base *const svbfdot_lane;
    extern const function_base *const svbfmlalb;
    extern const function_base *const svbfmlalb_lane;
    extern const function_base *const svbfmlalt;
    extern const function_base *const svbfmlalt_lane;
    extern const function_base *const svbfmmla;
    extern const function_base *const svbic;
    extern const function_base *const svbrka;
    extern const function_base *const svbrkb;
    extern const function_base *const svbrkn;
    extern const function_base *const svbrkpa;
    extern const function_base *const svbrkpb;
    extern const function_base *const svcadd;
    extern const function_base *const svclasta;
    extern const function_base *const svclastb;
    extern const function_base *const svcls;
    extern const function_base *const svclz;
    extern const function_base *const svcmla;
    extern const function_base *const svcmla_lane;
    extern const function_base *const svcmpeq;
    extern const function_base *const svcmpeq_wide;
    extern const function_base *const svcmpge;
    extern const function_base *const svcmpge_wide;
    extern const function_base *const svcmpgt;
    extern const function_base *const svcmpgt_wide;
    extern const function_base *const svcmple;
    extern const function_base *const svcmple_wide;
    extern const function_base *const svcmplt;
    extern const function_base *const svcmplt_wide;
    extern const function_base *const svcmpne;
    extern const function_base *const svcmpne_wide;
    extern const function_base *const svcmpuo;
    extern const function_base *const svcnot;
    extern const function_base *const svcnt;
    extern const function_base *const svcntb;
    extern const function_base *const svcntb_pat;
    extern const function_base *const svcntd;
    extern const function_base *const svcntd_pat;
    extern const function_base *const svcnth;
    extern const function_base *const svcnth_pat;
    extern const function_base *const svcntp;
    extern const function_base *const svcntw;
    extern const function_base *const svcntw_pat;
    extern const function_base *const svcompact;
    extern const function_base *const svcreate2;
    extern const function_base *const svcreate3;
    extern const function_base *const svcreate4;
    extern const function_base *const svcvt;
    extern const function_base *const svcvtnt;
    extern const function_base *const svdiv;
    extern const function_base *const svdivr;
    extern const function_base *const svdot;
    extern const function_base *const svdot_lane;
    extern const function_base *const svdup;
    extern const function_base *const svdup_lane;
    extern const function_base *const svdupq;
    extern const function_base *const svdupq_lane;
    extern const function_base *const sveor;
    extern const function_base *const sveorv;
    extern const function_base *const svexpa;
    extern const function_base *const svext;
    extern const function_base *const svextb;
    extern const function_base *const svexth;
    extern const function_base *const svextw;
    extern const function_base *const svget2;
    extern const function_base *const svget3;
    extern const function_base *const svget4;
    extern const function_base *const svindex;
    extern const function_base *const svinsr;
    extern const function_base *const svlasta;
    extern const function_base *const svlastb;
    extern const function_base *const svld1;
    extern const function_base *const svld1_gather;
    extern const function_base *const svld1ro;
    extern const function_base *const svld1rq;
    extern const function_base *const svld1sb;
    extern const function_base *const svld1sb_gather;
    extern const function_base *const svld1sh;
    extern const function_base *const svld1sh_gather;
    extern const function_base *const svld1sw;
    extern const function_base *const svld1sw_gather;
    extern const function_base *const svld1ub;
    extern const function_base *const svld1ub_gather;
    extern const function_base *const svld1uh;
    extern const function_base *const svld1uh_gather;
    extern const function_base *const svld1uw;
    extern const function_base *const svld1uw_gather;
    extern const function_base *const svld2;
    extern const function_base *const svld3;
    extern const function_base *const svld4;
    extern const function_base *const svldff1;
    extern const function_base *const svldff1_gather;
    extern const function_base *const svldff1sb;
    extern const function_base *const svldff1sb_gather;
    extern const function_base *const svldff1sh;
    extern const function_base *const svldff1sh_gather;
    extern const function_base *const svldff1sw;
    extern const function_base *const svldff1sw_gather;
    extern const function_base *const svldff1ub;
    extern const function_base *const svldff1ub_gather;
    extern const function_base *const svldff1uh;
    extern const function_base *const svldff1uh_gather;
    extern const function_base *const svldff1uw;
    extern const function_base *const svldff1uw_gather;
    extern const function_base *const svldnf1;
    extern const function_base *const svldnf1sb;
    extern const function_base *const svldnf1sh;
    extern const function_base *const svldnf1sw;
    extern const function_base *const svldnf1ub;
    extern const function_base *const svldnf1uh;
    extern const function_base *const svldnf1uw;
    extern const function_base *const svldnt1;
    extern const function_base *const svlen;
    extern const function_base *const svlsl;
    extern const function_base *const svlsl_wide;
    extern const function_base *const svlsr;
    extern const function_base *const svlsr_wide;
    extern const function_base *const svmad;
    extern const function_base *const svmax;
    extern const function_base *const svmaxnm;
    extern const function_base *const svmaxnmv;
    extern const function_base *const svmaxv;
    extern const function_base *const svmin;
    extern const function_base *const svminnm;
    extern const function_base *const svminnmv;
    extern const function_base *const svminv;
    extern const function_base *const svmla;
    extern const function_base *const svmla_lane;
    extern const function_base *const svmls;
    extern const function_base *const svmls_lane;
    extern const function_base *const svmmla;
    extern const function_base *const svmov;
    extern const function_base *const svmsb;
    extern const function_base *const svmul;
    extern const function_base *const svmul_lane;
    extern const function_base *const svmulh;
    extern const function_base *const svmulx;
    extern const function_base *const svnand;
    extern const function_base *const svneg;
    extern const function_base *const svnmad;
    extern const function_base *const svnmla;
    extern const function_base *const svnmls;
    extern const function_base *const svnmsb;
    extern const function_base *const svnor;
    extern const function_base *const svnot;
    extern const function_base *const svorn;
    extern const function_base *const svorr;
    extern const function_base *const svorv;
    extern const function_base *const svpfalse;
    extern const function_base *const svpfirst;
    extern const function_base *const svpnext;
    extern const function_base *const svprfb;
    extern const function_base *const svprfb_gather;
    extern const function_base *const svprfd;
    extern const function_base *const svprfd_gather;
    extern const function_base *const svprfh;
    extern const function_base *const svprfh_gather;
    extern const function_base *const svprfw;
    extern const function_base *const svprfw_gather;
    extern const function_base *const svptest_any;
    extern const function_base *const svptest_first;
    extern const function_base *const svptest_last;
    extern const function_base *const svptrue;
    extern const function_base *const svptrue_pat;
    extern const function_base *const svqadd;
    extern const function_base *const svqdecb;
    extern const function_base *const svqdecb_pat;
    extern const function_base *const svqdecd;
    extern const function_base *const svqdecd_pat;
    extern const function_base *const svqdech;
    extern const function_base *const svqdech_pat;
    extern const function_base *const svqdecp;
    extern const function_base *const svqdecw;
    extern const function_base *const svqdecw_pat;
    extern const function_base *const svqincb;
    extern const function_base *const svqincb_pat;
    extern const function_base *const svqincd;
    extern const function_base *const svqincd_pat;
    extern const function_base *const svqinch;
    extern const function_base *const svqinch_pat;
    extern const function_base *const svqincp;
    extern const function_base *const svqincw;
    extern const function_base *const svqincw_pat;
    extern const function_base *const svqsub;
    extern const function_base *const svrbit;
    extern const function_base *const svrdffr;
    extern const function_base *const svrecpe;
    extern const function_base *const svrecps;
    extern const function_base *const svrecpx;
    extern const function_base *const svreinterpret;
    extern const function_base *const svrev;
    extern const function_base *const svrevb;
    extern const function_base *const svrevh;
    extern const function_base *const svrevw;
    extern const function_base *const svrinta;
    extern const function_base *const svrinti;
    extern const function_base *const svrintm;
    extern const function_base *const svrintn;
    extern const function_base *const svrintp;
    extern const function_base *const svrintx;
    extern const function_base *const svrintz;
    extern const function_base *const svrsqrte;
    extern const function_base *const svrsqrts;
    extern const function_base *const svscale;
    extern const function_base *const svsel;
    extern const function_base *const svset2;
    extern const function_base *const svset3;
    extern const function_base *const svset4;
    extern const function_base *const svsetffr;
    extern const function_base *const svsplice;
    extern const function_base *const svsqrt;
    extern const function_base *const svst1;
    extern const function_base *const svst1_scatter;
    extern const function_base *const svst1b;
    extern const function_base *const svst1b_scatter;
    extern const function_base *const svst1h;
    extern const function_base *const svst1h_scatter;
    extern const function_base *const svst1w;
    extern const function_base *const svst1w_scatter;
    extern const function_base *const svst2;
    extern const function_base *const svst3;
    extern const function_base *const svst4;
    extern const function_base *const svstnt1;
    extern const function_base *const svsub;
    extern const function_base *const svsubr;
    extern const function_base *const svsudot;
    extern const function_base *const svsudot_lane;
    extern const function_base *const svtbl;
    extern const function_base *const svtmad;
    extern const function_base *const svtrn1;
    extern const function_base *const svtrn1q;
    extern const function_base *const svtrn2;
    extern const function_base *const svtrn2q;
    extern const function_base *const svtsmul;
    extern const function_base *const svtssel;
    extern const function_base *const svundef;
    extern const function_base *const svundef2;
    extern const function_base *const svundef3;
    extern const function_base *const svundef4;
    extern const function_base *const svunpkhi;
    extern const function_base *const svunpklo;
    extern const function_base *const svusdot;
    extern const function_base *const svusdot_lane;
    extern const function_base *const svusmmla;
    extern const function_base *const svuzp1;
    extern const function_base *const svuzp1q;
    extern const function_base *const svuzp2;
    extern const function_base *const svuzp2q;
    extern const function_base *const svwhilele;
    extern const function_base *const svwhilelt;
    extern const function_base *const svwrffr;
    extern const function_base *const svzip1;
    extern const function_base *const svzip1q;
    extern const function_base *const svzip2;
    extern const function_base *const svzip2q;
  }
  namespace neon_sve_bridge_functions
  {
    extern const function_base *const svset_neonq;
    extern const function_base *const svget_neonq;
    extern const function_base *const svdup_neonq;
  }
}

#endif
