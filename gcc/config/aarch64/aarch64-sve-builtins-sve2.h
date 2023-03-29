/* ACLE support for AArch64 SVE (__ARM_FEATURE_SVE intrinsics)
   Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef GCC_AARCH64_SVE_BUILTINS_SVE2_H
#define GCC_AARCH64_SVE_BUILTINS_SVE2_H

namespace aarch64_sve
{
  namespace functions
  {
    extern const function_base *const svaba;
    extern const function_base *const svabalb;
    extern const function_base *const svabalt;
    extern const function_base *const svabdlb;
    extern const function_base *const svabdlt;
    extern const function_base *const svadalp;
    extern const function_base *const svadclb;
    extern const function_base *const svadclt;
    extern const function_base *const svaddhnb;
    extern const function_base *const svaddhnt;
    extern const function_base *const svaddlb;
    extern const function_base *const svaddlbt;
    extern const function_base *const svaddlt;
    extern const function_base *const svaddp;
    extern const function_base *const svaddwb;
    extern const function_base *const svaddwt;
    extern const function_base *const svaesd;
    extern const function_base *const svaese;
    extern const function_base *const svaesimc;
    extern const function_base *const svaesmc;
    extern const function_base *const svbcax;
    extern const function_base *const svbdep;
    extern const function_base *const svbext;
    extern const function_base *const svbgrp;
    extern const function_base *const svbsl;
    extern const function_base *const svbsl1n;
    extern const function_base *const svbsl2n;
    extern const function_base *const svcdot;
    extern const function_base *const svcdot_lane;
    extern const function_base *const svcvtlt;
    extern const function_base *const svcvtx;
    extern const function_base *const svcvtxnt;
    extern const function_base *const sveor3;
    extern const function_base *const sveorbt;
    extern const function_base *const sveortb;
    extern const function_base *const svhadd;
    extern const function_base *const svhistcnt;
    extern const function_base *const svhistseg;
    extern const function_base *const svhsub;
    extern const function_base *const svhsubr;
    extern const function_base *const svldnt1_gather;
    extern const function_base *const svldnt1sb_gather;
    extern const function_base *const svldnt1sh_gather;
    extern const function_base *const svldnt1sw_gather;
    extern const function_base *const svldnt1ub_gather;
    extern const function_base *const svldnt1uh_gather;
    extern const function_base *const svldnt1uw_gather;
    extern const function_base *const svlogb;
    extern const function_base *const svmatch;
    extern const function_base *const svmaxp;
    extern const function_base *const svmaxnmp;
    extern const function_base *const svmlalb;
    extern const function_base *const svmlalb_lane;
    extern const function_base *const svmlalt;
    extern const function_base *const svmlalt_lane;
    extern const function_base *const svmlslb;
    extern const function_base *const svmlslb_lane;
    extern const function_base *const svmlslt;
    extern const function_base *const svmlslt_lane;
    extern const function_base *const svminp;
    extern const function_base *const svminnmp;
    extern const function_base *const svmovlb;
    extern const function_base *const svmovlt;
    extern const function_base *const svmullb;
    extern const function_base *const svmullb_lane;
    extern const function_base *const svmullt;
    extern const function_base *const svmullt_lane;
    extern const function_base *const svnbsl;
    extern const function_base *const svnmatch;
    extern const function_base *const svpmul;
    extern const function_base *const svpmullb;
    extern const function_base *const svpmullb_pair;
    extern const function_base *const svpmullt;
    extern const function_base *const svpmullt_pair;
    extern const function_base *const svqabs;
    extern const function_base *const svqcadd;
    extern const function_base *const svqdmlalb;
    extern const function_base *const svqdmlalb_lane;
    extern const function_base *const svqdmlalbt;
    extern const function_base *const svqdmlalt;
    extern const function_base *const svqdmlalt_lane;
    extern const function_base *const svqdmlslb;
    extern const function_base *const svqdmlslb_lane;
    extern const function_base *const svqdmlslbt;
    extern const function_base *const svqdmlslt;
    extern const function_base *const svqdmlslt_lane;
    extern const function_base *const svqdmulh;
    extern const function_base *const svqdmulh_lane;
    extern const function_base *const svqdmullb;
    extern const function_base *const svqdmullb_lane;
    extern const function_base *const svqdmullt;
    extern const function_base *const svqdmullt_lane;
    extern const function_base *const svqneg;
    extern const function_base *const svqrdcmlah;
    extern const function_base *const svqrdcmlah_lane;
    extern const function_base *const svqrdmulh;
    extern const function_base *const svqrdmulh_lane;
    extern const function_base *const svqrdmlah;
    extern const function_base *const svqrdmlah_lane;
    extern const function_base *const svqrdmlsh;
    extern const function_base *const svqrdmlsh_lane;
    extern const function_base *const svqrshl;
    extern const function_base *const svqrshrnb;
    extern const function_base *const svqrshrnt;
    extern const function_base *const svqrshrunb;
    extern const function_base *const svqrshrunt;
    extern const function_base *const svqshl;
    extern const function_base *const svqshlu;
    extern const function_base *const svqshrnb;
    extern const function_base *const svqshrnt;
    extern const function_base *const svqshrunb;
    extern const function_base *const svqshrunt;
    extern const function_base *const svqsubr;
    extern const function_base *const svqxtnb;
    extern const function_base *const svqxtnt;
    extern const function_base *const svqxtunb;
    extern const function_base *const svqxtunt;
    extern const function_base *const svraddhnb;
    extern const function_base *const svraddhnt;
    extern const function_base *const svrax1;
    extern const function_base *const svrhadd;
    extern const function_base *const svrshl;
    extern const function_base *const svrshr;
    extern const function_base *const svrshrnb;
    extern const function_base *const svrshrnt;
    extern const function_base *const svrsra;
    extern const function_base *const svrsubhnb;
    extern const function_base *const svrsubhnt;
    extern const function_base *const svsbclb;
    extern const function_base *const svsbclt;
    extern const function_base *const svshllb;
    extern const function_base *const svshllt;
    extern const function_base *const svshrnb;
    extern const function_base *const svshrnt;
    extern const function_base *const svsli;
    extern const function_base *const svsm4e;
    extern const function_base *const svsm4ekey;
    extern const function_base *const svsqadd;
    extern const function_base *const svsra;
    extern const function_base *const svsri;
    extern const function_base *const svstnt1_scatter;
    extern const function_base *const svstnt1b_scatter;
    extern const function_base *const svstnt1h_scatter;
    extern const function_base *const svstnt1w_scatter;
    extern const function_base *const svsubhnb;
    extern const function_base *const svsubhnt;
    extern const function_base *const svsublb;
    extern const function_base *const svsublbt;
    extern const function_base *const svsublt;
    extern const function_base *const svsubltb;
    extern const function_base *const svsubwb;
    extern const function_base *const svsubwt;
    extern const function_base *const svtbl2;
    extern const function_base *const svtbx;
    extern const function_base *const svuqadd;
    extern const function_base *const svwhilege;
    extern const function_base *const svwhilegt;
    extern const function_base *const svwhilerw;
    extern const function_base *const svwhilewr;
    extern const function_base *const svxar;
  }
}

#endif
