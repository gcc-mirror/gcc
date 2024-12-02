/* function_base declaration for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
   Contributed by Ju-Zhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

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

#ifndef GCC_RISCV_VECTOR_BUILTINS_BASES_H
#define GCC_RISCV_VECTOR_BUILTINS_BASES_H

namespace riscv_vector {

enum frm_op_type
{
  NO_FRM,
  HAS_FRM,
};

namespace bases {
extern const function_base *const vsetvl;
extern const function_base *const vsetvlmax;
extern const function_base *const vle;
extern const function_base *const vse;
extern const function_base *const vlm;
extern const function_base *const vsm;
extern const function_base *const vlse;
extern const function_base *const vsse;
extern const function_base *const vluxei8;
extern const function_base *const vluxei16;
extern const function_base *const vluxei32;
extern const function_base *const vluxei64;
extern const function_base *const vloxei8;
extern const function_base *const vloxei16;
extern const function_base *const vloxei32;
extern const function_base *const vloxei64;
extern const function_base *const vsuxei8;
extern const function_base *const vsuxei16;
extern const function_base *const vsuxei32;
extern const function_base *const vsuxei64;
extern const function_base *const vsoxei8;
extern const function_base *const vsoxei16;
extern const function_base *const vsoxei32;
extern const function_base *const vsoxei64;
extern const function_base *const vadd;
extern const function_base *const vsub;
extern const function_base *const vrsub;
extern const function_base *const vand;
extern const function_base *const vor;
extern const function_base *const vxor;
extern const function_base *const vsll;
extern const function_base *const vsra;
extern const function_base *const vsrl;
extern const function_base *const vmin;
extern const function_base *const vmax;
extern const function_base *const vminu;
extern const function_base *const vmaxu;
extern const function_base *const vmul;
extern const function_base *const vmulh;
extern const function_base *const vmulhu;
extern const function_base *const vmulhsu;
extern const function_base *const vdiv;
extern const function_base *const vrem;
extern const function_base *const vdivu;
extern const function_base *const vremu;
extern const function_base *const vneg;
extern const function_base *const vnot;
extern const function_base *const vsext;
extern const function_base *const vzext;
extern const function_base *const vwadd;
extern const function_base *const vwsub;
extern const function_base *const vwmul;
extern const function_base *const vwaddu;
extern const function_base *const vwsubu;
extern const function_base *const vwmulu;
extern const function_base *const vwmulsu;
extern const function_base *const vwcvt_x;
extern const function_base *const vwcvtu_x;
extern const function_base *const vadc;
extern const function_base *const vsbc;
extern const function_base *const vmadc;
extern const function_base *const vmsbc;
extern const function_base *const vnsrl;
extern const function_base *const vnsra;
extern const function_base *const vncvt_x;
extern const function_base *const vmerge;
extern const function_base *const vmv_v;
extern const function_base *const vmseq;
extern const function_base *const vmsne;
extern const function_base *const vmslt;
extern const function_base *const vmsgt;
extern const function_base *const vmsle;
extern const function_base *const vmsge;
extern const function_base *const vmsltu;
extern const function_base *const vmsgtu;
extern const function_base *const vmsleu;
extern const function_base *const vmsgeu;
extern const function_base *const vmacc;
extern const function_base *const vnmsac;
extern const function_base *const vmadd;
extern const function_base *const vnmsub;
extern const function_base *const vwmacc;
extern const function_base *const vwmaccu;
extern const function_base *const vwmaccsu;
extern const function_base *const vwmaccus;
extern const function_base *const vsadd;
extern const function_base *const vssub;
extern const function_base *const vsaddu;
extern const function_base *const vssubu;
extern const function_base *const vaadd;
extern const function_base *const vasub;
extern const function_base *const vaaddu;
extern const function_base *const vasubu;
extern const function_base *const vsmul;
extern const function_base *const vssra;
extern const function_base *const vssrl;
extern const function_base *const vnclip;
extern const function_base *const vnclipu;
extern const function_base *const vmand;
extern const function_base *const vmnand;
extern const function_base *const vmandn;
extern const function_base *const vmxor;
extern const function_base *const vmor;
extern const function_base *const vmnor;
extern const function_base *const vmorn;
extern const function_base *const vmxnor;
extern const function_base *const vmmv;
extern const function_base *const vmclr;
extern const function_base *const vmset;
extern const function_base *const vmnot;
extern const function_base *const vcpop;
extern const function_base *const vfirst;
extern const function_base *const vmsbf;
extern const function_base *const vmsif;
extern const function_base *const vmsof;
extern const function_base *const viota;
extern const function_base *const vid;
extern const function_base *const vfadd;
extern const function_base *const vfadd_frm;
extern const function_base *const vfsub;
extern const function_base *const vfsub_frm;
extern const function_base *const vfrsub;
extern const function_base *const vfrsub_frm;
extern const function_base *const vfwadd;
extern const function_base *const vfwadd_frm;
extern const function_base *const vfwsub;
extern const function_base *const vfwsub_frm;
extern const function_base *const vfmul;
extern const function_base *const vfmul_frm;
extern const function_base *const vfdiv;
extern const function_base *const vfdiv_frm;
extern const function_base *const vfrdiv;
extern const function_base *const vfrdiv_frm;
extern const function_base *const vfwmul;
extern const function_base *const vfwmul_frm;
extern const function_base *const vfmacc;
extern const function_base *const vfmacc_frm;
extern const function_base *const vfnmsac;
extern const function_base *const vfnmsac_frm;
extern const function_base *const vfmadd;
extern const function_base *const vfmadd_frm;
extern const function_base *const vfnmsub;
extern const function_base *const vfnmsub_frm;
extern const function_base *const vfnmacc;
extern const function_base *const vfnmacc_frm;
extern const function_base *const vfmsac;
extern const function_base *const vfmsac_frm;
extern const function_base *const vfnmadd;
extern const function_base *const vfnmadd_frm;
extern const function_base *const vfmsub;
extern const function_base *const vfmsub_frm;
extern const function_base *const vfwmacc;
extern const function_base *const vfwmacc_frm;
extern const function_base *const vfwnmacc;
extern const function_base *const vfwnmacc_frm;
extern const function_base *const vfwmsac;
extern const function_base *const vfwmsac_frm;
extern const function_base *const vfwnmsac;
extern const function_base *const vfwnmsac_frm;
extern const function_base *const vfsqrt;
extern const function_base *const vfsqrt_frm;
extern const function_base *const vfrsqrt7;
extern const function_base *const vfrec7;
extern const function_base *const vfrec7_frm;
extern const function_base *const vfmin;
extern const function_base *const vfmax;
extern const function_base *const vfsgnj;
extern const function_base *const vfsgnjn;
extern const function_base *const vfsgnjx;
extern const function_base *const vfneg;
extern const function_base *const vfabs;
extern const function_base *const vmfeq;
extern const function_base *const vmfne;
extern const function_base *const vmflt;
extern const function_base *const vmfgt;
extern const function_base *const vmfle;
extern const function_base *const vmfge;
extern const function_base *const vfclass;
extern const function_base *const vfmerge;
extern const function_base *const vfmv_v;
extern const function_base *const vfcvt_x;
extern const function_base *const vfcvt_x_frm;
extern const function_base *const vfcvt_xu;
extern const function_base *const vfcvt_xu_frm;
extern const function_base *const vfcvt_rtz_x;
extern const function_base *const vfcvt_rtz_xu;
extern const function_base *const vfcvt_f;
extern const function_base *const vfcvt_f_frm;
extern const function_base *const vfwcvt_x;
extern const function_base *const vfwcvt_x_frm;
extern const function_base *const vfwcvt_xu;
extern const function_base *const vfwcvt_xu_frm;
extern const function_base *const vfwcvt_rtz_x;
extern const function_base *const vfwcvt_rtz_xu;
extern const function_base *const vfwcvt_f;
extern const function_base *const vfncvt_x;
extern const function_base *const vfncvt_x_frm;
extern const function_base *const vfncvt_xu;
extern const function_base *const vfncvt_xu_frm;
extern const function_base *const vfncvt_rtz_x;
extern const function_base *const vfncvt_rtz_xu;
extern const function_base *const vfncvt_f;
extern const function_base *const vfncvt_f_frm;
extern const function_base *const vfncvt_rod_f;
extern const function_base *const vredsum;
extern const function_base *const vredmaxu;
extern const function_base *const vredmax;
extern const function_base *const vredminu;
extern const function_base *const vredmin;
extern const function_base *const vredand;
extern const function_base *const vredor;
extern const function_base *const vredxor;
extern const function_base *const vwredsum;
extern const function_base *const vwredsumu;
extern const function_base *const vfredusum;
extern const function_base *const vfredusum_frm;
extern const function_base *const vfredosum;
extern const function_base *const vfredosum_frm;
extern const function_base *const vfredmax;
extern const function_base *const vfredmin;
extern const function_base *const vfwredosum;
extern const function_base *const vfwredosum_frm;
extern const function_base *const vfwredusum;
extern const function_base *const vfwredusum_frm;
extern const function_base *const vmv_x;
extern const function_base *const vmv_s;
extern const function_base *const vfmv_f;
extern const function_base *const vfmv_s;
extern const function_base *const vslideup;
extern const function_base *const vslidedown;
extern const function_base *const vslide1up;
extern const function_base *const vslide1down;
extern const function_base *const vfslide1up;
extern const function_base *const vfslide1down;
extern const function_base *const vrgather;
extern const function_base *const vrgatherei16;
extern const function_base *const vcompress;
extern const function_base *const vundefined;
extern const function_base *const vreinterpret;
extern const function_base *const vlmul_ext;
extern const function_base *const vlmul_trunc;
extern const function_base *const vset;
extern const function_base *const vget;
extern const function_base *const vcreate;
extern const function_base *const read_vl;
extern const function_base *const vleff;
extern const function_base *const vlenb;
extern const function_base *const vlseg;
extern const function_base *const vsseg;
extern const function_base *const vlsseg;
extern const function_base *const vssseg;
extern const function_base *const vluxseg;
extern const function_base *const vloxseg;
extern const function_base *const vsuxseg;
extern const function_base *const vsoxseg;
extern const function_base *const vlsegff;
extern const function_base *const vlb;
extern const function_base *const vlh;
extern const function_base *const vlw;
extern const function_base *const vlbu;
extern const function_base *const vlhu;
extern const function_base *const vlwu;
extern const function_base *const vsb;
extern const function_base *const vsh;
extern const function_base *const vsw;
extern const function_base *const vlsb;
extern const function_base *const vlsh;
extern const function_base *const vlsw;
extern const function_base *const vlsbu;
extern const function_base *const vlshu;
extern const function_base *const vlswu;
extern const function_base *const vssb;
extern const function_base *const vssh;
extern const function_base *const vssw;
extern const function_base *const vlxb;
extern const function_base *const vlxh;
extern const function_base *const vlxw;
extern const function_base *const vlxbu;
extern const function_base *const vlxhu;
extern const function_base *const vlxwu;
extern const function_base *const vsxb;
extern const function_base *const vsxh;
extern const function_base *const vsxw;
extern const function_base *const vsuxb;
extern const function_base *const vsuxh;
extern const function_base *const vsuxw;
extern const function_base *const vext_x_v;
/* Below function_base are Vectro Crypto*/
extern const function_base *const vandn;
extern const function_base *const vbrev;
extern const function_base *const vbrev8;
extern const function_base *const vrev8;
extern const function_base *const vclz;
extern const function_base *const vctz;
extern const function_base *const vrol;
extern const function_base *const vror;
extern const function_base *const vwsll;
extern const function_base *const vclmul;
extern const function_base *const vclmulh;
extern const function_base *const vghsh;
extern const function_base *const vgmul;
extern const function_base *const vaesef;
extern const function_base *const vaesem;
extern const function_base *const vaesdf;
extern const function_base *const vaesdm;
extern const function_base *const vaesz;
extern const function_base *const vaeskf1;
extern const function_base *const vaeskf2;
extern const function_base *const vsha2ms;
extern const function_base *const vsha2ch;
extern const function_base *const vsha2cl;
extern const function_base *const vsm4k;
extern const function_base *const vsm4r;
extern const function_base *const vsm3me;
extern const function_base *const vsm3c;
/* Zvfbfmin*/
extern const function_base *const vfncvtbf16_f;
extern const function_base *const vfncvtbf16_f_frm;
extern const function_base *const vfwcvtbf16_f;
/* Zvfbfwma */
extern const function_base *const vfwmaccbf16;
extern const function_base *const vfwmaccbf16_frm;
}

} // end namespace riscv_vector

#endif
