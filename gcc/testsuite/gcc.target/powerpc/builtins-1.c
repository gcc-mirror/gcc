/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-additional-options "-mbig" { target powerpc64le-*-* } } */

#include <altivec.h>

vector float
foo0 (vector float fa, vector float fb)
{
  return vec_and (fa, fb);
}

vector float
foo1 (vector float fa, vector float fb)
{
  return vec_cpsgn (fa, fb);
}

vector float
foo2 (vector float fa, vector float fb)
{
  return vec_mergeh (fa, fb);
}

vector float
foo3 (vector float fa, vector float fb)
{
  return vec_mergel (fa, fb);
}

vector double
foo4 (vector double da, vector double db)
{
  return vec_and (da, db);
}

vector long long
foo5 (vector long long la, vector long long lb)
{
  return vec_and (la, lb);
}

vector long long
foo6 (vector long long la, vector bool long long ld)
{
  return vec_and (la, ld);
}

vector long long
foo7 (vector bool long long ld, vector long long lb)
{
  return vec_and (ld, lb);
}

vector unsigned long long
foo8 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_and (ua, ub);
}

vector unsigned long long
foo9 (vector unsigned long long ua, vector bool long long ud)
{
  return vec_and (ua, ud);
}

vector unsigned long long
foo10 (vector bool long long ud, vector unsigned long long ub)
{
  return vec_and (ud, ub);
}

vector long long
foo11 (vector long long la, vector long long lb)
{
  return vec_andc (la, lb);
}

vector long long
foo12 (vector long long la, vector bool long long ld)
{
  return vec_andc (la, ld);
}

vector long long
foo13 (vector bool long long ld, vector long long lb)
{
  return vec_andc (ld, lb);
}

vector unsigned long long
foo14 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_andc (ua, ub);
}

vector unsigned long long
foo15 (vector unsigned long long ua, vector bool long long ud)
{
  return vec_andc (ua, ud);
}

vector unsigned long long
foo16 (vector bool long long ud, vector unsigned long long ub)
{
  return vec_andc (ud, ub);
}

vector double
foo17 (vector double de, vector double df)
{
  return vec_cpsgn (de, df);
}

vector double
foo18 (vector double de, vector double df)
{
  return vec_mergeh (de, df);
}

vector double
foo19 (vector double de, vector double df)
{
  return vec_mergel (de, df);
}

vector long long
foo20 (vector long long la, vector long long lb)
{
  return vec_mergeh (la, lb);
}

vector long long
foo21 (vector long long la, vector bool long long ld)
{
  return vec_mergeh (la, ld);
}

vector long long
foo22 (vector bool long long ld, vector long long lb)
{
  return vec_mergeh (ld, lb);
}

vector unsigned long long
foo23 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_mergeh (ua, ub);
}

vector unsigned long long
foo24 (vector unsigned long long ua, vector bool long long ud)
{
  return vec_mergeh (ua, ud);
}

vector unsigned long long
foo25 (vector bool long long ud, vector unsigned long long ub)
{
  return vec_mergeh (ud, ub);
}

vector pixel
foo26 (vector pixel pa, vector pixel pb)
{
  return vec_mergeh (pa, pb);
}

vector pixel
foo27 (vector pixel pa, vector pixel pb)
{
  return vec_mergel (pa, pb);
}

vector long long
foo28 (vector long long la, vector long long lb)
{
  return vec_mergel (la, lb);
}

vector long long
foo29 (vector long long la, vector bool long long ld)
{
  return vec_mergel (la, ld);
}

vector long long
foo30 (vector bool long long ld, vector long long lb)
{
  return vec_mergel (ld, lb);
}

vector unsigned long long
foo31 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_mergel (ua, ub);
}

vector unsigned long long
foo32 (vector unsigned long long ua, vector bool long long ud)
{
  return vec_mergel (ua, ud);
}

vector unsigned long long
foo33 (vector bool long long ud, vector unsigned long long ub)
{
  return vec_mergel (ud, ub);
}

vector long long
foo34 (vector long long la, vector long long lb)
{
  return vec_nor (la, lb);
}

vector long long
foo35 (vector long long la, vector bool long long ld)
{
  return vec_nor (la, ld);
}

vector long long
foo36 (vector bool long long ld, vector long long lb)
{
  return vec_nor (ld, lb);
}

vector unsigned long long
foo37 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_nor (ua, ub);
}

vector unsigned long long
foo38 (vector unsigned long long ua, vector bool long long ud)
{
  return vec_nor (ua, ud);
}

vector unsigned long long
foo39 (vector bool long long ud, vector unsigned long long ub)
{
  return vec_nor (ud, ub);
}

vector long long
foo40 (vector long long la, vector long long lb, vector unsigned char ca)
{
  return vec_perm (la, lb, ca);
}

vector unsigned char
foo41 (vector unsigned char ca, vector unsigned char cbb)
{
  return vec_and (ca, cbb);
}

vector unsigned char
foo42 (vector unsigned char ca, vector unsigned char cbb)
{
  return vec_andc (ca, cbb);
}

vector unsigned char
foo43 (vector unsigned char ca, vector unsigned char cbb)
{
  return vec_mergel (ca, cbb);
}

vector unsigned long long
foo44 (vector unsigned long long ua, vector unsigned long long ub,
       vector unsigned char ca)
{
  return vec_perm (ua, ub, ca);
}

vector long long
foo45 (vector long long la, vector long long lb, vector unsigned long long uc)
{
  return vec_sel (la, lb, uc);
}

vector long long
foo46 (vector long long la, vector long long lb, vector bool long long ld)
{
  return vec_sel (la, lb, ld);
}

vector unsigned long long
foo47 (vector unsigned long long ua, vector unsigned long long ub,
       vector long long lc)
{
  return vec_sel (ua, ub, lc);
}

vector unsigned long long
foo48 (vector unsigned long long ua, vector unsigned long long ub,
       vector unsigned long long uc)
{
  return vec_sel (ua, ub, uc);
}

vector unsigned long long
foo49 (vector unsigned long long ua, vector unsigned long long ub,
       vector bool long long ld)
{
  return vec_sel (ua, ub, ld);
}

vector long long
foo50 (vector long long la, vector long long lb)
{
  return vec_xor (la, lb);
}

vector long long
foo51 (vector long long la, vector bool long long ld)
{
  return vec_xor (la, ld);
}

vector long long
foo52 (vector bool long long ld, vector long long la)
{
  return vec_xor (ld, la);
}

vector unsigned long long
foo53 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_xor (ua, ub);
}

vector unsigned long long
foo54 (vector unsigned long long ua, vector bool long long ud)
{
  return vec_xor (ua, ud);
}

vector unsigned long long
foo55 (vector bool long long ud, vector unsigned long long ua)
{
  return vec_xor (ud, ua);
}

int
foo56 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_all_eq (ua, ub);
}

int
foo57 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_all_ge (ua, ub);
}

int
foo58 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_all_gt (ua, ub);
}

int
foo59 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_all_le (ua, ub);
}

int
foo60 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_all_lt (ua, ub);
}

int
foo61 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_all_ne (ua, ub);
}

int
foo62 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_any_eq (ua, ub);
}

int
foo63 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_any_ge (ua, ub);
}

int
foo64 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_any_gt (ua, ub);
}

int
foo65 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_any_le (ua, ub);
}

int
foo66 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_any_lt (ua, ub);
}

int
foo67 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_any_ne (ua, ub);
}

vector short
foo68 (vector short ssa, vector short ssb)
{
  return vec_and (ssa, ssb);
}

vector short
foo69 (vector short ssa, vector short ssb)
{
  return vec_mergeh (ssa, ssb);
}

vector short
foo70 (vector short ssa, vector short ssb)
{
  return vec_mergel (ssa, ssb);
}

vector int
foo71 (vector int sia, vector int sib)
{
  return vec_and (sia, sib);
}

vector int
foo72 (vector int sia, vector int sib)
{
  return vec_andc (sia, sib);
}

vector int
foo73 (vector int sia, vector int sib)
{
  return vec_mergel (sia, sib);
}

vector unsigned int
foo74 (vector unsigned int uia, vector unsigned int uib)
{
  return vec_and (uia, uib);
}

vector unsigned int
foo75 (vector unsigned int uia, vector unsigned int uib)
{
  return vec_andc (uia, uib);
}

vector unsigned int
foo76 (vector unsigned int uia, vector unsigned int uib)
{
  return vec_mergel (uia, uib);
}

vector bool char
foo77 (vector bool char bca, vector bool char bcb)
{
  return vec_and (bca, bcb);
}

vector bool char
foo78 (vector bool char bca, vector bool char bcb)
{
  return vec_andc (bca, bcb);
}

vector bool char
foo79 (vector bool char bca, vector bool char bcb)
{
  return vec_mergel (bca, bcb);
}

vector bool short
foo80 (vector bool short bsa, vector bool short bsb)
{
  return vec_and (bsa, bsb);
}

vector bool short
foo81 (vector bool short bsa, vector bool short bsb)
{
  return vec_andc (bsa, bsb);
}

vector bool short
foo82 (vector bool short bsa, vector bool short bsb)
{
  return vec_mergel (bsa, bsb);
}

vector bool int
foo83 (vector bool int bia, vector bool int bib)
{
  return vec_and (bia, bib);
}

vector bool int
foo84 (vector bool int bia, vector bool int bib)
{
  return vec_andc (bia, bib);
}

vector bool int
foo85 (vector bool int bia, vector bool int bib)
{
  return vec_mergel (bia, bib);
}

vector unsigned int
foo86 (vector unsigned long long ua, vector unsigned long long ub)
{
  return vec_packsu (ua, ub);
}

vector long long
foo87 (vector long long la)
{
  return vec_cntlz (la);
}

vector unsigned long long
foo88 (vector unsigned long long ua)
{
  return vec_cntlz (ua);
}

vector int
foo89 (vector int sia)
{
  return vec_cntlz (sia);
}

vector unsigned int
foo90 (vector unsigned int uia)
{
  return vec_cntlz (uia);
}

vector short
foo91 (vector short ssa)
{
  return vec_cntlz (ssa);
}

vector unsigned short
foo92 (vector unsigned short usa, vector unsigned short usb)
{
  return vec_and (usa, usb);
}

vector unsigned short
foo93 (vector unsigned short usa, vector unsigned short usb)
{
  return vec_andc (usa, usb);
}

vector unsigned short
foo94 (vector unsigned short usa)
{
  return vec_cntlz (usa);
}

vector unsigned short
foo95 (vector unsigned short usa, vector unsigned short usb)
{
  return vec_mergeh (usa, usb);
}

vector unsigned short
foo96 (vector unsigned short usa, vector unsigned short usb)
{
  return vec_mergel (usa, usb);
}

vector signed char
foo97 (vector signed char sca)
{
  return vec_cntlz (sca);
}

vector signed char
foo98 (vector signed char sca, vector signed char scb)
{
  return vec_mergel (sca, scb);
}

vector unsigned char
foo99 (vector unsigned char ca)
{
  return vec_cntlz (ca);
}

vector double
foo100 (vector double *y)
{
  return vec_xl (0, y);
}

void
foo101 (vector double dd, vector double *z)
{
  vec_xst (dd, 0, z);
}

vector double
foo102 (vector double dd)
{
  return vec_round (dd);
}

vector double
foo103 (vector double dd)
{
  return vec_rsqrt (dd);
}

vector double
foo104 (vector double dd)
{
  return vec_rsqrte (dd);
}

vector float
foo105 (vector float fa)
{
  return vec_round (fa);
}

vector float
foo106 (vector float fa)
{
  return vec_rsqrt (fa);
}

vector float
foo107 (vector float fa)
{
  return vec_rsqrte (fa);
}

vector double
foo108 (vector double de)
{
  return vec_splat (de, 0);
}

vector double
foo109 (vector double de)
{
  return vec_splat (de, 1);
}

vector long long
foo110 (vector long long l2)
{
  return vec_splat (l2, 0);
}

vector long long
foo111 (vector long long l2)
{
  return vec_splat (l2, 1);
}

vector unsigned long long
foo112 (vector unsigned long long u2)
{
  return vec_splat (u2, 0);
}

vector unsigned long long
foo113 (vector unsigned long long u2)
{
  return vec_splat (u2, 1);
}

vector bool long long
foo114 (vector bool long long ld)
{
  return vec_splat (ld, 0);
}

vector bool long long
foo115 (vector bool long long ld)
{
  return vec_splat (ld, 1);
}

vector bool long long
foo116 (vector bool long long la, vector bool long long lb)
{
  return vec_mergee (la, lb);
}

vector bool long long
foo117 (vector bool long long la, vector bool long long lb)
{
  return vec_mergeo (la, lb);
}

vector bool long long
foo118 (vector bool long long la, vector bool long long lb)
{
  return vec_and (la, lb);
}

vector long long
foo119 (vector long long l3, vector long long l4)
{
  return vec_div (l3, l4);
}

vector unsigned long long
foo120 (vector unsigned long long u3, vector unsigned long long u4)
{
  return vec_div (u3, u4);
}

vector long long
foo121 (vector long long la, vector long long lb)
{
  return vec_mergee (la, lb);
}

vector long long
foo122 (vector long long la, vector long long lb)
{
  return vec_mergeo (la, lb);
}

vector unsigned long long
foo123 (vector unsigned long long u3, vector unsigned long long u4)
{
  return vec_mergee (u3, u4);
}

vector unsigned long long
foo124 (vector unsigned long long u3, vector unsigned long long u4)
{
  return vec_mergeo (u3, u4);
}

vector long long
foo125 (vector long long l3, vector long long l4)
{
  return vec_mul (l3, l4);
}

vector unsigned long long
foo126 (vector unsigned long long u3, vector unsigned long long u4)
{
  return vec_mul (u3, u4);
}

vector int
foo127 (vector float fa)
{
  return vec_cts (fa, 0x1F);
}

vector unsigned int
foo128 (vector float fa)
{
  return vec_ctu (fa, 0x1F);
}

vector float
foo129 (vector float fa, vector float fb)
{
  return vec_mergee (fa, fb);
}

vector float
foo130 (vector float fa, vector float fb)
{
  return vec_mergeo (fa, fb);
}

vector double
foo131 (vector double da, vector double db)
{
  return vec_mergee (da, db);
}

vector double
foo132 (vector double da, vector double db)
{
  return vec_mergeo (da, db);
}

vector float
foo133 (vector signed int si_a)
{
  return vec_ctf (si_a, 1);
}

vector float
foo134 (vector unsigned int ui_a)
{
  return vec_ctf (ui_a, 2);
}

vector bool char
foo135 (vector bool char bca)
{
  return vec_splat (bca, 0);
}

vector signed char
foo136 (vector signed char sca)
{
  return vec_splat (sca, 1);
}

vector unsigned char
foo137 (vector unsigned char ucbc)
{
  return vec_splat (ucbc, 2);
}

vector bool int
foo138 (vector bool int bia)
{
  return vec_splat (bia, 3);
}

vector signed int
foo139 (vector signed int sia)
{
  return vec_splat (sia, 1);
}

vector unsigned int
foo140 (vector unsigned int uia)
{
  return vec_splat (uia, 2);
}

vector bool int
foo141 (vector bool int bia, vector bool int bib)
{
  return vec_mergee (bia, bib);
}

vector signed int
foo142 (vector signed int sia, vector signed int sib)
{
  return vec_mergee (sia, sib);
}

vector unsigned int
foo143 (vector unsigned int uia, vector unsigned int uib)
{
  return vec_mergee (uia, uib);
}

vector bool char
foo144 (vector bool char bca, vector bool char bcb)
{
  return vec_mergeh (bca, bcb);
}

vector signed char
foo145 (vector signed char sca, vector signed char scb)
{
  return vec_mergeh (sca, scb);
}

vector bool int
foo146 (vector bool int bia, vector bool int bib)
{
  return vec_mergeh (bia, bib);
}

vector signed int
foo147 (vector signed int sia, vector signed int sib)
{
  return vec_mergeh (sia, sib);
}

vector unsigned int
foo148 (vector unsigned int uia, vector unsigned int uib)
{
  return vec_mergeh (uia, uib);
}

vector bool short
foo149 (vector bool short bsa, vector bool short bsb)
{
  return vec_mergeh (bsa, bsb);
}

vector bool int
foo150 (vector bool int bia, vector bool int bib)
{
  return vec_mergeo (bia, bib);
}

vector signed int
foo151 (vector signed int sia, vector signed int sib)
{
  return vec_mergeo (sia, sib);
}

vector unsigned int
foo152 (vector unsigned int uia, vector unsigned int uib)
{
  return vec_mergeo (uia, uib);
}

vector pixel
foo153 (vector pixel pa)
{
  return vec_splat (pa, 1);
}

vector bool short
foo154 (vector bool short bsa)
{
  return vec_splat (bsa, 0);
}

vector signed short
foo155 (vector signed short ssa)
{
  return vec_splat (ssa, 2);
}

vector unsigned short
foo156 (vector unsigned short usa)
{
  return vec_splat (usa, 1);
}

/* Expected results:
   vec_all_eq          vcmpequd.
   vec_all_ge          vcmpgtud.
   vec_all_ne          vcmpequd.
   vec_any_eq          vcmpequd.
   vec_any_ne          vcmpequd.
   vec_all_gt          vcmpgtud.
   vec_all_le          vcmpgtud.
   vec_all_lt          vcmpgtud.
   vec_any_ge          vcmpgtud.
   vec_any_gt          vcmpgtud.
   vec_any_lt          vcmpgtud.
   vec_any_le          vcmpgtud.
   vec_and             xxland
   vec_andc            xxlandc
   vec_cntlz           vclzd, vclzb, vclzw, vclzh
   vec_cpsgn           xvcpsgnsp, xvcpsgndp
   vec_ctf             vcfux, vcfsx
   vec_cts             vctsxs
   vec_ctu             vctuxs
   vec_div             divd, divdu | __divdi3, __udivdi3
   vec_mergel          vmrglb, vmrglh, xxmrglw, xxpermdi
   vec_mergeh          xxmrghw, vmrghh, vmrghb, xxpermdi
   vec_mul             mulld | mullw, mulhwu
   vec_nor             xxlnor
   vec_packsu          vpkudus
   vec_perm            vperm
   vec_round           xvrdpi
   vec_sel             xxsel
   vec_xor             xxlxor 
   vec_rsqrt           xvrsqrtesp, xvrsqrtedp
   vec_rsqrte          xvrsqrtesp, xvrsqrtedp
   vec_xl              lxvd2x
   vec_xst             stxvd2x
   vec_splat           xxspltb | vspltb, xxspltw | vspltw, xxsplth | vsplth,
                       xxpermdi
   vec_mergee          xxpermdi, vmrgew
   vec_mergeo          xxpermdi, vmrgow  */

/* { dg-final { scan-assembler-times {\mvcmpequd\.} 4 } } */
/* { dg-final { scan-assembler-times {\mvcmpgtud\.} 8 } } */
/* { dg-final { scan-assembler-times {\mxxland\M} 17 } } */
/* { dg-final { scan-assembler-times {\mxxlandc\M} 13 } } */
/* { dg-final { scan-assembler-times {\mvclzb\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvclzd\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvclzw\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvclzh\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxvcpsgnsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvcpsgndp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvcfsx\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvcfux\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvctsxs\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvctuxs\M} 1 } } */

/* { dg-final { scan-assembler-times {\mvmrghb\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvmrghh\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxmrghw\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxmrglw\M} 4 } } */
/* { dg-final { scan-assembler-times {\mvmrglh\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxlnor\M} 6 } } */
/* { dg-final { scan-assembler-times {\mvpkudus\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxvrdpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvrfin\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxsel\M} 5 } } */
/* { dg-final { scan-assembler-times {\mxxlxor\M} 6 } } */
/* { dg-final { scan-assembler-times {\mdivd\M} 2  { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mdivdu\M} 2  { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mmulld\M} 4  { target lp64 } } } */
/* check for .__divdi3 (AIX), __divdi3 (Linux) and ___divdi3 (Darwin) */
/* { dg-final { scan-assembler-times {\mbl \.?_?__divdi3\M} 2   { target { ilp32 } } } } */
/* check for both .__udivdi3 (AIX), __udivdi3 (Linux) and ___udivdi3 (Darwin) */
/* { dg-final { scan-assembler-times {\mbl \.?_?__udivdi3\M} 2  { target { ilp32 } } } } */
/* { dg-final { scan-assembler-times {\mmullw\M} 12  { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mxvrsqrtesp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxvrsqrtedp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvcfsx\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvcfux\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvspltb|xxspltb\M} 3 } } */
/* { dg-final { scan-assembler-times {\mvspltw|xxspltw\M} 3 } } */
/* { dg-final { scan-assembler-times {\mvmrgow\M} 4 } } */
/* { dg-final { scan-assembler-times {\mvmrglb\M} 3 } } */
/* { dg-final { scan-assembler-times {\mvmrgew\M} 4 } } */
/* { dg-final { scan-assembler-times {\mvsplth|xxsplth\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 44 } } */
