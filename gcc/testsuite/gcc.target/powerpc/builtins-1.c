/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O0 -mno-fold-gimple -dp" } */
/* { dg-prune-output "gimple folding of rs6000 builtins has been disabled." } */

#include <altivec.h>

vector double y = { 2.0, 4.0 };
vector double z;

int main ()
{
  vector float fa = {1.0, 2.0, 3.0, -4.0};
  vector float fb = {-2.0, -3.0, -4.0, -5.0};
  vector float fd = vec_and (fa, fb);
  vector float fc = vec_cpsgn (fa, fb);
  vector float fe = vec_mergeh (fa, fb);
  vector float ff = vec_mergel (fa, fb);

  vector double da = {1.0, 2.0};
  vector double db = {-2.0, -3.0};
  vector double dz = vec_and (da, db);

  vector signed int si_a = {1, 2, 3, 4};
  vector unsigned int ui_a = {1, 2, 3, 4};

  vector long long la = {5L, 14L};
  vector long long lb = {3L, 86L};
  vector long long lc = vec_and (la, lb);
  vector bool long long ld = {0, -1};
  vector long long le = vec_and (la, ld);
  vector long long lf = vec_and (ld, lb);

  vector unsigned long long ua = {5L, 14L};
  vector unsigned long long ub = {3L, 86L};
  vector unsigned long long uc = vec_and (ua, ub);
  vector bool long long ud = {0, -1};
  vector unsigned long long ue = vec_and (ua, ud);
  vector unsigned long long uf = vec_and (ud, ub);

  vector long long lg = vec_andc (la, lb);
  vector long long lh = vec_andc (la, ld);
  vector long long li = vec_andc (ld, lb);

  vector unsigned long long ug = vec_andc (ua, ub);
  vector unsigned long long uh = vec_andc (ua, ud);
  vector unsigned long long ui = vec_andc (ud, ub);

  vector double de = {1.0, -4.0};
  vector double df = {-2.0, 5.0};
  vector double dg = vec_cpsgn (de, df);
  vector double dzw = vec_mergeh (de, df);
  vector double dze = vec_mergel (de, df);

  vector long long lj = vec_mergeh (la, lb);
  vector long long lk = vec_mergeh (la, ld);
  vector long long ll = vec_mergeh (ld, la);

  vector unsigned long long uj = vec_mergeh (ua, ub);
  vector unsigned long long uk = vec_mergeh (ua, ud);
  vector unsigned long long ul = vec_mergeh (ud, ua);

  vector pixel pa = {9, 16, 25, 36, 1, 2, 3, 4};
  vector pixel pb = {25, 36, 1, 2, 45, 3, 4, 99};
  vector pixel pc = vec_mergeh (pa, pb);
  vector pixel pd = vec_mergel (pa, pb);

  vector long long lm = vec_mergel (la, lb);
  vector long long ln = vec_mergel (la, ld);
  vector long long lo = vec_mergel (ld, la);

  vector unsigned long long um = vec_mergel (ua, ub);
  vector unsigned long long un = vec_mergel (ua, ud);
  vector unsigned long long uo = vec_mergel (ud, ua);

  vector long long lp = vec_nor (la, lb);
  vector long long lq = vec_nor (la, ld);
  vector long long lr = vec_nor (ld, la);

  vector unsigned long long up = vec_nor (ua, ub);
  vector unsigned long long uq = vec_nor (ua, ud);
  vector unsigned long long ur = vec_nor (ud, ua);

  vector unsigned char ca = {0,4,8,1,5,9,2,6,10,3,7,11,15,12,14,13};
  vector unsigned char cbb = {5,4,8,3,1,9,2,6,10,3,7,11,15,12,14,13};

  vector unsigned char ucba = {5,4,8,3,1,9,2,6,10,3,7,11,15,12,14,13};
  vector unsigned char ucbb = {5,4,8,3,1,9,2,6,10,3,7,11,15,12,14,13};
  vector unsigned char ucbc = {5,4,8,3,1,9,2,6,10,3,7,11,15,12,14,13};

  vector long long lv = vec_perm (la, lb, ca);

  vector unsigned char  ucm = vec_and (ca, cbb);
  vector unsigned char  ucn = vec_andc (ca, cbb);
  vector unsigned char  uco = vec_mergel (ca, cbb);

  vector unsigned long long uv = vec_perm (ua, ub, ca);

  vector long long lx = vec_sel (la, lb, uc);
  vector long long ly = vec_sel (la, lb, ld);

  vector unsigned long long uw = vec_sel (ua, ub, lc);
  vector unsigned long long ux = vec_sel (ua, ub, uc);
  vector unsigned long long uy = vec_sel (ua, ub, ld);

  vector long long lz = vec_xor (la, lb);
  vector long long l0 = vec_xor (la, ld);
  vector long long l1 = vec_xor (ld, la);

  vector unsigned long long uz = vec_xor (ua, ub);
  vector unsigned long long u0 = vec_xor (ua, ud);
  vector unsigned long long u1 = vec_xor (ud, ua);

  int ia = vec_all_eq (ua, ub);
  int ib = vec_all_ge (ua, ub);
  int ic = vec_all_gt (ua, ub);
  int id = vec_all_le (ua, ub);
  int ie = vec_all_lt (ua, ub);
  int ig = vec_all_ne (ua, ub);

  int ih = vec_any_eq (ua, ub);
  int ii = vec_any_ge (ua, ub);
  int ij = vec_any_gt (ua, ub);
  int ik = vec_any_le (ua, ub);
  int il = vec_any_lt (ua, ub);
  int im = vec_any_ne (ua, ub);

  vector short ssa = {9, 16, 25, 36, 1, 2, 3, 4};
  vector short ssb = {-8, -27, -64, -125, 2, 3, 5, 3};
  vector short sscc = vec_and (ssa, ssb);
  vector short sscd = vec_mergeh (ssa, ssb);
  vector short ssce = vec_mergel (ssa, ssb);

  vector int sia = {9, 16, 25, 36};
  vector int sib = {-8, -27, -64, -125};
  vector int sicc = vec_and (sia, sib);
  vector int sicd = vec_andc (sia, sib);
  vector int sig = vec_mergel (sia, sib);

  vector unsigned int uia = {9, 16, 25, 36};
  vector unsigned int uib = {8, 27, 64, 125};
  vector unsigned int uicc = vec_and (uia, uib);
  vector unsigned int uidd = vec_andc (uia, uib);
  vector unsigned int uig = vec_mergel (uia, uib);

  vector bool char bca = {0, 1, 4, 7};
  vector bool char bcb = {-8, 9, 2, 9};
  vector bool char bcc = vec_and (bca, bcb);
  vector bool char bcd = vec_andc (bca, bcb);
  vector bool char bce = vec_mergel (bca, bcb);

  vector bool short bsa = {0, -1, -1, 0, 3, 4, 6, 7};
  vector bool short bsb = {-1, -1, 0, -1, 0, 0, 0, 0};
  vector bool short bscc = vec_and (bsa, bsb);
  vector bool short bscd = vec_andc (bsa, bsb);
  vector bool short bsce = vec_mergel (bsa, bsb);

  vector bool int bia = {0, -1, -1, 0};
  vector bool int bib = {-1, -1, 0, -1};
  vector bool int bicc = vec_and (bia, bib);
  vector bool int bicd = vec_andc (bia, bib);
  vector bool int bide = vec_mergel (bia, bib);

  vector unsigned int uie = vec_packsu (ua, ub);

  vector long long l2 = vec_cntlz (la);
  vector unsigned long long u2 = vec_cntlz (ua);
  vector int sie = vec_cntlz (sia);
  vector unsigned int uif = vec_cntlz (uia);
  vector short sszz = vec_cntlz (ssa);

  vector unsigned short usa = {81, 72, 63, 54, 45, 36, 27, 18};
  vector unsigned short usb = {81, 72, 63, 54, 45, 36, 27, 18};
  vector unsigned short usd = vec_and (usa, usb);
  vector unsigned short use = vec_andc (usa, usb);
  vector unsigned short usc = vec_cntlz (usa);
  vector unsigned short uscd = vec_mergeh (usa, usb);
  vector unsigned short usce = vec_mergel (usa, usb);

  vector signed char sca = {-4, 3, -9, 15, -31, 31, 0, 0,
		            1, 117, -36, 99, 98, 97, 96, 95};
  vector signed char scb = vec_cntlz (sca);
  vector signed char scc = vec_mergel (sca, scb);

  vector unsigned char uca = {4, 3, 9, 15, 30, 31, 0, 0,
			      1, 117, 36, 99, 98, 97, 96, 95};
  vector unsigned char cb = vec_cntlz (ca);

  vector double dd = vec_xl (0, &y);
  vec_xst (dd, 0, &z);

  vector double dzz = vec_round (dd);
  vector double dzz1 = vec_rsqrt (dd);
  vector double dzz2 = vec_rsqrte (dd);

  vector float ff1 = vec_round (fa);
  vector float ff2 = vec_rsqrt (fa);
  vector float ff3 = vec_rsqrte (fa);

  vector double dff = vec_splat (de, 0);
  vector double dgg = vec_splat (de, 1);
  vector long long l3 = vec_splat (l2, 0);
  vector long long l4 = vec_splat (l2, 1);
  vector unsigned long long u3 = vec_splat (u2, 0);
  vector unsigned long long u4 = vec_splat (u2, 1);
  vector bool long long l5 = vec_splat (ld, 0);
  vector bool long long l6 = vec_splat (ld, 1);
  vector bool long long l10 = vec_mergee (ld, ld);
  vector bool long long l11 = vec_mergeo (ld, ld);
  vector bool long long l15 = vec_and (ld, ld);
  
  vector long long l7 = vec_div (l3, l4);
  vector unsigned long long u5 = vec_div (u3, u4);
  vector long long l12 = vec_mergee (la, lb);
  vector long long l13 = vec_mergeo (la, lb);
  vector unsigned long long u8 = vec_mergee (u3, u4);
  vector unsigned long long u9 = vec_mergeo (u3, u4);

  vector long long l8 = vec_mul (l3, l4);
  vector unsigned long long u6 = vec_mul (u3, u4);

  vector int sz = vec_cts (fa, 0x1F);
  vector unsigned int usz = vec_ctu (fa, 0x1F);

  vector float f1 = vec_mergee (fa, fb);
  vector float f2 = vec_mergeo (fa, fb);

  vector double d1 = vec_mergee (da, db);
  vector double d2 = vec_mergeo (da, db);

  vector float f3 = vec_ctf (si_a, 1);
  vector float f4 = vec_ctf (ui_a, 2);

  vector bool char z_vbc2 = vec_splat (bca, 0);
  vector signed char z_vsc1 = vec_splat (sca, 1);
  vector unsigned char z_vuc1 = vec_splat (ucbc, 2);

  vector bool int z_vbi1 = vec_splat (bia, 3);
  vector signed int z_vsi1 = vec_splat (sia, 1);
  vector unsigned int z_vui1 = vec_splat (uia, 2);

  vector bool int z_bi2 = vec_mergee (bia, bib);
  vector signed int z_si2 = vec_mergee (sia, sib);
  vector unsigned int z_ui2 = vec_mergee (uia, uib);
  
  vector bool char z_bc2 = vec_mergeh (bca, bcb);
  vector signed char z_sc2 = vec_mergeh (sca, scb);
  vector bool int z_bi3 = vec_mergeh (bia, bib);
  vector signed int z_si3 = vec_mergeh (sia, sib);
  vector unsigned int z_ui3 = vec_mergeh (uia, uib);
  vector bool short z_bs1 = vec_mergeh (bsa, bsb);

  vector bool int z_bi4 = vec_mergeo (bia, bib);
  vector signed int z_si4 = vec_mergeo (sia, sib);
  vector unsigned int z_ui4 = vec_mergeo (uia, uib);
  
  vector pixel int z_vp1 = vec_splat (pa, 1);
  vector bool short z_bs2 = vec_splat (bsa, 0);
  vector short signed int z_vss1 = vec_splat (ssa, 2);
  vector unsigned short int z_vuss1 = vec_splat (usa, 1);

  return 0;
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
   vec_cpsgn           xvcpsgnsp           
   vec_ctf             xvmuldp 
   vec_cts             vctsxs
   vec_ctu             vctuxs
   vec_div             divd, divdu | __divdi3(), __udivdi3()
   vec_mergel          vmrghb, vmrghh, xxmrghw
   vec_mergeh          xxmrglw, vmrglh, vmrglb
   vec_mul             mulld | mullw, mulhwu
   vec_nor             xxlnor
   vec_packsu          vpkudus
   vec_perm            vperm
   vec_round           xvrdpi
   vec_sel             xxsel
   vec_xor             xxlxor 
   vec_rsqrt           xvrsqrtesp
   vec_rsqrte          xvrsqrtesp
   vec_xl              lxvd2x
   vec_xst             stxvd2x
   vec_splat           xxspltb, xxspltw, vsplth
   vec_mergee          xxmrgld, vmrgow
   vec_mergeo          xxmrghd, vmrgew  */

/* { dg-final { scan-assembler-times "vcmpequd" 8 } } */
/* { dg-final { scan-assembler-times "vcmpgtud" 16 } } */
/* { dg-final { scan-assembler-times "xxland" 30 } } */
/* { dg-final { scan-assembler-times "xxlandc" 13 } } */
/* { dg-final { scan-assembler-times "vclzb" 2 } } */
/* { dg-final { scan-assembler-times "vclzd" 2 } } */
/* { dg-final { scan-assembler-times "vclzw" 2 } } */
/* { dg-final { scan-assembler-times "vclzh" 2 } } */
/* { dg-final { scan-assembler-times "xvcpsgnsp" 1 } } */
/* { dg-final { scan-assembler-times "xvcpsgndp" 1 } } */
/* { dg-final { scan-assembler-times "xvmuldp" 2 } } */
/* { dg-final { scan-assembler-times "xvcvdpsxds" 0 } } */
/* { dg-final { scan-assembler-times "vctsxs" 2 } } */
/* { dg-final { scan-assembler-times "xvcvdpuxds" 0 } } */
/* { dg-final { scan-assembler-times "vctuxs" 2 } } */

/* { dg-final { scan-assembler-times "vmrghb" 4 { target be } } } */
/* { dg-final { scan-assembler-times "vmrghb" 5 { target le } } } */
/* { dg-final { scan-assembler-times "vmrghh" 8 } } */
/* { dg-final { scan-assembler-times "xxmrghw" 8 } } */
/* { dg-final { scan-assembler-times "xxmrglw" 8 } } */
/* { dg-final { scan-assembler-times "vmrglh" 8 } } */
/* { dg-final { scan-assembler-times "xxlnor" 6 } } */
/* { dg-final { scan-assembler-times {\mvpkudus\M} 1 } } */
/* { dg-final { scan-assembler-times "vperm" 4 } } */
/* { dg-final { scan-assembler-times "xvrdpi" 2 } } */
/* { dg-final { scan-assembler-times "xxsel" 10 } } */
/* { dg-final { scan-assembler-times "xxlxor" 6 } } */
/* { dg-final { scan-assembler-times "divd" 8  { target lp64 } } } */
/* { dg-final { scan-assembler-times "divdu" 2  { target lp64 } } } */
/* { dg-final { scan-assembler-times "mulld" 4  { target lp64 } } } */
/* check for .__divdi3 (AIX), __divdi3 (Linux) and ___divdi3 (Darwin) */
/* { dg-final { scan-assembler-times {\mbl \.?_?__divdi3\M} 2   { target { ilp32 } } } } */
/* check for both .__udivdi3 (AIX), __udivdi3 (Linux) and ___udivdi3 (Darwin) */
/* { dg-final { scan-assembler-times {\mbl \.?_?__udivdi3\M} 2  { target { ilp32 } } } } */
/* { dg-final { scan-assembler-times "mullw" 12  { target ilp32 } } } */
/* { dg-final { scan-assembler-times "mulhwu" 4  { target ilp32 } } } */
/* { dg-final { scan-assembler-times "xxmrgld" 0 } } */
/* { dg-final { scan-assembler-times "xxmrghd" 0 } } */
/* { dg-final { scan-assembler-times "xvrsqrtesp" 2 } } */
/* { dg-final { scan-assembler-times "xvrsqrtedp" 2 } } */
/* { dg-final { scan-assembler-times "xxspltd" 8 } } */
/* { dg-final { scan-assembler-times "vcfsx" 2 } } */
/* { dg-final { scan-assembler-times "vcfux" 2 } } */
/* { dg-final { scan-assembler-times "vspltb" 6 } } */
/* { dg-final { scan-assembler-times "vspltw" 0 } } */
/* { dg-final { scan-assembler-times "vmrgow" 8 } } */
/* { dg-final { scan-assembler-times "vmrglb" 5 { target le } } } */
/* { dg-final { scan-assembler-times "vmrglb" 6 { target be } } } */
/* { dg-final { scan-assembler-times "vmrgew" 8 } } */
/* { dg-final { scan-assembler-times "vsplth" 8 } } */
/* { dg-final { scan-assembler-times "vcmpequd." 8 } } */
/* { dg-final { scan-assembler-times "vcmpgtud." 16 } } */
/* { dg-final { scan-assembler-times "vrfin" 2 } } */

