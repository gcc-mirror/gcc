/* This test is included into builtins-1-be.c and builtins-1-le.c to test on
   Big Endian and Little Endian machines.  */
/* This file is based on builtins-1.h.  In this variant, most variables have
   been marked as extern to prevent optimization-away. */

#include <altivec.h>

vector double y = { 2.0, 4.0 };
extern vector double z;

int main ()
{
  vector float fa = {1.0, 2.0, 3.0, -4.0};
  vector float fb = {-2.0, -3.0, -4.0, -5.0};
  extern vector float fd; fd = vec_and (fa, fb);
  extern vector float fc; fc = vec_cpsgn (fa, fb);
  extern vector float fe; fe = vec_mergeh (fa, fb);
  extern vector float ff; ff = vec_mergel (fa, fb);

  vector double da = {1.0, 2.0};
  vector double db = {-2.0, -3.0};
  extern vector double dz; dz = vec_and (da, db);

  vector long long la = {5L, 14L};
  vector long long lb = {3L, 86L};
  extern vector long long lc; lc = vec_and (la, lb);
  vector bool long long ld = {0, -1};
  extern vector long long le; le = vec_and (la, ld);
  extern vector long long lf; lf = vec_and (ld, lb);

  vector unsigned long long ua = {5L, 14L};
  vector unsigned long long ub = {3L, 86L};
  extern vector unsigned long long uc; uc = vec_and (ua, ub);
  vector bool long long ud = {0, -1};
  extern vector unsigned long long ue; ue = vec_and (ua, ud);
  extern vector unsigned long long uf; uf = vec_and (ud, ub);

  extern vector long long lg; lg = vec_andc (la, lb);
  extern vector long long lh; lh = vec_andc (la, ld);
  extern vector long long li; li = vec_andc (ld, lb);

  extern vector unsigned long long ug; ug = vec_andc (ua, ub);
  extern vector unsigned long long uh; uh = vec_andc (ua, ud);
  extern vector unsigned long long ui; ui = vec_andc (ud, ub);

  vector double de = {1.0, -4.0};
  vector double df = {-2.0, 5.0};
  extern vector double dg; dg = vec_cpsgn (de, df);
  extern vector double dzw; dzw = vec_mergeh (de, df);
  extern vector double dze; dze = vec_mergel (de, df);

  extern vector long long lj; lj = vec_mergeh (la, lb);
  extern vector long long lk; lk = vec_mergeh (la, ld);
  extern vector long long ll; ll = vec_mergeh (ld, la);

  extern vector unsigned long long uj; uj = vec_mergeh (ua, ub);
  extern vector unsigned long long uk; uk = vec_mergeh (ua, ud);
  extern vector unsigned long long ul; ul = vec_mergeh (ud, ua);

  vector pixel pa = {9, 16, 25, 36, 1, 2, 3, 4};
  vector pixel pb = {25, 36, 1, 2, 45, 3, 4, 99};
  extern vector pixel pc; pc = vec_mergeh (pa, pb);
  extern vector pixel pd; pd = vec_mergel (pa, pb);

  extern vector long long lm; lm = vec_mergel (la, lb);
  extern vector long long ln; ln = vec_mergel (la, ld);
  extern vector long long lo; lo = vec_mergel (ld, la);

  extern vector unsigned long long um; um = vec_mergel (ua, ub);
  extern vector unsigned long long un; un = vec_mergel (ua, ud);
  extern vector unsigned long long uo; uo = vec_mergel (ud, ua);

  extern vector long long lp; lp = vec_nor (la, lb);
  extern vector long long lq; lq = vec_nor (la, ld);
  extern vector long long lr; lr = vec_nor (ld, la);

  extern vector unsigned long long up; up = vec_nor (ua, ub);
  extern vector unsigned long long uq; uq = vec_nor (ua, ud);
  extern vector unsigned long long ur; ur = vec_nor (ud, ua);

  extern vector long long ls; ls = vec_or (la, lb);
  extern vector long long lt; lt = vec_or (la, ld);
  extern vector long long lu; lu = vec_or (ld, la);

  extern vector unsigned long long us; us = vec_or (ua, ub);
  extern vector unsigned long long ut; ut = vec_or (ua, ud);
  extern vector unsigned long long uu; uu = vec_or (ud, ua);

  vector unsigned char ca = {0,4,8,1,5,9,2,6,10,3,7,11,15,12,14,13};
  vector unsigned char cbb = {5,4,8,3,1,9,2,6,10,3,7,11,15,12,14,13};

  extern vector long long lv; lv = vec_perm (la, lb, ca);

  extern vector unsigned char  ucm; ucm = vec_and (ca, cbb);
  extern vector unsigned char  ucn; ucn = vec_andc (ca, cbb);
  extern vector unsigned char  uco; uco = vec_mergel (ca, cbb);

  extern vector unsigned long long uv; uv = vec_perm (ua, ub, ca);

  extern vector long long lw; lw = vec_sel (la, lb, lc);
  extern vector long long lx; lx = vec_sel (la, lb, uc);
  extern vector long long ly; ly = vec_sel (la, lb, ld);

  extern vector unsigned long long uw; uw = vec_sel (ua, ub, lc);
  extern vector unsigned long long ux; ux = vec_sel (ua, ub, uc);
  extern vector unsigned long long uy; uy = vec_sel (ua, ub, ld);

  extern vector long long lz; lz = vec_xor (la, lb);
  extern vector long long l0; l0 = vec_xor (la, ld);
  extern vector long long l1; l1 = vec_xor (ld, la);

  extern vector unsigned long long uz; uz = vec_xor (ua, ub);
  extern vector unsigned long long u0; u0 = vec_xor (ua, ud);
  extern vector unsigned long long u1; u1 = vec_xor (ud, ua);

  extern int ia; ia = vec_all_eq (ua, ub);
  extern int ib; ib = vec_all_ge (ua, ub);
  extern int ic; ic = vec_all_gt (ua, ub);
  extern int id; id = vec_all_le (ua, ub);
  extern int ie; ie = vec_all_lt (ua, ub);
  extern int ig; ig = vec_all_ne (ua, ub);

  extern int ih; ih = vec_any_eq (ua, ub);
  extern int ii; ii = vec_any_ge (ua, ub);
  extern int ij; ij = vec_any_gt (ua, ub);
  extern int ik; ik = vec_any_le (ua, ub);
  extern int il; il = vec_any_lt (ua, ub);
  extern int im; im = vec_any_ne (ua, ub);

  vector short ssa = {9, 16, 25, 36, 1, 2, 3, 4};
  vector short ssb = {-8, -27, -64, -125, 2, 3, 5, 3};
  extern vector short sscc; sscc = vec_and (ssa, ssb);
  extern vector short sscd; sscd = vec_mergeh (ssa, ssb);
  extern vector short ssce; ssce = vec_mergel (ssa, ssb);

  vector int sia = {9, 16, 25, 36};
  vector int sib = {-8, -27, -64, -125};
  extern vector int sicc; sicc = vec_and (sia, sib);
  extern vector int sicd; sicd = vec_andc (sia, sib);
  extern vector int sig; sig = vec_mergel (sia, sib);

  vector unsigned int uia = {9, 16, 25, 36};
  vector unsigned int uib = {8, 27, 64, 125};
  extern vector unsigned int uicc; uicc = vec_and (uia, uib);
  extern vector unsigned int uidd; uidd = vec_andc (uia, uib);
  extern vector unsigned int uig; uig = vec_mergel (uia, uib);

  vector bool char bca = {0, 1, 4, 7};
  vector bool char bcb = {-8, 9, 2, 9};
  extern vector bool char bcc; bcc= vec_and (bca, bcb);
  extern vector bool char bcd; bcd = vec_andc (bca, bcb);
  extern vector bool char bce; bce = vec_mergel (bca, bcb);

  vector bool short bsa = {0, -1, -1, 0, 3, 4, 6, 7};
  vector bool short bsb = {-1, -1, 0, -1, 0, 0, 0, 0};
  extern vector bool short bscc; bscc = vec_and (bsa, bsb);
  extern vector bool short bscd; bscd = vec_andc (bsa, bsb);
  extern vector bool short bsce; bsce = vec_mergel (bsa, bsb);

  vector bool int bia = {0, -1, -1, 0};
  vector bool int bib = {-1, -1, 0, -1};
  extern vector bool int bicc; bicc = vec_and (bia, bib);
  extern vector bool int bicd; bicd = vec_andc (bia, bib);
  extern vector bool int bide; bide = vec_mergel (bia, bib);

  extern vector unsigned int uie; uie = vec_packsu (ua, ub);

  extern vector long long l2; l2 = vec_cntlz (la);
  extern vector unsigned long long u2; u2 = vec_cntlz (ua);
  extern vector int sie; sie = vec_cntlz (sia);
  extern vector unsigned int uif; uif = vec_cntlz (uia);
  extern vector short sszz; sszz = vec_cntlz (ssa);

  vector unsigned short usa = {81, 72, 63, 54, 45, 36, 27, 18};
  vector unsigned short usb = {81, 72, 63, 54, 45, 36, 27, 18};
  extern vector unsigned short usd; usd = vec_and (usa, usb);
  extern vector unsigned short use; use = vec_andc (usa, usb);
  extern vector unsigned short usc; usc = vec_cntlz (usa);
  extern vector unsigned short uscd; uscd = vec_mergeh (usa, usb);
  extern vector unsigned short usce; usce = vec_mergel (usa, usb);

  vector signed char sca = {-4, 3, -9, 15, -31, 31, 0, 0,
		            1, 117, -36, 99, 98, 97, 96, 95};
  extern vector signed char scb; scb = vec_cntlz (sca);
  extern vector signed char scc; scc = vec_mergel (sca, scb);

  extern vector unsigned char cb; cb = vec_cntlz (ca);

  extern vector double dd; dd = vec_xl (0, &y);
  vec_xst (dd, 0, &z);

  extern vector double dzz; dzz = vec_round (dd);
  extern vector double dzz1; dzz1 = vec_rsqrt (dd);
  extern vector double dzz2; dzz2 = vec_rsqrte (dd);

  extern vector double dff; dff = vec_splat (de, 0);
  extern vector double dgg; dgg = vec_splat (de, 1);
  extern vector long long l3; l3 = vec_splat (l2, 0);
  extern vector long long l4; l4 = vec_splat (l2, 1);
  extern vector unsigned long long u3; u3 = vec_splat (u2, 0);
  extern vector unsigned long long u4; u4 = vec_splat (u2, 1);
  extern vector bool long long l5; l5 = vec_splat (ld, 0);
  extern vector bool long long l6; l6 = vec_splat (ld, 1);
  extern vector bool long long l10; l10 = vec_mergee (ld, ld);
  extern vector bool long long l11; l11 = vec_mergeo (ld, ld);

  extern vector long long l7; l7 = vec_div (l3, l4);
  extern vector unsigned long long u5; u5 = vec_div (u3, u4);
  extern vector long long l12; l12 = vec_mergee (la, lb);
  extern vector long long l13; l13 = vec_mergeo (la, lb);
  extern vector unsigned long long u8; u8 = vec_mergee (u3, u4);
  extern vector unsigned long long u9; u9 = vec_mergeo (u3, u4);

  extern vector long long l8; l8 = vec_mul (l3, l4);
  extern vector unsigned long long u6; u6 = vec_mul (u3, u4);

  extern vector double dh; dh = vec_ctf (la, -2);
  extern vector double di; di = vec_ctf (ua, 2);
  extern vector int sz; sz = vec_cts (fa, 0x1F);
  extern vector long long l9; l9 = vec_cts (dh, -2);
  extern vector unsigned long long u7; u7 = vec_ctu (di, 2);
  extern vector unsigned int usz; usz = vec_ctu (fa, 0x1F);

  extern vector float f1; f1 = vec_mergee (fa, fb);
  extern vector float f2; f2 = vec_mergeo (fa, fb);

  extern vector double d1; d1 = vec_mergee (da, db);
  extern vector double d2; d2 = vec_mergeo (da, db);

  return 0;
}

