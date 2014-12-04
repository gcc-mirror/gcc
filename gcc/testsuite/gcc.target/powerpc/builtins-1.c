/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O0" } */

/* Test that a number of newly added builtin overloads are accepted
   by the compiler.  */

#include <altivec.h>

vector double y = { 2.0, 4.0 };
vector double z;

int main ()
{
  vector float fa = {1.0, 2.0, 3.0, -4.0};
  vector float fb = {-2.0, -3.0, -4.0, -5.0};
  vector float fc = vec_cpsgn (fa, fb);

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

  vector double da = {1.0, -4.0};
  vector double db = {-2.0, 5.0};
  vector double dc = vec_cpsgn (da, db);

  vector long long lj = vec_mergeh (la, lb);
  vector long long lk = vec_mergeh (la, ld);
  vector long long ll = vec_mergeh (ld, la);

  vector unsigned long long uj = vec_mergeh (ua, ub);
  vector unsigned long long uk = vec_mergeh (ua, ud);
  vector unsigned long long ul = vec_mergeh (ud, ua);

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

  vector long long ls = vec_or (la, lb);
  vector long long lt = vec_or (la, ld);
  vector long long lu = vec_or (ld, la);

  vector unsigned long long us = vec_or (ua, ub);
  vector unsigned long long ut = vec_or (ua, ud);
  vector unsigned long long uu = vec_or (ud, ua);

  vector unsigned char ca = {0,4,8,1,5,9,2,6,10,3,7,11,15,12,14,13};
  vector long long lv = vec_perm (la, lb, ca);
  vector unsigned long long uv = vec_perm (ua, ub, ca);

  vector long long lw = vec_sel (la, lb, lc);
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

  vector int sia = {9, 16, 25, 36};
  vector int sib = {-8, -27, -64, -125};
  vector int sic = vec_mergee (sia, sib);
  vector int sid = vec_mergeo (sia, sib);

  vector unsigned int uia = {9, 16, 25, 36};
  vector unsigned int uib = {8, 27, 64, 125};
  vector unsigned int uic = vec_mergee (uia, uib);
  vector unsigned int uid = vec_mergeo (uia, uib);

  vector bool int bia = {0, -1, -1, 0};
  vector bool int bib = {-1, -1, 0, -1};
  vector bool int bic = vec_mergee (bia, bib);
  vector bool int bid = vec_mergeo (bia, bib);

  vector unsigned int uie = vec_packsu (ua, ub);

  vector long long l2 = vec_cntlz (la);
  vector unsigned long long u2 = vec_cntlz (ua);
  vector int sie = vec_cntlz (sia);
  vector unsigned int uif = vec_cntlz (uia);
  vector short ssa = {20, -40, -60, 80, 100, -120, -140, 160};
  vector short ssb = vec_cntlz (ssa);
  vector unsigned short usa = {81, 72, 63, 54, 45, 36, 27, 18};
  vector unsigned short usb = vec_cntlz (usa);
  vector signed char sca = {-4, 3, -9, 15, -31, 31, 0, 0,
		            1, 117, -36, 99, 98, 97, 96, 95};
  vector signed char scb = vec_cntlz (sca);
  vector unsigned char cb = vec_cntlz (ca);

  vector double dd = vec_xl (0, &y);
  vec_xst (dd, 0, &z);

  vector double de = vec_round (dd);

  vector double df = vec_splat (de, 0);
  vector double dg = vec_splat (de, 1);
  vector long long l3 = vec_splat (l2, 0);
  vector long long l4 = vec_splat (l2, 1);
  vector unsigned long long u3 = vec_splat (u2, 0);
  vector unsigned long long u4 = vec_splat (u2, 1);
  vector bool long long l5 = vec_splat (ld, 0);
  vector bool long long l6 = vec_splat (ld, 1);

  vector long long l7 = vec_div (l3, l4);
  vector unsigned long long u5 = vec_div (u3, u4);

  vector long long l8 = vec_mul (l3, l4);
  vector unsigned long long u6 = vec_mul (u3, u4);

  vector double dh = vec_ctf (la, -2);
  vector double di = vec_ctf (ua, 2);
  vector long long l9 = vec_cts (dh, -2);
  vector unsigned long long u7 = vec_ctu (di, 2);

  return 0;
}
