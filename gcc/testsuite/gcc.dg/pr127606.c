/* { dg-do run { target lp64 } } */
/* { dg-options "-O3 -fno-guess-branch-probability -fno-tree-loop-im -fno-tree-pre -fno-forward-propagate -fno-ipa-ra -fno-tree-ch" } */

struct a {
  short d[2];
};
short m, d, p, aa, ab = -101, ac = -10493, ad = -65, ae, af = -15067, ag, t, ah,
                   z, ai, *aj, **ak;
int n, q, r = -66, al, am = -781047284, an, ao = -39, ap, aq, ar, as, at, au,
          av, aw, u, ax, w, ay, az, ba, bb, bc, bd = -284627199, be, bf, bg, bh,
          bi, bj, bk, bl, *bm, **bn;
long e, f, bo, bp = -342246050408, bq, br, bs, bt, bu, bv, bw, bx, by, bz,
               ca = 6, cb, cc, cd, *ce, *cf, *cg, *ch, *ci, *cj;
float l, o = -3.75f, s;
char ck, v, cl;
__attribute__((noinline)) int cm(int cn, int co) {
  static int g[256];
  for (int h = 0; h < 256; ++h) {
    unsigned c = h;
    for (int i = 0; i < 8; ++i)
      c = c & 1 ? c >> 1 ^ 3988292384 : c >> 1;
    g[h] = c;
  }
  unsigned j = cn;
  int k = co;
  for (int b = 0; b < 4; ++b) {
    char a = k;
    k >>= 8;
    j = j >> 8 ^ g[(j ^ a) & 255];
  }
  return j;
}
void cp(long, int, long);
int cq(short p1, short p2, int cr, int cs, short ct) {
  m = n = cm(0, (unsigned long)cr << 40 >> 40);
  n = cm(n, 0);
  n = cm(n, cs);
  n = cm(n, (unsigned long)ct >> 52);
  n = cm(n, 0);
  n = cm(n, 93);
  n = cm(n, 0);
  return n;
}
void cu(char);
int cv(int cn) {
  cm(0, 0);
  cm(0, cn);
  return 0;
}
int cw(int, char, float, char, char, char, short);
int cx(short cn, char co, float cr) {
  short cy = -3410;
  ak = &aj;
  ce = &e;
  d = cn - -32563;
  q = 1083998199 - 1083474983 + 16;
  f = -1074528307 - ((long)((unsigned long)cy << 48) >> 48);
  *ce = 1073609055;
  l = -84541440;
  s = 8.0f * cr;
  *ak = &p;
  cy = -16400;
  *aj = 267 - cn;
  al = cm(0, d);
  al = cm(al, (long)((unsigned long)cy << 48) >> 48);
  al = cm(al, e);
  al = cm(al, 0);
  al = cm(al, f);
  al = cm(al, 1.5f);
  al = cm(al, 0);
  al = cm(al, o);
  al = cm(al, 3.0f);
  al = cm(al, 1.5f);
  al = cm(al, 0);
  al = cm(al, 1.0f);
  al = cm(al, l);
  al = cm(al, 84933729);
  al = cm(al, p);
  al = cm(al, r);
  al = cm(al, q);
  al = cm(al, s);
  al = cm(al, 0);
  al = cm(al, (long)((unsigned long)cn << 48) >> 48);
  al = cm(al, co);
  al = cm(al, 0);
  al = cm(al, 0);
  al = cm(al, 0);
  al = cm(al, 0);
  al = cm(al, 0);
  al = cm(al, 0);
  al = cm(al, 0);
  aa = *aj;
  al = cm(al, aa);
  bo = *ce;
  al = cm(al, bo);
  return al;
}
int cz(int cn, short co, long cr, int cs, long ct, short da, int db) {
  long dc = 17;
  cf = cg = ch = &dc;
  *cg = dc + dc;
  ae = co - 9 * co;
  an = -368690105 - -2048327106;
  ap = cm(0, (long)((unsigned long)ad << 52) >> 52);
  ap = cm(ap, (long)((unsigned long)ab << 52) >> 52);
  ap = cm(ap, 11);
  ap = cm(ap, 25022);
  ap = cm(ap, (long)((unsigned long)ac << 48) >> 48);
  ap = cm(ap, 261555065);
  ap = cm(ap, 29095);
  ap = cm(ap, ct);
  ap = cm(ap, dc);
  ap = cm(ap, (long)((unsigned long)ae << 48) >> 48);
  ap = cm(ap, (long)((unsigned long)af << 48) >> 48);
  ap = cm(ap, 0);
  ap = cm(ap, am);
  ap = cm(ap, 14);
  ap = cm(ap, -712613994);
  ap = cm(ap, -591966605);
  ap = cm(ap, bp);
  ap = cm(ap, an);
  ap = cm(ap, ao);
  ap = cm(ap, 4.0);
  ap = cm(ap, cn);
  ap = cm(ap, co);
  ap = cm(ap, cr);
  ap = cm(ap, (long)((unsigned long)cs << 44) >> 44);
  ap = cm(ap, ct);
  ap = cm(ap, (long)((unsigned long)da << 52) >> 52);
  ap = cm(ap, 0);
  ap = cm(ap, db);
  bq = *cf;
  ap = cm(ap, bq);
  br = *cg;
  ap = cm(ap, br);
  bs = *ch;
  ap = cm(ap, bs);
  return ap;
}
void dd(long cn) {
  bv = 1837996544 + cn;
  bw = bv % 3037000493;
  by = 1487194 * cn;
  bx = by % 3037000493;
  by = 1944416850 * bw % 3037000493;
  bx = bx + by;
  cp(bx, 3, -1);
}
void cp(long cn, int co, long cr) {
  int x, de, df, dg = 0, dh, di = 11, dj, *dk;
  char dl = 0, dm, y, dn, dp;
  long dq = -82;
  struct a dr = {{0,0}};
  short ds = 0, dt = 0, du;
  dk = &u;
  do {
    dh = cn;
    aw = dh % 46337;
    w = 40033 * aw;
    au = w + 31847;
    au = au % 46337;
    ck = (long)((unsigned long)dq << 16) >> 16;
    av = ck % di + 11;
    av = av % di;
    v = 2 * av % di;
    dm = cn;
    y = dm % di + 11;
    y = y % di;
    dp = 10 * y % di;
    dn = dp + 4;
    t = 45 * cr;
    dj = cw(au - 20753 + 2050618351, -111, -1044328.0f, v - 6 - 104, dn - 5,
            -19, t) -
         95733627;
    ag = (unsigned long)dq << 16 >> 6;
    ds = ds + dt;
    ds = ds % 81;
    dt = 98 * ag;
    du = dr.d[0];
    de = (long)dl << 40 >> 40;
    aq = 30 ^ de;
    df = 127 ^ de;
    aq = aq & df;
    ar = cq(ds, du, -4079617 ^ aq, 54, -1) - 2032857596;
    *dk = dj - ar;
    x = (21 >> co) + ~u;
    dr.d[0] = 6;
    if (0 <= dl - 31) {
      dq = cr;
      ax = -10 * co;
      dg = dg + 9;
      at = cv(dg - 7) - 309410;
      u = co + at;
    }
    dl = 127;
  } while (0 <= u);
  as = 10 + x;
  cu(as - 10 - 1);
}
void cu(char cn) {
  bb = cw(2050618351, -111, -1044328.0f, cn - 103, -72, -19, 136) + 979061894;
  ba = 130280446 + bb;
  if (-2 * ba != -262668294)
    az = 1067210928 / ay;
}
int cw(int cn, char co, float cr, char cs, char ct, char da, short db) {
  long dv = -197;
  ci = &ca;
  bn = &bm;
  cj = &cb;
  *cj = -66322669 - dv;
  ah = 2048 - -79 - 2048;
  do {
    *ci = ~dv + (ca <= 0 ? 1008729983 : -1039925336);
    z = ah - 21 * ah - 3 * ah;
    dv = cb + *ci;
    bh = 128 + 126 - 95 / da;
    bk = cx(-16164, 121, -0.5f) - 826019009;
    bi = bk + cn;
    bm = &bf;
  } while (8 < -ca);
  *bm = (long)((unsigned long)da << 56) >> 56;
  *cj = ca - 2116026151;
  cl = da - 75;
  *bn = &bi;
  bg = da - 57 + ~cs;
  be = 1991637 * bi - -2146475557 - 2129375996;
  bc = -7342433 - ((long)((unsigned long)2048 << 52) >> 52);
  ai = -257 + 32765 + db;
  bz = 376721798 - 2146435071 / ca;
  dv = -6029336;
  ca = 97517562 - -10 * cb;
  bl = cz(-1309835519, 2167, 663091339, -34828, 70368711542989, z - 206,
          -1040119841) -
       498025;
  bj = cm(0, (long)((unsigned long)bc << 40) >> 40);
  bj = cm(bj, (long)((unsigned long)z << 52) >> 52);
  bj = cm(bj, bz);
  bj = cm(bj, be);
  bj = cm(bj, ai);
  bj = cm(bj, bd);
  bj = cm(bj, bf);
  bj = cm(bj, ca);
  bj = cm(bj, (long)((unsigned long)cl << 56) >> 56);
  bj = cm(bj, 3);
  bj = cm(bj, (unsigned long)bh << 56 >> 56);
  bj = cm(bj, bg);
  bj = cm(bj, 67);
  bj = cm(bj, dv);
  bj = cm(bj, cb);
  bj = cm(bj, bi);
  bj = cm(bj, bl);
  bj = cm(bj, (long)((unsigned long)co << 56) >> 56);
  bj = cm(bj, cr);
  bj = cm(bj, 0);
  bj = cm(bj, (long)((unsigned long)cs << 56) >> 56);
  bj = cm(bj, (long)((unsigned long)ct << 56) >> 56);
  bj = cm(bj, (long)((unsigned long)da << 56) >> 56);
  bj = cm(bj, (unsigned long)db << 48 >> 48);
  cc = *ci;
  bj = cm(bj, cc);
  cd = *cj;
  bj = cm(bj, cd);
  bj = cm(bj, *bm);
  return bj;
}
int main() {
  bt = 1316079754;
  bu = 2397044987 * 111;
  bt = bt + bu;
  dd(bt);
}
