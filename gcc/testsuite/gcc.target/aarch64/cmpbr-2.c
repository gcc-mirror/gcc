/* { dg-do compile } */
/* { dg-options "-O2" } */
/* PR target/121388 */

#pragma GCC target "+cmpbr"

extern int a, b, s;
typedef struct {
  unsigned long d[2];
} g;
typedef struct {
  long d[4];
} h;
typedef struct {
  unsigned long d[];
} j;
typedef struct {
  long d[];
} k;
typedef union {
  struct {
    short l;
    unsigned m;
  } i;
  long double f;
} n;
g o[] = {{}};
const g aa[1];
h ab, t;
h ac[1];
long p, r, ae, af, ag, ah, aj, ak, al, an, ao, aq, ar, aw, ax, ba, bb, bc, bd;
unsigned long q, am, ay, w;
g ad;
unsigned ai, ap, at, au, av, az;
short as, v, be;
long double u;
double f() {
  long bf;
  g c;
  unsigned long bg;
  int e, bh;
  j x;
  if (q << 61 == 3ull << 61) {
    if (q & 58) {
      return u;
    }
    as = 8;
    return a;
  }
  e = c.d[1] = q & ((1ull << 49) - 1);
  bg = p;
  if (101086242752 < c.d[1] ||
      (101086242752 == c.d[1] && 4003012203950112767 < p))
    c.d[1] = p;
  if (c.d[1] && p == 0) {
    n bi;
    bi.i.l = be;
    return bi.f;
  }
  s = c.d[1] == 0 ? p ?: p : __builtin_clzll(c.d[1]);
  s == 0 ? c.d[1] : s >= 64 ? c.d[1] : (c.d[1] = s, bg = 0);
  if (e >= 3) {
    if (a) {
      n bi;
      bi.i.m = 0;
      return bi.f;
    }
    return ar;
  }
  if (e <= -4985)
    e = 4985;
  ad = (aa + 5)[e];
  bh = s;
  if (r && (bg = 0))
    t = ab;
  t = ac[e];
  k bj, bk;
  ao = bg;
  az = bg;
  al = az;
  ay = aw = bg >> 2;
  b = ay + (aq > 2) < bg * ap;
  ay = az;
  bd = av = w;
  ah = bb;
  bj.d[4] = am;
  an = c.d[1] >> an;
  ax = bg * az;
  aj = t.d[1];
  az = w = ax;
  bb = az;
  ag = aj;
  long bl = af = w + az < w;
  au = aw;
  am = au < w || w < ak + am;
  bk.d[3] = bl + az;
  ai = a < aj;
  at = aj + b < ai;
  x.d[3] = ba;
  bc = bk.d[3] + bc + bj.d[4];
  bf = ae;
  if (x.d[3] || o[bf & 1].d[0]) {
    if (bf == 0)
      ;
    else if (bf == 3 && bh)
      if ((a & 3 && x.d[3] < 3ull << 62) || (q && x.d[3]))
        b = 6;
  }
  return a;
}
