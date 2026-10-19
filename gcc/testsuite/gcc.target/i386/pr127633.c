/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O3 -funroll-all-loops -fno-vect-cost-model" } */

int d, ab = -104, ac, ad = -170, ae = -51, af = -151, ag = -1966532803, ah, ai,
       aj, ak, al, am, an, *ao, v, ap, aq;
short w, y, z, ar, as, at, au, av, aw, ax = -156, ay, az, ba;
long bb = -38, bc, bd, be, bf;
double bg, bh, *bi;
char bj;
int a(unsigned bk, unsigned bl) {
  return bk^bl;
}
[[gnu::noinline,gnu::noclone]]
int aa(short bn) {
  return bn;
}
[[gnu::noinline,gnu::noclone]]
int s(long bk, short bl, short bm, long bn, int bo, short bp, int bq) {
  int bt, bu = aa(-1);
  double bv, bw, *by, *bz, **ca;
  __attribute__((__vector_size__(8 * sizeof(double)))) double cb;
  cb[4] = 1.75;
  cb[5] = -8.0;
  cb[7] = 1.75;
  ca = &by;
  bt = bu;
  bv = -65536;
  bz = &bw;
  cb[2] = 0 + (bm ? -8.0 : bg);
  if (-bl >= 0) {
    if (bm)
      goto bx;
    ca = &bz;
  }
  cb[3] = 524420 + bt;
  az = bl - (1646 ^ bl);
  bw = 2.0 * bv;
  bv = 2.0 * bw;
  *ca = &bv;
bx:
  cb[1] = -4.0 + 2.5 * bv;
  bc = 267503909 - bn;
  bi = *ca;
  *by = 131072 - 5.0 * bw;
  *bz = 3.0 * bg + 0.375 * bv;
  ay = ba = bl + 15 * az;
  bj = -__builtin_popcount((unsigned char)(109 + az));
  an = 0;
  
  an = a(an, cb[1]);
  an = a(an, cb[2]);
  an = a(an, cb[3]);
  an = a(an, cb[4]);
  an = a(an, cb[5]);
  an = a(an, cb[7]);

  an = a(an, bw);
  an = an^(int)bg;
  return an;
}
int main() {
  am = s(1, 1, 1, 1, 3, 1, 1);
  al = am;
  if (al != 0xfff97f7f)
    __builtin_abort ();
  return 0;
}
