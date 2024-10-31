/* { dg-do run } */
/* { dg-options "-O3" } */
int a, f = 1, h, l, m = 1, o, r = 4, q, s, x, e, aa, ab, ac, *ad, ae = 5, **y, **af, ag, ah, ai, aj;
static int c[6], d, g[6][5], n, *v = &s, ak;
volatile int p;
const volatile int al;
static volatile int t, u, w = 3, z, am, an;
static int ao();
void ap();
static void aq() {
  int ar[4] = {6, 6, 6, 6}, as[1], i, j;
  as[0] = 0;
  if (m) {
    int at[11] = {4, 4, 6, 5, 7, 0, 7, 6, 7, 6, 6}, *au, *av[7], k;
    au = (int*) &au;
    for (i = 0; i < 1; i++)
      for (j = 0; j < 1; j++)
        for (k = 0; k < 7; k++) {
          (t || n) && u;
          av[k] = 0;
        }
    y = av;
    while (o) {
      int *b[2] = {as, ar};
      *af = at;
    }
    m = 0;
  }
}
inline void ap() {
  for (; l <= 4; l++) {
    *v = 0;
    aq();
    if (a)
      break;
    for (; q; q++)
      ;
  }
}
int ao() {
  int be = 0, j;
  if (n)
    aa = d = 0;
  l = 0;
  for (; be < 2; be++) {
    int bf[7][2];
    for (ai = 0; ai < 7; ai++)
      for (j = 0; j < 2; j++)
        bf[ai][j] = 5;
    if (be) {
      for (; h >= 0; h--) {
        while (z >= w) {
          ap();
          *ad = 0;
        }
        ap();
      }
      return bf[3][0];
    }
    if (bf[3][0])
      continue;
    while (1)
      ;
  }
  return 0;
}
static void aw() {
  for (; ah; ah++) {
    p = 0;
    p = 0;
  }
  int ax = ~e;
 L1:
  e = a = 0;
 L2:
  if (!r)
    goto L3;
  if (!ax)
    goto L2;
  if (d)
    goto L1;
  if (!ae)
    goto L1;
  if (w && x <= 808 && f)
    ag = ao();
  g[0][4] = ag;
  if (a) {
    int bd;
    n++;
    while (n)
      for (bd = 0; bd < 7; bd++) {
        am;
        am;
        am;
        am;
        d = c[d ^ am];
      }
  } else {
  L3:
    an;
    for (; ak; ak++) {
      int bc = 7;
      for (; bc >= 0; bc--) {
        al;
        al;
        d = f && an;
        an;
      }
    }
  }
}
int main() {
  int k;
  for (; aj < 6; aj++)
    c[0] = aj;
  aw();
  for (aj = 0; aj < 6; aj++)
    for (k = 0; k < 5; k++)
      d = c[d ^ g[aj][k]];
  if (d != 5)
    __builtin_abort();
  return 0;
}
