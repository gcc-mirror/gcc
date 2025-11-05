/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -w -O0" { target rv64 } } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -w -O0" { target rv32 } } */


typedef int a;
typedef signed char b;
typedef char c;
typedef short d;
typedef unsigned short e;
typedef a f;
typedef unsigned g;
typedef long h;
h j, k, l, m, n, o;
int p, q, r, s;
short t;
volatile a u;
a v[];
char w, x;
a *y, *z;
a **aa;
__attribute__((always_inline)) b __attribute__((vector_size(16)))
ab(f __attribute__((vector_size(8 * sizeof(f)))), d ac,
   d __attribute__((vector_size(2 * sizeof(d)))), d) {
  return __builtin_shufflevector(
      (b __attribute__((vector_size(16)))) __builtin_convertvector(
          (d __attribute__((vector_size(16 *sizeof(d))))){
              ac, ac, ac, ac, ac, ac, ac, ac, ac, ac, ac, ac, ac},
          c __attribute__((vector_size(16)))) |
          __builtin_convertvector(
              (d __attribute__((vector_size(16 *sizeof(d))))){
                  ac, ac, ac, ac, ac, ac, ac, ac, ac, ac, ac, ac, ac},
              c __attribute__((vector_size(16)))),
      __builtin_convertvector(
          (d __attribute__((vector_size(16 *sizeof(d))))){
              ac, ac, ac, ac, ac, ac, ac, ac, ac, ac, ac, ac, ac},
          b __attribute__((vector_size(16)))),
      3, 21, 0, 2, 2, 7, 1, 8, 4, 0, 8, 0, 8, 9, 5, 6);
}
__attribute__((always_inline)) g
ad(d ae, h __attribute__((vector_size(32 * sizeof(h))))) {
  g f = 6318;
  return (8 ? ae / 786856318u : 0) & ae;
}
a(af)(a, int);
void(ag)(long);
char(ah)(char, char);
char(ai)(char);
short(aj)(short, short);
int ak(long, int *, int *, char, int);
void al(signed, a *, int *, long);
char am(int *, short, short);
void an(int *, long, int);
void ao(int, int *, a *);
a ap() {
  int *aq, *ar, *as;
  short at;
  char au, av, aw = 2;
  long ax, ay, az = j;
  int ba, i;
  g __attribute__((vector_size(16 * sizeof(g)))) bb = {80};
  b __attribute__((vector_size(4))) bc = {6};
  int bd[1];
  char *be = &w;
  int bf, bg = q;
  a **bh[] = {
      &y, &z, &z, &y, &y, &y, &y, &y, &z, &z, &y, &z, &y, &y, &y, &y, &z, &y,
      &z, &y, &y, &y, &z, &z, &z, &y, &z, &z, &z, &y, &z, &z, &y, &z, &z, &y,
      &z, &z, &z, &y, 0,  &z, 0,  &y, 0,  &y, &y, &z, &z, &y, &y, 0,  &z, 0,
      &z, 0,  &y, &z, &z, 0,  &z, 0,  &z, &z, &z, &y, &z, &z, &y, &z, &z, &y,
      0,  &z, 0,  &z, &z, &y, 0,  &z, 0,  &y, 0,  &y, &y, &z, &z, &y, &y, 0,
      &z, 0,  &z, 0,  &y, &z, &z, 0,  &z, 0,  &z, &z, &z, &y, &z, &z, &y, &z,
      &z, &y, 0,  &z, 0,  &z, &z, &y, 0,  &z, 0,  &y, 0,  &y, &y, &z, &z, &y,
      &y, 0,  &z, 0,  &z, 0,  &y, &z, &z, 0,  &z, 0,  &z, &z, &z, &y, &z, &z,
      &y, &z, &z, &y, 0,  &z, 0,  &z, &z, &y, 0,  &z, 0,  &y, 0,  &y, &y, &z,
      &z, &y, &y, 0,  &z, 0,  &z, 0,  &y, &z, &z, 0,  0,  &z, 0,  &z, &z, &z,
      &y, &z, &z, &y, &z, &z, &y, 0,  &z, 0,  0,  &z, &z};
  for (; i; i++)
    bd[i] = p;
  h __attribute__((vector_size(32 * sizeof(h))))
  bi = {2681, 2681, 2681, 2681, 2681, 2681, 2681, 2681, 2681, 2681, 2681,
        2681, 2681, 2681, 2681, 2681, 2681, 2681, 2681, 2681, 2681, 2681,
        2681, 2681, 2681, 2681, 2681, 2681, 2681, 2681, 2681, 2681},
  bj = __builtin_convertvector(
      (c __attribute__((vector_size(32)))){
          aw, aw, aw, aw, aw, aw, aw, aw, aw, aw, aw, aw, aw, aw, aw, aw,
          aw, aw, aw, aw, aw, aw, aw, aw, aw, aw, aw, aw, aw, aw, aw, aw},
      h __attribute__((vector_size(32 * sizeof(h))))),
  bk = __builtin_convertvector(
      __builtin_shufflevector(bb, bb, 4, 8, 7, 9, 1, 10, 4, 7, 0, 4, 3, 5, 6, 7,
                              6, 2, 2, 20, 6, 4, 7, 7, 9, 7, 4, 9, 8, 6, 1, 0,
                              6, 9),
      h __attribute__((vector_size(32 * sizeof(h)))));
  bb = __builtin_convertvector(
      ab(__builtin_shufflevector(
             __builtin_shufflevector(
                 __builtin_convertvector(
                     __builtin_shufflevector(bb, bb, 1, 31, 8, 2, 3, 7, 4, 0, 7,
                                             3, 4, 6, 7, 1, 9, 3, 8, 7, 1, 8, 5,
                                             3, 9, 9, 0, 3, 2, 8, 5, 2, 5, 3),
                     f __attribute__((vector_size(32 * sizeof(f))))),
                 (f __attribute__((vector_size(32 * sizeof(f))))){
                     800761418310502961587690471176286910032020044212442466872080013589354162852207417903424527024812447907811618435019152886919380169872910001752451018659493155196043018716516518746289614523948734758456011127254301274351182132760058399143431214610613191313926994549901191890929084305862034120561651877003645},
                 32, 44),
             (f __attribute__((vector_size(2 * sizeof(f))))){o}, 1, 0, 3, 0, 2,
             1, 3, 3),
         ad(__builtin_clzg((g)aw, (f)bb[9]),
            (h __attribute__((vector_size(32 * sizeof(h))))){
                bi[0] ?: bk[0],    bi[1] ? 1 : bk[1],  bi[2] ? 2 : bk[2],
                bi[3] ? 3 : bk[3], bi[4] ? 4 : bk[4],  bi[5] ? 5 : bk[5],
                bi[6] ? 6 : bk[6], bi[7] ? 7 : bk[7],  bi[8] ? 8 : bk[8],
                bi[9] ? 9 : bk[9], bi[0] ? 10 : bk[0], bi[1] ? 1 : bk[1],
                bi[2] ? 2 : bk[2], bi[3] ? 3 : bk[3],  bi[4] ? 4 : bk[4],
                bi[5] ? 5 : bk[5], bi[6] ? 6 : bk[6],  bi[7] ? 7 : bk[7],
                bi[8] ? 8 : bk[8], bi[9] ? 9 : bk[9],  bi[0] ? 20 : bk[0],
                bi[1] ? 1 : bk[1], bi[2] ? 2 : bk[2],  bi[3] ? 3 : bk[3],
                bi[4] ? bj[4] : 4, bi[5] ?: 5,         bi[6] ?: 6,
                bi[7] ? 0 : 7,     bi[8] ?: 8,         bi[9] ? 0 : 9,
                bi[0] ? 0 : 30,    bi[1] ?: 1}),
         (d __attribute__((vector_size(2 * sizeof(d)))))
                 __builtin_shufflevector(
                     __builtin_convertvector(
                         __builtin_shufflevector(bb, bb, 2, 7, 21, 6),
                         e __attribute__((vector_size(4 * sizeof(e))))),
                     __builtin_convertvector(
                         (c __attribute__((vector_size(4)))){aw, aw},
                         e __attribute__((vector_size(4 * sizeof(e))))),
                     5, 1) +
             (__builtin_convertvector(
                  __builtin_shufflevector(bb, bb, 4, 5),
                  e __attribute__((vector_size(2 * sizeof(e))))) <=
              __builtin_convertvector(
                  (c __attribute__((vector_size(2)))){aw},
                  e __attribute__((vector_size(2 * sizeof(e)))))),
         n ? bb[5] << n : aw),
      g __attribute__((vector_size(16 * sizeof(g)))));
  ag(aw & t);
  at = aj(aw, v[1]);
  au = ah(at, aw);
  ba = af((1 == ax != aw) <= aw <= au, aw);
  ao(0, &bd[0], &r);
  o = ay;
  an(aq, aw, k);
  av = am(ar, l, k);
  *be = ai(*be);
  al(x, as, &bd[0], aw);
  bg = ak(u, &s, &bf, aw, aw);
  as = *aa;
  return m;
}
