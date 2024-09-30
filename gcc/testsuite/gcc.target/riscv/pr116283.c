/* { dg-do compile } */
/* { dg-additional-options "-std=gnu99 -w -march=rv64id_zbs -mabi=lp64d" } */

char c;
#define d(a, b)                                                                \
  {                                                                            \
    __typeof__(a) e = a;                                                       \
    e;                                                                         \
  }
long f;
void g(signed h[][9][9][9][9]) {
  for (unsigned i = f; i; i += 3)
    c = (d(1 << (3629 & h[i][i][1][5][i]), ));
}

