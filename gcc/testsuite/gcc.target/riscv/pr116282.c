/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zba_zbkb -mabi=lp64d" } */
short a;
long b;
char c, d;
void e(int f[][4][24][4], long g[][24][24][24]) {
  for (unsigned h = 2;; h = 3)
    for (long i = 0; i < 4; i = 5006368639)
      for (int j = 0; j < 4; j = 4) {
        for (long k = -4294967294; k < (c ?: f[0][2][6][j]); k += b)
          a = g[k][j][0][h];
        for (; f ? d : 0;)
          ;
      }
}

