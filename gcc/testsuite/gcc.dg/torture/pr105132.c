/* { dg-do compile } */
/* { dg-additional-options "-march=skylake-avx512" { target x86_64-*-* i?86-*-* } } */

short a;
extern int b[];
int c;
void d(long f[][5][5][17], int g[][5][5][17]) {
  for (short e = 0; e < 17; e++) {
    a = g[19][2][3][e];
    b[e] = c & (f[3][2][3][e] && g[19][2][3][e]);
  }
}
