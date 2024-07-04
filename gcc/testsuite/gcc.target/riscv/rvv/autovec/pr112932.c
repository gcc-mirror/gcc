/* { dg-do run } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target riscv_v } */

#include <assert.h>
int a, j, n, b, c, o, d, g, h;
int e[8];
long f[8][6];
void l() {
  o = -27;
  for (; o; o++) {
    *e = 1;
    if (a >= n) {
      d = 0;
      for (; d <= 7; d++)
        e[d] = c;
    }
  }
  j = 0;
  for (; j < 8; j++) {
    g = 0;
    for (; g < 2; g++) {
      h = 1;
      for (; h < 3; h++)
        f[j][g * 2 + h] = 1;
    }
  }
  unsigned long *m = &f[1][1];
  *m = 0;
}
int main() {
  l();
  assert (f[0][1] == 1);
  assert (f[0][2] == 1);
  assert (f[0][3] == 1);
  assert (f[0][4] == 1);
  assert (f[1][1] == 0);
  assert (f[1][2] == 1);
  assert (f[1][3] == 1);
  assert (f[1][4] == 1);
  assert (f[2][1] == 1);
  assert (f[2][2] == 1);
  assert (f[2][3] == 1);
  assert (f[2][4] == 1);
  assert (f[3][1] == 1);
  assert (f[3][2] == 1);
  assert (f[3][3] == 1);
  assert (f[3][4] == 1);
  assert (f[4][1] == 1);
  assert (f[4][2] == 1);
  assert (f[4][3] == 1);
  assert (f[4][4] == 1);
  assert (f[5][1] == 1);
  assert (f[5][2] == 1);
  assert (f[5][3] == 1);
  assert (f[5][4] == 1);
  assert (f[6][1] == 1);
  assert (f[6][2] == 1);
  assert (f[6][3] == 1);
  assert (f[6][4] == 1);
  assert (f[7][1] == 1);
  assert (f[7][2] == 1);
  assert (f[7][3] == 1);
  assert (f[7][4] == 1);
}

