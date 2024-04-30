/* { dg-do run } */
/* { dg-require-effective-target riscv_v } */
/* { dg-options { -march=rv64gcv_zvl256b -mabi=lp64d -fwhole-program -O3 -mrvv-vector-bits=zvl  } } */

int f[18];
int g[18];
int h[18][18][18];
int a[324];
long b[18];
int *i = g;
int (*j)[18][18] = h;
int z;
int main() {
  for (int m = 0; m < 18; ++m)
    f[m] = 3;
  for (int m = 0; m < 18; m += 1)
    for (int n = 0; n < 18; n += 3) {
      a[m * 8 + n] = j[m][m][0] ? i[n] : 0;
      b[n] = f[n] ? -i[m] : 0;
    }
  for (long n = 0; n < 8; ++n)
    z = a[n];
  if (b[15] != 0)
    __builtin_abort();
}
