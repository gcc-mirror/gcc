/* { dg-do run { target { riscv_v && rv64 } } } */
/* { dg-additional-options "-std=c99 -O3 -march=rv64gcv_zvl256b -mrvv-vector-bits=zvl" } */

int a;
int b[14];
char c[14][14];

int main() {
  for (long f = 0; f < 14; ++f)
    for (long g = 0; g < 4; ++g)
      c[f][g] = 1;

  for (short f = 0; f < 12; f += 1)
    c[f][f] = b[f];

  for (long f = 0; f < 4; ++f)
    for (long g = 0; g < 14; ++g)
      a ^= c[f][g];

  if (a != 0)
    __builtin_abort ();

  return 0;
}
