/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv_zvl256b -mabi=lp64d -mrvv-vector-bits=zvl" } */

long a;
short b[60000];
short c[20];
int main() {
  for (short d = 0; d < 20; d += 3) {
    c[d] = 0;
    for (int e = 0; e < 20; e += 2)
      for (int f = 1; f < 20; f += 2)
        a += (unsigned)b[f + e];
  }
  if (a != 0)
    __builtin_abort ();
}

/* { dg-final { scan-assembler-times "vsetivli\tzero,1" 0 } } */
