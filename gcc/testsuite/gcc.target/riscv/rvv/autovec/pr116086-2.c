/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-max-lmul=m2" } */

long a;
long b;
long c[80];
int main() {
    for (int d = 0; d < 16; d++)
      c[d] = a;
    for (int d = 16; d < 80; d++)
      c[d] = c[d - 2];
    for (int d = 0; d < 80; d += 8)
      b += c[d];
    if (b != 0)
      __builtin_abort ();
}

/* { dg-final { scan-assembler-times "vmv1r" 0 } } */
