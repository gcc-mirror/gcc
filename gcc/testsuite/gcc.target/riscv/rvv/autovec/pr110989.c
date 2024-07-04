/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=scalable -Ofast -fno-schedule-insns -fno-schedule-insns2" } */

int a, b, c;
double *d;
void e() {
  double f;
  for (; c; c++, d--)
    f = *d ?: *(&a + c);
  b = f;
}

/* { dg-final { scan-assembler-times {vsetvli} 3 } }  */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e64,\s*m1,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e64,\s*m1,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 } } */
