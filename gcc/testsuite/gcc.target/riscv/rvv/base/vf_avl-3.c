/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

void foo (int *src, int *dst, int size) {
 int i;
 for (i = 0; i < size; i++)
  *dst++ = *src & 0x80 ? (*src++ & 0x7f) : -*src++;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 } } */
