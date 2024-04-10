/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

int a, b, c, e;
short d[7][7] = {};
int foo() {
  short f;
  c = 0;
  for (; c <= 6; c++) {
    e |= d[c][c] & 1;
    b &= f & 3;
  }
  return a;
}

/* { dg-final { scan-assembler-not {vsetvli\s+[a-x0-9]+,\s*zero} } } */
