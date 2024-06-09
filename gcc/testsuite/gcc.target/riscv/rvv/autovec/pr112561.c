/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3 -ftree-vectorize -mrvv-vector-bits=zvl -mcmodel=medlow" } */

int printf(char *, ...);
int a, b, c, e;
short d[7][7] = {};
int main() {
  short f;
  c = 0;
  for (; c <= 6; c++) {
    e |= d[c][c] & 1;
    b &= f & 3;
  }
  printf("%d\n", a);
  return 0;
}
