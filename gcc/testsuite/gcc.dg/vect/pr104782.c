/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-march=armv8.2-a+sve -msve-vector-bits=128" { target aarch64-*-* } } */

int a, b, c;
static int d;
short *q;
void f() {
  int *p = &d;
  b = 9;
  for (b = 9; b; b--) {
    a = 2;
    for (c = 2; c <= 9; c++) {
      for (int i = 0; i < 3; i++)
        *p |= (*q)++;
    }
  }
}
