/* PR rtl-optimization/98791  */
/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize" } */
#include <arm_sve.h>
extern char a[11];
extern long b[];
void f() {
  for (int d; d < 10; d++) {
    a[d] = svaddv(svptrue_b8(), svdup_u8(0));
    b[d] = 0;
  }
}
