/* { dg-do compile } */
/* { dg-additional-options "-O2" } */
#include <arm_sve.h>
unsigned c;
long d;
void f() {
  unsigned char *b;
  svbool_t x = svptrue_b8();
  svuint32_t g;
  svuint8_t h, i;
  d = 0;
  for (; (unsigned *)d < &c; d += 16) {
    h = svld1rq(x, &b[d]);
    g = svdot_lane(g, i, h, 3);
  }
  svst1_vnum(x, &c, 8, g);
}
