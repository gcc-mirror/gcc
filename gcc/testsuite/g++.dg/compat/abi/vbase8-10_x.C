// { dg-options -w }

#include "vbase8-10.h"

extern void check_C0 (C0&, int);
extern void check_C1 (C1&, int);
extern void check_C2 (C2&, int);
extern void check_C3 (C3&, int);
extern void check_C4 (C4&, int);
extern void check_C5 (C5&, int);
extern void check_C6 (C6&, int);
extern void check_C7 (C7&, int);
extern void check_C8 (C8&, int);
extern void check_C9 (C9&, int);

void
vbase8_10_x (void)
{
  C0 c0;
  C1 c1;
  C2 c2;
  C3 c3;
  C4 c4;
  C5 c5;
  C6 c6;
  C7 c7;
  C8 c8;
  C9 c9;

  c0.i0 = 0;
  c1.i1 = 101;
  c2.i2 = 202;
  c3.i3 = 303;
  c4.i4 = 404;
  c5.i5 = 505;
  c6.i6 = 606;
  c7.i7 = 707;
  c8.i8 = 808;
  c9.i9 = 909;

  check_C0 (c0, 0);
  check_C1 (c1, 101);
  check_C2 (c2, 202);
  check_C3 (c3, 303);
  check_C4 (c4, 404);
  check_C5 (c5, 505);
  check_C6 (c6, 606);
  check_C7 (c7, 707);
  check_C8 (c8, 808);
  check_C9 (c9, 909);
}
