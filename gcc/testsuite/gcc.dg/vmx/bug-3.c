#include "harness.h"

static vector unsigned int value = { 9, 9, 9, 9 };

void varargsC2 (int p1, int p2, int p3, int p4, int p5, int p6, int p7,
		int p8, vector unsigned int p9, int p10)
{
  int i1;
  int i2;
  int i3;
  int i4;
  int i5;
  int i6;
  int i7;
  int i8;
  vector unsigned int i9;
  int i10;

  i1 = p1;
  i2 = p2;
  i3 = p3;
  i4 = p4;
  i5 = p5;
  i6 = p6;
  i7 = p7;
  i8 = p8;
  i9 = p9;
  i10 = p10;

  check(i1 == 1, "i1");
  check(i2 == 2, "i2");
  check(i3 == 3, "i3");
  check(i4 == 4, "i4");
  check(i5 == 5, "i5");
  check(i6 == 6, "i6");
  check(i7 == 7, "i7");
  check(i8 == 8, "i8");
  check(vec_all_eq(i9, value), "i9");
  check(i10 == 10, "i10");
}

void test()
{
  varargsC2 (1, 2, 3, 4, 5, 6, 7, 8, ((vector unsigned int){9,9,9,9}), 10);
}
