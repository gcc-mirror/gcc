#include "harness.h"
#include <stdarg.h>

static void
varargsn003(vector float p1, vector float p2, vector float p3, ...)
{
  va_list ap;
  vector float i1;
  vector float i2;
  vector float i3;
  vector float i4;
  vector float i5;
  vector float i6;
  vector float i7;
  vector float i8;
  vector float i9;
  vector float i10;
  vector float i11;
  vector float i12;
  vector float i13;
  vector float i14;
  vector float i15;
  int i16;

  va_start(ap, p3);
  i1 = p1;
  i2 = p2;
  i3 = p3;
  i4 = va_arg(ap, vector float);
  i5 = va_arg(ap, vector float);
  i6 = va_arg(ap, vector float);
  i7 = va_arg(ap, vector float);
  i8 = va_arg(ap, vector float);
  i9 = va_arg(ap, vector float);
  i10 = va_arg(ap, vector float);
  i11 = va_arg(ap, vector float);
  i12 = va_arg(ap, vector float);
  i13 = va_arg(ap, vector float);
  i14 = va_arg(ap, vector float);
  i15 = va_arg(ap, vector float);
  i16 = va_arg(ap, int);
  va_end(ap);

  check(vec_all_eq(i1, ((vector float){1.14e+09, 4.29e+08, -1.58e+09, 1.66e+09})), "i1");
  check(vec_all_eq(i2, ((vector float){-1.83e+09, -6.79e+08, 1.58e+09, -3.38e+08})), "i2");
  check(vec_all_eq(i3, ((vector float){-1.19e+09, -4.27e+08, 6.84e+08, 1.21e+08})), "i3");
  check(vec_all_eq(i4, ((vector float){1.47e+09, 9.17e+08, 3.45e+08, -1.17e+08})), "i4");
  check(vec_all_eq(i5, ((vector float){3.08e+08, 1.2e+08, 1.73e+09, 1.77e+09})), "i5");
  check(vec_all_eq(i6, ((vector float){1.89e+09, 2.06e+09, 2.64e+08, 1.05e+09})), "i6");
  check(vec_all_eq(i7, ((vector float){5.45e+08, 1.37e+09, -8.2e+08, 4.32e+07})), "i7");
  check(vec_all_eq(i8, ((vector float){3.47e+08, -1.66e+09, 1.25e+09, 1.53e+09})), "i8");
  check(vec_all_eq(i9, ((vector float){-6.04e+08, 1.48e+09, -1.48e+09, 1.92e+09})), "i9");
  check(vec_all_eq(i10, ((vector float){-1.66e+09, -8.92e+08, -3.78e+08, 2.11e+09})), "i10");
  check(vec_all_eq(i11, ((vector float){-7.46e+08, 4.01e+08, -1.78e+09, 1.83e+09})), "i11");
  check(vec_all_eq(i12, ((vector float){1.83e+09, 5.73e+08, -2.96e+08, -7.46e+08})), "i12");
  check(vec_all_eq(i13, ((vector float){-2.01e+09, 9.89e+08, -1.92e+09, 2.09e+09})), "i13");
  check(vec_all_eq(i14, ((vector float){1.95e+09, -2.41e+08, 2.67e+08, 1.67e+09})), "i14");
  check(vec_all_eq(i15, ((vector float){-2.12e+09, 8.18e+08, 9.47e+08, -1.25e+09})), "i15");
  check(i16 == -947264420, "i16");
}

static void test()
{
  varargsn003(((vector float){1.14e+09, 4.29e+08, -1.58e+09, 1.66e+09}),
	      ((vector float){-1.83e+09, -6.79e+08, 1.58e+09, -3.38e+08}),
	      ((vector float){-1.19e+09, -4.27e+08, 6.84e+08, 1.21e+08}),
	      ((vector float){1.47e+09, 9.17e+08, 3.45e+08, -1.17e+08}),
	      ((vector float){3.08e+08, 1.2e+08, 1.73e+09, 1.77e+09}),
	      ((vector float){1.89e+09, 2.06e+09, 2.64e+08, 1.05e+09}),
	      ((vector float){5.45e+08, 1.37e+09, -8.2e+08, 4.32e+07}),
	      ((vector float){3.47e+08, -1.66e+09, 1.25e+09, 1.53e+09}),
	      ((vector float){-6.04e+08, 1.48e+09, -1.48e+09, 1.92e+09}),
	      ((vector float){-1.66e+09, -8.92e+08, -3.78e+08, 2.11e+09}),
	      ((vector float){-7.46e+08, 4.01e+08, -1.78e+09, 1.83e+09}),
	      ((vector float){1.83e+09, 5.73e+08, -2.96e+08, -7.46e+08}),
	      ((vector float){-2.01e+09, 9.89e+08, -1.92e+09, 2.09e+09}),
	      ((vector float){1.95e+09, -2.41e+08, 2.67e+08, 1.67e+09}), ((vector float){-2.12e+09, 8.18e+08, 9.47e+08, -1.25e+09}), -947264420);
}
