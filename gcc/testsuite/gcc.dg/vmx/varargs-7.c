#include "harness.h"
#include <stdarg.h>

static void
varargsn001(vector unsigned int p1, vector unsigned int p2,
	    vector unsigned int p3, vector unsigned int p4,
	    vector unsigned int p5, vector unsigned int p6,
	    vector unsigned int p7, vector unsigned int p8,
	    vector unsigned int p9, vector unsigned int p10,
	    vector unsigned int p11, vector unsigned int p12,
	    vector unsigned int p13, ...)
{
  va_list ap;
  vector unsigned int i1;
  vector unsigned int i2;
  vector unsigned int i3;
  vector unsigned int i4;
  vector unsigned int i5;
  vector unsigned int i6;
  vector unsigned int i7;
  vector unsigned int i8;
  vector unsigned int i9;
  vector unsigned int i10;
  vector unsigned int i11;
  vector unsigned int i12;
  vector unsigned int i13;
  vector unsigned int i14;
  int i15;

  va_start(ap, p13);

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
  i11 = p11;
  i12 = p12;
  i13 = p13;
  i14 = va_arg(ap, vector unsigned int);
  i15 = va_arg(ap, int);
  va_end(ap);

  check(vec_all_eq(i1, ((vector unsigned int){1,1,1,1})), "i1");
  check(vec_all_eq(i2, ((vector unsigned int){2,2,2,2})), "i2");
  check(vec_all_eq(i3, ((vector unsigned int){3,3,3,3})), "i3");
  check(vec_all_eq(i4, ((vector unsigned int){4,4,4,4})), "i4");
  check(vec_all_eq(i5, ((vector unsigned int){5,5,5,5})), "i5");
  check(vec_all_eq(i6, ((vector unsigned int){6,6,6,6})), "i6");
  check(vec_all_eq(i7, ((vector unsigned int){7,7,7,7})), "i7");
  check(vec_all_eq(i8, ((vector unsigned int){8,8,8,8})), "i8");
  check(vec_all_eq(i9, ((vector unsigned int){9,9,9,9})), "i9");
  check(vec_all_eq(i10, ((vector unsigned int){10,10,10,10})), "i10");
  check(vec_all_eq(i11, ((vector unsigned int){11,11,11,11})), "i11");
  check(vec_all_eq(i12, ((vector unsigned int){12,12,12,12})), "i12");
  check(vec_all_eq(i13, ((vector unsigned int){13,13,13,13})), "i13");
  check(vec_all_eq(i14, ((vector unsigned int){14,14,14,14})), "i14");
  check(i15 == 15, "i15");
}

static void test()
{
  varargsn001(((vector unsigned int){1,1,1,1}),
	      ((vector unsigned int){2,2,2,2}),
	      ((vector unsigned int){3,3,3,3}),
	      ((vector unsigned int){4,4,4,4}),
	      ((vector unsigned int){5,5,5,5}),
	      ((vector unsigned int){6,6,6,6}),
	      ((vector unsigned int){7,7,7,7}),
	      ((vector unsigned int){8,8,8,8}),
	      ((vector unsigned int){9,9,9,9}),
	      ((vector unsigned int){10,10,10,10}),
	      ((vector unsigned int){11,11,11,11}),
	      ((vector unsigned int){12,12,12,12}),
	      ((vector unsigned int){13,13,13,13}),
	      ((vector unsigned int){14,14,14,14}),
	      15);
}
