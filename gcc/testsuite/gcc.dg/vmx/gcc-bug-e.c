#include "harness.h"

typedef struct n_a
{
  signed char m1;
  short m2;
  int m3;
  double m4;
  vector float m5;
}
n_a;

static void
initn_a(signed char p1, short p2, int p3, double p4, vector float p5)
{
  n_a i;
  static struct
  {
    n_a b;
    char a;
  }
  x;

  i.m1 = p1;
  i.m2 = p2;
  i.m3 = p3;
  i.m4 = p4;
  i.m5 = p5;

  check(i.m1 == -17, "i.m1");
  check(i.m2 == 9165, "i.m2");
  check(i.m3 == -1857760764, "i.m3");
  check(i.m4 == 7.3e+18, "i.m4");
  check(vec_all_eq(i.m5, ((vector float){-5.02e+08,
					  -4.34e+08,
					  -1.04e+09,
					   1.42e+09})), "i.m5");
}

static void test()
{
  initn_a(-17, 9165, -1857760764, 7.3e+18,
	  ((vector float){-5.02e+08, -4.34e+08, -1.04e+09, 1.42e+09}));
}
