#include "harness.h"
#include <stdarg.h>

typedef struct n_a
{
  signed char m1;
  short m2;
  int m3;
  double m4;
  vector float m5;
}
n_a;

void
varlistn_a(signed char p1, va_list ap)
{
  n_a q;
  q.m1 = p1;
  q.m2 = va_arg(ap, int);
  q.m3 = va_arg(ap, int);
  q.m4 = va_arg(ap, double);
  q.m5 = va_arg(ap, vector float);

  check(q.m1 == 77, "q.m1");
  check(q.m2 == 1924, "q.m2");
  check(q.m3 == -1471601920, "q.m3");
  check(q.m4 == 3.65e+18, "q.m4");
  check(vec_all_eq(q.m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})), "q.m5");
}

void
varargsn_a(signed char p1, ...)
{
  n_a r, s;
  va_list ap;

  va_start(ap, p1);
  r.m1 = p1;
  r.m2 = va_arg(ap, int);
  r.m3 = va_arg(ap, int);
  r.m4 = va_arg(ap, double);
  r.m5 = va_arg(ap, vector float);
  va_end(ap);

  check(r.m1 == 77, "r.m1");
  check(r.m2 == 1924, "r.m2");
  check(r.m3 == -1471601920, "r.m3");
  check(r.m4 == 3.65e+18, "r.m4");
  check(vec_all_eq(r.m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})), "r.m5");

  va_start(ap, p1);
  s.m1 = p1;
  s.m2 = va_arg(ap, int);
  s.m3 = va_arg(ap, int);
  s.m4 = va_arg(ap, double);
  s.m5 = va_arg(ap, vector float);
  va_end(ap);

  check(s.m1 == 77, "s.m1");
  check(s.m2 == 1924, "s.m2");
  check(s.m3 == -1471601920, "s.m3");
  check(s.m4 == 3.65e+18, "s.m4");
  check(vec_all_eq(s.m5, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08})), "s.m5");

  va_start(ap, p1);
  varlistn_a(p1, ap);
  va_end(ap);
}



void test()
{
  varargsn_a(77, 1924, -1471601920, 3.65e+18, ((vector float){-1.38e+09, 5.96e+08, 6.88e+08, -3.2e+08}));
}
