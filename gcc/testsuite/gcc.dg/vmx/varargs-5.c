#include "harness.h"
#include <stdarg.h>

typedef struct n025
{
  int m1;
  double m2;
  int m3;
  vector signed int m4;
}
n025;

static void
varlistn025(int p1, double p2, va_list ap)
{
  n025 q;
  q.m1 = p1;
  q.m2 = p2;
  q.m3 = va_arg(ap, int);
  q.m4 = va_arg(ap, vector signed int);

  check(q.m1 == 1363477585, "q.m1");
  check(q.m2 == -8.72e+18, "q.m2");
  check(q.m3 == 198652649, "q.m3");
  check(vec_all_eq(q.m4, ((vector signed int){323001541, -1353029458, 1756879633, -327031280})),
	"q.m5");
}


void
varargsn025(int p1, double p2, ...)
{
  n025 r, s;
  va_list ap;

  va_start(ap, p2);
  r.m1 = p1;
  r.m2 = p2;
  r.m3 = va_arg(ap, int);
  r.m4 = va_arg(ap, vector signed int);
  va_end(ap);

  check(r.m1 == 1363477585, "r.m1");
  check(r.m2 == -8.72e+18, "r.m2");
  check(r.m3 == 198652649, "r.m3");
  check(vec_all_eq(r.m4, ((vector signed int){323001541, -1353029458, 1756879633, -327031280})),
	"r.m5");

  va_start(ap, p2);
  s.m1 = p1;
  s.m2 = p2;
  s.m3 = va_arg(ap, int);
  s.m4 = va_arg(ap, vector signed int);
  va_end(ap);

  check(s.m1 == 1363477585, "s.m1");
  check(s.m2 == -8.72e+18, "s.m2");
  check(s.m3 == 198652649, "s.m3");
  check(vec_all_eq(s.m4, ((vector signed int){323001541, -1353029458, 1756879633, -327031280})),
	"s.m5");

  va_start(ap, p2);
  varlistn025(p1, p2, ap);
  va_end(ap);
}

static void test()
{
  varargsn025(1363477585, -8.72e+18, 198652649,
	      ((vector signed int){323001541, -1353029458, 1756879633, -327031280}));
}
