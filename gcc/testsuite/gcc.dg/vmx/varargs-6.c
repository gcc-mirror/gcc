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
varargsn025(int p1, double p2, ...)
{
  n025 q;
  va_list ap;
  va_start(ap, p2);
  q.m1 = p1;
  q.m2 = p2;
  q.m3 = va_arg(ap, int);
  q.m4 = va_arg(ap, vector signed int);
  va_end(ap);

  check(q.m1 == 1363477585, "q.m1");
  check(q.m2 == -8.72e+18, "q.m2");
  check(q.m3 == 198652649, "q.m3");
  check(vec_all_eq(q.m4, ((vector signed int){323001541, -1353029458, 1756879633, -327031280})),
	"q.m4");
}

static void test()
{
  varargsn025(1363477585, -8.72e+18, 198652649, ((vector signed int){323001541, -1353029458, 1756879633, -327031280}));
}
