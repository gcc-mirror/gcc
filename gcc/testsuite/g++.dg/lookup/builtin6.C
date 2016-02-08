// PR c++/69657
// { dg-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump "ABS_EXPR" "gimple" } }

namespace foo
{
  inline double
  abs(double x)
  { return __builtin_fabs(x); }
}
using foo::abs;

extern "C" int abs(int);

namespace bar {
  using ::abs;
}

int
wrapper (int x)
{
  return bar::abs (x) + bar::abs(x);
}
