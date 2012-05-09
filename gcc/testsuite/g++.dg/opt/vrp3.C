// PR tree-optimization/53239
// { dg-do run }
// { dg-options "-O2" }
// { dg-additional-sources "vrp3-aux.cc" }

#include "vrp3.h"

struct M
{
  M (R m);
  R val;
  static int compare (M const &, M const &);
};

inline M const &
min (M const & t1, M const & t2)
{
  return R::compare (t1.val, t2.val) < 0 ? t1 : t2;
}

M::M (R m)
{
  val = m;
}

M
test (M *x)
{
  M n (R (0, 0));

  for (int i = 0; i < 2; i++)
    {
      M p = x[i];
      n = min (n, p);
    }

  if (n.val.r2 != 2 || n.val.r1 != 1)
    __builtin_abort ();
  return n;
}

int
main ()
{
  M x[2] = { M (R (1, 2)), M (R (1, 1)) };
  test (x);
}
