// { dg-do compile }
// { dg-options "" }

#include "vrp3.h"

R::R ()
{
  r1 = r2 = 1;
}

R::R (int n, int d)
{
  r1 = n;
  r2 = d;
}

int
R::compare (R const &r, R const &s)
{
  return (int) (r.r1 * s.r2 - s.r1 * r.r2);
}
