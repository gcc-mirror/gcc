// { dg-do assemble  }
// Bug: g++ thinks that int->long is a promotion.

long f (long, long);
double f (double, double);

void g (double d)
{
  f (d, 0);
}
