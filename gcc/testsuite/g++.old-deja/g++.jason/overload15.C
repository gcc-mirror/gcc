// Bug: g++ thinks that int->long is a promotion.
// Build don't link:

long f (long, long);
double f (double, double);

void g (double d)
{
  f (d, 0);
}
