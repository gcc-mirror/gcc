#include <float.h>

extern void abort(void);

static const double C = DBL_MAX;

double foo(double x)
{
        return ( ( (x * C) * C ) * C);
}

int main ()
{
  double d = foo (0.0);
  if (d != 0.0)
   abort ();

  return 0;
}
