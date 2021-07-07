#include <math.h>
#include <stdio.h>

int main ()
{
  double a = 1.0/0.0;

  if (! isfinite (a))
    printf ("infinity detected\n");

  a = 1.0/1.0;

  if (isfinite (a))
    printf ("number is now finite\n");
  return 0;
}
