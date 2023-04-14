/* { dg-do run } */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

double num = 100;
double denom = 50;

union bits {
  double fp;
  uint64_t ull;
};

int main (int argc, char* argv[])
{
  union bits recip;
  union bits res;

  recip.fp = 1.0 / denom;

  if (recip.ull != 0x3f947ae147ae147b)
    {
      fprintf (stderr, "incorrectly-rounded reciprocal: %llx", recip.ull);
      exit (1);
    }

  res.fp = num / denom;

  if (res.ull != 0x4000000000000000)
    {
      fprintf (stderr, "incorrectly-rounded quotient: %llx", res.ull);
      exit (1);
    }

  return 0;
}
