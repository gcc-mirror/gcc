#include "tree-vect.h"

char mask[128];

double __attribute__((noipa))
foo (double *a, int n)
{
  double sum = 0.0;
  for (int i = 0; i < n; ++i)
    {
      double val;
      if (mask[i])
        val = a[i];
      else
        val = -0.0;
      sum = sum + val;
    }
  return sum;
}

double a[128];

int main()
{
  check_vect ();

#pragma GCC novector
  for (int i = 0; i < 128; ++i)
    {
      a[i] = (i * 7) % 15;
      mask[i] = (i + 1) & 4;
    }

  double sum = foo (a, 87);
  double sum2 = 0.0;
#pragma GCC novector
  for (int i = 0; i < 87; ++i)
    {
      double val;
      if (mask[i])
        val = a[i];
      else
        val = -0.0;
      sum2 = sum2 + val;
    }

  if (sum != sum2)
    __builtin_abort ();
  return 0;
}
