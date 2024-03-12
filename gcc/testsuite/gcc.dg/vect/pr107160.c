#include <math.h>

#define N 128
float fl[N];

__attribute__ ((noipa)) void
init ()
{
  for (int i = 0; i < N; i++)
    fl[i] = i;
}

__attribute__ ((noipa)) float
foo (int n1)
{
  float sum0, sum1, sum2, sum3;
  sum0 = sum1 = sum2 = sum3 = 0.0f;

  int n = (n1 / 4) * 4;
  for (int i = 0; i < n; i += 4)
    {
      sum0 += fabs (fl[i]);
      sum1 += fabs (fl[i + 1]);
      sum2 += fabs (fl[i + 2]);
      sum3 += fabs (fl[i + 3]);
    }

  return sum0 + sum1 + sum2 + sum3;
}

int
main ()
{
  init ();
  float res = foo (80);
  if (res != 3160)
    __builtin_abort ();
  return 0;
}
