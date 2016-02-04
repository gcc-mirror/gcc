/* { dg-do compile } */

int a, b;

void
__sched_cpucount (void)
{
  while (b)
    {
      long l = b++;
      a += __builtin_popcountl(l);
    }
}

void
slp_test (int *x, long *y)
{
  for (int i = 0; i < 512; i += 4)
    {
      x[i] = __builtin_popcountl(y[i]);
      x[i + 1] = __builtin_popcountl(y[i + 1]);
      x[i + 2] = __builtin_popcountl(y[i + 2]);
      x[i + 3] = __builtin_popcountl(y[i + 3]);
    }
}
