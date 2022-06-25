/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-loop-vectorize -fno-tree-dominator-opts" } */
/* { dg-require-effective-target lp64 } */

double p[1000];
double q[1000];

void
f1 (double *p, double *q)
{
  p = (double *) __builtin_assume_aligned (p, sizeof (double) * 2);
  q = (double *) __builtin_assume_aligned (q, sizeof (double) * 2);
  for (unsigned int i = 2; i < ~0U - 4; i += 4)
    {
      double a = q[i + 2] + p[i + 2];
      double b = q[i + 3] + p[i + 3];
      q[i + 2] = a;
      q[i + 3] = b;
    }
}

void
f2 (double *p, double *q)
{
  p = (double *) __builtin_assume_aligned (p, sizeof (double) * 2);
  q = (double *) __builtin_assume_aligned (q, sizeof (double) * 2);
  for (unsigned int i = 0; i < ~0U - 3; i += 4)
    {
      double a = q[i + 4] + p[i + 4];
      double b = q[i + 5] + p[i + 5];
      q[i + 4] = a;
      q[i + 5] = b;
    }
}

void
f3 (double *p, double *q)
{
  p = (double *) __builtin_assume_aligned (p, sizeof (double) * 2);
  q = (double *) __builtin_assume_aligned (q, sizeof (double) * 2);
  for (unsigned int i = 0; i < 1000; i += 4)
    {
      double a = q[i - 2] + p[i - 2];
      double b = q[i - 1] + p[i - 1];
      q[i - 2] = a;
      q[i - 1] = b;
    }
}

void
f4 (double *p, double *q)
{
  p = (double *) __builtin_assume_aligned (p, sizeof (double) * 2);
  q = (double *) __builtin_assume_aligned (q, sizeof (double) * 2);
  for (unsigned int i = 2; i < 1000; i += 4)
    {
      double a = q[i - 4] + p[i - 4];
      double b = q[i - 3] + p[i - 3];
      q[i - 4] = a;
      q[i - 3] = b;
    }
}

/* { dg-final { scan-tree-dump-not "optimized: basic block" "slp1" } } */
