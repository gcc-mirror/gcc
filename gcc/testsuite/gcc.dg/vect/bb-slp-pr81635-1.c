/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-loop-vectorize" } */
/* { dg-require-effective-target vect_double } */
/* { dg-require-effective-target lp64 } */

void
f1 (double *p, double *q)
{
  p = (double *) __builtin_assume_aligned (p, sizeof (double) * 2);
  q = (double *) __builtin_assume_aligned (q, sizeof (double) * 2);
  for (unsigned int i = 0; i < 1000; i += 4)
    {
      double a = q[i] + p[i];
      double b = q[i + 1] + p[i + 1];
      q[i] = a;
      q[i + 1] = b;
    }
}

void
f2 (double *p, double *q)
{
  p = (double *) __builtin_assume_aligned (p, sizeof (double) * 2);
  q = (double *) __builtin_assume_aligned (q, sizeof (double) * 2);
  for (unsigned int i = 2; i < ~0U - 4; i += 4)
    {
      double a = q[i] + p[i];
      double b = q[i + 1] + p[i + 1];
      q[i] = a;
      q[i + 1] = b;
    }
}

void
f3 (double *p, double *q)
{
  p = (double *) __builtin_assume_aligned (p, sizeof (double) * 2);
  q = (double *) __builtin_assume_aligned (q, sizeof (double) * 2);
  for (unsigned int i = 0; i < ~0U - 3; i += 4)
    {
      double a = q[i + 2] + p[i + 2];
      double b = q[i + 3] + p[i + 3];
      q[i + 2] = a;
      q[i + 3] = b;
    }
}

void
f4 (double *p, double *q)
{
  p = (double *) __builtin_assume_aligned (p, sizeof (double) * 2);
  q = (double *) __builtin_assume_aligned (q, sizeof (double) * 2);
  for (unsigned int i = 0; i < 500; i += 6)
    for (unsigned int j = 0; j < 500; j += 4)
      {
	double a = q[j] + p[i];
	double b = q[j + 1] + p[i + 1];
	q[i] = a;
	q[i + 1] = b;
      }
}

void
f5 (double *p, double *q)
{
  p = (double *) __builtin_assume_aligned (p, sizeof (double) * 2);
  q = (double *) __builtin_assume_aligned (q, sizeof (double) * 2);
  for (unsigned int i = 2; i < 1000; i += 4)
    {
      double a = q[i - 2] + p[i - 2];
      double b = q[i - 1] + p[i - 1];
      q[i - 2] = a;
      q[i - 1] = b;
    }
}

double p[1000];
double q[1000];

void
f6 (int n)
{
  for (unsigned int i = 0; i < n; i += 4)
    {
      double a = q[i] + p[i];
      double b = q[i + 1] + p[i + 1];
      q[i] = a;
      q[i + 1] = b;
    }
}

/* { dg-final { scan-tree-dump-times "optimized: basic block" 6 "slp1" } } */
