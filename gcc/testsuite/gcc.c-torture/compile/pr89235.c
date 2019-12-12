/* { dg-require-effective-target fopenmp } */
/* { dg-options "-S -fopenmp -fsave-optimization-record -ftree-parallelize-loops=2 -fno-tree-vectorize --param ggc-min-expand=0" } */

int a1, dr, xm, ly, zb, g9, il;

long int wt;
unsigned int mq;
int br, e7, rm, t4, jb, ry;

int
fi (void);

int
z5 (int fl)
{
  while (br < 1)
    while (e7 != 0)
      while (mq != 1)
        {
          if (!!fl)
            {
              wt = rm;
              fi ();
            }

          ++mq;
        }

  return 0;
}

void
gg (void)
{
  t4 = rm = z5 (rm);
  jb = z5 (rm);
  ry = fi ();
}

#pragma omp declare simd
void
hl (void)
{
  for (;;)
    {
      gg ();
      gg ();
      gg ();
    }
}

#pragma omp declare simd
int
cw (void)
{
  return 0;
}
