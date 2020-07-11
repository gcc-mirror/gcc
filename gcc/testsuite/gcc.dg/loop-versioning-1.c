/* { dg-options "-O3 -fdump-tree-lversion-details" } */
/* { dg-require-effective-target size32plus } */

/* The simplest IV case.  */

void
f1 (double *x, int stepx, int n)
{
  for (int i = 0; i < n; ++i)
    x[stepx * i] = 100;
}

void
f2 (double *x, int stepx, int limit)
{
  for (int i = 0; i < limit; i += stepx)
    x[i] = 100;
}

void
f3 (double *x, int stepx, int limit)
{
  for (double *y = x; y < x + limit; y += stepx)
    *y = 100;
}

void
f4 (double *x, int stepx, unsigned int n)
{
  for (unsigned int i = 0; i < n; ++i)
    x[stepx * i] = 100;
}

void
f5 (double *x, int stepx, unsigned int limit)
{
  for (unsigned int i = 0; i < limit; i += stepx)
    x[i] = 100;
}

void
f6 (double *x, int stepx, unsigned int limit)
{
  for (double *y = x; y < x + limit; y += stepx)
    *y = 100;
}

#if __SIZEOF_SIZE_T__ < 4
double x[1000];
#else
double x[10000];
#endif

void
g1 (int stepx, int n)
{
  for (int i = 0; i < n; ++i)
    x[stepx * i] = 100;
}

void
g2 (int stepx, int limit)
{
  for (int i = 0; i < limit; i += stepx)
    x[i] = 100;
}

void
g3 (int stepx, int limit)
{
  for (double *y = x; y < x + limit; y += stepx)
    *y = 100;
}

void
g4 (int stepx, unsigned int n)
{
  for (unsigned int i = 0; i < n; ++i)
    x[stepx * i] = 100;
}

void
g5 (int stepx, unsigned int limit)
{
  for (unsigned int i = 0; i < limit; i += stepx)
    x[i] = 100;
}

void
g6 (int stepx, unsigned int limit)
{
  for (double *y = x; y < x + limit; y += stepx)
    *y = 100;
}

/* { dg-final { scan-tree-dump-times {want to version containing loop} 12 "lversion" } } */
/* { dg-final { scan-tree-dump-times {versioned this loop} 12 "lversion" } } */
