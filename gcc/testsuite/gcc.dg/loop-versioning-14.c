/* { dg-options "-O3 -fdump-tree-lversion-details" } */

/* Test that we don't try to version for a step of 1 when that would
   cause the iterations to leave a gap between accesses.  */

void
f1 (unsigned short *x, int stepx, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i * stepx * 4] = 100;
      x[i * stepx * 4 + 1] = 99;
    }
}

void
f2 (unsigned short *x, int stepx, int n)
{
  for (int i = 0; i < n; i += stepx * 4)
    {
      x[i] = 100;
      x[i + 1] = 99;
    }
}

void
f3 (unsigned short *x, int stepx, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i * stepx * 4 - 16] = 100;
      x[i * stepx * 4 - 15] = 99;
    }
}

void
f4 (unsigned short *x, int stepx, int n)
{
  for (int i = 0; i < n; i += stepx * 4)
    {
      x[i - 16] = 100;
      x[i - 15] = 99;
    }
}

void
f5 (unsigned short *x, int stepx, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i * stepx * 64 - 16] = 100;
      x[i * stepx * 64 + 15] = 99;
    }
}

void
f6 (unsigned short *x, int stepx, int n)
{
  for (int i = 0; i < n; i += stepx * 64)
    {
      x[i - 16] = 100;
      x[i + 15] = 99;
    }
}

void
f7 (unsigned short *x, int stepx, int n)
{
  for (unsigned short *y = x; y < x + n; y += stepx * 4)
    {
      y[0] = 100;
      y[1] = 99;
    }
}

unsigned short x[1000];

void
g1 (int stepx, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i * stepx * 4] = 100;
      x[i * stepx * 4 + 1] = 99;
    }
}

void
g2 (int stepx, int n)
{
  for (int i = 0; i < n; i += stepx * 4)
    {
      x[i] = 100;
      x[i + 1] = 99;
    }
}

void
g3 (int stepx, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i * stepx * 4 - 16] = 100;
      x[i * stepx * 4 - 15] = 99;
    }
}

void
g4 (int stepx, int n)
{
  for (int i = 0; i < n; i += stepx * 4)
    {
      x[i - 16] = 100;
      x[i - 15] = 99;
    }
}

void
g5 (int stepx, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i * stepx * 64 - 16] = 100;
      x[i * stepx * 64 + 15] = 99;
    }
}

void
g6 (int stepx, int n)
{
  for (int i = 0; i < n; i += stepx * 64)
    {
      x[i - 16] = 100;
      x[i + 15] = 99;
    }
}

void
g7 (int stepx, int n)
{
  for (unsigned short *y = x; y < x + n; y += stepx * 4)
    {
      y[0] = 100;
      y[1] = 99;
    }
}

/* { dg-final { scan-tree-dump-not {want to version} "lversion" } } */
/* { dg-final { scan-tree-dump-not {versioned} "lversion" } } */
