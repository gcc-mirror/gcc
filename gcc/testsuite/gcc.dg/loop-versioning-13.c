/* { dg-options "-O3 -fdump-tree-lversion-details" } */

/* Test that we do version for a step of 1 when that would lead the
   iterations to access consecutive groups.  */

void
f1 (unsigned short *x, int stepx, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i * stepx * 2] = 100;
      x[i * stepx * 2 + 1] = 99;
    }
}

void
f2 (unsigned short *x, int stepx, int n)
{
  for (int i = 0; i < n; i += stepx * 2)
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
      x[i * stepx * 2 - 16] = 100;
      x[i * stepx * 2 - 15] = 99;
    }
}

void
f4 (unsigned short *x, int stepx, int n)
{
  for (int i = 0; i < n; i += stepx * 2)
    {
      x[i - 16] = 100;
      x[i - 15] = 99;
    }
}

void
f5 (unsigned short *x, int stepx, int n)
{
  for (unsigned short *y = x; y < x + n; y += stepx * 2)
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
      x[i * stepx * 2] = 100;
      x[i * stepx * 2 + 1] = 99;
    }
}

void
g2 (int stepx, int n)
{
  for (int i = 0; i < n; i += stepx * 2)
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
      x[i * stepx * 2 - 16] = 100;
      x[i * stepx * 2 - 15] = 99;
    }
}

void
g4 (int stepx, int n)
{
  for (int i = 0; i < n; i += stepx * 2)
    {
      x[i - 16] = 100;
      x[i - 15] = 99;
    }
}

void
g5 (int stepx, int n)
{
  for (unsigned short *y = x; y < x + n; y += stepx * 2)
    {
      y[0] = 100;
      y[1] = 99;
    }
}

/* { dg-final { scan-tree-dump-times {want to version containing loop} 10 "lversion" } } */
/* { dg-final { scan-tree-dump-times {versioned this loop} 10 "lversion" } } */
