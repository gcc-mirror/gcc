/* { dg-options "-O3 -fdump-tree-lversion-details" } */
/* { dg-require-effective-target size20plus } */

/* Versioning for step == 1 in these loops would allow loop interchange,
   but otherwise isn't worthwhile.  At the moment we decide not to version.  */

void
f1 (double x[][100], int step, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[j * step][i] = 100;
}

void
f2 (double x[][100], int step, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[j][i * step] = 100;
}

void
f3 (double x[][100], int step, int limit)
{
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < limit; j += step)
      x[j][i] = 100;
}

void
f4 (double x[][100], int step, int limit)
{
  for (int i = 0; i < limit; i += step)
    for (int j = 0; j < 100; ++j)
      x[j][i] = 100;
}

double x[100][100];

void
g1 (int step, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[j * step][i] = 100;
}

void
g2 (int step, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[j][i * step] = 100;
}

void
g3 (int step, int limit)
{
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < limit; j += step)
      x[j][i] = 100;
}

void
g4 (int step, int limit)
{
  for (int i = 0; i < limit; i += step)
    for (int j = 0; j < 100; ++j)
      x[j][i] = 100;
}

/* { dg-final { scan-tree-dump-not {want to version} "lversion" } } */
/* { dg-final { scan-tree-dump-not {versioned} "lversion" } } */
