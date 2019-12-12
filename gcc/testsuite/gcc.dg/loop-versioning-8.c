/* { dg-options "-O3 -fdump-tree-lversion-details" } */

/* Versioning for step == 1 in these loops would allow loop interchange,
   but otherwise isn't worthwhile.  At the moment we decide not to version.  */

struct foo {
  int a[100];
};

void
f1 (struct foo *x, int step, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[j * step].a[i] = 100;
}

void
f2 (struct foo *x, int step, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[j].a[i * step] = 100;
}

void
f3 (struct foo *x, int step, int limit)
{
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < limit; j += step)
      x[j].a[i] = 100;
}

void
f4 (struct foo *x, int step, int limit)
{
  for (int i = 0; i < limit; i += step)
    for (int j = 0; j < 100; ++j)
      x[j].a[i] = 100;
}

/* { dg-final { scan-tree-dump-not {want to version} "lversion" } } */
/* { dg-final { scan-tree-dump-not {versioned} "lversion" } } */
