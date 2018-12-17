/* { dg-options "-O3 -fdump-tree-lversion-details" } */

/* Check that versioning can handle arrays of structures.  */

struct foo {
  int a, b, c;
};

void
f1 (struct foo *x, int stepx, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[stepx * i].a = 1;
      x[stepx * i].b = 2;
      x[stepx * i].c = 3;
    }
}

void
f2 (struct foo *x, int stepx, int limit)
{
  for (int i = 0; i < limit; i += stepx)
    {
      x[i].a = 1;
      x[i].b = 2;
      x[i].c = 3;
    }
}

/* { dg-final { scan-tree-dump-times {want to version containing loop} 2 "lversion" } } */
/* { dg-final { scan-tree-dump-times {versioned this loop} 2 "lversion" } } */
