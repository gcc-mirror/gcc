/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -fdump-tree-vect-details" } */

double x[256];

void foo(void)
{
  int i;
  for (i=0; i<256; ++i)
    x[i] = x[i] * x[i];
}

/* { dg-final { scan-tree-dump "pattern recognized" "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
