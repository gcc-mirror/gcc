/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-math-errno -fdump-tree-vect-details" } */

double x[256];

void foo(void)
{
  int i;
  for (i=0; i<256; ++i)
    x[i] = __builtin_pow (x[i], 0.5);
}

/* { dg-final { scan-tree-dump "pattern recognized" "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
