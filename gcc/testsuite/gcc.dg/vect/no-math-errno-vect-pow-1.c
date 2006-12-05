/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

double x[256];

void foo(void)
{
  int i;
  for (i=0; i<256; ++i)
    x[i] = __builtin_pow (x[i], 0.5);
}

/* { dg-final { scan-tree-dump "pattern recognized" "vect" { xfail spu*-*-* } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
