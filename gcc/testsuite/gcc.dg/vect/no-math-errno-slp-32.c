/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

double x[256];

void foo(void)
{
  int i;
  for (i=0; i<128; ++i)
   {
    x[2*i] = __builtin_pow (x[2*i], 0.5);
    x[2*i+1] = __builtin_pow (x[2*i+1], 0.5);
   }
}

/* { dg-final { scan-tree-dump "pattern recognized" "vect" { xfail spu*-*-* } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
