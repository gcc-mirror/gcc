/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops -fdump-tree-cunroll-details" } */

__attribute__ ((pure))
int bla(int);

int foo(void)
{
  int i;
  int sum;

  /* This loop used to appear to be too large for unrolling.  */
  for (i = 0; i < 4; i++)
    {
      sum += bla (i);
      sum += bla (2*i);
      sum += bla (3*i);
      sum += bla (4*i);
      sum += bla (5*i);
      sum += bla (6*i);
      sum += bla (7*i);
      sum += bla (8*i);
    }
  return sum;
}

/* { dg-final { scan-tree-dump-times "Completely unroll loop 3 times" 1 "cunroll" } } */

/* { dg-final { cleanup-tree-dump "cunroll" } } */
