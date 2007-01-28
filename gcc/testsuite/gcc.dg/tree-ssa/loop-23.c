/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops -fdump-tree-cunroll-details" } */

void bla(int);

void foo(void)
{
  int i;

  /* This loop used to appear to be too large for unrolling.  */
  for (i = 0; i < 4; i++)
    {
      bla (i);
      bla (2*i);
      bla (3*i);
      bla (4*i);
      bla (5*i);
      bla (6*i);
      bla (7*i);
      bla (8*i);
    }
}

/* { dg-final { scan-tree-dump-times "Unrolled loop 1 completely" 1 "cunroll" } } */

/* { dg-final { cleanup-tree-dump "cunroll" } } */
