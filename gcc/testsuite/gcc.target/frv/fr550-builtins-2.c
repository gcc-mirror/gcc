/* Test prefetch support.  */
/* { dg-do compile } */

void foo (void *x)
{
  __data_prefetch0 (x);
}

/* { dg-final { scan-assembler "\tdcpl " } } */
