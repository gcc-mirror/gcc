/* Test prefetch support.  */
/* { dg-options "-mcpu=fr400" } */
/* { dg-do compile } */

void foo (void *x)
{
  __data_prefetch0 (x);
}

/* { dg-final { scan-assembler "\tdcpl " } } */
