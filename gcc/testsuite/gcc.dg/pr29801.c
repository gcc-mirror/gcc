/* We used to crash in ccp here, because the initial constant value of 2
   was changed to 5.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

static const int a = 2;

int test (int param)
{
  int *p = (int *) &a;

  if (param)
    *p = 5;

  return a;
}

/* Check that we return the correct (unchanged) value.  */

/* { dg-final { scan-tree-dump-times "return 2" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 5" 0 "optimized" } } */

