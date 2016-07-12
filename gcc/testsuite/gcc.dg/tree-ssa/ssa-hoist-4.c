/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* From PR21485.  */

long
NumSift (long *array, int b, unsigned long k)
{
  if (b)
    if (array[k] < array[k + 1L])
      ++k;
  return array[k];
}

/* There should be only two loads left.  And the final value in the
   if (b) arm should be if-converted:
     tem1 = array[k];
     if (b)
       tem1 = MAX (array[k+1], tem1)
     return tem1;  */

/* { dg-final { scan-tree-dump-times "= \\*" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= PHI" 1 "optimized" } } */
