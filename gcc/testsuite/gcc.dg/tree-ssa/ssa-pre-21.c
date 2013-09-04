/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

long
NumSift (long *array, unsigned long k)
{
  if (array[k] < array[k + 1L])
    ++k;
  return array[k];
}

/* There should be only two loads left.  */

/* { dg-final { scan-tree-dump-times "= \\\*\[^\n;\]*;" 2 "pre" { xfail { ! size32plus } } } } */ /* xfail: PR tree-optimization/58169 */
/* { dg-final { cleanup-tree-dump "pre" } } */
