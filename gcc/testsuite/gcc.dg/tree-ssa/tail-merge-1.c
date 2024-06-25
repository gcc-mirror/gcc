/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dce4" } */

void foo1 (int *restrict a, int *restrict b, int *restrict c,
	   int *restrict d, int *restrict res, int n)
{
  for (int i = 0; i < n; i++)
    res[i] = a[i] ? b[i] : (c[i] ? b[i] : d[i]);
}

/* After tail-merging (run during PRE) we should end up merging the two
   blocks dereferencing 'b', ending up with two iftmp assigns and the
   iftmp PHI def.  */
/* { dg-final { scan-tree-dump-times "iftmp\[^\r\n\]* = " 3 "dce4" } } */
