/* { dg-do assemble } */
/* { dg-options "-Os -fdump-tree-ivopts -save-temps" } */

extern void foo2 (short*);

void
tr4 (short array[], int n)
{
  int x;
  if (n > 0)
    for (x = 0; x < n; x++)
      foo2 (&array[x]);
}

/* { dg-final { scan-tree-dump-times "PHI <ivtmp" 1 "ivopts"} } */
/* { dg-final { scan-tree-dump-times "PHI <" 1 "ivopts"} } */
/* { dg-final { object-size text <= 26 { target arm_thumb2 } } } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */
/* { dg-final { cleanup-saved-temps "ivopts" } } */
