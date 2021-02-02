/* { dg-do assemble } */
/* { dg-options "-Os -fdump-tree-ivopts -save-temps -fno-tree-loop-distribute-patterns" } */

void
tr5 (short array[], int n)
{
  int x;
  if (n > 0)
    for (x = 0; x < n; x++)
      array[x] = 0;
}

/* { dg-final { scan-tree-dump-times "PHI <" 1 "ivopts"} } */
/* { dg-final { object-size text <= 20 { target { arm_thumb2_no_arm_v8_1_lob } } } } */
/* { dg-final { object-size text <= 32 { target { arm_nothumb && { ! arm_iwmmxt_ok } } } } } */
/* { dg-final { object-size text <= 36 { target { arm_nothumb && arm_iwmmxt_ok }  } } } */
