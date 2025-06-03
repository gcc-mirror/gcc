  /* { dg-do compile } */
  /* { dg-options "-O2 -ftree-vectorize -fdump-tree-ifcvt-stats" } */

void
test (int *dst, float *arr, int *pred, int n)
{
  for (int i = 0; i < n; i++)
    {
      int pred_i = pred[i];
      float arr_i = arr[i];

      dst[i] = pred_i ? (int)arr_i : 5;
    }
}

/* We expect this to fail if_convertible_loop_p so long as we have no
   conditional IFN for FIX_TRUNC_EXPR.  */

/* { dg-final { scan-tree-dump-times "Applying if-conversion" 0 "ifcvt" } } */
