/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Check that when the variant has an empty parameter list, the host-to-device 
   pointer conversion still happens.  */

void variant_fn();  // Assume C < C23; in C++/C23 it becomes the same as '…(void)'.

#pragma omp declare variant(variant_fn) match(construct={dispatch}) adjust_args(need_device_ptr: x,y)
void bar(int *x, int *y);

void sub(int *x, int *y)
{
  #pragma omp dispatch is_device_ptr(y)
     bar(x, y);
}

/* { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(x, D\\.\[0-9\]+\\);" 1 "gimple" } } */
