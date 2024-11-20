/* { dg-do compile } */
/* { dg-additional-options "-std=c23" } */


void variant_fn(int *x, int *y, int *z);

[[omp::directive (declare variant(variant_fn) match(construct={dispatch}) adjust_args(need_device_ptr: x,y) adjust_args(nothing: z))]]
void bar(int *x, int *y, int *z);

void sub(int *x, int *y, int *z)
{
  [[omp::directive (dispatch is_device_ptr(y))]]
     bar(x, y, z);
  [[omp::directive (dispatch device(0))]]
     bar(x, y, z);
  [[omp::directive (dispatch nocontext(1) novariants(1))]]
     bar(x, y, z);
  [[omp::directive (dispatch depend(inout: x) nowait)]]
     bar(x, y, z);
}

