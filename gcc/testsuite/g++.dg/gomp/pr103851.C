// { dg-do compile }
// { dg-options "-fopenmp -ftrivial-auto-var-init=pattern" }

struct _Deque_base {
    long _M_map_size;
    int *_M_start;
    int *_M_finish;
};
void morphologicalFilter1D()
{
#pragma omp parallel
  {
    struct _Deque_base vals[4];
  }
}
