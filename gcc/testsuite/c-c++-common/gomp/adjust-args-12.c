/* { dg-additional-options "-fdump-tree-gimple" }  */

/* Valid constant-expressions in numeric ranges.  */

void v (int *, int *, int *, int *, int *) {}

#pragma omp declare variant(v) match(construct={dispatch}) \
			       adjust_args(need_device_ptr: 5-4:6-4, 4)
void b (int *, int *, int *, int *, int *) {}

void f (int *p0, int *p1, int *p2, int *p3, int *p4)
{
  #pragma omp dispatch
  b (p0, p1, p2, p3, p4);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p0, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p1, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p3, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v \\(D\.\[0-9\]+, D\.\[0-9\]+, p2, D\.\[0-9\]+, p4\\);" "gimple" } }  */
}
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 1 "gimple" } }  */
