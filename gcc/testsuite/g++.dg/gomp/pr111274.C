// { dg-do "compile" }

// This example used to ICE in fixup_blocks_walker due to a BIND_EXPR with
// null BIND_EXPR_BLOCK.

struct _Vector_base {
  ~_Vector_base();
};
int ColumnSmallestLastOrdering_OMP_i_MaxNumThreads,
    ColumnSmallestLastOrdering_OMP_i_MaxDegree;
void ColumnSmallestLastOrdering_OMP() {
#pragma omp for
  for (int i = 0; i < ColumnSmallestLastOrdering_OMP_i_MaxNumThreads; i++)
    new _Vector_base[ColumnSmallestLastOrdering_OMP_i_MaxDegree];
}
