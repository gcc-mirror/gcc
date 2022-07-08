/* { dg-do compile } */
/* { dg-options "-fopenmp -O2 -fdump-tree-omplower" } */ 

/* Ensure that the omp_max_vf, omp_max_simt_vf, and omp_max_simd_vf are working
   properly to set the OpenMP vectorization factor for the offload target, and
   not just for the host.  */

float
foo (float * __restrict x, float * __restrict y)
{
  float sum = 0.0;

#pragma omp target teams distribute parallel for simd map(tofrom: sum) reduction(+:sum)
  for (int i=0; i<1024; i++)
    sum += x[i] * y[i];

  return sum;
}

/* { dg-final { scan-tree-dump  "safelen\\(64\\)" "omplower" { target amdgcn_offloading_enabled } } } */
/* { dg-final { scan-tree-dump  "safelen\\(32\\)" "omplower" { target { { nvptx_offloading_enabled } && { ! amdgcn_offloading_enabled } } } } } */
