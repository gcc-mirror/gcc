/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#define N 100

void
f (int x[], int y[], int z[])
{
  int i;

  #pragma omp target map(to: x, y) map(from: z)  /* { dg-bogus "'target' construct with nested 'teams' construct contains directives outside of the 'teams' construct" "PR118694" { xfail offload_nvptx } }  */ 
    #pragma omp metadirective \
	when (device={arch("nvptx")}: teams loop) \
	default (parallel loop)
      for (i = 0; i < N; i++)
	z[i] = x[i] * y[i];
}

/* If offload device "nvptx" isn't supported, the front end can eliminate
   that alternative and not produce a metadirective at all.  Otherwise this
   won't be resolved until late.  */
/* { dg-final { scan-tree-dump-not "#pragma omp metadirective" "gimple" } } */
/* { dg-final { scan-tree-dump-not " teams" "gimple" { target { ! offload_nvptx } } } } */
/* { dg-final { scan-tree-dump "variant.\[0-9\]+ = \\\[omp_next_variant\\\] OMP_NEXT_VARIANT <0,\[\r\n \]+construct context = 14\[\r\n \]+1: device = \\{arch \\(.nvptx.\\)\\}\[\r\n \]+2: >;" "gimple" { target { offload_nvptx } } } } */
