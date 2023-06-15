/* Ensure that the gimplifier does not remove any existing clauses as
   it inserts new implicit data clauses.  */

/* { dg-additional-options "-fdump-tree-gimple" }  */

#define N 100
static int a[N], b[N];

int main(int argc, char *argv[])
{
  int i;

#pragma acc data copyin(a[0:N]) copyout (b[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      b[i] = a[i];
  }

 return 0;
}

// { dg-final { scan-tree-dump-times "omp target oacc_data map\\(from:b\\\[0\\\] \\\[len: 400\\\]\\) map\\(to:a\\\[0\\\] \\\[len: 400\\\]\\)" 1 "gimple" } }
/* This isn't expected to work while the "lexical inheritance" support is
   reverted.  */
// { dg-final { scan-tree-dump-times "omp target oacc_parallel map\\(force_present:b\\\[0\\\] \\\[len: 400\\\]\\) map.alloc:b \\\[pointer assign, bias: 0\\\]\\) map\\(force_present:a\\\[0\\\] \\\[len: 400\\\]\\) map\\(alloc:a \\\[pointer assign, bias: 0\\\]\\)" 0 "gimple" } }
