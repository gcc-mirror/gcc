/* { dg-additional-options "-fdump-tree-gimple-lineno" }  */

int
main (void)
{
  double a[10], a_h[10];
  int myId = -1;
#pragma omp target map(tofrom:a)
#pragma omp taskloop simd shared(a) lastprivate(myId) /* { dg-line here } */
    for(int i = 0 ; i < 10; i++) if (a[i] != a_h[i]) { }
}

/* { dg-final { scan-tree-dump-times "#pragma omp taskloop" 3 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "(?n)\\\[.*pr104968.c:[get-absolute-line '' here]:.*\\\] #pragma omp taskloop" 3 "gimple" } }  */
