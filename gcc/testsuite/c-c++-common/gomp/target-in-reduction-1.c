void
foo (int i, int j, int k)
{
  #pragma omp target in_reduction (+:i) private (i)	/* { dg-error "'i' appears more than once in data-sharing clauses" } */
  ;
  #pragma omp target private (i) in_reduction (+:i)	/* { dg-error "'i' appears both in data and map clauses" } */
  ;
  #pragma omp target in_reduction (+:i) firstprivate (i)	/* { dg-error "'i' appears more than once in data-sharing clauses" } */
  ;								/* { dg-error "'i' appears both in data and map clauses" "" { target *-*-* } .-1 } */
  #pragma omp target firstprivate (i) in_reduction (+:i)	/* { dg-error "'i' appears both in data and map clauses" } */
  ;
}
