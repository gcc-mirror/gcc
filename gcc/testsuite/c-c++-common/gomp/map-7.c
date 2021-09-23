/* { dg-do compile } */

void
foo (void)
{
  /* Test to ensure that the close modifier is parsed and ignored in map clauses. */

  #define N 1024
  int always[N];
  int close;

  #pragma omp target map(always[:N]) 
  ;

  #pragma omp target map(close, always[:N]) 
  ;

  #pragma omp target map(always[:N], close) 
  ;
}
