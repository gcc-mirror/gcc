/* { dg-do compile } */

struct S { int s; } s;

void
foo (void)
{
  #pragma omp parallel
  {
    #pragma omp cancel parallel if (s)	/* { dg-error "used struct type value where scalar is required" } */
  }
}
