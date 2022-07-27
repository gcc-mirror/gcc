/* { dg-do compile } */

void
foo ()
{
  int f = 0;
  #pragma omp scope firstprivate(f)	/* { dg-error "firstprivate variable 'f' is private in outer context" } */
  f++;
}
