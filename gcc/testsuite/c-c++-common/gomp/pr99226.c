/* PR fortran/99226 */
/* { dg-do compile } */

void
foo (int n)
{
  int i;
  #pragma omp target	/* { dg-error "construct with nested 'teams' construct contains directives outside of the 'teams' construct" } */
  {
    #pragma omp teams distribute dist_schedule (static, n + 4)
    for (i = 0; i < 8; i++)
      ;
    #pragma omp teams distribute dist_schedule (static, n + 4)
    for (i = 0; i < 8; i++)
      ;
  }
}
