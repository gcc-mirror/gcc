/* PR middle-end/102504 */
/* { dg-do compile } */

int
foo ()
{
  int r = 0;
  #pragma omp scope reduction(+:r)	/* { dg-error "reduction variable 'r' is private in outer context" } */
  r++;
  return r;
}
