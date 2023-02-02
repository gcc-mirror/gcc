/* PR middle-end/108435 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp" } */

int
main ()
{
  int i, j;
  void
  bar (void)
  {
    #pragma omp for simd collapse(2)
     for (i = 1; i <= 16; i++)
       for (j = 1; j <= 16; j++)
	 ;
  }
  bar ();
}
