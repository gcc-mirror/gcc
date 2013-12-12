/* PR libgomp/59467 */

int v;

void
foo (void)
{
  int x = 0, y = 0;
  #pragma omp parallel
  {
    int z;
    #pragma omp single copyprivate (x)	/* { dg-error "is not threadprivate or private in outer context" } */
    {
      #pragma omp atomic write
	x = 6;
    }
    #pragma omp atomic read
    z = x;
    #pragma omp atomic
    y += z;
  }
  #pragma omp parallel
  {
    int z;
    #pragma omp single copyprivate (v)	/* { dg-error "is not threadprivate or private in outer context" } */
    {
      #pragma omp atomic write
	v = 6;
    }
    #pragma omp atomic read
    z = v;
    #pragma omp atomic
    y += z;
  }
  #pragma omp parallel private (x)
  {
    int z;
    #pragma omp single copyprivate (x)
    {
      #pragma omp atomic write
	x = 6;
    }
    #pragma omp atomic read
    z = x;
    #pragma omp atomic
    y += z;
  }
  x = 0;
  #pragma omp parallel reduction (+:x)
  {
    #pragma omp single copyprivate (x)
    {
      #pragma omp atomic write
	x = 6;
    }
    #pragma omp atomic
    y += x;
  }
  #pragma omp single copyprivate (x)
  {
    x = 7;
  }
  #pragma omp single copyprivate (v)	/* { dg-error "is not threadprivate or private in outer context" } */
  {
    #pragma omp atomic write
      v = 6;
  }
}
