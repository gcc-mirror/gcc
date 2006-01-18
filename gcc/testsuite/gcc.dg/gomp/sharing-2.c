/* { dg-do compile } */

void
foo (void)
{
  int i;
  int a[10];
  #pragma omp parallel private (i) shared (a)
  {
    i = 1;
    #pragma omp parallel shared (a, i)
    {
      #pragma omp master
	i = 2;
      #pragma omp parallel private (i) shared (a)
      {
	for (i = 0; i < 10; i++)
	  a[i] = i + 1;
      }
      #pragma omp master
	i = 3;
    }
    i = 4;
  }
}
