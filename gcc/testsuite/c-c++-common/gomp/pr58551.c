/* PR middle-end/58551 */
/* { dg-do compile } */
/* { dg-options "-O0 -fopenmp" } */

void
foo (int *a)
{
  int i;
  for (i = 0; i < 8; i++)
    #pragma omp task
    if (a[i])
      __builtin_abort ();
}

void bar (int, int);

void
baz (int *a)
{
  int i;
  for (i = 0; i < 8; i++)
    #pragma omp task
    if (a[i])
      {
	int j, k;
	for (j = 0; j < 10; j++)
	  for (k = 0; k < 8; k++)
	    bar (j, k);
	for (k = 0; k < 12; k++)
	  bar (-1, k);
	__builtin_abort ();
      }
}
