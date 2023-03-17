/* PR middle-end/108685 */
/* { dg-do compile } */

void
foo (int a)
{
  for (int m = 0; m < 10; m++)
    #pragma omp for collapse(2) ordered(4)
    for (int i = 0; i < 2; i++)
      for (int j = 0; j < a; j++)
	for (int k = 0; k < 2; k++)
	  for (int l = 0; l < a; l++)
	    {
	      #pragma omp ordered depend (source)
	      __builtin_abort ();
	    }
}
