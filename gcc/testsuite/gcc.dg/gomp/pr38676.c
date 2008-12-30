/* PR middle-end/38676 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int
main ()
{
  int bar, foo = 1;
#pragma omp parallel for shared(foo)
  for (bar = 0; bar < 3; bar++)
    {
      switch (foo)
	{
	case 1:
	  break;
	}
    }
  return 0;
}
