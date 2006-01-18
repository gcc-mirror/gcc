/* { dg-do compile } */

void
a27 ()
{
  int i, a;
#pragma omp parallel private(a)
  {
#pragma omp parallel for private(a)
    for (i = 0; i < 10; i++)
      {
	/* do work here */
      }
  }
}
