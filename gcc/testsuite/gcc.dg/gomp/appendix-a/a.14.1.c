/* { dg-do compile } */

void
a14 ()
{
  int i = 1;
#pragma omp parallel sections
  {
#pragma omp section
    {
#pragma omp critical (name)
      {
#pragma omp parallel
	{
#pragma omp single
	  {
	    i++;
	  }
	}
      }
    }
  }
}
