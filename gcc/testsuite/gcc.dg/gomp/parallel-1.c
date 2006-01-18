// { dg-do compile }

void foo()
{
  int i;

  #pragma omp parallel
    {
    #pragma omp parallel
      {
      #pragma omp parallel
        {
 	  i++;
	}
      }
    }
}
