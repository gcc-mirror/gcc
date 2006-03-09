// { dg-do compile }

void foo()
{
  int i;

  #pragma omp parallel default(none)		// { dg-error "enclosing" }
    {
    #pragma omp parallel
      {
      #pragma omp parallel default(none)	// { dg-error "enclosing" }
        {
 	  i++;					// { dg-error "not specified" }
	}
      }
    }
}
