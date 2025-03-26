// { dg-do compile }

void foo()
{
  int i;

  #pragma omp parallel default(none)		// { dg-message "note: enclosing 'parallel'" }
    {
    #pragma omp parallel
      {
      #pragma omp parallel default(none)	// { dg-message "note: enclosing 'parallel'" }
        {
 	  i++;					// { dg-error "not specified" }
	}
      }
    }
}
