// { dg-do compile }

void foo()
{
  int i, j;
  for (i = 0; i < 10; ++i)
    {
      #pragma omp for
      for (j = ({ continue; 0; });	// { dg-error "invalid branch" }
	   j < ({ continue; 10; });	// { dg-error "invalid branch" }
	   j += ({ continue; 1; }))	// { dg-error "invalid branch" }
	continue;

      #pragma omp for
      for (j = ({ break; 0; });		// { dg-error "invalid branch" }
	   j < ({ break; 10; });	// { dg-error "invalid branch" }
	   j += ({ break; 1; }))	// { dg-error "invalid branch" }
	break;				// { dg-error "break" }
    }
}
