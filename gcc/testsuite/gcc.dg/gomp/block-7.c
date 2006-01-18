// { dg-do compile }

void foo()
{
  int i, j;
  for (i = 0; i < 10; ++i)
    {
      #pragma omp for
      for (j = ({ continue; 0; });	// { dg-error "invalid exit" }
	   j < ({ continue; 10; });	// { dg-error "invalid exit" }
	   j += ({ continue; 1; }))	// { dg-error "invalid exit" }
	continue;

      #pragma omp for
      for (j = ({ break; 0; });		// { dg-error "invalid exit" }
	   j < ({ break; 10; });	// { dg-error "invalid exit" }
	   j += ({ break; 1; }))	// { dg-error "invalid exit" }
	break;				// { dg-error "break" }
    }
}
