// { dg-do compile }

void foo()
{
  int i, j;
  for (i = 0; i < 10; ++i)
    {
      #pragma omp for
      for (j = ({ continue; 0; }); // { dg-error "invalid branch to/from OpenMP structured block" }
	   j < ({ continue; 10; }); // { dg-error "invalid branch to/from OpenMP structured block" }
	   j += ({ continue; 1; })) // { dg-error "invalid branch to/from OpenMP structured block" }
	continue;

      #pragma omp for
      for (j = ({ break; 0; }); // { dg-error "invalid branch to/from OpenMP structured block" }
	   j < ({ break; 10; }); // { dg-error "invalid branch to/from OpenMP structured block" }
	   j += ({ break; 1; })) // { dg-error "invalid branch to/from OpenMP structured block" }
	break;				// { dg-error "break" }
    }
}
