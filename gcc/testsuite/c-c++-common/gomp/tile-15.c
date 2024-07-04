/* It isn't really clear what is supposed to be valid and what isn't when mixing
   imperfectly nested loops with generated loops.  Sorry for now until that is
   clarified.  */
void foo (int, int);

void
bar (void)
{
  #pragma omp for collapse(2)		/* { dg-message "imperfectly nested loop using generated loops" "" { target c } } */
  for (int i = 0; i < 32; ++i)		/* { dg-message "imperfectly nested loop using generated loops" "" { target c++ } } */
    {
      foo (i, -1);
      #pragma omp tile sizes (2)
      for (int j = 0; j < 32; ++j)
	foo (i, j);
      foo (i, -2);
    }
}

void
baz (void)
{
  #pragma omp for collapse(2)		/* { dg-message "imperfectly nested loop using generated loops" "" { target c } } */
  for (int i = 0; i < 32; ++i)		/* { dg-message "imperfectly nested loop using generated loops" "" { target c++ } } */
    {
      foo (i, -1);
      #pragma omp tile sizes (2, 2)
      for (int j = 0; j < 32; ++j)
	#pragma omp tile sizes (2, 2)
	for (int k = 0; k < 32; ++k)
	  for (int l = 0; l < 32; ++l)
	    foo (i + k, j + l);
      foo (i, -2);
    }
}

void
qux (void)
{
  #pragma omp for collapse(2)		/* { dg-message "imperfectly nested loop using generated loops" "" { target c } } */
  for (int i = 0; i < 32; ++i)		/* { dg-message "imperfectly nested loop using generated loops" "" { target c++ } } */
    {
      int m = i + 6;
      foo (i, -1);
      #pragma omp tile sizes (2)
      for (int j = m; j < 32; ++j)
	foo (i, j);
      foo (i, -2);
    }
}

void
freddy (void)
{
  #pragma omp for collapse(2)		/* { dg-message "imperfectly nested loop using generated loops" "" { target c } } */
  for (int i = 0; i < 32; ++i)		/* { dg-message "imperfectly nested loop using generated loops" "" { target c++ } } */
    {
      int m = i + 6;
      foo (i, -1);
      #pragma omp tile sizes (2, 2)
      for (int j = 0; j < 32; ++j)
	#pragma omp tile sizes (2, 2)
	for (int k = 0; k < 32; ++k)
	  for (int l = m; l < 32; ++l)
	    foo (i + k, j + l);
      foo (i, -2);
    }
}
