void test ()
{
#pragma omp tile sizes (2,4,6)
  for (unsigned i = 0; i < 10; i++)
    for (unsigned j = 0; j < 10; j++)
      {
	float intervening_decl = 0; /* { dg-bogus "not enough for loops to collapse" "TODO C/C++ imperfect loop nest handling" { xfail c++ } } */
	/* { dg-bogus "not enough perfectly nested loops" "TODO C/C++ imperfect loop nest handling" { xfail c } .-1 } */
#pragma omp unroll partial(2)
	for (unsigned k = 0; k < 10; k++);
      }
}
