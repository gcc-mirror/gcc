void test ()
{
#pragma omp tile sizes (2,4,6)
  for (unsigned i = 0; i < 10; i++) /* { dg-error "inner loops must be perfectly nested" { target c } } */
    for (unsigned j = 0; j < 10; j++)
      {
	float intervening_decl = 0; /* { dg-bogus "not enough for loops to collapse" "TODO C/C++ imperfect loop nest handling" { xfail c++ } } */

#pragma omp unroll partial(2)
	for (unsigned k = 0; k < 10; k++);
      }
}
