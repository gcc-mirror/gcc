// PR c++/41967
// { dg-do compile }
// { dg-options "-fopenmp" }

int
foo ()
{
  int sum = 0;
#pragma omp for collapse(2)
  for (int i = 0; i < 5; ++i)
    {
      for (int j = 0; j < 5; ++j)
	++sum;
      ++sum;	// { dg-error "collapsed loops not perfectly nested" }
    }
  return sum;
}
