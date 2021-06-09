// PR c++/100957
// { dg-do compile }

struct S {
  S ()
  {
  #pragma omp for ordered(2)
    for (int i = 0; i < 32; ++i)
      for (int j = 0; j < 32; ++j)
	{
	#pragma omp ordered depend(source)
	  ;
	#pragma omp ordered depend(sink: i - 1, j - 1)
	}
  }
};
