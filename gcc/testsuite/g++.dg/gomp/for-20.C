// PR c++/60146
// { dg-do compile }
// { dg-options -fopenmp }

int foo() { return 0; }

template<typename T> void bar()
{
#pragma omp parallel for
  for (T i = foo(); i < 8; ++i) {}
}

void baz()
{
  bar<int>();
}
