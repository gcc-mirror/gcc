// { dg-options "-fno-openmp" }

namespace A
{
  namespace omp_all_memory		// { dg-bogus "expected" }
  {
  }
}

namespace B
{
  template <int N>
  void omp_all_memory () {}		// { dg-bogus "expected" }
}

namespace C
{
  template <int N>
  struct omp_all_memory {};		// { dg-bogus "expected" }
}

namespace D
{
  template <int omp_all_memory>		// { dg-bogus "expected" }
  struct S {};
}
