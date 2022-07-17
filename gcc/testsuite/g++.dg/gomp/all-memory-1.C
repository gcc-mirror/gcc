namespace A
{
  namespace omp_all_memory		// { dg-error "expected" }
  {
  }
}

namespace B
{
  template <int N>
  void omp_all_memory () {}		// { dg-error "expected" }
}

namespace C
{
  template <int N>
  struct omp_all_memory {};		// { dg-error "expected" }
}

namespace D
{
  template <int omp_all_memory>		// { dg-error "expected" }
  struct S {};
}
