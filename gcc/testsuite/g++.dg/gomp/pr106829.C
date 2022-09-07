// PR c++/106829

namespace std
{
  template <typename> class complex;
  template <> struct complex<double> { complex (double); _Complex double d; };
}
struct S { void static foo (); };

void
S::foo ()
{
#pragma omp target
  std::complex<double> c = 0.0;
}
