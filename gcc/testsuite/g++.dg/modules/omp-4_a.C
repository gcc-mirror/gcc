// PR c++/119864
// { dg-additional-options "-fmodules -fopenmp" }
// { dg-module-cmi p1 }

export module p1;

export
template<unsigned>
struct T
{
  double d;

  T &operator +=(T const &x) { d += x.d; return *this; }
};

export
template<unsigned d>
T<d> sum(T<d> const *p, unsigned N)
{
T<d> Sum = {};

#pragma omp declare reduction(Op: T<d>: omp_out += omp_in) initializer(omp_priv = {})
#pragma omp parallel for reduction(Op: Sum)
for (unsigned i = 0u; i < N; ++i)
  {
  Sum += *p;
  ++p;
  }
return Sum;
}
