// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-O3 -msse2" }

// You can make NON-template typedefs with a large alignment.
typedef double AlignedDoubleType __attribute__((aligned(16)));

template <typename RealType>
RealType f(const RealType* p)
{
  // But if you use a template parameter it complains.
  typedef RealType AlignedRealType __attribute__((aligned(16)));

  return p[0];
}

double f2(const double* p)
{
  return f<double>(p);
}
