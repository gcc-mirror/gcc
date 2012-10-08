// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-O3 -msse2 -std=c++11" }
// { dg-require-effective-target sse2 }

// You can make NON-template typedefs with a large alignment.
typedef double AlignedDoubleType [[gnu::aligned(16)]];

template <typename RealType>
RealType f(const RealType* p)
{
  // But if you use a template parameter it complains.
  typedef RealType AlignedRealType [[gnu::aligned(16)]];

  return p[0];
}

double f2(const double* p)
{
  return f<double>(p);
}
