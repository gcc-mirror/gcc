// PR/99728
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-lim2-details -w -Wno-psabi" }

typedef double __m256d __attribute__((vector_size(sizeof (double) * 4)));
extern __inline __m256d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_set1_pd (double __A)
{
  return __extension__ (__m256d){ __A, __A, __A, __A };
}

// simple OO wrapper around __m256d
struct Tvsimple
  {
  __m256d v;
  Tvsimple &operator+=(const Tvsimple &other) {v+=other.v; return *this;}
  Tvsimple operator*(double val) const { Tvsimple res; res.v = v*_mm256_set1_pd(val); return res;}
  Tvsimple operator*(Tvsimple val) const { Tvsimple res; res.v = v*val.v; return res; }
  Tvsimple operator+(Tvsimple val) const { Tvsimple res; res.v = v+val.v; return res; }
  Tvsimple operator+(double val) const { Tvsimple res; res.v = v+_mm256_set1_pd(val); return res;}
  };

template<typename vtype> struct s0data_s
  { vtype sth, corfac, scale, lam1, lam2, csq, p1r, p1i, p2r, p2i; };

template<typename vtype> void foo(s0data_s<vtype> & __restrict__ d,
  const double * __restrict__ coef, const double * __restrict__ alm,
  unsigned long l, unsigned long il, unsigned long lmax)
  {
// critical loop
  while (l<=lmax)
    {
    d.p1r += d.lam2*alm[2*l];
    d.p1i += d.lam2*alm[2*l+1];
    d.p2r += d.lam2*alm[2*l+2];
    d.p2i += d.lam2*alm[2*l+3];
    Tvsimple tmp = d.lam2*(d.csq*coef[2*il] + coef[2*il+1]) + d.lam1;
    d.lam1 = d.lam2;
    d.lam2 = tmp;
    ++il; l+=2;
    }
  }

// this version has dead stores at the end of the loop
template void foo<>(s0data_s<Tvsimple> & __restrict__ d,
  const double * __restrict__ coef, const double * __restrict__ alm,
  unsigned long l, unsigned long il, unsigned long lmax);

// The aggregate copy in the IL should not prevent all store-motion
// { dg-final { scan-tree-dump-times "Executing store motion" 4 "lim2" } }
