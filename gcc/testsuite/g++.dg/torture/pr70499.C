// { dg-do compile }
// { dg-additional-options "-w -Wno-psabi" }
// { dg-additional-options "-mavx" { target x86_64-*-* i?86-*-* } }

typedef double __m256d __attribute__ ((__vector_size__ (32), __may_alias__));

struct SIMD {
  __m256d data;
  SIMD() {};
  SIMD (double val) { }
  SIMD(__m256d _data) { data = _data; }
  SIMD operator* (SIMD a) { return a; }
};

struct Foo {
  SIMD val;
  SIMD dval[2];
  __attribute__((__always_inline__)) SIMD & Value() throw() { return val; }
  __attribute__((__always_inline__)) Foo operator* ( const Foo & y) throw() 
    {
      Foo res;
      SIMD hx;
      SIMD hy;
      res.Value() = hx*hy;
      res.dval[0] = hx*hy;
      return res;
    }
};

template<typename Tx>  
__attribute__((__always_inline__)) inline void inlineFunc(Tx hx[]) {
    Tx x = hx[0], y = hx[1];
    Tx lam[1] = (x*y);
}

void FooBarFunc () {
  Foo adp[2];
  inlineFunc (adp);
}
