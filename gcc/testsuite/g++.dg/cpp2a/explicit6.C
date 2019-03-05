// P0892R2
// { dg-do compile }
// { dg-options "-std=c++2a" }

template<int T = 1>
struct S {
  explicit(T) operator int();
};

template<typename T, int N>
struct R {
  explicit(N) operator T();
};

template<typename T>
struct U {
  explicit((T) 1.0) operator T();
};

int
main ()
{
  S s;
  int i1 = s; // { dg-error "cannot convert" }
  S<true> s2;
  int i2 = s2; // { dg-error "cannot convert" }
  S<false> s3;
  int i3 = s3;
  int i4{s};
  int i5{s2};
  int i6{s3};

  R<int, true> r;
  int i7 = r; // { dg-error "cannot convert" }
  R<int, false> r2;
  int i8 = r2;

  U<int> u;
  int i9 = u; // { dg-error "cannot convert" }
  int i10{u};
}
