// P0892R2
// { dg-do compile }
// { dg-options "-std=c++2a" }

template<int T = 1>
struct S {
  explicit(T) S(int);
  explicit(!T) S(int, int);
};

template<typename T, int N>
struct S2 {
  explicit(N) S2(T);
};

template<typename T>
struct S3 {
  explicit((T) 1.0) S3(int);
};

int
main ()
{
  S<> s1 = { 1 }; // { dg-error "converting" }
  S<true> s2 = { 1 }; // { dg-error "converting" }
  S<false> s3 = { 1 };
  S<> s4{ 1 };
  S<true> s5{ 1 };
  S<> s6 = { 1, 2 };
  S<true> s7 = { 1, 2 };
  S<false> s8 = { 1, 2 }; // { dg-error "converting" }
  S<false> s9{ 1, 2 };

  const int x = 1;
  S<x> s10 = { 1 }; // { dg-error "converting" }
  S<x> s11{ 2 };

  S2<int, true> s12 = { 1 }; // { dg-error "converting" }

  S3<int> s13 = { 1 }; // { dg-error "converting" }
}
