// P0846R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

struct S {
  template<typename T> int foo(T);
  template<typename T> int foo(T, T);
  template<typename T> int foo(T, T, T);
};

template<typename T>
struct W {
  template<typename U> T foo(U);
  template<typename U> T foo(U, U);
  template<typename U> T foo(U, U, U);
};

void
test ()
{
  S s;
  s.foo<int>(1);
  s.foo<int>(1, 2);
  s.foo<int>(1, 2, 3);

  W<int> w;
  w.foo<int>(1);
  w.foo<int>(1, 2);
  w.foo<int>(1, 2, 3);

  w.nothere<int>(1); // { dg-error "has no member|expected" }
  s.nothere<int>(1); // { dg-error "has no member|expected" }
}
