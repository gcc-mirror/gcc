// PR c++/71954
// A version of var-templ84.C where the partial specializations depend on
// outer template parameters.
// { dg-do compile { target c++14 } }

template<class T>
struct A {
  template<class U, class V> static const int var = 0;
  template<class U> static const int var<U*, T> = 1;
  template<class U> static const int var<const U*, T> = 2;
};

static_assert(A<int>::var<int, int> == 0, "");
static_assert(A<int>::var<int*, int> == 1, "");
static_assert(A<int>::var<const int*, int> == 2, "");

static_assert(A<int>::var<int, char> == 0, "");
static_assert(A<int>::var<int*, char> == 0, "");
static_assert(A<int>::var<const int*, char> == 0, "");
