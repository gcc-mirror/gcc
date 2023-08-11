// PR c++/71954
// { dg-do compile { target c++14 } }

struct A {
  template<class T> static const int var = 0;
  template<class T> static const int var<T*> = 1;
  template<class T> static const int var<const T*> = 2;
};

static_assert(A::var<int> == 0, "");
static_assert(A::var<int*> == 1, "");
static_assert(A::var<const int*> == 2, "");
