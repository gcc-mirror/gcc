// PR c++/17743

template<typename T>
struct X {
  typedef char layout_type[sizeof(T)]
  __attribute ((aligned(__alignof(double))));
  layout_type data;
};

template<typename T>
struct Y {
  typedef char layout_type[sizeof(T)]
  __attribute ((aligned(__alignof(T))));
  layout_type data;
};

template<typename T>
struct Z {
  typedef char layout_type[sizeof(T)]
  __attribute ((aligned(__alignof(T))));
  struct Z2 {
    layout_type data;
  } in;
};

template<typename T>
struct A;

template <typename T>
struct A<T*> {
  typedef char layout_type[sizeof(T)]
  __attribute ((aligned(__alignof(T))));
  layout_type data;
};

template<bool> struct StaticAssert;
template<> struct StaticAssert<true> {};

StaticAssert<__alignof(X<double>) == __alignof(double)> d1;
StaticAssert<__alignof(Y<double>) == __alignof(double)> d2;
StaticAssert<__alignof(Z<double>) == __alignof(double)> d3;
StaticAssert<__alignof(A<double*>) == __alignof(double)> d4;
