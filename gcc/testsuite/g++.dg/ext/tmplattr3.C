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

template<bool> struct StaticAssert;
template<> struct StaticAssert<true> {};

StaticAssert<__alignof(X<double>) == __alignof(double)> d1;
StaticAssert<__alignof(Y<double>) == __alignof(double)> d2;
