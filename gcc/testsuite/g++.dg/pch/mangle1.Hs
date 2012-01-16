// PR c++/51827
// { dg-options "-flto -std=c++0x" }

template<typename X> struct S { };
template<typename Y> struct T {
  template <typename ... A> T(S <A ...>);
};
inline void f(T<int>) { }
