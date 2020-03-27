// DR 1710 - Missing template keyword in class-or-decltype
// { dg-do compile { target c++14 } }

template <typename T> struct S {
  template<typename TT> struct A { };
  typedef A<int> type;
};
template <typename T> typename S<int>::template A<int> foo;
template <typename T> typename S<int>::template type foo2; // { dg-error "expected template-id" }
