// { dg-do compile { target c++11 } }

template <typename> struct A {
  template <typename, > struct __construct_helper;  // { dg-error "expected" }
  template <typename... _Args>
  using __has_construct typename __construct_helper<_Args...>::type;  // { dg-error "expected" }
} struct : A<int> {   // { dg-error "expected" }
  // { dg-error "-:expected" "" { target *-*-* } .+1 }
