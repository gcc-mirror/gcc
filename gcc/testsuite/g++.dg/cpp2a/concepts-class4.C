// PR c++/103341
// { dg-do compile { target c++20 } }

template<class T, class U> concept same_as = __is_same(T, U);
template<class T>
struct A {
  static inline same_as<T> auto value = 0; // { dg-error "constraint" }
};

template struct A<int>; // { dg-bogus "" }
template struct A<bool>; // { dg-message "required from here" }
