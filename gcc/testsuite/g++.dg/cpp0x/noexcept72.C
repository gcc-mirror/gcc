// PR c++/101715
// { dg-do compile { target c++11 } }

template <typename T> struct S {
  S<T> bar() noexcept(T::value);  // #1
  S<T> foo() noexcept(T::value);  // #2
};

template <typename T> S<T> S<T>::foo() noexcept(T::value) {}  // #3

template <typename T> struct S2 {
  S2<T> bar1() noexcept(T::value);
  S2<T> bar2() noexcept(T::value);
  S2<T> bar3() noexcept(T::value);
  S2<T> bar4() noexcept(T::value);
  S2<T> bar5() noexcept(T::value);
  S2<T> baz() noexcept(T::value2);
  S2<T> foo() noexcept(T::value);
};

template <typename T> S2<T> S2<T>::foo() noexcept(T::value) {}
