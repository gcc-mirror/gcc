// { dg-do compile { target c++20 } }

template<typename T>
struct S {
  using type = T::type; // { dg-bogus "" }
};

template<typename T>
auto f() {
  return [] <typename U> (U) {
    // Verify that partial instantiation of this generic lambda doesn't cause
    // these requirements to get checked out of order.
    static_assert(!requires { typename U::type; typename S<T>::type; });
    return 0;
  };
}

int a = f<int>()(0);
