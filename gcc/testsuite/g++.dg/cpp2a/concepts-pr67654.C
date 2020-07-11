// PR c++/67427
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts-ts" }

template <bool... Values> struct and_c_impl {
  static constexpr bool value = true;
};

template <bool ValueFirst, bool... ValuesRest>
struct and_c_impl<ValueFirst, ValuesRest...> {
  static constexpr bool value = ValueFirst && and_c_impl<ValuesRest...>::value;
};

template <bool... Values> constexpr bool and_c() {
  return and_c_impl<Values...>::value;
}

template<class T> concept bool C() {
  return true;
}

template<class... Tx>
struct A {
  A() requires and_c<C<Tx>()...>() = default;
};

int main() {
  A<int, double> a;
  return 0;
}
