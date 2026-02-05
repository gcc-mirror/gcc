// PR c++/122169
// { dg-do compile { target c++26 } }

template<class T, class U>
concept same_as = __is_same(T, U);

template<class... Ts>
void f() {
  using T = Ts...[0];
  static_assert(same_as<const T, const int>);

  []<int I>() {
    using U = Ts...[I];
    static_assert(same_as<const U, const int>);
  }.template operator()<0>();
}

template void f<int>();


template<class... Ts>
void g() {
  using T = Ts...[0];
  static_assert(same_as<const T, const volatile int>);

  []<int I>() {
    using U = Ts...[I];
    static_assert(same_as<const U, const volatile int>);
  }.template operator()<0>();
}

template void g<volatile int>();
