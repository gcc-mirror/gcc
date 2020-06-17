// DR 1710 - Missing template keyword in class-or-decltype
// { dg-do compile }

template<typename T> struct D : T::template B<int>::template C<int> {};
template<typename T> struct D2 : T::B<int>::template C<int> {};
template<typename T> struct D3 : T::template B<int>::C<int> {};
template<typename T> struct D4 : T::B<int>::C<int> {};
template<typename T> struct D5 : T::template B<int>::type::type {};
template<typename T> struct D6 : T::B<int>::type::type {};
