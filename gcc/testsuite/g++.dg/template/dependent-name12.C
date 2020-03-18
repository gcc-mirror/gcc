// PR c++/94057 - template keyword in a typename-specifier.

template <bool> struct A;
template <typename, typename> struct B;
template <typename T, typename U, typename V> struct B<T U::*, V> {
  typename A<V::x>::type::type t;
};
