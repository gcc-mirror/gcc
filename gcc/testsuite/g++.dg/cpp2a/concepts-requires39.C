// PR c++/117887
// { dg-do compile { target c++20 } }

template<bool V> struct A { static constexpr bool value = V; };

template<class T>
using AT = A<requires { requires sizeof(T) != sizeof(T*); }>;

template<class T> struct B { using type = T; };

template<class T>
void f() {
  static_assert( B<AT<T>>::type::value);
  static_assert(!B<AT<T*>>::type::value);
}

template void f<char>();
