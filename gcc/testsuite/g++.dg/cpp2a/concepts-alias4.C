// { dg-do compile { target c++20 } }

template <typename T> struct A { };
template <typename T> concept int_type = __is_same_as (T, int);
template <int_type T> using intA = A<T>;

template <class T> struct B {
  intA<T> a;			// { dg-error "" }
};
B<char> b;
