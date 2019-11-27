// { dg-do compile { target c++2a } }

template <typename T> struct A { };
template <typename T> concept int_type = __is_same_as (T, int);
template <int_type T> using intA = A<T>;

template <template <typename T> class TT> struct B {
  TT<char> tt;			// { dg-error "" }
};
B<intA> b;
