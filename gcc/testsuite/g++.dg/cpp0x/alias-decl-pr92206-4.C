// { dg-do compile { target c++11 } }

template <typename T> struct A { };
template <typename T, int = sizeof(typename T::type)> using AA = A<T>; // { dg-error  "char" }
template <typename T> using AAA = AA<T>;

template <class T> struct C { };
template <class T> struct B {
  C<AAA<T>> a;
};
B<char> b;			// { dg-message "" }
