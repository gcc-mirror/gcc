// { dg-do compile { target c++20 } }

template <class T> concept C = requires(T t) { t; };

template <class T> using A = decltype((T{}, int{}));

template <class T> concept D = C<A<T>>;

template <class T, class U> void f() requires D<T>;
template <class T> void g() requires D<T>;

void h() {
  f<int, int>();
  g<int>();
}
