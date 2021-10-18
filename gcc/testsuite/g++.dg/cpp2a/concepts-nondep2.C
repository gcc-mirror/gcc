// PR c++/102412
// { dg-do link { target c++20 } }

template<class T, class U> concept C = __is_same(T, U);

template<class T, bool = C<int, T>> void f();
template<> void f<int, true>() { }
template<> void f<char, false>() { }

template<bool = C<int, char>> void g();
template<> void g<false>() { }

template<bool = C<int, int>> void h();
template<> void h<true>() { }

int main() {
  f<int>();
  f<char>();
  g();
  h();
}
