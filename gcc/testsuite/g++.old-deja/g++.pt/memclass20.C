// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class X, class Y>
struct S{};

template <class X> 
struct S<int, X> {
  template <class W>
  struct I {};
};

template <class T>
void f() {
  typename S<T, T>::I<T> si;
}

template void f<int>();
