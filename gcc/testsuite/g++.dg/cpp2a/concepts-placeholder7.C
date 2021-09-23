// PR c++/99899
// { dg-do compile { target c++20 } }

template <class T> concept C1 = sizeof(T) > sizeof(int[1]);

template <class>
void f() {
  int x[] = {1,2};
  int y[] = {3};
  C1 auto [a,b] = x;
  C1 auto [c] = y; // { dg-error "constraints" }
}

template <class T>
void g() {
  T x[] = {1,2};
  T y[] = {3};
  C1 auto [a,b] = x;
  C1 auto [c] = y; // { dg-error "constraints" }
}
template void g<int>();


template <class... Ts> concept C2 = sizeof...(Ts) > 1;

struct S { int a, b; } s;

template <class T>
void h() {
  const C2<T> auto& [a, b] = s;
}
template void h<int>();
