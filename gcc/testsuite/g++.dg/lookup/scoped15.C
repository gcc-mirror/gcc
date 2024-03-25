// PR c++/112744
// { dg-do compile { target c++11 } }

struct A { constexpr static int a = 0; };
struct D : private A {};

// The injected-class-name of A is private when named in D, but if A is named
// some other way, there is no requirement in [class.access.base] for static data
// members that it be an accessible base.

void f() {
  D{}.A::a; // { dg-error "inaccessible" }
  D{}.::A::a;
}

template<class T>
void g() {
  D{}.T::a;
}

template void g<A>();
