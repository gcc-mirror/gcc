// { dg-options "-w" }

template <typename T>
struct S {
  int i;
  template <typename U> void f(U) {}
};

template<> 
template <typename U>
void S<int>::f(U) { i; }

void f() {
  S<int> s;
  s.f<int>(3);
}
