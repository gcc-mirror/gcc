// Test for decltype of direct decomposition.
// { dg-do compile { target c++17 } }

template <class,class> struct same_type;
template <class T> struct same_type<T,T> {};

struct A {
  int i;
  const int ci = 42;
  mutable int mi;
  int& r = i;
  const int& cr = ci;
} a;

void f() {
  auto [i,ci,mi,r,cr] = a;

  same_type<decltype(i),int>{};
  same_type<decltype(ci),const int>{};
  same_type<decltype(mi),int>{};
  same_type<decltype(r),int&>{};
  same_type<decltype(cr),const int&>{};
}
void frr() {
  auto &&[i,ci,mi,r,cr] = a;

  same_type<decltype(i),int>{};
  same_type<decltype(ci),const int>{};
  same_type<decltype(mi),int>{};
  same_type<decltype(r),int&>{};
  same_type<decltype(cr),const int&>{};
}
void fc() {
  const auto [i,ci,mi,r,cr] = a;

  same_type<decltype(i),const int>{};
  same_type<decltype(ci),const int>{};
  same_type<decltype(mi),int>{};
  same_type<decltype(r),int&>{};
  same_type<decltype(cr),const int&>{};
}
void frc() {
  const A ca{};
  auto &[i,ci,mi,r,cr] = ca;

  same_type<decltype(i),const int>{};
  same_type<decltype(ci),const int>{};
  same_type<decltype(mi),int>{};
  same_type<decltype(r),int&>{};
  same_type<decltype(cr),const int&>{};
}
