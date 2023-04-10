// Verify we can evaluate a non-dependent variable template-id ahead of time.
// { dg-do compile { target c++14 } }

template<int N>
constexpr int var = N;

template<int N> void f(int) = delete;
template<int N> void f(...);

template<class T>
void g() {
  f<var<42>>(0); // { dg-error "deleted" }
}
