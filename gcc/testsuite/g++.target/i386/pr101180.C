// PR c++/101180
// { dg-do compile { target c++11 } }

#pragma GCC target "avx"
template <typename> struct A {};
#pragma GCC push_options
#pragma GCC target "avx,avx2,bmi,bmi2,fma,f16c"
template <typename T> using B = A<T>;
template <typename> struct C;
template <> struct C<float> {
  __attribute__((always_inline)) float operator()(long) { return .0f; }
};
long d;
template <typename T> void e(B<T>) {
  T{C<T>()(d)};
}
template <typename T, typename FromT> void f(T d, FromT) {
  e(d);
}
int g;
void h() {
  A<float> i;
  f(i, g);
}
#pragma GCC pop_options
