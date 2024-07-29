// PR c++/115986
// { dg-do compile { target c++20 } }

template <typename T>
constexpr int b(T) {
  return 0;
}
consteval
#ifdef __SIZEOF_INT128__
__uint128_t
#else
unsigned long long
#endif
operator"" _c(const char*) { return 0; }
constexpr char e() {
  long f = true ? 0 : b(long(1));
  return b(f);
}
template <typename>
void d() {
  0_c;
  static_assert(e());
}
