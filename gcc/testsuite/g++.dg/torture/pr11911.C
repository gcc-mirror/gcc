// { dg-do compile }
// { dg-require-effective-target c++17 }

struct b {
  int a;
};
struct c {
  b d{};
  c() = default;
  c(c &) = delete;
};
struct e {
  c a{};
  e() {}
};
inline e f() { return {}; }
struct g {
  e cx;
  g() : cx{f()} {}
};
void h() { g i; }
