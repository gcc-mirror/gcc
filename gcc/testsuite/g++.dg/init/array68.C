// PR c++/126335
// { dg-do run { target c++20 } }

struct D {};
struct Lit {
  constexpr Lit(char const *) {}
  D str;
};
auto empty = Lit("");
struct S {
  D * p;
  constexpr S() { p = &empty.str; }
  S(Lit) {}
  ~S() {}
};
template<Lit L> S operator ""_s() { return L; }
struct M { S a, b; };
static M m[2]{{""_s, ""_s}, {}};
int main() { return m[1].a.p == nullptr; }
