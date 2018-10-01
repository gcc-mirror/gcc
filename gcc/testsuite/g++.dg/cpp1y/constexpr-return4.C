// PR c++/86678
// { dg-do compile { target c++14 } }

constexpr bool always_true() { return true; }
int f() { return 1; }
constexpr int g() {
  if (always_true())
    return 0;
  return f();
}
