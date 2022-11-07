// PR c++/92505
// { dg-do compile { target c++14 } }

struct S { mutable int m; };

static_assert(S{42}.m == 42, "");

constexpr int f() {
  S s = {40};
  s.m++;
  const auto& cs = s;
  ++cs.m;
  return cs.m;
}

static_assert(f() == 42, "");
