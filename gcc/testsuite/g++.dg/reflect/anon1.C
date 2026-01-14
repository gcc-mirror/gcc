// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// https://github.com/cplusplus/CWG/issues/705

auto foo() {
  union { int what; };
  return &[:^^what:];  // { dg-error "not a direct member of a named class" }
}

static union { int m; };
constexpr auto r = ^^m;
auto p = &[:r:]; // { dg-error "not a direct member of a named class" }

namespace N {
  static union { int mn; };
  constexpr auto rn = ^^mn;
  auto pn = &[:rn:];  // { dg-error "not a direct member of a named class" }
}

struct S {
  union {
    int m;
  };
};

constexpr auto r2 = ^^S::m;
constexpr auto p2 = &[: ^^r2 :];

void
f ()
{
  static union {
    int x;
  };
  auto rx = &[: ^^x :]; // { dg-error "not a direct member of a named class" }

  union {
    union {
      int u;
    };
  };
  auto ru = &[: ^^u :]; // { dg-error "not a direct member of a named class" }

  struct L {
    union {
      int z;
    };
  };
  auto rz = &[: ^^L::z :];
}
