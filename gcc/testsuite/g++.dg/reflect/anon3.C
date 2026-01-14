// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

auto foo() {
  union U { int what; };
  return &[:^^U::what:];
}

union U { int m; };
constexpr auto r = ^^U::m;
auto p = &[:r:];

namespace N {
  union V { int mn; };
  constexpr auto rn = ^^V::mn;
  auto pn = &[:rn:];
}

struct S {
  union W {
    int m;
  };
};

constexpr auto r2 = ^^S::W::m;
constexpr auto p2 = &[: ^^r2 :];

void
f ()
{
  union U1 {
    int x;
  };
  auto rx = &[: ^^U1::x :];

  union UU {
    union U2 {
      int u;
    };
  };
  auto ru = &[: ^^UU::U2::u :];

  struct L {
    union U3 {
      int z;
    };
  };
  auto rz = &[: ^^L::U3::z :];
}
