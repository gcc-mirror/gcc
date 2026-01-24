// PR c++/123557
// { dg-do compile { target c++11 } }

struct a {
  a() = default;
  a(a const &) = default;
  a(a &&);
};
struct c {
  c();
  a e;
};
constexpr a b;
c::c() : e(b) {}
