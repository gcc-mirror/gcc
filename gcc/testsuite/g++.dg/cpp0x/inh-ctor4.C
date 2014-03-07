// From N3337
// { dg-do compile { target c++11 } }

struct B1 {
  B1(int);
};
struct B2 {
  B2(int = 13, int = 42);
};
struct D1 : B1 {
  using B1::B1;
};
struct D2 : B2 {
  using B2::B2;
};

D1 d1(1);
D2 d2a(2), d2b(3,4);
