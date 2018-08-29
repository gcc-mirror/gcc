// PR c++/58328
// { dg-do compile { target c++11 } }

struct A1 {
  struct B1 {
    int y1 = 1;
  };

  A1(const B1& opts = B1()) {}  // { dg-error "default member initializer" }
};

struct A2 {
  struct B2 {
    int x2, y2 = 1;
  };

  A2(const B2& opts = B2()) {}  // { dg-error "default member initializer" }
};
