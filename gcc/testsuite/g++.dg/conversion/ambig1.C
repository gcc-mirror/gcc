// PR c++/19787

struct H {
  operator char(); // { dg-error "" }
  operator short(); // { dg-error "" }
};

int const& ref = H(); // { dg-error "" }
