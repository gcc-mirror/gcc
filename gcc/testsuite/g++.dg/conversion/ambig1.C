// PR c++/19787

struct H {
  operator char(); // { dg-message "note" }
  operator short(); // { dg-message "note" }
};

int const& ref = H(); // { dg-error "ambiguous" }
