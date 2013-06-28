// PR c++/19618

struct bset1 {
  bool bit : sizeof(bool) * __CHAR_BIT__ + 1;  // { dg-warning "exceeds" }
};

enum E {};

struct bset2 {
  E bit : sizeof(E) * __CHAR_BIT__ + 1;        // { dg-warning "exceeds" }
};

struct bset3 {
  bool bit : sizeof(bool) * __CHAR_BIT__;
};

struct bset4 {
  E bit : sizeof(E) * __CHAR_BIT__;
};
