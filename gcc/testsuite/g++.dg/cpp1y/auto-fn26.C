// PR c++/59766
// { dg-do compile { target c++14 } }

struct T {
  friend auto f() { }
};
