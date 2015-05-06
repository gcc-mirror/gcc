// PR c++/59766
// { dg-do compile { target c++1y } }

struct T {
  friend auto f() { }
};
