// PR c++/109918 - Exact PR testcase
// { dg-do compile }
// { dg-additional-options -Wall }

struct A {
  virtual operator int() { return 42; }
  virtual operator char() = 0;
};

struct B : public A {
  operator char() { return 'A'; }
};
