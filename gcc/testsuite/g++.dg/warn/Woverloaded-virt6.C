// PR c++/109918 - PR testcase with -Woverloaded-virtual=2
// { dg-do compile }
// { dg-additional-options -Woverloaded-virtual=2 }

struct A {
  virtual operator int() { return 42; }
  virtual operator char() = 0;
};

struct B : public A {
  operator char() { return 'A'; }
};
