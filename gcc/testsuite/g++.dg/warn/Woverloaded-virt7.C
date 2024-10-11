// PR c++/109918 - Test different CV-quals
// { dg-do compile }
// { dg-additional-options -Woverloaded-virtual }

struct A {
  virtual operator char() { return 'a'; }
  virtual operator char() const { return 'b'; } // { dg-warning "was hidden" }
  virtual operator int() { return 42; }
};

struct B : public A {
  operator char() { return 'A'; } // { dg-note "by" }
  operator int() { return 43; }
};

struct AA {
  virtual char func(char) { return 'a'; }
  virtual char func(char) const { return 'b'; } // { dg-warning "was hidden" }
  virtual int func(int) { return 42; }
};

struct BB : public AA {
  char func(char) { return 'A'; } // { dg-note "by" }
  int func(int) { return 43; }
};
