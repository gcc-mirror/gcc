// { dg-do run  }
// Bug: generated B::operator= tries to call A::operator=

#pragma implementation
#line 1 "synth5.h"
#pragma interface

struct A {
  virtual A& operator= (const A&) = 0;
};

struct B: public A {
};
#line 5 "synth5.C"
int main() { }
