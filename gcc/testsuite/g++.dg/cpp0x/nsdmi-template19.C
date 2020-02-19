// PR c++/93676 - value-init crash in template.
// { dg-do compile { target c++11 } }

struct P {
  int x = 0;
};

template<class T>
struct S {
  S() { new P[2][2]; }
};

S<int> s;
