// PR c+++/67592
// { dg-do compile { target c++11 } }

struct S {
  constexpr virtual int f() { return 1; }  // { dg-error "both 'virtual' and 'constexpr'" }
};
