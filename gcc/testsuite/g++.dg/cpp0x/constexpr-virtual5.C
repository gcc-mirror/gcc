// PR c+++/67592
// { dg-do compile { target c++11 } }

struct S {
  constexpr virtual int f() { return 1; }  // { dg-error "13:member .f. can be declared both .virtual. and .constexpr." "" { target c++17_down } }
};
