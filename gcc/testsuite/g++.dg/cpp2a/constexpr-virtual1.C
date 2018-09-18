// P1064R0
// { dg-do compile { target c++11 } }
// { dg-options "-pedantic-errors" }

struct X
{
  constexpr virtual int f() { return 0; } // { dg-error "member .f. can be declared both .virtual. and .constexpr. only" "" { target c++17_down } }
};
