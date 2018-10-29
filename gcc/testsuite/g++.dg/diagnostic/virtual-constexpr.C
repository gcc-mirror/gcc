// { dg-options "-fdiagnostics-show-caret -pedantic-errors" }
// { dg-do compile { target c++11 } }
// { dg-skip-if "virtual constexpr" { *-*-* } { "-std=gnu++2a" } { "" } }

struct S
{
  virtual constexpr void foo();  // { dg-error "3:member .foo. can be declared both .virtual. and .constexpr." }
/* { dg-begin-multiline-output "" }
   virtual constexpr void foo();
   ^~~~~~~ ~~~~~~~~~
   { dg-end-multiline-output "" } */
  constexpr virtual void bar();  // { dg-error "13:member .bar. can be declared both .virtual. and .constexpr." }
/* { dg-begin-multiline-output "" }
   constexpr virtual void bar();
   ~~~~~~~~~ ^~~~~~~
   { dg-end-multiline-output "" } */
};
