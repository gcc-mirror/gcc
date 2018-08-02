// { dg-options "-fdiagnostics-show-caret" }
// { dg-do compile { target c++11 } }

struct S
{
  virtual constexpr void foo();  // { dg-error "3:member .foo. cannot be declared both .virtual. and .constexpr." }
/* { dg-begin-multiline-output "" }
   virtual constexpr void foo();
   ^~~~~~~ ~~~~~~~~~
   { dg-end-multiline-output "" } */
  constexpr virtual void bar();  // { dg-error "13:member .bar. cannot be declared both .virtual. and .constexpr." }
/* { dg-begin-multiline-output "" }
   constexpr virtual void bar();
   ~~~~~~~~~ ^~~~~~~
   { dg-end-multiline-output "" } */
};
