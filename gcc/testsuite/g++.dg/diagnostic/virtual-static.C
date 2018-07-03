// { dg-options "-fdiagnostics-show-caret" }

struct S
{
  virtual static void foo();  // { dg-error "3:member .foo. cannot be declared both .virtual. and .static." }
/* { dg-begin-multiline-output "" }
   virtual static void foo();
   ^~~~~~~ ~~~~~~
   { dg-end-multiline-output "" } */
  static virtual void bar();  // { dg-error "10:member .bar. cannot be declared both .virtual. and .static." }
/* { dg-begin-multiline-output "" }
   static virtual void bar();
   ~~~~~~ ^~~~~~~
   { dg-end-multiline-output "" } */
};
