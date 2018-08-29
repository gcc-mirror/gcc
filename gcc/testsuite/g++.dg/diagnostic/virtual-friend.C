// { dg-options "-fdiagnostics-show-caret" }
// { dg-do compile { target c++11 } }

struct S
{
  virtual friend void foo();  // { dg-error "3:virtual functions cannot be friends" }
/* { dg-begin-multiline-output "" }
   virtual friend void foo();
   ^~~~~~~ ~~~~~~
   { dg-end-multiline-output "" } */
  friend virtual void bar();  // { dg-error "10:virtual functions cannot be friends" }
/* { dg-begin-multiline-output "" }
   friend virtual void bar();
   ~~~~~~ ^~~~~~~
   { dg-end-multiline-output "" } */
};
