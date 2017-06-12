// { dg-options "-Wextra-semi -fdiagnostics-show-caret" }

struct A
{
  A() {};  /* { dg-warning "after in-class function definition" }
  { dg-begin-multiline-output "" }
   A() {};
         ^
         -
  { dg-end-multiline-output "" } */

  void foo() {};  /* { dg-warning "after in-class function definition" }
  { dg-begin-multiline-output "" }
   void foo() {};
                ^
                -
  { dg-end-multiline-output "" } */

  friend void bar() {};  /* { dg-warning "after in-class function definition" }
  { dg-begin-multiline-output "" }
   friend void bar() {};
                       ^
                       -
  { dg-end-multiline-output "" } */
};
