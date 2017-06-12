// { dg-options "-fdiagnostics-show-caret" }

struct A
{
  void foo() const const;  /* { dg-error "duplicate cv-qualifier" }
  { dg-begin-multiline-output "" }
   void foo() const const;
                    ^~~~~
                    -----
  { dg-end-multiline-output "" } */
};

volatile volatile int i = 0;  /* { dg-error "duplicate" }
  { dg-begin-multiline-output "" }
 volatile volatile int i = 0;
          ^~~~~~~~
          --------
  { dg-end-multiline-output "" } */
