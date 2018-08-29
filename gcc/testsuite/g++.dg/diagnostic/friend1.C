// { dg-options "-fdiagnostics-show-caret" }

friend void foo();  /* { dg-error "used outside of class" }
  { dg-begin-multiline-output "" }
 friend void foo();
 ^~~~~~
 ------
  { dg-end-multiline-output "" } */
