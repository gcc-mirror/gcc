// { dg-options "-fdiagnostics-show-caret" }

signed unsigned int a;  // { dg-error "1:.signed. and .unsigned. specified together" }
/* { dg-begin-multiline-output "" }
 signed unsigned int a;
 ^~~~~~ ~~~~~~~~
   { dg-end-multiline-output "" } */
unsigned signed int b;  // { dg-error "10:.signed. and .unsigned. specified together" }
/* { dg-begin-multiline-output "" }
 unsigned signed int b;
 ~~~~~~~~ ^~~~~~
   { dg-end-multiline-output "" } */
