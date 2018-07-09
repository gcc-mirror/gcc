// { dg-options "-fdiagnostics-show-caret" }

long short int a;  // { dg-error "1:.long. and .short. specified together" }
/* { dg-begin-multiline-output "" }
 long short int a;
 ^~~~ ~~~~~
   { dg-end-multiline-output "" } */
short long int b;  // { dg-error "7:.long. and .short. specified together" }
/* { dg-begin-multiline-output "" }
 short long int b;
 ~~~~~ ^~~~
   { dg-end-multiline-output "" } */
