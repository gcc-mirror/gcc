// { dg-options "-fdiagnostics-show-caret" }

/* Verify that we highlight *which* string is too long.  */

char test[3][4] = { "ok", "too long", "ok" }; // { dg-error "initializer-string for array of chars is too long" }
/* { dg-begin-multiline-output "" }
 char test[3][4] = { "ok", "too long", "ok" };
                           ^~~~~~~~~~
   { dg-end-multiline-output "" } */
