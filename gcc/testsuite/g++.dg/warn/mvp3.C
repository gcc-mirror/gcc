// { dg-do compile }
// { dg-options "-Wparentheses -fdiagnostics-show-caret" }
// Test fix-it hints for the MVP warning.

void
g ()
{
  int (i); // { dg-warning "7:unnecessary parentheses" }
/* { dg-begin-multiline-output "" }
   int (i);
       ^~~
   { dg-end-multiline-output "" } */
// { dg-message "7:remove parentheses" "" { target *-*-* } 8 }
/* { dg-begin-multiline-output "" }
   int (i);
       ^~~
       - -
   { dg-end-multiline-output "" } */
  int (fn(void)); // { dg-warning "7:unnecessary parentheses" }
/* { dg-begin-multiline-output "" }
   int (fn(void));
       ^~~~~~~~~~
   { dg-end-multiline-output "" } */
// { dg-message "7:remove parentheses" "" { target *-*-* } 19 }
/* { dg-begin-multiline-output "" }
   int (fn(void));
       ^~~~~~~~~~
       -        -
   { dg-end-multiline-output "" } */
}
