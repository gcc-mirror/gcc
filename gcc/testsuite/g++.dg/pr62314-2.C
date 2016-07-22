// { dg-options "-fdiagnostics-show-caret" }

template<class T>
class a {} // { dg-error "11: expected .;. after class definition" }
class temp {};
a<temp> b;
struct b {
} // { dg-error "2: expected .;. after struct definition" }

/* Verify that we emit fixit hints.  */

/* { dg-begin-multiline-output "" }
 class a {}
           ^
           ;
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
 }
  ^
  ;
   { dg-end-multiline-output "" } */
