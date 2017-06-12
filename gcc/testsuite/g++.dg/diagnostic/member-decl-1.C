// { dg-options "-fdiagnostics-show-caret" }

struct A
{
  int i,;  /* { dg-error "stray .,. at end of member declaration" }
  { dg-begin-multiline-output "" }
   int i,;
        ^
        -
  { dg-end-multiline-output "" } */

  int j  /* { dg-error "expected .;. at end of member declaration" }
  { dg-begin-multiline-output "" }
   int j
       ^
        ;
  { dg-end-multiline-output "" } */
};
