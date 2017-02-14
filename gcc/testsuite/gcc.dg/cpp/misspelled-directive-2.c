/* { dg-options "-fdiagnostics-show-caret" } */

#endfi /* { dg-error "invalid preprocessing directive #endfi; did you mean #endif?" } */

/* Verify that we offer fix-it hints.  */
/* { dg-begin-multiline-output "" }
 #endfi
  ^~~~~
  endif
  { dg-end-multiline-output "" } */

/* Test coverage for the case of an unrecognized directive where no suggestion
   is offered.  */

#this_does_not_match_anything /* { dg-error "invalid preprocessing directive #this_does_not_match_anything" } */
/* { dg-begin-multiline-output "" }
 #this_does_not_match_anything
  ^~~~~~~~~~~~~~~~~~~~~~~~~~~~
  { dg-end-multiline-output "" } */

int make_non_empty;
