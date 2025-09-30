/* Verify that we use emit diagnostics for #pragma GCC suppress_coverage.  */

/* { dg-do assemble } */
/* { dg-options "-fdiagnostics-show-caret" } */

#pragma GCC suppress_coverage end
/* { dg-warning "no matching begin for '#pragma GCC suppress_coverage end'" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma GCC suppress_coverage end
         ^~~
   { dg-end-multiline-output "" }  */

#pragma GCC suppress_coverage
/* { dg-warning "'#pragma GCC suppress_coverage' must be followed by 'begin' or 'end'" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma GCC suppress_coverage
         ^~~
   { dg-end-multiline-output "" }  */

#pragma GCC suppress_coverage begin more
/* { dg-warning "junk at end of '#pragma GCC suppress_coverage'" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma GCC suppress_coverage begin more
                                     ^~~~
   { dg-end-multiline-output "" }  */

#pragma GCC suppress_coverage begin
/* { dg-warning "'#pragma GCC suppress_coverage begin' was already in effect, ignored" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma GCC suppress_coverage begin
         ^~~
   { dg-end-multiline-output "" }  */
