/* Test diagnostics for parameter mismatches.  Types that can't match
   ().  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

void f0(); /* { dg-error "previous declaration of 'f0' was here" } */
void f0(int, ...); /* { dg-error "conflicting types for 'f0'" } */
/* { dg-message "note: a parameter list with an ellipsis can't match an empty parameter name list declaration" "note" { target *-*-* } 8 } */
void f1(int, ...); /* { dg-error "previous declaration of 'f1' was here" } */
void f1(); /* { dg-error "conflicting types for 'f1'" } */
/* { dg-message "note: a parameter list with an ellipsis can't match an empty parameter name list declaration" "note" { target *-*-* } 11 } */
void f2(); /* { dg-error "previous declaration of 'f2' was here" } */
void f2(char); /* { dg-error "conflicting types for 'f2'" } */
/* { dg-message "note: an argument type that has a default promotion can't match an empty parameter name list declaration" "note" { target *-*-* } 14 } */
void f3(char); /* { dg-error "previous declaration of 'f3' was here" } */
void f3(); /* { dg-error "conflicting types for 'f3'" } */
/* { dg-message "note: an argument type that has a default promotion can't match an empty parameter name list declaration" "note" { target *-*-* } 17 } */
