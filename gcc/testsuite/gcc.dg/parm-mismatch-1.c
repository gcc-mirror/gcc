/* Test diagnostics for parameter mismatches.  Types that cannot match
   ().  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

void f0(); /* { dg-message "note: previous declaration of 'f0'" "note" } */
void f0(int, ...); /* { dg-error "conflicting types for 'f0'" } */
/* { dg-message "note: a parameter list with an ellipsis cannot match an empty parameter name list declaration" "note" { target *-*-* } .-1 } */
void f1(int, ...); /* { dg-message "note: previous declaration of 'f1'" "note" } */
void f1(); /* { dg-error "conflicting types for 'f1'" } */
/* { dg-message "note: a parameter list with an ellipsis cannot match an empty parameter name list declaration" "note" { target *-*-* } .-1 } */
void f2(); /* { dg-message "note: previous declaration of 'f2'" "note" } */
void f2(char); /* { dg-error "conflicting types for 'f2'" } */
/* { dg-message "note: an argument type that has a default promotion cannot match an empty parameter name list declaration" "note" { target *-*-* } .-1 } */
void f3(char); /* { dg-message "note: previous declaration of 'f3'" "note" } */
void f3(); /* { dg-error "conflicting types for 'f3'" } */
/* { dg-message "note: an argument type that has a default promotion cannot match an empty parameter name list declaration" "note" { target *-*-* } .-1 } */
