#include "operator-1.H"
int main(void){ major(0);} /* { dg-note "in expansion of macro 'major'" } */
/* Line numbers below pertain to the header file.  */
/* { dg-warning "Did not Work" "" { target *-*-* } 1 } */
/* { dg-note "in expansion of macro '__glibc_macro_warning1'" "" { target *-*-* } 3 } */
/* { dg-note "in expansion of macro '__glibc_macro_warning'" "" { target *-*-* } 4 } */
/* { dg-note "in expansion of macro '__SYSMACROS_DM1'" "" { target *-*-* } 6 } */
/* { dg-note "in expansion of macro '__SYSMACROS_DM'" "" { target *-*-* } 9 } */
