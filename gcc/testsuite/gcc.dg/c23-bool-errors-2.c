/* Test error-handling for legacy code that tries to
   use a variable named "bool" with C23.  */

/* { dg-do compile } */
/* { dg-options "-std=c23" } */

int bool; /* { dg-error "'bool' cannot be used here" } */
/* { dg-message "'bool' is a keyword with '-std=c23' onwards" "" { target *-*-* } .-1 } */
/* { dg-warning "useless type name in empty declaration" "" { target *-*-* } .-2 } */
