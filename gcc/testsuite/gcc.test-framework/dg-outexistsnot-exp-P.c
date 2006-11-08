/* { dg-do compile } */
/* { dg-options "-W -Werror" } */

int main (void) { 0; }   /* { dg-warning "no effect" } */

/* { dg-warning "warnings being treated as errors" "" { target *-*-* } 0 } */
/* { dg-final { output-exists-not { target *-*-* } } } */
