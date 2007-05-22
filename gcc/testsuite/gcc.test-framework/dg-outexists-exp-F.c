/* { dg-do compile } */
/* { dg-options "-Wunused-value -Werror" } */

int main (void) { 0; }   /* { dg-error "no effect" } */

/* { dg-message "warnings being treated as errors" "" { target *-*-* } 0 } */
/* { dg-final { output-exists { target *-*-* } } } */
