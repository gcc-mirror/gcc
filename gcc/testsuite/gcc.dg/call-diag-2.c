/* Test diagnostics for calling function returning qualified void or
   other incomplete type other than void.  PR 35210.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */

const void f_cv (void);
struct s f_s (void);
void f_v (void);

void g1 (void) { f_cv (); } /* { dg-error "qualified void" } */
void g2 (void) { f_s (); } /* { dg-error "invalid use of undefined type" } */
void g3 (void) { ((const void (*) (void)) f_v) (); } /* { dg-error "qualified void" } */
/* { dg-warning "function called through a non-compatible type" "cast" { target *-*-* } .-1 } */
void g4 (void) { ((struct s (*) (void)) f_v) (), (void) 0; } /* { dg-error "invalid use of undefined type" } */
/* { dg-warning "function called through a non-compatible type" "cast" { target *-*-* } .-1 } */
