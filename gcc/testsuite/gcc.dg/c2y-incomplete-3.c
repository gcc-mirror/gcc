/* Test C2y constraint against lvalue conversion of lvalues with incomplete
   type.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

struct s;
union u;

extern struct s vs, *ps;
extern _Atomic struct s vas, *pas;
extern union u vu, *pu;
extern _Atomic union u vau, *pau;

extern const void cv, *pcv;
extern _Atomic void av, *pav;

void
f ()
{
  vs; /* { dg-error "incomplete type" } */
  *ps; /* { dg-error "invalid use of undefined type" } */
  vas; /* { dg-error "incomplete type" } */
  *pas; /* { dg-error "invalid use of undefined type" } */
  vu; /* { dg-error "incomplete type" } */
  *pu; /* { dg-error "invalid use of undefined type" } */
  vau; /* { dg-error "incomplete type" } */
  *pau; /* { dg-error "invalid use of undefined type" } */
  cv; /* { dg-error "incomplete type" } */
  *pcv; /* { dg-error "invalid use of void expression" } */
  /* { dg-warning "dereferencing" "dereferencing" { target *-*-* } .-1 } */
  av; /* { dg-error "incomplete type" } */
  *pav; /* { dg-error "invalid use of void expression" } */
  /* { dg-warning "dereferencing" "dereferencing" { target *-*-* } .-1 } */
}
