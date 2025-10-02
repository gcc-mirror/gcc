/* Test C2y constraint against lvalue conversion of lvalues with incomplete
   type: not applied in C23 mode.  Although it is not clear that these
   constructs are valid in C23, we allow certain cases of qualified void
   there.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

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
  cv;
  *pcv;
  /* { dg-warning "dereferencing" "dereferencing" { target *-*-* } .-1 } */
  av;
  *pav;
  /* { dg-warning "dereferencing" "dereferencing" { target *-*-* } .-1 } */
}
