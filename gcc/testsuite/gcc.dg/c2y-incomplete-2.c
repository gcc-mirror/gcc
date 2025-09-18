/* Test member access to incomplete structures and unions (explicit constraint
   violation in C2y).  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

struct s1;
extern struct s1 vs1;
struct s1 { int a; int b : sizeof (vs1.a); }; /* { dg-error "invalid use of undefined type" } */
/* { dg-error "width not an integer constant" "constant" { target *-*-* } .-1 } */

union u1;
extern union u1 vu1;
union u1 { int a; int b : sizeof (vu1.a); }; /* { dg-error "invalid use of undefined type" } */
/* { dg-error "width not an integer constant" "constant" { target *-*-* } .-1 } */

struct s2;
extern struct s2 *ps2;
struct s2 { int a; int b : sizeof (ps2->a); }; /* { dg-error "invalid use of undefined type" } */
/* { dg-error "width not an integer constant" "constant" { target *-*-* } .-1 } */

union u2;
extern union u2 *pu2;
union u2 { int a; int b : sizeof (pu2->a); }; /* { dg-error "invalid use of undefined type" } */
/* { dg-error "width not an integer constant" "constant" { target *-*-* } .-1 } */
