/* Test diagnostics for empty structures and unions.  Test with
   -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

struct s0 {}; /* { dg-error "error: struct has no members" } */
union u0 {}; /* { dg-error "error: union has no members" } */
struct s1 { int : 1; }; /* { dg-error "error: struct has no named members" } */
union u1 { int : 1; }; /* { dg-error "error: union has no named members" } */
