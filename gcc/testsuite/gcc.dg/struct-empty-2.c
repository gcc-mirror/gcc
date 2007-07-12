/* Test diagnostics for empty structures and unions.  Test with
   -pedantic.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

struct s0 {}; /* { dg-warning "struct has no members" } */
union u0 {}; /* { dg-warning "union has no members" } */
struct s1 { int : 1; }; /* { dg-warning "struct has no named members" } */
union u1 { int : 1; }; /* { dg-warning "union has no named members" } */
