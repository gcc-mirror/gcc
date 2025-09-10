/* Test structures and unions without named members: implementation-defined in
   C2y, undefined behavior previously.  GCC has an extension here, but does not
   allow it in pedantic mode.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

struct s1 { }; /* { dg-error "struct has no members" } */
union u1 { }; /* { dg-error "union has no members" } */
struct s2 { struct { }; }; /* { dg-error "struct has no members" } */
struct s3 { int : 3; int : 4; }; /* { dg-error "struct has no named members" } */
