/* Test for invalid uses of flexible array members.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

struct s1 { int x[]; }; /* { dg-error "empty struct" "empty" } */
struct s2 { int :1; int x[]; }; /* { dg-error "empty struct" "empty" } */
struct s3 { int x[]; int y; }; /* { dg-error "not at end" "not at end" } */
struct s4 { int x; int y[]; };
