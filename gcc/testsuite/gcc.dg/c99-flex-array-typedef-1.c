/* Test for invalid uses of flexible array members.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

typedef int A[];
struct s1 { A x; }; /* { dg-error "empty struct" "empty" } */
struct s2 { int :1; A x; }; /* { dg-error "empty struct" "empty" } */
struct s3 { A x; int y; }; /* { dg-error "not at end" "not at end" } */
struct s4 { int x; A y; };
