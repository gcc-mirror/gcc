/* Test for arrays of incomplete and function types: a constraint violation
   in C99 only, though undefined (DR#047) before.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

typedef void func (void);
struct s;

extern int a[][]; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "array" "\[\]\[\] var" { target *-*-* } 11 } */

void f (int [][]); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "array" "\[\]\[\] arg" { target *-*-* } 14 } */

extern struct s b[]; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "array" "struct \[\] var" { target *-*-* } 17 } */

void g (struct s []); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "array" "struct \[\] arg" { target *-*-* } 20 } */

extern func c[]; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "array" "func \[\] var" { target *-*-* } 23 } */

void h (func []); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "array" "func \[\] arg" { target *-*-* } 26 } */
