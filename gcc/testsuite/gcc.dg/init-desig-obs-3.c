/* Test obsolete forms of designated initializers.  Test with
   -pedantic-errors.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */
struct s { int a; };
struct s s0 = { .a = 1 };
struct s s1 = { a: 1 }; /* { dg-error "obsolete use of designated initializer with ':'" } */

int x0[] = { [0] = 1 };
int x1[] = { [0] 1 }; /* { dg-error "obsolete use of designated initializer without '='" } */
