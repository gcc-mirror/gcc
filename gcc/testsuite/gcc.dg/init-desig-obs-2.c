/* Test obsolete forms of designated initializers.  Test with
   -pedantic.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic" } */
struct s { int a; };
struct s s0 = { .a = 1 };
struct s s1 = { a: 1 }; /* { dg-warning "warning: obsolete use of designated initializer with ':'" } */

int x0[] = { [0] = 1 };
int x1[] = { [0] 1 }; /* { dg-warning "warning: obsolete use of designated initializer without '='" } */
