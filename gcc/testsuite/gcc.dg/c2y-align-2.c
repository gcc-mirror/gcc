/* Test C2Y alignof on an incomplete array type: still not allowed for other
   incomplete types.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

int a = alignof(void); /* { dg-error "void" } */
struct s;
int b = alignof(struct s); /* { dg-error "incomplete" } */
