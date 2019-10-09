/* PR c/19999 */
/* { dg-do compile } */
/* { dg-options "-Wfloat-equal" } */

double a, b;
_Complex double c, d;
int f(void) { return a == b; } /* { dg-warning "24:comparing floating-point" } */
int g(void) { return c == d; } /* { dg-warning "24:comparing floating-point" } */
int h(void) { return a != b; } /* { dg-warning "24:comparing floating-point" } */
int i(void) { return c != d; } /* { dg-warning "24:comparing floating-point" } */
