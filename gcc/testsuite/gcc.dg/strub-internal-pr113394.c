/* { dg-do compile } */
/* { dg-options "-fstrub=internal -g" } */

/* gcc.c-torture/compile/20020210-1.c */
/* PR c/5615 */
void f(int a, struct {int b[a];} c) {} /* { dg-warning "anonymous struct" } */
