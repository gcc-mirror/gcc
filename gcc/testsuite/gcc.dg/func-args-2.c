/* PR c/27420 */
/* { dg-do compile } */
/* { dg-options "-w" } */

void foo();
void foo(struct A a) {}  /* { dg-error "incomplete type" } */
