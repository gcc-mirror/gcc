/* { dg-do compile { target h8300-*-* } } */
/* { dg-options "-ms -mn" } */

void f(void) __attribute__((interrupt_handler));
void g(void) { }
void f(void) { g(); }
