/* { dg-do assemble { target cet } } */
/* { dg-options "-fleading-underscore -fcf-protection" } */

void func(void) __asm("_func");
void _func(int x) {}
void func(void) {}
