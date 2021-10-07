/* PR c/32122 */
/* { dg-do compile } */
/* { dg-options "" } */
void foo(void *a) { goto *10000000; } /* { dg-error "computed goto must be pointer type" } */
void foo1(void *a) { goto *a; }

