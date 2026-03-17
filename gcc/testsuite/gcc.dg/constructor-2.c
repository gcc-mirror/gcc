/* { dg-do compile { target init_priority } } */
/* { dg-options "" } */

/* PR middle-end/120030 */

void f()  __attribute__ ((constructor (140))) __attribute__ ((constructor));
void g()  __attribute__ ((destructor (140))) __attribute__ ((destructor));
