/* PR sanitizer/82484 */
/* { dg-do compile } */

void foo(volatile int *ptr);
void a (volatile int b) { foo(&b); }
