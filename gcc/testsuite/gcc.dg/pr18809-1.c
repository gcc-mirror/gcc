/* PR c/18809 */
/* Origin: Andrew Pinski <pinskia@gcc.gnu.org> */

/* { dg-do compile } */

void foo(enum E e) {}   /* { dg-error "" } */
void bar() { foo(0); }  /* { dg-error "formal" } */
