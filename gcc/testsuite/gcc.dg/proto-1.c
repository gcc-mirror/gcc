/* PR c/28502 */
/* { dg-do compile } */

void foo() {}      /* { dg-error "previous" } */
void foo(void[]);  /* { dg-error "array of voids" } */
