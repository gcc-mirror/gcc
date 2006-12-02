/* PR c/27953 */

void foo(struct A a) {}  /* { dg-error "parameter list|definition|incomplete type" } */
void foo() {}            /* { dg-error "redefinition" } */
