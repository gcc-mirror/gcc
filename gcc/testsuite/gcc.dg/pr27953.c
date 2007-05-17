/* PR c/27953 */

void foo(struct A a) {}  /* { dg-warning "parameter list|definition|incomplete type" } */
void foo() {}            /* { dg-error "redefinition" } */
