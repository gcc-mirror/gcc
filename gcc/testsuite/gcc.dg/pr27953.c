/* PR c/27953 */

void foo(struct A a) {}  /* { dg-warning "declared inside parameter list" } */
/* { dg-warning "its scope is only" "" { target *-*-* } 3 } */
/* { dg-error "incomplete type" "" { target *-*-* } 3 } */

void foo() {}            /* { dg-error "redefinition" } */
/* { dg-error "previous definition" "" { target *-*-* } 3 } */
