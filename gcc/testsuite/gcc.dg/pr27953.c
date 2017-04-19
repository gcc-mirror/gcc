/* PR c/27953 */

void foo(struct A a) {}  /* { dg-warning "declared inside parameter list" "inside" } */
/* { dg-error "incomplete type" "incomplete" { target *-*-* } .-1 } */

void foo() {}            /* { dg-error "redefinition" "redef" } */
/* { dg-message "note: previous definition" "previous" { target *-*-* } 3 } */
