/* PR c/79758 */
/* { dg-do compile } */

void fn1 (int[a]) { }; /* { dg-error "undeclared here" } */
void fn1 (b) { }; /* { dg-error "redefinition" } */
/* { dg-warning "defaults to 'int'" "" { target *-*-* } .-1 } */
