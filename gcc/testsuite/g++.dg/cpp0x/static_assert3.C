// { dg-options "-std=c++0x" }
static_assert(7 / 0, "X"); // { dg-error "non-constant condition" "non-constant" }
// { dg-warning "division by zero" "zero" { target *-*-* } 2 }
// { dg-error "7 / 0.. is not a constant expression" "not a constant" { target *-*-* } 2 }
