const _NISIZE_DIV_P: &isize = &(1isize / 0);
// { dg-error "division by zero" "" { target *-*-* } .-1 }
// { dg-error "is not a constant expression" "" { target *-*-* } .-2 }
