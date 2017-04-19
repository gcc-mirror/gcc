! { dg-do preprocess }
! { dg-options "-std=f95 -fdiagnostics-show-option" }

#warning "Printed"
! { dg-warning "\"Printed\" .-Wcpp." "" { target *-*-* } .-1 }
