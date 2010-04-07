! { dg-do preprocess }
! { dg-options "-std=f95 -fdiagnostics-show-option -Werror=cpp" }

#warning "Printed"
! { dg-error "\"Printed\" .-Wcpp." "" { target *-*-* } 4 }
