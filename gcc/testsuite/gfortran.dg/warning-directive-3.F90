! { dg-do preprocess }
! { dg-options "-std=f95 -fdiagnostics-show-option -Werror -Wno-error=cpp" }

#warning "Printed"
! { dg-warning "\"Printed\" .-Wcpp." "" { target *-*-* } 4 }
