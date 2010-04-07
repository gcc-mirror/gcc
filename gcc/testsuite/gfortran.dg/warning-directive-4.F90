! { dg-do preprocess }
! { dg-options "-std=f95 -fdiagnostics-show-option -Wno-cpp" }

#warning "Not printed"
! { dg-bogus "." "" { target *-*-* } 4 }
