       ! { dg-do compile }
       ! { dg-options "-Wsurprising" }
       implicit none
       logical b
       b = 4Habcd ! { dg-warning "has undefined result" }
       end

! { dg-warning "Hollerith constant" "const" { target *-*-* } 5 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 5 }
