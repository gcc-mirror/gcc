       ! { dg-do compile }
       implicit none
       logical b
       b = 4Habcd ! { dg-warning "has undefined result" }
       end

! { dg-warning "Hollerith constant" "const" { target *-*-* } 4 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 4 }
