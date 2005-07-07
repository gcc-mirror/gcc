       ! { dg-do run }
       ! Program to test Hollerith constant.
       Program test
       implicit none
       integer* 4 i,j
       real r, x, y
       parameter (i = 4h1234)
       parameter (r = 4hdead)
       parameter (y = 4*r)
       parameter (j = selected_real_kind (i))
       x = 4H1234 
       x = sin(r)
       x = x * r
       x = x / r
       x = x + r
       x = x - r
       end
! { dg-warning "Hollerith constant" "const" { target *-*-* } 7 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 7 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 8 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 8 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 11 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 11 }

