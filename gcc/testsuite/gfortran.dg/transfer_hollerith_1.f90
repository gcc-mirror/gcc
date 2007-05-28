! { dg-do compile }
! { dg-options "-O2" }
! PR 31972, ICE in transfer of Hollerith constant
  integer, dimension(1) :: i
  integer :: j
  i = (/ transfer(4HSOLR, 0) /)

  j = transfer(0, 4HSOLR) ! { dg-error "must not be HOLLERITH" }
end

! { dg-warning "Hollerith constant" "const" { target *-*-* } 6 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 8 }

