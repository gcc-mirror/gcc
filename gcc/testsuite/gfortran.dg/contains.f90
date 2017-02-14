! { dg-do compile }
! { dg-options "-std=f2003" }
! Check whether empty contains are allowd
! PR fortran/29806
module x
 contains ! { dg-error "CONTAINS statement without FUNCTION or SUBROUTINE statement" }
end module x

program y
  contains  ! { dg-error "CONTAINS statement without FUNCTION or SUBROUTINE statement" }
end program y
