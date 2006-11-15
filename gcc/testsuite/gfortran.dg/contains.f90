! { dg-do compile }
! { dg-options "-std=f2003" }
! Check whether empty contains are allowd
! PR fortran/29806
module x
 contains
end module x ! { dg-error "CONTAINS statement without FUNCTION or SUBROUTINE statement" }

program y
  contains
end program y ! { dg-error "CONTAINS statement without FUNCTION or SUBROUTINE statement" }
