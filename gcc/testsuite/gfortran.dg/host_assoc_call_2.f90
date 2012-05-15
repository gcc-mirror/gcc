! { dg-do compile }
! Tests the fix for PR36700, in which the call to the function would
! cause an ICE.
!
! Contributed by <terry@chem.gu.se>
!
module Diatoms
  implicit none
contains
  function InitialDiatomicX () result(v4)    ! { dg-error "has a type" }
    real(kind = 8), dimension(4) :: v4
    v4 = 1
  end function InitialDiatomicX
  subroutine FindDiatomicPeriod
    call InitialDiatomicX ()    ! { dg-error "which is not consistent with the CALL" }
  end subroutine FindDiatomicPeriod
end module Diatoms
