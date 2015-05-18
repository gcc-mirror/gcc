! { dg-do compile }
! PR fortran/66044
!
! Original code from Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
subroutine p
end subroutine p

entry e        ! { dg-error "Unexpected ENTRY statement" }
end

module m
  type t
      contains
      entry e  ! { dg-error "Unexpected ENTRY statement" }
  end type
end module m
