! { dg-do compile }
!
! PR fortran/57553 - bad error message for invalid use of STORAGE_SIZE
!
! Testcase contributed by Tobias Burnus

subroutine S (A)
  character(len=*), intent(in) :: A
  integer, parameter :: ESize = (storage_size(a) + 7) / 8 ! { dg-error "does not reduce to a constant" }
end
