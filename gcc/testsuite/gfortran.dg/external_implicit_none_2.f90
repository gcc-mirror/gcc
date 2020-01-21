! { dg-do compile }
!
! PR fortran/93309
!
module m
  implicit none(external)
contains
  subroutine s
    implicit none(external) ! OK
  end subroutine
end module

module m2
  implicit none(external)
contains
  subroutine s
    call foo(1)  ! { dg-error "not explicitly declared" }
  end subroutine
end module

module m3
  implicit none(external)
contains
  subroutine s
    implicit none(external) ! OK
    implicit none(external) ! { dg-error "Duplicate IMPLICIT NONE statement" }
  end subroutine
end module
