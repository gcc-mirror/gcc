! { dg-do compile }
! PR fortran/60560
!
! Original test case by Marco Restelli.

module mstr
  implicit none
contains
  subroutine sub(s)
    character(len=*),      allocatable, intent(out) :: s(:)
    character(len=len(s)), allocatable              :: s_tmp(:)
    allocate(s_tmp(5))
    allocate(s(size(s_tmp)))          ! OK
    allocate(s_tmp(5),s(size(s_tmp))) ! { dg-error "same ALLOCATE statement" }
    allocate(s_tmp(5),s(len(s_tmp)))  ! { dg-error "same ALLOCATE statement" }
  end subroutine sub
end module mstr
