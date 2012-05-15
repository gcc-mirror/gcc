! { dg-do run }
!
! PR fortran/41850
!
module test_module
  implicit none
contains
  subroutine sub2(a)
    implicit none
    real,allocatable,intent(out),optional :: a(:)
    if(present(a)) then
      if(allocated(a)) call abort()
      allocate(a(1))
      a(1) = 5
    end if
  end subroutine sub2
  subroutine sub1(a)
    implicit none
    real,allocatable,intent(out),optional :: a(:)
!    print *,'in sub1'
    call sub2(a)
    if(present(a)) then
      if(a(1) /= 5) call abort()
    end if
  end subroutine sub1
end module test_module

program test
  use test_module
  implicit none
  real, allocatable :: x(:)
  allocate(x(1))
  call sub1()
  x = 8
  call sub1(x)
  if(x(1) /= 5) call abort()
end program
