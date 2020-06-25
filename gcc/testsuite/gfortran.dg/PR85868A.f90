! { dg-do run }
!
! PR fortran/85868
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
! 

program test
  
  implicit none
  
  integer, parameter :: e(*) = [1, 1, -1, -1, 0, 0, 1]
  
  integer, pointer :: t(:), u(:)
  integer          :: i
  
  allocate (t(-1:5))
  do i = -1, 5
    t(i) = i
  end do
  call p (t, e(1))     ! Pointer with lower bound = -1 from allocation
  u     => t           ! Pointer assignment sets same lower bound
  call p (u, e(2))
  !
  u     => t(:)        ! Pointer assignment with implicit lower bound (1)
  call p (u, e(3))
  call p (t(:), e(4))  ! Full array, behaves the same
  !
  call p (t(0:), e(5)) ! Array section
  u     => t(0:)       ! Pointer assignment with implicit lower bound (1)
  call p (u, e(6))
  u(0:) => t(0:)       ! Pointer assignment with given lower bound (0)
  call p (u, e(7))
  stop
  
contains
  
  subroutine p (a, v)
    integer, pointer, intent(in) :: a(:)
    integer,          intent(in) :: v
    
    if(a(1)/=v) stop 1001
    return
  end subroutine p
  
end program test

