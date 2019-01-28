! { dg-do run }
! PR 68426 - simplification used to fail.
  module m
    implicit none
    type t
      integer :: i
    end type t
    type(t), dimension(2), parameter :: a1  = (/ t(1), t(2) /)
    type(t), dimension(1), parameter :: c = spread ( a1(1), 1, 1 )
  end module m


program main
  use m
  if (c(1)%i /= 1) stop 1
end program main
