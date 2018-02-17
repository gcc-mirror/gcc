! { dg-do run }
!
! Test the fix for PR83611, in which the assignment caused a
! double free error and the initialization of 'foo' was not done.
!
module pdt_m
  implicit none
  type :: vec(k)
     integer, len :: k=3
     integer :: foo(k)=[1,2,3]
  end type vec
end module pdt_m

program test_pdt
  use pdt_m
  implicit none
  type(vec) :: u,v
  if (any (u%foo .ne. [1,2,3])) STOP 1
  u%foo = [7,8,9]
  v = u
  if (any (v%foo .ne. [7,8,9])) STOP 2
end program test_pdt
