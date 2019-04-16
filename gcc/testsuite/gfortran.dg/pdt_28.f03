! { dg-do run }
! { dg-options "-fbounds-check" }
!
! Test the fix for PR83731, where the following failed on the check for the
! value of the parameter 'k'.
!
! Contributed by Berke Durak  <berke.durak@gmail.com>
!
module pdt_m
  implicit none
  type :: vec(k)
     integer, len :: k=10
     integer :: foo(k)
  end type vec
contains
  function total(a)
    type(vec(k=*)), intent(in) :: a ! Would compare with the default initializer.
    integer :: total
    
    total=sum(a%foo)
  end function total
end module pdt_m

program test_pdt
  use pdt_m
  implicit none
  type(vec(k=123)) :: u

  u%foo=1
  if (total(u) .ne. u%k) STOP 1
end program test_pdt
