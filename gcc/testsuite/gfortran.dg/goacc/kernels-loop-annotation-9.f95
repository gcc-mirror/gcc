! { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-Wopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-do compile }

! Test that a loop with a return statement in the body gives a hard
! error.

function f (a, b)
  implicit none

  real :: f
  real, intent (in), dimension (16) :: a, b

  integer :: i
  real :: t

  t = 0.0

!$acc kernels

  do i = 1, 16
    if (a(i) < 0 .or. b(i) < 0) then
      f = 0.0
      return	! { dg-error "invalid branch" }
    end if
    t = t + a(i) * b(i)
  end do

  f = t

!$acc end kernels

end function f
