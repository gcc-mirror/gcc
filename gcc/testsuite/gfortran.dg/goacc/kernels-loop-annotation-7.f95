! { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-Wopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-do compile }

! Test that a loop with a random function call in the body cannot
! be annotated. 


function f (a, b)
  implicit none

  real :: f
  real, intent (in), dimension (16) :: a, b

  integer :: i
  real :: t

  interface
    function g (x)
      real :: g
      real, intent (in) :: x
    end function g

    subroutine h (x)
      real, intent (in) :: x
    end subroutine h
  end interface

  t = 0.0

!$acc kernels
  do i = 1, 16
    t = t + g (a(i) * b(i))  ! { dg-warning "Function call" }
  end do

  do i = 1, 16
    call h (t) ! { dg-warning "Subroutine call" }
    t = t + a(i) * b(i)
  end do

  f = t
!$acc end kernels

end function f

! { dg-final { scan-tree-dump-times "acc loop private.* auto" 0 "original" } }

