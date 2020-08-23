! { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-Wopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-do compile }

! Test that a loop with a case construct in the body can be annotated. 

function f (a, b)
  implicit none

  real :: f
  real, intent (in), dimension (16) :: a, b

  integer :: i
  real :: t

!$acc kernels

  do i = 1, 16
    select case (i)
      case (1)
        t = a(i) * b(i)
      case default
        t = t + a(i) * b(i)
    end select
  end do

  f = t

!$acc end kernels

end function f

! { dg-final { scan-tree-dump-times "acc loop auto" 1 "original" } }
