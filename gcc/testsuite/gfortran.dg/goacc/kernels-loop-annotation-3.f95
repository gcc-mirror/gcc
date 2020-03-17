! { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-Wopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-do compile }

! Test that a loop with a conditional in the body can be annotated. 

function f (a, b)
  implicit none

  real :: f
  real, intent (in), dimension (16) :: a, b

  integer :: i
  real :: t

  t = 0.0

!$acc kernels

  do i = 1, 16
    if (a(i) > 0 .and. b(i) > 0) then
      t = t + a(i) * b(i)
    end if
  end do

  f = t

!$acc end kernels

end function f

! { dg-final { scan-tree-dump-times "acc loop private.* auto" 1 "original" } }
