! { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-Wopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-do compile }

! Test that a loop with calls to intrinsics in the body can be annotated.

subroutine f (n, input, out1, out2)
  implicit none
  integer :: n
  integer, intent (in), dimension (n) :: input
  integer, intent (out), dimension (n) :: out1, out2

  integer :: i

!$acc kernels

  do i = 1, n
      out1(i) = min (i, input(i))
      out2(i) = not (input(i))
  end do
!$acc end kernels

end subroutine f

! { dg-final { scan-tree-dump-times "acc loop auto" 1 "original" } }
