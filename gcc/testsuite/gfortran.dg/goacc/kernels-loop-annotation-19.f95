! { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-Wopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-do compile }

! Test that "acc kernels loop" directive causes annotation of the entire
! loop nest in the presence of a collapse clause.

subroutine f (a, b)

  implicit none
  real, intent (in), dimension(20) :: a
  real, intent (out), dimension(20) :: b
  integer :: k, l, m

!$acc kernels loop collapse(2)
  do k = 1, 20
    do l = 1, 20
      do m = 1, 20
	b(m) = a(m);
      end do
    end do
  end do

end subroutine f

! { dg-final { scan-tree-dump-times "acc loop .*collapse.2." 1 "original" } }
! { dg-final { scan-tree-dump-times "acc loop auto" 1 "original" } }

