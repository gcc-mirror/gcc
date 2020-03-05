! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
subroutine foo (a, b, c, n)
  implicit none
  real a(*), b(*), c(*)
  integer :: i, n
  external bar
!DIR$ unroll (4)
!GCC$ unroll 4
  do i = 1, n
     a(i) = b(i) + c(i)
  end do
!DIR$ ivdep
!GCC$ ivdep
  do i = 1, n
     a(i) = b(i) + c(i)
  end do
!DIR$ vector
!GCC$ vector
  do i = 1, n
     a(i) = b(i) + c(i)
  end do
!DIR$ novector
!GCC$ novector
  do i = 1, n
     a(i) = b(i) + c(i)
  end do
!GCC$ ivdep
!GCC$ vector
  do i = 1, n
     a(i) = b(i) + c(i)
  end do
!DIR$ noinline
!GCC$ noinline          ! { dg-warning "Unclassifiable GCC directive" }
  call bar (a)
end subroutine foo
! { dg-final { scan-tree-dump-times "ANNOTATE_EXPR" 6 "original" } }
