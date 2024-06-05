! { dg-additional-options "-fdump-tree-original" }

subroutine test1
  implicit none
  integer :: i
  !$omp unroll
  do i = 1,10
    call dummy(i)
  end do
end subroutine test1

subroutine test2
  implicit none
  integer :: i
  !$omp unroll full
  do i = 1,10
    call dummy(i)
  end do
end subroutine test2

! { dg-final { scan-tree-dump-times "#pragma omp unroll" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp unroll full" 1 "original" } }
