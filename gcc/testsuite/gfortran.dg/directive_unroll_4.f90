! { dg-do compile }
! { dg-options "-O2 -funroll-all-loops -fdump-rtl-loop2_unroll-details -fdump-tree-cunrolli-details" }
! Test that
! #pragma GCC unroll n
! works

subroutine test1(a)
  implicit NONE
  integer :: a(8)
  integer (kind=4) :: i
!GCC$ unroll 0
  DO i=1, 8, 1
    call dummy(a(i))
  ENDDO
end subroutine test1

subroutine test2(a, n)
  implicit NONE
  integer :: a(n)
  integer (kind=1), intent(in) :: n
  integer (kind=4) :: i
!GCC$ unroll 0
  DO i=1, n, 1
    call dummy(a(i))
  ENDDO
end subroutine test2

! { dg-final { scan-tree-dump "Not unrolling loop .: user didn't want it unrolled completely" "cunrolli" } } */
! { dg-final { scan-rtl-dump-times "Not unrolling loop, user didn't want it unrolled" 2 "loop2_unroll" } } */
