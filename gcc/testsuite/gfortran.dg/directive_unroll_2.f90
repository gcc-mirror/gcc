! { dg-do compile }
! { dg-options "-O -fdump-tree-cunroll-details -fdump-rtl-loop2_unroll-details" }
! Test that
! #pragma GCC unroll n
! works

subroutine test1(a)
  implicit NONE
  integer :: a(8)
  integer (kind=4) :: i
!GCC$ unroll 8
  DO i=1, 8, 1
    call dummy(a(i))
  ENDDO
! { dg-final { scan-tree-dump "12:.*: loop with 7 iterations completely unrolled" "cunroll" } } */
end subroutine test1

subroutine test2(a, n)
  implicit NONE
  integer :: a(n)
  integer (kind=1), intent(in) :: n
  integer (kind=4) :: i
!GCC$ unroll 8
  DO i=1, n, 1
    call dummy(a(i))
  ENDDO
! { dg-final { scan-rtl-dump "24:.: note: loop unrolled 7 times" "loop2_unroll" } }
end subroutine test2

subroutine test3(a, n)
  implicit NONE
  integer (kind=1), intent(in) :: n
  integer :: a(n)
  integer (kind=4) :: i
!GCC$ unroll 8
  DO i=n, 1, -1
    call dummy(a(i))
  ENDDO
! { dg-final { scan-rtl-dump "36:.: note: loop unrolled 7 times" "loop2_unroll" } }
end subroutine test3

subroutine test4(a, n)
  implicit NONE
  integer (kind=1), intent(in) :: n
  integer :: a(n)
  integer (kind=4) :: i
!GCC$ unroll 8
  DO i=1, n, 2
    call dummy(a(i))
  ENDDO
! { dg-final { scan-rtl-dump "48:.: note: loop unrolled 7 times" "loop2_unroll" } }
end subroutine test4
