! { dg-do compile }

! Test that
! #pragma GCC unroll n
! rejects invalid n and improper use

subroutine wrong1(n)
  implicit NONE
  integer (kind=1), intent(in) :: n
  integer (kind=4) :: i
!GCC$ unroll 999999999 ! { dg-error "non-negative integral constant less than" }
  DO i=0, n, 1
    call dummy1(i)
  ENDDO
end subroutine wrong1

subroutine wrong2(a, b, n)
  implicit NONE
  integer (kind=1), intent(in) :: n
  integer :: a(n), b(n)
  integer (kind=4) :: i
!GCC$ unroll -1 ! { dg-error "non-negative integral constant less than" }
  DO i=1, n, 2
    call dummy2(a(i), b(i), i)
  ENDDO
end subroutine wrong2

subroutine wrong3(a, b, n)
  implicit NONE
  integer (kind=1), intent(in) :: n
  integer :: a(n), b(n)
  integer (kind=4) :: i
!GCC$ unroll 8
  write (*,*) "wrong"! { dg-error "directive does not commence a loop" }
  DO i=n, 1, -1
    call dummy2(a(i), b(i), i)
  ENDDO
end subroutine wrong3
