! { dg-do compile }
! <http://news.gmane.org/find-root.php?message_id=%3C563B78B5.5090506%40acm.org%3E>
! { dg-xfail-if "TODO" { *-*-* } }

! test for private variables in a reduction clause

program test
  implicit none
  integer, parameter :: n = 100
  integer :: i, k

!  !$acc parallel private (k) reduction (+:k)
!  do i = 1, n
!     k = k + 1
!  end do
!  !$acc end parallel

  !$acc parallel private (k)
  !$acc loop reduction (+:k)
  do i = 1, n
     k = k + 1
  end do
  !$acc end parallel
end program test
