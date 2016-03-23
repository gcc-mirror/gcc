! { dg-do compile }

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
  k = 0
  !$acc loop reduction (+:k)
  do i = 1, n
     k = k + 1
  end do
  !$acc end parallel
end program test
