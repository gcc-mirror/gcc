! test for private variables in a reduction clause

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" } for
! testing/documenting aspects of that functionality.

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
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  do i = 1, n
     k = k + 1
  end do
  !$acc end parallel


!  !$acc serial private (k) reduction (+:k)
!  do i = 1, n
!     k = k + 1
!  end do
!  !$acc end serial

  !$acc serial private (k)
  k = 0
  !$acc loop reduction (+:k)
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  do i = 1, n
     k = k + 1
  end do
  !$acc end serial

end program test
