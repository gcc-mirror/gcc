! { dg-do run }
!
! Test the fix for PR66679.
!
! Contributed by Miha Polajnar  <polajnar.miha@gmail.com>
!
program main
  implicit none
  class(*), allocatable :: vec(:)
  integer :: var, ans(2)
  allocate(vec(2),source=[1_4, 2_4])

! This worked correctly.
  if (any (transfer(vec,[var],2) .ne. [1_4, 2_4])) stop 1

! This caused an ICE.
  if (any ([transfer(vec(1),[var]), transfer(vec(2),[var])] .ne. [1_4, 2_4])) stop 2
end program main
