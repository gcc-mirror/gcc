! { dg-do run }
!
! Check that MVBITS is available for the largest integer kind (PR 67140)
!
program test
  use iso_fortran_env
  integer, parameter :: k = integer_kinds(size(integer_kinds))

  integer(kind=k) :: i = 6
  call mvbits(7_k,2,2,i,0)
  if (i /= 5) STOP 1
end
