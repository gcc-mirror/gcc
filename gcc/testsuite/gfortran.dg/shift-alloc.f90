! { dg do-run }
! PR 22144: eoshift1, eoshift3 and cshift1 used to lack memory
! allocation, which caused the writes to segfault.
program main
  implicit none
  integer, dimension (:,:),allocatable :: a
  integer, dimension (3) :: sh, bo
  character(len=80) line1, line2
  integer :: i
  
  allocate (a(3,3))
  a = reshape((/(i,i=1,9)/),shape(a))
  sh = (/ 2, -1, -2 /)
  bo = (/ -3, -2, -1 /)
  write(unit=line1,fmt='(10I5)') cshift(a, shift=sh)
  write(unit=line1,fmt='(10I5)') eoshift(a, shift=sh)
  write(unit=line1,fmt='(10I5)') eoshift(a, shift=sh, boundary=bo)
end program main
