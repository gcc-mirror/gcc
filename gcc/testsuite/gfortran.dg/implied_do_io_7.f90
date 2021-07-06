! { dg-do run }
! PR 100227 - this was falsely optimized, leading to nonsense  results.
! Original test case by "Mathieu".

program p
  implicit none
  integer, parameter :: nbmode = 3
  integer :: k
  real    :: mass(nbmode*2)
  character (len=80) :: line
  do k = 1, nbmode*2
     mass(k) = k
  end do
  write (unit=line,fmt='(*(F6.2))') (mass(k+k), k=1,nbmode)
  if (line /= '  2.00  4.00  6.00') stop 1
end program
