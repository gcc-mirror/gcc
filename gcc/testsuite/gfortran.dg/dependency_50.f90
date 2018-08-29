! { dg-do  run }
! PR 83224 - dependency mishandling with an array constructor
! Original test case by Urban Jost
program dusty_corner
  implicit none
  character(len=:),allocatable :: words(:)

  words=[character(len=3) :: 'one', 'two']
  words=[character(len=5) :: words, 'three']
  if (any(words /= [ "one  ", "two  ", "three"])) STOP 1

end program dusty_corner
