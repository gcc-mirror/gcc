! We want to check that ISHFT evaluates its arguments only once
!
! { dg-do run }
! { dg-options "-fdump-tree-original" }

program test

  if (leadz (foo()) /= bit_size(0) - 1) STOP 1
  if (leadz (foo()) /= bit_size(0) - 2) STOP 2
  if (trailz (foo()) /= 0) STOP 3
  if (trailz (foo()) /= 2) STOP 4
  if (trailz (foo()) /= 0) STOP 5
  if (trailz (foo()) /= 1) STOP 6

contains
  
  integer function foo ()
    integer, save :: i = 0
    i = i + 1
    foo = i
  end function

end program

! The regexp "foo ()" should be seen once in the dump:
!   -- once in the function definition itself
!   -- plus as many times as the function is called
!
! { dg-final { scan-tree-dump-times "foo *\\\(\\\)" 7 "original" } }
