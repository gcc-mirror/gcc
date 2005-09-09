! { dg-do run }
! This program tests that use associated common blocks work.
!
! provided by Paul Thomas - pault@gcc.gnu.org
!
module m1
  common /x/ a
end module m1
module m2
  common /x/ a
end module m2

subroutine foo ()
  use m2
  if (a.ne.99.0) call abort ()
end subroutine foo

program collision
  use m1
  use m2, only: b=>a
  b = 99.0
  call foo ()
end program collision

