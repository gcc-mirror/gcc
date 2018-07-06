! { dg-do run }
! Tests the fix for PR32047, in which the null agument
! function for the character length would cause an ICE.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org >
!
module test1
  implicit none
contains
  character(f()) function test2() result(r)
    interface
      pure function f()
        integer f
      end function f
    end interface
    r = '123'
  end function test2
end module test1

pure function f()
  integer :: f
  f = 3
end function f

program test
  use test1
  implicit none
  if(len (test2()) /= 3) STOP 1
  if(test2() /= '123') STOP 2
end program test
