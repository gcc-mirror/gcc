! { dg-do compile }
!
! Test the fix for PR79402, in which the module procedure 'fun1' picked
! up a spurious symbol for the dummy 'n' in the specification expression
! for the result 'y'.
!
! Contributed by Chris Coutinho  <chrisbcoutinho@gmail.com>
!
module mod
  interface myfun
    module function fun1(n) result(y)
      integer,  intent(in)    :: n
      real, dimension(n)  :: y
    end function fun1
  end interface myfun

end module mod

submodule (mod) submod
contains
  module procedure fun1
    integer :: i
    y = [(float (i), i = 1, n)]
  end procedure fun1
end submodule

  use mod
  print *, fun1(10)
end
