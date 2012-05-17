! { dg-do compile }
! Test the fix for PR25135 in which the ambiguity between subroutine
! foo in m_foo and interface foo in m_bar was not recognised.
!
!Contributed by Yusuke IGUCHI <iguchi@coral.t.u-tokyo.ac.jp>
!
module m_foo
contains
  subroutine foo
    print *, "foo"
  end subroutine
end module

module m_bar
  interface foo
    module procedure bar
  end interface
contains
  subroutine bar
    print *, "bar"
  end subroutine
end module

use m_foo
use m_bar

call foo ! { dg-error "is an ambiguous reference" } 
end
