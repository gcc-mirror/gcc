! { dg-do compile }
!
! Test the fix for PR78474.
!
! Contributed by Nicholas Brearly  <nick.brealey@cobham.com>
!
module mtop
  implicit none
  real :: r
  interface
    module subroutine sub1()
    end subroutine
  end interface
  interface
    module subroutine sub2()
    end subroutine
  end interface
  interface
    module subroutine sub3()
    end subroutine
  end interface
end module mtop

submodule (mtop) submod
  implicit none
  real :: s
contains
  module subroutine sub1
    r = 0.0
  end subroutine sub1
end

submodule (mtop:submod) subsubmod
contains
  module subroutine sub2
    r = 1.0
    s = 1.0
  end subroutine sub2
end

submodule (mtop:submod:subsubmod) subsubsubmod ! { dg-error "Syntax error in SUBMODULE statement" }
contains
  module subroutine sub3
    r = 2.0
    s = 2.0
  end subroutine sub3
end
