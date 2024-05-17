! { dg-do compile }
! Test fix for regression caused by r14-9489 - valid code only.
! Contributed by Harald Anlauf  <anlauf@gcc.gnu.org>
!
module p
  implicit none
contains
  subroutine foo
    class(*), allocatable :: c
    c = 'abc'
    select type (c)
    type is (character(*))
      if (c .ne. 'abc') stop 1
! Regression caused ICE here - valid substring reference
      if (c(2:2) .ne. 'b') stop 2
    end select
  end
  subroutine bar  ! This worked correctly
    class(*), allocatable :: c(:)
    c = ['abc','def']
    select type (c)
    type is (character(*))
      if (any (c .ne. ['abc','def'])) stop 3
      if (any (c(:)(2:2) .ne. ['b','e'])) stop 4
    end select
  end
end module p

  use p
  call foo
  call bar
end
