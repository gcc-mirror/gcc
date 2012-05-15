! { dg-do compile }
!
! This tests the fix for PR36325, which corrected for the fact that a
! specific or generic INTERFACE statement implies the EXTERNAL attibute.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module a
  interface
    subroutine foo
    end subroutine
  end interface
  external foo  ! { dg-error "Duplicate EXTERNAL attribute" }
end module

module b
  interface
    function sin (x)
      real :: sin, x
    end function
  end interface
  intrinsic sin  ! { dg-error "EXTERNAL attribute conflicts with INTRINSIC attribute" }
end module

! argument checking was not done for external procedures with explicit interface
program c
  interface
    subroutine bar(x)
      real :: x
    end subroutine
  end interface
  call bar()  ! { dg-error "Missing actual argument" }
end program
