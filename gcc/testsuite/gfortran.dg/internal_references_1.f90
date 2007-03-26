! { dg-do compile }
! This tests the patch for PRs 24327, 25024 & 25625, which
! are all connected with references to internal procedures.
! This is a composite of the PR testcases; and each is
! labelled by PR.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
! PR25625 - would neglect to point out that there were 2 subroutines p.
module m
  implicit none
contains

  subroutine p (i)   ! { dg-error "is already defined" }
    integer :: i
  end subroutine

  subroutine p (i)   ! { dg-error "is already defined" }
   integer :: i
  end subroutine
end module
!
! PR25124 - would happily ignore the declaration of foo in the main program.
program test
real :: foo, x      ! { dg-error "explicit interface and must not have attributes declared" }
x = bar ()          ! This is OK because it is a regular reference.
x = foo ()
contains
    function foo () ! { dg-error "explicit interface and must not have attributes declared" }
      foo = 1.0
    end function foo
    function bar ()
      bar = 1.0
    end function bar
end program test
! { dg-final { cleanup-modules "m" } }
