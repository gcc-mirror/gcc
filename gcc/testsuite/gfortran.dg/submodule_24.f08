! { dg-do compile }
! Test the fix for PR79447, in which the END PROCEDURE statement
! for MODULE PROCEDURE foo was not accepted.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
module foo_interface
  implicit none
  interface
    module subroutine foo()
    end subroutine
  end interface
end module foo_interface

submodule(foo_interface) foo_implementation
contains
    module procedure foo
    contains
      module subroutine bar()
      end subroutine
    end procedure
   !end subroutine ! gfortran accepted this invalid workaround
end submodule
