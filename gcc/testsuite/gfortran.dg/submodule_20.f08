! { dg-do compile }
!
! Test the fix for PR77903
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
module one_module
  implicit none
  interface
    module function one()
    end function
    integer module function two()
    end function
  end interface
end module

submodule(one_module) one_submodule
  implicit none
contains
  integer module function one()  ! { dg-error "Type mismatch" }
    one = 1
  end function
  integer(8) module function two()  ! { dg-error "Type mismatch" }
    two = 2
  end function
end submodule

