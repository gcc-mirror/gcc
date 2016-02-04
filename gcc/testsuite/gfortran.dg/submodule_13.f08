! { dg-do compile }
!
! Checks the fix for PR68534 in which checks for the number
! failed if either the interface or the module procedure had
! no dummies.
!
! Reported on clf at:
! https://groups.google.com/forum/#!topic/comp.lang.fortran/-ZgbM5qkFmc
!
module m
  implicit none
    interface
      module subroutine foo()
      end subroutine foo

      module subroutine bar(i)
        integer, intent(inout) :: i
      end subroutine bar
   end interface
end module m

submodule(m) sm
contains
  module subroutine foo(i) ! { dg-error "Mismatch in number of MODULE PROCEDURE formal" }
    integer, intent(inout) :: i
    i = 42
  end subroutine foo

  module subroutine bar ! { dg-error "Mismatch in number of MODULE PROCEDURE formal" }
    print *, "bar"
  end subroutine bar
end submodule sm
