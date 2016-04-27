! { dg-do compile }
!
! Check the fix for PR70031, where the 'module' prefix had to preceed
! 'function/subroutine' in the interface (or in the CONTAINS section.
!
! As reported by "Bulova" on
! https://groups.google.com/forum/#!topic/comp.lang.fortran/hE8LkVMhghQ
!
module test
  Interface
    Module Recursive Subroutine sub1 (x)
      Integer, Intent (InOut) :: x
    End Subroutine sub1
    module recursive function fcn1 (x) result(res)
      integer, intent (inout) :: x
      integer :: res
    end function
  End Interface
end module test

submodule(test) testson
  integer :: n = 10
contains
  Module Procedure sub1
    If (x < n) Then
        x = x + 1
        Call sub1 (x)
    End If
  End Procedure sub1
  module function fcn1 (x) result(res)
    integer, intent (inout) :: x
    integer :: res
    res = x - 1
    if (x > 0) then
      x = fcn1 (res)
    else
      res = x
    end if
  end function
end submodule testson

  use test
  integer :: x = 5
  call sub1(x)
  if (x .ne. 10) call abort
  x = 10
  if (fcn1 (x) .ne. 0) call abort
end
! { dg-final { cleanup-submodules "test@testson" } }
