! { dg-do compile }
!
! PR fortran/34689
!
! The following (cf. libgomp.fortran/appendix-a/a.33.3.f90)
! was rejected because the intent check missed a FL_FUNCTION
! for the result variable.
!
function test()
  implicit none
  integer :: test
  interface
    subroutine foo(a)
      integer, intent(inout) :: a
    end subroutine foo
  end interface
  call foo(test)
end function test
