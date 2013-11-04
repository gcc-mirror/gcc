! { dg-do compile }
!
! Issue came up during the review of PR fortran/58793
!
! Test for TS29113:2012's C407b.
! 
program test
  use iso_c_binding
  integer,target ::aa
  call up(c_loc(aa))
contains
  subroutine up(x)
    class(*) :: x
  end subroutine
  subroutine bar(x)
   type(*) :: x
   call up(x) ! { dg-error "Assumed-type actual argument at .1. requires that dummy argument 'x' is of assumed type" }
  end subroutine bar
end program test
