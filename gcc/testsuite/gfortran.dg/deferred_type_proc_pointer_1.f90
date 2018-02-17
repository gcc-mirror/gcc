! { dg-do compile }
!
! PR fortran/45170
! PR fortran/52158
!
! Contributed by Tobias Burnus

module test
 implicit none
 type t
   procedure(deferred_len), pointer, nopass :: ppt
 end type t
contains
 function deferred_len()
   character(len=:), allocatable :: deferred_len
   deferred_len = 'abc'
 end function deferred_len
 subroutine doIt()
   type(t) :: x
   x%ppt => deferred_len
   if ("abc" /= x%ppt()) STOP 1
 end subroutine doIt
end module test

use test
call doIt ()
end
