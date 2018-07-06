! { dg-do compile }
!
! PR fortran/45170
! PR fortran/52158

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
   character(:), allocatable :: temp
   x%ppt => deferred_len
   temp = deferred_len()
   if ("abc" /= temp) STOP 1
 end subroutine doIt
end module test

use test
call doIt ()
end
