! { dg-do compile }
!
! Test the fix for PR86408.
!
! Contributed by Janus Weil  <janus@gcc.gnu.org>
!
module m

   implicit none

   type, abstract :: t
   contains
      procedure(ifc), deferred :: tbf
      procedure :: tbs
   end type

   abstract interface
      function ifc(x) result(str)
         import :: t
         class(t) :: x
         character(len=:), allocatable :: str
      end function
   end interface

contains

   subroutine tbs(x)
      class(t) :: x
      print *, x%tbf()
   end subroutine

end
