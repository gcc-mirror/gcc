! { dg-do compile }
!
! Test the fix for PR86242, in which the procedure pointer in 'tester'
! was being copied as if it were an allocatable class component.
!
! Contributed by <cfd@mnet-mail.de>
!
module test

   implicit none

   private
   public :: tester

   type :: wrapper
      integer(4) :: n
   end type wrapper

   type :: output
      real(8) :: dummy
   end type output

   type :: tester
      class(wrapper),  allocatable :: wrap
      procedure(proc1), pointer :: ptr => null()
   end type tester

   abstract interface
      function proc1(self) result(uc)
         import :: tester, output
         class(tester), intent(in) :: self
         class(output), allocatable :: uc
      end function proc1
   end interface

end module test

! Comment #2 from Janus Weil  <janus@gcc.gnu.org>
module test1

   implicit none

   type :: output
   end type

   type :: tester
      integer,  allocatable :: wrap
      procedure(proc1), pointer, nopass :: ptr
   end type

   interface                              ! Originally abstract
      function proc1() result(uc)
         import :: output
         class(output), allocatable :: uc ! Works if a pointer
      end function
   end interface

! PR82969 from Gerhard Steinmetz  <gscfq@t-online.de>
   type t
      real, allocatable :: x(:)
      procedure(f), nopass, pointer :: g
   end type
contains
   function f() result(z)
      class(t), allocatable :: z
   end

end module test1
