! { dg-do compile }
! Tests the fix for PR33945, the host association of overloaded_type_s
! would be incorrectly blocked by the use associated overloaded_type.
!
! Contributed by Jonathan Hogg  <J.Hogg@rl.ac.uk>
!
module dtype
   implicit none

   type overloaded_type
      double precision :: part
   end type

   interface overloaded_sub
      module procedure overloaded_sub_d
   end interface

contains
   subroutine overloaded_sub_d(otype)
      type(overloaded_type), intent(in) :: otype

      print *, "d type = ", otype%part
   end subroutine
end module

module stype
   implicit none

   type overloaded_type
      real :: part
   end type

   interface overloaded_sub
      module procedure overloaded_sub_s
   end interface

contains
   subroutine overloaded_sub_s(otype)
      type(overloaded_type), intent(in) :: otype

      print *, "s type = ", otype%part
   end subroutine
end module

program test
   use stype, overloaded_type_s => overloaded_type
   use dtype, overloaded_type_d => overloaded_type
   implicit none

   type(overloaded_type_s) :: sval
   type(overloaded_type_d) :: dval

   sval%part = 1
   dval%part = 2

   call fred(sval, dval)

contains
   subroutine fred(sval, dval)
      use stype

      type(overloaded_type_s), intent(in) :: sval  ! This caused an error
      type(overloaded_type_d), intent(in) :: dval

      call overloaded_sub(sval)
      call overloaded_sub(dval)
   end subroutine
end program
! { dg-final { cleanup-modules "stype dtype" } }
