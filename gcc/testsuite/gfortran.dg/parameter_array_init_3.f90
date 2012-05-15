! { dg-do compile }
! Test the fix for PR34476 in which an 'out of bounds' error would be
! generated for the array initializations AND the implicit index 'i'
! would be rejected.
!
! Reported by Tobias Burnus <burnus@gcc.gnu.org> following a thread
! on comp.lang.fortran (see PR)
!
module abuse_mod
   implicit none
   integer i
   character(8), parameter :: HEX1 = '40490FDB'
   integer(1), parameter :: MSKa1(len(HEX1)) =  [(1,i=1,len(HEX1))]
   integer(1), parameter :: ARR1(len(HEX1)) = [( MSKa1(i), i=1,len(HEX1) )]
end module abuse_mod
