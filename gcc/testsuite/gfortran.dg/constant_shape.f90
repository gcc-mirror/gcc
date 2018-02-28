! { dg-do compile }
!
! PR 78392: ICE in gfc_trans_auto_array_allocation, at fortran/trans-array.c:5979
!
! Contributed by Janus Weil <janus@gcc.gnu.org>
! Error message update with patch for PR fortran/83633
!
module mytypes
   implicit none
 contains
   pure integer function get_i ()
     get_i = 13
   end function
end module

program test
  use mytypes
  implicit none
  integer, dimension(get_i()) :: x  ! { dg-error "array with nonconstant bounds" }
  print *, size (x)                 ! { dg-error "has no IMPLICIT type" }
end
