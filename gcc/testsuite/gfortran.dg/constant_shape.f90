! { dg-do compile }
!
! PR 78392: ICE in gfc_trans_auto_array_allocation, at fortran/trans-array.c:5979
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

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
  integer, dimension(get_i()) :: x  ! { dg-error "must have constant shape" }
  print *, size (x)
end
