! { dg-do compile }
! PR fortran/60126 - ICE on pointer rank remapping
! Based on testcase by Michel Valin <mfvalin at gmail dot com>

subroutine simple_bug_demo
  implicit none
  interface
     function offset_ptr_R4(nelements) result (dest)
       implicit none
       real, pointer, dimension(:) :: dest
       integer, intent(IN) :: nelements
     end function offset_ptr_R4
  end interface

  real, dimension(:,:), pointer :: R2D

  R2D(-2:2,-3:3) => offset_ptr_R4(100)
end
