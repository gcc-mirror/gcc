! { dg-do run }
! { dg-require-visibility "" }
!
! PR 36704: Procedure pointer as function result
!
! Original test case from James Van Buskirk.
!
! Adapted by Janus Weil <janus@gcc.gnu.org>

module store_subroutine
   implicit none

   abstract interface
      subroutine sub(i)
        integer, intent(inout) :: i
      end subroutine sub
   end interface

   procedure(sub), pointer, private :: psub => NULL()

contains

   subroutine set_sub(x)
      procedure(sub) x
      psub => x
   end subroutine set_sub

   function get_sub()
      procedure(sub), pointer :: get_sub
      get_sub => psub
   end function get_sub

end module store_subroutine

program test
   use store_subroutine
   implicit none
   procedure(sub), pointer :: qsub
   integer :: k = 1

   call my_sub(k)
   if (k/=3) call abort
   qsub => get_sub()
   call qsub(k)
   if (k/=9) call abort
end program test

recursive subroutine my_sub(j)
   use store_subroutine
   implicit none
   integer, intent(inout) :: j
   j = j*3
   call set_sub(my_sub)
end subroutine my_sub
