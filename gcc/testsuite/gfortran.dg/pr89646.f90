! { dg-do compile }
! PR fortran/89646
! Original testcase contributed by Ian Harvey <ian_harvey at bigpond dot com>
!
! This code use to give spurious warnings about aliasing.
!
module m
   implicit none
   type :: t
   end type t
   contains
      ! To reproduce, both actual arguments must be TARGET, 
      ! both arguments must be of derived type.
      subroutine s
         type(t), target :: a(5)
         type(t), target :: b(5)
         call move(a, b)
      end subroutine s
      ! To reproduce, called procedure must be elemental.
      elemental subroutine move(x, y)
         type(t), intent(inout) :: x
         type(t), intent(out) :: y
      end subroutine move
end module m
