! { dg-do run }
! Tests the check for PR31292, in which the module procedure
! statement would put the symbol for assign_t in the wrong
! namespace and this caused the interface checking to fail.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module chk_gfortran
   implicit none
   type t
      integer x
   end type t
   contains
      function is_gfortran()
         logical is_gfortran
         interface assignment(=)
            module procedure assign_t
         end interface assignment(=)
         type(t) y(3)

         y%x = (/1,2,3/)
         y = y((/2,3,1/))
         is_gfortran = y(3)%x == 1
      end function is_gfortran

      elemental subroutine assign_t(lhs,rhs)
         type(t), intent(in) :: rhs
         type(t), intent(out) :: lhs

         lhs%x = rhs%x
      end subroutine assign_t
end module chk_gfortran

program fire
   use chk_gfortran
   implicit none
   if(.not. is_gfortran()) STOP 1
end program fire
