! { dg-do compile }
! Test the fix for PR25099, in which conformance checking was not being
! done for elemental subroutines and therefore for interface assignments.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
module elem_assign
   implicit none
   type mytype
      integer x
   end type mytype
   interface assignment(=)
      module procedure myassign
   end interface assignment(=)
   contains
      elemental subroutine myassign(x,y)
         type(mytype), intent(out) :: x
         type(mytype), intent(in) :: y
         x%x = y%x
      end subroutine myassign
end module elem_assign

   use elem_assign
   integer :: I(2,2),J(2)
   type (mytype) :: w(2,2), x(4), y(5), z(4)
! The original PR
   CALL S(I,J) ! { dg-error "Incompatible ranks in elemental procedure" }
! Check interface assignments
   x = w       ! { dg-error "Incompatible ranks in elemental procedure" }
   x = y       ! { dg-error "Different shape for elemental procedure" }
   x = z
CONTAINS
   ELEMENTAL SUBROUTINE S(I,J)
     INTEGER, INTENT(IN) :: I,J
   END SUBROUTINE S
END
