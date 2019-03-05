! { dg-do compile }
module base
   implicit none
contains
   subroutine summation(i)
      integer, intent(in) :: i
   end subroutine
end module

module extended
   use base
   implicit none
contains
   subroutine summation()  ! { dg-error "is already defined" }
   end subroutine          ! { dg-error "Expecting END MODULE statement" }
end module
! { dg-prune-output "is already defined at" }
