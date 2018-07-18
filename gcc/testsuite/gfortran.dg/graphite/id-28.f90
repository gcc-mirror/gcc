! Verify we elide modulo operations we cannot represent
module OPMATRIX_MODULE
   implicit none
   type opmatrix_type
   real(kind=kind(1.0d0)), dimension(:,:), pointer :: restricted
   end type
   interface zero_
      module procedure zero
   end interface
contains
   subroutine zero(self)
      type(opmatrix_type) :: self
      self%restricted = 0.0d0
   end subroutine
end
