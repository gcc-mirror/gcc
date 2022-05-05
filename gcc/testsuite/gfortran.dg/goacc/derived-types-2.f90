! { dg-additional-options -Wuninitialized }

module bar
  type :: type1
     real(8), pointer, public :: p(:) => null()
  end type
  type :: type2
     class(type1), pointer :: p => null()
  end type
end module

subroutine foo (var)
   use bar
   type(type2), intent(inout) :: var
   !$acc enter data create(var%p%p)
end subroutine
