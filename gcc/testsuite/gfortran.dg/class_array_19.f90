! { dg-do compile }
!
! PR 57285: [OOP] ICE on invalid: "gfc_array_dimen_size(): Bad dimension" due to SIZE intrinsic with invalid dim on CLASS dummy
!
! Contributed by Lorenz Hüdepohl <bugs@stellardeath.org>

  type type_t
  end type
contains
  subroutine foo(a)
    class(type_t), intent(in) :: a(:)
    type(type_t) :: c(size(a,dim=2))   ! { dg-error "is not a valid dimension index" }
  end subroutine
end  
