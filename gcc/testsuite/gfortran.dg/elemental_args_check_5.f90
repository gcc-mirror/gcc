! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
!
  type t
  end type t
  type t2
  end type t2
contains
elemental subroutine foo0(v) ! OK
  class(t), intent(in) :: v
end subroutine

elemental subroutine foo1(w) ! { dg-error "Argument 'w' of elemental procedure at .1. cannot have the ALLOCATABLE attribute" }
  class(t), allocatable, intent(in) :: w
end subroutine

elemental subroutine foo2(x) ! { dg-error "Argument 'x' of elemental procedure at .1. cannot have the POINTER attribute" }
  class(t), pointer, intent(in) :: x
end subroutine

elemental subroutine foo3(y) ! { dg-error "Coarray dummy argument 'y' at .1. to elemental procedure" }
  class(t2), intent(in) :: y[*]
end subroutine

elemental subroutine foo4(z) ! { dg-error "Argument 'z' of elemental procedure at .1. must be scalar" }
  class(t), intent(in) :: z(:)
end subroutine

end
