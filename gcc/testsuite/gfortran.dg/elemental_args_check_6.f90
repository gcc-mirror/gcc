! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/52013
!
type t
end type t
contains
  elemental subroutine f(x) 
    class(t), intent(inout) :: x ! Valid
  end subroutine
  elemental subroutine g(y) ! { dg-error "Coarray dummy argument 'y' at .1. to elemental procedure" }
    class(t), intent(inout) :: y[*]
  end subroutine
end
