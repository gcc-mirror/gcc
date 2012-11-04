! { dg-do compile }
!
! PR 55199: [OOP] Equivalenced variable has wrong type when used with generic member function
!
! Contributed by Rich Townsend <townsend@astro.wisc.edu>

module assoc_err_m
  implicit none
  type :: foo_t
   contains
     procedure :: func_1
     generic   :: func => func_1
  end type
contains
  real function func_1 (this)
    class(foo_t), intent(in) :: this
  end function
end module

program assoc_err
  use assoc_err_m
  implicit none
  type(foo_t) :: f
  associate(b => f%func())
    print *, 1. + b
  end associate
end program

! { dg-final { cleanup-modules "assoc_err_m" } }
