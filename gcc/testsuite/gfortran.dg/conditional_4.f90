! { dg-do compile }
! { dg-options "-std=f2023" }
program conditional_resolve
  implicit none
  integer :: i = 42
  integer, parameter :: ucs4 = selected_char_kind('ISO_10646')
  character(kind=1) :: k1 = "k1"
  character(kind=ucs4) :: k4 = "k4"
  integer, dimension(1) :: a_1d
  integer, dimension(1, 1) :: a_2d
  logical :: l1(2)
  integer :: i1(2)
  type :: Point
    real :: x = 0.0
  end type Point
  type(Point) :: p1, p2

  i = (l1 ? 1 : -1) ! { dg-error "Condition in conditional expression must be a scalar logical" }
  i = (i ? 1 : -1) ! { dg-error "Condition in conditional expression must be a scalar logical" }
  i = (i /= 0 ? 1 : "oh no") ! { dg-error "must have the same declared type" }
  i = (i /= 0 ? k1 : k4) ! { dg-error "must have the same kind parameter" }
  i = (i /= 0 ? a_1d : a_2d) ! { dg-error "must have the same rank" }
  p1 = (i /= 0 ? p1 : p2) ! { dg-error "Sorry, only integer, logical, real, complex and character types are currently supported for conditional expressions" }
  i1 = (i /= 0 ? i1 : i1 + 1) ! { dg-error "Sorry, array is currently unsupported for conditional expressions" }
end program conditional_resolve
