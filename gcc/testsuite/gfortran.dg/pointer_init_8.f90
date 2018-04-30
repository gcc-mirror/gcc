! { dg-do run }
!
! PR 57306: [OOP] ICE on valid with class pointer initialization
!
! Contributed by Andrew Benson <abensonca@gmail.com>

module m
  type :: c
  end type c
  type, extends(c) :: d
  end type d
  type(c), target :: x
  type(d), target :: y
end module m

 use m
  class(c), pointer :: px => x
  class(c), pointer :: py => y

  if (.not. associated(px, x))   STOP 1
  if (.not. same_type_as(px, x)) STOP 2
  if (.not. associated(py, y))   STOP 3
  if (.not. same_type_as(py, y)) STOP 4
end 
