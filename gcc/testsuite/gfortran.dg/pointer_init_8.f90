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

  if (.not. associated(px, x))   call abort()
  if (.not. same_type_as(px, x)) call abort()
  if (.not. associated(py, y))   call abort()
  if (.not. same_type_as(py, y)) call abort()
end 

! { dg-final { cleanup-modules "m" } }
