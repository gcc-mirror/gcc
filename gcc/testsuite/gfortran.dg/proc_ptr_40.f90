! { dg-do compile }
!
! PR 56261: [OOP] seg fault call procedure pointer on polymorphic array
!
! Contributed by Andrew Benson <abensonca@gmail.com>

  implicit none
  type :: nc
  end type
  external :: qq
  procedure(  ), pointer :: f1
  procedure(ff), pointer :: f2
  
  f1 => ff  ! { dg-error "Explicit interface required" }
  f2 => qq  ! { dg-error "Explicit interface required" }

contains

  subroutine ff (self)
    class(nc) :: self
  end subroutine

end
