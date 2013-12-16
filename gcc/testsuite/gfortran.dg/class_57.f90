! { dg-do compile }
!
! PR 59502: [OOP] ICE on invalid on pointer assignment to non-pointer CLASS
!
! Contributed by Andrew Benson <abensonca@gmail.com>

  implicit none

  type :: d
  end type

  type :: p
    class(d) :: cc   ! { dg-error "must be allocatable or pointer" }
  end type

contains

  function pc(pd)
    type(p) :: pc
    class(d), intent(in), target :: pd
    pc%cc => pd   ! { dg-error "Non-POINTER in pointer association context" }
  end function

end
