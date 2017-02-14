! { dg-do compile }
!
! PR 66366: [OOP] ICE on invalid with non-allocatable CLASS variable
!
! Contributed by Andrew Benson <abensonca@gmail.com>

module bug

  type :: t1d
   contains
     procedure :: interpolate => interp
  end type t1d

  type :: tff
     class(t1d) :: transfer  ! { dg-error "must be allocatable or pointer" }
  end type tff

contains

  double precision function interp(self)
    implicit none
    class(t1d), intent(inout) :: self
    return
  end function interp

  double precision function fvb(self)
    implicit none
    class(tff), intent(inout) :: self
    fvb=self%transfer%interpolate()  ! { dg-error "is not a member of" }
    return
  end function fvb

end module bug
