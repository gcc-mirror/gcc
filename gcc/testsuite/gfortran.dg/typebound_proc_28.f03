! { dg-do compile }
!
! PR 56266: [OOP] ICE on invalid in gfc_match_varspec
!
! Contributed by Andrew Benson <abensonca@gmail.com>

module t

  implicit none

  type nc
   contains
     procedure :: encM => em
  end type nc

contains

  double precision function em(self)
    class(nc) :: self
    em=0.
  end function

  double precision function cem(c)
    type(nc) :: c
    cem=c(i)%encM()   ! { dg-error "Unclassifiable statement" }
  end function

end module
