! { dg-do compile }
!
! PR 49196: [OOP] gfortran compiles invalid generic TBP: dummy arguments are type compatible
!
! Contributed by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>

module generic

  type :: a_type
   contains
     procedure :: a_subroutine
  end type a_type

  type,extends(a_type) :: b_type
   contains
     procedure :: b_subroutine
     generic :: g_sub => a_subroutine,b_subroutine  ! { dg-error "are ambiguous" }
  end type b_type

contains

  subroutine a_subroutine(this)
    class(a_type)::this
  end subroutine a_subroutine

  subroutine b_subroutine(this)
    class(b_type)::this
  end subroutine b_subroutine

end module generic 
