! { dg-do compile }
! PR fortran/106946
! ICE in resolve_component on invalid CLASS declaration with missing comma.
!
! The bad declarations below should diagnose and continue without leaving
! behind dangling CLASS container symbols in the surrounding namespace.

program p
  type :: u
  end type

  type :: v
  end type

  type :: w
  end type

  type :: t1
    class(u), allocatable :: a b  ! { dg-error "Syntax error in data declaration" }
  end type

  type :: t2
    class(v), pointer :: p q  ! { dg-error "Syntax error in data declaration" }
  end type

  type :: t3
    class(w), allocatable :: ok
    class(w), allocatable :: x y  ! { dg-error "Syntax error in data declaration" }
  end type
end
