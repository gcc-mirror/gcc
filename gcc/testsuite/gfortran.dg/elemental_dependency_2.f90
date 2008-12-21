! { dg-do compile }
!
! PR fortran/38487
! Spurious warning on pointers as elemental subroutine actual arguments
!
! Contributed by Harald Anlauf <anlauf@gmx.de>

module gfcbug82
  implicit none
  type t
    real, pointer :: q(:) =>NULL()
    real, pointer :: r(:) =>NULL()
  end type t
  type (t), save :: x, y
  real, dimension(:), pointer, save :: a => NULL(), b => NULL()
  real, save :: c(5), d
contains
  elemental subroutine add (q, r)
    real, intent (inout) :: q
    real, intent (in)    :: r
    q = q + r
  end subroutine add

  subroutine foo ()
      call add (y% q, x% r)
      call add (y% q, b   )
      call add (a   , x% r)
      call add (a   , b   )
      call add (y% q, d   )
      call add (a   , d   )
      call add (c   , x% r)
      call add (c   , b   )
  end subroutine foo
end module gfcbug82

! { dg-final { cleanup-modules "gfcbug82" } }
