! { dg-do compile }
! Tests the fix for a further regression caused by the
! fix for PR28788, as noted in reply #9 in the Bugzilla
! entry by Martin Reinecke <martin@mpa-garching.mpg.de>.
! The problem was caused by certain types of references
! that point to a deleted derived type symbol, after the
! type has been associated to another namespace. An
! example of this is the specification expression for x
! in subroutine foo below.  At the same time, this tests
! the correct association of typeaa between a module
! procedure and a new definition of the type in MAIN.
!
module types

  type :: typea
    sequence
    integer :: i
  end type typea

  type :: typeaa
    sequence
    integer :: i
  end type typeaa

  type(typea) :: it = typea(2)

end module types
!------------------------------
module global

  use types, only: typea, it

contains

  subroutine foo (x)
    use types
    type(typeaa) :: ca
    real :: x(it%i)
    common /c/ ca
    x = 42.0
    ca%i = 99
  end subroutine foo

end module global
!------------------------------
  use global, only: typea, foo
  type :: typeaa
    sequence
    integer :: i
  end type typeaa
  type(typeaa) :: cam
  real :: x(4)
  common /c/ cam
  x = -42.0
  call foo(x)
  if (any (x .ne. (/42.0, 42.0, -42.0, -42.0/))) call abort ()
  if (cam%i .ne. 99) call abort ()
end
! { dg-final { cleanup-modules "types global" } }
