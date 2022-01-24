! PR101333
! { dg-do compile}
!
! TS 29113
! C407c An assumed-type actual argument that corresponds to an
! assumed-rank dummy argument shall be assumed-shape or assumed-rank.
!
! This constraint is renumbered C711 in the 2018 Fortran standard.

module m
  interface
    subroutine g (a, b)
      implicit none
      type(*) :: a(..)
      integer :: b
    end subroutine
  end interface
end module

! Check that assumed-shape works.

subroutine s0 (x)
  use m
  implicit none
  type(*) :: x(:)

  call g (x, 1)
end subroutine

! Check that assumed-rank works.

subroutine s1 (x)
  use m
  implicit none
  type(*) :: x(..)

  call g (x, 1)
end subroutine

! Check that assumed-size gives an error.

subroutine s2 (x)
  use m
  implicit none
  type(*) :: x(*)

  call g (x, 1)  ! { dg-error "Assumed-type actual argument at .1. corresponding to assumed-rank dummy argument 'a' must be assumed-shape or assumed-rank" }
end subroutine

! Check that a scalar gives an error.
subroutine s3 (x)
  use m
  implicit none
  type(*) :: x

  call g (x, 1)  ! { dg-error "Assumed.type" }
end subroutine

! Explicit-shape assumed-type actual arguments are forbidden implicitly
! by c407a (C709 in the 2018 standard).  They're not allowed as dummy
! arguments, and assumed-type entities can only be declared as dummy
! arguments, so there is no other way to construct one to pass as an
! actual argument.
