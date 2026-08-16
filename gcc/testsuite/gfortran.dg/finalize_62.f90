! { dg-do run }
!
! PR fortran/110626 - in a derived-type intrinsic assignment, a finalizable
! component that has a defined assignment is finalized twice: once by the
! whole-derived-type finalization of the lhs and once by the INTENT(OUT)
! argument of the defined assignment.  The second finalization must see the
! value left by the first one, not a stale copy, matching other compilers.
!
module pr110626
  implicit none

  type :: cell
     integer :: tag = 0
   contains
     final     :: wipe
     procedure :: copyinto
     generic   :: assignment(=) => copyinto
  end type

  type :: box
     type(cell) :: c
  end type

  integer :: nf = 0
  integer :: ncopy = 0
  integer :: seen_final(4) = 0
  integer :: seen_copy = -99

contains

  subroutine wipe (self)
    type(cell), intent(inout) :: self
    nf = nf + 1
    if (nf <= size (seen_final)) seen_final(nf) = self%tag
    self%tag = -1
  end subroutine

  subroutine copyinto (dst, src)
    class(cell), intent(out) :: dst
    type(cell),  intent(in)  :: src
    ncopy = ncopy + 1
    seen_copy = dst%tag
    dst%tag = src%tag + 1
  end subroutine

end module

program p
  use pr110626
  implicit none
  type(box) :: src, dst

  src%c%tag = 7
  dst%c%tag = 42

  nf = 0; ncopy = 0
  dst = src

  if (nf /= 2)            stop 1   ! two finalizations of the old component
  if (seen_final(1) /= 42) stop 2  ! first sees the old value
  if (seen_final(2) /= -1) stop 3  ! second sees the post-finalization value
  if (ncopy /= 1)        stop 4    ! defined assignment runs once
  if (seen_copy /= 0)    stop 5    ! INTENT(OUT) default-initialised before body
  if (dst%c%tag /= 8)    stop 6    ! result is src + 1
end program
