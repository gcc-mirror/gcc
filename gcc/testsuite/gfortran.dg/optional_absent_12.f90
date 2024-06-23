! { dg-do run }
! { dg-additional-options "-fcheck=array-temps" }
!
! PR fortran/55978 - comment#19
!
! Test passing of (missing) optional dummy to optional array argument

program test
  implicit none
  integer, pointer :: p(:) => null()
  call one (p)
  call one (null())
  call one ()
  call three ()
contains
  subroutine one (y)
    integer, pointer, optional, intent(in) :: y(:)
    call two (y)
  end subroutine one

  subroutine three (z)
    integer, allocatable, optional, intent(in) :: z(:)
    call two (z)
  end subroutine three

  subroutine two (x)
    integer, optional, intent(in) :: x(*)
    if (present (x)) stop 1
  end subroutine two
end
