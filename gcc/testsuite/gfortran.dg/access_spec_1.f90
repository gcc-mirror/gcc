! { dg-do compile }
! { dg-require-visibility "" }
! PR fortran/31472
! Access specifications: Valid Fortran 2003 code
module mod
  implicit none
  private
  integer, public :: i
  integer, private :: z
  integer :: j, x
  private :: j
  public  :: x
  type, public :: bar
    PRIVATE
    integer, public :: y  ! Fortran 2003
    integer, private :: z  ! Fortran 2003
  end type
end module
