! { dg-do compile }
! A public subroutine can have private-type, dummy arguments
! in Fortran 2003 (but not in Fortran 95).
! See private_type_1.f90 for the F95 test.
!
module modboom
  implicit none
  private
  public:: dummysub
  type:: intwrapper
    integer n
  end type intwrapper
contains
  subroutine dummysub(size, arg_array)
   type(intwrapper) :: size
   real, dimension(size%n) :: arg_array
   real :: local_array(4)
  end subroutine dummysub
end module modboom

! { dg-final { cleanup-modules "modboom" } }
