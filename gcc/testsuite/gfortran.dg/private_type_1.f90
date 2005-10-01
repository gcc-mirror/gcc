! { dg-do compile }
! PR21986 - test based on original example.
! A public subroutine must not have private-type, dummy arguments.
! Contributed by Paul Thomas <pault@gcc.gnu.org>
module modboom
  implicit none
  private
  public:: dummysub ! { dg-error "PRIVATE type and cannot be a dummy argument" }
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

