! { dg-do run }
!
! PR fortran/34187
! The binding label was not exported for private procedures
! with public generic interfaces.
!
module mod
  use iso_c_binding, only: c_int
  implicit none
  private
  public :: gen, c_int
  interface gen
    module procedure  test
  end interface gen
contains
  subroutine test(a) bind(c, name="myFunc")
    integer(c_int), intent(out) :: a 
    a = 17
  end subroutine test
end module mod

program main
  use mod
  implicit none
  integer(c_int) :: x
  x = -44
  call gen(x)
  if(x /= 17) call abort()
end program main
