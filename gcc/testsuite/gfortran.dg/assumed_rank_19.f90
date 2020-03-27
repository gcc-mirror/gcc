! { dg-do run }
!
! PR fortran/93957
!
! Contributed by José Rui Faustino de Sousa

function f_ice(this) result(that) bind(c)
  use, intrinsic :: iso_c_binding, only: c_int

  implicit none
  
  integer(kind=c_int), intent(in) :: this(..)
  integer(kind=c_int)             :: that

  that = size(this)
  return
end function f_ice

program ice_p
  use, intrinsic :: iso_c_binding, only: c_int
  implicit none

  interface
    function f_ice(this) result(that) bind(c)
      use, intrinsic :: iso_c_binding, only: c_int
      integer(kind=c_int), intent(in) :: this(..)
      integer(kind=c_int)             :: that
    end function f_ice
  end interface

  integer(kind=c_int), parameter :: n = 10
    
  integer(kind=c_int) :: intp(n)

  if(size(intp)/=n)  stop 1
  if(f_ice(intp)/=n) stop 2
end program ice_p
