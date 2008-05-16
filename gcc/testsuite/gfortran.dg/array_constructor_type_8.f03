! { dg-do run }
!
! PR fortran/27997
!
! Array constructor with typespec, check for regression
!
program test
  implicit none
  type :: real_info
    integer :: kind
  end type real_info
  type (real_info) :: real_infos(1) = (/ real_info (4) /)
end program test
