! { dg-do compile }
!
! PR 42888: [4.5 Regression] ICE in fold_convert_loc, at fold-const.c:2670
!
! Contributed by Harald Anlauf <anlauf@gmx.de>

  implicit none

  type t
     integer :: X = -999.0   ! Real initializer!
  end type t

  type(t), allocatable :: x
  class(t), allocatable :: y,z

  allocate (x)
  allocate (y)
  allocate (t::z)

end
