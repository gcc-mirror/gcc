! PR fortran/113503
! { dg-do compile }

program pr113503
  implicit none
  type :: T
    character(len=:), allocatable :: u
  end type
  character(len=20) :: us(1) = 'foo'
  type(T) :: x
  x = T(u = us(1))
end
