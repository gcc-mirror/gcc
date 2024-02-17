! PR fortran/113503
! { dg-do compile }
! { dg-options "-O2 -fno-inline -Wuninitialized" }

program pr113503
  implicit none
  type :: T
    character(len=:), allocatable :: u
  end type
  character(len=20) :: us(1) = 'foobar'
  type(T) :: x
  x = T(u = trim (us(1))) ! { dg-bogus "is used uninitialized" }
  call foo
contains
  subroutine foo
    if (x%u /= 'foobar') stop 1
  end subroutine
end
