! PR fortran/49698
! { dg-do compile }
subroutine foo (x, y, z)
  type S
    integer, pointer :: e => null()
  end type S
  type T
    type(S), dimension(:), allocatable :: a
  end type T
  type(T) :: x, y
  integer :: z, i
  forall (i = 1 : z)
    y%a(i)%e => x%a(i)%e
  end forall
end subroutine foo
