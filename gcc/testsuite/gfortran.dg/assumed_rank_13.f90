! { dg-do compile }
!
! PR fortran/57458
!
!

  integer, pointer, asynchronous :: i(:)
  integer, pointer, volatile :: j(:)
  call foo(i)
  call foo2(i)
  call foo3(j)
  call foo4(j)
contains
  subroutine foo(x)
    type(*), dimension(:), asynchronous :: x
  end subroutine foo
  subroutine foo2(x)
    type(*), dimension(..), asynchronous :: x
  end subroutine foo2
  subroutine foo3(x)
    type(*), dimension(:), asynchronous :: x
  end subroutine foo3
  subroutine foo4(x)
    type(*), dimension(..), asynchronous :: x
  end subroutine foo4
end
