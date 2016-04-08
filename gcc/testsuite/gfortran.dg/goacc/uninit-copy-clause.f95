! { dg-do compile }
! { dg-additional-options "-Wuninitialized" }

subroutine foo
  integer :: i

  !$acc kernels
  i = 1
  !$acc end kernels

end subroutine foo

subroutine foo2
  integer :: i

  !$acc kernels copy (i)
  i = 1
  !$acc end kernels

end subroutine foo2

subroutine foo3
  integer :: i

  !$acc kernels copyin (i)
  i = 1
  !$acc end kernels

end subroutine foo3
