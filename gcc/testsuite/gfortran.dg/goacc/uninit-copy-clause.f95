! { dg-do compile }
! { dg-additional-options "-Wuninitialized" }

subroutine foo
  integer :: i

  !$acc kernels
  ! { dg-warning "'i' is used uninitialized in this function" "" { target *-*-* } .-1 }
  !TODO See discussion in '../../c-c++-common/goacc/uninit-copy-clause.c'.
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
