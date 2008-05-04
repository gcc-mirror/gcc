! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Incorrect extent in return value of MAXLOC intrnisic: is 3, should be 2" }
module tst
contains
  subroutine foo(res)
    integer(kind=4), allocatable :: f(:,:)
    integer, dimension(:) :: res
    allocate (f(2,5))
    f = 3
    res = maxloc(f,mask=f>2)
  end subroutine foo

end module tst
program main
  use tst
  implicit none
  integer :: res(3)
  call foo(res)
end program main
! { dg-output "Fortran runtime error: Incorrect extent in return value of MAXLOC intrnisic: is 3, should be 2" }
! { dg-final { cleanup-modules "tst" } }
