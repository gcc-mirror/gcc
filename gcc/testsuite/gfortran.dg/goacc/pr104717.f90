! Extracted from 'libgomp.oacc-fortran/privatized-ref-2.f90'.

! { dg-additional-options "-O1 -fstack-arrays -fipa-pta" }

program main
  implicit none (type, external)
  integer :: j
  integer, allocatable :: A(:)

  A = [(3*j, j=1, 10)]
  call foo (A, size(A))
  deallocate (A)
contains
  subroutine foo (array, nn)
    integer :: i, nn
    integer :: array(nn)

    !$acc parallel copyout(array)
    array = [(-i, i = 1, nn)]
    !$acc end parallel
  end subroutine foo
end
