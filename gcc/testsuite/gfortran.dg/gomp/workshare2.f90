! { dg-do compile }
! { dg-options "-fopenmp -ffrontend-optimize -fdump-tree-original" }
! PR 50690 - this used to ICE because workshare could not handle
! BLOCKs.
! To test for correct execution, run this program (but don't forget
! to unset the stack limit).
program foo
  implicit none
  integer, parameter :: n = 10000000
  real, parameter :: eps = 3e-7
  integer :: i,j
  real :: A(n), B(5), C(n)
  real :: tmp
  B(1) = 3.344
  tmp = B(1)
  do i=1,10
     call random_number(a)
     c = a
     !$omp parallel default(shared)
     !$omp workshare
     A(:) = A(:)*cos(B(1))+A(:)*cos(B(1))
     !$omp end workshare nowait
     !$omp end parallel ! sync is implied here
  end do

  c = c*tmp + c*tmp

  do j=1,n
     if (abs(a(j)-c(j)) > eps) then
        print *,1,j,a(j), c(j)
        STOP 1
     end if
  end do

  do i=1,10
     call random_number(a)
     c = a
     !$omp parallel workshare default(shared)
     A(:) = A(:)*cos(B(1))+A(:)*cos(B(1))
     !$omp end parallel workshare
  end do

  c = c*tmp + c*tmp
  do j=1,n
     if (abs(a(j)-c(j)) > eps) then
        print *,2,j,a(j), c(j)
        STOP 2
     end if
  end do

end program foo
! { dg-final { scan-tree-dump-times "__var" 0 "original" } }
