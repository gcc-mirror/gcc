! { dg-do compile }
! { dg-options "-ffrontend-optimize -fdump-tree-original -fopenmp" }
! Test that common function elimination is done within the OMP parallel
! blocks even if there is a workshare around it.
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
     !$omp parallel workshare
     !$omp parallel default(shared)
     !$omp do
     do j=1,n
       A(j) = A(j)*cos(B(1))+A(j)*cos(B(1))
     end do
     !$omp end do
     !$omp end parallel
     !$omp end parallel workshare
  end do

  c = c*cos(b(1))+ c*cos(b(1))

  do j=1,n
     if (abs(a(j)-c(j)) > eps) then
        print *,1,j,a(j), c(j)
        call abort
     end if
  end do

end program foo
! { dg-final { scan-tree-dump-times "__builtin_cosf" 2 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
