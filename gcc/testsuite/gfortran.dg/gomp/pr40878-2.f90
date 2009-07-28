! PR fortran/40878
! { dg-do compile }
! { dg-options "-fopenmp" } 

subroutine test1
  integer :: j, k
  integer, parameter :: m = 2
!$omp parallel do collapse(m) schedule (static,1)
  do k = 1, 2
    do j = 1, 6
    enddo
  enddo
!$omp end parallel do
end
subroutine test2
  integer :: j, k
!$omp parallel do collapse(2) schedule (static,1)
  do k = 1, 2
    do j = 1, 6
    enddo
  enddo
!$omp end parallel do
end
