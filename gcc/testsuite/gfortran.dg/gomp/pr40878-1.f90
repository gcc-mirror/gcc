! PR fortran/40878
! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine test1
  integer :: j, k
  integer :: m = 2
!$omp parallel do collapse(m) schedule (static,1) ! { dg-error "Constant expression required" }
  do k = 1, 2
    do j = 1, 6
    enddo
  enddo
!$omp end parallel do
end
subroutine test2
  integer :: j, k
  integer :: m
  m = 2
!$omp parallel do collapse(m) schedule (static,1) ! { dg-error "Constant expression required" }
  do k = 1, 2
    do j = 1, 6
    enddo
  enddo
!$omp end parallel do
end
subroutine test3
  integer :: j, k
  integer, parameter :: m = 0
!$omp parallel do collapse(m) schedule (static,1) ! { dg-error "not constant positive integer" }
  do k = 1, 2
    do j = 1, 6
    enddo
  enddo
!$omp end parallel do
end
subroutine test4
  integer :: j, k
  integer, parameter :: m = -2
!$omp parallel do collapse(m) schedule (static,1) ! { dg-error "not constant positive integer" }
  do k = 1, 2
    do j = 1, 6
    enddo
  enddo
!$omp end parallel do
end
subroutine test5
  integer :: j, k
!$omp parallel do collapse(0) schedule (static,1) ! { dg-error "not constant positive integer" }
  do k = 1, 2
    do j = 1, 6
    enddo
  enddo
!$omp end parallel do
end
subroutine test6
  integer :: j, k
!$omp parallel do collapse(-1) schedule (static,1) ! { dg-error "not constant positive integer" }
  do k = 1, 2
    do j = 1, 6
    enddo
  enddo
!$omp end parallel do
end
