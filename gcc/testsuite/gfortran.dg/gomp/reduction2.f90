! { dg-do compile }

subroutine f1
  integer :: i
  i = 0
!$omp parallel reduction (ior:i)
  i = ior (i, 3)
!$omp end parallel
!$omp parallel reduction (ior:i)
  i = ior (i, 16)
!$omp end parallel
end subroutine f1
subroutine f2
  integer :: i
  i = ior (2, 4)
!$omp parallel reduction (ior:i)
  i = ior (i, 3)
!$omp end parallel
end subroutine f2
subroutine f3
  integer :: i
  i = 6
!$omp parallel reduction (ior:i)
  i = ior (i, 3)
!$omp end parallel
end subroutine f3
subroutine f4
  integer :: i, ior
  i = 6
!$omp parallel reduction (ior:i)
  i = ior (i, 3)
!$omp end parallel
end subroutine f4
