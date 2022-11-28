subroutine foo
implicit none
integer :: i, a(5)

!$omp do nowait nowait  ! { dg-error "Duplicated 'nowait' clause" }
do i = 1, 5
end do

!$omp do
do i = 1, 5
end do
!$omp do nowait nowait  ! { dg-error "Duplicated 'nowait' clause" }

!$omp do nowait
do i = 1, 5
end do
!$omp end do nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp do simd nowait
do i = 1, 5
end do
!$omp end do simd nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp scope nowait
!$omp end scope nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp sections nowait
  !$omp section
  block; end block
!$omp end sections nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp single nowait
!$omp end single nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp target nowait
!$omp end target nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp target parallel nowait
!$omp end target parallel nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp target parallel do nowait
do i = 1, 5
end do
!$omp end target parallel do nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp target parallel do simd nowait
do i = 1, 5
end do
!$omp end target parallel do simd nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp target parallel loop nowait
do i = 1, 5
end do
!$omp end target parallel loop nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp target teams distribute parallel do nowait
do i = 1, 5
end do
!$omp end target teams distribute parallel do nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp target teams distribute parallel do simd nowait
do i = 1, 5
end do
!$omp end target teams distribute parallel do simd nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp target simd nowait
do i = 1, 5
end do
!$omp end target simd nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp target teams nowait
!$omp end target teams nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp target teams distribute nowait
do i = 1, 5
end do
!$omp end target teams distribute nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp target teams distribute simd nowait
do i = 1, 5
end do
!$omp end target teams distribute simd nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp target teams loop nowait
do i = 1, 5
end do
!$omp end target teams loop nowait  ! { dg-error "Duplicated NOWAIT clause" }

!$omp workshare nowait
A(:) = 5
!$omp end workshare nowait  ! { dg-error "Duplicated NOWAIT clause" }
end
