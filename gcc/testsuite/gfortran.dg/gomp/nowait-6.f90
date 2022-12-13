! invalid nowait

subroutine foo
implicit none
integer :: i, a(5)
!$omp atomic write nowait  ! { dg-error "Failed to match clause" }
i = 5
!$omp end atomic  ! { dg-error "Unexpected ..OMP END " }

!$omp critical nowait  ! { dg-error "Failed to match clause" }
!$omp end critical  ! { dg-error "Unexpected ..OMP END " }

!$omp distribute nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end distribute  ! { dg-error "Unexpected ..OMP END " }

!$omp distribute parallel do nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end distribute parallel do  ! { dg-error "Unexpected ..OMP END " }

!$omp distribute parallel do simd nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end distribute parallel do simd  ! { dg-error "Unexpected ..OMP END " }

!$omp parallel sections nowait  ! { dg-error "Failed to match clause" }
  !$omp section  ! { dg-error "Unexpected ..OMP SECTION statement" }
  block; end block
!$omp end parallel sections  ! { dg-error "Unexpected ..OMP END " }

!$omp distribute simd nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end distribute simd  ! { dg-error "Unexpected ..OMP END " }

!$omp masked nowait  ! { dg-error "Failed to match clause" }
!$omp end masked  ! { dg-error "Unexpected ..OMP END " }

!$omp masked taskloop nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end masked taskloop  ! { dg-error "Unexpected ..OMP END " }

!$omp masked taskloop simd nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end masked taskloop simd  ! { dg-error "Unexpected ..OMP END " }

!$omp master nowait  ! { dg-error "Unexpected junk" }
!$omp end master  ! { dg-error "Unexpected ..OMP END " }

!$omp master taskloop nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end master taskloop  ! { dg-error "Unexpected ..OMP END " }

!$omp master taskloop simd nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end master taskloop simd  ! { dg-error "Unexpected ..OMP END " }

!$omp ordered nowait  ! { dg-error "Failed to match clause" }
!$omp end ordered  ! { dg-error "Unexpected ..OMP END " }

!$omp parallel nowait  ! { dg-error "Failed to match clause" }
!$omp end parallel  ! { dg-error "Unexpected ..OMP END " }

!$omp parallel workshare nowait  ! { dg-error "Failed to match clause" }
a(:) = 5
!$omp end parallel workshare  ! { dg-error "Unexpected ..OMP END " }

!$omp parallel do nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end parallel do  ! { dg-error "Unexpected ..OMP END " }

!$omp parallel do simd nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end parallel do simd  ! { dg-error "Unexpected ..OMP END " }

!$omp parallel masked nowait  ! { dg-error "Failed to match clause" }
!$omp end parallel masked  ! { dg-error "Unexpected ..OMP END " }

!$omp parallel masked taskloop nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end parallel masked taskloop  ! { dg-error "Unexpected ..OMP END " }

!$omp parallel masked taskloop simd nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end parallel masked taskloop simd  ! { dg-error "Unexpected ..OMP END " }

!$omp parallel master nowait  ! { dg-error "Failed to match clause" }
!$omp end parallel master  ! { dg-error "Unexpected ..OMP END " }

!$omp parallel master taskloop nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end parallel master taskloop  ! { dg-error "Unexpected ..OMP END " }

!$omp parallel master taskloop simd nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end parallel master taskloop simd  ! { dg-error "Unexpected ..OMP END " }

!$omp simd nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end simd  ! { dg-error "Unexpected ..OMP END " }

!$omp task nowait  ! { dg-error "Failed to match clause" }
!$omp end task  ! { dg-error "Unexpected ..OMP END " }

!$omp taskgroup nowait  ! { dg-error "Failed to match clause" }
!$omp end taskgroup  ! { dg-error "Unexpected ..OMP END " }

!$omp taskloop nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end taskloop  ! { dg-error "Unexpected ..OMP END " }

!$omp taskloop simd nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end taskloop simd  ! { dg-error "Unexpected ..OMP END " }

!$omp teams nowait  ! { dg-error "Failed to match clause" }
!$omp end teams  ! { dg-error "Unexpected ..OMP END " }

!$omp teams distribute nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end teams distribute  ! { dg-error "Unexpected ..OMP END " }

!$omp teams distribute parallel do nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end teams distribute parallel do  ! { dg-error "Unexpected ..OMP END " }

!$omp teams distribute parallel do simd nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end teams distribute parallel do simd  ! { dg-error "Unexpected ..OMP END " }

!$omp teams distribute simd nowait  ! { dg-error "Failed to match clause" }
do i = 1, 5
end do
!$omp end teams distribute simd  ! { dg-error "Unexpected ..OMP END " }

!$omp target data map(tofrom:i) nowait  ! { dg-error "Failed to match clause" }
!$omp end target data  ! { dg-error "Unexpected ..OMP END " }

end
! { dg-prune-output "Unexpected end of file" }
