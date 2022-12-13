! invalid nowait

subroutine foo
implicit none
integer :: i, a(5)
!$omp atomic write
i = 5
!$omp end atomic nowait  ! { dg-error "Unexpected junk" }

!$omp critical
!$omp end critical nowait  ! { dg-error "Unexpected junk" }

!$omp distribute
do i = 1, 5
end do
!$omp end distribute nowait  ! { dg-error "Unexpected junk" }

!$omp distribute parallel do
do i = 1, 5
end do
!$omp end distribute parallel do nowait  ! { dg-error "Unexpected junk" }

!$omp distribute parallel do simd
do i = 1, 5
end do
!$omp end distribute parallel do simd nowait  ! { dg-error "Unexpected junk" }

!$omp parallel sections
  !$omp section
  block; end block
!$omp end parallel sections nowait  ! { dg-error "Unexpected junk" }

!$omp distribute simd
do i = 1, 5
end do
!$omp end distribute simd nowait  ! { dg-error "Unexpected junk" }

!$omp masked
!$omp end masked nowait  ! { dg-error "Unexpected junk" }

!$omp masked taskloop
do i = 1, 5
end do
!$omp end masked taskloop nowait  ! { dg-error "Unexpected junk" }

!$omp masked taskloop simd
do i = 1, 5
end do
!$omp end masked taskloop simd nowait  ! { dg-error "Unexpected junk" }

!$omp master
!$omp end master nowait  ! { dg-error "Unexpected junk" }

!$omp master taskloop
do i = 1, 5
end do
!$omp end master taskloop nowait  ! { dg-error "Unexpected junk" }

!$omp master taskloop simd
do i = 1, 5
end do
!$omp end master taskloop simd nowait  ! { dg-error "Unexpected junk" }

!$omp ordered
!$omp end ordered nowait  ! { dg-error "Unexpected junk" }

!$omp parallel
!$omp end parallel nowait  ! { dg-error "Unexpected junk" }

!$omp parallel workshare
a(:) = 5
!$omp end parallel workshare nowait  ! { dg-error "Unexpected junk" }

!$omp parallel do
do i = 1, 5
end do
!$omp end parallel do nowait  ! { dg-error "Unexpected junk" }

!$omp parallel do simd
do i = 1, 5
end do
!$omp end parallel do simd nowait  ! { dg-error "Unexpected junk" }

!$omp parallel masked
!$omp end parallel masked nowait  ! { dg-error "Unexpected junk" }

!$omp parallel masked taskloop
do i = 1, 5
end do
!$omp end parallel masked taskloop nowait  ! { dg-error "Unexpected junk" }

!$omp parallel masked taskloop simd
do i = 1, 5
end do
!$omp end parallel masked taskloop simd nowait  ! { dg-error "Unexpected junk" }

!$omp parallel master
!$omp end parallel master nowait  ! { dg-error "Unexpected junk" }

!$omp parallel master taskloop
do i = 1, 5
end do
!$omp end parallel master taskloop nowait  ! { dg-error "Unexpected junk" }

!$omp parallel master taskloop simd
do i = 1, 5
end do
!$omp end parallel master taskloop simd nowait  ! { dg-error "Unexpected junk" }

!$omp simd
do i = 1, 5
end do
!$omp end simd nowait  ! { dg-error "Unexpected junk" }

!$omp task
!$omp end task nowait  ! { dg-error "Unexpected junk" }

!$omp taskgroup
!$omp end taskgroup nowait  ! { dg-error "Unexpected junk" }

!$omp taskloop
do i = 1, 5
end do
!$omp end taskloop nowait  ! { dg-error "Unexpected junk" }

!$omp taskloop simd
do i = 1, 5
end do
!$omp end taskloop simd nowait  ! { dg-error "Unexpected junk" }

!$omp teams
!$omp end teams nowait  ! { dg-error "Unexpected junk" }

!$omp teams distribute
do i = 1, 5
end do
!$omp end teams distribute nowait  ! { dg-error "Unexpected junk" }

!$omp teams distribute parallel do
do i = 1, 5
end do
!$omp end teams distribute parallel do nowait  ! { dg-error "Unexpected junk" }

!$omp teams distribute parallel do simd
do i = 1, 5
end do
!$omp end teams distribute parallel do simd nowait  ! { dg-error "Unexpected junk" }

!$omp teams distribute simd
do i = 1, 5
end do
!$omp end teams distribute simd nowait  ! { dg-error "Unexpected junk" }

!$omp target data map(tofrom:i)
!$omp end target data nowait  ! { dg-error "Unexpected junk" }

end  ! { dg-error "Unexpected END statement" }
! { dg-prune-output "Unexpected end of file" }
