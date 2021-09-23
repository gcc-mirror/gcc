subroutine foo()
implicit none
integer :: i, r
!$omp taskloop reduction(task, +: r)  ! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i = 1, 64
end do
!$omp taskloop simd reduction(task, +: r)  ! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i = 1, 64
end do
!$omp master taskloop reduction(task, +: r)  ! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i = 1, 64
end do
!$omp master taskloop simd reduction(task, +: r)  ! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i = 1, 64
end do
!$omp parallel master taskloop reduction(task, +: r)  ! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i = 1, 64
end do
!$omp parallel master taskloop simd reduction(task, +: r)  ! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i = 1, 64
end do

!$omp taskloop reduction(inscan, +: r)  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
do i = 1, 64                            ! { dg-error "OMP SCAN between two structured-block-sequences" "" { target *-*-* } .-1 }
end do
!$omp taskloop simd reduction(inscan, +: r)  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
do i = 1, 64                                 ! { dg-error "OMP SCAN between two structured-block-sequences" "" { target *-*-* } .-1 }
end do
!$omp master taskloop reduction(inscan, +: r) ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
do i = 1, 64
end do
!$omp master taskloop simd reduction(inscan, +: r)  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
do i = 1, 64
end do
!$omp parallel master taskloop reduction(inscan, +: r)  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
do i = 1, 64                                 ! { dg-error "OMP SCAN between two structured-block-sequences" "" { target *-*-* } .-1 }
end do
!$omp parallel master taskloop simd reduction(inscan, +: r)   ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
do i = 1, 64                                 ! { dg-error "OMP SCAN between two structured-block-sequences" "" { target *-*-* } .-1 }
end do
end


subroutine bar()
implicit none
integer :: i, r
r = 0
!$omp parallel reduction(+:r)
  !$omp master taskloop in_reduction(+:r)
    do i = 1, 64
    end do
  !$omp master taskloop simd in_reduction(+:r)
    do i = 1, 64
    end do
  !$omp master
    !$omp taskloop in_reduction(+:r)
      do i = 1, 64
      end do
    !$omp taskloop simd in_reduction(+:r)
      do i = 1, 64
      end do
  !$omp end master
!$omp end parallel

!$omp parallel master taskloop in_reduction(+:r)  ! { dg-error "Failed to match clause" }
    do i = 1, 64
    end do

!$omp parallel master taskloop simd in_reduction(+:r)  ! { dg-error "Failed to match clause" }
    do i = 1, 64
    end do
end
