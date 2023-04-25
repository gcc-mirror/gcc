subroutine foo()
implicit none
integer :: i, r
!$omp loop reduction(task, +: r)  ! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i = 1, 64
end do
!$omp teams loop reduction(task, +: r)  ! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i = 1, 64
end do
!$omp parallel loop reduction(task, +: r)  ! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i = 1, 64
end do
!$omp target teams loop reduction(task, +: r)  ! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i = 1, 64
end do
!$omp target parallel loop reduction(task, +: r)  ! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i = 1, 64
end do

!$omp loop reduction(inscan, +: r)  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
  ! { dg-error "With INSCAN at .1., expected loop body with ..OMP SCAN between two structured block sequences" "" { target *-*-* } .-1 }
do i = 1, 64
end do
!$omp teams loop reduction(inscan, +: r)  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
  ! { dg-error "With INSCAN at .1., expected loop body with ..OMP SCAN between two structured block sequences" "" { target *-*-* } .-1 }
do i = 1, 64
end do
!$omp parallel loop reduction(inscan, +: r) ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
  ! { dg-error "With INSCAN at .1., expected loop body with ..OMP SCAN between two structured block sequences" "" { target *-*-* } .-1 }
do i = 1, 64
end do
!$omp target teams loop reduction(inscan, +: r)  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
  ! { dg-error "With INSCAN at .1., expected loop body with ..OMP SCAN between two structured block sequences" "" { target *-*-* } .-1 }
do i = 1, 64
end do
!$omp target parallel loop reduction(inscan, +: r)  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
  ! { dg-error "With INSCAN at .1., expected loop body with ..OMP SCAN between two structured block sequences" "" { target *-*-* } .-1 }
do i = 1, 64
end do

!$omp loop bind(target)  ! { dg-error "17: Expected TEAMS, PARALLEL or THREAD as binding in BIND" }
do i = 1, 64
end do

!$omp loop bind(teams) bind(teams)  ! { dg-error "Duplicated 'bind' clause" }
do i = 1, 64
end do

end
