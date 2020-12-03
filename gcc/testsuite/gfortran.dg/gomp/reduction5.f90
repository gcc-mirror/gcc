! { dg-do compile }
!
implicit none
integer :: a, b, i
a = 0

!$omp parallel reduction(foo,+:a)  ! { dg-error "26: Failed to match clause" }
do i=1,10
  a = a + 1
end do
!$omp end parallel  ! { dg-error "Unexpected !.OMP END PARALLEL statement" }

!$omp parallel reduction(task +:a) ! { dg-error "30: Comma expected at" }
do i=1,10
  a = a + 1
end do
!$omp end parallel  ! { dg-error "Unexpected !.OMP END PARALLEL statement" }

!$omp task in_reduction(foo,+:a)  ! { dg-error "25: Failed to match clause" }
  a = a + 1
!$omp end task  ! { dg-error "Unexpected !.OMP END TASK statement" }

!$omp taskloop reduction(inscan,+:a) in_reduction(+:b) ! { dg-error "34: Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i=1,10
  a = a + 1
end do

!$omp taskloop reduction(task,+:a) in_reduction(+:b) ! { dg-error "32: Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
do i=1,10
  a = a + 1
end do

!$omp teams reduction(inscan,+:b) ! { dg-error "31: Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
  a = a + 1
!$omp end teams

!$omp teams reduction(task, +:b) ! { dg-error "30: Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
  a = a + 1
!$omp end teams

end
