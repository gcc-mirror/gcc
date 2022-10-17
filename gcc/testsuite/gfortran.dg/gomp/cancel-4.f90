subroutine f4
  !$omp cancellation point ! { dg-error "Expected construct-type PARALLEL, SECTIONS, DO or TASKGROUP in .OMP CANCELLATION POINT statement at" }
  if (.false.) then
!$omp cancellation EKAHI ! { dg-error "Unclassifiable OpenMP directive" }
  end if
!$omp cancellation HO OKAHI ! { dg-error "Unclassifiable OpenMP directive" }

!$omp cancellation point ! { dg-error "Expected construct-type PARALLEL, SECTIONS, DO or TASKGROUP in .OMP CANCELLATION POINT statement at" }
end
