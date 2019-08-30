! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine foo
  integer :: i
  !$omp do schedule (nonmonotonic: static, 2)	! { dg-error "NONMONOTONIC modifier specified for STATIC schedule kind" }
  do i = 0, 64
  end do
  !$omp do schedule (nonmonotonic : static)	! { dg-error "NONMONOTONIC modifier specified for STATIC schedule kind" }
  do i = 0, 64
  end do
  !$omp do schedule (nonmonotonic : runtime)	! { dg-error "NONMONOTONIC modifier specified for RUNTIME schedule kind" }
  do i = 0, 64
  end do
  !$omp do schedule (nonmonotonic : auto)	! { dg-error "NONMONOTONIC modifier specified for AUTO schedule kind" }
  do i = 0, 64
  end do
  !$omp do schedule (nonmonotonic : dynamic) ordered	! { dg-error "NONMONOTONIC schedule modifier specified with ORDERED clause" }
  do i = 0, 64
    !$omp ordered
    !$omp end ordered
  end do
  !$omp do ordered schedule(nonmonotonic : dynamic, 5)	! { dg-error "NONMONOTONIC schedule modifier specified with ORDERED clause" }
  do i = 0, 64
    !$omp ordered
    !$omp end ordered
  end do
  !$omp do schedule (nonmonotonic : guided) ordered(1)	! { dg-error "NONMONOTONIC schedule modifier specified with ORDERED clause" }
  do i = 0, 64
    !$omp ordered depend(sink: i - 1)
    !$omp ordered depend(source)
  end do
  !$omp do ordered(1) schedule(nonmonotonic : guided, 2)	! { dg-error "NONMONOTONIC schedule modifier specified with ORDERED clause" }
  do i = 0, 64
    !$omp ordered depend(source)
    !$ordered depend(sink: i - 1)
  end do
  !$omp do schedule (nonmonotonic , monotonic : dynamic)	! { dg-error "Both MONOTONIC and NONMONOTONIC schedule modifiers specified" }
  do i = 0, 64
  end do
  !$omp do schedule (monotonic,nonmonotonic:dynamic)	! { dg-error "Both MONOTONIC and NONMONOTONIC schedule modifiers specified" }
  do i = 0, 64
  end do
end subroutine foo
