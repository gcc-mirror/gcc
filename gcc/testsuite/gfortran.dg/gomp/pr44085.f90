! PR middle-end/44085
! { dg-do compile }
! { dg-require-effective-target tls_native }
! { dg-options "-fopenmp" }

  integer, save :: thr1, thr2
  integer :: thr3, thr4
  common /thrs/ thr3, thr4
!$omp threadprivate (thr1, thr2, /thrs/)

!$omp task untied		! { dg-message "note: enclosing task" }
  thr1 = thr1 + 1		! { dg-error "used in untied task" }
  thr2 = thr2 + 2		! { dg-error "used in untied task" }
  thr3 = thr3 + 3		! { dg-error "used in untied task" }
  thr4 = thr4 + 4		! { dg-error "used in untied task" }
!$omp end task

!$omp task
  thr1 = thr1 + 1
  thr2 = thr2 + 2
  thr3 = thr3 + 3
  thr4 = thr4 + 4
!$omp end task

  end
