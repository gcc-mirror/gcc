! PR fortran/80918
! { dg-do compile }

subroutine foo (a)
  integer :: a(*)
  !$omp task depend(inout:a)
  !$omp end task
  !$omp task depend(inout:a)
  !$omp end task
end subroutine foo
