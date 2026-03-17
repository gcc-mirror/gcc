! { dg-do compile }
! { dg-additional-options "-fopenmp" }

program p
  type t
    integer :: a(2)
  end type
  type(t) :: x(8)

  !$omp task depend (iterator(j=1:8), out:x(j)%a)
  !$omp end task

  !$omp task depend (iterator(j=1:8), out:x(j)%a(1))
  !$omp end task
end
