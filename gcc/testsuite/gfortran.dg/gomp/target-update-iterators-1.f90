! { dg-do compile }
! { dg-options "-fopenmp" }

program test
  implicit none

  integer, parameter :: DIM1 = 17
  integer, parameter :: DIM2 = 39

  type :: array_ptr
    integer, pointer :: ptr(:)
  end type

  type (array_ptr) :: x(DIM1), y(DIM1)

  !$omp target update to (iterator(i=1:DIM1): x(i)%ptr(:))

  !$omp target update to (iterator(i=1:DIM1): x(i)%ptr(:DIM2), y(i)%ptr(:))

  !$omp target update to (iterator(i=1:DIM1), present: x(i)%ptr(:))

  !$omp target update to (iterator(i=1:DIM1), iterator(j=i:DIM2): x(i)%ptr(j)) ! { dg-error "too many 'iterator' modifiers at .1." }

  !$omp target update to (iterator(i=1:DIM1), something: x(i, j)) ! { dg-error "Syntax error in OpenMP variable list at .1." }
end program
