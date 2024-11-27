! { dg-do compile }
! { dg-options "-fopenmp" }

program main
  implicit none

  integer, parameter :: DIM1 = 17
  integer, parameter :: DIM2 = 39
  type :: array_ptr
    integer, pointer :: ptr(:)
  end type
  
  type (array_ptr) :: x(DIM1), y(DIM1)

  !$omp target map (iterator(i=1:DIM1), to: x(i)%ptr(:))
  !$omp end target

  !$omp target map (iterator(i=1:DIM1), to: x(i)%ptr(:), y(i)%ptr(:))
  !$omp end target

  !$omp target map (iterator(i=1:DIM1), to: x(i)%ptr(:) + 3) ! { dg-error "Syntax error in OpenMP variable list at .1." }
  !$omp end target ! { dg-error "Unexpected \\\!\\\$OMP END TARGET statement at .1." }

  !$omp target map(iterator(i=1:DIM1), iterator(j=1:DIM2), to: x(i)%ptr(j)) ! { dg-error "too many 'iterator' modifiers at .1." }
  !$omp end target ! { dg-error "Unexpected \\\!\\\$OMP END TARGET statement at .1." }
end program
