! PR fortran/100965
! { dg-do compile }

implicit none
  character(len=:), allocatable :: s
  logical :: l
  !$omp target map(from: l)
    l = allocated (s)
  !$omp end target
  if (l) stop 1

  !$omp target map(from: l)
    l = allocated (s)
  !$omp end target
  if (l) stop 2
end
