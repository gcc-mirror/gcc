! PR fortran/104826
program pr104826
  character(:), allocatable :: x
  save
  !$omp target
  x = 'abc'
  !$omp end target
end
