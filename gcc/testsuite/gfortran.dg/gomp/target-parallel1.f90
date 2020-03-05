!$omp target parallel
  print *, 'Hello, world'
!$omp end target parallel
end
