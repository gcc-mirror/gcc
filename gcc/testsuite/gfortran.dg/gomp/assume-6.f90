!$omp assume no_openmp no_openmp_something  ! { dg-error "24: Failed to match clause" }
block
end block

!$omp assume no_openmp no_openmp_routines no_openmp_constructs no_openmp_constructs ! { dg-error "63: Duplicated 'no_openmp_constructs' clause" }
block
end block
end
