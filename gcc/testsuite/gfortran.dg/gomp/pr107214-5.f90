integer :: x, y

!$omp target in_reduction(+: x) private(x)  ! { dg-error "Symbol 'x' present on multiple clauses" }
x = x + 1
!$omp end target

!$omp target in_reduction(+: y) firstprivate(y)  ! { dg-error "Symbol 'y' present on both data and map clauses" }
y = y + 1
!$omp end target

end
