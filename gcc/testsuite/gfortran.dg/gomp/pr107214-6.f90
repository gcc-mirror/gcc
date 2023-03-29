! { dg-do compile }

integer :: x

!$omp target map(x) private(x)  ! { dg-error "Symbol 'x' present on multiple clauses" }
x = x + 1
!$omp end target

end
