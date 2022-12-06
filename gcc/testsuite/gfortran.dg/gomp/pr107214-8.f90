! { dg-do compile }

integer, allocatable :: x
integer, pointer :: y

!$omp target map(x) has_device_addr(x)  ! { dg-error "Symbol 'x' present on multiple clauses" }
!$omp end target

!$omp target map(y) is_device_ptr(y)  ! { dg-error "Symbol 'y' present on multiple clauses" }
!$omp end target

!$omp target firstprivate(x) has_device_addr(x)  ! { dg-error "Symbol 'x' present on multiple clauses" }
!$omp end target

!$omp target firstprivate(y) is_device_ptr(y)  ! { dg-error "Symbol 'y' present on multiple clauses" }
!$omp end target

end
