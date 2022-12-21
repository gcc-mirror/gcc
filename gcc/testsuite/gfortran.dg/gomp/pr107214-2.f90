integer :: y

!$omp target has_device_addr(y) firstprivate(y)  ! { dg-error "Symbol 'y' present on multiple clauses" }
!$omp end target

end
