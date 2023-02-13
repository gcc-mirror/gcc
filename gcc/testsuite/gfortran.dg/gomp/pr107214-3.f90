program p
integer :: y

!$omp target map(y) firstprivate(y)  ! { dg-error "Symbol 'y' present on both data and map clauses" }
y = y + 1
!$omp end target

!$omp target simd map(y) firstprivate(y)  ! { dg-error "Symbol 'y' present on both data and map clauses" }
do i=1,1
  y = y + 1
end do
!$omp end target simd

end program p
