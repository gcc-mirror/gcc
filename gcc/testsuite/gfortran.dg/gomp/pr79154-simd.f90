! { dg-options "-fno-openmp -fopenmp-simd" }
!
pure subroutine bar(a)
  integer, intent(in) :: a(:)
  !$omp target enter data map(to:a)   ! Ignored with -fopenmp-simd otherwise invalid in PURE
end

pure subroutine foo(a,b)
  integer, intent(out) :: a(5)
  integer, intent(in) :: b(5)
  !$omp target teams distribute simd ! { dg-error "may not appear in a PURE procedure" }
  do i=1, 5
    a(i) = b(i)
  end do
  !$omp end target teams distribute
end subroutine
