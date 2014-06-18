! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine foo (r)
  integer :: i, r
  !$omp target
  !$omp target teams distribute parallel do reduction (+: r) ! { dg-warning "target construct inside of target region" }
    do i = 1, 10
      r = r + 1
    end do
  !$omp end target
end subroutine
