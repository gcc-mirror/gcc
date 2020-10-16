module m
 integer t;
 !$omp threadprivate(t)
end

subroutine f1
  use m
  implicit none
  integer :: i
  !$omp simd order(concurrent)  ! { dg-error "enclosing region" }
  do i = 1, 64
    t = t + 1  ! { dg-error "threadprivate variable 't' used in a region with 'order\\(concurrent\\)' clause" } */
  end do
end

subroutine f2
  use m
  implicit none
  integer :: i
  !$omp do simd order(concurrent) ! { dg-error "enclosing region" }
  do i = 1, 64
    t = t + 1  ! { dg-error "threadprivate variable 't' used in a region with 'order\\(concurrent\\)' clause" } */
    ! { dg-error "enclosing region" "" { target *-*-* } .-1 }
  end do
end

subroutine f3
  use m
  implicit none
  integer :: i
  !$omp do order(concurrent)  ! { dg-error "enclosing region" }
  do i = 1, 64
    t = t + 1  ! { dg-error "threadprivate variable 't' used in a region with 'order\\(concurrent\\)' clause" } */
  end do
end
