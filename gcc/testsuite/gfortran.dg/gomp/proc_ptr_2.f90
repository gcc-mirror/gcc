! { dg-do compile }
  procedure(foo), pointer :: ptr
  integer :: i
  ptr => foo
!$omp do reduction (+ : ptr)	! { dg-error "Procedure pointer|not found" }
  do i = 1, 10
  end do
!$omp simd linear (ptr)		! { dg-error "must be INTEGER" }
  do i = 1, 10
  end do
contains
  subroutine foo
  end subroutine
end
