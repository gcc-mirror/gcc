! { dg-do compile }


subroutine bar(a)
  implicit none
  integer  :: a
  integer, allocatable :: var
!$omp requires dynamic_allocators
!$omp target
  !$omp allocate (var)
  allocate (var)
!$omp end target

end subroutine

