! { dg-do compile }

subroutine bar(a)
  implicit none
  integer  :: a
  integer, allocatable :: var
!$omp target
  !$omp allocate (var) ! { dg-error "'allocate' directive must specify an allocator here" }
  allocate (var)
!$omp end target

end subroutine

