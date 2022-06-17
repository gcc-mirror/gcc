! { dg-do compile }

program main
  use omp_lib
  implicit none
  integer, allocatable :: arr(:)
  integer (omp_allocator_handle_kind) :: bar

  !$omp target allocate(bar : arr) ! { dg-error "allocator ''bar'' requires 'uses_allocators.bar.' clause in target region" }
  block
    allocate(arr(100))
  end block

end program main
