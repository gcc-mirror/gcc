! { dg-do compile }
! { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" }

program main
  use omp_lib
  implicit none
  integer, allocatable :: arr(:)
  integer (omp_allocator_handle_kind) :: bar, foo

  type (omp_alloctrait), parameter :: traits_array(*) = &
       [omp_alloctrait(omp_atk_pinned,omp_atv_true),&
       omp_alloctrait(omp_atk_partition,omp_atv_nearest)]

  !$omp target allocate(bar : arr) uses_allocators(bar)
  block
    allocate(arr(100))
  end block

  !$omp target uses_allocators(omp_default_mem_alloc)
  block
  end block

  !$omp target uses_allocators(bar(traits_array), foo (traits_array))
  block
  end block

  !$omp target uses_allocators(traits(traits_array) : bar)
  block
  end block

  !$omp target parallel uses_allocators(memspace (omp_low_lat_mem_space) : bar)
  block
  end block

  !$omp target parallel uses_allocators(memspace (omp_high_bw_mem_space), traits(traits_array) : bar)
  block
    use iso_c_binding
    type(c_ptr) :: ptr
    integer(c_size_t) :: sz = 32
    ptr = omp_alloc (sz, bar)
    call omp_free (ptr, bar)
  end block

end program main

! { dg-final { scan-tree-dump "pragma omp target allocate\\(allocator\\(bar\\):arr\\) private\\(bar\\) uses_allocators\\(bar: memspace\\(\\), traits\\(\\)\\)" "original" } }
! { dg-final { scan-tree-dump "pragma omp target" "original" } }
! { dg-final { scan-tree-dump "pragma omp target private\\(foo\\) uses_allocators\\(foo: memspace\\(\\), traits\\(traits_array\\)\\) private\\(bar\\) uses_allocators\\(bar: memspace\\(\\), traits\\(traits_array\\)\\)" "original" } }
! { dg-final { scan-tree-dump "pragma omp target private\\(bar\\) uses_allocators\\(bar: memspace\\(\\), traits\\(traits_array\\)\\)" "original" } }
! { dg-final { scan-tree-dump "pragma omp target private\\(bar\\) uses_allocators\\(bar: memspace\\(.\\), traits\\(\\)\\)" "original" } }
! { dg-final { scan-tree-dump "pragma omp target private\\(bar\\) uses_allocators\\(bar: memspace\\(.\\), traits\\(traits_array\\)\\)" "original" } }
! { dg-final { scan-tree-dump-times "__builtin_omp_init_allocator" 6 "gimple" } }
! { dg-final { scan-tree-dump-times "__builtin_omp_destroy_allocator" 6 "gimple" } }
