! { dg-do compile }

program main
  use omp_lib
  implicit none
  integer (omp_allocator_handle_kind) :: bar, foo

  type (omp_alloctrait), parameter :: traits_array(*) = &
       [omp_alloctrait(omp_atk_pinned,omp_atv_true),&
       omp_alloctrait(omp_atk_partition,omp_atv_nearest)]

  !$omp target uses_allocators(omp_non_existant_alloc) ! { dg-error "Symbol 'omp_non_existant_alloc' at .1. is ambiguous" }
  block
  end block

  !$omp target uses_allocators(bar(traits_array), foo (traits_array), ) ! { dg-error "Expected identifier at .1." }
  block                                                                 ! { dg-error "Failed to match clause at .1." "" { target *-*-* } .-1 }
  end block

  !$omp target uses_allocators(traits(xyz) : bar) ! { dg-error "Symbol 'xyz' at .1. is ambiguous" }
  block
  end block

  !$omp target uses_allocators(memspace(omp_non_existant_mem_space) : foo) ! { dg-error "Symbol 'omp_non_existant_mem_space' at .1. is ambiguous" }
  block
  end block

  !$omp target uses_allocators(traits(traits_array), traits(traits_array) : bar) ! { dg-error "duplicate 'traits' modifier at .1." }
  block
  end block

  !$omp target uses_allocators(memspace(omp_default_mem_space), memspace(omp_default_mem_space) : foo) ! { dg-error "duplicate 'memspace' modifier at .1." }
  block
  end block

  !$omp target uses_allocators(memspace(omp_default_mem_space), traits(traits_array), traits(traits_array) : foo) ! { dg-error "duplicate 'traits' modifier at .1." }
  block
  end block

  !$omp target uses_allocators (omp_null_allocator) ! { dg-error "'omp_null_allocator' cannot be used in 'uses_allocators' clause at .1." }
  block
  end block

  !$omp target uses_allocators (memspace(omp_high_bw_mem_space) : foo, bar) ! { dg-error "'uses_allocators' clause only accepts a single allocator when using modifiers at .1." }
  block
  end block

  !$omp target uses_allocators (memspace(omp_high_bw_mem_space) : foo(foo_traits)) ! { dg-error "legacy 'foo\\\(foo_traits\\\)' traits syntax not allowed in 'uses_allocators' clause when using modifiers at .1." }
  block
  end block

end program main
