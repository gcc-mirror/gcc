use iso_c_binding
implicit none
        integer, parameter :: omp_allocator_handle_kind = c_intptr_t
        integer, parameter :: omp_alloctrait_key_kind = c_int
        integer, parameter :: omp_alloctrait_val_kind = c_intptr_t
        integer, parameter :: omp_memspace_handle_kind = c_intptr_t
        integer (omp_memspace_handle_kind), &
           parameter :: omp_default_mem_space = 0
        integer (kind=omp_allocator_handle_kind), &
           parameter :: omp_default_mem_alloc = 1
        type omp_alloctrait
          integer (kind=omp_alloctrait_key_kind) key
          integer (kind=omp_alloctrait_val_kind) value
        end type omp_alloctrait
        interface
          function omp_alloc (size, allocator) bind(c)
            use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t
            import :: omp_allocator_handle_kind
            type(c_ptr) :: omp_alloc
            integer(c_size_t), value :: size
            integer(omp_allocator_handle_kind), value :: allocator
          end function omp_alloc
        end interface
contains
subroutine x
integer :: mem
type(omp_alloctrait), parameter:: mem2(1) = [omp_alloctrait(1,1)]
integer(omp_allocator_handle_kind) :: var
!$omp target uses_allocators(memspace(omp_default_mem_space), traits(mem2) : var) defaultmap(none)
block;
type(c_ptr) ::c
c = omp_alloc(omp_default_mem_space, 20_8)
end block
!$omp target uses_allocators(omp_default_mem_alloc, var(mem2))
block; end block
end
end
