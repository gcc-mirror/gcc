! { dg-do compile }

module test
  integer, allocatable :: mvar1
  integer, allocatable :: mvar2
  integer, allocatable :: mvar3
end module

subroutine foo(x, y)
  use omp_lib
  implicit none
  integer  :: x
  integer  :: y
  
  integer, allocatable :: var1(:)
  integer, allocatable :: var2(:)
  integer, allocatable :: var3(:)
  integer, allocatable :: var4(:)
  integer, allocatable :: var5(:)
  integer, allocatable :: var6(:)
  integer, allocatable :: var7(:)
  integer, allocatable :: var8(:)
  integer, allocatable :: var9(:)

  !$omp allocate (var1) allocator(10) ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind at .1." }
  allocate (var1(x))

  !$omp allocate (var2)  ! { dg-error "'var2' in 'allocate' directive at .1. is not present in associated 'allocate' statement." }
  allocate (var3(x))

  !$omp allocate (x) ! { dg-message "sorry, unimplemented: 'allocate' directive that is not associated with an 'allocate' statement is not supported." }
  x = 2

  !$omp allocate (var4) ! { dg-error "'var4' with ALLOCATABLE attribute is not allowed in 'allocate' directive at .1. as this directive is not associated with an 'allocate' statement." } 
  ! { dg-message "sorry, unimplemented: 'allocate' directive that is not associated with an 'allocate' statement is not supported." "" { target *-*-* } .-1 }
  y = 2

  !$omp allocate (var5)
  !$omp allocate  ! { dg-error "Empty variable list is not allowed at .1. when multiple 'allocate' directives are associated with an 'allocate' statement." }
  allocate (var5(x))

  !$omp allocate (var6)
  !$omp allocate (var7)  ! { dg-error "'var7' in 'allocate' directive at .1. is not present in associated 'allocate' statement." }
  !$omp allocate (var8)  ! { dg-error "'var8' in 'allocate' directive at .1. is not present in associated 'allocate' statement." }
  allocate (var6(x))

  !$omp allocate (var9)
  !$omp allocate (var9)  ! { dg-error "'var9' is used in multiple 'allocate' directives at .1." }
  allocate (var9(x))

end subroutine

function outer(a)
  IMPLICIT NONE

  integer :: outer, a
  integer, allocatable :: var1

  outer = inner(a) + 5
  return

  contains

    integer function inner(x)
    integer :: x
    integer, allocatable :: var2

    !$omp allocate (var1, var2) ! { dg-error "'var1' is not in the same scope as 'allocate' directive at .1." }
    allocate (var1, var2)

    inner = x + 10
    return
    end function inner

end function outer

subroutine bar(s)
  use omp_lib
  use test
  integer  :: s
  integer, save, allocatable :: svar1
  integer, save, allocatable :: svar2
  integer, save, allocatable :: svar3

  type (omp_alloctrait) :: traits(3)
  integer (omp_allocator_handle_kind) :: a

  traits = [omp_alloctrait (omp_atk_alignment, 64), &
            omp_alloctrait (omp_atk_fallback, omp_atv_null_fb), &
            omp_alloctrait (omp_atk_pool_size, 8192)]
  a = omp_init_allocator (omp_default_mem_space, 3, traits)
  if (a == omp_null_allocator) stop 1

  !$omp allocate (mvar1) allocator(a) ! { dg-error "'mvar1' should use predefined allocator at .1." }
  allocate (mvar1)

  !$omp allocate (mvar2) ! { dg-error "'mvar2' should use predefined allocator at .1." }
  allocate (mvar2)

  !$omp allocate (mvar3) allocator(omp_low_lat_mem_alloc)
  allocate (mvar3)

  !$omp allocate (svar1)  allocator(a) ! { dg-error "'svar1' should use predefined allocator at .1." }
  allocate (svar1)

  !$omp allocate (svar2) ! { dg-error "'svar2' should use predefined allocator at .1." }
  allocate (svar2)

  !$omp allocate (svar3) allocator(omp_low_lat_mem_alloc)
  allocate (svar3)
end subroutine

