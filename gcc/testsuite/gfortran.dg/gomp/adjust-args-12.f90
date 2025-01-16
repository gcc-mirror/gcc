! { dg-additional-options "-fdump-tree-gimple" }

! PR fortran/118321

! Ensure that hidden arguments (return by reference) do not mess up the
! argument counting of need_device_ptr

! Here, we want to process the 3rd argument: 'c' as dummy argument = 'y' as actual.


! { dg-final { scan-tree-dump-times "__builtin_omp_get_mapped_ptr" 1 "gimple" } }
! { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(y, D\.\[0-9\]+\\);" "gimple" } }

! { dg-final { scan-tree-dump "ffff \\(&pstr.\[0-9\], &slen.\[0-9\], &\"abc\"\\\[1\\\]\{lb: 1 sz: 1\}, x, D\.\[0-9\]+, z, &\"cde\"\\\[1\\\]\{lb: 1 sz: 1\}, 3, 3\\);" "gimple" } }

module m
  use iso_c_binding
  implicit none (type, external)
contains
  character(:) function ffff (a,b,c,d,e)
    allocatable :: ffff
    character(*) :: a, e
    type(c_ptr), value :: b,c,d
  end
  character(:) function gggg (a,b,c,d,e)
    !$omp declare variant(ffff) match(construct={dispatch})  &
    !$omp&                      adjust_args(need_device_ptr : c)
    allocatable :: gggg
    character(*) :: a, e
    type(c_ptr), value :: b,c,d
  end
end module m

use m
implicit none (type, external)
type(c_ptr) :: x,y,z
character(len=:), allocatable :: str
!$omp dispatch
  str = gggg ("abc", x, y, z, "cde")
end
