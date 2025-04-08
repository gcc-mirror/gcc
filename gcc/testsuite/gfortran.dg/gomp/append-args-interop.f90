! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

! Test that interop objects are implicitly created/destroyed when a dispatch
! construct doesn't provide enough of them to satisfy the declare variant
! append_args clause. 

module m
  use iso_c_binding, only: c_intptr_t
  integer, parameter :: omp_interop_kind = c_intptr_t
contains
subroutine g(x,y,z)
  integer(omp_interop_kind) :: x, y, z
  value :: y
end
subroutine f()
  !$omp declare variant(f: g) append_args(interop(target), interop(prefer_type("cuda","hip"), targetsync), interop(target,targetsync,prefer_type({attr("ompx_foo")}))) match(construct={dispatch})
end
end

use m
!$omp dispatch device(99)
  call f()
end

! { dg-final { scan-tree-dump "__builtin_GOMP_interop \\(99, 3, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+, &pref_type\.\[0-9\]+, " "gimple" } }
! { dg-final { scan-tree-dump "__builtin_GOMP_interop \\(99, 0, 0B, 0B, 0B, 0, 0B, 3, &interopobjs\.\[0-9\]+," "gimple" } }
! { dg-final { scan-tree-dump "g \\(&interop\.\[0-9\]+, interop\.\[0-9\]+, &interop\.\[0-9\]+\\)" "gimple" } }
