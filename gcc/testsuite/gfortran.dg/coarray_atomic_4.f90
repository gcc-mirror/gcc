! { dg-do compile }
! { dg-options "-fcoarray=single -fdump-tree-original" }
!
use iso_fortran_env, only: atomic_int_kind, atomic_logical_kind
implicit none

intrinsic :: atomic_define
intrinsic :: atomic_ref
intrinsic :: atomic_cas
intrinsic :: atomic_add
intrinsic :: atomic_and
intrinsic :: atomic_or
intrinsic :: atomic_xor
intrinsic :: atomic_fetch_add
intrinsic :: atomic_fetch_and
intrinsic :: atomic_fetch_or
intrinsic :: atomic_fetch_xor
integer(atomic_int_kind) :: caf[*], var
logical(atomic_logical_kind) :: caf_log[*], var2
integer :: stat
integer(1) :: var3
logical(1) :: var4

call atomic_define(caf, var, stat=stat)
call atomic_define(caf_log, var2, stat=stat)

call atomic_ref(var, caf, stat=stat)
call atomic_ref(var2, caf_log, stat=stat)

call atomic_cas(caf, var, 3_atomic_int_kind, 5_1, stat=stat)
call atomic_cas(caf_log, var2, .true._atomic_logical_kind, &
                .false._2, stat=stat)

call atomic_add(caf, 77, stat=stat)
call atomic_and(caf, 88, stat=stat)
call atomic_or(caf, 101, stat=stat)
call atomic_xor(caf, 105_2, stat=stat)

call atomic_fetch_add(caf, var3, var, stat=stat)
call atomic_fetch_and(caf, 22_1, var, stat=stat)
call atomic_fetch_or(caf, var3, var, stat=stat)
call atomic_fetch_xor(caf, 47_2, var, stat=stat)

end

! All the atomic calls:
! { dg-final { scan-tree-dump-times "  __atomic_store_4 \\(&caf, \\(integer\\(kind=4\\)\\) var, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "  __atomic_store_4 \\(&caf_log, \\(logical\\(kind=4\\)\\) var2, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "var = \\(integer\\(kind=4\\)\\) __atomic_load_4 \\(&caf, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "var2 = \\(logical\\(kind=4\\)\\) __atomic_load_4 \\(&caf_log, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "  __atomic_compare_exchange_4 \\(&caf, &var, 5, 0, 0, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "  __atomic_compare_exchange_4 \\(&caf_log, &var2, 0, 0, 0, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "  __atomic_fetch_add_4 \\(&caf, 77, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "  __atomic_fetch_and_4 \\(&caf, 88, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "  __atomic_fetch_or_4 \\(&caf, 101, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "  __atomic_fetch_xor_4 \\(&caf, 105, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "var = \\(integer\\(kind=4\\)\\) __atomic_fetch_add_4 \\(&caf, \\(integer\\(kind=4\\)\\) var3, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "var = \\(integer\\(kind=4\\)\\) __atomic_fetch_and_4 \\(&caf, 22, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "  var = \\(integer\\(kind=4\\)\\) __atomic_fetch_or_4 \\(&caf, \\(integer\\(kind=4\\)\\) var3, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "  var = \\(integer\\(kind=4\\)\\) __atomic_fetch_xor_4 \\(&caf, 47, 0\\);" 1 "original" } }

! CAS: Handle "compare" argument
! { dg-final { scan-tree-dump-times "var = 3;" 1 "original" } }
! { dg-final { scan-tree-dump-times "var2 = 1;" 1 "original" } }

! All calls should have a stat=0
! { dg-final { scan-tree-dump-times "stat = 0;" 14 "original" } }

