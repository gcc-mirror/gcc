! { dg-do compile }
! { dg-options "-fcoarray=lib -fdump-tree-original" }
!
! PR fortran/53526
!
! Check handling of move_alloc with coarrays

subroutine ma_scalar (aa, bb)
  integer, allocatable :: aa[:], bb[:]
  call move_alloc(aa,bb)
end

subroutine ma_array (cc, dd)
  integer, allocatable :: cc(:)[:], dd(:)[:]
  call move_alloc (cc, dd)
end

! { dg-final { scan-tree-dump-times "free" 0 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_sync_all" 2 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister" 2 "original" } }
! { dg-final { scan-tree-dump-times "\\*bb = \\*aa" 1 "original" } }
! { dg-final { scan-tree-dump-times "\\*dd = \\*cc" 1 "original" } }
