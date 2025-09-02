! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=lib" }
!
! Coarray sync memory managed by the external library
!
implicit none
integer :: stat
character(len=42) :: msg
sync memory
sync memory(stat=stat)
sync memory(errmsg=msg)
sync memory(errmsg=msg, stat=stat)
end

! { dg-final { scan-tree-dump-times "_gfortran_caf_sync_memory \\(0B, 0B, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_sync_memory \\(&stat, 0B, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_sync_memory \\(0B, &&msg, 42\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_sync_memory \\(&stat, &&msg, 42\\);" 1 "original" } }
