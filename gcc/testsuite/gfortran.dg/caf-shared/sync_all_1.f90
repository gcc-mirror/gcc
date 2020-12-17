! { dg-do compile }
! { dg-options "-fdump-tree-original" }
program main
  sync all
end program main
! { dg-final { scan-tree-dump-times "__atomic_thread_fence \\(4\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_cas_coarray_sync_all" 1 "original" } }


