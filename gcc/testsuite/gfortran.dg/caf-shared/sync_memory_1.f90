! { dg-do compile }
! { dg-options "-fdump-tree-original" }
program main
  sync memory
end program main
! { dg-final { scan-tree-dump-times "__atomic_thread_fence \\(4\\)" 1 "original" } }
