! { dg-do compile }
! { dg-options "-fdump-tree-original" }
program main
  real, allocatable :: a[:]
  allocate (a[*])
  deallocate (a)
end program main
! { dg-final { scan-tree-dump-times "_gfortran_cas_coarray_sync_all" 1 "original" } }

